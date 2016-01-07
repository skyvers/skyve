/*
 * Isomorphic SmartClient
 * Version SNAPSHOT_v10.1p_2015-12-10 (2015-12-10)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */
//> @object PortalPosition
//
// Represents the position of a +link{Portlet} within a +link{PortalLayout}, indicating the
// column, row, and position within the row.
//
// @visibility external
// @treeLocation Client Reference/Layout/PortalLayout
//<

//> @attr portalPosition.colNum (int : 0 : IR)
//
// The column number occupied by a +link{Portlet} within a +link{PortalLayout}.
//
// @visibility external
//<

//> @attr portalPosition.rowNum (int : 0 : IR)
//
// The row number occupied by a +link{Portlet} within a +link{PortalLayout} column.
//
// @visibility external
//<

//> @attr portalPosition.position (int : 0 : IR)
//
// The position occupied by a +link{Portlet} within a +link{PortalLayout} row
// (generally 0, unless there is more than one Portlet in the row).
//
// @visibility external
//<

//> @class Portlet
// Custom subclass of Window configured to be embedded within a PortalLayout.
// @visibility external
// @treeLocation Client Reference/Layout/PortalLayout
//<
isc.defineClass("Portlet", "Window").addProperties({
    showShadow:false,
    
    // enable predefined component animation
    animateMinimize:true,

    // Window is draggable with "outline" appearance by default.
    // "target" is the solid appearance.
    dragAppearance:"outline",
   
    //>@attr portlet.dragType (string : "Portlet" : IRWA)
    // <p>By default, +link{portalLayout.portletDropTypes} is set so that any component can be
    // dragged into a +link{PortalLayout}. If the component is not a +link{Portlet},
    // it will be automatically be wrapped in a newly created +link{Portlet}.</p>
    //
    // <p>If you prefer to only allow real +link{Portlet,Portlets} to be dragged into a 
    // +link{PortalLayout}, then you can set +link{portalLayout.portletDropTypes} to 
    // <code>["Portlet"]</code>, since <code>Portlet.dragType</code> defaults to 
    // <code>"Portlet"</code>.</p>
    //
    // <p>if you want to allow some +link{Portlet,Portlets} to be dropped on a +link{PortalLayout} 
    // but not others, then you can specify a custom <code>dragType</code> for +link{Portlet,Portlets}, 
    // and then set +link{PortalLayout.portletDropTypes} as appropriate.</p>
    //
    // @group dragdrop
    // @see canvas.dragType
    // @see portalLayout.portletDropTypes
    // @visibility external
    //<
    dragType: "Portlet",

    //>@attr portlet.canDrop (boolean : true : IRW)
    // Portlets have canDrop set to true to enable drag/drop reposition within the portalLayout
    // @visibility external
    // @example repositionPortlets
    //<
    canDrop:true,

    // We don't want to allow the Portlet itself to overflow
    overflow: "hidden",

    //>@attr portlet.minHeight (Number : 60 : IRW)
    // Specifies a minimum height for the Portlet. The height of rows within a +link{PortalLayout}
    // will be adjusted to take into account the minHeight of all the Portlets in that row.
    // @see Canvas.minHeight
    // @visibility external
    //<
    minHeight: 60,

    setMinHeight : function (height) {
        this.minHeight = height;
        if (this.portalRow) this.portalRow._checkPortletHeights();
    },

    //>@attr portlet.minWidth (Number : 70 : IRW)
    // Specifies a minimum width for the Portlet.
    // @see Canvas.minWidth
    // @visibility external
    //<
    minWidth: 70,

    setMinWidth : function (width) {
        if (this.minWidth == width) return;
        this.minWidth = width;
        if (this.portalRow) this.portalRow.reflow("Portlet minWidth changed");
    },

    //>@attr portlet.height (Number or String : null : IRW)
    // 
    // If you initialize the height of a Portlet, then that height will be used as the 
    // Portlet's +link{rowHeight,rowHeight} (if no rowHeight is set).
    // <p>
    // After initialization, the +link{PortalLayout} manages the height of Portlets. If you
    // want to change the height, use +link{setRowHeight()}.
    //
    // @see rowHeight
    // @see setRowHeight()
    // @visibility external
    //<

    //>@method portlet.setHeight()
    //
    // The height of a Portlet is managed by the +link{PortalLayout}. If you want to change
    // the Portlet's height, use +link{setRowHeight()} instead.
    //
    // @group sizing
    // @param height (number) new height
    // @see setRowHeight()
    // @see rowHeight
    // @visibility external
    //<

    //>@attr portlet.rowHeight (Number or String : null : IRW)
    // If you set the rowHeight of a Portlet before adding it to a +link{PortalLayout}, then
    // the height will be used when creating the new row. If adding the Portlet
    // to an existing row (or dragging it there), the Portlet's rowHeight will be used if 
    // the row's height has not already been specified. However, if you 
    // set the rowHeight of a Portlet after adding it to the PortalLayout, then the height
    // of the entire row will always be adjusted to match.
    // <p>
    // You can also just specify +link{Canvas.height,height} when initializing a Portlet, and it
    // will be applied to the rowHeight when added to a PortalLayout. However, changing the Portlet's
    // height after initialization will not affect the row.
    // <p>
    // Note that getting the rowHeight will indicate the rowHeight specified for this Portlet,
    // not the actual height of the row it is in.
    // @group sizing
    // @example portalLayoutColumnHeight
    // @visibility external
    //<

    // Also see the code in portalRow.addPortlets to see how the rowHeight and _userHeight are applied there.
    // Note that we do not keep track of changes to the real row's height -- we are only responding
    // to explicitly setting rowHeight on the portlet. Tracking the real row's height may sometimes make some
    // sense, but the user's intentions aren't necessarily easy to model -- for instance, the user may have
    // shrunk a row but want the portlet to automatically expand if dragged to an empty column.

    //>@method portlet.setRowHeight()
    // Sets the height of the Portlet's row (and, thus, indirectly sets the Portlet's own height).
    // Use this instead of using +link{setHeight()} directly.
    // @param height (number) new height
    // @group sizing
    // @visibility external
    //<
    setRowHeight : function (height) {
        this.rowHeight = height;
        if (this.portalRow) this.portalRow.setHeight(height);
    },

    //> @method portlet.getPortalLayout()
    // Gets the +link{PortalLayout} which encloses this Portlet (or null, if none).
    // @return (PortalLayout) the PortalLayout enclosing this Portlet
    // @visibility external
    //<
    getPortalLayout : function () {
        if (this.portalRow) {
            return this.portalRow.portalLayout;
        } else {
            return null;
        }
    },

    //> @method portlet.getPortalPosition()
    // Gets the position of the Portlet within its +link{PortalLayout}. Returns null
    // if the Portlet is not in a PortalLayout.
    // @return (PortalPosition) the position of the Portlet
    // @visibility external
    //<
    getPortalPosition : function () {
        var layout = this.getPortalLayout();
        if (layout) {
            return layout.getPortalPosition(this);
        } else {
            return null;
        }
    },

    // Resize from any edge except corners -- the resize may be "grabbed" by a parent,
    // and it's simpler if we don't have to deal with resizing two directions at once.
    resizeFrom: ["T", "B", "L", "R"],
	
    edgeCursorMap: {
        "L": "col-resize",
        "R": "col-resize",
        "T": "row-resize",
        "B": "row-resize"
	},

    // customize the appearance and order of the controls in the window header
    // (could do this in load_skin.js instead)
    showMaximizeButton: true,
    headerControls:["headerLabel", "minimizeButton", "maximizeButton", "closeButton"],

    // show either a shadow, or translucency, when dragging a portlet
    // (could do both at the same time, but these are not visually compatible effects)
    //showDragShadow:true,
    dragOpacity:30,
    
    //>@attr portlet.showCloseConfirmationMessage (Boolean : true : IRW)
    // If true, +link{closeConfirmationMessage} will be displayed before portlets are closed
    // @visibility external
    //<
    showCloseConfirmationMessage:true,
    
    //>@attr portlet.closeConfirmationMessage (string : "Close portlet?" : IRW)
    // Confirmation message to show the user when closing portlets if
    // +link{showCloseConfirmationMessage} is true.
    // @visibility external
    // @group i18nMessages
    //<
    closeConfirmationMessage:"Close portlet?",
   
    //>@attr portlet.closeConfirmationDialogProperties (Dialog Properties : null : IRW)
    // If specified, this properties block will be passed to +link{isc.confirm()} as the
    // properties parameter when the +link{closeConfirmationMessage} is shown, 
    // allowing developers to customize the appear of the confirmation
    // dialog (modifying its title, etc).
    //
    // @visibility external
    // @group i18nMessages
    //<
   
    //>@attr portlet.destroyOnClose (Boolean : false : IRW)
    // Whether to call +link{Canvas.destroy,destroy()} when closing the Portlet.
    // @visibility external
    //<

    //>@method portlet.closeClick()
    // Handles a click on the close button of this portlet.  The default implementation
    // calls +link{close()}.
    // <P>
    // <smartclient>Override this method if you want
    // other actions to be taken.</smartclient>
    // <smartgwt>Developers may use <code>addCloseClickHandler()</code> to provide custom
    // handling when the user clicks this button.</smartgwt>
    // Custom implementations may call <code>close()</code> to trigger the default behavior.
    // @visibility external
    //<
    
    //> @method portlet.close()
    // <code>close()</code> method overridden to show 
    // +link{portlet.closeConfirmationMessage} to the user before
    // removing the portlet from the PortalLayout via +link{portalLayout.removePortlet()}
    // @visibility external
    //<
    close : function () {
        var portalLayout = this.getPortalLayout();
        if (portalLayout && portalLayout.willClosePortlet) {
            var proceed = portalLayout.willClosePortlet(this);
            // Require explicit return of false to cancel
            if (proceed === false) return;
        }

        if (this.showCloseConfirmationMessage) {
            isc.confirm(this.closeConfirmationMessage, 
                    {target:this, methodName:"confirmedClosePortlet"},
                    this.closeConfirmationDialogProperties);
        } else {
            this.confirmedClosePortlet(true);
        }
    },
    
    confirmedClosePortlet : function (value) {
        if (!value) return;
    
        // If we have an editContext, we'll do the removal from there whether or not
        // we are in editMode
        if (this.editContext && this.editNode) {
            this.editContext.removeNode(this.editNode);
        } else {
            if (this.portalRow) {
                this.portalRow.removePortlets(this);
            } else {
                this.clear();
            }
        }

        if (this.destroyOnClose) this.markForDestroy();
    },

    onMaximizeClick : function () {
        var portalLayout = this.getPortalLayout();
        if (portalLayout && portalLayout.willMaximizePortlet) {
            // Require explicit return of false to cancel
            return portalLayout.willMaximizePortlet(this) === false ? false : true;
        } else {
            return true;
        }
    },

    onMinimizeClick : function () {
        var portalLayout = this.getPortalLayout();
        if (portalLayout && portalLayout.willMinimizePortlet) {
            // Require explicit return of false to cancel
            return portalLayout.willMinimizePortlet(this) === false ? false : true;
        } else {
            return true;
        }
    },

    onRestoreClick : function () {
        var portalLayout = this.getPortalLayout();
        if (portalLayout && portalLayout.willRestorePortlet) {
            // Require explicit return of false to cancel
            return portalLayout.willRestorePortlet(this) === false ? false : true;
        } else {
            return true;
        }
    },

    _createPlaceholder : function () {
        if (this._portletPlaceholder) return;
        
        // First we record the absolute coordinates of the portlet, so that
        // when we remove it from the layout, we can draw it at the same place ...
        // that will make the animation look right.
        var width = this.getVisibleWidth(), 
            height = this.getVisibleHeight(),
            pageLeft = this.getPageLeft(), 
            pageTop = this.getPageTop(),
            userHeight = this._userHeight,
            userWidth = this._userWidth
        ;

        this._portletPlaceholder = isc.Canvas.create({
            width: width,
            height: height,
            minHeight: this.getMinHeight(),
            minWidth: this.getMinWidth(),
            minimized: this.minimized,
            _userHeight: this._userHeight,
            _userWidth: this._userWidth,
            _portlet: this
        });

        if (this.parentElement) {
            this.masterLayout = this.parentElement;
            this.masterLayout.portletMaximizing = true;
            this.masterLayout.replaceMember(this, this._portletPlaceholder, false);
            this.masterLayout.portletMaximizing = false;
        }

        // Now that we've swapped it out of the layout, redraw it in the same place
        this.setWidth(width);
        this.setHeight(height);

        // But, give it the same _userHeight and _userWidth so they get restored.
        this._userHeight = userHeight;
        this._userWidth = userWidth;

        this.moveTo(pageLeft, pageTop);
        this.bringToFront();
        this.draw();
    },

    _destroyPlaceholder : function () {
        if (!this._portletPlaceholder) return;
        
        if (this.masterLayout && this.masterLayout.hasMember(this._portletPlaceholder)) {
            this.masterLayout.portletMaximizing = true;
            this.masterLayout.replaceMember(this._portletPlaceholder, this);
            this.masterLayout.portletMaximizing = false;
        }
        this._portletPlaceholder._portlet = null;
        this._portletPlaceholder.destroy();
        
        delete this._portletPlaceholder;
        delete this.masterLayout;
    },

    maximize : function () {
        this._createPlaceholder();

        // Now that we've redrawn it outside the layout, do the regular maximizing
        this.delayCall("doMaximize");
    },
   
    restore : function () {
        // If we're restoring the portlet, make sure that its row is restored
        // first, to make space for it. We don't need to check other portlets,
        // since if one is restored then the row needs to be restored. Note
        // that this is a no-op if the row is already restored.
        if (this.portalRow) this.portalRow.restore();
        
        this.Super("restore", arguments);
    },

    completeRestore : function () {
        this.Super("completeRestore", arguments);
        this._destroyPlaceholder();
        if (this.portalRow) this.portalRow._checkPortletHeights();

        var portalLayout = this.getPortalLayout();
        if (portalLayout && portalLayout.portletRestored) {
            portalLayout.portletRestored(this);
        }
    },

    doMaximize : function () {
        this.Super("maximize", arguments);
    },

    completeMaximize : function () {
        this.Super("completeMaximize", arguments);
        var portalLayout = this.getPortalLayout();
        if (portalLayout && portalLayout.portletMaximized) {
            portalLayout.portletMaximized(this);
        }
    },

    completeMinimize : function () {
        this.Super("completeMinimize", arguments);
        this._percent_height = null;
        this._percent_width = null;
        this._destroyPlaceholder();
        if (this.portalRow) this.portalRow._checkPortletHeights();
        
        var portalLayout = this.getPortalLayout();
        if (portalLayout && portalLayout.portletMinimized) {
            portalLayout.portletMinimized(this);
        }
    },
    
    resized : function (deltaX, deltaY, reason) {
        this.Super("resized", arguments); 
        
        // If we're maximizing or restoring, then ignore the resize
        if (this.masterLayout) return;

        var portalLayout = this.getPortalLayout();
        if (portalLayout) portalLayout._portletsResized();
    }
});

// A resizeBar for Portlets and PortalRows. We don't actually want to show a
// visible resizeBar between Portlets and between PortalRows, because the UI
// becomes visually cluttered with the edges of the Portlet (which are
// themselves draggable). As far as spacing goes, simply using membersMargin
// would be sufficient. However, using membersMargin creates a zone in which
// drags cannot easily be initiated, because the membersMargin is not literally
// a margin ... it merely affects the location of the next member. So there is
// nothing in the "margin" to initiate a drag. Thus, we need a specialized
// resizeBar in order to have both spacing and resizing by initiating a drag
// in that space. Essentially, this is an invisible resizeBar.
isc.defineClass("PortalResizeBar", isc.Canvas);

isc.PortalResizeBar.addProperties({
    dragStartDistance: 1,
    isMouseTransparent: true,
    overflow: "hidden",
    _redrawWithMaster: false, 
    _resizeWithMaster: false,

    // We'll resize the target ...
    canDragResize: true,

    initWidget : function () {
        this.Super("initWidget", arguments);

        this.dragTarget = this.target;
	},
    
    edgeCursorMap: {
        "R": "col-resize",
        "B": "row-resize"
	},

    // Return the event edge relative to the target
    getEventEdge : function () {
        return this.vertical ? "R" : "B";
    }
});

// provides a menu for adding a remove columns
isc.defineClass("PortalColumnHeader", "HLayout").addProperties({
    height: 20,
    noResizer: true,
    minWidth: 154,

    border:"1px solid #CCCCCC",

    // allow dragging by the header
    canDragReposition: true, 

    initWidget : function () {
        this.Super("initWidget", arguments);

        // header drags the portalColumn
        this.dragTarget = this.creator;

        this.addMember(isc.LayoutSpacer.create());

        this.menu = this.getMenuConstructor().create({
            width: 150,
            portalColumn: this.creator,
            data: [{
                title: "Remove Column",
                click: "menu.portalColumn.removeSelf()",
                // Don't offer to remove the last column.
                enableIf: function (target, menu, item) {
                    return menu.portalColumn.portalLayout.getMembers().length > 1;
                }
            },{
                title: "Add Column",
                click: "menu.portalColumn.addNewColumn()"
            }]
        });

        this.addMember(isc.MenuButton.create({
            title: "Column Properties",
            width: 150,
            menu: this.menu
        }));

        this.addMember(isc.LayoutSpacer.create());
    }
});

// Manages horizontal vs vertical drag and drop such that a drop to the sides is a drop within
// this PortalRow and a drop above or below is a drop within the parent, before or after this
// PortalRow.
// Created whenever a drop occurs in a PortalColumn (even if it's the first drop).
// Note that you can drop just about anything on a PortalRow -- it will be wrapped in a Portlet
// if necessary.
isc.defineClass("PortalRow", "Layout").addProperties({
    vertical : false,

    // This makes VisualBuilder generate code inline for PortalRows, rather than
    // creating standalone.
    _generated: true,

    // To apply the minWidth of Portlets 
    respectSizeLimits: true,

    // Note that by default, we stretch column widths so that the scroll bars will not appear
    // at the row level ... see portalLayout.canStretchColumnWidths ... thus this setting should
    // only matter when canStretchColumnWidths is false.
    overflow: "auto",

    // leave some space between portlets
    resizeBarClass: "PortalResizeBar",
    resizeBarSize: 3,
    defaultResizeBars: "middle",

    // enable drop handling
    canAcceptDrop:true,
    
    // change appearance of drag placeholder and drop indicator
    dropLineThickness:2,
    dropLineProperties:{backgroundColor:"blue"},
    
    edgeCursorMap: {
        "T": "row-resize",
        "B": "row-resize"
    },

    // Will accept a portlets attribute and add it (or them, if an array) to the portalRow
    initWidget : function () {
        this.Super("initWidget", arguments);
        if (this.portlets) this.addPortlets(this.portlets);
        this.portlets = null;
    },

    // When creating resizeBars for Portlets, intialize canDragResize
    createResizeBar : function () {
        var resizeBar = this.Super("createResizeBar", arguments);
        resizeBar.canDragResize = this.canResizePortlets;
        return resizeBar;
    },
    
    setCanResizePortlets : function (canResize) {
        this.canResizePortlets = canResize;

        // If the row has a resizeBar, update it
        if (this._resizeBar) this._resizeBar.canDragResize = canResize;

        // And update portlets and their resizeBars
        this.getPortlets().map(function (portlet) {
            portlet.canDragResize = canResize;
            if (portlet._resizeBar) portlet._resizeBar.canDragResize = canResize;
        });
    },

    // Check whether we want to grab the resize started by a Portlet
    prepareForDragging : function () {
        var EH = this.ns.EH;

        // If no one has already set the dragTarget, then do the usual thing to set it
        if (!EH.dragTarget) this.Super("prepareForDragging", arguments);

        // Make certain adjustments if the dragTarget is one of our Portlets
        if (this.hasMember(EH.dragTarget) && EH.dragOperation == EH.DRAG_RESIZE) {
            switch (EH.resizeEdge) {
                case "B":
                case "T":
                    // Don't resize the Portlet -- resize me instead!
                    EH.dragTarget = this;
                    break;

                case "L":
                    var index = this.getMemberNumber(EH.dragTarget);
                    if (index > 0) {
                        // If we're resizing from the left edge, and there is a previous portlet
                        // then switch to it, and switch edges -- this makes the layout reflow
                        // in a way that makes more sense to the user.
                        EH.dragTarget = this.getMember(index - 1);
                        EH.resizeEdge = "R";
                    } else {
                        // If we're resizing from the left edge, and there is no previous portlet,
                        // then the user essentially wants to move this portlet left. The only
                        // way to accomplish that is to move the column to the left, so we actually
                        // need to resize the column. Note that the prepareForDragging event will
                        // bubble up to the PortalColumn, which may make further adjustments.
                        EH.dragTarget = this.portalColumn;
                    }
                    break;

                case "R":
                    var index = this.getMemberNumber(EH.dragTarget);
                    var peers = this.getMembers().length;
                    
                    if (index == peers - 1) {
                        // If we're resizing the last Portlet in the row, then the user likely
                        // means to be resizing the column instead.
                        EH.dragTarget = this.portalColumn;
                    }
                    break;
            }
        }

        // Check whether we are the dragTarget ... either natively or because
        // we took over the drag.
        if (EH.dragTarget == this && EH.resizeEdge == "T" && EH.dragOperation == EH.DRAG_RESIZE) {
            // If we're resizing from the top edge, then resize fhe
            // previous row from the bottom edge instead (unless there
            // is no previous row, in which case we cancel). The reason
            // is that the Layout will then reflow in ways that seem
            // more natural to the user. We could just disallow
            // resizing from the top edge instead, but this seems to
            // work well.
            var index = this.parentElement.getMemberNumber(this);
            
            if (index > 0) {
                EH.dragTarget = this.parentElement.getMember(index - 1);
                EH.resizeEdge = "B";
            } else {
                EH.dragTarget = null;
            }
        }
    },

    // Reset each portlet's resizeFrom so that we don't advertise resizes which
    // we would end up canceling.
    // 
    // Note that we enforce PortalLayout.canResizeColumns in cases where
    // resizing a Portlet would be delegated up to resize the column. Thus, if
    // PortalLayout.canResizeColumns is false, then resizing Portlets from the
    // left or right edge will only be allowed where we are really just
    // resizing Portlets (i.e. where there are multiple Portlets in the row).
    _updatePortletResizeFrom : function () {
        // Bail if we have no members or no parent
        if (this.members.length == 0 || !this.parentElement || !this.portalLayout) return;

        var rowIndex = this.parentElement.getMemberNumber(this);
        var columnIndex = this.portalLayout.getPortalColumnNumber(this.portalColumn);

        // For the row itself.
        this.resizeFrom = rowIndex > 0 ? ["T", "B"] : ["B"];

        for (var portletIndex = 0; portletIndex < this.members.length; portletIndex++) { 
            // We always allow "B" (assuming that canResizePortlets is true -- otherwise,
            // it doesn't matter). This allows the bottom-most Portlet to be enlarged,
            // which would make sense to force overflow, if that is what the user wants
            // (i.e. vertical scrolling).
            var resizeFrom = ["B"];

            // We allow "T" except in the top row, since resizing the top Portlet from
            // the top edge doesn't make sense (it's pinned to the top anyway).
            if (rowIndex > 0) resizeFrom.add("T");

            // We allow "L" if there is a Portlet to our left, since in that
            // case we really would be resizing Portlets. If not, we check for
            // a column to our left (since dragging the left-most column to the
            // left doesn't make sense). If there is a column to our left, the
            // drag would be delegated up to the column, so we check whether
            // canResizeColumns is set. 
            if (portletIndex > 0 || (columnIndex > 0 && this.portalLayout.canResizeColumns)) {
                resizeFrom.add("L");
            }

            // We allow "R" if there is a Portlet to our right, since in that
            // case we really would be resizing Portlets. If not, then the drag
            // would be delegated up to the column, so we consult
            // canResizeColumns. This allows dragging the right-most column to
            // the right, since that can make sense to force overflow if that
            // is what the user wants (i.e. horizontal scrolling).
            if (portletIndex != this.members.length - 1 || this.portalLayout.canResizeColumns) {
                resizeFrom.add("R");
            }

            var portlet = this.getMember(portletIndex);
            portlet.resizeFrom = resizeFrom;
        }
    },

    // number of pixels you have to be within the left or right border of a portlet for us to
    // show a drop to the left or right of this portlet.  If not within this margin, drop is
    // indicated above or below instead.
    hDropOffset: 15,
    isHDrop : function () {
        var dropPosition = this.getDropPosition();
        var dropOverTarget = this.getMember(dropPosition == 0 ? 0 : dropPosition - 1);
        if (!dropOverTarget.containsEvent() && dropPosition < this.members.length) {
            dropOverTarget = this.getMember(dropPosition);
        }

        var targetOffsetX = dropOverTarget.getOffsetX();
        if (targetOffsetX < this.hDropOffset || targetOffsetX > dropOverTarget.getVisibleWidth() - this.hDropOffset) {
            return true;
        } else {
            return false;
        }
    },

    // We pass through the drop if it is a PortalColumn, since it doesn't make sense to drop
    // a PortalColumn on a PortalRow -- the PortalLayout will handle it.
    isPortalColumnDrop : function () {
        var dragTarget = this.ns.EH.dragTarget;
        var type = dragTarget.getDragType();
        if (type == "PortalColumn") return true;

        //>EditMode
        if (dragTarget.isA("Palette")) {
            var data = dragTarget.getDragData(),
                component = (isc.isAn.Array(data) ? data[0] : data);
            if (component.type == "PortalColumn" || component.className == "PortalColumn") return true;
        }
        //<EditMode

        return false;
    },

    willAcceptDrop : function() {
        // If we're dropping a PortalColumn, then the dropTarget should be the PortalLayout
        if (this.isPortalColumnDrop()) return null;

        // If we're not near a horizontal edge, then the rowLayout should handle it
        if (!this.isHDrop()) return null;

        // By default, portalLayout.willAcceptPortletDrop just calls back to our
        // superclass. But subclasses of portalLayout could do something different.
        return this.portalLayout.willAcceptPortletDrop(
            this.ns.EH.dragTarget,
            this.portalLayout.getPortalColumnNumber(this.portalColumn),
            this.portalColumn.getPortalRowNumber(this),
            this.getDropPosition()
        );
    },

    dropMove : function () {
        if (this.willAcceptDrop()) {
            // If we are the actual dropTarget, then hide our parent's drop line and show ours
            this.parentElement.hideDropLine();
            this.showDropLine();
            
            // We've handled it, so stop the bubbling
            return false;
        } else {
            // Let it bubble
            this.hideDropLine();
            return true;
        }
    },
    
    dropOver : function () {
        // same logic as dropMove
        return this.dropMove();
    },

    getDropComponent : function (dragTarget, dropPosition) {
        var dropComponent = this.portalLayout.getDropPortlet(
            dragTarget,
            this.portalLayout.getPortalColumnNumber(this.portalColumn),
            this.portalColumn.getPortalRowNumber(this),
            dropPosition
        );
        
        //>EditMode
        if (this.handleDroppedEditNode) dropComponent = this.handleDroppedEditNode(dropComponent, dropPosition);
        //<EditMode

        if (dropComponent) {
            // Check if the dropComponent is a Portlet (or subclass) ... if not, wrap it in one.
            if (!isc.isA.Portlet(dropComponent)) {
                dropComponent = isc.Portlet.create({
                    autoDraw: false,
                    title: "",
                    items: dropComponent,
                    destroyOnClose: true
                });
            }
        }

        return dropComponent;
    }, 
 
    setMinHeight : function (height) {
        if (this.minHeight == height) return;
        this.minHeight = height;
        if (this.portalColumn) this.portalColumn.rowLayout.reflow("PortalRow minHeight changed");
    },

    shouldAlterBreadth : function (member) {
        // Don't change height of minimized members
        if (member.minimized) return false;

        return this.Super("shouldAlterBreadth", arguments);
    },

    _checkPortletHeights : function () {
        // Check if all the portlets are minimized ... in that case, minimize
        // the row. Otherwise, restore the row. Note that both minimize and
        // restore are no-ops if we're already in the desired state.
        if (this.members.map(function (portlet) {
            return portlet.minimized;
        }).and()) {
            this.minimize();
        } else {
            this.setMinHeight(
                this.members.map("getMinHeight").max() + 
                this._getBreadthMargin() +
                this.getVMarginBorder()
            ); 
            this.restore();
        }
    },

    minimize : function () {
        if (this.minimized) return;
            
        this.setMinHeight(0);
        
        this._restoreHeight = this.getHeight();
        this._restoreUserHeight = this._userHeight;

        this.setHeight(
            this.members.map("getHeight").max() + 
            this._getBreadthMargin() +
            this.getVMarginBorder()
        );

        this.minimized = true;
    },

    restore : function () {
        if (!this.minimized) return;

        this.setHeight(this._restoreHeight);
        this._userHeight = this._restoreUserHeight;

        delete this._restoreHeight;
        delete this._restoreUserHeight;

        this.minimized = false;
    },

    // We always reflow the PortalLayout when reflowing a row, because the layout may need
    // to resize a column. This could possibly be optimized so that we don't need to reflow
    // the PortalLayout every time.
    reflow : function () {
        if (this.portalLayout) this.portalLayout.reflow("portalRow reflowed");
        this.Super("reflow", arguments);
    },

    // The sizing policy we want is a bit special. We want to manage percent sizes (or * sizes)
    // like a Layout, but we want to be able to force overflow (though minWidth or simply
    // manually setting widths) like a stack. The easiest way seems to be to manipulate
    // getTotalMemberSpace to ensure that we're big enough. We implement _getDesiredMemberSpace
    // separately so that the PortalColumn and PortalLayout can access it.
    _getDesiredMemberSpace : function () {
        return this.members.map(function (member) {
            if (isc.isA.Number(member._userWidth)) {
                return Math.max(member._userWidth, member.minWidth);
            } else {
                // If the _userWidth is a percentage, then we reserve enough room
                // for its minWidth. That way, we'll always have enough room to
                // let the percentage resolve to the minWidth, but the percentage
                // can resolve to more if more space is available.
                return member.minWidth;
            }
        }).sum();
    },
      
    // Returns the part of the width not accounted for in desiredMemberSpace or totalMemberSpace
    _getWidthOverhead : function () {
        return this.getMarginSpace() + this.getHMarginBorder();
    },

    getTotalMemberSpace : function () {
        var normalMemberSpace = this.Super("getTotalMemberSpace", arguments);
        var desiredMemberSpace = this._getDesiredMemberSpace();
        if (normalMemberSpace < desiredMemberSpace) {
            return desiredMemberSpace;
        } else {
            // If we have enough member space, return what the parent would give us,
            // so we'll fill it.
            return normalMemberSpace;
        }
    },

    // Called when the PortalColumn is resized to be smaller ... force each
    // row's width to be smaller if necessary, by shrinking portlets from the
    // right, respecting only the portlet's minWidth.
    _applyMaxWidth : function (width) {
        var length = this.members.length;
        if (length == 0) return;
        
        var excessWidth = this._getDesiredMemberSpace() + this._getWidthOverhead() - width;
        if (excessWidth <= 0) return;
        
        // Iterate backwards through the members while we still have excessWidth to remove
        for (var i = length - 1; i >= 0 && excessWidth > 0; i--) {
            var portlet = this.getMember(i);
            var currentWidth = portlet.getWidth();
            var subtract = Math.min(currentWidth - portlet.minWidth, excessWidth);
            if (subtract > 0) {
                portlet.setWidth(currentWidth - subtract);
                excessWidth -= subtract; 
            }
        }
    },
    
    membersChanged : function () {
        // Ignore if we're just swapping placeholders in and out
        if (this.portletMaximizing) return;

        if (this.members.length == 0) {
            //>EditMode
            if (this.editContext && this.editNode) this.editContext.removeNode(this.editNode);
            //<EditMode
            this.destroy();
        } else {
            this._checkPortletHeights();
            this._updatePortletResizeFrom();
        }
        
        if (this.portalLayout && this.portalLayout.portletsChanged) this.portalLayout.portletsChanged();

        // No need to invoke Super - membersChanged is undefined by default
    },

    // Apply portlet logic when adding portlets as members. This allows smoother interaction
    // with existing drag and drop code in Layout.js, since that code ultimately calls
    // addMembers and removeMembers
    addMembers : function (portlets, index) {
        // Don't do anything special if we're just swapping placeholders in and out
        if (this.portletMaximizing) return this.Super("addMembers", arguments);

        if (!isc.isAn.Array(portlets)) portlets = [portlets];

        var self = this;
        portlets.map(function (portlet) {
            // Don't deal with portlet placeholders here
            if (portlet._portlet) return;

            // Apply canResizePortlets
            portlet.canDragResize = self.canResizePortlets;
            if (portlet._resizeBar) portlet._resizeBar.canDragResize = self.canResizePortlets;

            // Check if the portlet has a specified height
            var userHeight = portlet._userHeight;
            if (userHeight && !portlet.minimized) {
                // The portlet's height should always be 100%, but we specify null here
                // because otherwise this sets _userHeight to 100% as well, and that 
                // will get picked up the *next* time we move the portlet.
                portlet._userHeight = null;
                portlet._percent_height = null;

                // If there is no explicit rowHeight, then use the userHeight
                if (!portlet.rowHeight) portlet.rowHeight = userHeight;
            }
            
            // Apply the rowHeight if specified
            if (portlet.rowHeight) {
                // We only apply the rowHeight when adding a portlet if the row's height has
                // not already been explicity indicated
                if (!self._userHeight) {
                    self.setHeight(portlet.rowHeight);
                    // This is needed if the row is still being initialized ... otherwise,
                    // the height gets reset later.
                    self._userHeight = isc.NumberUtil.parseIfNumeric(portlet.rowHeight);
                }
            }
        });

        this.Super("addMembers", arguments);

        // Need to do this after Super, since the addMembers may imply a removeMembers
        // which would set portalRow to null
        portlets.map(function (portlet) {
            // Add a reference back, since a maximized portlet cannot get to us
            // via parentElement
            portlet.portalRow = self;

            // If we just added minimized portlets, and they are the only
            // portlets, then we just minimized ourselves, but we don't have a
            // sensible height to return to. So, pick that up from the portlet.
            if (self.minimized && (self.members.length == portlets.length)) {
                self._restoreHeight = portlet._restoreRowHeight;
                self._restoreUserHeight = portlet._restoreRowUserHeight;

                delete portlet._restoreRowHeight;
                delete portlet._restoreRowUserHeight;
            }
        });

        //>EditMode
        // If we have an editContext and we aren't coming from addPortlets, then we check
        // whether the portlets have an editNode ... if so, we should add it
        if (this.editContext && !this._addingPortlets) {
            for (var i = 0; i < portlets.length; i++) {
                var portlet = portlets[i];
                if (portlet.editNode) {
                    this.editContext.addNode(portlet.editNode, this.editNode, index + i, null, true);
                }
            }
        }
        //<EditMode
    },

    addPortlets : function (portlets, index) {
        //>EditMode
        // We keep track of whether we're calling addMembers from here in order to know whether
        // to check for editNodes. The assumption is that if we're calling from here, we have
        // already dealt with editNodes appropriately.
        //<EditMode
        this._addingPortlets = true;
        this.addMembers(portlets, index);
        delete this._addingPortlets;
    },

    addPortlet : function (portlet, index) {
        this.addPortlets(portlet, index);
    },

    // Catch removeMembers which occurs via drag & drop
    removeMembers : function (portlets) {
        this.Super("removeMembers", arguments);
        
        if (!this.portletMaximizing) {
            if (!isc.isAn.Array(portlets)) portlets = [portlets];

            var self = this;
            portlets.map(function (portlet) {
                if (portlet.portalRow) portlet.portalRow = null;
          
                if (portlet.minimized) {
                    // If the portlet is minimized, then we need to tag it with
                    // our _restoreHeight. That way, if it's dropped in another
                    // column, we can know what size to make the row we
                    // auto-create.
                    portlet._restoreRowHeight = self._restoreHeight;
                    portlet._restoreRowUserHeight = self._restoreUserHeight;
                }

                //>EditMode
                // If we have an editContext, then we check whether we got here via
                // removePortlets. If we did, then we assume that the code has done
                // the right thing re: editContext. If not, then we're probably doing
                // a drag & drop from Layout.js, so we should remove the component
                if (self.editContext && portlet.editNode && !self._removingPortlets) {
                    // Note that we skip live removal, since we'll have just done that
                    self.editContext.removeNode(portlet.editNode, true);
                }
                //<EditMode
            });
        }
    },

    removePortlets : function (portlets) {
        if (!isc.isAn.Array(portlets)) portlets = [portlets];

        var self = this;
        portlets.map(function (portlet) {
            var placeholder = portlet._portletPlaceholder;
            if (placeholder) {
                self.removeMembers(placeholder);
                delete placeholder._portlet;
                delete portlet._portletPlaceholder;
                placeholder.destroy();
                portlet.deparent();
                portlet.clear();
                portlet.portalRow = null;
            } else {
                //>EditMode
                // We keep track of whether we are calling removeMembers from here in order
                // to know whether to check for editNodes. The assumption is that if we are
                // calling from here, we have already done the appropriate thing with any
                // editNodes.
                //<EditMode
                self._removingPortlets = true;
                self.removeMembers(portlet);
                delete self._removingPortlets;
            }
        });
    },

    removePortlet : function (portlet) {
        this.removePortlets(portlet);
    },

    getPortlet : function (id) {
        return this.getMember(id);
    },

    getPortlets : function () {
        return this.getMembers().map(function (member) {
            // If the member has a _portlet, then it is really a placeholder and we
            // return the _portlet
            if (member._portlet) {
                return member._portlet;
            } else {
                return member;
            }
        });
    },

    getPortalPosition : function (portlet) {
        var position = this.getPortlets().indexOf(portlet);
        if (position < 0) {
            // Return null if it wasn't found
            return null;
        } else {
            // Otherwise, start building up the PortalPosition object ... the callers
            // will supply what they know.
            return {position: position};
        }
    }
});

isc.PortalRow.addClassMethods({
    // Check for the case where all the sizes are pixels. In that case, we won't be able to fill
    // the totalSize, because there won't be anything to stretch. If so, make the last size a *, so
    // it will get the space. The net effect is a bit of a cross between a Layout and a Stack, since
    // you can resize to more than the totalSize, but not less.
    applyStretchResizePolicy : function (sizes, totalSize, minSize, modifyInPlace, propertyTarget) {
        if (propertyTarget.portalLayout && propertyTarget.portalLayout.preventRowUnderflow) {
            if (sizes && sizes.length > 0) {
                var allNumeric = sizes.map(function (size) {
                    return isc.isA.Number(size);
                }).and();
                
                if (allNumeric) {
                    var totalNumericSizes = sizes.sum();
                    if (totalNumericSizes < totalSize) {
                        sizes[sizes.length - 1] = "*";
                    }
                }
            }
        }

        return this.Super("applyStretchResizePolicy", arguments);
    }
});

// A vertical layout rendered within a PortalColumn, containing the PortalRows.
// A separate object for the PortalColumnBody avoids having to adjust for whether the
// PortalColumn's columnHeader is showing.
// Note that you can drop just about anything on a PortalColumnBody -- it will be wrapped
// with a Portlet and a PortalRow if necessary.
isc.defineClass("PortalColumnBody", "Layout").addProperties({
    vertical: true,
    layoutMargin: 3,
    
    resizeBarClass: "PortalResizeBar",
    resizeBarSize: 3,
    defaultResizeBars: "middle",
    
    canAcceptDrop: true,
    canDrag: false,
    dropLineThickness: 2,
    dropLineProperties: {backgroundColor: "blue"},
    width: "100%",

    // Handles resizes from the edge of the column, where there are no resizeBars.
    canDragResize: true,
    dragTarget: "parent",
    edgeCursorMap: {
        "L": "col-resize",
        "R": "col-resize"
    },

    // When creating resizeBars for rows, intialize canDragResize
    createResizeBar : function () {
        var resizeBar = this.Super("createResizeBar", arguments);
        resizeBar.canDragResize = this.creator.canResizePortlets;
        return resizeBar;
    },
    
    // To apply the minHeight of rows. 
    respectSizeLimits: true,

    membersChanged : function () {
        this.members.map(function (row) {
            row._updatePortletResizeFrom();
        });
    },

    // The sizing policy we want is a bit special. We want to manage percent sizes (or * sizes)
    // like a Layout, but we want to be able to force overflow (though minHeight or simply
    // manually setting heights) like a stack. The easiest way seems to be to manipulate
    // getTotalMemberSpace to ensure that we're big enough.
    getTotalMemberSpace : function () {
        var normalMemberSpace = this.Super("getTotalMemberSpace", arguments);
        var specialMemberSpace = this.members.map(function (row) {
            if (isc.isA.Number(row._userHeight)) {
                return Math.max(row._userHeight, row.minHeight);
            } else {
                // If the _userHeight is a percentage, we reserve enough space for the minHeight.
                // That way, there will always be enough space for the percentage to resolve
                // to the minHeight -- if more space is available, it can resolve to more.
                return row.minHeight;
            }
        }).sum();
        return Math.max(normalMemberSpace, specialMemberSpace);
    },

    // We pass through the drop if it is a PortalColumn, since it doesn't make sense to drop
    // a PortalColumn on another PortalColumn -- the PortalLayout will handle it.
    isPortalColumnDrop : function () {
        var dragTarget = this.ns.EH.dragTarget;
        var type = dragTarget.getDragType();
        if (type == "PortalColumn") return true;

        //>EditMode
        if (dragTarget.isA("Palette")) {
            var data = dragTarget.getDragData(),
                component = (isc.isAn.Array(data) ? data[0] : data);
            if (component.type == "PortalColumn" || component.className == "PortalColumn") return true;
        }
        //<EditMode

        return false;
    },

    willAcceptDrop : function () {
        // If this is a portalColumn drop, then the portalLayout should be the target,
        // so let it bubble
        if (this.isPortalColumnDrop()) return null;

        // By default, portalLayout.willAcceptPortletDrop just calls back to our
        // superclass. But subclasses of portalLayout could do something different.
        return this.creator.portalLayout.willAcceptPortletDrop(
            this.ns.EH.dragTarget,
            this.creator.portalLayout.getPortalColumnNumber(this.creator),
            this.getDropPosition(),
            null
        );
    },

    // We can share dropMove and dropOver with PortalRow, since they need to do the same thing here
    dropMove : isc.PortalRow.getInstanceProperty("dropMove"),
    dropOver : isc.PortalRow.getInstanceProperty("dropOver"),

    getDropComponent : function (dragTarget, dropPosition) {
        var dropComponent = this.creator.portalLayout.getDropPortlet(
            dragTarget,
            this.creator.portalLayout.getPortalColumnNumber(this.creator),
            dropPosition,
            null
        );
       
        //>EditMode
        if (this.handleDroppedEditNode) dropComponent = this.handleDroppedEditNode(dropComponent, dropPosition);
        //<EditMode

        if (dropComponent) {
            // Check if the dropComponent is a Portlet (or subclass) ... if not, wrap it in one.
            if (!isc.isA.Portlet(dropComponent)) {
                dropComponent = isc.Portlet.create({
                    autoDraw: false,
                    title: "",
                    items: dropComponent,
                    destroyOnClose: true
                });
            }
            
            // We need to check whether the dropComponent is already the only portlet
            // in an existing row. If so, we can simplify by just dropping
            // the row -- that is what the user will have meant. 
            var currentRow = dropComponent.portalRow;
            if (currentRow && currentRow.parentElement == this && currentRow.getMembers().length == 1) {
                return currentRow;
            } else {
                this.creator.addPortlet(dropComponent, dropPosition);
                
                // Cancel the drop, since we've handled it.
                return false;
            }
        } else {
            // Return the dropComponent if falsy, so we can distinguish between false (cancel bubbling)
            // and null (continue bubbling)
            return dropComponent;
        }
    } 
});

isc.PortalColumnBody.addClassMethods({
    // Check for the case where all the sizes are pixels. In that case, we won't be able to fill
    // the totalSize, because there won't be anything to stretch. If so, make the last size a *, so
    // it will get the space. The net effect is a bit of a cross between a Layout and a Stack, since
    // you can resize to more than the totalSize, but not less.
    applyStretchResizePolicy : function (sizes, totalSize, minSize, modifyInPlace, propertyTarget) {
        var portalLayout = propertyTarget.creator.portalLayout;
        if (portalLayout && portalLayout.preventColumnUnderflow) {
            if (sizes && sizes.length > 0) {
                var allNumeric = sizes.map(function (size) {
                    return isc.isA.Number(size);
                }).and();
                
                if (allNumeric) {
                    var totalNumericSizes = sizes.sum();
                    if (totalNumericSizes < totalSize) {
                        // Give the last non-minmized portal row the extra space ...
                        var lastNonMinimizedRow = propertyTarget.members.findNextIndex(
                            sizes.length - 1, function (row) {
                                return !row.minimized;
                            }, true, 0
                        );

                        if (lastNonMinimizedRow != -1) {
                            sizes[lastNonMinimizedRow] = "*";
                        }
                    }
                }
            }
        }

        return this.Super("applyStretchResizePolicy", arguments);
    }
});


// Vertical layout based container rendered within a PortalLayout.
// PortalColumns are automatically constructed by the PortalLayout class and will not typically
// be directly instantiated.
//
// The only reason to expose this would be to allow customization of appearance - and it makes
// more sense to do that via attributes on the PortalLayout itself.
isc.defineClass("PortalColumn", "Layout").addProperties({
    vertical:true,
    minWidth: 80,

    // This makes VisualBuilder generate code inline for PortalColumns, rather than
    // creating standalone.
    _generated: true,

    // Can drag the PortalColumn, but it does not handle drops ... the PortalColumnBody
    // manages that.
    dragAppearance: "outline",
    canAcceptDrop: false,
    canDrop: true,
    dragType: "PortalColumn",
    
    // Handles a drag initiated right on the border, which won't be picked up elsewhere
    canDrag: false,
    canDragResize: true,
    edgeCursorMap: {
        "L": "col-resize",
        "R": "col-resize"
    },
    
    // The columnHeader is handled as an AutoChild
    showColumnHeader: true,
    columnHeaderConstructor: "PortalColumnHeader",
    columnHeaderDefaults: {
        title: "Column"
    },

    // Make showColumnHeader updatable
    setShowColumnHeader : function (show) {
        if (show) {
            if (this.showColumnHeader) return;
            this.showColumnHeader = show;
            this.addAutoChild("columnHeader", {autoParent: "none"});
            this.addMember(this.columnHeader, 0);
        } else {
            if (!this.showColumnHeader) return;
            this.showColumnHeader = show;
            this.removeMember(this.columnHeader);
            this.columnHeader = null;
        }
    },

    // The rowLayout is where the actual rows go ... this avoids having to
    // continually adjust the code for whether the columnHeader is
    // showing or not.
    //
    // These Autochild settings are referenced in PortalLayout as well, since that is where they
    // are exposed as an API. Changing rowLayoutDefaults there will also change it
    // here, but that is fine, since people should be using changeDefaults() anyway. The 
    // settings are then copied dynamically from PortalLayout when it creates columns.
    rowLayoutDefaults: {
        _constructor: "PortalColumnBody"
    },

    // Will accept a portalRows attribute, containing portalRows to insert,
    // or properties to be used to construct them.
    initWidget : function () {
        this.Super("initWidget", arguments);
        this.addAutoChild("columnHeader");
        this.addAutoChild("rowLayout");

        // Only apply portletDropTypes if it was supplied. Otherwise, we would clobber the
        // autoChild settings for dropTypes (if any) -- we only do that if portletDropTypes
        // was specified.
        if (this.portletDropTypes) this.rowLayout.dropTypes = this.portletDropTypes;

        if (this.portalRows) this.addPortalRows(this.portalRows);
        this.portalRows = null;
    },

    // This computes the width that we want, given the _userWidth of all of our Portlets.
    // PortalLayout.getTotalMemberSpace and PortalLayout.applyStretchResizePolicy use
    // this as part of the sizing process.
    _getDesiredWidth : function () {
        var rows = this.getPortalRows();
        if (rows.length == 0) {
            return Math.max(this.minWidth, (this.columnHeader ? this.columnHeader.minWidth : 0));
        } else {
            var desiredWidth = rows.map(function (row) {
                return row._getDesiredMemberSpace() + row._getWidthOverhead();
            }).max() + this._getWidthOverhead();

            return Math.max(desiredWidth, this.minWidth, (this.columnHeader ? this.columnHeader.minWidth : 0));
        }
    },

    // Returns the width overhead that a PortalRow doesn't know about -- that is, the width
    // added at the PortalColumnBody and PortalColumn levels.
    _getWidthOverhead : function () {
        var overhead = this._getBreadthMargin() +
                       this.getHMarginBorder() +
                       this.rowLayout._getBreadthMargin() +
                       this.rowLayout.getHMarginBorder();

        if (this.rowLayout.vscrollOn) overhead += this.rowLayout.getScrollbarSize();

        return overhead;
    },

    prepareForDragging : function () {
        var EH = this.ns.EH;

        if (EH.dragTarget) {
            // If someone has already claimed a dragTarget, check whether it is us ... if
            // so, we'll continue on to the adjustment below. Otherwise, not.
            if (EH.dragTarget != this) return;
        } else {
            // If no one has claimed a dragTarget, then do the standard checks
            this.Super("prepareForDragging", arguments);
        }

        // Then, make some adjustments if we are the target and we're dragging the left edge
        if (EH.dragTarget == this && EH.dragOperation == EH.DRAG_RESIZE && EH.resizeEdge == "L") {
            var index = this.portalLayout.getMemberNumber(this);
            if (index > 0) {
                // If we're resizing from the left edge, and there is a previous column
                // then switch to it, and switch edges -- this makes the layout reflow
                // in a way that makes more sense to the user.
                EH.dragTarget = this.portalLayout.getMember(index - 1);
                EH.resizeEdge = "R";
            } else {
                // If we're resizing from the left edge, and there is no previous column,
                // then cancel.
                EH.dragTarget = null;
            }
        } 
    },

    dragResized : function (deltaX, deltaY) {
        if (deltaX < 0) {
            // If we're drag-resized to be smaller, then adjust any rows which
            // would otherwise force us to be bigger.
            var maxRowWidth = this.getWidth() - this._getWidthOverhead();
            this.getPortalRows().map(function (row) {
                row._applyMaxWidth(maxRowWidth);
            });
        }

        this.Super("dragResized", arguments);
    },

    resized : function (deltaX, deltaY, reason) {
        this.Super("resized", arguments); 
        
        // Only catch width changes, since height is managed
        if (!deltaX) return;

        if (this.portalLayout) this.portalLayout._portletsResized();
    },

    addNewColumn : function () {
        this.portalLayout.addColumnAfter(this);
    },

    removeSelf : function () {
        this.portalLayout.removeColumn(this.portalLayout.getMemberNumber(this));
    },

    // See comment on rowLayoutDefaults re: the reference in PortalLayout
    rowConstructor: "PortalRow",
    
    // Creates rows via AutoChild logic, or modifies existing rows
    // to prepare them to be added to the column. Note that rowConstructor,
    // rowDefaults and rowProperties will have been copied from the portalLayout.
    makePortalRow : function (props) {
        if (props == null) props = {};
        
        var dynamicProperties = {
            portalLayout: this.portalLayout,
            portalColumn: this,
            canResizePortlets: this.canResizePortlets
        };
        
        // Only supply portletDropTypes if it was specified. This avoids clobbering
        // the autoChild settings for dropTypes -- we only clobber them if portletDropTypes
        // was specified.
        if (this.portletDropTypes) dynamicProperties.dropTypes = this.portletDropTypes;

        var portalRow;
        if (isc.isA.PortalRow(props)) {
            // If we're given an already created PortalRow, then use setProperties
            // to add the dynamicProperties.
            props.setProperties(dynamicProperties);
            portalRow = props;
        } else {
            // Otherwise, construct it as an autoChild
            isc.addProperties(props, dynamicProperties);
            portalRow = this.createAutoChild("row", props);
        }
        return portalRow;
    },

    setCanResizePortlets : function (canResize) {
        this.canResizePortlets = canResize;
        this.getPortalRows().map(function (row) {
            row.setCanResizePortlets(canResize);
        });
    },

    setPortletDropTypes : function (portletDropTypes) {
        this.portletDropTypes = portletDropTypes;
        this.rowLayout.dropTypes = portletDropTypes;
        this.getPortalRows().map(function (row) {
            row.dropTypes = portletDropTypes;
        });
    },

    addPortalRows : function (rows, position) {
        if (!isc.isAn.Array(rows)) rows = [rows];
        var self = this;
        rows = rows.map(function (row) {
            return self.makePortalRow(row);
        });
        this.rowLayout.addMembers(rows, position);
    },

    addPortalRow : function (row, position) {
        this.addPortalRows(row, position);
    },

    removePortalRows : function (rows) {
        this.rowLayout.removeMembers(rows);
    },

    removePortalRow : function (row) {
        this.removePortalRows(row);
    },

    getPortalRows : function () {
        return this.rowLayout.getMembers();
    },

    getPortalRowNumber : function (id) {
        return this.rowLayout.getMemberNumber(id);
    },

    getPortalRow : function (rowID) {
        return this.rowLayout.getMember(rowID);
    },

    // Returns flat list of portlets
    getPortlets : function () {
        var portlets = [];
        this.getPortalRows().map(function (row) {
            portlets.addList(row.getPortlets());
        });
        return portlets;
    },

    // Returns portlets in array of arrays, corresponding to rows
    getPortletArray : function () {
        return this.getPortalRows().map(function (row) {
            return row.getPortlets();
        });
    },
    
    getPortalPosition : function (portlet) {
        var rows = this.getPortalRows();

        for (var rowNum = 0; rowNum < rows.length; rowNum++) {
            // Ask each row to get the portal position. If it's found,
            // add the rowNum ... otherwise, return null.
            var position = rows[rowNum].getPortalPosition(portlet);
            if (position) {
                position.rowNum = rowNum;
                return position;
            }
        }

        return null;
    },

    getPortlet : function (id) {
        var rows = this.getPortalRows();
        for (var x = 0; x < rows.length; x++) {
            var portlet = rows[x].getPortlet(id);
            if (portlet) return portlet;
        }
        return null;
    },

    // Adds portlets, auto-wrapping them in rows
    addPortlets : function (portlets, position) {        
        if (!isc.isAn.Array(portlets)) portlets = [portlets];

        var self = this;
        var rows = portlets.map(function(portlet) {
            return self.makePortalRow({
                portlets: portlet
            });
        });

        this.addPortalRows(rows, position);
    },

    addPortlet : function (portlet, position) {
        this.addPortlets(portlet, position);
    },

    addPortletToExistingRow : function (portlet, rowNum, rowOffset) {
        var rows = this.rowLayout.getMembers();

        if (rows == null || rows.length <= rowNum) {
            if (this.editContext && this.editNode && portlet.editNode) {
                this.editContext.addNode(portlet.editNode, this.editNode, rows.length); 
            } else {
                this.addPortlet(portlet, rows.length);
            }
        } else {
            var portalRow = this.rowLayout.getMember(rowNum);
            if (portalRow.editContext && portalRow.editNode && portlet.editNode) {
                portalRow.editContext.addNode(portlet.editNode, portalRow.editNode, rowOffset);
            } else {
                portalRow.addPortlets(portlet, rowOffset);
            }
        }
    }
});


//>	@class	PortalLayout
// A PortalLayout is a special subclass of Layout designed to display +link{Portlet} windows.
// A PortalLayout displays Portlets in columns and supports drag-drop interaction for moving 
// Portlets around within the PortalLayout. Portlets may be drag-reordered within columns, dragged
// into other columns, or dragged next to other Portlets to sit next to them horizontally
// within a column. 
//
// @visibility external
// @treeLocation Client Reference/Layout
//<
isc.defineClass("PortalLayout", "Layout").addProperties({
    vertical:false,

    //> @attr portalLayout.overflow (Overflow : "auto" : IRW)
    // 
    // Controls how the PortalLayout reacts when column widths or +link{Portlet} widths
    // overflow the width of the PortalLayout. By default, the PortalLayout scrolls
    // when necessary. You can also use overflow: visible or overflow: hidden, with the 
    // usual results -- see +link{canResizePortlets} for a further explanation of column widths.
    // <p>
    // Note that overflowing height is also affected by +link{columnOverflow,columnOverflow}.
    // By default, each column will scroll individually -- you can change
    // columnOverflow to "auto" to scroll the whole PortalLayout instead.
    //
    // @see canResizePortlets
    // @see columnOverflow
    // @see Canvas.overflow
    // @example portalLayoutColumnHeight
    // @group sizing
    // @visibility external
    //<
    overflow: isc.Canvas.AUTO,
    
    // Vertically, it seems to make most sense to scroll each column individually (when
    // required). Imagine a left-hand column with a tall Portlet and several others
    // (forcing scrolling), and a right-hand column with a single Portlet. If the scrolling
    // were at the PortalLayout level, then the right-hand column would be enlarged and the
    // single Portlet would have to scroll -- which means that it would not be entirely visible.
    // That seems undesirable, so (by default) we scroll each column vertically, rather than the whole
    // PortalLayout. However, this is configurable here.
    
    //> @attr portalLayout.columnOverflow (Overflow : "auto" : IRWA)
    //
    // Controls the +link{Canvas.overflow,overflow} setting for each column. If set to "auto" (the
    // default) then each column will scroll individually (if its +link{Portlet,Portlets} overflow
    // the column height). You can also use "hidden" to clip overflowing heights, or "visible" to
    // show the overflow. The effect of "visible" will depend on the setting for +link{PortalLayout.overflow}
    // -- by default, the PortalLayout as a whole will scroll when necessary.
    //
    // @see overflow
    // @see Canvas.overflow
    // @example portalLayoutColumnHeight
    // @group sizing
    // @visibility external
    //<
    columnOverflow: isc.Canvas.AUTO,

    //> @method portalLayout.setColumnOverflow()
    // Sets +link{columnOverflow} and updates existing columns to reflect the new setting.
    // @param overflow (Overflow) Overflow setting for columns
    // @see columnOverflow
    // @example portalLayoutColumnHeight
    // @visibility external
    //<
    setColumnOverflow : function (overflow) {
        this.columnOverflow = overflow;
        this.rowLayoutDefaults.overflow = overflow;
        this.getPortalColumns().map(function (column) {
            column.rowLayout.setOverflow(overflow);
        });
    },

    // Horizontally, we could scroll each row individually, each column, or the PortalLayout
    // as a whole. Permitting all three is undesirable, as in the worst-case scenario one
    // would end up with three horizontal scroll-bars adjacent to each other (one for the row,
    // one for the column and one for the PortalLayout as a whole -- I've seen it, and it's
    // not pretty). 
    //
    // Now, we're ultimately going to have to allow for the PortalLayout as a whole to scroll
    // horizontally, because the user can keep adding columns until the PortalLayout overflows.
    // So, it seems simplest to arrange things so that it is only the PortalLayout as a whole
    // that ever scrolls horizontally -- that way, we only ever have the one set of scrollbars.
    //
    // To accomplish this, by default, the portalLayout.overflow property is "auto" by default,
    // so we'll scroll the whole layout if necessary.
    //
    // To ensure that we don't scroll the rows horizontally, we turn on portalLayout.canStretchColumnWidths 
    // by default. This stretchs a column to accomodate the _userWidth specified for its
    // Portlets. However, this does not affect the _userWidth of the column itself, so
    // the column "remembers" its intrinsic width, and will snap back to it when it no
    // longer needs to be stretched (e.g. when you drag a wide Portlet to a different column).
    //
    // If one column needs to stretch, and portalLayout.canShrinkColumnWidths is set, then 
    // portalLayout.applyStretchResizePolicy will check
    // to see if another PortalColumn can shrink -- that is, whether its intrinsic width
    // is wider than the width of its Portlets requires. If so, shrinking the other column
    // will avoid forcing the PortalLayout as a whole to scroll, which is desirable.
    //
    // If canStretchColumnWidths is off, then the overflow for the rows is set to "auto", since
    // individual rows may need to scroll if the column cannot stretch.
    
    //> @attr portalLayout.canStretchColumnWidths (Boolean : true : IRWA)
    //
    // Controls whether the PortalLayout will stretch column widths, if needed to accommodate the
    // width of +link{Portlet,Portlets}. If set, columns will overflow their widths in order to accomodate 
    // the widths of their Portlets.
    // <p>
    // With the default setting of +link{overflow}: auto, the PortalLayout as a whole will scroll 
    // horizontally if needed. Depending on the setting of +link{canShrinkColumnWidths,canShrinkColumnWidths}, 
    // other columns may shrink to avoid overflow on the PortalLayout as a whole.
    // <p>
    // If <code>canStretchColumnWidths</code> is turned off, then individual rows will scroll
    // horizontally in order to accommodate Portlets that are wider than their column width allows.
    //
    // @see canShrinkColumnWidths
    // @see canResizePortlets
    // @see overflow
    // @group sizing
    // @visibility external
    //<
    canStretchColumnWidths: true,

    //> @method portalLayout.setCanStretchColumnWidths()
    // Sets +link{canStretchColumnWidths} and reflows to reflect the new setting.
    // @param canStretch (boolean) Whether columns can stretch to accommodate +link{Portlet} widths.
    // @see canStretchColumnWidths
    // @see canShrinkColumnWidths
    // @group sizing
    // @visibility external
    //<
    setCanStretchColumnWidths : function (canStretch) {
        this.canStretchColumnWidths = canStretch;
        this.reflow("canStretchColumnWidths changed");
    },

    //> @attr portalLayout.canShrinkColumnWidths (Boolean : true : IRWA)
    //
    // Controls whether the PortalLayout will shrink column widths to avoid overflowing the PortalLayout 
    // horizontally. If the PortalLayout would otherwise overflow its width, it will check each column
    // to see whether it is wider than necessary to accommodate its +link{Portlet,Portlets}. If so,
    // the column may shrink to avoid having to scroll the PortalLayout.
    //
    // @see canStretchColumnWidths
    // @group sizing
    // @visibility external
    //<
    canShrinkColumnWidths: true,

    //> @method portalLayout.setCanShrinkColumnWidths()
    // Sets +link{canShrinkColumnWidths} and reflows to reflect the new setting.
    // @param canShrink (boolean) Whether columns can shrink to avoid overflowing the PortalLayout's width.
    // @see canShrinkColumnWidths
    // @see canStretchColumnWidths
    // @group sizing
    // @visibility external
    //<
    setCanShrinkColumnWidths : function (canShrink) {
        this.canShrinkColumnWidths = canShrink;
        this.reflow("canShrinkColumnWidths changed");
    },

    //> @attr portalLayout.stretchColumnWidthsProportionally (Boolean : false : IRWA)
    //
    // When +link{canStretchColumnWidths,stretching column widths}, should we stretch all column widths proportionally,
    // or just stretch the columns that need extra width?
    // <p>
    // Note that this implies turning off +link{canShrinkColumnWidths,canShrinkColumnWidths}.
    //
    // @see canStretchColumnWidths
    // @see canShrinkColumnWidths
    // @group sizing
    // @visibility external
    //<

    //> @method portalLayout.setStretchColumnWidthsProportionally()
    // Sets +link{stretchColumnWidthsProportionally} and reflows to reflect the new setting.
    // @param stretchProportionally (boolean) Whether to stretch column widths proportionally
    // @see stretchColumnWidthsProportionally
    // @group sizing
    // @visibility external
    //<
    setStretchColumnWidthsProportionally : function (stretchProportionally) {
        this.stretchColumnWidthsProportionally = stretchProportionally;
        this.reflow("stretchColumnWidthsProportionally changed");
    },

    //> @attr portalLayout.preventUnderflow (Boolean : true : IRW)
    //
    // Controls whether the last column will be stretched to fill the PortalLayout's width,
    // or left at its specified width.
    //
    // @group sizing
    // @example portalColumnWidth
    // @visibility external
    //<
    preventUnderflow: true,

    //> @method portalLayout.setPreventUnderflow()
    // Sets +link{preventUnderflow,preventUnderflow} and reflows the layout to implement it.
    // @param preventUnderflow (boolean) Whether to stretch the last column to fill the PortalLayout's width.
    // @group sizing
    // @example portalColumnWidth
    // @visibility external
    //<
    setPreventUnderflow : function (preventUnderflow) {
        if (this.preventUnderflow == preventUnderflow) return;
        this.preventUnderflow = preventUnderflow;
        this.reflow("preventUndeflow changed");
    },

    //> @attr portalLayout.preventColumnUnderflow (Boolean : true : IRW)
    //
    // Controls whether the last +link{Portlet} in a column will be stretched to fill the column's height,
    // or left at its specified height.
    //
    // @group sizing
    // @example portletHeight
    // @visibility external
    //<
    preventColumnUnderflow: true,

    //> @method portalLayout.setColumnPreventUnderflow()
    // Sets +link{preventColumnUnderflow,preventColumnUnderflow} and reflows the layout to implement it.
    // @param preventColumnUnderflow (boolean) Whether to stretch the last +link{Portlet} in a column to 
    // fill the column's height.
    // @group sizing
    // @example portletHeight
    // @visibility external
    //<
    setPreventColumnUnderflow : function (preventColumnUnderflow) {
        if (this.preventColumnUnderflow == preventColumnUnderflow) return;
        this.preventColumnUnderflow = preventColumnUnderflow;
        this.getPortalColumns().map(function (column) {
            column.rowLayout.reflow("preventColumnUnderflow changed");
        });
    },

    //> @attr portalLayout.preventRowUnderflow (Boolean : true : IRW)
    //
    // Controls whether the last +link{Portlet} in a row will be stretched to fill the row's width,
    // or left at its specified width.
    //
    // @group sizing
    // @example portletWidth
    // @visibility external
    //<
    preventRowUnderflow: true,

    //> @method portalLayout.setPreventRowUnderflow()
    // Sets +link{preventRowUnderflow,preventRowUnderflow} and reflows the layout to implement it.
    // @param preventRowUnderflow (boolean) Whether to stretch the last +link{Portlet} in a row to
    // to fill the row's width.
    // @group sizing
    // @example portletWidth
    // @visibility external
    //<
    setPreventRowUnderflow : function (preventRowUnderflow) {
        if (this.preventRowUnderflow == preventRowUnderflow) return;
        this.preventRowUnderflow = preventRowUnderflow;
        this.getPortalColumns().map(function (column) {
            column.getPortalRows().map(function (row) {
                row.reflow("preventRowUnderflow changed");
            });
        });
    },

    //> @attr portalLayout.portlets (Array of Portlet : null : I)
    // A convenience attribute which you can use to populate a PortalLayout with +link{Portlet,Portlets}
    // on initialization. After initialization, use +link{addPortlet(),addPortlet()} or drag-and-drop to add
    // Portlets, and +link{getPortlets(),getPortlets()} or +link{getPortletArray(),getPortletArray()}
    // to get Portlets.
    // <p>
    // To create one column, you can provide an array of Portlets.
    // <p>
    // To create multiple columns, provide an array of arrays (where the first level represents columns,
    // and the second represents Portlets).
    // <p>
    // To put multiple portlets in the same row, provide a third level to the array.
    // <p>
    // Note that +link{numColumns,numColumns} is ignored if you provide the portlets attribute, since
    // the array will indicate how many columns to create. You can provide an empty second-level
    // array to create a blank column, if needed.
    // @see getPortlets()
    // @see getPortletArray()
    // @see addPortlet()
    // @see numColumns
    // @example repositionPortlets
    // @visibility external
    //<

    //> @attr portalLayout.numColumns (int : 2 : IR)
    // Initial number of columns to show in this PortalLayout. Note that after initialization
    // columns should be added / removed via +link{addColumn()} and +link{removeColumn}.
    // numColumns is ignored if you initialize the +link{portlets} attribute, since the portlets
    // attribute will imply how many columns to create.
    // @see portlets
    // @getter getNumColumns
    // @visibility external
    //<
    //
    
    numColumns:2,
    
    //> @method portalLayout.getNumColumns()
    // Returns the current number of columns displayed in this PortalLayout.
    // @return numColumns (Integer)
    // @visibility external
    //<
    // Overridden to return this.getMembers.length. Will have been set up at initialization time.
    getNumColumns : function () {
        return this.getMembers().length;
    },
    
    //> @attr portalLayout.showColumnMenus (Boolean : true : IRW)
    // Should a menu be shown within each column with options to add / remove columns?
    // @example addRemovePortalColumn
    // @visibility external
    //<
    showColumnMenus:true,
    
    //> @method portalLayout.setShowColumnMenus()
    // Sets +link{showColumnMenus} and updates existing columns to reflect the new setting.
    // @param showMenus (boolean) Whether to show column menus
    // @visibility external
    //<
    setShowColumnMenus : function (show) {
        if (this.showColumnMenus == show) return;
        this.showColumnMenus = show;
        this.getPortalColumns().map(function (column) {
            column.setShowColumnHeader(show);
        });
        this.reflow("showColumnMenus changed");
    },
 
    //> @attr portalLayout.columnBorder (string : "1px solid gray" : IRW)
    // Border to show around columns in this PortalLayout
    // @visibility external
    //<
    columnBorder:"1px solid gray",
    
    
    //> @method portalLayout.setColumnBorder()
    // Sets the columnBorder for to the specified value and updates any drawn columns to reflect
    // this.
    // @param columnBorder (string) New border to show around columns
    // @visibility external
    //<
    setColumnBorder : function (columnBorder) {
        this.columnBorder = columnBorder;
        var members = this.members || [];
        for (var i = 0; i < members.length; i++) {
            members[i].setBorder(columnBorder);
        }
    },
    
    //> @attr portalLayout.canResizeColumns (Boolean : true : IRW)
    // Are columns in this PortalLayout drag-resizeable?
    // <p>
    // Note that the <u>displayed</u> width of a column will automatically shrink and stretch
    // to accomodate the width of +link{Portlet,Portlets} -- see +link{canStretchColumnWidths,canStretchColumnWidths}
    // and +link{canShrinkColumnWidths,canShrinkColumnWidths} for an explanation. 
    // This setting affects the <u>intrinsic</u> width of a column --
    // that is, the width it will try to return to when not necessary to stretch or shrink
    // to accomodate Portlet widths.
    // @see canStretchColumnWidths
    // @see canShrinkColumnWidths
    // @setter setCanResizeColumns()
    // @group sizing
    // @example portalColumnWidth
    // @visibility external
    //<
    canResizeColumns: true,

    //> @method portalLayout.setCanResizeColumns()
    // Set whether columns in this portalLayout are drag-resizable, and update any
    // drawn columns to reflect this.
    // @param canResize (Boolean) Whether columns are drag-resizable
    // @see canResizeColumns
    // @group sizing
    // @visibility external
    //<
    setCanResizeColumns : function (resizeColumns) {
        this.canResizeColumns = resizeColumns;
        
        // Also update the resizeFrom of columns, and portlets, since that
        // depends in part on canResizeColumns
        this._updateResizeFrom();
    },

    //>!BackCompat 2011.07.11 renamed canResizeRows to canResizePortlets
    //> @attr portalLayout.canResizeRows (Boolean : false : IRW)
    // Should vertical drag-resize of portlets within columns be allowed?
    // @deprecated Use +link{canResizePortlets,canResizePortlets} instead.
    // @visibility external
    //<
    
    //> @method portalLayout.setCanResizeRows()
    // Set whether vertical drag-resize of portlets within columns is allowed, and
    // update any drawn columns to reflect this.
    // @param canResize (Boolean) Whether drag-resize of portlets within columns is allowed
    // @deprecated Use +link{setCanResizePortlets(),setCanResizePortlets()} instead.
    // @visibility external
    //<
    setCanResizeRows : function (resizeRows) {
        this.setCanResizePortlets(resizeRows);
    },
    //<!BackCompat

    //> @attr portalLayout.canResizePortlets (Boolean : false : IRW)
    // Should the height and width of +link{Portlet,Portlets} be drag-resizable?
    // <p>
    // Note that changing the <b>height</b> of a Portlet will change the height of all
    // the Portlets in the same row to match.
    // <p>
    // If the height of Portlets causes a column to overflow, that column will scroll vertically
    // (independent of other columns), depending on the +link{columnOverflow,columnOverflow} setting.
    // <p>
    // Changing the <b>width</b> of a Portlet will potentially cause columns to stretch
    // and shrink their <u>displayed</u> widths in order to accomodate the Portlets,
    // depending on the value of +link{portalLayout.canStretchColumnWidths,canStretchColumnWidths} and 
    // +link{portalLayout.canShrinkColumnWidths,canShrinkColumnWidths}.
    // <p>
    // However, the <u>instrinsic</u> width of the columns will remain the same,
    // so that the columns will resume their former widths when no longer necessary 
    // to stretch or shrink to accomodate the widths of Portlets. 
    // To allow drag-resizing of the intrinsic width of columns, see +link{canResizeColumns,canResizeColumns}.
    // <p>
    // The net effect is that (by default) PortalLayouts behave like layouts when Portlet sizes do
    // not cause overflow, but behave more like stacks when overflow occurs.
    //
    // @setter setCanResizePortlets
    // @see canResizeColumns
    // @see canStretchColumnWidths
    // @see canShrinkColumnWidths
    // @see columnOverflow
    // @group sizing
    // @example resizingPortlets
    // @visibility external
    //<

    //> @method portalLayout.setCanResizePortlets()
    // Set whether the height and width of +link{Portlet,Portlets} should be drag-resizable, and
    // update any drawn Portlets to reflect this.
    // @param canResize (Boolean) Whether drag-resizing the height and width of portlets is allowed
    // @see canResizePortlets
    // @group sizing
    // @visibility external
    //<
    setCanResizePortlets : function (resizePortlets) {
        this.canResizePortlets = resizePortlets;
        this.getPortalColumns().map(function (column) {
            column.setCanResizePortlets(resizePortlets);
        });
    },

    //> @attr portalLayout.portletDropTypes (Array of String : null : IRW)
    // <p>The +link{Canvas.dropTypes,dropTypes} to be applied when dropping +link{Portlet,Portlets}
    // on this <code>PortalLayout</code>, or when dropping other components to be auto-wrapped in
    // a +link{Portlet}. If you set this, then you will need to set an equivalent +link{Canvas.dragType} on
    // anything to be dragged into this <code>PortalLayout</code> (including +link{Portlet,Portlets}).</p>
    //
    // <p>As a convenience, +link{Portlet.dragType} defaults to <code>"Portlet"</code>. Thus, if you want 
    // to allow +link{Portlet,Portlets} to be dropped on this <code>PortalLayout</code>, but disable 
    // auto-wrapping of other components, you can set <code>portletDropTypes</code> to 
    // <code>["Portlet"]</code>.</p>
    //
    // <p>If you want to allow some +link{Portlet,Portlets} to be dropped on this <code>PortalLayout</code> but 
    // not others, then set a custom +link{portlet.dragType,dragType} for the +link{Portlet,Portlets}, and 
    // set <code>portletDropTypes</code> to match.</p>
    //
    // <p>If you want to have different <code>dropTypes</code> for +link{row,rows} and 
    // +link{rowLayout,rowLayouts}, you can specify <code>dropType</code> on the +link{row,row} 
    // or +link{rowLayout,rowLayout} autochildren instead.</p>
    //
    // <p>For more control over what can be dropped, you can also implement 
    // +link{willAcceptPortletDrop(),willAcceptPortletDrop()}.
    //
    // @setter setPortletDropTypes
    // @see Canvas.dropTypes
    // @group dragdrop
    // @visibility external
    //<

    //> @method portalLayout.setPortletDropTypes()
    // <p>Sets the +link{portletDropTypes,portletDropTypes} to be applied when dropping +link{Portlet,Portlets}
    // on this <code>PortalLayout</code>, or when dropping other components to be auto-wrapped in
    // a +link{Portlet}.</p>
    //
    // @param portletDropTypes (Array of String) dropTypes to apply when dropping +link{Portlet,Portlets}
    // @see portletDropTypes
    // @group dragdrop
    // @visibility external
    //<
    setPortletDropTypes : function (portletDropTypes) {
        this.portletDropTypes = portletDropTypes;
        this.getPortalColumns().map(function (column) {
            column.setPortletDropTypes(portletDropTypes);
        });
    },

    //> @method portalLayout.willAcceptPortletDrop() [A]
    // <p>Returns true if the dragged +link{Portlet}, or other component, can be dropped onto
    // this <code>PortalLayout</code> (other components would be auto-wrapped in a <code>Portlet</code>).</p>
    //
    // <p>The default implementation acts like +link{Canvas.willAcceptDrop}, except applying
    // +link{portalLayout.portletDropTypes,portletDropTypes} rather than +link{portalLayout.dropTypes,dropTypes}.
    // You can subclass to apply other (or additional) criteria</p>
    //
    // @return	(boolean)	true if the +link{Portlet} or other component being dragged can be dropped on this PortalLayout,
    //                      false otherwise
    //
    // @param dragTarget (Canvas) The +link{Portlet}, or other component, being dragged
    // @param colNum (int) indicates which column the portlet would be dropped on.
    // @param rowNum (int) indicates the row number being dropped on.
    // @param [dropPosition] (int) Drop position within an existing row. If the dropPosition 
    //  is null, then that means that a new row will be created.
    // @see Canvas.dragType    
    // @see PortalLayout.portletDropTypes
    // @see portletsChanged()
    // @group dragdrop
    // @visibility external
    //<
    willAcceptPortletDrop : function(dragTarget, colNum, rowNum, dropPosition) {
        // By default, we call back to the Super of the dropTarget. But, we get
        // it by colNum and rowNum, since that is more useful information as an API for
        // possible sub-classes. (And, it is easier to implement the SmartGWT wrapper that way).
        var dropTarget = this.getPortalColumn(colNum).rowLayout;

        // If the dropPosition is specified, then the dropTarget is actually the row
        if (dropPosition != null) dropTarget = dropTarget.getMember(rowNum);

        // Call the super of dropTarget.willAcceptDrop, since it is willAcceptDrop itself 
        // which will have called us. Thus, by default, this is all a no-op ... it exists
        // only as an override point.
        return dropTarget.Super("willAcceptDrop", arguments);
    },

    // This allows drag/drop reordering within the portal layout
    canAcceptDrop: true,
    
    //> @attr portalLayout.dropTypes (Array of String : ["PortalColumn"] : IR)
    // <code>dropTypes</code> is set to <code>["PortalColumn"]</code>
    // in order to allow the dragging of columns within the <code>PortalLayout</code>. 
    // To control <code>dropTypes</code> when +link{Portlet,Portlets} or other components are dragged
    // into the <code>PortalLayout</code>, use +link{portalLayout.portletDropTypes,portletDropTypes} instead.
    //
    // @group dragdrop
    // @see portalLayout.portletDropTypes
    // @visibility external
    //<
    dropTypes: ["PortalColumn"],

    // change appearance of drag placeholder and drop indicator
    dropLineThickness:2,
    dropLineProperties:{backgroundColor:"blue"},

    initWidget : function () {
        this.Super("initWidget", arguments);
   
        this.setCanResizeColumns(this.canResizeColumns);
        this.setColumnOverflow(this.columnOverflow);

        //>!BackCompat 2011.07.11 Renamed canResizeRows to canResizePortlets
        if (this.canResizeRows != null) this.setCanResizePortlets(this.canResizeRows);
        //<!BackCompat

        if (this.portalColumns) {
            this.addPortalColumns(this.portalColumns);
        
            // Don't hang on to the initialization value
            delete this.portalColumns;
        } else if (this.portlets) {
            var self = this;
            
            // If we've been given a single Portlet, make it an array
            if (!isc.isAn.Array(this.portlets)) this.portlets = [this.portlets];
            
            // If the first element in the array is a Portlet (not another array), then
            // we can only have a single column -- so we create the multi-array structure
            if (!isc.isAn.Array(this.portlets[0])) this.portlets = [this.portlets];
            
            // Now we're guaranteed that the first level is an array representing columns
            this.portlets.map(function (column) {
                var portalColumn = self.makePortalColumn();
                self.addPortalColumn(portalColumn);

                // If the column is a single portlet (not an array), then make it an array
                if (!isc.isAn.Array(column)) column = [column];

                // Now, we're guaranteed that the column is an array of rows
                column.map(function (row) {
                    var portalRow = portalColumn.makePortalRow();
                    portalColumn.addPortalRow(portalRow);

                    // Now, we should have either a Portlet or an array of Portlets, so we can just add them
                    portalRow.addPortlets(row);
                });
            });

            // We don't need to hang on to the initialization value
            delete this.portlets;
        } else {
            if (this.numColumns) {
                for (var x = 0; x < this.numColumns; x++) {
                    this.addColumn();
                }
            }
        }
    },

    //> @method portalLayout.getDropPortlet()
    // This method is called when the user drops components into the rows or columns of this
    // PortalLayout.
    // <P>
    // Overriding this method allows you to modify drop behaviour when creating or reordering
    // portlets via drag & drop. You can return the dragTarget for the standard behavior, 
    // or null to cancel the drop.
    // <P>
    // Otherwise, return the component you want to be dropped (as for +link{layout.getDropComponent}).
    // You will generally want to return a +link{Portlet} or subclass. However, you can return
    // any +link{Canvas}, and it will automatically be wrapped in a Portlet if necessary.
    // @param dragTarget (Canvas) drag target
    // @param colNum (int) indicates which column the portlet is being dropped on.
    // @param rowNum (int) indicates the row number being dropped on.
    // @param [dropPosition] (int) Drop position within an existing row. If the dropPosition 
    //  is null, then that means that a new row will be created.
    // @return (Canvas) drop-component or custom Portlet to embed in the portalLayout. Returning
    //  null will cancel the drop.
    // @example portletContentsDragging
    // @example portalCrossWindowDrag
    // @see willAcceptPortletDrop()
    // @see portletsChanged()
    // @visibility external
    //<
    // This is called from portalColumnBody.getDropComponent and portalRow.getDropComponent.
    // Note that return type is documented as Canvas but overrides would probably 
    // always return a Portlet. 
    getDropPortlet : function (dragTarget, colNum, rowNum, dropPosition) {
        return dragTarget;
    },

    //> @attr portalLayout.row (MultiAutoChild Layout : null : A)
    // Automatically generated horizontal +link{Layout} used to create rows of +link{Portlet,Portlets} via
    // +link{Class.createAutoChild,createAutoChild()}. Since this is an +link{AutoChild}, you can use 
    // rowDefaults and rowProperties to customize the rows.
    // <p>
    // Rows are created inside +link{rowLayout,rowLayouts}, which in turn are inside +link{column,columns}.
    // @see portalLayout.column
    // @see portalLayout.rowLayout
    // @visibility external
    //<
    // Note that the actual call to createAutoChild is in PortalColumn. We expose the property
    // here instead because we're trying to avoid exposing PortalColumn and PortalRow.
    rowConstructor: isc.PortalColumn.getInstanceProperty("rowConstructor"),

    //> @attr portalLayout.rowLayout (MultiAutoChild Layout : null : A)
    // Automatically generated vertical +link{Layout} used to create columns of +link{Portlet,Portlets} via
    // +link{Class.createAutoChild,createAutoChild()}. Since this is an +link{AutoChild}, you can use 
    // rowLayoutDefaults and rowLayoutProperties to customize the layout used to contain the rows.
    // <p>
    // The rowLayout is the actual container for +link{row,rows} of +link{Portlet,Portlets}. See +link{column,column} for
    // the column as a whole, which may include a menu as well (depending on +link{showColumnMenus,showColumnMenus}).
    // If you want to style the columns as a whole,
    // use columnDefaults or columnProperties, but if you want to style the layout that actually contains the
    // rows, use rowLayoutDefaults or rowLayoutProperties.
    // @see portalLayout.rowLayout
    // @see portalLayout.row
    // @visibility external
    //<
    // Note that the actual call to addAutoChild is in PortalColumn. We expose teh property
    // here instead because we're trying to avoid exposing PortalColumn and PortalRow.
    rowLayoutDefaults: isc.PortalColumn.getInstanceProperty("rowLayoutDefaults"),
 
    //> @attr portalLayout.column (MultiAutoChild Layout : null : A)
    // Automatically generated vertical +link{Layout} used to create columns of +link{Portlet,Portlets} via
    // +link{Class.createAutoChild,createAutoChild()}. Since this is an +link{AutoChild}, you can use 
    // columnDefaults and columnProperties to customize the columns.
    // <p>
    // The column includes a menu, if +link{showColumnMenus,showColumnMenus} is true, and a +link{rowLayout,rowLayout} which
    // actually contains the +link{row,rows}. Therefore, if you want to style the columns as a whole,
    // use columnDefaults or columnProperties, but if you want to style the layout that contains the
    // rows, use rowLayoutDefaults or rowLayoutProperties.
    // @see portalLayout.rowLayout
    // @see portalLayout.row
    // @visibility external
    //<
    columnConstructor: "PortalColumn",

    // Make columns using autoChild logic, or apply this PortalLayout's
    // settings to an existing PortalColumn. Note that we copy rowConstructor etc. in order
    // to pass the AutoChild logic down for rows.
    makePortalColumn : function (props) {
        if (props == null) props = {};

        var dynamicProperties = {
            portalLayout: this,
            showColumnHeader: this.showColumnMenus,
            border: this.columnBorder,
            canResizePortlets: this.canResizePortlets,
            rowConstructor: this.rowConstructor,
            rowDefaults: this.rowDefaults,
            rowProperties: this.rowProperties,
            rowLayoutDefaults: this.rowLayoutDefaults,
            rowLayoutProperties: this.rowLayoutProperties
        }

        // Only pass on portletDropTypes if it has been specified -- otherwise, we'll
        // clobber any autoChild settings for dropTypes
        if (this.portletDropTypes) dynamicProperties.portletDropTypes = this.portletDropTypes;

        var portalColumn;
        if (isc.isA.PortalColumn(props)) {
            // If we're given an already created PortalColumn, then use setProperties
            // to make it conform to the PortalLayout settings here.
            props.setProperties(dynamicProperties);
            portalColumn = props;
        } else {
            // Otherwise, construct it as an autoChild
            isc.addProperties(props, dynamicProperties);
            portalColumn = this.createAutoChild("column", props);
        }
        return portalColumn;
    },

    // Apply portalColumn logic when adding PoralColumns as members. This allows smoother interaction
    // with existing drag and drop code in Layout.js, since that code ultimately calls
    // addMembers and removeMembers
    addMembers : function (columns, index) {
        if (!isc.isAn.Array(columns)) columns = [columns];
        
        var self = this;
        columns = columns.map(function (column) {
            return self.makePortalColumn(column);
        });
        
        this.Super("addMembers", arguments);

        //>EditMode
        // If we have an editContext and we aren't coming from addPortalColumns, then we check
        // whether the columns have an editNode ... if so, we should add it
        if (this.editContext && !this._addingPortalColumns) {
            for (var i = 0; i < columns.length; i++) {
                var column = columns[i];
                if (column.editNode) {
                    this.editContext.addNode(column.editNode, this.editNode, index + i, null, true);
                }
            }
        }
        //<EditMode
    },

    addPortalColumns : function (columns, index) {
       this._addingPortalColumns = true;
       this.addMembers(columns, index);
       delete this._addingPortalColumns;
    },

    addPortalColumn : function (column, index) {
        this.addPortalColumns(column, index);
    },

    // Catch removeMembers which occurs via drag & drop
    removeMembers : function (portalColumns) {
        this.Super("removeMembers", arguments);

        //>EditMode
        // If we have an editContext, then we check whether we got here via
        // removePortalColumns. If we did, then we assume that the code has done
        // the right thing re: editContext. If not, then we're probably doing
        // a drag & drop from Layout.js, so we should remove the component
        if (this.editContext && !this._removingPortalColumns) {
            if (!isc.isAn.Array(portalColumns)) portalColumns = [portalColumns];
            var self = this;
            portalColumns.map(function (column) {
                if (column.editNode) {
                    // Note that we skip live removal, since we'll have just done that
                    self.editContext.removeNode(column.editNode, true);
                }
            });
        }
        //<EditMode
    },

    removePortalColumns : function (columns) {
        this._removingPortalColumns = true;
        this.removeMembers(columns);
        delete this._removingPortalColumns;
    },

    removePortalColumn : function (column) {
        this.removePortalColumns(column);
    },

    //> @method portalLayout.addColumn()
    // Adds a new portal column to this layout at the specified position
    // @param index (int) target position for the new column
    // @visibility external
    //<
    addColumn : function (index) {
        //>EditMode
        // PortalLayout is a little special with respect to EditMode, since the
        // whole purpose of PortalLayout is to be editable ... thus, it makes
        // more sense to integrate the editing code here, rather than relying
        // on separate code in EditMode.js. For instance, it makes more sense
        // to rely on the standard PortalLayout interface for adding columns,
        // rather than forcing the user to drag a column from a palette.
        // 
        // Thus, when adding a Column, we note whether there is an edit context
        // around and, if so, ask it to do it. That will also eventually run
        // through addPortalColumn, given the standard sequence of events.
        //<EditMode
        if (this.editContext) {
            var columnNode = this.editContext.makeEditNode({
                type: this.columnConstructor
            });
            this.editContext.addNode(columnNode, this.editNode, index);
        } else {
            //>EditMode
            // This is a bit hackish to generate nice ID's in cases where we
            // will soon be put into an editContext
            //<EditMode
            var autoId = "";
            var typeCount = 0;
            while (window[(autoId = "PortalColumn" + typeCount++)]) {}
 
            this.addPortalColumn({ID: autoId}, index);
        }
    },
    
    //> @method portalLayout.removeColumn()
    // Removes the specified column from this layout.
    // All portlets displayed within this column will be destroyed when the column is removed.
    // @param index (int) column number to remove
    // @visibility external
    //<
    removeColumn : function (index) {
        var column = this.members[index];
        if (column != null) {
            if (this.editContext && column.editNode) {
                this.editContext.removeNode(column.editNode);
            } else {
                column.destroy();
            }
        }
    },
    
    // addColumnAfter is used by the header menus shown within columns if appropriate
    addColumnAfter : function (portalColumn) {
        var targetIndex = this.getMemberNumber(portalColumn) + 1;
        this.addColumn(targetIndex);        
    },
   
    //>@method portalLayout.getPortlets()
    // Returns a flat array of all the +link{Portlet,Portlets} in this PortalLayout.
    // @return portlets (Array of Portlet)
    // @visibility external
    // @see getPortletArray() 
    //<
    getPortlets : function () {
        var portlets = [];
        this.getPortalColumns().map(function (column) {
            portlets.addList(column.getPortlets());
        });
        return portlets;
    },
    
    //>@method portalLayout.getPortletArray()
    // Returns a multi-level array of the +link{Portlet,Portlets} in this PortalLayout,
    // where the first level corresponds to columns, the second to rows, and the third
    // to Portlets within rows.
    // @return portlets (Array of Array of Array of Portlet)
    // @visibility external
    // @see getPortlets() 
    //<
    getPortletArray : function () {
        return this.getPortalColumns().map(function (column) {
            return column.getPortletArray();
        });
    },
    
    //> @method portalLayout.getPortalPosition()
    // Gets the position of the +link{Portlet} within this PortalLayout. Returns null
    // if the Portlet is not in this PortalLayout.
    // @param portlet (Portlet) the Portlet for which to get the position
    // @return (PortalPosition) the position of the Portlet
    // @visibility external
    //<
    getPortalPosition : function (portlet) {
        var columns = this.getPortalColumns();

        for (var colNum = 0; colNum < columns.length; colNum++) {
            // We ask each column to find the portlet and fill in the rowNum
            // and position ... if it's found, we add the colNum and return.
            var position = columns[colNum].getPortalPosition(portlet);
            if (position) {
                position.colNum = colNum;
                return position;
            }
        }

        // If it wasn't found, then return null
        return null;
    },
     
    //>@method portalLayout.addPortlet()
    // Adds a +link{Portlet} instance to this portalLayout in the specified position.
    // PortalLayouts use columns to manage the positions of their portlets.
    // Each column is a vertical stack containing a number of rows. By default 
    // a portlet within a column will take up the entire width of the column (so there is
    // one portlet per row within the column), but developers may also place
    // more than one portlet side-by-side on a row within a column - see the
    // <code>positionInExistingRow</code> parameter.
    // 
    // @param portlet (Portlet) Portlet to add to this layout.
    // @param [colNum] (integer) Column in which the Portlet should be added. If unspecified,
    //  portlet will be added to the first column.  If specified, but the 
    //  specified column does not exist, a column is automatically added at the specified 
    //  colNum index.
    // @param [rowWithinCol] (integer) Row-position within the specified column for this portlet.
    //  If unspecified defaults to zero - the portlet will be added to the top of the column.
    //  By default a new row will be added to the column for the portlet. Use the 
    //  <code>positionInExistingRow</code> parameter to add the portlet to an existing
    //  row.
    // @param [positionInExistingRow] (integer) Position within an existing row in the
    //  column. If this parameter is passed, this portlet will be added to the
    //  existing row at <code>rowWithinCol</code>, at the specified position. 
    //  This allows developers to place multiple portlets side by side on a row within 
    //  the column.<br>
    //  If omitted a new row will be created in the column for the portlet.
    //
    // @visibility external
    //<
    //>EditMode in EditMode users can drag/drop from paletteNodes to add portlets to columns.
    // This will never run through this method so this is not a valid override point to catch every
    // newly added portlet //<EditMode
    addPortlet : function (portlet, colNum, rowNum, rowOffset) {
        if (rowNum == null) rowNum = 0;
        if (colNum == null) colNum = 0;
        
        var column = this.getMember(colNum);
        if (column == null) {
           this.addColumn(colNum);
           column = this.getMember(colNum);
        }
        if (rowOffset == null) {
            if (column.editContext && column.editNode && portlet.editNode) {
                column.editContext.addNode(portlet.editNode, column.editNode, rowNum);
            } else {
                column.addPortlet(portlet, rowNum); 
            }
         } else {
            column.addPortletToExistingRow(portlet, rowNum, rowOffset);
        }
    },

    // The sizing policy we want is a bit special. We want to manage percent sizes (or * sizes)
    // like a Layout, but we want to be able to force overflow (though minWidth or simply
    // manually setting widths) like a stack. The easiest way seems to be to manipulate
    // getTotalMemberSpace to ensure that we're big enough.
    getTotalMemberSpace : function () {
        var normalMemberSpace = this.Super("getTotalMemberSpace", arguments);
        var specialMemberSpace = this.members.map(function (member) {
            if (isc.isA.Number(member._userWidth)) {
                return Math.max(member._userWidth, member.minWidth);
            } else {
                return member.minWidth;
            }
        }).sum();
        return Math.max(normalMemberSpace, specialMemberSpace);
    },

    //>@method portalLayout.setColumnWidth()
    // Sets the width of a column in the PortalLayout.
    // <p>
    // Note that this sets the intrinsic width of the column. Columns may also
    // automatically stretch and shrink to accomodate the width of
    // +link{Portlet,Portlets}.
    // @param colNumber (Integer) Which column's width to set.
    // @param width (Number or String) How wide to make the column
    // @see Canvas.setWidth()
    // @visibility external
    //<
    setColumnWidth : function (columnNumber, width) {
        var column = this.getPortalColumn(columnNumber);
        if (!column) return;
        // This automatically adjusts the editNode if present
        if (column.editContext && column.editNode) {
            column.editContext.setNodeProperties(column.editNode, {
                width: width
            });
        } else {
            column.setWidth(width);
        }
    },
    
    //>@method portalLayout.getColumnWidth()
    // Gets the width of a column in the PortalLayout.
    // @param colNumber (int) Which column's width to get
    // @return width (int)
    // @see Canvas.getWidth()
    // @visibility external
    //<
    getColumnWidth : function (columnNumber) {
        var column = this.getPortalColumn(columnNumber);
        if (column) {
            return column.getWidth();
        } else {
            return null;
        }
    },

    getPortalColumns : function () {
        return this.getMembers();
    },

    getPortalColumn : function (columnID) {
        return this.getMember(columnID);
    },

    getPortalColumnNumber : function (columnID) {
        return this.getMemberNumber(columnID);
    },

    getColumn : function (colNum) {
        return this.getPortalColumn(colNum);
    },
    
    //>@method portalLayout.removePortlet()
    // Removes a +link{Portlet} which is currently rendered in this PortalLayout.
    // Portlet will not be destroyed by default - if this is desired the calling code should
    // do this explicitly.
    // @param portlet (Portlet) portlet to remove
    // @visibility external
    //<
    //>EditMode We *DO* auto-destroy portlets on closeclick in editMode if they were dragged in
    // from a paletteNode //<EditMode
    removePortlet : function (portlet) {
        if (this.editContext && portlet.editNode) {
            this.editContext.removeNode(portlet.editNode);
        } else {
            if (portlet.portalRow) portlet.portalRow.removePortlets(portlet);
            // Note: the row will self-destruct if appropriate -- see membersChanged handler
        }
    },

    _updateResizeFrom : function () {
        var firstColumn  = true;
        var self = this;

        this.getPortalColumns().map(function (column) {
            if (self.canResizeColumns) {
                // If canResizeColumns is on, then we always allow resizing
                // from the right edge. Even if this is the right-most column,
                // we allow it to be enlarged, since the user may want to force
                // overflow (i.e. horizontal scrolling).
                //
                // We allow resizing from the left edge if this is not the first
                // column.
                column.rowLayout.resizeFrom = column.resizeFrom = firstColumn ? ["R"] : ["L", "R"];
            } else {
                // If canResizeColumns is off, then don't allow any resizes
                column.rowLayout.resizeFrom = column.resizeFrom = [];
            }

            // And update the resizeFrom for each portlet in the column
            column.getPortalRows().map(function (row) {
                row._updatePortletResizeFrom();
            });

            firstColumn = false;
        });
    },

    membersChanged : function () {
        this._updateResizeFrom();
    },

    //>@method portalLayout.willMaximizePortlet()
    // Method called when a +link{Portlet} in this PortalLayout is about to be
    // maximized. Note that this method is only called when the user explicitly
    // clicks on the portlet's +link{window.showMaximizeButton, maximize button} --
    // it is not called when programmatically maximizing a portlet via
    // +link{window.maximize(),maximize()}.
    // <p>
    // Return false to cancel the action.
    // 
    // @param portlet (Portlet) the Portlet which will be maximized
    // @return (boolean) whether the action should proceed
    // @see portletMaximized()
    // @visibility external
    //<
    
    //>@method portalLayout.portletMaximized()
    // Notification method called after a portlet has been maximized (whether by
    // user action or programmatically).
    //
    // @param portlet (Portlet) the Portlet which was maximized
    // @see willMaximizePortlet()
    // @visibility external
    //<
     
    //>@method portalLayout.willMinimizePortlet()
    // Method called when a +link{Portlet} in this PortalLayout is about to be
    // minimized. Note that this method is only called when the user explicitly
    // clicks on the portlet's +link{window.showMinimizeButton, minimize button} --
    // it is not called when programmatically minimizing a portlet via
    // +link{window.minimize(),minimize()}.
    // <p>
    // Return false to cancel the action.
    // 
    // @param portlet (Portlet) the Portlet which will be minimized
    // @return (boolean) whether the action should proceed
    // @see portletMinimized()
    // @visibility external
    //<
    
    //>@method portalLayout.portletMinimized()
    // Notification method called after a portlet has been minimized (whether by
    // user action or programmatically).
    //
    // @param portlet (Portlet) the Portlet which was minimized
    // @see willMinimizePortlet()
    // @visibility external
    //<
     
    //>@method portalLayout.willRestorePortlet()
    // Method called when a +link{Portlet} in this PortalLayout is about to be
    // restored to its normal place (after having been
    // +link{portletMaximized(),maximized}. Note that this method is only
    // called when the user explicitly clicks on the portlet's
    // +link{window.restoreButton, restore button} -- it is not called when
    // programmatically restoring a portlet via +link{window.restore(),restore()}.
    // <p>
    // Return false to cancel the action.
    // 
    // @param portlet (Portlet) the Portlet which will be restored
    // @return (boolean) whether the action should proceed
    // @see portletRestored()
    // @visibility external
    //<

    //>@method portalLayout.portletRestored()
    // Notification method called after a portlet has been restored to its normal place
    // (after having been maximized). The method is called whether the restore is
    // via user action or done programmatically.
    //
    // @param portlet (Portlet) the Portlet which was restored
    // @see willRestorePortlet()
    // @visibility external
    //<

    //>@method portalLayout.willClosePortlet()
    // Method called when a +link{Portlet} in this PortalLayout is about to be closed.
    // This method is called before +link{portlet.showCloseConfirmationMessage} is applied.
    // Note that this method is called only when the user explicitly closes a Portlet.
    // It is not called when programmatically removing a Portlet via +link{removePortlet()}.
    // <p>
    // Return false to cancel the action.
    // 
    // @param portlet (Portlet) the Portlet which will be closed
    // @return (boolean) whether the action should proceed
    // @see portlet.showCloseConfirmationMessage
    // @see portletsChanged()
    // @visibility external
    //<
    
    //>@method portalLayout.portletsChanged()
    // Fires at initialization if the PortalLayout has any initial
    // +link{Portlet,portlets}, and then fires whenever portlets are added,
    // removed or reordered.
    //
    // @see layout.membersChanged()
    // @visibility external
    //<
    

    //>@method portalLayout.portletsResized()
    // Fires when +link{Portlet,portlets} or columns in this PortalLayout are resized.
    // Note that this fires on a short delay -- otherwise, it would fire multiple times
    // for each change, since most portlet size changes will affect multiple portlets.
    // Does not fire when a portlet is +link{portletMaximized(),maximized} or
    // +link{portletRestored(),restored}.
    // @visibility external
    //<

    _portletsResized : function () {
        if (this.portletsResized) {
            this.fireOnPause("portletsResized", "portletsResized", 100);
        }
    }
});

isc.PortalLayout.addClassMethods({
    // Like PortalRow & others, we're using this to make sure that the members fill the space
    // if they all have numeric sizes -- otherwise, there would be nothing to stretch.
    // Then, we check to see whether any columns need to stretch to avoid horizontal overflow. If
    // extra width is required, we check other columns to see if they can shrink (to avoid PortalLayout overflow).
    applyStretchResizePolicy : function (sizes, totalSize, minSize, modifyInPlace, propertyTarget) {
        if (propertyTarget.preventUnderflow) {
            if (sizes && sizes.length > 0) {
                var allNumeric = sizes.map(function (size) {
                    return isc.isA.Number(size);
                }).and();
                
                if (allNumeric) {
                    var totalNumericSizes = sizes.sum();
                    if (totalNumericSizes < totalSize) {
                        sizes[sizes.length - 1] = "*";
                    }
                }
            }
        }

        var newSizes = this.Super("applyStretchResizePolicy", arguments);
        if (modifyInPlace) newSizes = sizes;

        var desiredWidths = propertyTarget.getPortalColumns().map("_getDesiredWidth");
        var extraWidth = 0;

        if (propertyTarget.canStretchColumnWidths) {
            if (propertyTarget.stretchColumnWidthsProportionally) {
                // If we're maintaining the relative size of column widths, we figure out
                // the maximum percentage stretch and then apply it everywhere
                var maxStretchRatio = 1;
                
                for (var i = 0; i < newSizes.length; i++) {
                    var stretchRatio = (desiredWidths[i] / newSizes[i]);
                    maxStretchRatio = Math.max(maxStretchRatio, stretchRatio);
                }

                if (maxStretchRatio > 1) {
                    for (var i = 0; i < newSizes.length; i++) {
                        newSizes[i] = newSizes[i] * maxStretchRatio;
                    }
                }
            } else {
                // If we're not maintain the relative size of column widths, then just stretch
                // the columns that need it, and shrink other columns if allowed (and possible)
                for (var i = 0; i < newSizes.length; i++) {
                    if (desiredWidths[i] > newSizes[i]) {
                        extraWidth += desiredWidths[i] - newSizes[i];
                        newSizes[i] = desiredWidths[i];
                    } else if (extraWidth && propertyTarget.canShrinkColumnWidths) {
                        var excess = newSizes[i] - desiredWidths[i];
                        var difference = Math.min(extraWidth, excess);
                        newSizes[i] -= difference;
                        extraWidth -= difference;
                    }
                }
                
                // Check if there is any extraWidth remaining at the end
                if (extraWidth && propertyTarget.canShrinkColumnWidths) {
                    for (var i = 0; i < newSizes.length; i++) {
                        if (desiredWidths[i] < newSizes[i]) {
                            var excess = newSizes[i] - desiredWidths[i];
                            var difference = Math.min(extraWidth, excess);
                            newSizes[i] -= difference;
                            extraWidth -= difference;
                            if (extraWidth == 0) break;
                        }
                    }
                }
            }
        }

        return newSizes;
    }
});
