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
//> @class EditProxy
// An EditProxy is attached to an editable component when editMode is enabled. This proxy
// has methods and properties which affect the component during editing.
//
// @group devTools
// @treeLocation Client Reference/Tools
// @visibility external
//<


isc.defineClass("EditProxy", "Class");

isc.EditProxy.addClassProperties({
    resizeThumbConstructor:isc.Canvas,
    resizeThumbDefaults:{
        autoDraw:false,
        destroyWithMaster:false,
        _showWithMaster:true,
        _setOpacityWithMaster:false,
        overflow:"hidden", 
        canDrag:true,
        canDragResize:true,
        // resizeEdge should be the edge of the target, not the thumb
        getEventEdge : function () { return this.edge; },
        depeered : function (oldMaster, name) {
            this.hide();
        }
    },

    minimumDropMargin: 2,
    minimumDropTargetSize: 10,

    // Resize thumbs
    // ---------------------------------------------------------------------------------------

    // NOTE: EditProxy thumbs vs one-piece mask?
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
            thumbClass = isc.ClassFactory.getClass(this.resizeThumbConstructor, true);
        for (var thumbPosition in edgeCursors) {
            var corner = thumbPosition.length == 1;
            // NOTE: can't use standard autoChild creation because we are in static scope -
            // thumbs are globally shared
            thumbs[thumbPosition] = thumbClass.create({
                 ID:"isc_resizeThumb_" + thumbPosition,
                 edge:thumbPosition,
                 snapTo:thumbPosition,
                 snapOffsetTop:(thumbPosition[0] === "T" ? -7 : (thumbPosition[0] === "B" ? 7 : 0)),
                 snapOffsetLeft:(thumbPosition[thumbPosition.length - 1] === "R" ? 7 : (thumbPosition[thumbPosition.length - 1] === "L" ? -7 : 0)),
                 styleName: corner? "resizeThumb": "resizeThumb cornerResizeThumb",
                 width: corner? 7 : 9,
                 height: corner? 7 : 9
            }, this.resizeThumbDefaults, this.resizeThumbProperties)
        }
        this._resizeThumbs = thumbs;

        this._observer = isc.Class.create();
    },

    // target is either the mask or the masked-component
    showResizeThumbs : function (target) {
        if (!target) return;
        if (target.editProxy) {
            if (!target.editProxy.hasEditMask()) {
                // Component has no edit mask so resize thumbs are not applicable
                return;
            }
            target = target.editProxy.getEditMask();
        }

        if (!isc.EditProxy._resizeThumbs) isc.EditProxy._makeResizeThumbs();

        var thumbSize = isc.EditProxy.resizeThumbDefaults.width,
            thumbs = isc.EditProxy._resizeThumbs;

        for (var thumbName in thumbs) {
            var thumb = thumbs[thumbName];
            // set all the thumbs to drag resize the canvas we're masking
            thumb.dragTarget = target;
            // show all the thumbs
            thumb.bringToFront();
            thumb.show();
            target.addPeer(thumb);
        }

        this._thumbTarget = target;
        target.enableKeyMovement(true);
    },

    hideResizeThumbs : function () {
        var thumbs = this._resizeThumbs;
        for (var thumbName in thumbs) {
            var thumb = thumbs[thumbName];
            thumb.depeer();
        }
        if (this._thumbTarget) this._thumbTarget.enableKeyMovement(false);

        this._thumbTarget = null;
    },

    getThumbTarget : function () {
        return this._thumbTarget;
    },

    // Splits a string into an array of values based on the <separatorChar>.
    // Handles escaping of <separatorChar>.
    splitString : function (string, separatorChar, escapeChar) {
        if (!string) return [];
        var chars = string.split(""),
            escaped = false,
            values = [],
            value = []
        ;
        for (var i = 0; i < chars.length; i++) {
            var char = chars[i];
            if (char == escapeChar && !escaped) {
                escaped = true;
                // eat escape char
            } else if (escaped) {
                // Only un-escape an escaped separatorChar
                if (char != separatorChar) value.push(escapeChar);
                value.push(char);
                escaped = false;
            } else if (char == separatorChar) {
                // save trimmed value
                values.push(isc.EditProxy.trimTrailingSpaces(value.join("").replace(/^\s+/g,"")));
                value = [];
            } else {
                value.push(char);
            }
        }
        if (value.length > 0) {
            // save trimmed value
            values.push(isc.EditProxy.trimTrailingSpaces(value.join("").replace(/^\s+/g,"")));
        }
        return values;
    },

    
    parseStringValueMap : function (string, separatorChar, escapeChar, displaySeparatorChar, selectedChar, matchDisplayWithValue, alwaysUseMap) {
        var values = isc.EditProxy.splitString(string, separatorChar, escapeChar);
        if (values.length == 0) return {};

        var map = {},
            array = [],
            selectedValues = [],
            usingMap = alwaysUseMap,
            splitCount = 0,
            majority = (values.length/2) << 0
        ;
        for (var i = 0; i < values.length; i++) {
            var value = values[i];
            if (!displaySeparatorChar) {
                var result = isc.EditProxy._getSelectedValue(value, escapeChar, selectedChar);
                if (result.selected) selectedValues.push(result.value);
                array.push(result.value);
            } else {
                // If there are a mixture of value:displayValue and value only records
                // entered, we will process these into both the array and map. If the
                // number of value:displayValue records becomes a majority, the map
                // will be returned; otherwise the array is used.
                var displayValues = isc.EditProxy.splitString(value, displaySeparatorChar, escapeChar);
                var result = isc.EditProxy._getSelectedValue(displayValues[0], escapeChar, selectedChar);
                if (result.selected) selectedValues.push(result.value);

                array.push(value);

                if (displayValues.length == 1) {
                    map[result.value] = (matchDisplayWithValue ? result.value : null);
                } else {
                    value = result.value;
                    var result = isc.EditProxy._getSelectedValue(displayValues[1], escapeChar, selectedChar);
                    map[value] = result.value;
                    if (result.selected && !selectedValues.contains(value)) selectedValues.push(value);

                    // If we reached a majority of split values, return map
                    if (++splitCount > majority) usingMap = true;
                }
            }
        }

        return {
            valueMap: (usingMap ? map : array),
            value: (selectedValues.length == 0 ? null : (selectedValues.length == 1 ? selectedValues[0] : selectedValues))
        };
    },

    // Trim trailing spaces from the string respecting escaped spaces (i.e. "\ ")
    trimTrailingSpaces : function (string, escapeChar) {
        escapeChar = escapeChar || "\\";

        var length = string.length;
        if (length == 1) {
            if (string == " ") return "";
            return string;
        }

        for (var i = length-1; i > 0; i--) {
            var c = string.substring(i,i+1),
                pc = string.substring(i-1,i)
            ;
            if (c != " " || pc == "\\") {
                string = string.substring(0,i+1);
                break;
            } 
        }
        return string;
    },

    _getSelectedValue : function (string, escapeChar, selectedChar) {
        var result = {};
        if (selectedChar && string.startsWith(selectedChar)) {
            result.value = string.substring(1).replace(/^\s+/,"");
            result.selected = true;
        } else if (selectedChar && string.endsWith(selectedChar)) {
            if (string.substring(string.length-2,string.length-1) != escapeChar) {
                result.value = isc.EditProxy.trimTrailingSpaces(string.substring(0,string.length-1));
                result.selected = true;
            } else {
                result.value = string;
            }
        } else {
            result.value = string;
        }
        return result;
    },

    // helper for extracting meaningful properties defaults using schema
    filterLiveObjectBySchema : function(nodeType, liveObject) {
        var result = {};

        var schema = isc.DS.get(nodeType);
        if (!schema) return result;

        var fields = schema.fields;
        for (var key in fields) {
            var value = liveObject[key];
            if (fields.hasOwnProperty(key) && liveObject.hasOwnProperty(key)) {
                if (!isc.isAn.Object(value)) result[key] = value;
            }
        }
        return result;
    }
});

isc.EditProxy.addProperties({
    //> @attr editProxy.autoMaskChildren  (Boolean : null : IR)
    // When child nodes are added to an EditContext, should they be masked by setting
    // +link{editNode.useEditMask} <code>true</code> if not explicitly set?
    //
    // @visibility external
    //<

    //> @attr editProxy.canSelectChildren    (Boolean : null : IRW)
    // Whether to allow selection of the children of this +link{EditNode}.  The appearance and
    // behavior of selected components is controlled by +link{selectedAppearance}, or centrally
    // across an +link{EditContext} via +link{editContext.selectedAppearance}.
    // <p>
    // Individual children can be marked non-selectable via setting +link{editProxy.canSelect}
    // to <code>false</code>.
    // <p>
    // Use the +link{EditContext} to access and manipulate the currently selected set of
    // EditNodes, via APIs such as +link{editContext.getSelectedEditNode()},
    // +link{editContext.selectSingleEditNode()} and
    // +link{editContext.selectedEditNodesUpdated}.
    //
    // @visibility external
    // @see editContext.canSelectEditNodes
    //<

    //> @attr editProxy.childrenSnapToGrid (Boolean : null : IRW)
    // If not null the +link{canvas.childrenSnapToGrid} on the component represented by this
    // EditProxy is set to this value only while in edit mode. This allows snapToGrid functionality
    // to be enforced during edit mode but not when live.
    //
    // @visibility external
    //<

    //> @attr editProxy.childrenSnapResizeToGrid (Boolean : null: IRW)
    // If not null the +link{canvas.childrenSnapResizeToGrid} on the component represented by this
    // EditProxy is set to this value only while in edit mode. This allows snapResizeToGrid functionality
    // to be enforced during edit mode but not when live.
    //
    // @visibility external
    //<

    //> @method editProxy.setCanSelectChildren() (A)
    // Setter for +link{editProxy.canSelectChildren,canSelectChildren}.
    // @param canSelect (boolean) the new canSelectChildren
    //
    // @visibility external
    //<
    setCanSelectChildren : function (canSelect) {
        if (canSelect == this.canSelectChildren) return;

        // Update properties to match new selection option
        this.restoreOverrideProperties();
        this.canSelectChildren = canSelect;
        this.saveOverrideProperties();
    },

    //> @attr editProxy.canSelect    (Boolean : null : IR)
    // Can this component be selected? Selection is allowed unless this
    // property is set to <code>false</code>.
    // @visibility external
    //<

    //> @attr editProxy.allowNestedDrops (Boolean : null : IR)
    // This property acts as a component-specific override for the +link{EditContext.allowNestedDrops}
    // property. Unless explicitly set to false, the +link{EditContext.allowNestedDrops} controls whether
    // a drop can be made into this component.
    // @visibility external
    //<

    // Edit Mask
    // ---------------------------------------------------------------------------------------

    // At the Canvas level the Edit Mask provides moving, resizing, and standard context menu items.
    // The editMask should be extended on a per-widget basis to add things like drop behaviors or
    // additional context menu items.  Any such extensions should be delineated with 
    //>EditMode 
    //<EditMode
    // .. markers so it can be eliminated from normal builds.

    //> @attr editProxy.editMask (AutoChild Canvas : null : IR)
    // An editMask is created for any component placed into editMode with
    // +link{editNode.useEditMask}:true.
    // <P>
    // Common customization properties can be provided by +link{editContext.editMaskProperties}.
    //
    // @visibility external
    //<

    editMaskDefaults:{

        canFocus: true,

        // Thumb handling
        // ---------------------------------------------------------------------------------------
        draw : function () {
            this.Super("draw", arguments);

            // stay above the master
            this.observe(this.masterElement, "setZIndex", "observer.moveAbove(observed)");

            // match the master's prompt (native tooltip).  Only actually necessary in Moz since IE
            // considers the eventMask transparent with respect to determining the prompt.
            this.observe(this.masterElement, "setPrompt", "observer.setPrompt(observed.prompt)");

            // disable/re-enable key movement during inline edits
            this.observe(this.masterElement.editProxy, "startInlineEditing", "observer.editingStarted()");
            this.observe(this.masterElement.editProxy, "inlineEditingComplete", "observer.editingComplete()");

            return this;
        },
        parentVisibilityChanged : function () {
            this.Super("parentVisibilityChanged", arguments);
            if (isc.EditProxy.getThumbTarget() == this) isc.EditProxy.hideResizeThumbs();
        },
        // The _resizeWithMaster setting doesn't handle the DynamicForm situation where
        // overflow:visible is used.
        masterResized : function (deltaX, deltaY, reason) {
            // Resizing of the mask normally attempts to update the master element as well.
            // That is us. To prevent this recursive call set the flag used internally by
            // resized() to skip resizing the master.
            this._resizingMaster = true;
            this.resizeTo(this.masterElement.getVisibleWidth(), this.masterElement.getVisibleHeight());
            this._resizingMaster = false;
        },

        handleClick : function () {
            this.Super("handleClick", arguments);
            this.select();

            var component = this.masterElement;
            if (component.editProxy.supportsInlineEdit && 
                    this.editContext.enableInlineEdit &&
                    component.editProxy.inlineEditEvent == "click")
            {
                component.editProxy.startInlineEditing();
            }
            if (this.editContext.editMaskClicked) {
                this.editContext.editMaskClicked(component.editNode, component);
            }
            return isc.EH.STOP_BUBBLING;
        },

        handleDoubleClick : function () {
            this.Super("handleDoubleClick", arguments);
            this.select();

            var component = this.masterElement;
            if (component.editProxy.supportsInlineEdit && 
                    this.editContext.enableInlineEdit &&
                    (component.editProxy.inlineEditEvent == "doubleClick" || component.editProxy.inlineEditEvent == "dblOrKeypress"))
            {
                component.editProxy.startInlineEditing();
            }
            return isc.EH.STOP_BUBBLING;
        },

        // Only used with no group selection mask
        enableKeyMovement : function (enable) {
            if (enable) {
                if (!this._keyPressEventID) {
                    this._keyPressEventID = isc.Page.setEvent("keyPress", this);
                }
            } else {
                if (this._keyPressEventID) {
                    isc.Page.clearEvent("keyPress", this._keyPressEventID);
                    delete this._keyPressEventID;
                }
            }
        },

        // Keypress positioning of mask must be disabled while in inline edit
        // mode or the key is processed outside of the entry
        editingStarted : function () {
            this._keyPressEnabledBeforeEdit = (this._keyPressEventID != null);
            if (this._keyPressEnabledBeforeEdit) this.enableKeyMovement(false);
        },
        
        editingComplete : function () {
            if (this._keyPressEnabledBeforeEdit) this.enableKeyMovement(true);
        },

        select : function () {
            if (this.editPaneProxy && this.editPaneProxy.canSelectChildren) {
                var target = this.getTarget(),
                    multiSelect = (this.editContext.selectionType == isc.Selection.MULTIPLE)
                ;
                if (target.editProxy && target.editProxy.canSelect == false) return;
                if (this.editPaneProxy.bringToFrontOnSelect) target.bringToFront();
                else this.bringToFront();
    
                var modifierKeyDown = (isc.EH.shiftKeyDown() || (isc.Browser.isWin && isc.EH.ctrlKeyDown()));
    
                if (!this.editContext.isComponentSelected(target)) {
                    if (!multiSelect || !modifierKeyDown) {
                        this.editContext.selectSingleComponent(target);
                    } else {
                        this.editContext.selectComponent(target);
                    }
                } else {
                    if (!multiSelect || !modifierKeyDown) {
                        this.editContext.selectSingleComponent(target);
                    } else {
                        this.editContext.deselectComponents(target);
                    }
                }
            }
        },

        // Event Bubbling
        // ---------------------------------------------------------------------------------------

        

        
        moveAbove : function (canvas) {
            if (!this.editContext.isComponentSelected(this.masterElement) ||
                    this.getZIndex(true) <= canvas.getZIndex(true))
            {
                this.Super("moveAbove", arguments);
            }
        },

        // prevent bubbling to the editor otherwise we'll start a selection while trying to
        // select/move a component
        handleMouseDown : function () {
            this.Super("handleMouseDown", arguments);
            return isc.EH.STOP_BUBBLING;
        },

        handleMouseUp : function () {
            this.Super("handleMouseUp", arguments);
            return isc.EH.STOP_BUBBLING;
        },

        dragRepositionStart : function() {
            if (this.editPaneProxy && (!this.editPaneProxy.canSelectChildren || this.editPaneProxy.canSelect == false)) {
                // Cancel drag
                return false;
            }
            var target = this.getTarget();

            if (this.editPaneProxy.bringToFrontOnSelect) target.bringToFront();
            else this.bringToFront();
            // When we start to drag a component it should be selected
            if (this.editPaneProxy && this.editPaneProxy.canSelectChildren &&
                (this.editContext.selectionType != isc.Selection.MULTIPLE ||
                        !this.editContext.isComponentSelected(target))) 
            {
                this.editContext.selectSingleComponent(target);
            }
        },

        pageKeyPress : function (target, eventInfo) {
            // If root pane (or child) does not have focus, ignore keyPress 
            var rootPane = this.editContext.getRootEditNode().liveObject;
            if (!rootPane.containsFocus()) return;
            
            var key = isc.EH.getKeyEventCharacter();
            if (!isc.isA.AlphaNumericChar(key)) {
                var masked = this.masterElement,
                    selection = masked.editContext.getSelectedComponents()
                ;

                // If our masked component is not selected, ignore the keypress
                if (!selection.contains(masked)) return;

                // Ignore keyboard movement for percentage-placed components
                if (this.isPercent(masked.left) || this.isPercent(masked.top)) return;

                // Ignore keyboard movement If component is positioned by snapTo with offset in percentage
                if (masked.snapTo && 
                        (this.isPercent(masked.snapOffsetLeft) || this.isPercent(masked.snapOffsetTop)))
                {
                    return;
                }

                var parent = masked.parentElement,
                    shiftPressed = isc.EH.shiftKeyDown(),
                    vGap = (shiftPressed ? 1 : parent.snapVGap),
                    hGap = (shiftPressed ? 1 : parent.snapHGap),
                    delta = [0,0],
                    result = false
                ;

                switch (isc.EH.getKey()) {
                case "Arrow_Up":
                    delta = [0, vGap * -1];
                    break;
                case "Arrow_Down":
                    delta = [0, vGap];
                    break;
                case "Arrow_Left":
                    delta = [hGap * -1, 0];
                    break;
                case "Arrow_Right":
                    delta = [hGap, 0];
                    break;
                default:
                    result = null;
                    break;
                }

                
                if (delta[0] != 0 || delta[1] != 0) {
                    parent._movingSelection = true;
                    if (masked.snapTo) {
                        // Instead of repositioning component directly, just adjust the
                        // snapOffsets
                        masked.setSnapOffsetLeft((masked.snapOffsetLeft || 0) + delta[0]);
                        masked.setSnapOffsetTop((masked.snapOffsetTop || 0) + delta[1]);
                    } else {
                        masked.moveBy(delta[0], delta[1]);
                    }
                    parent._movingSelection = false;
                }
                return result;
            }
        },

        _$percent: "%",
        isPercent : function (value) {
            return (isc.isA.String(value) && isc.endsWith(value, this._$percent));
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
        dragRepositionAppearance:"target",
    
        // don't allow setDragTracker to bubble in case some parent tries to set it inappropriately
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

            if (isc.EditProxy.getThumbTarget() == this) isc.EditProxy.showResizeThumbs(this);
        },

        resized : function() {
            this.Super("resized", arguments);

            // Recalculate dropMargin based on new visible size
            if (this.editPaneProxy) this.editPaneProxy.updateDropMargin();

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
            if (isc.EditProxy.getThumbTarget() == this) isc.EditProxy.showResizeThumbs(this);

            this._resizingMaster = false;
        },

        // Editing Context Menus
        // ---------------------------------------------------------------------------------------
        // standard context menu items plus the ability to add "editMenuItems" on the master
        showContextMenu : function () {
            // Showing context menu should also shift selected component unless
            // the component is part of a selection already.
            var target = this.masterElement,
                targetSelected = this.editContext.isComponentSelected(target);
            if (this.editPaneProxy.canSelectChildren && this.editPaneProxy.canSelect != false) {
                if (!targetSelected) {
                    this.editContext.selectSingleComponent(target);
                }
            }

            // Show multiple-selection menu iff menu target is part of selection
            var selection = this.editContext.getSelectedComponents(),
                menuItems;
            if (selection.length > 1 && targetSelected) { 
                // multi-select
                menuItems = this.multiSelectionMenuItems;
            } else {
                menuItems = this.standardMenuItems;
            }

            if (!this.contextMenu) this.contextMenu = this.getMenuConstructor().create({});
            this.contextMenu.setData(menuItems);

            // NOTE: show the menu on the mask to allow reference to the editPane
            // and/or proxy.
            this.contextMenu.showContextMenu(this);
            return false;
        },
        // Menu actions
        componentsRemove : function () {
            this.editContext.getSelectedComponents().map("destroy");
        },
        componentsBringToFront : function () {
            this.editContext.getSelectedComponents().map("bringToFront");
        },
        componentsSendToBack : function () {
            this.editContext.getSelectedComponents().map("sendToBack");
        },
        // Single and multiple-selection menus
        standardMenuItems:[
            {title:"Bring to front", click:"target.componentsBringToFront()"},
            {title:"Send to back", click:"target.componentsSendToBack()"},
            {title:"Remove", click:"target.componentsRemove()"}
        ],
        multiSelectionMenuItems: [
            {title:"Bring to front", click:"target.componentsBringToFront()"},
            {title:"Send to back", click:"target.componentsSendToBack()"},
            {title: "Remove selected items", click:"target.componentsRemove()"}
        ]
    }
});

isc.EditProxy.addMethods({

    setEditMode : function (editingOn) {
        if (editingOn) {
            this.saveOverrideProperties();
            // Calculate dropMargin based on visible size
            if (!isc.isA.FormItem(this.creator)) this.updateDropMargin();
            if (this.hasEditMask()) this.showEditMask();
        } else {
            this.restoreOverrideProperties();
            this.hideEditMask();
        }

        // Convert property to boolean if needed
        if (this.persistCoordinates != null) {
            if (isc.isA.String(this.persistCoordinates)) this.persistCoordinates = (this.persistCoordinates == "true");
        }
    },

    getOverrideProperties : function () {
        var properties = {
            canAcceptDrop: true,
            canDropComponents: true
        };

        if (this.canSelectChildren) {
            isc.addProperties(properties, {
                canDrag: true,
                dragAppearance: "none",
                overflow: "hidden"
            });
        }
        if (this.childrenSnapToGrid != null) {
            if (isc.isA.String(this.childrenSnapToGrid)) this.childrenSnapToGrid = (this.childrenSnapToGrid == "true");
            properties.childrenSnapToGrid = this.childrenSnapToGrid;
        }
        if (this.childrenSnapAlign != null) {
            if (isc.isA.String(this.childrenSnapAlign)) this.childrenSnapAlign = (this.childrenSnapAlign == "true");
            properties.childrenSnapAlign = this.childrenSnapAlign;
        }
        if (this.childrenSnapResizeToGrid != null) {
            if (isc.isA.String(this.childrenSnapResizeToGrid)) this.childrenSnapResizeToGrid = (this.childrenSnapResizeToGrid == "true");
            properties.childrenSnapResizeToGrid = this.childrenSnapResizeToGrid;
        }
        return properties;
    },

    // Called after a new node is created by a drop
    nodeDropped : function () {
        if (this.inlineEditOnDrop) {
            // Give the object a chance to draw before we start the edit, otherwise the 
            // editor co-ordinates will be wrong
            this.delayCall("startInlineEditing");
        }
    },

    editTitle : function (liveObject, initialValue, completionCallback) {
        var liveObject = liveObject || this.creator,
            left,
            width,
            top;

        if (isc.isA.Button(liveObject)) {  // This includes Labels and SectionHeaders
            left = liveObject.getPageLeft() + liveObject.getLeftBorderSize() + liveObject.getLeftMargin() + 1 
                                                  - liveObject.getScrollLeft(); 
            width = liveObject.getVisibleWidth() - liveObject.getLeftBorderSize() - liveObject.getLeftMargin() 
                               - liveObject.getRightBorderSize() - liveObject.getRightMargin() - 1;
        } else if (isc.isA.StretchImgButton(liveObject)) {
            left = liveObject.getPageLeft() + liveObject.capSize;
            width = liveObject.getVisibleWidth() - liveObject.capSize * 2;
        } else {
            isc.logWarn("Ended up in editTitle with a StatefulCanvas of type '" + 
                    liveObject.getClass() + "'.  This is neither a Button " +
                        "nor a StretchImgButton - editor will work, but will hide the " +
                        "entire component it is editing");
            left = liveObject.getPageLeft();
            width = liveObject.getVisibleWidth();
        }

        isc.Timer.setTimeout({target: isc.EditContext,
                              methodName: "manageTitleEditor", 
                              args: [liveObject, left, width, top, null, initialValue, null, completionCallback]}, 0);
    },

    // This function is only called for ImgTabs that need to be scrolled into view
    repositionTitleEditor : function () {
        var liveObject = this.creator;
        var left = liveObject.getPageLeft() + liveObject.capSize,
            width = liveObject.getVisibleWidth() - liveObject.capSize * 2;
        
        isc.EditContext.positionTitleEditor(liveObject, left, width);
    },

    // Save/restore property functionality
    // ---------------------------------------------------------------------------------------

    // These methods are based on Class.saveToOriginalValues and Class.restoreFromOriginalValues.
    // This is necessary because edit values can be merged into saved values and should be
    // restored when done.
    saveOverrideProperties : function () {
        var properties = this.getOverrideProperties();
        this.overrideProperties(properties);
    },
    
    restoreOverrideProperties : function () {
        var properties = this.getOverrideProperties();
        this.restoreProperties(isc.getKeys(properties));
    },

    overrideProperties : function (properties) {
        this.creator.saveToOriginalValues(isc.getKeys(properties));
        this.creator.setProperties(properties);
    },

    restoreProperties : function (fieldNames) {
        if (fieldNames == null) return;
        this.creator.restoreFromOriginalValues(fieldNames);
    },

    // Edit Mask
    // ---------------------------------------------------------------------------------------

    showEditMask : function (editPane) {
        var liveObject = this.creator,
            svgID = liveObject.getID() + ":<br>" + liveObject.src;

        // create an edit mask if we've never created one or it was destroyed
        
        if (!this._editMask || this._editMask.destroyed) {

            // special SVG handling
            // FIXME: move all SVG-specific handling to SVG.js
            var svgProps = { };
            if (isc.SVG && isc.isA.SVG(liveObject) && isc.Browser.isIE) {
                isc.addProperties(svgProps, {
                    backgroundColor : "gray",
                    mouseOut : function () { this._maskTarget.Super("_hideDragMask"); },
                    contents : isc.Canvas.spacerHTML(10000,10000, svgID)
                });
            }
    
            var props = isc.addProperties({}, this.editMaskDefaults, this.editMaskProperties, 
                                          // assume the editContext is the parent if none is
                                          // provided
                                          {editPane:editPane,
                                           editPaneProxy:editPane.editProxy,
                                           editContext:liveObject.editContext || liveObject.parentElement, 
                                           keepInParentRect: liveObject.keepInParentRect},
                                          svgProps);
            this._editMask = isc.EH.makeEventMask(liveObject, props);
        }
        this._editMask.show();

        // SVG-specific
        if (isc.SVG && isc.isA.SVG(liveObject)) {
            if (isc.Browser.isIE) liveObject.showNativeMask();
            else {
                liveObject.setBackgroundColor("gray");
                liveObject.setContents(svgID);
            }
        }
    },
    hideEditMask : function () {
        if (this._editMask) this._editMask.hide();
    },
    setEditMaskBorder : function (style) {
        if (this._editMask) this._editMask.setBorder(style);
    },
    hasEditMask : function () {
        return (this._editMask != null);
    },
    getEditMask : function () {
        return this._editMask;
    },


    // Drag move and resize
    // ---------------------------------------------------------------------------------------
    // Implemented in Canvas.childResized and Canvas.childMoved.

    mouseDown : function (event) {
        var liveObject = this.creator,
            target = isc.EH.getTarget()
        ;

        // Even in editMode some components need to pass on special
        // clicks to parts of the component. An example is a Tab which
        // has a close icon that should still close the tab in editMode.
        if (target == liveObject && liveObject.useEventParts) {
            if (liveObject.firePartEvent(event, isc.EH.MOUSE_DOWN) == false) return false;
        }
    },

    dragMove : function() {
        if (this.creator.dragMove) return this.creator.dragMove();
    },

    // Snap grid
    // --------------------------------------------------------------------------------------------

    dragRepositionStart : function() {
        var retVal;
        if (this.creator.dragRepositionStart) retVal = this.creator.dragRepositionStart();
        // Show snap grid
        this._showSnapGrid(true);

        return retVal;
    },

    dragRepositionStop : function() {
        var retVal;
        if (this.creator.dragRepositionStop) retVal = this.creator.dragRepositionStop();
        // Hide snap grid
        this._showSnapGrid(false);

        return retVal;
    },

    dragResizeStart : function() {
        var retVal;
        if (this.creator.dragResizeStart) retVal = this.creator.dragResizeStart();
        // Show snap grid
        this._showSnapGrid(true);

        return retVal;
    },

    dragResizeStop : function() {
        var retVal;
        if (this.creator.dragResizeStop) retVal = this.creator.dragResizeStop();
        // Hide snap grid
        this._showSnapGrid(false);

        return retVal;
    },

    // Selection
    // ---------------------------------------------------------------------------------------

    //> @attr editProxy.selectedAppearance (SelectedAppearance : null : IR)
    // Appearance that is applied to selected component. Default value is determined from
    // +link{editContext.selectedAppearance}.
    // <P>
    // When value is <code>null</code> the appearance is determined by:
    // <ul>
    // <li>If multiple selection is enabled, "tintMask" is used</li>
    // <li>Otherwise, "outlineMask" is used 
    // </ul>
    // @visibility external
    // @see editProxy.selectedBorder
    // @see editProxy.selectedTintColor
    // @see editProxy.selectedTintOpacity
    //<

    //> @attr editProxy.selectedBorder (String : null : IR)
    // Set the CSS border to be applied to the selection outline of the selected components.
    // Default value is determined from +link{editContext.selectedBorder}.
    // This property is used when +link{editProxy.selectedAppearance} is <code>outlineMask</code>
    // or <code>outlineEdges</code>.
    //
    // @visibility external
    //<

    //> @attr editProxy.selectedLabelBackgroundColor (String : null : IR)
    // The background color for the selection outline label. The
    // default is defined on +link{SelectionOutline} or +link{editContext.selectedLabelBackgroundColor}.
    // <P>
    // NOTE: A selected component label is only supported when
    // +link{editProxy.selectedAppearance,selectedAppearance} is "outlineEdges".
    //
    // @visibility external
    //<

    //> @attr editProxy.selectedTintColor (CSSColor : null : IR)
    // Mask color applied to +link{editProxy.editMask,editMask} of selected component when
    // +link{editProxy.selectedAppearance,selectedAppearance} is "tintMask".
    // Default value is determined from +link{editContext.selectedTintColor}.
    // @visibility external
    // @see editProxy.selectedTintOpacity
    //<

    //> @attr editProxy.selectedTintOpacity (Number : null : IR)
    // Opacity applied to +link{editProxy.editMask,editMask} of selected component when
    // +link{editProxy.selectedAppearance,selectedAppearance} is "tintMask".
    // @visibility external
    // @see editProxy.selectedTintColor
    //<

    click : function () {
        if (this.creator.editNode) {
            isc.EditContext.selectCanvasOrFormItem(this.creator, true);
            return isc.EH.STOP_BUBBLING;
        }
    },

    getAllSelectableComponents : function () {
        var liveObject = this.creator;

        if (!liveObject.children) return null;
        var components = [];
        for (var i = 0; i < liveObject.children.length; i++) {
            var child = this.deriveSelectedComponent(liveObject.children[i]);
            if (child) components.add(child);
        }
        return components;
    },

    _$tintMask:"tintMask",
    _$outlineMask:"outlineMask",
    _$outlineEdges:"outlineEdges",
    _getSelectedAppearance : function () {
        if (this.selectedAppearance) return this.selectedAppearance;
        return (this.creator.editContext.selectionType == isc.Selection.MULTIPLE ? this._$tintMask : this._$outlineMask);
    },

    //> @method editProxy.showSelectedAppearance
    // This method applies the +link{editProxy.selectedAppearance,selectedAppearance} to the selected component
    // or resets it to the non-selected appearance. Override this method to create a custom
    // appearance.
    //
    // @param show (boolean) true to show component as selected, false otherwise
    // @visibility external
    //<
    showSelectedAppearance : function (show, hideLabel, showThumbsOrDragHandle) {
        var undef,
            mode = this._getSelectedAppearance()
        ;

        if (mode == this._$tintMask) {
            var editMask = this.getEditMask();
            if (!editMask || editMask.destroyed) return;

            // Save original background color and opacity
            if (editMask._originalBackgroundColor === undef) {
                editMask._originalBackgroundColor = (editMask.backgroundColor === undef ? null : editMask.backgroundColor);
            }
            if (editMask._originalOpacity === undef) {
                editMask._originalOpacity = (editMask.opacity === undef ? null : editMask.opacity);
            }

            // Set or reset background color
            if (show && this.selectedTintColor != editMask.backgroundColor) {
                editMask.setBackgroundColor(this.selectedTintColor);
            } else if (!show && editMask._originalBackgroundColor != editMask.backgroundColor) {
                editMask.setBackgroundColor(editMask._originalBackgroundColor);
            }

            // Set or reset opacity
            if (show && this.selectedTintOpacity != editMask.opacity) {
                editMask.setOpacity(this.selectedTintOpacity);
            } else if (!show && editMask._originalOpacity != editMask.opacity) {
                editMask.setOpacity(editMask._originalOpacity);
            }

            // Show/hide thumbs
            if (show && showThumbsOrDragHandle) isc.EditProxy.showResizeThumbs(editMask);
            else isc.EditProxy.hideResizeThumbs();

            // If not selected, make sure editMask is pushed back just
            // above the component.
            if (!show) editMask.moveAbove(this.creator);
        } else if (mode == this._$outlineMask) {
            var editMask = this.getEditMask();
            if (!editMask || editMask.destroyed) return;
            
            // Save original border
            if (editMask._originalBorder === undef) {
                editMask._originalBorder = (editMask.border === undef ? null : editMask.border);
            }

            // Set or reset border
            if (show && this.selectedBorder != editMask.border) {
                editMask.setBorder(this.selectedBorder);
            } else if (!show && editMask._originalBorder != editMask.border) {
                editMask.setBorder(editMask._originalBorder);
            }

            // Show/hide thumbs
            if (show && showThumbsOrDragHandle) isc.EditProxy.showResizeThumbs(editMask);
            else isc.EditProxy.hideResizeThumbs();

            // If not selected, make sure editMask is pushed back just
            // above the component.
            if (!show) editMask.moveAbove(this.creator);
        } else if (mode == this._$outlineEdges) {
            var object = this.creator;

            if (show) {
                var underlyingObject,
                    label;
                if (object._visualProxy) {
                    var type = object.type || object._constructor;
                    label = "[" + type + " " + (object.name ? "name:" : "ID");
                    label += object.name || object.ID;
                    label += "]";
                    underlyingObject = object;
                    object = object._visualProxy;
                }

                var editContext = this.creator.editContext,
                    showLabel = !hideLabel
                ;

                // Update SelectionOutline with this context's properties
                if (this.selectedBorder) isc.SelectionOutline.border = this.selectedBorder;
                if (editContext.selectedLabelBackgroundColor) isc.SelectionOutline.labelBackgroundColor = editContext.selectedLabelBackgroundColor;

                // Disable selection label if context has it disabled
                if (editContext.showSelectedLabel == false) showLabel = false;

                // Allow context user to override the selectionLabel text
                if (showLabel != false && !label && editContext.getSelectedLabelText) {
                    label = editContext.getSelectedLabelText(object);
                }
                isc.SelectionOutline.select(object, false, showLabel, label, this.getResizeEdges());

                // Show drag handle (except on TabBar controls)
                
                if (showThumbsOrDragHandle && !isc.isA.TabBar(object.parentElement)) {
                    isc.SelectionOutline.showDragHandle();
                }
                if (this.overrideDragProperties) this.overrideDragProperties();
            } else if (isc.SelectionOutline.getSelectedObject() == object) {
                isc.SelectionOutline.deselect();
                if (this.restoreDragProperties) this.restoreDragProperties();
            }
        }
    },

    getResizeEdges : function () {
        // If parent component is a H/VLayout or Stack configure the highlight to
        // allow resizing of the component from along the length axis.
        var liveObject = this.creator,
            editContext = liveObject.editContext,
            node = liveObject.editNode,
            parentNode = liveObject.editContext.getEditNodeTree().getParent(node),
            resizeFrom
        ;
        if (parentNode) {
            var parentLiveObject = parentNode.liveObject;
            if (parentLiveObject) {
                if (isc.isA.Layout(parentLiveObject)) {
                    var vertical = parentLiveObject.vertical,
                        fill = ((vertical ? parentLiveObject.vPolicy : parentLiveObject.hPolicy) == isc.Layout.FILL),
                        childCount = parentLiveObject.getMembers().length,
                        objectIndex = parentLiveObject.getMemberNumber(liveObject),
                        lastMember = (objectIndex == (childCount-1)),
                        canResize = (!fill || !lastMember)
                    ;
                    if (canResize) {
                        resizeFrom = (vertical ? "B" : "R");
                    }
                }
                if (parentLiveObject.editProxy) {
                    if ((editContext.persistCoordinates == null && parentLiveObject.editProxy.persistCoordinates) ||
                            (editContext.persistCoordinates && parentLiveObject.editProxy.persistCoordinates != false))
                    {
                        resizeFrom = ["B", "R"];
                    }
                }
            }
        }
        return resizeFrom;
    },

    // Inline edit handling
    // ---------------------------------------------------------------------------------------

    doubleClick : function () {
        var liveObject = this.creator;

        if (this.supportsInlineEdit && 
                liveObject.editContext.enableInlineEdit &&
                (this.inlineEditEvent == "doubleClick" || this.inlineEditEvent == "dblOrKeypress"))
        {
            this.startInlineEditing();
        }
        return isc.EH.STOP_BUBBLING;
    },

    // Drag/drop method overrides
    // ---------------------------------------------------------------------------------------

    willAcceptDrop : function (changeObjectSelection) {
        var liveObject = this.creator;
        this.logInfo("editProxy.willAcceptDrop for " + liveObject.ID, "editModeDragTarget");
        var dragData = liveObject.ns.EH.dragTarget.getDragData(),
            dragType,
            draggingFromPalette = true;

        // If dragData is null, this is probably because we are drag-repositioning a component
        // in a layout - the dragData is the component itself
        if (dragData == null || (isc.isAn.Array(dragData) && dragData.length == 0)) {
            draggingFromPalette = false;
            this.logInfo("dragData is null - using the dragTarget itself", "editModeDragTarget");
            dragData = liveObject.ns.EH.dragTarget;
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

        var hiliteCanvas = this.findEditNode(dragType);

        var canAdd = this.canAdd(dragType);

        // If canAdd is false, then we conclusively deny the add, without checking parents
        if (canAdd === false) return false;

        // If canAdd is falsy but not false (i.e. null or undefined), then we
        // check ancestors which are in editMode, to see if they can accept the
        // drop.
        
        if (dragType == null || !canAdd) {
            this.logInfo(liveObject.ID + " does not accept drop of type " + dragType, "editModeDragTarget");

            var ancestor = liveObject.parentElement;
            while (ancestor && !ancestor.editorRoot) {
                if (ancestor.editingOn) {
                    // Note that this may itself recurse to further ancestors ...
                    // thus, once it returns, all ancestors have been checked.
                    var ancestorAcceptsDrop = ancestor.editProxy.willAcceptDrop();
                    if (!ancestorAcceptsDrop) {
                        this.logInfo("No ancestor accepts drop", "editModeDragTarget");
                        if (changeObjectSelection != false) {
                            if (hiliteCanvas && hiliteCanvas.editProxy) {
                                hiliteCanvas.editProxy.showSelectedAppearance(false);
                            }
                            this.setNoDropIndicator();
                        }
                        // Pass through the null or false response
                        return ancestorAcceptsDrop;
                    }
                    this.logInfo("An ancestor accepts drop", "editModeDragTarget");
                    return true;
                }
                // Note that the effect of the return statements in the
                // condition above is that we'll stop walking
                // the ancestor tree at the first parent where editingOn is true ...
                // at that point, we'll re-enter editProxy.willAcceptDrop
                ancestor = ancestor.parentElement;
            }

            // Given the return statements in the while condition above, we'll only get
            // here if no ancestor had editingOn: true
            this.logInfo(liveObject.ID + " has no parentElement in editMode", "editModeDragTarget");
            if (changeObjectSelection != false) {
                if (hiliteCanvas && hiliteCanvas.editProxy) {
                    hiliteCanvas.editProxy.showSelectedAppearance(false);
                }
                this.setNoDropIndicator();
            }

            // The effect of returning "false" here (rather than "null"), is
            // that we don't let the potential drop bubble outside of the
            // ancestors that are in editMode. That is, if the EditContext as a
            // whole can't handle the drop, we indicate to callers that it
            // shouldn't bubble to ancestors of the EditContext.
            return false;
        }

        // This canvas can accept the drop, so select its top-level parent (in case it's a 
        // sub-component like a TabSet's PaneContainer)
        this.logInfo(liveObject.ID + " is accepting the " + dragType + " drop", "editModeDragTarget");
        if (hiliteCanvas) {
            if (changeObjectSelection != false) {
                this.logInfo(liveObject.ID + ": selecting editNode object " + hiliteCanvas.ID);
                if (hiliteCanvas.editProxy) {
                    hiliteCanvas.editProxy.showSelectedAppearance(true, false);
                    hiliteCanvas.editProxy.clearNoDropIndicator();
                }
            }
            return true;
        } else {
            this.logInfo("findEditNode() returned null for " + liveObject.ID, "editModeDragTarget");
        }
        
        
        if (changeObjectSelection != false) {
            this.logInfo("In editProxy.willAcceptDrop, '" + liveObject.ID + "' was willing to accept a '" + 
                     dragType + "' drop but we could not find an ancestor with an editNode");
        }
        return true;
    }, 

    // Can a component be dropped at this level in the hierarchy?
    canDropAtLevel : function () {
        var liveObject = this.creator,
            editContext = liveObject.editContext,
            rootNode = editContext.getRootEditNode(),
            rootObject = editContext.getLiveObject(rootNode)
        ;
        
        return this.allowNestedDrops != false && 
            (editContext.allowNestedDrops != false || liveObject == rootObject);
    },

    // Override to provide special editNode canvas selection (note that this impl does not 
    // care about dragType, but some special implementations - eg, TabSet - return different
    // objects depending on what is being dragged)
    findEditNode : function (dragType) {
        var liveObject = this.creator;
        if (!liveObject.editNode) {
            this.logInfo("Skipping '" + liveObject + "' - has no editNode", "editModeDragTarget");
            if (liveObject.parentElement && 
                liveObject.parentElement.editProxy && 
                liveObject.parentElement.editProxy.findEditNode) 
            {
                return liveObject.parentElement.editProxy.findEditNode(dragType);
            } else {
                return null;
            }
        }
        return liveObject;
    },

    // Tests whether this Canvas can accept a child of type "type".  If it can't, and "type"
    // names some kind of FormItem, then we'll accept it if this Canvas is willing to accept a
    // child of type "DynamicForm" -- we'll cope with this downstream by auto-wrapping the
    // dropped FormItem inside a DynamicForm that we create for that very purpose.  Similarly,
    // if the type represents some type of DrawItem then we'll accept the child if this Canvas
    // can contain a DrawPane.
    
    _excludedFields: {
        "children": true,
        "peers": true
    },
    canAdd : function (type) {
        if (!this.canDropAtLevel()) return false;
        var liveObject = this.creator;
        if (liveObject.getObjectField(type) == null) {
            var clazz = isc.ClassFactory.getClass(type);
            if (clazz) {
                if (clazz.isA("FormItem")) {
                    return (liveObject.getObjectField("DynamicForm", this._excludedFields) != null);
                } else if (clazz.isA("DrawItem")) {
                    return (liveObject.getObjectField("DrawPane", this._excludedFields) != null);
                }
            }
            // By default, return null to indicate that we can't add the item,
            // but callers may wish to check our parent. Subclasses can return
            // "false" to suggest to callers that they should not check parents
            // ...  that is, that we "claim" the potential add and conclusively
            // reject it. This matches the semantics of willAcceptDrop()
            return null;
        } else {
            return true;
        }
    },

    // Canvas.clearNoDropindicator no-ops if the internal _noDropIndicator flag is null.  This
    // isn't good enough in edit mode because a canvas can be dragged over whilst the no-drop
    // cursor is showing, and we want to revert to a droppable cursor regardless of whether 
    // _noDropIndicatorSet has been set on this particular canvas. 
    clearNoDropIndicator : function (type) {
        var liveObject = this.creator;
        if (liveObject._noDropIndicatorSet) delete liveObject._noDropIndicatorSet;
        liveObject._updateCursor();
        
        // XXX May need to add support for no-drop drag tracker here if we ever implement 
        // such a thing in Visual Builder
    },
    
    // Special editMode version of setNoDropCursor - again, because the base version no-ops in 
    // circumstances where we need it to refresh the cursor.
    setNoDropIndicator : function () {
        var liveObject = this.creator;
        liveObject._noDropIndicatorSet = true;
        liveObject._applyCursor(liveObject.noDropCursor);
    },

    

    defaultDropMargin: 10,
    dropMargin: 10,
    updateDropMargin : function () {

        // Fix up the dropMargin to prevent not-very-tall canvas from passing *every* drop 
        // through to parent layouts
        var liveObject = this.creator,
            newDropMargin = this.defaultDropMargin;
        if (newDropMargin * 2 > liveObject.getVisibleHeight() - isc.EditProxy.minimumDropTargetSize) {
            newDropMargin = Math.round((liveObject.getVisibleHeight() - isc.EditProxy.minimumDropTargetSize) / 2);
            if (newDropMargin < isc.EditProxy.minimumDropMargin) newDropMargin = isc.EditProxy.minimumDropMargin; 
        }
        this.dropMargin = newDropMargin;
    },

    shouldPassDropThrough : function () {
        var liveObject = this.creator,
            source = isc.EH.dragTarget,
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

        // If we do not have an editable parent willing to accept the drop, then
        // return false (i.e. we should *not* pass the drop through).
        if (liveObject.parentElement == null ||
            liveObject.parentElement.editProxy == null ||
            !liveObject.parentElement.editProxy.willAcceptDrop(false))
        {
            this.logInfo(liveObject.ID + " is not passing drop through - no ancestor is willing to " + 
                        "accept the drop", "editModeDragTarget");
            return false;
        }

        var x = isc.EH.getX(),
            y = isc.EH.getY(),
            work = liveObject.getPageRect(),
            rect = {
                left: work[0], 
                top: work[1], 
                right: work[0] + work[2], 
                bottom:work[1] + work[3]
            }
            
        if (!liveObject.orientation || liveObject.orientation == "vertical") {
            if (x < rect.left + this.dropMargin  || x > rect.right - this.dropMargin) {
                this.logInfo("Close to right or left edge - passing drop through to parent for " +
                        liveObject.ID, "editModeDragTarget");
                return true;
            }
        }
        if (!liveObject.orientation || liveObject.orientation == "horizontal") {
            if (y < rect.top + this.dropMargin  || y > rect.bottom - this.dropMargin) {
                this.logInfo("Close to top or bottom edge - passing drop through to parent for " + 
                        liveObject.ID, "editModeDragTarget");
                return true;
            }
        }

        this.logInfo(liveObject.ID + " is not passing drop through", "editModeDragTarget");
        return false;
    },
    
    
    drop : function () {
        if (this.shouldPassDropThrough()) {
            return;
        }
    
        var liveObject = this.creator,
            source = isc.EH.dragTarget,
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

        // If node is dropped from a tree, clean it of internal properties
        if (source.isA("TreeGrid")) {
            paletteNode = source.data.getCleanNodeData([paletteNode], false, false, false)[0];
        }

        // Palette node could be modified later if there are palettized components within.
        // Copy it now so that future drops are not affected.
        paletteNode = isc.clone(paletteNode);

        // if the source isn't a Palette, we're drag/dropping an existing component, so remove the 
        // existing component and re-create it in its new position
        if (!source.isA("Palette")) {
            isc.SelectionOutline.hideDragHandle();
            if (source == liveObject) return;  // Can't drop a component onto itself
            var editContext = liveObject.editContext,
                editNode = liveObject.editNode,
                tree = editContext.getEditNodeTree(),
                oldParent = tree.getParent(source.editNode);
            editContext.removeNode(source.editNode);
            var node;
            if (source.isA("FormItem")) {
                if (source.isA("CanvasItem")) {
                    node = editContext.addNode(source.canvas.editNode, editNode);
                } else {
                    node = editContext.addWithWrapper(source.editNode, editNode);
                }
            } else if (source.isA("DrawItem")) {
                node = editContext.addWithWrapper(source.editNode, editNode, true);
            } else {
                node = editContext.addNode(source.editNode, editNode);
                // Assign position based on the dragRect because the mouse pointer is
                // likely offset from there into what was the dragHandle and we want
                // the drop to occur where the target outline shows
                var dragRect = isc.EH.getDragRect(),
                    x = (dragRect ? dragRect[0] - liveObject.getPageLeft() : liveObject.getOffsetX()),
                    y = (dragRect ? dragRect[1] - liveObject.getPageTop() : liveObject.getOffsetY())
                ;
                node.liveObject.moveTo(x, y);
            }
            if (node && node.liveObject) {
                isc.EditContext.selectCanvasOrFormItem(node.liveObject, true);
            }
        } else {
        	var skipSnapToGrid = isc.EH.shiftKeyDown();
            // loadData() operates asynchronously, so we'll have to finish the item drop off-thread
            if (paletteNode.loadData && !paletteNode.isLoaded) {
                var _this = this;
                paletteNode.loadData(paletteNode, function (loadedNode) {
                    loadedNode = loadedNode || paletteNode;
                    loadedNode.isLoaded = true;
                    _this.completeItemDrop(loadedNode, skipSnapToGrid)
                    loadedNode.dropped = paletteNode.dropped;
                });
                return isc.EH.STOP_BUBBLING;
            }

            this.completeItemDrop(paletteNode, skipSnapToGrid);
        }
        return isc.EH.STOP_BUBBLING;
    },

    completeItemDrop : function (paletteNode, skipSnapToGrid) {
        var liveObject = this.creator;

        if (!liveObject.editContext) return;
        
        var editContext = liveObject.editContext,
            nodeType = paletteNode.type || paletteNode.className,
            editNode,
            wrapped = false
        ;
        var clazz = isc.ClassFactory.getClass(nodeType);
        if (clazz && (clazz.isA("FormItem") || clazz.isA("DrawItem"))) {
            editNode = editContext.makeEditNode(paletteNode);
            if (clazz && clazz.isA("FormItem")) {
                editNode = liveObject.editContext.addWithWrapper(editNode, liveObject.editNode);
            } else {
                editNode = liveObject.editContext.addWithWrapper(editNode, liveObject.editNode, true);
            }
            wrapped = true;
        } else {
            var nodes = editContext.addFromPaletteNodes([paletteNode], liveObject.editNode);
            if (nodes && nodes.length > 0) editNode = nodes[0];
        }
        // move new component to the current mouse position.
        // if editNode was wrapped, update the wrapper node position
        var node = editNode;
        if (wrapped) {
            var tree = editContext.getEditNodeTree(),
                parent = tree.getParent(node)
            ;
            if (parent) node = parent;
        }
        var x = liveObject.getOffsetX(),
            y = liveObject.getOffsetY()
        ;
        // Respect snapTo grid if specified 
        if (liveObject.childrenSnapToGrid && !skipSnapToGrid) {
            x = liveObject.getHSnapPosition(x);
            y = liveObject.getVSnapPosition(y);
        }
        if (node.liveObject && node.liveObject.moveTo) node.liveObject.moveTo(x, y);
        if (this.canSelectChildren && editNode.liveObject.editProxy != null &&
            editNode.liveObject.editProxy.canSelect != false)
        {
            editContext.selectSingleComponent(node.liveObject);
        }

        // Let node's proxy know that it has just been dropped in place
        if (node.liveObject && node.liveObject.editProxy && node.liveObject.editProxy.nodeDropped) {
            node.liveObject.editProxy.nodeDropped();
        }
    },

    dropMove : function () {
        if (!this.canDropAtLevel()) return;

        if (!this.willAcceptDrop()) return false;
        if (!this.shouldPassDropThrough()) {
            
            if (this.creator.dropMove && this.creator.getClass() != isc.Canvas &&
                    this.creator.getClass() != isc.EditPane && this.creator.getClass() != isc.TabSet &&
                    this.creator.getClass() != isc.DetailViewer)
            {
                this.creator.Super("dropMove", arguments);
            }
            var liveObject = this.creator,
                parentElement = liveObject.parentElement;
            if (parentElement && parentElement.hideDropLine) {
                parentElement.hideDropLine();
                if (parentElement.isA("FormItem")) {
                    parentElement.form.hideDragLine();
                } else if (parentElement.isA("DrawItem")) {
                    parentElement.drawPane.hideDragLine();
                }
            }
            return isc.EH.STOP_BUBBLING;
        }
    },

    dropOver : function () {
        if (!this.canDropAtLevel()) return;

        if (!this.willAcceptDrop()) return false;
        if (!this.shouldPassDropThrough()) {
            if (this.creator.dropMove && this.creator.getClass() != isc.Canvas &&
                    this.creator.getClass() != isc.EditPane && this.creator.getClass() != isc.DrawPane && 
                    this.creator.getClass() != isc.TabSet && this.creator.getClass() != isc.DetailViewer)
            {
                this.creator.Super("dropOver", arguments);
            }
            var liveObject = this.creator,
                parentElement = liveObject.parentElement;
            if (parentElement && parentElement.hideDropLine) {
                parentElement.hideDropLine();
                if (parentElement.isA("FormItem")) {
                    parentElement.form.hideDragLine();
                } else if (parentElement.isA("DrawItem")) {
                    parentElement.drawPane.hideDragLine();
                }
            }
            // Show snap grid
            this._showSnapGrid(true);

            return isc.EH.STOP_BUBBLING;
        }
        // Show snap grid
        this._showSnapGrid(true);
    },

    dropOut : function () {
        this.showSelectedAppearance(false);
	    if (this.creator.dropOut) this.creator.dropOut();
        // Hide snap grid
        this._showSnapGrid(false);
    },

    // In editMode, we allow dragging the selected canvas using the drag-handle
    // This involves overriding some default behaviors at the widget level.
    overrideDragProperties : function () {
        if (this._overrideDrag) return;
        var editContext = this.creator.editContext;
        var properties = {
            canDrop: true,
            dragAppearance: "outline",
            // These method overrides are to clobber special record-based drag handling
            // implemented by ListGrid and its children
            dragStart : function () { return true; },
            dragMove : function () { return true; },
            setDragTracker : function () {isc.EH.setDragTracker(""); return false; },
            dragStop : function () {
                isc.SelectionOutline.hideProxyCanvas();
                isc.SelectionOutline.positionDragHandle();
            }
        };
 
        this.overrideProperties(properties);
        this._overrideDrag = true;
    },
    
    restoreDragProperties : function () {
        this.creator.restoreFromOriginalValues([
            "canDrag", 
            "canDrop",
            "dragAppearance",
            "dragStart",
            "dragMove",
            "dragStop",
            "setDragTracker"
        ]);
        this._overrideDrag = false;
    },

    _showSnapGrid : function (show) {
        var liveObject = this.creator;
        if (liveObject.childrenSnapToGrid || liveObject.childrenSnapResizeToGrid) {
            liveObject.setShowSnapGrid(show);
        }
    },

    // DataBoundComponent functionality
    // ---------------------------------------------------------------------------------------

    // In editMode, when setDataSource is called, generate editNodes for each field so that the
    // user can modify the generated fields.
    // On change of DataSource, remove any auto-gen field that the user has not changed.
    
    setDataSource : function (dataSource, fields, forceRebind) {
        //this.logWarn("editProxy.setDataSource called" + isc.Log.getStackTrace());

        var liveObject = this.creator;

        // _loadingNodeTree is a flag set by Visual Builder - its presence indicates that we are 
        // loading a view from disk.  In this case, we do NOT want to perform the special 
        // processing in this function, otherwise we'll end up with duplicate components in the
        // componentTree.  So we'll just fall back to the base impl in that case.
        if (isc._loadingNodeTree) {
            liveObject.setDataSource(dataSource, fields);
            return;
        }

        if (dataSource == null) return;
        if (dataSource == liveObject.dataSource && !forceRebind) return;

        var fields = liveObject.getFields(),
            keepFields = [],
            removeNodes = [];

        // remove all automatically generated fields that have not been edited by the user
        
        if (fields) {
            var tree = liveObject.editContext.getEditNodeTree(),
                parentNode = tree.findById(liveObject.ID),
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

                if (editNode && editNode.autoGen && !this.fieldEdited(liveObject, editNode)) {
                    removeNodes.add(editNode);
                } else if (editNode) {
                    var keepField = isc.addProperties({}, field.editingOn ? field.editNode.defaults : field);
                    keepFields.add(field);
                }
            }

            liveObject.setFields(keepFields);

            // See GridEditProxy.setInlineEditText for details
            if (!this._skipAddDefaultFields) {
                for (var i = 0; i < removeNodes.length; i++) {
                    liveObject.editContext.removeNode(removeNodes[i], true);
                }
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
            if (!liveObject.shouldUseField(field, dataSource)) continue;
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

        liveObject.setDataSource(dataSource, keepFields);

        // See GridEditProxy.setInlineEditText for details
        if (!this._skipAddDefaultFields) {
            for (var key in fields) {
                var field = fields[key];
    
                // What constitutes a "field" varies by DBC type
                var fieldConfig = this.makeFieldPaletteNode(field, schema);
                var editNode = liveObject.editContext.makeEditNode(fieldConfig);
                //this.logWarn("editProxy.setDataSource adding field: " + field.name);
                liveObject.editContext.addNode(editNode, liveObject.editNode, null, null, true);
            }
            //this.logWarn("editProxy.setDataSource done adding fields");
        }
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

    // Makes a palette node for a DataSourceField
    makeFieldPaletteNode : function (field, dataSource, defaults) {
        
        // works for ListGrid, TreeGrid, DetailViewer, etc.  DynamicForm overrides
        var fieldType = this.creator.Class + "Field",
            defaults = isc.addProperties({}, defaults, {name: field.name})
        ;
        var paletteNode = {
            type: fieldType,
            autoGen: true,
            defaults: defaults
        };

        // install a type if one is present in DSF
        if (field.type) defaults.type = field.type;
        else     delete defaults.type;

        // XXX this makes the code more verbose since the title could be left blank and be
        // inherited from the DataSource.  However if we don't supply one here, currently
        // the process of creating an editNode and adding to the editTree generates a title
        // anyway, and without using getAutoTitle().
        if (field.title || !defaults.title) {
            defaults.title = field.title || dataSource.getAutoTitle(field.name);
        }
        return paletteNode;
    },

    // Component editor handling
    // ---------------------------------------------------------------------------------------

    //> @attr editProxy.supportsInlineEdit (Boolean : true : IR)
    // Whether this EditProxy has an inline edit behavior, which allows an end user to
    // configure a component by editing a simple text representation of its configuration.  
    // <p>
    // For example, when inline edit is enabled, a +link{SelectItem} allows
    // +link{selectItemEditProxy.getInlineEditText,editing its valueMap} as a comma-separated
    // string, and a +link{ListGrid}'s columns and data can be edited as several lines of
    // comma-separated headings and data values.
    // <p>
    // See +link{editProxy.inlineEditEvent} for more details and configuration options.
    //
    // @visibility external
    //<
    supportsInlineEdit: true,

    //> @attr editProxy.inlineEditOnDrop (Boolean : null : IR)
    // Should the inline editor be shown when new component is first dropped?
    //
    // @visibility external
    //<

    //> @attr editProxy.inlineEditEvent (InlineEditEvent : null : IR)
    // Event that triggers inline editing, showing the +link{inlineEditForm}, which consists of a single
    // text input (single or multi-line according to +link{inlineEditMultiline}) shown in the
    // +link{inlineEditForm} AutoChild.
    // <p>
    // The initial value in the form comes from +link{getInlineEditText()} and is applied via
    // +link{setInlineEditText()}.
    // <p>
    // Many +link{EditProxy} subclasses have built-in modes for inline editing.
    //
    // @visibility external
    //<
    inlineEditEvent: "doubleClick",

    //> @type InlineEditEvent
    // Event that will trigger inline editing.  See +link{editProxy.inlineEditEvent}.
    //
    // @value "click"             A single mouse click triggers inline editing
    // @value "doubleClick"       A double click triggers inline editing
    // @value "none"              No mouse event will trigger inline editing, but it can still
    //                            be triggered by a call to +link{EditProxy.startInlineEditing()}.
    // @value "dblOrKeypress"     A double click triggers inline editing.  In addition, <i>if
    //                            the widget is selected</i>, starting to type triggers inline editing.
    //
    // @group editing
    // @visibility external
    //<

    //> @method editProxy.startInlineEditing() 
    // Manual means of triggering inline editing.  See +link{inlineEditEvent}.
    //
    // @param [appendChar] (String) optional String to append to current value as editing starts
    // @visibility external
    //<
    _$editField:"edit",
    startInlineEditing : function (appendChar, key) {
        if (!this.supportsInlineEdit || !this.creator.editContext.enableInlineEdit) return;

        var form = this.createInlineEditForm(),
            value = this.getInlineEditText(),
            isBackspace = key === "Backspace"
        ;
        if (appendChar != null) value = (value ? value + appendChar : appendChar);
        else if (isBackspace) {
            if (!value) value = "";
            else {
                value = String(value);
                value = value.substring(0, value.length - 1);
            }
        }

        form.setValues({ edit: value });
        this.inlineEditForm = form;

        // Create or clear editor layout
        if (!this.inlineEditLayout) {
            this.inlineEditLayout = this.createInlineEditLayout();
        } else if (this.inlineEditLayout.getMembers().length > 0) {
            this.inlineEditLayout.removeMembers(this.inlineEditLayout.getMembers());
        }

        var editor = this.inlineEditLayout;
        editor.addMember(form);

        if (this.inlineEditInstructions) {
            // Initialize style from Hover on first use 
            if (!this.inlineEditInstructionLabelDefaults.baseStyle) {
                isc.EditProxy.changeDefaults("inlineEditInstructionLabelDefaults", {
                    baseStyle: isc.Hover.hoverCanvasDefaults.baseStyle
                });
            }

            if (!this.inlineEditInstructionLabel) {
                this.inlineEditInstructionLabel = this.createInlineEditInstructionLabel();
            }
            this.inlineEditInstructionLabel.setContents(this.inlineEditInstructions);

            editor.addMember(this.inlineEditInstructionLabel);
        }

        this.positionAndSizeInlineEditor();

        editor.show();

        // Configure click mask around editor so it can be closed when
        // clicking outside of it
        editor.showClickMask(
                {
                    target: editor,
                    methodName: "dismissEditor"
                },
                "soft",
                // Don't mask editor
                [editor]
        );
        
        var item = form.getItem(this._$editField);
        if (item) {
            item.focusInItem();

            if (appendChar || isBackspace) {
                var valueLength = (value != null ? value.length : 0);
                item.delayCall("setSelectionRange", [valueLength, valueLength]);
            } else {
                item.delayCall("selectValue");
            }
        }
    },

    createInlineEditLayout : function () {
        return isc.VStack.create({
            dismissEditor : function () {
                // Automatic blur event on form will save value if needed
                this.hide();
            }
        });
    },

    createInlineEditForm : function () {
        var editFieldConfig =  isc.addProperties(
            {
                name: this._$editField,
                type: (this.inlineEditMultiline ? "TextArea" : "text"),
                allowNativeResize: this.inlineEditMultiline,
                width: "*", height: "*",
                showTitle: false
            },
            {
                keyPress : function (item, form, keyName) {
                    if (keyName == "Escape") {
                        form.discardUpdate = true;
                        form.parentElement.hide();
                    } else if (keyName == "Enter") {
                        if (!isc.isA.TextAreaItem(item)) item.blurItem();
                    }
                }, 
                blur : function (form, item) {
                    form.saveOrDiscardValue();
                    form.parentElement.hide();
                    if (form.creator.inlineEditingComplete) form.creator.inlineEditingComplete();
                }
            }
        );

        var form = this.createAutoChild("inlineEditForm", {
            margin: 0, padding: 0, cellPadding: 0,
            // set a min width larger than the Framework default for reasonable editing space
            minWidth: (this.inlineEditMultiline ? 250 : 80),
            fields: [editFieldConfig],
            saveOrDiscardValue : function () {
                if (!this.discardUpdate) {
                    var value = this.getValue(this.creator._$editField);
                    this.creator.setInlineEditText(value);
                }
            }
        });

        return form;
    },

    createInlineEditInstructionLabel : function () {
        return this.createAutoChild("inlineEditInstructionLabel");
    },

    positionAndSizeInlineEditor : function () {
        this.positionInlineEditor();
        this.sizeInlineEditor();
    },

    positionInlineEditor : function () {
        var liveObject = this.creator,
            left = liveObject.getPageLeft(),
            top = liveObject.getPageTop()
        ;
        this.inlineEditLayout.moveTo(left, top);
    },

    sizeInlineEditor : function () {
        var liveObject = this.creator,
            layout = this.inlineEditLayout,
            width = liveObject.getVisibleWidth(),
            minWidth = this.inlineEditForm.minWidth || 1,
            height = liveObject.getVisibleHeight(),
            minHeight = this.inlineEditForm.minHeight || 1
        ;

        // Adjust width and height for minimum
        width = Math.max(width, minWidth);
        if (this.inlineEditMultiline) height = Math.min(Math.max(height, 50), 200);
        else height = minHeight;

        layout.setWidth(width);
        this.inlineEditForm.setHeight(height);
    },

    // Method called when inline editing completes (save or cancel).
    // Can be observed to perform operation upon completion.
    inlineEditingComplete : function () { },

    //> @attr editProxy.inlineEditForm (MultiAutoChild DynamicForm : null : IR)
    // See +link{editProxy.inlineEditEvent}.
    //
    // @visibility external
    //<
    inlineEditFormConstructor: "DynamicForm",
    inlineEditFormDefaults: {
        minWidth: 80,
        minHeight: 20,
        numCols: 1
    },

    //> @attr editProxy.inlineEditInstructionLabel (AutoChild Label : null : IR)
    // Label AutoChild used to display +link{inlineEditInstructions} below the text entry
    // area if provided. Defaults to the same styling as the system +link{Hover}.
    //
    // @visibility external
    //<
    inlineEditInstructionLabelConstructor: "Label",
    inlineEditInstructionLabelDefaults: {
        height: 10  // Small height to allow auto-fit vertically
    },

    //> @attr editProxy.inlineEditInstructions (HTMLString : null : IR)
    // Instructions that appear below the text entry area if inline editing is enabled.  See
    // +link{editProxy.inlineEditEvent} and +link{editProxy.inlineEditInstructionLabel}.
    //
    // @visibility external
    //<

    //> @attr editProxy.inlineEditMultiline (Boolean : false : IR)
    // Whether inline editing should be single or multi-line.
    // <p>
    // Single-line input appears at the control's top-left corner, multiline covers the control.
    //
    // @visibility external
    //<
    inlineEditMultiline: false,

    //> @method editProxy.getInlineEditText()
    // Returns the text based on the current component state to be edited inline.
    // Called by the +link{inlineEditForm} to obtain the starting edit value.
    // <p>
    // For a canvas with <code>isGroup</code> enabled, the <code>groupTitle</code>
    // is returned. Otherwise the <code>contents</code> is returned.
    //
    // @visibility external
    //<
    getInlineEditText : function () {
        if (this.creator.isGroup) return this.creator.groupTitle;
        return this.creator.getContents();
    },

    //> @method editProxy.setInlineEditText()
    // Save the new value into the component's state. Called by the
    // +link{inlineEditForm} to commit the change.
    // <p>
    // For a canvas with <code>isGroup</code> enabled, the <code>groupTitle</code>
    // is updated. Otherwise the <code>contents</code> is updated.
    //
    // @param newValue (String) the new component state
    //
    // @visibility external
    //<
    setInlineEditText : function (newValue) {
        var liveObject = this.creator,
            properties
        ;

        if (liveObject.isGroup) properties = { groupTitle: newValue };
        else properties = { contents: newValue };

        liveObject.editContext.setNodeProperties(liveObject.editNode, properties);
    }
});


// Edit Proxy for Canvas
//-------------------------------------------------------------------------------------------

//> @class CanvasEditProxy
// +link{EditProxy} that handles +link{Canvas,Canvas} objects when editMode is enabled.
//
// @group devTools
// @treeLocation Client Reference/Tools/EditProxy
// @visibility external
//<
isc.defineClass("CanvasEditProxy", "EditProxy").addProperties({
    //> @attr editProxy.bringToFrontOnSelect (Boolean : null : IR)
    // Should component be brought to front when selected? Applies when +link{editNode.useEditMask}:true.
    //
    // @visibility external
    //<

    //> @attr editProxy.persistCoordinates (Boolean : null : IRW)
    // Changes to all child +link{EditNode.liveObject,liveObject}'s position
    // and size can be persisted to their +link{EditNode,EditNodes} based on this
    // attribute setting and +link{EditContext.persistCoordinates}. This
    // applies to both programmatic calls and user interaction (drag reposition
    // or drag resize).
    // <p>
    // The default value of <code>null</code> allows +link{EditContext.persistCoordinates}
    // to control all coordinate persistence. An explicit value of <code>false</code>
    // overrides the EditContext setting so that no children of the component save coordinates.
    // <p>
    // All coordinate persisting can be disabled with +link{EditContext.persistCoordinates}.
    // Additionally, all control of persistence can be deferred to each EditProxy by setting
    // +link{EditContext.persistCoordinates} to <code>null</code>.
    //
    // @visibility external
    //<
    persistCoordinates: null
});

isc.CanvasEditProxy.addMethods({
    // Hoop selection
    // --------------------------------------------------------------------------------------------

    //> @attr editProxy.hoopSelector (AutoChild Canvas : null : IR)
    // Hoop selector canvas used for selecting multiple components.
    // <P>
    // Common customization properties can be provided by +link{editContext.hoopSelectorProperties}.
    //
    // @visibility external
    //<
    hoopSelectorDefaults: {
        _constructor:"Canvas",
        _isHoopSelector:true,   // Allow saveCoordinates to skip
        autoDraw:false,
        keepInParentRect: true,
        redrawOnResize:false,
        overflow: "hidden"
    },

    mouseDown : function (event) {
        var result = this.Super("mouseDown", arguments);
        if (result == false) return result;

        var liveObject = this.creator,
            target = isc.EH.getTarget()
        ;

        if (!this.canSelectChildren || this.canSelect == false) return;
        var editContext = liveObject.editContext;

        // don't start hoop selection unless the mouse went down on the Canvas itself, as
        // opposed to on one of the live objects
        if (target != liveObject) return;

        // Since mouse is pressed outside of a component clear current selection
        if (!(isc.EH.shiftKeyDown() || (isc.Browser.isWin && isc.EH.ctrlKeyDown()))) {
            editContext.deselectAllComponents();
        }

        if (editContext.selectionType != isc.Selection.MULTIPLE) return;

        if (this.hoopSelector == null) {
            var properties = isc.addProperties({},
                    this.hoopSelectorDefaults, 
                    this.hoopSelectorProperties,
                    { border: this.selectedBorder },
                    { left: isc.EH.getX(), top: isc.EH.getY() }
                );

            // Create hoop selector as a child on our liveObject
            this.hoopSelector = liveObject.createAutoChild("hoopSelector", properties);
            liveObject.addChild(this.hoopSelector);
        }
        this._hoopStartX = liveObject.getOffsetX();
        this._hoopStartY = liveObject.getOffsetY();

        // Save current selection to determine if this mouseDown is paired
        // with a mouseUp that does not change the selection. In that case
        // we should not fire the selectedEditNodesUpdated event.
        this._startingSelection = editContext.getSelectedComponents();

        this.resizeHoopSelector();
        this.hoopSelector.show();
    },

    // resize hoop on dragMove
    // hide selector hoop on mouseUp or dragStop
    dragMove : function() {
        this.Super("dragMove", arguments);
        if (this.hoopSelector && this.hoopSelector.isVisible()) this.resizeHoopSelector();
    },

    dragStop : function() {
        if (this.hoopSelector && this.hoopSelector.isVisible()) {
            this.hoopSelector.hide();
            var currentSelection = this.creator.editContext.getSelectedComponents();
            if (!this._startingSelection.equals(currentSelection)) {
                this.creator.editContext.showGroupSelectionBox();
                // Fire callback now that selection has completed 
                this.creator.editContext.fireSelectedEditNodesUpdated();
            }
        }
    },

    mouseUp : function () {
        if (!this.canSelectChildren) return;
        if (this.hoopSelector && this.hoopSelector.isVisible()) {
            this.hoopSelector.hide();
            var currentSelection = this.creator.editContext.getSelectedComponents();
            if (!this._startingSelection.equals(currentSelection)) {
                this.creator.editContext.showGroupSelectionBox();
                // Fire callback now that selection has completed 
                this.creator.editContext.fireSelectedEditNodesUpdated();
            }
        }
    },

    // figure out which components intersect the selector hoop, and show the selected outline on
    // those
    updateCurrentSelection : function () {
        var liveObject = this.creator,
            editContext = liveObject.editContext,
            isDrawPane = isc.isA.DrawPane(liveObject)
        ;

        var children = (isDrawPane ? liveObject.drawItems : liveObject.children);
        if (!children) return;
        var oldSelection = editContext.getSelectedComponents(),
            matchFunc = (editContext.hoopSelectionMode == "intersects" ? "intersects" : "encloses"),
            modifierKeyDown = (isc.EH.shiftKeyDown() || (isc.Browser.isWin && isc.EH.ctrlKeyDown()))
        ;

        // make a list of all the children which currently intersect the selection hoop.
        // Update editContext selectedComponents directly because we don't want to fire
        // the selectedEditNodesUpdated event during hoop dragging.
        if (!modifierKeyDown) editContext.selectedComponents = [];
        for (var i = 0; i < children.length; i++) {
            var child = children[i],
                isInternal = (child.creator && (isc.isA.DrawKnob(child.creator) || child._internal))
            ;

            if (!isInternal && this.hoopSelector[matchFunc](child)) {
                if (!isDrawPane) child = this.deriveSelectedComponent(child);
                if (child && !editContext.selectedComponents.contains(child)) {
                    if (child.editProxy && child.editProxy.canSelect != false) {
                        editContext.selectedComponents.add(child);
                    }
                }
            }
        }

        // set outline on components currently within the hoop
        for (var i = 0; i < editContext.selectedComponents.length; i++) {
            editContext.selectedComponents[i].editProxy.showSelectedAppearance(true, true);
        }

        // de-select anything that is no longer within the hoop
        if (!modifierKeyDown) {
            oldSelection.removeList(editContext.selectedComponents);
            for (var i = 0; i < oldSelection.length; i++) {
                oldSelection[i].editProxy.showSelectedAppearance(false);
            }
        }
    },

    // given a child in the canvas, derive the editComponent if there is one
    deriveSelectedComponent : function (comp) {
        var liveObject = this.creator;

        // if the component has a master, it's either an editMask or a peer of some editComponent
        if (comp.masterElement) return this.deriveSelectedComponent(comp.masterElement);
        if (!comp.parentElement || comp.parentElement == liveObject) {
            // if it has an event mask, it's an edit component
            if (comp.editProxy && comp.editProxy.hasEditMask()) return comp;
            // otherwise it's a mask or the hoop
            return null;
        }
        // XXX does this case exist?  how can a direct child have a parent element other than its
        // parent?
        return this.deriveSelectedComponent(comp.parentElement);
    },

    // resize selector to current mouse coordinates
    resizeHoopSelector : function () {
        var liveObject = this.creator,
            x = liveObject.getOffsetX(),
            y = liveObject.getOffsetY();

        if (this.hoopSelector.keepInParentRect) {
            if (x < 0) x = 0;
            var parentHeight = this.hoopSelector.parentElement.getVisibleHeight();
            if (y > parentHeight) y = parentHeight;
        }
    
        // resize to the distances from the start coordinates
        this.hoopSelector.resizeTo(Math.abs(x-this._hoopStartX), Math.abs(y-this._hoopStartY));

        // if we are above/left of the origin set top/left to current mouse coordinates,
        // otherwise to start coordinates.
        if (x < this._hoopStartX) this.hoopSelector.setLeft(x);
        else this.hoopSelector.setLeft(this._hoopStartX);

        if (y < this._hoopStartY) this.hoopSelector.setTop(y);
        else this.hoopSelector.setTop(this._hoopStartY);

        // figure out which components are now in the selector hoop
        this.updateCurrentSelection();
    }
});

// Edit Proxy for Layout
//-------------------------------------------------------------------------------------------

//> @class LayoutEditProxy
// +link{EditProxy} that handles +link{Layout} objects when editMode is enabled.
//
// @group devTools
// @treeLocation Client Reference/Tools/EditProxy
// @visibility external
//<
isc.defineClass("LayoutEditProxy", "CanvasEditProxy").addMethods({

    drop : function () {
        var liveObject = this.creator;

        if (this.shouldPassDropThrough()) {
            liveObject.hideDropLine();
            return;
        }

        isc.EditContext.hideAncestorDragDropLines(liveObject);

        var source = isc.EH.dragTarget,
            editNode,
            dropType;

        if (!source.isA("Palette")) {
            if (source.isA("FormItemProxyCanvas")) {
                source = source.formItem;
            }
            dropType = source._constructor || source.Class;
        } else {
            var paletteNode = source.transferDragData();
            editNode = liveObject.editContext.makeEditNode(paletteNode);
            editNode.dropped = true;
            dropType = editNode.type || editNode.className;
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
            dropTargetNode = this.modifyEditNode(editNode, dropTargetNode, dropType);
            if (!dropTargetNode) {
                liveObject.hideDropLine();
                return isc.EH.STOP_BUBBLING;
            }
        }


        // if the source isn't a Palette, we're drag/dropping an existing component, so remove the 
        // existing component and re-create it in its new position
        if (!source.isA("Palette")) {
            isc.SelectionOutline.hideDragHandle();
            if (source == liveObject) return;  // Can't drop a component onto itself
            var tree = liveObject.editContext.getEditNodeTree(),
                oldParent = tree.getParent(source.editNode),
                oldIndex = tree.getChildren(oldParent).indexOf(source.editNode),
                newIndex = liveObject.getDropPosition(dropType);
                liveObject.editContext.removeNode(source.editNode);

            // If we've moved the child component to a slot further down in the same parent, 
            // indices will now be off by one because we've just removeed it from its old slot
            if (oldParent == this.editNode && newIndex > oldIndex) newIndex--;
            var node;
            if (source.isA("FormItem")) {
                // If the source is a CanvasItem, unwrap it and insert the canvas into this Layout
                // directly; otherwise, we would end up with teetering arrangments of Canvases in
                // inside CanvasItems inside DynamicForms inside CanvasItems inside DynamicForms...
                if (source.isA("CanvasItem")) {
                    node = liveObject.editContext.addNode(source.canvas.editNode, dropTargetNode, newIndex);
                } else {
                    // Wrap the FormItem in a DynamicForm
                    node = liveObject.editContext.addWithWrapper(source.editNode, dropTargetNode);
                }
            } else if (source.isA("DrawItem")) {
                // Wrap the DrawItem in a DrawPane
                node = liveObject.editContext.addWithWrapper(source.editNode, dropTargetNode, true);
            } else {
                node = liveObject.editContext.addNode(source.editNode, dropTargetNode, newIndex);
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
                nodeAdded = liveObject.editContext.addWithWrapper(editNode, dropTargetNode);
            } else if (clazz && clazz.isA("DrawItem")) {
                // Create a wrapper form to allow the DrawItem to be added to this Canvas
                nodeAdded = liveObject.editContext.addWithWrapper(editNode, dropTargetNode, true);
            } else {
                nodeAdded = liveObject.editContext.addNode(editNode, dropTargetNode,
                        liveObject.getDropPosition(dropType));
            }
            if (nodeAdded != null) {
                if (editNode.liveObject.editProxy && editNode.liveObject.editProxy.nodeDropped) {
                    editNode.liveObject.editProxy.nodeDropped();
                }
            }
        }

        liveObject.hideDropLine();
        return isc.EH.STOP_BUBBLING;
    },

    dropMove : function () {
        if (!this.willAcceptDrop()) return false;
        if (!this.shouldPassDropThrough()) {
            var liveObject = this.creator;
            if (liveObject.dropMove) liveObject.dropMove();
            if (liveObject.parentElement && liveObject.parentElement.hideDropLine) {
                liveObject.parentElement.hideDropLine();
                if (liveObject.parentElement.isA("FormItem")) {
                    liveObject.parentElement.form.hideDragLine();
                } else if (liveObject.parentElement.isA("DrawItem")) {
                    liveObject.parentElement.drawPane.hideDragLine();
                }
            }
            return isc.EH.STOP_BUBBLING;        
        } else {
            this.creator.hideDropLine();
        }
    },

    dropOver : function () {
        if (!this.willAcceptDrop()) return false;
        if (!this.shouldPassDropThrough()) {
            var liveObject = this.creator;
            if (liveObject.dropOver) liveObject.dropOver();
            if (liveObject.parentElement && liveObject.parentElement.hideDropLine) {
                liveObject.parentElement.hideDropLine();
                if (liveObject.parentElement.isA("FormItem")) {
                    liveObject.parentElement.form.hideDragLine();
                } else if (liveObject.parentElement.isA("DrawItem")) {
                    liveObject.parentElement.drawPane.hideDragLine();
                }
            }
            return isc.EH.STOP_BUBBLING;        
        } else {
            this.creator.hideDropLine();
        }
    },
    
    // Component editor handling
    // ---------------------------------------------------------------------------------------

    supportsInlineEdit: false
});    


isc.defineClass("DeckEditProxy", "LayoutEditProxy").addMethods({
    // When a pane is dropped into the Deck, set the new pane as the current pane.
    addPane : function (pane, index) {
        var liveObject = this.creator;
        liveObject.addPane(pane, index);
        liveObject.setCurrentPane(pane);
    }
});


//Edit Proxy for NavPanel
//-------------------------------------------------------------------------------------------
isc.defineClass("NavPanelEditProxy", "LayoutEditProxy").addMethods({
    supportsInlineEdit: true,

    // inline editing will only be triggered programmatically
    inlineEditEvent: "none",

    startItemInlineEditing : function (navItem, recordNum) {
        this._inlineEditItem = navItem;
        this._inlineEditRecordNum = recordNum;
        this.startInlineEditing();
    },

    positionAndSizeInlineEditor : function () {
        var liveObject = this.creator,
            liveNavGrid = liveObject.navGrid,
            pageOffsets = liveNavGrid.getPageOffsets(),
            rowTop = liveNavGrid.getRowTop(this._inlineEditRecordNum),
            width = liveNavGrid.getVisibleWidth(),
            minWidth = this.inlineEditForm.minWidth || 1,
            height = liveNavGrid.getRowHeight(this._inlineEditItem, this._inlineEditRecordNum),
            minHeight = this.inlineEditForm.minHeight || 1;
        this.inlineEditLayout.setRect(pageOffsets.left,
                                      pageOffsets.top + rowTop,
                                      Math.max(minWidth, width),
                                      Math.max(minHeight, height));
    },

    getInlineEditText : function () {
        var inlineEditItem = this._inlineEditItem;
        if (inlineEditItem == null || inlineEditItem.title == null) return "";
        return inlineEditItem.title;
    },

    setInlineEditText : function (newValue) {
        var liveObject = this.creator;
        var editContext = liveObject.editContext;
        editContext.setNodeProperties(this._inlineEditItem.editNode, { title: newValue });

        // Redraw the navGrid body immediately so that the old title is not briefly visible.
        var liveNavGrid = liveObject.navGrid;
        if (liveNavGrid != null) {
            var liveNavGridBody = liveNavGrid.body;
            if (liveNavGridBody != null) liveNavGridBody.redrawIfDirty("setInlineEditText");
        }
    },

    onFolderDrop: function (draggedNodes, folder, targetIndex, dropPosition, sourceWidget) {
        var liveObject = this.creator;
        var editContext = folder.editContext || liveObject.editContext;
        var editNode = folder.editNode || liveObject.editNode;
        var dropType = draggedNodes[0].type;

        // If the dropType is null/undefined (this indicates that a live object is being dropped
        // rather than a palette node), then return early so that we do not crash.
        if (dropType == null) return;

        var addedNavItemNode;
        if (dropPosition == "over") {
            // If the user drops a NavItem node, then create a new NavItem where the NavItem
            // node was dropped.
            if (dropType == "NavItem") {
                addedNavItemNode = editContext.addFromPaletteNode(draggedNodes[0], editNode);

            // If the user drops a widget node onto a header NavItem, then implicitly create a
            // new NavItem whose pane is the widget created from the dropped widget node.
            // This makes sense because a header NavItem cannot itself have a pane.
            
            } else if (editNode.liveObject.isHeader) {
                var navItemNode = editContext.findPaletteNode("type", "NavItem")
                var innerNode = editContext.addFromPaletteNode(navItemNode, editNode, targetIndex);
                addedNavItemNode = innerNode;
                var itemPaneNode = innerNode.liveObject.editContext.addFromPaletteNode(draggedNodes[0], innerNode);
                liveObject.setItemPane(innerNode.liveObject, itemPaneNode.liveObject);

            // Otherwise, create a widget from the dropped node (presumably a widget node) and
            // set the item pane of whichever NavItem onto which the node was dropped.
            } else {
                var itemPaneNode = editContext.addFromPaletteNode(draggedNodes[0], editNode);
                liveObject.setItemPane(folder, itemPaneNode.liveObject);
            }

        } else {
            // If the user drops a NavItem node over the blank area of the navGrid, then create
            // a new NavItem.
            if (dropType == "NavItem") {
                addedNavItemNode = editContext.addFromPaletteNode(draggedNodes[0], editNode, targetIndex);

            // If the user drops a widget node over the blank area of the navGrid, then implicitly
            // create a new NavItem whose pane is the widget created from the dropped widget node.
            } else {
                var navItemNode = editContext.findPaletteNode("type", "NavItem")
                var innerNode = editContext.addFromPaletteNode(navItemNode, editNode, targetIndex);
                addedNavItemNode = innerNode;
                var itemPaneNode = innerNode.liveObject.editContext.addFromPaletteNode(draggedNodes[0], innerNode);
                liveObject.setItemPane(innerNode.liveObject, itemPaneNode.liveObject);
            }
        }

        // Start inline editing of any new non-separator NavItem.
        if (addedNavItemNode != null) {
            var addedNavItem = addedNavItemNode.liveObject;
            if (!addedNavItem.isSeparator) {
                liveObject.editProxy.delayCall("startItemInlineEditing", [addedNavItem, liveObject.navGrid.getRecordIndex(addedNavItem)]);
            }
        }

        return false;
    },
    setEditMode : function (editingOn) {
        var properties = this.Super("setEditMode", arguments);
        if (editingOn) {
            this.creator.navGrid.canAcceptDroppedRecords = true;
            this.creator.navGrid.canDragRecordsOut = true;
            this.creator.navGrid.canReorderRecords = false;
            this.creator.navGrid.canReparentNodes = false;
            this.creator.navGrid.canDropOnLeaves = true;
            this.creator.navGrid.onFolderDrop = this.onFolderDrop;
            this.creator.navGrid.dragDataAction = "copy";
            this.creator.navGrid.showOpenIcons = true;
            this.creator.navGrid.showDropIcons = true;
            
            this.creator.navGrid._setUpDragProperties();

            // Update the NavPanel's editNode with the current currentItemId
            this.creator.editContext.setNodeProperties(this.creator.editNode, { currentItemId: this.creator.currentItemId });

        } else {
            this.creator.navGrid.canAcceptDroppedRecords = false;
            this.creator.navGrid.canDropOnLeaves = false;
            this.creator.navGrid.canDragRecordsOut = false;
            this.creator.navGrid.canReorderRecords = false;
            this.creator.navGrid.canReparentNodes = false;
            delete this.creator.navGrid.onFolderDrop;
            this.creator.navGrid._setUpDragProperties();
        }
    },
    canAdd : function (dropType) {
        var liveObject = this.creator;
        if (dropType == "NavItem" && !liveObject.navGrid.containsEvent()) {
            return false;
        }
        return true;
    },
    drop : function () {
        this.creator.navDeck.setBorder("");
        if (!this.creator.navGrid.containsEvent()) {
            if (this.shouldPassDropThrough()) {
                return;
            }

            var liveObject = this.creator,
                source = isc.EH.dragTarget,
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

            // If node is dropped from a tree, clean it of internal properties
            if (source.isA("TreeGrid")) {
                paletteNode = source.data.getCleanNodeData([paletteNode], false, false, false)[0];
            }

            // Palette node could be modified later if there are palettized components within.
            // Copy it now so that future drops are not affected.
            paletteNode = isc.clone(paletteNode);

            // if the source isn't a Palette, we're drag/dropping an existing component, so remove the 
            // existing component and re-create it in its new position
            if (!source.isA("Palette")) {
                isc.SelectionOutline.hideDragHandle();
                if (source == liveObject) return;  // Can't drop a component onto itself
                var editContext = liveObject.editContext,
                    editNode = liveObject.editNode,
                    tree = editContext.getEditNodeTree(),
                    oldParent = tree.getParent(source.editNode);
                editContext.removeNode(source.editNode);
            }
            
            var folder = this.creator.navGrid.getSelectedRecord();
            if (folder == null) {
                folder = this.creator.navGrid.data.getRoot();
            }
            this.onFolderDrop([paletteNode], folder, 0, "over");
        }
        return isc.EH.STOP_BUBBLING;
    },

    dropOut : function () {
        this.creator.navDeck.setBorder("");
        return isc.EH.STOP_BUBBLING;
    },

    dropMove : function () {
        if (!this.willAcceptDrop()) return false;
        if(this.creator.hideDropLine) this.creator.hideDropLine();
        if (!this.shouldPassDropThrough()) {
            if (this.creator.navGrid.containsEvent()) {
                this.creator.navDeck.setBorder("");
            } else {
                this.creator.navDeck.setBorder("2px dashed blue");
            }
            return isc.EH.STOP_BUBBLING;
        }
    }
});


// Edit Proxy for SplitPane
// -------------------------------------------------------------------------------------------

//> @class SplitPaneEditProxy
// +link{EditProxy} that handles +link{SplitPane} objects when editMode is enabled.
//
// @group devTools
// @treeLocation Client Reference/Tools/EditProxy
// @visibility external
//<
isc.defineClass("SplitPaneEditProxy", "LayoutEditProxy").addMethods({

    // When a component is dragged onto a SplitPane show an overlay with 3 panes as targets
    // for the drop. Panes show the current component, if any. 

    

    // Reject additions via the EditTree since it's not clear what pane
    // the addition would target.
    canAddToParent : function (type) {
        return false;
    },

    drop : function () {
        // SplitPane doesn't accept drop directly; drop is performed on overlay
        return isc.EH.STOP_BUBBLING;        
    },

    dropOver : function () {
        if (!this.willAcceptDrop()) return false;
        if (!this.shouldPassDropThrough()) {
            this.showDropOverlay(true);
        }
        return isc.EH.STOP_BUBBLING;
    },

    dropOut : function () {
        var dragData = this.ns.EH.dragTarget.getDragData();
        this.showDropOverlay(false);
        return isc.EH.STOP_BUBBLING;
    },

    dropOverlayDefaults: {

        backgroundColor: "white",
        visibility: "hidden",
        width: "100%",
        height: "100%",

        paneDefaults : {
            _constructor: "VLayout",
            border: "1px solid blue",
            align: "center",
            showDropLines: false
        },

        initWidget : function () {
            this.Super("initWidget", arguments);

            this.navPane = this.createPane("30%", "Navigation Pane", "navigationPane");
            this.listPane = this.createPane("100%", "List Pane", "listPane");
            this.detailPane = this.createPane("100%", "Detail Pane", "detailPane");

            var rightLayout = isc.VLayout.create({
                widht: "70%",
                members: [ this.listPane, this.detailPane ] 
            });
            this.addChild(isc.HLayout.create({
                width: "100%",
                height: "100%",
                members: [ this.navPane, rightLayout ]
            }));
            
            this._panes = [ this.navPane, this.listPane, this.detailPane ];
        },

        createPane : function (width, title, parentProperty) {
            var initialText = this.getPaneText(title, false, parentProperty),
                label = isc.Label.create({
                    align: "center",
                    overflow: "hidden",
                    contents: initialText
                })
            ;

            return this.createAutoChild("pane", {
                width: width,
                members: [ label ],
                name: title,
                canAcceptDrop: true,
                parentProperty: parentProperty,
                drop : function () {
                    return this.creator.creator.addPane(this.parentProperty);
                }
            });
        },

        setPaneLabel : function (pane, text) {
            var label = pane.getMember(0);
            label.setContents(text);
        },

        getPaneText : function (title, over, parentProperty) {
            var titleStyle = (over ? "style='color:#0000ff'" : ""),
        		text = "<span " + titleStyle + ">" + title + "</span>",
                editProxy = this.creator,
                liveObject = editProxy.creator,
                component = liveObject[parentProperty]
    		;
            if (component) {
                var label = (this.editContext.getSelectedLabelText
                                ? this.editContext.getSelectedLabelText(component)
                                : component.toString());
                text += "<br>Currently: <span style='color:#666666'>" + label + "</span>";
            }
            return text;
        },

        canAcceptDrop: true,
        drop : function () {
            
            if (this._dropOutPane) {
                this.creator.addPane(this._dropOutPane.parentProperty);
            }
            return isc.EH.STOP_BUBBLING;
        },
        dropMove : function () {
            var dropPane,
                x = isc.EH.getX(),
                y = isc.EH.getY()
            ;
            for (var i = 0; i < this._panes.length; i++) {
                if (this._panes[i].containsPoint(x, y)) {
                    dropPane = this._panes[i];
                    break;
                }
            }
            if (!this._lastDropPane || this._lastDropPane != dropPane) {
                if (this._lastDropPane && this._lastDropPane != dropPane) {
                    var text = this.getPaneText(this._lastDropPane.name, false, this._lastDropPane.parentProperty);
                    this.setPaneLabel(this._lastDropPane, text);
                }
                if (dropPane) {
                    var text = this.getPaneText(dropPane.name, true, dropPane.parentProperty);
                    this.setPaneLabel(dropPane, text);
                }

                this._lastDropPane = dropPane;
            }
            return isc.EH.STOP_BUBBLING;
        },
        dropOver : function () {
            this._dropOutPane = null;
        },
        dropOut : function () {
            
            var dropPane,
                x = isc.EH.getX(),
                y = isc.EH.getY()
            ;
            for (var i = 0; i < this._panes.length; i++) {
                if (this._panes[i].containsPoint(x, y)) {
                    dropPane = this._panes[i];
                    break;
                }
            }
            this._dropOutPane = dropPane;

            this.hide();

            return isc.EH.STOP_BUBBLING;
        },

        show : function () {
            this.Super("show", arguments);
            delete this._lastDropPane;
            for (var i = 0; i < this._panes.length; i++) {
                var pane = this._panes[i],
                    text = this.getPaneText(pane.name, false, pane.parentProperty)
                ;
                this.setPaneLabel(pane, text);
            }
        },

        draw : function () {
            this.Super("draw", arguments);

            // stay above the parent
            if (!this.isObserving(this.creator.creator, "setZIndex")) {
                this.observe(this.creator.creator, "setZIndex", "observer.moveAbove(observed)");
            }

            return this;
        },

        // Event Bubbling
        // ---------------------------------------------------------------------------------------

        // XXX FIXME: this is here to maintain z-order on dragReposition.  EH.handleDragStop()
        // brings the mask to the front when we stop dragging - which is not what we want, so we
        // suppress it here.
        bringToFront : function () { },
    
        // Resize
        // ---------------------------------------------------------------------------------------

        resized : function() {
            this.Super("resized", arguments);

            var master = this.creator.creator;
            if (master) {
                // the widget we're masking may overflow, so redraw if necessary to get new size so,
                // and match its overflow'd size
                master.redrawIfDirty();
                this.resizeTo(master.getVisibleWidth(), master.getVisibleHeight());
            }
        }
    },

    showDropOverlay : function (show) {
        if (show) {
            var liveObject = this.creator;
            if (!this._dropOverlay) {
                var props = isc.addProperties({}, this.dropOverlayDefaults, this.dropOverlayProperties, {
                    editContext: liveObject.editContext,
                    creator: this
                });
                this._dropOverlay = isc.Canvas.create(props);
                liveObject.addChild(this._dropOverlay);
            }
            this._dropOverlay.show();
        } else if (this._dropOverlay) {
            this._dropOverlay.hide();
        }
    },

    addPane : function (parentProperty) {
        var liveObject = this.creator,
            source = isc.EH.dragTarget,
            editNode,
            dropType
        ;

        if (!source.isA("Palette")) {
            if (source.isA("FormItemProxyCanvas")) {
                source = source.formItem;
            }
            dropType = source._constructor || source.Class;
        } else {
            var paletteNode = source.transferDragData();
            editNode = liveObject.editContext.makeEditNode(paletteNode);
            editNode.dropped = true;
            editNode.defaults.parentProperty = parentProperty;
            dropType = editNode.type || editNode.className;
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
            dropTargetNode = this.modifyEditNode(editNode, dropTargetNode, dropType);
            if (!dropTargetNode) {
                liveObject.hideDropLine();
                return isc.EH.STOP_BUBBLING;
            }
        }

        // if the source isn't a Palette, we're drag/dropping an existing component, so remove the 
        // existing component and re-create it in its new position
        if (!source.isA("Palette")) {
            isc.SelectionOutline.hideDragHandle();
            if (source == liveObject) return;  // Can't drop a component onto itself
            var tree = liveObject.editContext.getEditNodeTree(),
                oldParent = tree.getParent(source.editNode),
                oldIndex = tree.getChildren(oldParent).indexOf(source.editNode),
                newIndex = liveObject.getDropPosition(dropType);
                liveObject.editContext.removeNode(source.editNode)
            ;

            // If we've moved the child component to a slot further down in the same parent, 
            // indices will now be off by one because we've just removeed it from its old slot
            if (oldParent == this.editNode && newIndex > oldIndex) newIndex--;
            var node;
            if (source.isA("FormItem")) {
                // If the source is a CanvasItem, unwrap it and insert the canvas into this Layout
                // directly; otherwise, we would end up with teetering arrangments of Canvases in
                // inside CanvasItems inside DynamicForms inside CanvasItems inside DynamicForms...
                if (source.isA("CanvasItem")) {
                    source.canvas.editNode.defaults.parentProperty = parentProperty;
                    node = liveObject.editContext.addNode(source.canvas.editNode, dropTargetNode, newIndex, parentProperty);
                } else {
                    // Wrap the FormItem in a DynamicForm
                    source.editNode.defaults.parentProperty = parentProperty;
                    node = liveObject.editContext.addWithWrapper(source.editNode, dropTargetNode, null, parentProperty);
                }
            } else if (source.isA("DrawItem")) {
                // Wrap the DrawItem in a DrawPane
                source.editNode.defaults.parentProperty = parentProperty;
                node = liveObject.editContext.addWithWrapper(source.editNode, dropTargetNode, true, parentProperty);
            } else {
                source.editNode.defaults.parentProperty = parentProperty;
                node = liveObject.editContext.addNode(source.editNode, dropTargetNode, newIndex, parentProperty);
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
                nodeAdded = liveObject.editContext.addWithWrapper(editNode, dropTargetNode, null, parentProperty);
            } else if (clazz && clazz.isA("DrawItem")) {
                // Create a wrapper form to allow the DrawItem to be added to this Canvas
                nodeAdded = liveObject.editContext.addWithWrapper(editNode, dropTargetNode, true, parentProperty);
            } else {
                nodeAdded = liveObject.editContext.addNode(editNode, dropTargetNode,
                        liveObject.getDropPosition(dropType), parentProperty);
            }
            if (nodeAdded != null) {
                if (editNode.liveObject.editProxy && editNode.liveObject.editProxy.nodeDropped) {
                    editNode.liveObject.editProxy.nodeDropped();
                }
            }
        }

        liveObject.hideDropLine();
        return isc.EH.STOP_BUBBLING;
        
    },

    // Component editor handling
    // ---------------------------------------------------------------------------------------

    supportsInlineEdit: false
});    

// Edit Proxy for SectionStack
//-------------------------------------------------------------------------------------------

//> @class SectionStackEditProxy
// +link{EditProxy} that handles +link{SectionStack} objects when editMode is enabled.
//
// @group devTools
// @treeLocation Client Reference/Tools/EditProxy
// @visibility external
//<
isc.defineClass("SectionStackEditProxy", "LayoutEditProxy").addMethods({

    canAdd : function (type) { 
        if (!this.canDropAtLevel()) return false;

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
        return null;
    },

    //  Return the modified editNode (or a completely different one); return false to abandon 
    //  the drop
    modifyEditNode : function (paletteNode, newEditNode, dropType) {
        if (dropType == "SectionStackSection") return newEditNode;
        var dropPosition = this.creator.getDropPosition();
        if (dropPosition == 0) {
            isc.warn("Cannot drop before the first section header");
            return false;
        }

        var headers = this._getHeaderPositions();
        for (var i = headers.length-1; i >= 0; i--) {
            if (dropPosition > headers[i]) {
                // Return the edit node off the section header
                return this.creator.getSectionHeader(i).editNode;
            }
        }
        // Shouldn't ever get here
        return newEditNode;
    },

    //  getDropPosition() - explicitly called from SectionStack.getDropPosition if the user isn't doing
    //  a drag reorder of sections.
    getDropPosition : function (dropType) {
        var pos = this.creator.invokeSuper(isc.SectionStack, "getDropPosition");
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
        var liveObject = this.creator,
            headers = [],
            j = 0;
        for (var i = 0; i < liveObject.getMembers().length; i++) {
            if (liveObject.getMember(i).isA(liveObject.sectionHeaderClass)) {
                headers[j++] = i;
            }
        }
        return headers;
    },
    
    // Component editor handling
    // ---------------------------------------------------------------------------------------

    supportsInlineEdit: false
});


// Edit Proxy for TabSet
//-------------------------------------------------------------------------------------------

//> @class TabSetEditProxy
// +link{EditProxy} that handles +link{TabSet} objects when editMode is enabled.
//
// @group devTools
// @treeLocation Client Reference/Tools/EditProxy
// @visibility external
//<
isc.defineClass("TabSetEditProxy", "CanvasEditProxy").addMethods({

    // Don't persist coordinates on tab panes
    persistCoordinates: false,
    inlineEditOnDrop: true,

    setEditMode : function(editingOn) {
        this.Super("setEditMode", arguments);

        // If we're going into edit mode, add close icons to every tab
        var liveObject = this.creator;
        if (editingOn) {
            for (var i = 0; i < liveObject.tabs.length; i++) {
                var tab = liveObject.tabs[i];
                this.saveTabProperties(tab);
            }
            liveObject.closeClick = function(tab) {
                // Suppress normal click behavior
            }
        } else {
            // If we're coming out of edit mode, revert to whatever was on the init data
            for (var i = 0; i < liveObject.tabs.length; i++) {
                var tab = liveObject.tabs[i];
                this.restoreTabProperties(tab);
            }
        }
        
        // Set edit mode on the TabBar and PaneContainer.  Note that we deliberately pass null as
        // the editNode - this allows the components to pick up the special editMode method 
        // overrides, but prevents them from actually being edited
        liveObject.tabBar.setEditMode(editingOn, liveObject.editContext, null);
        liveObject.paneContainer.setEditMode(editingOn, liveObject.editContext, null);
    },

    saveTabProperties : function (tab) {
        var liveTab = this.creator.getTab(tab);
        if (liveTab) {
            liveTab.saveToOriginalValues(["closeClick", "icon", "iconSize",
                                          "iconOrientation", "iconAlign", "disabled"]);
        }
    },

    restoreTabProperties : function (tab) {
        var liveTab = this.creator.getTab(tab);
        if (liveTab) {
            liveTab.restoreFromOriginalValues(["closeClick", "icon", "iconSize",
                                               "iconOrientation", "iconAlign", "disabled"]);
        }
    },

    // Component editor handling
    // ---------------------------------------------------------------------------------------

    inlineEditInstructions: "Enter tab titles (comma separated)",

    //> @method tabSetEditProxy.getInlineEditText()
    // Returns the text based on the current component state to be edited inline.
    // Called by the +link{editProxy.inlineEditForm} to obtain the starting edit value.
    // <p>
    // Returns a comma-separated list of tab titles. A " [x]" suffix is added
    // for any tab with <code>canClose</code> enabled.
    //
    // @visibility external
    //<
    getInlineEditText : function () {
        var liveObject = this.creator,
            tabs = liveObject.tabs,
            editText = null
        ;

        for (var i = 0; i < tabs.length; i++) {
            var title = tabs[i].title.replace(/,/, "\\,");

            editText = (editText ? editText + ", " : "") + title + (tabs[i].canClose ? " [x]" : "");
        }
        return editText;
    },
    
    //> @method tabSetEditProxy.setInlineEditText()
    // Save the new value into the component's state. Called by the
    // +link{editProxy.inlineEditForm} to commit the change.
    // <p>
    // Takes a comma-separated list of tab titles. Add " [x]" to a title
    // to enable <code>canClose</code> for the tab.
    //
    // @param newValue (String) the new component state
    //
    // @visibility external
    //<
    setInlineEditText : function (newValue) {
        var tabNames = isc.EditProxy.splitString(newValue, ",", "\\");

        // Determine which tabs allow closing
        var canClose = [];
        for (var i = 0; i < tabNames.length; i++) {
            if (tabNames[i].endsWith("[x]") || tabNames[i].endsWith("[X]")) {
                tabNames[i] = tabNames[i].replace(/\[[xX]\]/, "").replace(/\s+$/, "");
                canClose.add(tabNames[i]);
            }
        }

        // Remove tabs not in new title list
        // and update canClose on existing tabs
        var liveObject = this.creator,
            tabs = liveObject.tabs,
            nodesToRemove = [],
            existingTabNames = []
        ;
        if (tabs.length > 0) {
            for (var i = 0; i < tabs.length; i++) {
                if (!tabNames.contains(tabs[i].title)) {
                    nodesToRemove.add(tabs[i].editNode);
                } else if (tabNames.contains(tabs[i].title)) {
                    existingTabNames.add(tabs[i].title);
                    this.creator.editContext.setNodeProperties(tabs[i].editNode, { canClose: canClose.contains(tabs[i].title) });
                }
            }
        }

        nodesToRemove.map(function (node) { 
            liveObject.editContext.removeNode(node);
        });

        // Add new tabs
        for (var i = 0; i < tabNames.length; i++) {
            if (existingTabNames.contains(tabNames[i])) continue;

            var tab = {
                type: "Tab",
                defaults: {
                    title: tabNames[i]
                }
            };
            var node = this.creator.editContext.addNode(this.creator.editContext.makeEditNode(tab),
                                                     this.creator.editNode, i);
            this.addDefaultPane(node);
        }
    },

    addDefaultPane : function (tabNode) {
        if (!tabNode) return;
        var defaultPane = isc.addProperties({}, this.creator.defaultPaneDefaults);
        if (!defaultPane.type && !defaultPane.className) {
            defaultPane.type = defaultPane._constructor || this.creator.defaultPaneConstructor;
        }
        this.creator.editContext.addNode(this.creator.editContext.makeEditNode(defaultPane), tabNode);
    },

    // Extra stuff to do when tabSet.addTabs() is called when the tabSet is in an editable context
    // (though not necessarily actually in editMode)
    addTabsEditModeExtras : function (newTabs) {
        // If the TabSet is in editMode, put the new tab(s) into edit mode too
        if (this.creator.editingOn) {
            for (var i = 0; i < newTabs.length; i++) {
                this.saveTabProperties(newTabs[i]);
            }
        }
    },

    // Extra stuff to do when tabSet.removeTabs() is called when the tabSet is in an editable 
    // context (though not necessarily actually in editMode)
    removeTabsEditModeExtras : function () { },

    //Extra stuff to do when tabSet.reorderTab() is called when the tabSet is in an editable 
    //context (though not necessarily actually in editMode)
    reorderTabsEditModeExtras : function (originalPosition, moveToPosition) {
        if (this.creator.editContext && this.creator.editContext.reorderNode) {
            this.creator.editContext.reorderNode(this.creator.editNode, originalPosition, moveToPosition);
        }
    },

    // Override of EditProxy.findEditNode.  If the item being dragged is a Tab, falls back to the 
    // Canvas impl (which will return the TabSet itself).  If the item being dragged is not a 
    // Tab, returns the currently selected Tab if it has an editNode, otherwise the first Tab 
    // with an editNode, otherwise returns the result of calling the parent element's 
    // findEditNode(), because this is a TabSet with no tabs in edit mode
    findEditNode : function (dragType) {
        this.logInfo("In TabSet.findEditNode, dragType is " + dragType, "editModeDragTarget");
        if (dragType != "Tab") {
            var tab = this.creator.getTab(this.creator.getSelectedTabNumber());
            if (tab && tab.editNode) return tab;
            for (var i = 0; i < this.creator.tabs.length; i++) {
                tab = this.creator.getTab(i);
                if (tab.editNode) return tab;
            }
            if (this.creator.parentElement) return this.creator.parentElement.editProxy.findEditNode(dragType);
        }
        return this.Super("findEditNode", arguments);
    },

    // Override completeItemDrop() to add the default pane to tabs (and drop into 
    // edit-title)
    completeItemDrop : function (paletteNode, itemIndex, rowNum, colNum, side, callback) {
        this.Super("completeItemDrop", arguments);
        if (paletteNode && (paletteNode.type || paletteNode.className) == "Tab") {
            var liveObj = paletteNode.liveObject;
            this.addDefaultPane(paletteNode);
            this.creator.selectTab(liveObj);
            
            liveObj.editProxy.delayCall("editTitle"); 
        }
    }

});


isc.defineClass("TabBarEditProxy", "CanvasEditProxy").addMethods({
    
    findEditNode : function (dragType) {
        
        if (dragType == "Tab") {
            // Delegate to the TabSet's findEditNode()
            return this.creator.parentElement.editProxy.findEditNode(dragType);
        } else if (this.creator.parentElement && isc.isA.Layout(this.creator.parentElement.parentElement)) {
            return this.creator.parentElement.parentElement.editProxy.findEditNode(dragType);
        }
        
        return this.Super("findEditNode", arguments);
    },

    // Component editor handling
    // ---------------------------------------------------------------------------------------

    supportsInlineEdit: true,
    
    startInlineEditing : function () {
        var tabset = this.creator.parentElement;
        tabset.editProxy.startInlineEditing();
    }
});

//> @class StatefulCanvasEditProxy
// +link{EditProxy} that handles +link{StatefulCanvas} objects when editMode is enabled.
//
// @group devTools
// @treeLocation Client Reference/Tools/EditProxy
// @visibility external
//<
isc.defineClass("StatefulCanvasEditProxy", "CanvasEditProxy").addMethods({

    inlineEditOnDrop: true,

    getOverrideProperties : function () {
        var properties = this.Super("getOverrideProperties", arguments);
        // Prevent a StatefulCanvas from accidentally allowing drops.
        // Also allows a parent snapGrid to be properly applied.
        delete properties.canAcceptDrop;
        delete properties.canDropComponents;
        return properties;
    },

    click : function (event, eventInfo) {
        var result = this.Super("click", arguments);

        
        if (this.creator.handleActivate) this.creator.handleActivate(event, eventInfo);
        return result;
    },
   
    // Component editor handling
    // ---------------------------------------------------------------------------------------

    //> @method statefulCanvasEditProxy.getInlineEditText()
    // Returns the text based on the current component state to be edited inline.
    // Called by the +link{editProxy.inlineEditForm} to obtain the starting edit value.
    // <p>
    // Returns the component's title.
    //
    // @visibility external
    //<
    getInlineEditText : function () {
        return this.creator.getTitle();
    },
    
    //> @method statefulCanvasEditProxy.setInlineEditText()
    // Save the new value into the component's state. Called by the
    // +link{editProxy.inlineEditForm} to commit the change.
    // <p>
    // Updates the component's title.
    //
    // @param newValue (String) the new component title
    //
    // @visibility external
    //<
    setInlineEditText : function (newValue) {
        var liveObject = this.creator;

        liveObject.editContext.setNodeProperties(liveObject.editNode, { title: newValue });
    }
});

//> @class ImgEditProxy
// +link{EditProxy} that handles +link{Img} objects when editMode is enabled.
//
// @group devTools
// @treeLocation Client Reference/Tools/EditProxy
// @visibility external
//<
isc.defineClass("ImgEditProxy", "CanvasEditProxy");

//> @class TooLStripSeparatorEditProxy
// +link{EditProxy} that handles +link{ToolStripSeparator} objects when editMode is enabled.
//
// @group devTools
// @treeLocation Client Reference/Tools/EditProxy
// @visibility external
//<
isc.defineClass("ToolStripSeparatorEditProxy", "ImgEditProxy").addMethods({
    inlineEditOnDrop: false,
    canSelectChildren: false
});

//> @class LabelEditProxy
// +link{EditProxy} that handles +link{Label} and +link{SectionHeader} objects when editMode is enabled.
//
// @group devTools
// @treeLocation Client Reference/Tools/EditProxy
// @visibility external
//<
isc.defineClass("LabelEditProxy", "StatefulCanvasEditProxy").addMethods({

    // Component editor handling
    // ---------------------------------------------------------------------------------------

    //> @method labelEditProxy.getInlineEditText()
    // Returns the text based on the current component state to be edited inline.
    // Called by the +link{editProxy.inlineEditForm} to obtain the starting edit value.
    // <p>
    // Returns the component's <code>contents</code>.
    //
    // @visibility external
    //<
    getInlineEditText : function () {
        if (isc.isA.SectionHeader(this.creator)) {
            return this.creator.getTitle();
        }
        return this.creator.getContents();
    },
    
    //> @method labelEditProxy.setInlineEditText()
    // Save the new value into the component's state. Called by the
    // +link{editProxy.inlineEditForm} to commit the change.
    // <p>
    // Updates the component's <code>contents</code>.
    //
    // @param newValue (String) the new component contents
    //
    // @visibility external
    //<
    setInlineEditText : function (newValue) {
        var liveObject = this.creator;

        if (isc.isA.SectionHeader(liveObject)) {
            liveObject.editContext.setNodeProperties(liveObject.editNode, { title: newValue });
        } else {
            liveObject.editContext.setNodeProperties(liveObject.editNode, { contents: newValue });
        }
    }
});

//> @class ProgressbarEditProxy
// +link{EditProxy} that handles +link{Progressbar} objects when editMode is enabled.
//
// @group devTools
// @treeLocation Client Reference/Tools/EditProxy
// @visibility external
//<
isc.defineClass("ProgressbarEditProxy", "StatefulCanvasEditProxy").addMethods({

    inlineEditOnDrop: false,

    // Component editor handling
    // ---------------------------------------------------------------------------------------

    //> @method progressbarEditProxy.getInlineEditText()
    // Returns the text based on the current component state to be edited inline.
    // Called by the +link{editProxy.inlineEditForm} to obtain the starting edit value.
    // <p>
    // Returns the component's <code>percentDone</code>.
    //
    // @visibility external
    //<
    getInlineEditText : function () {
        return (this.creator.percentDone != null ? this.creator.percentDone.toString() : "");
    },

    //> @method progressbarEditProxy.setInlineEditText()
    // Save the new value into the component's state. Called by the
    // +link{editProxy.inlineEditForm} to commit the change.
    // <p>
    // Updates the component's <code>percentDone</code>.
    //
    // @param newValue (String) the new component percentDone
    //
    // @visibility external
    //<
    setInlineEditText : function (newValue) {
        var liveObject = this.creator;

        var parsedValue = parseInt(newValue);
        if (isNaN(parsedValue)) parsedValue = null;

        liveObject.editContext.setNodeProperties(liveObject.editNode, { percentDone: parsedValue });
    }
});

//> @class WindowEditProxy
// +link{EditProxy} that handles +link{Window} objects when editMode is enabled.
//
// @group devTools
// @treeLocation Client Reference/Tools/EditProxy
// @visibility external
//<
isc.defineClass("WindowEditProxy", "LayoutEditProxy").addMethods({

    // Component editor handling
    // ---------------------------------------------------------------------------------------

    supportsInlineEdit: true,

    //> @method windowEditProxy.getInlineEditText()
    // Returns the text based on the current component state to be edited inline.
    // Called by the +link{editProxy.inlineEditForm} to obtain the starting edit value.
    // <p>
    // Returns the component's title.
    //
    // @visibility external
    //<
    getInlineEditText : function () {
        return this.creator.title;
    },
    
    //> @method windowEditProxy.setInlineEditText()
    // Save the new value into the component's state. Called by the
    // +link{editProxy.inlineEditForm} to commit the change.
    // <p>
    // Updates the component's title.
    //
    // @param newValue (String) the new component title
    //
    // @visibility external
    //<
    setInlineEditText : function (newValue) {
        var liveObject = this.creator;

        liveObject.editContext.setNodeProperties(liveObject.editNode, { title: newValue });
    }
});

//> @class DetailViewerEditProxy
// +link{EditProxy} that handles +link{DetailViewer} components when editMode is enabled.
//
// @group devTools
// @treeLocation Client Reference/Tools/EditProxy
// @visibility external
//<
isc.defineClass("DetailViewerEditProxy", "CanvasEditProxy").addMethods({

    // Component editor handling
    // ---------------------------------------------------------------------------------------

    supportsInlineEdit: true,
    inlineEditMultiline: true,
    inlineEditInstructions: "Enter options, one per line. Use \"*\" to mark the selected option. " +
    "Use \"Field:Value\" to create a mapping between fields and values.",

    //> @attr detailViewerEditProxy.dataSeparatorChar (String : "," : IR)
    // If +link{editProxy.inlineEditEvent,inline editing} for this viewer edits the
    // +link{detailViewer.data}, character that should be used as a separator between
    // values, or between pairs of field name vs values if the user is entering such
    // a +link{ValueMap} using the +link{dataDisplaySeparatorChar,dataDisplaySeparatorChar}.
    // <p>
    // If +link{editProxy.inlineEditMultiline} is enabled, newlines will be used as value
    // separators and the <code>dataSeparatorChar</code>
    // <p>
    // The +link{dataEscapeChar,dataEscapeChar} can be used to enter the separator
    // char as part of a field name or value.
    //
    // @visibility external
    //<
    dataSeparatorChar: ",",

    //> @attr detailViewerEditProxy.dataDisplaySeparatorChar (String : ":" : IR)
    // If +link{editProxy.inlineEditEvent,inline editing} for this viewer edits the
    // +link{detailViewer.data}, character that should be used as a separator for
    // entering +link{ValueMap}-style entries that map from a field name to a value.
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
    // Any entry without a separator char has an implied value of <code>null</code>.
    // For example, for this input:
    // <pre>
    //       Fixed:Reported Fixed, WontFix:Won't Fix, Resolved
    // </pre>
    // The resulting <code>data</code> would be:
    // <pre>
    //   {
    //      "Fixed" : "Reported Fixed",
    //      "WontFix" : "Won't Fix",
    //      "Resolved" : null
    //   }
    // </pre>
    // <p>
    // The +link{dataEscapeChar,dataEscapeChar} can be used to enter literal colon characters.
    // <p>
    // Set <code>dataDisplaySeparatorChar</code> to null to prevent entry of values
    // - user input will always be treated as just a list of legal field names.
    //
    // @visibility external
    //<
    dataDisplaySeparatorChar: ":",

    //> @attr detailViewerEditProxy.dataEscapeChar (String : "\" : IR)
    // If +link{editProxy.inlineEditEvent,inline editing} for this viewer edits the
    // +link{detailViewer.data}, character that can be used to enter literal separator
    // chars (such as the +link{dataSeparatorChar,dataSeparatorChar}) or literal
    // leading or trailing whitespace.
    // <p>
    // Repeat this character twice to enter it literally.  For example, with the default
    // of "\", inputting "\\" would result in a literal backslash in the value.
    //
    // @visibility external
    //<
    dataEscapeChar: "\\",

    //> @method detailViewerEditProxy.getInlineEditText()
    // Returns the text based on the current component state to be edited inline.
    // Called by the +link{editProxy.inlineEditForm} to obtain the starting edit value.
    // <p>
    // Returns the component's data one-field-per-line as specified in
    // +link{detailViewerEditProxy.dataDisplaySeparatorChar}.
    //
    // @visibility external
    //<
    getInlineEditText : function () {
        var liveObject = this.creator;

        if (!liveObject.data) return null;

        var separatorChar = (this.inlineEditMultiline ? "\n" : this.dataSeparatorChar),
            values = liveObject.data,
            fields = liveObject.fields,
            string = ""
        ;

        for (var i = 0; i < fields.length; i++) {
            var field = fields[i],
                value = values[field.name]
            ;
            if (value != null) value = value.replace(this.dataDisplaySeparatorChar, this.dataEscapeChar + this.dataDisplaySeparatorChar);
            string = string + (string.length > 0 ? separatorChar : "") + 
                field.name + 
                (value != null ? this.dataDisplaySeparatorChar + value : ""); 
        }
        return string;
    },

    //> @method detailViewerEditProxy.setInlineEditText()
    // Save the new value into the component's state. Called by the
    // +link{editProxy.inlineEditForm} to commit the change.
    // <p>
    // Updates the component's <code>data</code> and <code>fields</code>.
    //
    // @param newValue (String) the new component data
    //
    // @visibility external
    //<
    setInlineEditText : function (newValue) {
        var separatorChar = (this.inlineEditMultiline ? "\n" : this.dataSeparatorChar);
        var value = isc.EditProxy.parseStringValueMap(newValue,
                separatorChar,
                this.dataEscapeChar,
                this.dataDisplaySeparatorChar,
                null,
                false,
                true);
        // Extract field definitions from map
        var values = value.valueMap,
            fields = []
        ;
        for (var key in values) fields.add({ name: key });
        this.creator.editContext.setNodeProperties(this.creator.editNode, { data: values, fields: fields });
    }
});

//> @class MenuEditProxy
// +link{EditProxy} that handles +link{MenuButton} and +link{MenuBar} objects when editMode is enabled.
//
// @group devTools
// @treeLocation Client Reference/Tools/EditProxy
// @visibility external
//<
isc.defineClass("MenuEditProxy", "CanvasEditProxy").addClassMethods({
    // Parse a MenuButton string which has a single menu
    parseMenuButtonString : function (string) {
        var menus = isc.MenuEditProxy.parseMenuBarString(string, true);
        return (menus.length > 0 ? menus[0] : null);
    },

    // Parse a MenuBar string which has multiple menus
    parseMenuBarString : function (string, singleMenu) {
        // Each menu (button) starts at a title definition: --, == or title:
        // and runs until the next title or end of entry
        var items = string.trim().split("\n"),
            menuLines = [],
            menus = [],
            menuTitle
        ;
        for (var i = 0; i < items.length; i++) {
            var item = items[i].trim();
            if (/^-+$/.test(item) || /^=+$/.test(item)) {
                menuLines.add(item);
                continue;
            }
            if (item.startsWith("--") || item.startsWith("==") || item.startsWith("title:")) {
                if (menuLines.length > 0) {
                    var menuItems = isc.MenuEditProxy.parseMenuString(menuLines.join("\n")),
                        menu = {
                            title: menuTitle || "Menu",
                            data: menuItems
                        }
                    ;

                    menus.add(menu);
                    menuLines = [];

                    if (singleMenu) return menus;
                } else if (menuTitle) {
                    menus.add({ title: menuTitle });

                    if (singleMenu) return menus;
                }
                if (item.startsWith("title:")) {
                    menuTitle = item.replace(/^title:/, "").trim();
                } else {
                    menuTitle = item.substring(2).trim();
                }
            } else {
                menuLines.add(item);
            }
        }

        if (menuLines.length > 0 || menuTitle) {
            var menuItems = this.parseMenuString(menuLines.join("\n")),
                menu = {
                    title: menuTitle || "Menu",
                    data: menuItems
                }
            ;

            menus.add(menu);
        }

        return menus;
    },

    // Parse a single-menu
    parseMenuString : function (string) {
        var items = string.trim().split("\n");
        var menuItems = [];
        for (var i = 0; i < items.length; i++) {
            var item = items[i];
            if (/^-+$/.test(item) || /^=+$/.test(item)) {
                menuItems.add({isSeparator: true});
            } else {
                var itemParts = item.split(",");
                var name = itemParts[0];
                var menuItem = {
                    title: isc.MenuEditProxy.parseTextWikiSymbols(itemParts[0])
                }
                if (itemParts.length > 1) {
                    menuItem.keyTitle = itemParts[1];
                }
                if (menuItem.title.endsWith(">")) {
                    menuItem.title = menuItem.title.substring(0, menuItem.title.length-1);
                    menuItem.submenu = [{}];
                }
                if (menuItem.title.startsWith("x") || menuItem.title.startsWith("o")) {
                    menuItem.title = menuItem.title.substring(1);
                    menuItem.checked = true;
                }
                if (menuItem.title.startsWith("-") && menuItem.title.endsWith("-")) {
                    menuItem.title = menuItem.title.substring(1,menuItem.title.length-1);
                    menuItem.enabled = false;
                }
                menuItems.add(menuItem);
            }
        }
        return menuItems;
    },

    // Tool function for parsing balsamiq text - it encoded using wiki-style
    // Replaces '\r' by '<br/>', '[text]' by text in a link, '*text*' by text in bold,
    // '_text_' by text in italic.
    // See:  http://support.balsamiq.com/customer/portal/articles/110121
    parseTextWikiSymbols : function (text) {
        var italic = false;
        var bold = false;
        var link = false;
        var res = [];
        for (var i = 0; i < text.length; i++) {
            var c = text.charAt(i);
            if (c == '\\') {
                if( (i + 1) < text.length && text.charAt(i + 1) == 'r') {
                    c = "<br/>";
                    i++;    
                }
            } else if (c == '[' && text.indexOf("]",i + 1) > 0) {
                c = "<a href='#'>";
                link = true;
            } else if (c == ']') {
                if (link) {
                    c = "</a>";
                    link = false;
                }
            } else if (c == '*') {
                if (bold) {
                    bold = false;
                    c = "</b>";
                } else {
                    bold = true;
                    c = "<b>";
                }
            } else if (c == '_') {
                if (italic) {
                    italic = false;
                    c = "</i>";
                } else {
                    italic = true;
                    c = "<i>";
                }
            }
            res.push(c);
        }
        return res.join("");
    },

    // Given a menu { title, data } return the wiki-style string defintion
    menuToWikiText : function (menu) {
        var string = "== " + menu.title + "\n";
        if (menu.data) {
            var menuItems = menu.data;
            for (var i = 0; i < menuItems.length; i++) {
                string += isc.MenuEditProxy.menuItemToWikiText(menuItems[i]) + "\n";
            }
        }
        return string;
    },

    // Given a menuItem return the wiki-style string defintion
    menuItemToWikiText : function (menuItem) {
        var string = "";
        if (menuItem.isSeparator) return "---";
        if (menuItem.enabled == false) string += "-";
        else if (menuItem.checked) string += "x ";
        string += menuItem.title;
        if (menuItem.submenu) string += " >";
        if (menuItem.keyTitle) string += "," + menuItem.keyTitle;

        return string;
    }
});

isc.MenuEditProxy.changeDefaults("inlineEditFormDefaults", { minHeight: 150 });

isc.MenuEditProxy.addMethods({

    // Component editor handling
    // ---------------------------------------------------------------------------------------

    supportsInlineEdit: true,
    inlineEditMultiline: true,

    //> @method menuEditProxy.getInlineEditText()
    // Returns the text based on the current component state to be edited inline.
    // Called by the +link{editProxy.inlineEditForm} to obtain the starting edit value.
    // <p>
    // Returns the component's menu definition in wiki-style.
    //
    // @visibility external
    //<
    getInlineEditText : function () {
        var liveObject = this.creator;

        var string = "";
        if (isc.isA.MenuButton(liveObject)) {
            var menu = liveObject.menu || liveObject.data;
            string += isc.MenuEditProxy.menuToWikiText({ title: liveObject.title, data: menu });
        } else {
            var menus = liveObject.menus;
            for (var i = 0; i < menus.length; i++) {
                string += isc.MenuEditProxy.menuToWikiText(menus[i]);
            }
        }
        return string;
    },

    //> @method menuEditProxy.setInlineEditText()
    // Save the new value into the component's state. Called by the
    // +link{editProxy.inlineEditForm} to commit the change.
    // <p>
    // Updates the component's menu.
    // <p>
    // Lines starting with "--", "==" or "title:" are considered titles for the
    // MenuButtons. The menuItem definitions follow the title to define the menu
    // contents.
    // <p>
    // Each menuItem title is entered on its own line. A keyTitle can follow the title
    // separated by a comma. A leading "x" or "o" marks the menuItem as checked.
    // MenuItems can be marked as disabled with a leading or trailing dash (-).
    // A sub-menu is indicated with a trailing &gt;. Any line consisting entirely of
    // one or more dashes (-) or equals (=) indicates a separator line. 
    //
    // @param newValue (String) the new component menu
    //
    // @visibility external
    //<
    setInlineEditText : function (newValue) {
        var liveObject = this.creator;

        if (isc.isA.MenuButton(liveObject)) {
            var menu = isc.MenuEditProxy.parseMenuButtonString(newValue);
            if (menu) {
                liveObject.editContext.setNodeProperties(liveObject.editNode, menu);
            } else {
                liveObject.editContext.removeNodeProperties(liveObject.editNode, ["title", "menu"]);
            }
        } else {
            // If the MenuBar was loaded the individual menus will be extracted into
            // the editTree. This is not necessary and if menus are updated on the
            // MenuBar itself they will be serialized along with the editTree nodes.
            // The editNodes are just dropped at this point.
            var editTree = liveObject.editContext.getEditNodeTree(),
                childNodes = editTree.getChildren(liveObject.editNode)
            ;
            if (childNodes && childNodes.length > 0) editTree.removeList(childNodes);

            var menus = isc.MenuEditProxy.parseMenuBarString(newValue);
            liveObject.editContext.setNodeProperties(liveObject.editNode, { menus: menus });
        }
    }
});
