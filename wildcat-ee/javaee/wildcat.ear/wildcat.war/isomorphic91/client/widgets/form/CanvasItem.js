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






//> @class CanvasItem
// FormItem which renders a Canvas inline in a DynamicForm instance.
// <P>
// CanvasItem is +link{FormItem.shouldSaveValue,shouldSaveValue}:false by default, meaning that
// no value from the CanvasItem will be present in +link{dynamicForm.getValues()} and no value will be
// saved when +link{dynamicForm.saveData()} is called.  This is appropriate if your Canvas does
// not participate in displaying or editing the values of the form and is embedded in the form
// for layout purposes only.
// <P>
// If you set +link{FormItem.shouldSaveValue,shouldSaveValue}:true, a 
// +link{CanvasItem.showValue(),showValue} event will be raised to provide a value that your
// item should display.  Handle this event by calling methods on the Canvas you've created
// to cause the value to be displayed.
// <P>
// The +link{CanvasItem.showValue,showValue} event will be triggered in various situations where
// the form receives data, including a call to +link{dynamicForm.setValues()},
// +link{dynamicForm.editRecord()}, or if +link{dynamicForm.fetchData()} is called and a Record
// is returned.  Bear in mind that the <code>showValue</code> event can be called when the form
// and your item have not yet been drawn; in this case, store the value for later display.
// <P>
// To provide a value to the form, call +link{CanvasItem.storeValue()} whenever the user changes
// the value in your Canvas.  Note that the form <b>will not</b> call
// <code>canvasItem.getValue()</code> in order to discover your item's value, so there is no
// purpose in overriding this method - use <code>storeValue</code> to proactively inform the
// form about changes instead.  This approach is necessary in order to enable change events.
// <P>
// If you cannot easily detect changes to values in your Canvas, a workaround is to call
// <code>storeValue</code> right before the form saves.
//
// @treeLocation Client Reference/Forms/Form Items
// @visibility external
//<
isc.ClassFactory.defineClass("CanvasItem", "FormItem");
isc.CanvasItem.addProperties({
    width:"*", height:null,

    
    
    //> @attr canvasItem.height (int | String : null : IRW)
    // Height of the Canvas.  Can be either a number indicating a fixed height in pixels, a
    // percentage indicating a percentage of the overall form's height, or "*" indicating take
    // whatever remaining space is available. See the +link{group:formLayout} overview for details.
    // <p>
    // Height may also be explicitly specified on the +link{canvasItem.canvas}. In this
    // any <code>canvasItem.height</code> will be ignored in favor of the value applied 
    // to the canvas directly. In either case, percentage values will be resolved using
    // standard formItem sizing rules as described in +link{group:formLayout}
    // @visibility external
    //<
    
    
    //> @attr canvasItem.multiple (Boolean : null : IR)
    // Whether this CanvasItem is intended to hold multiple values.
    // <smartgwt>
    // <P>
    // This attribute can affect the return type of getValue(). If this CanvasItem is storing
    // multiple values, then the return type of getValue() is <code>JavaScriptObject</code>
    // (a JavaScript array object) if multiple is null or false. However, if multiple is true,
    // then the return type is either <code>List</code> or <code>RecordList</code>.
    // </smartgwt>
    // @visibility external
    //<

    //> @attr canvasItem.shouldSaveValue (Boolean : false : IR)
    // Should this item's value be saved in the form's values and hence returned from
    // +link{dynamicForm.getValues,form.getValues()}?
    // <P>
    // See the +link{CanvasItem} class overview for a discussion of values-handling in 
    // CanvasItems
    // @visibility external
    //<
    shouldSaveValue:false,
    
    // override _setElementValue() to call showValue()
    _setElementValue : function (newValue, dataValue) {
        
        var undef;
        if (dataValue === undef) {  
            dataValue = this._value;
        }
        
        this.showValue(newValue, dataValue, this.form, this);
    },  
    
    // Have showvalue no-op by default -- documented in registerStringMethods
    showValue : function (displayValue, dataValue) {
    },
    
    //> @method canvasItem.storeValue()
    // @include formItem.storeValue()
	// @visibility external
	//<
	// Implemented at the FormItem level

    // ------------------------
    // Methods applied directly to this.canvas (fired in the scope of the canvas, not the
    // canvasItem-  use this.canvasItem to point back to the item).
    
    // set up observation of resizing
    
    _canvas_resized : function (deltaX, deltaY, reason) {
        this.Super("resized", arguments);

        // CanvasResized will cause a form redraw - only call this if we're not drag
        // resizing to avoid redrawing repeatedly during drag resize interactions.
        if (!this.dragResizing() && this.canvasItem) {
            this.canvasItem.canvasResized(deltaX, deltaY, reason);
        }
    },
    _canvas_dragResized : function () {
        this.canvasItem.canvasResized(1,1); // HACK: 
        return this.Super("dragResized", arguments);
    },
    
    _canvas_focusInNextTabElement : function (forward, mask) {
        if (isc.isA.DynamicForm(this)) {
            return this.Super("_focusInNextTabElement", arguments);
        } else 
            return this.canvasItem.form._focusInNextTabElement(forward, mask);
    },
    _canvas_getTabIndexSpan : function () {
        if (isc.isA.DynamicForm(this)) {
            return this.Super("getTabIndexSpan", arguments);
        }
        // This will go through all descendents recursively to figure out where they should
        // be in the tab-order.
        var tabStops = [];
        this.canvasItem._getCanvasTabDescendents(this, tabStops);
        var span = 0;
        for (var i = 0; i < tabStops.length; i++) {
            if (tabStops[i] == this) span +=1
            else span += tabStops[i].getTabIndexSpan();
        }
        return span
    },

    
  	//> @attr	canvasItem.canvas		(Canvas : null : [IRW])
    //
    // The canvas that will be displayed inside this item.  You can pass an instance you've 
    // already created, or its global ID as a String. You can also implement 
    // +link{CanvasItem.createCanvas()} to dynamically create the canvas when the FormItem
    // is initialized.
    // <P>
    // If <code>canvas</code> and <code>createCanvas()</code> are unspecified, the 
    // canvas for this item will be auto-created using the overrideable defaults:
    // +link{attr:CanvasItem.canvasProperties} and +link{attr:CanvasItem.canvasConstructor}
    // <P>
    // Note that +link{canvas.canvasItem} will be set on the canvas to point back to this
    // item.
    //
    //  @visibility external
	//<
	
	//> @attr canvas.canvasItem (CanvasItem : null : R)
	// If this canvas is being displayed in a +link{CanvasItem}, this property will be set
	// to point at the item. Otherwise this property will be null.
	// @visibility external
	//<

  	//> @attr	canvasItem.canvasConstructor		(String : "Canvas" : [IRW])
    //
    // If +link{canvasItem.canvas, this.canvas} is not specified as a canvas instance at init
    // time, a canvas will be created instead. This property denotes the class of that widget
    // (Should be set to the name of a subclass of Canvas).
    //
    //  @visibility external
	//<
    
    
    
    //> @attr	canvasItem.canvasDefaults		(Object : {} : [IRW])
    //
    // Default properties for the canvas if this.canvas is not already a canvas instance.
    //
    //  @visibility external
	//<
    
    //> @attr	canvasItem.canvasProperties		(Object : {} : [IRW])
    //
    // Properties to apply to this canvas on creation if this.canvas is not already a canvas 
    // instance.
    //
    //  @visibility external
	//<
    

    //> @attr canvasItem.autoDestroy (Boolean : false : [IRWA])
    // Should this item's +link{canvasItem.canvas,canvas} be automatically destroyed when the item 
    // is destroyed? Form items are destroyed automatically when a call to
    // +link{DynamicForm.setItems()} removes them from their parent form, or if their
    // parent form is destroyed. This property governs whether, when this occurs, 
    // the item's canvas should also be +link{Canvas.destroy(),destroyed}.
    // <P>
    // Setting this property to true
    // is typically appropriate for cases where a custom CanvasItem automatically creates
    // its canvas as part of its initialization flow.
    // Note that once a canvas has been destroyed it can not be re-used elsewhere within
    // an application.
    //
    // @visibility external
    //<
    
    autoDestroy: false

    //showTitle:false
    
    //> @attr CanvasItem.overflow (Overflow : null : IR)
    // CanvasItems support specifying overflow for the Canvas directly on the item.
    // @visibility external
    //<
});
isc.CanvasItem.addMethods({
    init : function () {
        this.Super("init", arguments);
        this._createCanvas(this.form, this);
        this.canvasInitialized = true;
    },
    
    // A straight canvasItem is non-editable. Subclasses that allow editing (such as the sliderItem)
    // will override this method to return true
    
    isEditable : function () {
        return false;
    },
    
    // override _canFocus -- if our canvas is focusable, we're focusable
    _canFocus : function () {
        // If the developer explicitly marks the item as canFocus true, assume they know what
        // they're doing.
        if (this.canFocus == true) return this.canFocus;
        var canvas = this.canvas;
        if (canvas && canvas._canFocus) return canvas._canFocus();
        return false;
    },
    
    // _createCanvas    Method to ensure that the canvas for this item has been instantiated
    // with appropriate properties, and added to the containerWidget as a child.
    _createCanvas : function () {

        // CanvasItem.canvas
        // - can be defined as a live canvas instance
        // - can be defined as a properties block to turn into a live canvas
        // If this.createCanvas() is specified, this.canvas is dynamically set to the
        // result of calling that method
        // 
        // If this.canvas is still unspecified we'll use the autoChild pattern
        // (canvasConstructor, canvasDefaults, canvasProperties)
        //
        // Undocumented: The autoChild subsystem also supports dynamically getting defaults via
        // getDynamicDefaults() which would allow dynamic customization of the canvas autoChild
        // when it's auto-created.
        if (this.createCanvas != null) {
            var canvas = this.canvas;
            this.canvas = this.fireCallback("createCanvas", "form,item", arguments);
            // if this.createCanvas existed but returned nothing, hang onto our original
            // canvas object
            // It could just modify it in place and we should allow that
            if (this.canvas == null) this.canvas = canvas;
        }
        
        //>DEBUG
        if (!isc.isAn.Object(this.canvas) && !this.canvasProperties && !window[this.canvas]) {

            //>EditMode
            // Visual Builder habitually creates CanvasItems which will be furnished with a canvas
            // only after insertion into the editContext (because the canvas itself also needs to 
            // be put into edit mode and added to the editContext).  We don't want the default 
            // canvas because it means we get a warning in the log about creating it, then another  
            // warning immediately that we've deleted it...
            if (isc.designTime) return;
            //<EditMode

            this.logWarn("CanvasItem: " + 
                        (this.getFieldName() ? this.getFieldName() : this.getID()) + 
                        " defined with no canvas property - creating a default " +
                        "canvas for this item.");
        }
        //<DEBUG
        
        // If the supplied canvas property is a string containing the ID of a valid Canvas 
        // object, use that Canvas object 
        if (!isc.isAn.Object(this.canvas) && isc.isA.Canvas(window[this.canvas])) {
            this.canvas = window[this.canvas]
        }

        var canvasProps = {
            // don't redraw when the form redraws - if the developer wishes to redraw this canvas
            // they will have to call redraw() on this form item, or on the Canvas directly.
            _redrawWithParent: false,
            // one time flag to suppress automatic drawing on addChild().  Very key, as a form
            // that has setItems() called on it after creation recreates all items with the
            // form already drawn.
            _dontDrawOnAddChild: true,
            
            // Don't auto-destroy with parent.
            // We can render our canvas as a child of some
            // arbitrary container widget, and we don't want it to be destroyed automatically
            // when that gets destroyed if our form / formItem isn't explicitly destroyed.
            // We use this pattern in Grid editing / filterEditor stuff: If the grid-body 
            // in which the edit item was embedded is destroyed and rebuilt, we use the
            // same FormItem in the new version of the grid body, so we just reparent the
            // canvas rather than destroying and rebuilding.
            // If the form is actually destroyed we should get a destroy call on the item
            // which will clean up the canvases of all CanvasItems.
            destroyWithParent:false,
            
            // Always set initial tabIndex to -1. This will avoid the canvas from ever getting
            // an auto-assigned tab index and having a prev/next tab-widget.
            // Note that as part of 'setElementTabIndex()' we will explicitly assign the
            // desired tabIndex.
            tabIndex:-1,
            
            //hideUsingDisplayNone:true,

            disabled: this.shouldDisableCanvas() 
        };
        canvasProps.resized = this._canvas_resized;
        canvasProps.dragResized = this._canvas_dragResized;
        
        // Have standard events on the canvas fall back through to us.
        
        
        canvasProps.handleClick = function () {
            var rv = this.Super("handleClick", arguments);
            // if this is a StatefulCanvas, rely on the 'handleActivate' path to 
            // fire the item level click handler.
            if (!isc.isA.StatefulCanvas(this) && this.canvasItem) {
                rv = this.canvasItem.handleClick() && rv;
            }
            return rv;
        }
        // handleActivate will only be called for StatefulCanvas and subclasses
        canvasProps.handleActivate = function () {
            var rv = this.Super("handleActivate", arguments);
            if (this.canvasItem) rv = this.canvasItem.handleClick() && rv;
            return rv;
        }
        
        canvasProps.handleDoubleClick = function () {
            var rv = this.Super("handleDoubleClick", arguments);
            if (this.canvasItem) rv = this.canvasItem.handleDoubleClick() && rv;
            return rv;
        }
        
        canvasProps.handleKeyPress = function () {
            var rv = this.Super("handleKeyPress", arguments);
            if (this.canvasItem) {
                // fire item.keyPress / form.itemKeyPress if we get a keyPress on the 
                // target canvas (or in fact a sub-canvas thereof).
                var itemRV = this.canvasItem._fireKeyPressHandlers(
                        this.canvasItem,
                        this.canvasItem.form,
                        isc.EH.getKey(),
                        isc.EH.getKeyEventCharacterValue()
                );
                if (itemRV == false) rv = false;
            }
            return rv;
        }

        canvasProps.handleKeyDown = function (event,eventInfo) {
            var rv = this.Super("handleKeyDown", arguments);
            if (this.canvasItem) {
                var itemRV = this.canvasItem.handleKeyDown(event, eventInfo);
                if (itemRV == false) rv = false;
            }
            return rv;
        }
        canvasProps.handleKeyUp = function (event,eventInfo) {
            var rv;
            if (this.keyUp != null) {
                rv = this.keyUp(event, eventInfo);
            }
            if (this.canvasItem) {
                var itemRV = this.canvasItem.handleKeyUp(event, eventInfo);
                if (itemRV == false) rv = false;
            }
            return rv;
        }
        
        // Override synthetic focus manipulation methods to fall back to the DF, since that
        // already manages moving focus between form items on tab / shift tab keypress when
        // the clickMask is up 
        canvasProps._focusInNextTabElement = this._canvas_focusInNextTabElement;
        
        canvasProps.getTabIndexSpan = this._canvas_getTabIndexSpan;

        // pass our datasource, if any to the CanvasItem
        if (this.dataSource) canvasProps.dataSource = this.dataSource;
        // pass on our prompt (if any) to the CanvasItem
        if (this.prompt && this.applyPromptToCanvas) canvasProps.prompt = this.prompt;
        // pass on our overflow (is specified) to the canvas.
        if (this.overflow != null) canvasProps.overflow = this.overflow;
        
        // We'll set the tabIndex on the canvas when we write out our innerHTML.
        
        
        // pass on our accessKey if appropriate
        if (this.accessKey != null) canvasProps.accessKey = this.accessKey;
        
        // pass on 'showFocused'
        if (this.showFocused != null) canvasProps.showFocused = this.showFocused;
        if (this.showFocusedAsOver != null) canvasProps.showFocusedAsOver = this.showFocusedAsOver;
        
        if (isc.isA.String(this.canvas) && window[this.canvas]) this.canvas = window[this.canvas];
        
        // if the canvas hasn't been instantiated for us, use the autoChild method to create it
        // otherwise we have to do a bunch of manual patching to achieve the same effect.
        if (!isc.isA.Canvas(this.canvas)) {
            // pick up any properties specified directly on the 'this.canvas' object
            isc.addProperties(canvasProps, this.canvas, { canvasItem: this });
            if (canvasProps.ID != null) canvasProps.ID = null;
            // since we're auto-creating it's appropriate to autoDestroy
            this.autoDestroy = true;
            
            this.addAutoChild("canvas", canvasProps, isc.Canvas, this.containerWidget);
        } else {
            
            var originalProps = {};
            for (var prop in canvasProps) {
                originalProps[prop] = this.canvas[prop];
            }
            this.canvas._originalProps = originalProps

            // apply dynamic properties to existing canvas
            // (Call setter methods wherever necessary).
            this.canvas.setTabIndex(-1);
            if (this.applyPromptToCanvas) this.canvas.setPrompt(this.prompt);
            this.canvas.setAccessKey(this.accessKey);
            
            isc.addProperties(this.canvas, canvasProps, { canvasItem: this });
            if (canvasProps.dataSource) this.canvas.bindToDataSource();
            this.canvas.setDisabled(this.shouldDisableCanvas());
            this.containerWidget.addChild(this.canvas);
        }
        
        // We'll handle percent sizing specified on the embedded Canvas directly
        if (this.canvas != null) {
            this.canvas._initialPercentBox = this.canvas.percentBox;
            this.canvas.percentBox = "custom";
        }
        
        // If we're added to a containerWidget rather than the form, and a clickMask is showing
        // we may need to unmask explicitly
        
        if (this.containerWidget != this.form) {
            if (isc.EH.clickMaskUp()) {
                var CMIDs = isc.EH.getAllClickMaskIDs();
                for (var i = CMIDs.length -1; i >= 0; i--) {
                    var parentMasked = isc.EH.targetIsMasked(this.containerWidget, CMIDs[i]);
                    if (!parentMasked) {
                        isc.EH.addUnmaskedTarget(this.canvas, CMIDs[i]);
                        // We're iterating down from the top - once a widget is over one mask it's also
                        // over any masks below that one. Therefore we don't need to keep iterating 
                        // down to the bottom adding unmasked targets.
                        break;
                    }
                }
            }
        }
        if (this.canvas) this.observe(this.canvas, "_focusChanged", "observer.canvasFocusChanged()");
        
        if (isc.isA.DynamicForm(this.canvas)) {
            this.observe(this.canvas, "setFocusItem", "observer.nestedFormSetFocusItem()");
        }
    },
    
    //> @method canvasItem.setCanvas()
    // Setter to update the +link{canvasItem.canvas} at runtime
    // @param canvas (Canvas) New canvas to display.
    // @visibility external
    //<
    setCanvas : function (canvas) {
        if (isc.isA.Canvas(this.canvas)) {
            if (this.canvas != canvas) {
                // "destroy" the old canvas.
                // if this.autoDestroy is true, this will truly destroy it
                // otherwise it simply deparents it and cleans up various props we
                // scribble onto it connecting it with the CanvasItem.
                this._destroyCanvas();
            // nothing to do if we're passed in our existing canvas
            } else {
                return;
        }
        }
        
        if (canvas) this.canvas = canvas;
        // No need to call _createCanvas if we haven't yet completed init (occurs automatically at the
        // end of init())
        if (this.canvasInitialized) {
            this._createCanvas();
            // a redraw appears required to initially size and draw the CanvasItem.
            if (this.containerWidget.isDrawn()) this.containerWidget.markForRedraw();
        }
    },
    
    
    /*
    // Ensure redraw on this item redraws the Canvas.
    redraw : function (suppressCanvasRedraw) {
        // redraw the canvas before redrawing our innerHTML - allows us to calculate sizes
        // accurately, if they change.
        if (!suppressCanvasRedraw && this.canvas.isDrawn()) {
            this.canvas.redraw("canvasItem.redraw");
        }
        this.Super("redraw", arguments);
    },
    */


    //> @method canvasItem.canEditChanged()
    // Notification method called when +link{formItem.canEdit} is modified. Developers
    // may make use of this to toggle between an editable and a read-only appearance
    // for custom FormItems.
    // @param canEdit (boolean) New canEdit value
    // @visibility external
    //<

    //> @method canvasItem.readOnlyDisplayChanged()
    // Notification method called when +link{formItem.readOnlyDisplay} is modified. Developers
    // may make use of this to toggle between read-only appearances for custom FormItems.
    // @param appearance (ReadOnlyDisplayAppearance) New readOnlyDisplay value
    // @visibility external
    //<

    
    redrawn : function (suppressCanvasRedraw) {
        this.Super("redrawn", arguments);
        var disabled = this.shouldDisableCanvas();
        if (this.canvas.isDisabled() != disabled) this.canvas.setDisabled(disabled);
    },

    drawn : function () {
        this.Super("drawn", arguments);
        var canvas = this.canvas;
        if (canvas && canvas.parentElement != this.containerWidget) {
            this._createCanvas();
            this.placeCanvas();
        }
    },

    // clear the pointer on this item's canvas back to this item on destroy()
    destroy : function () {
        this._destroyCanvas();
        return this.Super("destroy", arguments);
    },

    _destroyCanvas : function () {
        if (this.canvas) {
            delete this.canvas.canvasItem;
            var parentElement = this.canvas.getParentCanvas();
            delete this.canvas.parentElement;
            if (parentElement == this.containerWidget) {
                this.containerWidget.removeChild(this.canvas);
            }
            if (this.autoDestroy) {
                this.canvas.destroy(true);
            } else {
                
                isc._allowDeleteFuncProperty = true;

                // reset the various odd overrides to resized etc that won't apply without
                // a canvasItem
                isc.addProperties(this.canvas, this.canvas._originalProps);
                delete this.canvas._originalProps;

                delete isc._allowDeleteFuncProperty;
                
                // clear and pull out of the containerWidget.
                if (this.canvas.isDrawn()) {
                    this.canvas.clear();
                }
                this.canvas.deparent();
                
                // ignore the focus methods we observed at init
                this.ignore(this.canvas, "_focusChanged");
                if (isc.isA.DynamicForm(this.canvas)) {
                    this.ignore(this.canvas, "setFocusItem");
                }
                
                // reset the "percentBox" flag.                
                this.canvas.percentBox = this.canvas._initialPercentBox;
                delete this.canvas._initialPercentBox;

                // and set this.canvas to null so this code doesn't run again later
                this.canvas = null;
            }
        }
    },

    placeCanvas : function (delayed) {
        var canvas = this.canvas;

        
        if (canvas.destroyed) return;

        // show or hide the Canvas according to whether this item is visible, so that showIfs work
        if (this.visible == false) {
            canvas.hide();
            
            isc.Canvas.moveOffscreen(canvas);
            return;
        }
 
        if (this.form && !this.form.isDrawn() && this.form.position == isc.Canvas.RELATIVE) {
            
            //this.logWarn("hiding Canvas during initial relative draw");
            canvas.hide();
            return;
        }

        // place the Canvas over the cell for the item

        // figure out the spacer's coordinates within the form
        var containerWidget = this.containerWidget,
            containerHandle = containerWidget.getHandle(),
            spacerParent = isc.Element.get(this.getID() + "_spacerParent");

        
        if (isc.Browser.isIE && !isc.Browser.isStrict && isc.Browser.version >= 9) {
            spacerParent = spacerParent && spacerParent.firstChild;
        }

        var spacerOffsets = spacerParent ? isc.Element.getOffsets(spacerParent, containerHandle) : [0,0],
            left = spacerOffsets[isc.Canvas.LEFT] + containerWidget.getScrollLeft(),
            top = spacerOffsets[isc.Canvas.TOP] + containerWidget.getScrollTop();

        
        //>SingleDiv
        if (!containerWidget._drewClipDiv) {
            var borderSizes = containerWidget._calculateBorderSize();
            left -= borderSizes.left;
            top -= borderSizes.top;
        }
        //<SingleDiv

        // this.logWarn("placing Canvas at: " + [left, top]);

        // place the Canvas on top of that cell
        if (canvas.getParentCanvas() != containerWidget) {
            containerWidget.addChild(canvas);
        }
        canvas.moveTo(left, top);
        if (canvas.visibility == isc.Canvas.HIDDEN) {
            canvas.show();
        }
        if (!canvas.isDrawn()) {
            canvas.draw();
        }

        
        if (!delayed && isc.Browser.isMac && isc.Browser.isMoz && left == 0 && top == 0) {
            isc.Timer.setTimeout({target:this, methodName:"_delayedPlaceCanvas"}, 0);
        }
    },

    _delayedPlaceCanvas : function () {
        this.placeCanvas(true);
    },

    cleared : function () {
        this.Super("cleared", arguments);
        // when a canvasItem gets cleared, ensure the canvas is cleared too
        if (this.canvas && this.canvas.isDrawn()) {
            this.canvas.clear();
            // Set the one-time flag to reset handle size on adjust overflow and
            // mark for adjust overflow
            this.containerWidget._resetHandleOnAdjustOverflow = true;
            this.containerWidget._markForAdjustOverflow("CanvasItem canvas cleared");
        }
    },
    
    ensureCanvasCleared : function () {
        if (this.canvas && this.canvas.isDrawn()) {
            this.canvas.clear();
            this.containerWidget._resetHandleOnAdjustOverflow = true;
            this.containerWidget._markForAdjustOverflow("CanvasItem canvas cleared");
        }
    },

    // ensure the canvas floats in the right place if the item is moved
    moved : function () {
        if (this.isDrawn() || (this.containerWidget != null && this.containerWidget.isDrawn())) {
            this.placeCanvas();
        }
    },

    // Sizing
    // ---------------------------------------------------------------------------------------
    

    checkCanvasOverflow : function () {
        return this.sizeCanvas(true);
    },
    
    // Historically we sized the canvas to fit the entire cell (using getInnerWidth/height).
    // This is actually not what we want and has been reported as a bug (for example here
    // http://forums.smartclient.com/showpost.php?p=96220&postcount=7)
    // We now use getTextBoxWidth() / height which sizes in a manner consistent with other
    // form items.
    // Retain an undocumented flag to use the old behavior as a hedge against backcompat
    // issues going forward
    sizeCanvasAsTextBox:true,
    _sizeTextBoxAsContentBox : function () {
        return false;
    },

    sizeCanvas : function (firstResizePass) {
        var canvas = this.canvas;

        // if we can't overflow in the height direction, we don't need to do anything on the
        // first sizing pass
        if (firstResizePass && 
            !(canvas.overflow == isc.Canvas.VISIBLE || canvas.overflow == isc.Canvas.CLIP_H))
        {
            this.logDebug("ignoring first pass, can't overflow", "canvasItemSizing");
            return;
        }

        // get the sizes specified by layout policy
        var policyWidth = this.sizeCanvasAsTextBox ? this.getTextBoxWidth() 
                                                    : this.getInnerWidth(),
            policyHeight = this.sizeCanvasAsTextBox ? this.getTextBoxHeight() 
                                                    : this.getInnerHeight(),
            resizeWidth, resizeHeight;
        // we feed the specified height (whether it appears on the Canvas or CanvasItem) to 
        // TableResizePolicy; if we give it a pixel size it will feed that back.  If we give it
        // a variable size (percent or "*"), that size will be incorporated into sizing the row
        // as a whole and we'll get the row height back.
        
        
        if (this.heightIncludesTitle && 
            this.showTitle && this.getTitleOrientation() == isc.Canvas.TOP) 
        {
            policyHeight -= this.form.getTitleHeight(this);
        }
        resizeHeight = policyHeight;

        // TableResizePolicy doesn't consider the specified width of items when determining
        // column widths.  Hence only apply the width if our width is unset or "*"
        var specifiedWidth = canvas._userWidth || this.width;
        resizeWidth = (specifiedWidth == null || specifiedWidth == "*" ? 
                       policyWidth : specifiedWidth);

        var percentWidth = canvas._percent_width,
            percentHeight = canvas._percent_height;
        
        if (percentWidth != null && percentWidth.endsWith("%")) {
            resizeWidth = Math.round(policyWidth * (parseInt(percentWidth)/100));
        }
        
        // if width is not increasing past the current overflowed size, don't try to reduce the
        // height of an height-overflowed Canvas to less than the overflowed size, because 
        // there's no reason to expect it to shrink (unless it's dirty, in which case we assume
        // it might change size)
        
        if (!canvas.isDirty() &&
            (resizeWidth == null || resizeWidth <= canvas.getVisibleWidth()) &&
            canvas.getHeight() < canvas.getVisibleHeight() && 
            resizeHeight <= canvas.getVisibleHeight()) 
        {
            this.logDebug("not applying height: " + resizeHeight + 
                          " to overflowed Canvas with height: " + canvas.getVisibleHeight(),
                          "canvasItemSizing");
            resizeHeight = null;
        }
        

        if (!isc.isA.Number(resizeWidth)) resizeWidth = null;
        if (!isc.isA.Number(resizeHeight)) resizeHeight = null;
        // actually call 'resizeTo' on the canvas
        this._setCanvasSize(resizeWidth,resizeHeight,percentWidth,percentHeight);

        this.logDebug("this._size: " + this._size + 
                      ", policy size: " + [policyWidth, policyHeight] +
                      ", specifiedSize: " + [specifiedWidth, canvas._userHeight || this.height] +
                      ", Resized Canvas to: " + [resizeWidth, resizeHeight],
                      "canvasItemSizing");

        // draw or redraw the Canvas so we get an accurate size
        if (!canvas.isDrawn()) {
            // skip the case where the containerWidget is undrawn (for example getting PrintHMTL of
            // an undrawn form)
            var containerDrawnState = this.containerWidget.getDrawnState();
            if (containerDrawnState == isc.Canvas.COMPLETE || 
                containerDrawnState == isc.Canvas.HANDLE_DRAWN) {
                    // move offscreen before draw so we don't impact the scroll height of
                    // the parent if we're not actually drawn
                    isc.Canvas.moveOffscreen(canvas);
                    canvas.draw();
            }
            
        } else canvas.redrawIfDirty("CanvasItem getting new size");
            
        var width = canvas.getVisibleWidth(),
            height = canvas.getVisibleHeight();

        this.logDebug("visible size of embedded Canvas: " + [width, height], 
                      "canvasItemSizing");
        
        // if the Canvas overflows in the height direction, set this as a minimum
        
        if (!firstResizePass) this.minHeight = null;
        else this.minHeight = height > canvas.getHeight() ? height : null;

        // policyHeight is the space allocated to the row(s) this Canvas spans.  If we've
        // exceeded the space that table sizing policy allocated to our row(s), the policy will
        // need to be rerun, in order to reallocate space among other items that can flex.
        // Note: doesn't matter if we've exceeded width; our width isn't taken into account by
        // the policy.
        // Note: doesn't matter whether the Canvas has actually overflowed it's own specified
        // size, which might be much smaller; eg a button next to a TextArea doesn't need to be
        // as tall as the TextArea.
        if (height > policyHeight) return true;
    },
    
    _setCanvasSize : function (resizeWidth, resizeHeight, percentWidth, percentHeight) {
        this._resizingCanvas = true;
        this.canvas.resizeTo(resizeWidth, resizeHeight);
        if (percentWidth != null) this.canvas._percent_width = percentWidth;
        if (percentHeight != null) this.canvas._percent_height = percentHeight;
        this._resizingCanvas = false;
    },

    // Rather than embedding HTML for the canvas into the form, we write out a spacer of the
    // appropriate size, and float the Canvas over it.
    getElementHTML : function (value) {
        var canvas = this.canvas;

        

        // size the Canvas to the final size determined by the resize policy
        this.sizeCanvas();

        // Ensure that the canvas has it's tab index written out as specified
        this._setElementTabIndex(this.getGlobalTabIndex());

        

        // IE has an issue with getOffsetTop method if there is transitional doctype is used
        // to overcome this we add invisible 0px border - this helps somehow
        var invisibleBorder = "";
        if (isc.Browser.isIE && isc.Browser.isTransitional) {
            invisibleBorder = "border:0px solid transparent;"
        }
        return "<SPAN style='padding:0px;margin:0px;" + invisibleBorder + "' ID='"
                + this.getID() + "_spacerParent'>" 
                + isc.Canvas.spacerHTML(canvas.getVisibleWidth(), canvas.getVisibleHeight()) 
                + "</SPAN>";
    },

    getPrintHTML : function (printProperties, callback) {
        // If we're printing, write the printHTML for our canvasItem directly into the item.
        
        if (this.canvas == null) return isc.nbsp;
        return this.canvas.getPrintHTML(printProperties, callback);
    },

    
    _applyHandlersToElement : function () {
        this._setUpIconEventHandlers();
    },
        
    
    // return specified widths (which can be %s or *s), whether they appear on the Canvas or
    // CanvasItem, to be fed to the table resize policy.  If there is no specified size, feed
    // the layout policy our default height.
    getHeight : function (reportOverflowedSize) {
        var canvas = this.canvas;
        if (reportOverflowedSize) {
            var visibleHeight = canvas.getVisibleHeight();
            if (visibleHeight > canvas.getHeight()) return visibleHeight;
        }
        return this.canvas._userHeight || this.height || this.canvas.defaultHeight;
    },

    getWidth : function () {
        return this.canvas._userWidth || this.width || this.canvas.defaultWidth;
    },

    // handle setWidth/Height(), as well as resize (below) - needed, for example, when used as 
    // an item in the FilterEditor of a ListGrid where a user can resize columns
    setWidth : function (width) {
        this.Super("setWidth", arguments);
        this.canvas.setWidth(Math.max(1, this.getInnerWidth() - this.getTotalIconsWidth()));
    },
    setHeight : function (height) {
        this.Super("setHeight", arguments);
        this.canvas.setHeight(this.getInnerHeight());
    },

    // if the Canvas is resized by the user or programmatically outside of CanvasItem's layout
    // code, 
    canvasResized : function (deltaX, deltaY, reason) {
        if (this._resizingCanvas) return;
        
        var canvas = this.canvas,
            newWidth = Math.max(1, canvas.getWidth()),
            newHeight = canvas.getHeight();

        
        if (reason != "init" && 
            reason != "overflow" && reason != "Overflow on initial draw") 
        {
            if (deltaX != null && deltaX != 0) canvas._userWidth = newWidth;
            if (deltaY != null && deltaY != 0) canvas._userHeight = newHeight;
        }

        this.logDebug("canvas resized: new specified sizes: " + [newWidth, newHeight],
                      "canvasItemSizing");

        if (!canvas.isDrawn()) return;
                      
        // redraw to change size
        this.redraw();
    },

    // shouldDisableCanvas - should we explicitly mark this items canvas as disabled
    // true if the item is disabled or read-only
    
    shouldDisableCanvas : function () {
        return this.isDisabled() || this.isReadOnly();
    },

    setReadOnlyDisplay : function (appearance) {
        this.readOnlyDisplay = appearance;
        if (this.canvas) {
            if (isc.isA.DynamicForm(this.canvas)) {
                this.canvas.readOnlyTextBoxStyle = this.getReadOnlyTextBoxStyle();
                this.canvas.setReadOnlyDisplay(appearance);
            }
        }
        if (this.readOnlyDisplayChanged) this.readOnlyDisplayChanged(appearance);
        if (!this.getCanEdit() && this.isDrawn()) this.redraw();
    },

    setCanEdit : function (canEdit) {
        var wasEditable = !this.isReadOnly();
        this.canEdit = canEdit;
        var isEditable = !this.isReadOnly();
        if (wasEditable != isEditable) {
            if (this.canvas) {
                if (this.canvas.setCanEdit) this.canvas.setCanEdit(canEdit);
            }
            this.updateCanEdit();
            if (this.canEditChanged) this.canEditChanged(canEdit);
            this.redraw();
        }
    },

    // override 'updateDisabled()' to disable the canvas
    updateDisabled : function (settingFormHandleDisabled) {
        this.Super("updateDisabled", arguments);
        if (!settingFormHandleDisabled || this.form == null || !this.form.contains(this.canvas)) {
            this.canvas.setDisabled(this.shouldDisableCanvas());
        }
    },

    // override 'updateCanEdit()' to simulate read-only state on the canvas by
    // disabling canvas but leaving item enabled so title doesn't use disabled state.
    updateCanEdit : function () {
        this.Super("updateCanEdit", arguments);
        this.canvas.setDisabled(this.shouldDisableCanvas());
    },

    // Update enabled/disabled state of the element to match our read-only/disabled state.
    // This avoids a complete redraw when changing read-only state.
    setElementReadOnly : function () {
        this._setElementEnabled(!this.isReadOnly() && !this.isDisabled());
    },

    // Override _setElementTabIndex() to update the tabindex of the canvas (and avoid redrawing
    // the form)
    _setElementTabIndex : function (index) {
        this._setCanvasTabIndex(index);
    },
    
    _getCanvasTabDescendents : function (canvas, targetArray) {
        // If a CanvasItem contains a DF which contains another CanvasItem, we already
        // manage the tab-index of the form - no need to attempt to directly manage
        // the tabIndex of canvii embedded in it via canvasItems!
        if (canvas.canvasItem != null && canvas.canvasItem != this) return;
        targetArray.add(canvas);
        var children = canvas.members || canvas.children || {};
        for (var i = 0; i < children.length; i++) {
            this._getCanvasTabDescendents(children[i], targetArray);
        }
    },
    
    _setCanvasTabIndex : function (index) {
        //this.logWarn(this.name + " setCanvasTabIndex running - index:" + index);
        var canvas = this.canvas,
            widgets = [];
        if (canvas) this._getCanvasTabDescendents(canvas, widgets);
        
        for (var i = 0; i < widgets.length; i++) {
            canvas = widgets[i];
            // Don't assign tab index if widget canvas is explicitly not a tab stop.
            // Avoid skipping base canvas.
            if (canvas != this.canvas && canvas.tabIndex == -1) continue;
            
            // clears any pointers to prev/next in auto-tab-order
            canvas._removeFromAutoTabOrder();
            // use the internal method so we don't hit the user-specified tabIndex ceiling
            //this.logWarn("assigning:" + index + " to " + canvas);            
            canvas._setTabIndex(index, false);
            // increment
            
            index += canvas == this.canvas ? 1 : canvas.getTabIndexSpan();
        }
    },
    
    // Override focusInItem / blurFocusItem to actually put focus into the canvas
    focusInItem : function (focusAtEndDirection) {
        if (this.canvas) {
            // We may be marked as canFocus:true and have a 'canvas' thats not explicitly
            // focusable but has focusable descendants. 
            var targets = [],
                canvas;
            this._getCanvasTabDescendents(this.canvas, targets);
            var start = focusAtEndDirection == false ? targets.length-1 : 0,
                end = focusAtEndDirection == false ? 0 : targets.length-1,
                step = focusAtEndDirection == false ? -1 : 1;
            
            for (var i = start; focusAtEndDirection != false ? i <= end : i >= end; i+=step) {
                if (targets[i].isDrawn() && targets[i].isVisible() && 
                    !targets[i].isDisabled() && targets[i]._canFocus()) 
                {
                    if (this.logIsDebugEnabled("syntheticTabIndex") && focusAtEndDirection != null) {
                        this.logDebug("focusInItem() - shifting focus to " +
                            (focusAtEndDirection ? "start" : "end") + 
                            " - moving focus to canvas:" + canvas,
                             "syntheticTabIndex");
                    }
                    canvas = targets[i];
                    break;
                }
            }
            if (canvas) {
                if (focusAtEndDirection != null) {
                    canvas.focusAtEnd(focusAtEndDirection);
                } else {
                    canvas.focus();
                }
                return;
            }
        }
        var isSynthetic = (focusAtEndDirection != null),
            showLogs= isSynthetic ? this.logIsDebugEnabled("syntheticTabIndex") : 
                        this.logIsDebugEnabled("nativeFocus");
        if (showLogs) {
            this.logDebug("focusInItem() unable to find focusable canvas." + 
                (isSynthetic ? " Attempting to focus at " + 
                            (focusAtEndDirection ? "start" : "end") : ""),
                 (isSynthetic ? "syntheticTabIndex" : "nativeFocus"));
        }
        return this.Super("focusInItem", arguments);
    },
    
    blurItem : function () {
        if (this.canvas) this.canvas.blur();
        return this.Super("blurItem", arguments);
    },
    
    // Override moveFocusWithinItem to handle shifting focus to nested descendants.
    _moveFocusWithinItem : function (forward) {
        var showLogs = this.logIsDebugEnabled("syntheticTabIndex");
    
        var focusCanvas = isc.EH.getFocusCanvas(),
            canvas = this.canvas,
            widgets = [];
        if (canvas) this._getCanvasTabDescendents(canvas, widgets);
        for (var i = 0; i < widgets.length; i++) {
            if (focusCanvas == widgets[i]) {
                var step = forward? 1 : -1,
                    current = i + step;
                var prevCanvas = widgets[i],
                    nextCanvas = widgets[current];
                while (nextCanvas != null) {

                    if (nextCanvas.isDrawn() && nextCanvas.isVisible()
                        && !nextCanvas.isDisabled() && nextCanvas._canFocus())
                    {
                        
                        if (!(!forward && isc.ListGrid &&
                              ( 
                                (isc.isA.ListGrid(nextCanvas.creator) &&
                                 (nextCanvas.creator.header == nextCanvas || 
                                  nextCanvas.creator.frozenHeader == nextCanvas)
                                ) ||
                                (isc.isA.ListGrid(nextCanvas) && 
                                 (nextCanvas.header == prevCanvas ||
                                  nextCanvas.frozenHeader == prevCanvas)
                                )
                              )
                             ) &&

                        
                            (isc.isA.DynamicForm(nextCanvas) &&
                             nextCanvas._getStartItemForFocusAtEnd(forward) != null)
                           )
                        {
                            if (showLogs) {
                                this.logDebug("CanvasItem shifting focus " +
                                    (forward ? "forward" : "backward")
                                    + " from " + 
                                    focusCanvas + " to " + nextCanvas,
                                    "syntheticTabIndex");
                            }
                            nextCanvas.focusAtEnd(forward);
                            return true;
                        }
                    }
                    prevCanvas = nextCanvas;
                    current = current + step;
                    nextCanvas = widgets[current];
                }
                // Must have been the last canvas
                break;
            }
        }
        if (showLogs) {
            this.logDebug("canvasItem.moveFocusWithinItem() current focus canvas:" + 
                focusCanvas + ", unable to find next focus canvas", "syntheticTabIndex");
        }
        return false;
    },
    
    // observation of focusChanged on the Canvas
    
    canvasFocusChanged : function () {
        
        var hasFocus = this.canvas.hasFocus;
        if (hasFocus) {
            this.form.setFocusItem(this);
            this.elementFocus();
        } else this.elementBlur();
    },
    
    
    nestedFormSetFocusItem : function () {
        this.form.setFocusItem(this);
    },
    
    //> @method canvasItem.hasAdvancedCriteria()
    // Overridden to return true if +link{canvasItem.canvas} is a dynamicForm.
    // See <smartclient>+link{getCriterion()}</smartclient>
    // <smartgwt><code>setCriterionGetter()</code></smartgwt>
    // for details of editing advanced criteria using nested
    // dynamicForms.
    // @return (Boolean) true if this item's canvas is a DynamicForm
    // @group criteriaEditing
    // @visibility external
    //<
    hasAdvancedCriteria : function () {
        if (this.editCriteriaInInnerForm && isc.isA.DynamicForm(this.canvas)) return true;
        return this.Super("hasAdvancedCriteria", arguments);
    },

    //> @method canvasItem.canEditCriterion()
    // AdvancedCriteria objects may be edited via nested dynamicForms as described in
    // +link{canvasItem.getCriterion()}
    // <P>
    // This method has been overridden to return true if this item's canvas is a DynamicForm,
    // where the +link{dynamicForm.operator} matches the operator of the criterion passed in
    // and dynamicForm contains items where +link{formItem.canEditCriterion()} returns true
    // for each sub-criterion in the object passed in.
    // @param criterion (Criterion) criteria to test
    // @return (boolean) returns true if the specified criterion may be edited by this item
    // @group criteriaEditing
    // @visibility external
    //<
    canEditCriterion : function (criterion) {
        if (this.editCriteriaInInnerForm && isc.isA.DynamicForm(this.canvas)) {
            if (criterion.operator != this.canvas.operator) return false;
            //this.logWarn("going to compare criterion:" + this.echo(criterion) + " with form: " +
            //    this.canvas);
            for (var i = 0; i < criterion.criteria; i++) {
                var items = this.canvas.getItems(),
                    foundItem;
                for (var ii = 0; ii < items.length; ii++) {
                    if (items[ii].canEditCriterion(criterion.criteria[i])) {
                        //this.logWarn("item:" + items[ii] + " can edit:" + this.echo(criterion.criteria[i]));
                        foundItem = true;
                        break;
                    }
                }
                //this.logWarn("found item:" + foundItem + " for criterion:" + this.echo(criterion.criteria[i]));
                if (!foundItem) return false;
            }
            return true;
        }
        return this.Super("canEditCriterion", arguments);
    },
    
    //> @attr canvasItem.editCriteriaInInnerForm (Boolean : true : IRA)
    // Flag to disable the criteria editing overrides described in
    // <smartclient>+link{getCriterion()}</smartclient>
    // <smartgwt><code>setCriterionGetter()</code></smartgwt> whereby
    // if this item contains a DynamicForm as its canvas, it will be used to edit nested
    // AdvancedCriteria automatically.
    // <P>
    // This flag is required for cases where a canvasItem contains a DynamicForm, but the form
    // is not set up to show inner field values of nested objects, and therefore should not
    // attempt to apply nested advanced criteria directly to the form.
    //
    // @visibility external
    //<
    // This is required for the RelativeDateItem. It's quite likely to be required for 
    // developer-defined canvasItems too. For simple items where the form just displays an atomic
    // field value in some custom way, setting this flag will be sufficient to get simple
    // operator-based advanced criteria editing working. If the inner form does something more
    // complex, developers would potentially need to override hasAdvancedCriteria, getCriterion, etc
    editCriteriaInInnerForm:true,

    //> @method canvasItem.getCriterion()
    // The standard formItem criteria editing APIs have been overridden in the canvasItem class
    // to simplify the editing of complex +link{AdvancedCriteria} objects using nested 
    // DynamicForms.
    // <P>
    // The following pattern is supported without need for further modification:<br>
    // A complex Advanced criteria object may have nested sub criteria using the <code>"and"</code>
    // or <code>"or"</code> operators. For example:
    // <pre>
    // { _constructor:"AdvancedCriteria",
    //   operator:"and",
    //   criteria:[
    //      {fieldName:"field1", value:"value1", operator:"iContains"},
    //      {operator:"or", criteria:[
    //          {fieldName:"innerField1", value:"value1", operator:"equals"},
    //          {fieldName:"innerField2", value:"value2", operator:"iContains"}
    //       ]
    //      }
    //   ]
    // }
    // </pre>
    // To create a form capable of editing the above criteria without providing custom overrides
    // to +link{formItem.getCriterion()} et al, you would create a form with 2 items.
    // The 'field1' criterion could be edited by a simple form item such as a TextItem.
    // The nested criteria ('innerField1' and 'innerField2') could be edited by a canvasItem
    // whose canvas property was set to a DynamicForm showing items capable of editing the 2
    // inner criteria, and whose operator was specified as "or".<br>
    // For example:
    // <pre>
    //  isc.DynamicForm.create({
    //      items:[
    //          {name:"field1", type:"TextItem"},
    //          {name:"nestedItem", shouldSaveValue:true, type:"CanvasItem",
    //              canvas:isc.DynamicForm.create({
    //                  operator:"or",
    //                  items:[
    //                      {name:"innerField1", type:"TextItem", operator:"equals"},
    //                      {name:"innerField2", type:"TextItem"}
    //                  ]
    //              })
    //          }
    //      ]
    //  });
    //  </pre>
    // This form would be able to edit the above advanced criteria object via
    // +link{dynamicForm.setValuesAsCriteria()}. Edited values would be retrieved via
    // +link{dynamicForm.getValuesAsCriteria()}.
    // <P>
    // Note that the canvas item has <code>shouldSaveValue</code> set to true - this is required
    // to ensure the nested form is actually passed the values to edit. 
    // <P>
    // The default implementation of this method checks for this.canvas being specified as a
    // dynamicForm, and in that case simply returns the result of 
    // +link{dynamicForm.getValuesAsAdvancedCriteria()} on the inner form.
    // <P>
    // Note that this functionality may be entirely bypassed by
    // setting +link{editCriteriaInInnerForm} to false. This flag is useful when defining a
    // dynamicForm based canvasItem which is not intended for editing nested data -- for example
    // if a standard atomic field value is being displayed in some custom way using a
    // DynamicForm embedded in the item.
    // 
    // @return (Criterion) criterion to merge with advanced criteria returned by 
    //  +link{dynamicForm.getValuesAsCriteria()}
    // @group criteriaEditing
    // @visibility external
    //<
    getCriterion : function () {
        if (this.editCriteriaInInnerForm && isc.isA.DynamicForm(this.canvas)) {
            return this.canvas.getValuesAsAdvancedCriteria();
        } else return this.Super("getCriterion", arguments);
    },
    
    //> @method canvasItem.setCriterion()
    // Display a +link{criterion} object in this item for editing. Overridden from 
    // +link{formItem.setCriterion()} in order to support editing nested criteria using 
    // nested dynamicForms as described in +link{canvasItem.getCriterion()}.
    // <P>
    // Implementation checks for this.canvas being specified as a DynamicForm, and applies
    // criterion directly to the embedded form via setValuesAsCriteria()
    // @param criterion (Criterion) criteria to edit
    // @group criteriaEditing
    // @visibility external
    //<
    setCriterion : function (criterion) {
        if (this.editCriteriaInInnerForm && isc.isA.DynamicForm(this.canvas)) {
            this.canvas.setValuesAsCriteria(criterion, true);
        } else return this.Super("setCriterion", arguments);
    },
    
    // we copy the prompt onto the Canvas by default - override setPrompt to do the same at runtime
    
    //> @attr canvasItem.applyPromptToCanvas (Boolean : true : IRW)
    // If +link{formItem.prompt} is specified for this item, should the prompt be applied to the
    // +link{canvasItem.canvas} for this item?
    // @visibility external
    //<
    applyPromptToCanvas:true,
    
    //> @attr canvasItem.prompt (string : null : IRW)
    // @include FormItem.prompt
    // @visibility external
    //<
    
    //>@method canvasItem.setPrompt()
    // Set the +link{formItem.prompt} for this item. Default implementation will also apply the
    // prompt to +link{canvasItem.canvas} if +link{canvasItem.applyPromptToCanvas} is true.
    // @param prompt (string) new prompt for the item.
    // @visibility external
    //<
    setPrompt : function (prompt) {
        this.Super("setPrompt", prompt);
        if (this.applyPromptToCanvas && this.canvas) {
            if (isc.isA.Canvas(this.canavs)) this.canvas.setPrompt(prompt);
            else this.canvas.prompt = prompt;
        }
    }
});

isc.CanvasItem.registerStringMethods({
    //> @method canvasItem.createCanvas ()
    // This method allows dynamic creation of a CanvasItem's canvas, rather than
    // setting +link{CanvasItem.canvas} statically.
    // If specified this +link{group:stringMethods,StringMethod} will be called when the form item is 
    // initialized and should return the Canvas to display for this item.
    // @param form (DynamicForm) the dynamicForm in which this item is contained
    // @param item (CanvasItem) the live form item instance
    // @return (Canvas) the canvas to be rendered inside this CanvasItem
    // @visibility external
    //<
    createCanvas:"form,item",
    
    
    //> @method canvasItem.showValue()
    // This method will be called whenever this FormItem's value is being set via a programmatic
    // call to e.g: +link{dynamicForm.setValues()} or +link{formItem.setValue()} and may be
    // overridden by CanvasItems intended to support displaying data values to update the
    // embedded Canvas to reflect the value passed in.
    // @param displayValue (any) new display value for the item.  This is the value after applying
    //   any custom formatter or valueMap
    // @param dataValue (any) underlying data value for the item
    // @param form (DynamicForm) the dynamicForm in which this item is contained
    // @param item (CanvasItem) the live form item instance
    // @visibility external
    //<
    showValue:"displayValue,dataValue,form,item"
});
