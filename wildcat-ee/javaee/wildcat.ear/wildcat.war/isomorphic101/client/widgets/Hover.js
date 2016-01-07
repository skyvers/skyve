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
//>	@class	Hover
// The Hover class handles showing a simple SmartClient canvas containing arbitrary HTML, or
// triggering some other action in response to a user holding the mouse-pointer (or hovering)
// over a specific widget.
//  @treeLocation Client Reference/Control
//  @visibility external
//<
 
// singleton that implements
//	-- hover timing
//	-- hover window display
//
// Hover.show() / Hover.hide() and some appearance management properties exposed to allow
// displaying / customizing content of hovers.
// Other Hover APIs exist at the widget level, enabling custom actions in response to hover 
// events.


isc.ClassFactory.defineClass("Hover");


isc.Hover.addClassProperties({
    // This delay is a default - may be overridden via the optional delay param passed to 
    // setAction()
    
	delay:500,
	//timer:null,
	
	//action:null,
	//actionArgs:null,
	//actionTarget:null,

	//isActive:false,
    
    //>@classAttr Hover.moveWithMouse (boolean : false : RWA)
    // When the Hover canvas is shown by default, should it move as the user moves the
    // mouse pointer?<br>
    // May be overridden by including a <code>moveWithMouse</code> attribute on the 
    // properties block passed to +link{Hover.show()}
    // @visibility external
    //<
	//moveWithMouse:false,
    
    //>@classAttr Hover.leftOffset (number : 15 : RW)
    // When positioning the hover canvas, this will be the default left offset from the 
    // mousepointer, if no explicit position was passed to the +link{Hover.show()} method
    // @visibility external
    //<
	leftOffset:15,

    //>@classAttr Hover.topOffset (number : 15 : RW)
    // When positioning the hover canvas, this will be the default top offset from the 
    // mousepointer, if no explicit position was passed to the +link{Hover.show()} method
    // @visibility external
    //<    
	topOffset:15,
	
	//>	@classAttr	canvas.hoverCanvas		(Canvas : null : RA)
	// Reference to the hoverCanvas currently visible.  Null if none.
	//<		   
    
    //hoverCanvas:null,

    //>	@classAttr	Hover.hoverCanvasDefaults   (object: {...} : IRW)
    // Defaults to apply to the Hover canvas shown when the user hovers over some widget.
    // By default this property is set to this object:<br><pre>
    //       { defaultWidth:100, 
    //         defaultHeight:1,
    //         baseStyle:"canvasHover",
    //         align:"left",
    //         valign:"top",
    //         opacity:null
    //        }
    // </pre><br>
    // Note that these properties can be overridden by individual widgets showing hovers, by
    // modifying +link{canvas.hoverWidth}, +link{canvas.hoverHeight}, 
    // +link{canvas.hoverStyle}, +link{canvas.hoverAlign}, +link{canvas.hoverVAlign}, 
    // +link{canvas.hoverOpacity}, and +link{canvas.hoverWrap}.
    // @visibility external
    //<
    hoverCanvasDefaults:{
        defaultWidth:100,
        defaultHeight:1,
        
		baseStyle:"canvasHover",
        align:isc.Canvas.LEFT,
		valign:isc.Canvas.TOP,
        wrap:true,
        
		autoDraw:false
        
	}										
});


isc.Hover.addClassMethods({

//>	@classMethod Hover.show()
// Displays a standard Hover canvas containing the specified HTML content.<br>
// This method may also be called to modify the content of the hover if it is already showing.
// Call +link{Hover.hide()} to hide the canvas again.<br>
// A common use case for calling this method is to asynchronously fetch detail data from the
// server about some component, and display it in the Hover canvas when the data is returned.
// Note that in this case you will typically need to verify that the user is still hovering 
// over the component in question before calling Hover.show() - if the user has moved the mouse 
// off the component, the information will not apply to whatever is now under the mouse. 
// Suggested approaches for handling this are to either use a +link{Canvas.mouseOut()} handler
// to track when the user moves off the component, or checking +link{EventHandler.getTarget()}
// as part of the asynchronous callback
// <p>
// The default Hover canvas position will be based on the mouse pointer position, adjusted by
// +link{Hover.leftOffset} and +link{Hover.topOffset}. If this position would render the
// Hover canvas partially clipped, it will be automatically modified to ensure the Hover 
// is entirely visible.
// @param contents (HTMLString | Canvas) contents for the hover
// @param properties (Label Properties) object containing attributes for managing the hover canvas' 
//  appearance. Valid properties include:<ul>
//  <li>left, top, width, height
//  <li>baseStyle
//  <li>opacity
//  <li>wrap
//  <li>moveWithMouse [overrides +link{Hover.moveWithMouse}]
// </ul>
//    
// @visibility external
//<
// @param rect (object) boundary rectangle along which the hoverCanvas should be drawn; if
//     left and top are specified in properties, this parameter is ignored
// @param [targetCanvas] (Canvas) Passed in by canvas.showHover() - allows us to track which canvas
//     showed the hover and handle cases such as that canvas being destroyed etc. 
show : function (contents, properties, rect, targetCanvas) {
    
    if (isc.isA.Canvas(contents)) {
        // we've been passed a Canvas as content for the hover - this will now become the 
        // hoverCanvas, rather than being the content for a newly created hoverCanvas
        this.showingHoverComponent = true;
        this.hoverCanvas = contents;
        this.hoverCanvas.hide = function () {
            this.Super("hide", arguments);
            isc.Hover.hoverCanvasHidden();
        };
        if (targetCanvas != null) {
            targetCanvas.hoverCanvas = contents;
        }
    }

	// position and show hoverCanvas with contents & properties
	if (!this.hoverCanvas) this._makeHoverCanvas();
	
	var hoverCanvas = this.hoverCanvas;

	// check parameters
	if (contents == null || contents == "") {
        hoverCanvas.hide();
		return;
	}

    if (this._hideOnMouseDownEvent) {
        isc.Page.clearEvent("mouseDown", this._hideOnMouseDownEvent);
    }
    if (targetCanvas && targetCanvas.hideHoverOnMouseDown) {
        this._hideOnMouseDownEvent = isc.Page.setEvent(
            "mouseDown", 
            this,
            "once",
            "hideHoverOnMouseDown"
        );
    }
    
    // remember which target showed the canvas
    // (Cleared on hoverCanvas.hide())
    this.lastHoverCanvas = targetCanvas;
    
    // set the hover to display the new contents
    if (!this.showingHoverComponent) hoverCanvas.setContents(contents);
	if (properties == null) properties = {};
	
    // Apply the properties to the hoverCanvas (except for positioning props)
    
    var defaults = this.hoverCanvasDefaults;
    if (hoverCanvas.setAlign) hoverCanvas.setAlign(properties.align || defaults.align);
    if (hoverCanvas.setVAlign) hoverCanvas.setVAlign(properties.valign || defaults.valign);
    if (hoverCanvas.setBaseStyle) hoverCanvas.setBaseStyle(properties.baseStyle || defaults.baseStyle);
    if (hoverCanvas.setOpacity) hoverCanvas.setOpacity(properties.opacity || defaults.opacity);
    if (hoverCanvas.setWrap) hoverCanvas.setWrap(properties.wrap != null ? properties.wrap : defaults.wrap);
    
    // Should we move the hover canvas around with the mouse
    if (properties.moveWithMouse != null) this._shouldMoveWithMouse = properties.moveWithMouse
    else this._shouldMoveWithMouse = this.moveWithMouse;
    
	// set properties of new hoverCanvas.
	// placement: by default, offset from mouse (no occlusion by mouse), and on-screen (if
    //            possible).  Can be modified by the caller with attributes of the properties
    //            parameter.
	// note that all properties set here (aside from left/top) must be set back to defaults in
    // Hover.hide()
	var lastX = isc.EH.getX(),
        lastY = isc.EH.getY(),
        left = properties.left,
        top = properties.top,
        // NOTE: boolean check OK because width and height can't validly be zero
		width = properties.width || (this.showingHoverComponent ? hoverCanvas.width : defaults.defaultWidth),
		height = properties.height || (this.showingHoverComponent ? hoverCanvas.height : defaults.defaultHeight);

    // If either left or top is specified in the arguments to Hover.show(), respect them and don't
    // use Canvas._placeRect to adjust the position of the hover
    if (left != null || top != null) {
        // default left and top if they weren't specified in the properties argument
        left = left ? left : lastX + this.leftOffset;
        top = top ? top : lastY + this.topOffset;
    } else {
        
        //this.logWarn("sizing hover to: " + [width, height]);
        hoverCanvas.setRect(null, -9999, width, height);
        if (!hoverCanvas.isDrawn()) hoverCanvas.draw();
        // Has to be visible as when we hide a shadow we shift it so it sits UNDER the widget.
        if (!hoverCanvas.isVisible()) hoverCanvas.show();
        else hoverCanvas.redrawIfDirty("placing hover");
        // Use '_placeRect' to position the hover next to a boundary rectangle with the mouse 
        // pointer as its center, a width of 2 * this.leftOffset and a height of 2* 
        // this.topOffset
        var avoidRect = rect ? rect : [lastX - this.leftOffset, lastY - this.topOffset,
                                       2 * this.leftOffset, 2 * this.topOffset];
        // call getPeerRect() to take into account dropShadow.  NOTE: technically if the hover
        // had peers to the left/top expanding the peer rect, we would need to place the hover
        // itself to the right/bottom of the position returned by placeRect()
        var hoverRect = hoverCanvas.getPeerRect();
        var pos = isc.Canvas._placeRect(hoverRect[2], hoverRect[3], avoidRect, 
            "bottom", false, "outside-right"
        );
        left = pos[0];
        top = pos[1];
    }
	hoverCanvas.setRect(left, top, width, height);
	hoverCanvas.bringToFront();	
	
	// show the hoverCanvas
	if (!hoverCanvas.isDrawn() || !hoverCanvas.isVisible()) hoverCanvas.show();
	
	// set a page-level mouseMove handler to move the hoverCanvas
	if (this._shouldMoveWithMouse) {
		this._mouseMoveHandler = 
            isc.Page.setEvent("mouseMove", function () { isc.Hover._moveWithMouse() });
	}
	
	return;
},
    
hideHoverOnMouseDown : function () {
    delete this._hideOnMouseDownEvent;
    this.hide();
},

// notification fired from the hover canvas on hide
hoverCanvasHidden : function () {
    var lhc = this.lastHoverCanvas;
    delete this.lastHoverCanvas;
    if (lhc != null) {
        // call an internal method so we can auto-destroy hover components with 
        // hoverAutoDestroy: true before calling the generic notification method
        lhc._hoverHidden();
    }
    if (this._hideOnMouseDownEvent) {
        isc.Page.clearEvent("mouseDown", this._hideOnMouseDownEvent);
        delete this._hideOnMouseDownEvent;
    }
},


//> @classMethod Hover.hide()
// Hide hover hover Canvas shown via +link{Hover.show()}
// @visibility external
//<
hide : function () {
	var hoverCanvas = isc.Hover.hoverCanvas;
	if (hoverCanvas != null) {

		// clear the page-level mouseMove handler that moves the hoverCanvas
		if (this._mouseMoveHandler) {
			isc.Page.clearEvent("mouseMove", this._mouseMoveHandler);
            delete this._mouseMoveHandler;
		}

		// hide the hoverCanvas - if the canvas was flagged with hoverAutoDestroy: true, it
        // is destroyed by the owning canvas at this point
		hoverCanvas.hide();

        if (this.showingHoverComponent) {
            if (!hoverCanvas) return;
            delete this.hoverCanvas;
            this.showingHoverComponent = false;
        } else {

            // move the hover offscreen to prevent page-level scrollbars if the hover extends out
            // of the page.
            var defaults = this.hoverCanvasDefaults;
            hoverCanvas.setRect(0, -1000);
        }
	}
},


_makeHoverCanvas : function () {
    
    var defaults = isc.addProperties({
        hide : function () {
            this.Super("hide", arguments);
            isc.Hover.hoverCanvasHidden();
        }
    }, this.hoverCanvasDefaults);

    this.hoverCanvas = isc.Label.create(defaults);

},

_moveWithMouse : function () {    
    // call getPeerRect to take into account dropShadow
    var hoverRect = this.hoverCanvas.getPeerRect();
    var pos = isc.Canvas._placeRect(
        hoverRect[2], hoverRect[3],
        this.getMousePointerRect(), "bottom", false, "outside-right"
    );
    this.hoverCanvas.moveTo(pos[0], pos[1]);
},

// Return a rectangle suitable for use as the bounding rectangle along which the hover should
// be drawn.
//
// Center point of the rectangle is the pointer location, and it extends in each direction a
// length equal to the offset along the axis (so the height = 2 * topOffset, 
// width = 2 * leftOffset)
getMousePointerRect : function () {
    return [
        isc.EH.getX() - this.leftOffset,
        isc.EH.getY() - this.topOffset,
        2 * this.leftOffset,
        2 * this.topOffset
    ];
},

//>	@classMethod Hover.setAction()
//		sets the action to be executed by the hover window
//
//		@param target (object) object to which action belongs (defaults to Hover).
//		@param action (method) action to be executed when timer has elapsed.
//		@param actionArgs (array) arguments for action method.
//      @param [delay] (number) optional ms delay to apply to the hover action
//<
setAction : function (target, action, actionArgs, delay) {
    if (delay == null) delay = this.delay;
    // if already active or no delay, apply action immediately
	if (this.isActive || delay == 0) {
        // see note below about IE JS errors with empty args
		action.apply((target ? target : this), actionArgs ? actionArgs : []);
		this.isActive = true;
	}
	// otherwise set up a delayed action
	else {
		if (this.timer != null) this.timer = isc.Timer.clear(this.timer);
		this.actionTarget = (target ? target : this);	
		this.action = action;
        
		this.actionArgs = actionArgs ? actionArgs : [];
		// maybe check actionTime in _doAction()
		// this.actionTime = timeStamp() + this.delay;
		this.timer = isc.Timer.setTimeout({target:isc.Hover, methodName:"_doAction"}, delay);
	}
},


_doAction : function () {
	if (this.action && !this.actionTarget.destroyed) {
        this.action.apply(this.actionTarget, this.actionArgs);
    }
    this.actionTarget = this.action = this.actionArgs = null;
	this.isActive = true;
},

//> @classMethod Hover.clear()
// If the hover canvas is currently showing, hides it via +link{Hover.hide()}
// If a hover action was set up via +link{Hover.setAction()}, clear this pending action now.
//<
clear : function () {
	this.hide();
	if (this.timer != null) this.timer = isc.Timer.clear(this.timer);
	this.actionTarget = this.action = this.actionArgs = null;
	this.isActive = false;
}


});
