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
/*-=-
    EXPERIMENTAL FUNCTIONALITY
    This functionality is provided for early evaluation and feedback purposes only. It is not fully
    tested, frozen, or supported by Isomorphic Software. Isomorphic may modify or remove this
    functionality without notice.
*/


//------------------------------------------------------------
//> @class DrawKnob
// Canvas that renders a +link{drawItem} into a +link{DrawPane} and provides interactivity for
// that drawItem, including drag and drop.
// <P>
// A DrawKnob can either be initialized with a +link{drawKnob.knobShape,DrawItem knobShape} or created via
// the +link{type:AutoChild} pattern.
// <P>
// DrawKnobs are used by the +link{drawItem.knobs,drawItem control knobs} subsystem.
// 
// @inheritsFrom Canvas
// @treeLocation Client Reference/Drawing
// @visibility drawing
//<

isc.defineClass("DrawKnob", "Canvas").addProperties({
    width:10, height:10, 
    overflow:"hidden",
    //border:"1px solid black",

    cursor:"crosshair",

    canDrag:true,
    dragAppearance:"none",

    keepInParentRect:true,
    autoDraw:false, // typically addChild to drawPane

    //> @attr drawKnob.knobShape (AutoChild DrawItem : null : R)
    // The +link{DrawItem} instance rendered into this DrawKnob's drawPane
    // @visibility drawing
    //<

    //> @attr drawKnob.knobShapeDefaults (DrawItem Properties : {...} : IRA)
    // Default properties for this component's +link{drawKnob.knobShape}. Has the
    // following properties by default:
    // <pre>
    //  radius : 5,
    //  lineWidth:2,
    //  fillColor:"#FF0000",
    //  fillOpacity:0.5,
    // </pre>
    // <smartclient>As with any auto-child defaults block, use +link{Class.changeDefaults()}
    // to modify this object.</smartclient>
    // @visibility drawing
    //<
    knobShapeDefaults: {
        _constructor:isc.DrawOval,
        // note that this is just the size of the visible shape - the size of the
        // draggable handle is governed by drawKnob.width / height
        radius:4.5,
        lineColor:"#333333",
        lineWidth:1,
        lineOpacity:1,
        fillColor:"#FF0000",
        fillOpacity:0.5,
        autoDraw:true,

        // suppress updateControlKnobs behaivor since we don't expect controlKnobs ON controlKnobs!
        updateControlKnobs : function () {
            return;
        },
        // Override erase to wipe the DrawKnob canvas out as well
        erase : function () {
            // erase() on the draw shape calls clear on the DrawKnob (canvas)
            // Avoid recursion via an "erasing" flag
            if (this.erasing) return;
            
            this.erasing = true;
            if (this.creator.isDrawn()) this.creator.clear();
            this.Super("erase", arguments);
            delete this.erasing;
        }
    },

    // Public Attributes:

    //> @attr DrawKnob.drawPane (DrawPane : null : IR)
    // +link{DrawPane} into which this DrawKnob's +link{drawKnob.knobShape} will be rendered.
    // @visibility drawing
    //<

    //> @attr DrawKnob.x (integer : null : IR)
    // X-Coordinate for this DrawKnob. DrawKnob will initially be drawn centered over this
    // coordinate
    // @visibility drawing
    //<

    //> @attr DrawKnob.y (integer : null : IR)
    // Y-Coordinate for this DrawKnob. DrawKnob will initially be drawn centered over this
    // coordinate
    // @visibility drawing
    //<

    initWidget : function () {
        
        if (isc.Browser.isIE && isc.Browser.version < 11 && this.drawPane.drawingType == "bitmap") {
            this.setBackgroundColor("White");
            this.setOpacity(0);
        }

        // Ensure that `x` and `y` are integers.
        var initialX = this.x,
            initialY = this.y,
            x = this.x = Math.round(initialX),
            y = this.y = Math.round(initialY);

        // Center the knob on `(x,y)`.
        var left = x - this.width / 2 + this.drawPane.getLeftPadding(),
            top = y - this.height / 2 + this.drawPane.getTopPadding();
        this.left = (x <= initialX ? Math.ceil(left) : Math.floor(left));
        this.top = (y <= initialY ? Math.ceil(top) : Math.floor(top));

        if (!isc.isA.DrawPane(this.drawPane)) {
            // A `drawPane` is required for creating a DrawKnob because properties of the
            // `drawPane` will be immediately accessed for the positioning of the `knobShape`.
            // If there is no `drawPane` then this knob will be unusable.  Log a warning in
            // this case.
            this.logWarn(
                "DrawPane provided in DrawKnob.drawPane is invalid so this knob cannot be " +
                "constructed.");
            return;
        }

        // create the shape that serves as a visible representation of the knob
        var knobShape = this.knobShape = this.createAutoChild("knobShape", {
            drawPane: this.drawPane,
            knobs: null,
            title: null,
            zIndex: this.creator.getZIndex(true),
            _sameZIndexAsMaster: true,

            exemptFromGlobalTransform: true,
            _globalTransformChanged : function () {
                // If the global transform changed then just recalculate the center point of
                // the knob.  The DrawKnob does not know how its center point `(x,y)` was
                // calculated so the user of the DrawKnob must calculate a new center point
                // and call `setCenterPoint()`.
                this.creator.resetKnobPosition();
            }
        });
        if (isc.isA.DrawItem(this.creator)) {
            this.creator.addPeer(knobShape);
        }
        knobShape.setCenterPoint(this.x, this.y);

        // Add to the `drawPane` as a child canvas.
        this.drawPane.addChild(this);
    },

    resetKnobPosition : function () {
        this.setCenterPoint(this.x, this.y, false);
    },

    //> @method DrawKnob.setCenterPoint()
    // Sets the center point of the drawKnob.  If the optional <code>viewboxCoords</code>
    // argument is passed, coordinates are expected to be adjusted for drawPane pan
    // and zoom.  Otherwise coordinates are expected to be absolute pixel coordinates within
    // the drawPane.
    // @param x (integer) new x coordinate for this drawKnob
    // @param y (integer) new y coordinate for this drawKnob
    // @param [viewboxCoords] (boolean) If <code>true</code>, the <code>x</code> and
    // <code>y</code> values are expected to be in the viewbox coordinate system (described
    // +link{class:DrawPane,here}) - already adjusted for any zoom or pan applied to the
    // drawPane.
    // @visibility drawing
    //<
    setCenterPoint : function (x, y, viewboxCoords) {
        var leftPadding = this.drawPane.getLeftPadding(),
            topPadding = this.drawPane.getTopPadding();
        if (viewboxCoords) {
            var screenCoords = this.drawPane.viewbox2global([x, y, 0, 0]);
            x = screenCoords[0] - leftPadding;
            y = screenCoords[1] - topPadding;
        }

        // Ensure that `x` and `y` are integers.
        var initialX = x, initialY = y;
        x = Math.round(x);
        y = Math.round(y);

        // Center the knob on `(x,y)`.
        var left = x - this.width / 2 + leftPadding,
            top = y - this.height / 2 + topPadding;
        left = (x <= initialX ? Math.ceil(left) : Math.floor(left)),
        top = (y <= initialY ? Math.ceil(top) : Math.floor(top));
        this._initialX = initialX;
        this._initialY = initialY;
        this.moveTo(left, top);
        delete this._initialX;
        delete this._initialY;
    },

    setRect : function (left, top, width, height, animating) {
        if (isc.isAn.Array(left)) {
            top = left[1];
            width = left[2];
            height = left[3];
            left = left[0];
        } else if (isc.isAn.Object(left)) {
            top = left.top;
            width = left.width;
            height = left.height;
            left = left.left;
        }

        var keepInParentRect = this.keepInParentRect;
        if (keepInParentRect) {
            var drawPaneWidth = this.drawPane.getWidth(),
                drawPaneHeight = this.drawPane.getHeight(),
                right = Math.min(left + width, drawPaneWidth),
                bottom = Math.min(top + height, drawPaneHeight);
            left = Math.max(left, 0);
            top = Math.max(top, 0);
            width = right - left;
            height = bottom - top;

            if (width < 1 || height < 1) {
                this.hide();
                return false;
            } else {
                this.show();
            }
        }

        return this.Super("setRect", [left, top, width, height, animating]);
    },

    draw : function () {
        this.drawPane.addDrawItem(this.knobShape);
        return this.Super("draw", arguments);
    },
    
    // when we're programmatically moved, update our knob shape position to match.
    // Exception: When a canvasItem is added to a drawPane, zoom / pan of the drawPane will
    // call moveTo() on the canvasItem. In this case our shape has already been repositioned.
    moved : function () {
        if (!this.synchingToPane) this.updateKnobShape();
    },

    // _updatePoints - move the knobShape directly, then fire the 'updatePoints' notification
    // that a shape such as a DrawRect can observe to change it's dimensions.
    // The state argument is either "start", "move", or "stop".
    _updatePoints : function (state) {
        var globalPoint = this.drawPane._pageToGlobal(isc.EH.getX(), isc.EH.getY()),
            left = globalPoint[0],
            top = globalPoint[1],
            dX = left - this.x,
            dY = top - this.y;

        // Call the observable / overrideable updatePoints() method.
        var updatePointsRetVal = this.updatePoints(left, top, dX, dY, state);
        return (updatePointsRetVal !== false);

        // Note: we rely on the updatePoints implementation to actually move the DrawKnob to the
        // appropriate position.
        // We *could* center ourselves over the mouse pointer here but this would not allow
        // things like restricting dragging per axis, so let the item actually shift us to the
        // right new position.
    },

    // updateKnobShape() - reposition the knobShape under the canvas - fired in response to 
    // canvas.moved()
    updateKnobShape : function () {
        var x = 0, y = 0;
        if (this._initialX != null && this._initialY != null) {
            x = this._initialX;
            y = this._initialY;
        } else {
            x = this.getLeft() + this.getWidth() / 2 - this.drawPane.getLeftPadding();
            y = this.getTop() + this.getHeight() / 2 - this.drawPane.getTopPadding();
        }
        this.x = Math.round(x);
        this.y = Math.round(y);

        x += this.drawPane.scrollLeft;
        y += this.drawPane.scrollTop;

        // Update our knobShape position.
        this.knobShape.setCenterPoint(isc.DrawItem._makeCoordinate(x),
                                      isc.DrawItem._makeCoordinate(y));
    },

    //> @method drawKnob.updatePoints()
    // Method called in response to the user dragging this DrawKnob. May be observed or overridden
    // to allow drawItems to react to user drag interactions on this knob.
    // <P>
    // Note that the default implementation does nothing. When working with draw knobs directly this
    // is typically where you would both update the shape being controlled by the draw knob, and
    // ensure the drawKnob gets repositioned. You may also need to update the drawKnob
    // position in response to the drawItem being repositioned, resized, etc.
    // 
    // @param x (integer) new x-coordinate of the drawKnob
    // @param y (integer) new y-coordinate of the drawKnob
    // @param dX (integer) horizontal distance moved
    // @param dY (integer) vertical distance moved
    // @param state (String) either "start", "move", or "stop", to indicate the current phase
    // of dragging of the DrawKnob for which the points need to be updated
    // @visibility drawing
    //<
    updatePoints: function (x, y, dX, dY, state) {
//!DONTOBFUSCATE  We need to observe complete with the intact var names
    },

    // On clear, wipe out the knobShape
    clear : function () {
        this.Super("clear", arguments);
        this.knobShape.erase();
    },

    dragStart : function () {
        return this._updatePoints("start");
    },
    dragMove : function () {
        return this._updatePoints("move");
    },
    dragStop : function () {
        return this._updatePoints("stop");
    }
});

// register updatePoints as a stringMethod
isc.DrawKnob.registerStringMethods({
    updatePoints:"x,y,dX,dY"
});
