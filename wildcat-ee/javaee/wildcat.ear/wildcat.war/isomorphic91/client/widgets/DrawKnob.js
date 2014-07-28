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
    
    //> @attr drawKnob.knobDefaults (DrawOval Properties : {...} : IRA)
    // Default properties for this component's +link{drawKnob.knobShape}. Has the
    // following properties by default:
    // <pre>
    //  radius : 5,
    //  lineWidth:2,
    //  fillColor:"#FF0000",
    //  fillOpacity:0.5,
    // </pre>
    // As with any auto-child defaults block, use +link{Class.changeDefaults()} to modify this
    // object.
    // @visibility drawing
    //<
    knobShapeDefaults : {
        _constructor:isc.DrawOval,
        // note that this is just the size of the visible shape - the size of the
        // draggable handle is governed by drawKnob.width / height
        radius : 5,
        lineWidth:2,
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

        this.left = this.x - this.width/2;
        this.top = this.y - this.height/2;

        // create the shape that serves as a visible representation of the knob
        this.knobShape = this.createAutoChild("knobShape", {
            drawPane: this.drawPane,
            centerPoint: [this.x, this.y]
        });

        // add to drawPane as a CanvasItem
        this._drawCallingAddCanvasItem = true;
        this.drawPane.addCanvasItem(this);
        delete this._drawCallingAddCanvasItem;
    },

    //> @method DrawKnob.setCenterPoint()
    // Sets the center point of the drawKnob. If the additional <code>drawingCoords</code> attribute
    // is passed, coordinates are expected to be already adjusted for drawPane pan and zoom.
    // Otherwise coordinates are expected to be absolute pixel coordinates within the drawPane.
    // @param x (integer) new x coordinate for this drawKnob
    // @param y (integer) new y coordinate for this drawKnob
    // @param [drawingCoords] (boolean) If specified, <code>x</code> and <code>y</code> values are
    //  expected to be drawing coordinates - already adjusted for any zoom or pan applied to the
    //  drawPane.
    // @visibility drawing
    //< 
    setCenterPoint : function (x, y, drawingCoords) {
        var screenLeft, screenTop;
        if (drawingCoords) {
            var screenCoords = this.drawPane.drawing2screen([x - this._drawingWidth/2,
                                                             y - this._drawingHeight/2, 0,0]);
            screenLeft = screenCoords[0],
            screenTop = screenCoords[1];
        } else {
            screenLeft = x - this.width/2;
            screenTop = y - this.height/2;
        }
        this.moveTo(screenLeft << 0, screenTop << 0);
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
        var drawnByAddCanvasItem = (this._drawCallingAddCanvasItem);
        
        if (!drawnByAddCanvasItem) return this.Super("draw", arguments);
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
        var x = isc.EH.getX(),
            y = isc.EH.getY(),
            screenX = x -
                (this.drawPane.getPageLeft() + this.drawPane.getLeftMargin() +
                 this.drawPane.getLeftBorderSize()),
            screenY = y -
                (this.drawPane.getPageTop() + this.drawPane.getTopMargin() + 
                 this.drawPane.getTopBorderSize());                 
        var drawingCoords = this.drawPane.screen2drawing(
        [
            screenX, screenY,
            0, 0 // not using width & height
        ]);
        
        var left = drawingCoords[0],
            top = drawingCoords[1],
            dX = left - (this._drawingLeft + this._drawingWidth/2),
            dY = top - (this._drawingTop + this._drawingHeight/2);

        // call the observable / overrideable updatePoints() method
        this.updatePoints(left, top, dX, dY, state);

        // Note: we rely on the updatePoints implementation to actually move the DrawKnob to the
        // appropriate position.
        // We *could* center ourselves over the mouse pointer here but this would not allow
        // things like restricting dragging per axis, so let the item actually shift us to the
        // right new position.
    },
    
    // updateKnobShape() - reposition the knobShape under the canvas - fired in response to 
    // canvas.moved()
    updateKnobShape : function () {

        var drawingCoords = this.drawPane.screen2drawing([
            this.getLeft() + this.getWidth()/2,
            this.getTop() + this.getHeight()/2,
            0, 0 // not using width & height
        ]);
        
        var x = drawingCoords[0],
            y = drawingCoords[1],
            newDrawingLeft =  x - this._drawingWidth/2,
            newDrawingTop =  y - this._drawingHeight/2;
            
        // remember to update cached drawing coords! (correcting center to topleft)
        this._drawingLeft = newDrawingLeft,
        this._drawingTop = newDrawingTop;
        
        // update our knobShape position
        this.knobShape.setCenterPoint(x,y);
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
    // We should destroy this.knobShape on destroy() but don't yet have a destroy method on
    // drawItems

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
