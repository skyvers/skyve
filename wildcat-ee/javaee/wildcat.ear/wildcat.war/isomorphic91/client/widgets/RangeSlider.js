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


//> @class RangeSlider
// A "double slider" allowing the user to select a range via two draggable thumbs.
//
//@treeLocation Client Reference/Control
//
// @visibility external
//<

//> @attr rangeSlider.startThumb (AutoChild Snapbar : null : IR)
// Thumb for the start of the range.
//
// @visibility external
//<

//> @attr rangeSlider.endThumb (AutoChild Snapbar : null : IR)
// Thumb for the end of the range
//
// @visibility external
//<

//> @method rangeSlider.changed()
// Notification fired when the selected range is changed by the end user.
//
// @param startValue (float) new start value
// @param endValue (float) new end value
// @param isDragging (boolean) whether the user is still in the middle of a drag, so that
//  expensive operations can be avoided if needed
//
// @visibility external
//<

//> @attr rangeSlider.track (AutoChild Canvas : null : IR)
// Optional track of the RangeSlider.  Set <code>showTrack</code> false to avoid showing
// a track so the RangeSlider can be superimposed over something else.
//
// @visibility external
//<

//> @attr rangeSlider.scrollbar (AutoChild Scrollbar : null : IR)
// Optional Scrollbar shown as a second way of adjusting the range.
//
// @visibility external
//<

isc.defineClass("RangeSlider", isc.Canvas);

isc.RangeSlider.addClassProperties({
    
    _epsilon: 1e-6
});

isc.RangeSlider.addProperties ({

//> @attr rangeSlider.vertical (boolean : false : IR)
// Whether the rangeSlider should be vertical or horizontal.  Default is horizontal.
//
// @visibility external
//<
    vertical: false,

//> @attr rangeSlider.minValue (float : 0 : IRW)
// Set the minimum value (left/top of slider).
//
// @visibility external
//<
    minValue: 0,

//> @attr rangeSlider.maxValue (float : 0 : IRW)
// Set the maximum value (right/bottom of slider).
//
// @visibility external
//<
    maxValue: 0,

//> @attr rangeSlider.startValue (float : 0 : IRW)
// The beginning of the selected range.
//
// @visibility external
//<
    startValue: 0,

//> @attr rangeSlider.endValue (float : 0 : IRW)
// The end of the selected range.
//
// @visibility external
//<
    endValue:0,
    
 // @attr rangeSlider.baseStyle (CSSStyleName : "rangeSlider" : IR)
 // Base style name for CSS styles applied to the background of the rangeSlider.  The following
 // suffixes are applied for different areas of the slider:
 // <ul>
 // <li> "Start": area of the slider before the startThumb
 // <li> "Selected": area of the slider between the thumbs (the selected range)
 // <li> "End": area of the slider after the endThumb
 // </ul>
 // .. and the following suffixes are applied in addition according to the slider's dynamic state:
 // <ul>
 // <li> "Over": if the mouse is over the segment
 // <li> "Down": if the mouse is down on the segment
 // </ul>
 // For example, if the mouse is down in the area before the start thumb, that area will have the
 // CSS style "rangeSliderStartDown".
 //
 // @visibility external
 //<
    baseStyle:"rangeSlider",
    
    overflow:"hidden",
    thumbSize:"7px",
    
    labelStartDefaults: {
    	_constructor:isc.Label,
    	wrap:false,
    	overflow:"hidden"
    },

    startThumbDefaults: {
    	_constructor:isc.Snapbar, 
        wrap:false,  
        overflow:"hidden", 
        canDrag:true,
        keepInParentRect: true,
        canCollapse:false,
        showGrip:true,
        showClosedGrip:false,
        _canDragWhenTargetIsHidden:true,

        dragStart : function() {
            this.creator.oldStartValue = this.creator.startValue;
            this.creator.dragpoint = this.creator.vertical?isc.Event.mouseDownEvent.y:isc.Event.mouseDownEvent.x;

            this.creator.isDragging = true;

            this.creator.fireChangedEvent();
        },
        
        dragMove : function() {
        	// get pixel range and convert it to value range
            var val = this.creator.vertical?this.creator.getValuesForPixels(isc.Event.lastEvent.y-this.creator.dragpoint):
            	this.creator.getValuesForPixels(isc.Event.lastEvent.x-this.creator.dragpoint);

            this.creator.setStartValue(this.creator.oldStartValue+val);
            
            this.creator.fireChangedEvent();
            
            return true;
        },

        dragStop : function() {
            this.creator.isDragging = false;
            this.creator.fireChangedEvent();
        }
     },

    labelDragDefaults: {
        _constructor:isc.Label,
        overflow:"hidden",
        canDrag:true,
        keepInParentRect: true,
        dragAppearance:"none",

        dragMove : function() {
        	
        	// get pixel range and convert it to value range
            var val = this.creator.vertical?this.creator.getValuesForPixels(isc.Event.lastEvent.y-this.creator.dragpoint):
            	this.creator.getValuesForPixels(isc.Event.lastEvent.x-this.creator.dragpoint);

            this.creator.setValues(this.creator.oldStartValue+val, this.creator.oldEndValue+val);
            
            this.creator.fireChangedEvent();

            return true;
        },

        dragStart : function() {
        	this.creator.oldStartValue = this.creator.startValue;
            this.creator.oldEndValue = this.creator.endValue;

            this.creator.dragpoint = this.creator.vertical?isc.Event.mouseDownEvent.y:isc.Event.mouseDownEvent.x;

            this.creator.isDragging = true;

            this.creator.fireChangedEvent();
        },

        dragStop : function() {
            this.creator.isDragging = false;

            this.creator.fireChangedEvent();
        }
    },

    labelEndDefaults: {
        _constructor:isc.Label,
        overflow:"hidden"
    },

    endThumbDefaults: {
        _constructor:isc.Snapbar, 
        canDrag:true,
        overflow:"hidden", 
        keepInParentRect: true,
        canCollapse:false,
        showGrip:true,
        _canDragWhenTargetIsHidden:true,

        dragStart : function() {
            this.creator.oldEndValue = this.creator.endValue;
            this.creator.dragpoint = this.creator.vertical?isc.Event.mouseDownEvent.y:isc.Event.mouseDownEvent.x;

            this.creator.isDragging = true;
            this.creator.fireChangedEvent();
        },

        dragMove : function() {
        	// get pixel range and convert it to value range
            var val = this.creator.vertical?this.creator.getValuesForPixels(isc.Event.lastEvent.y-this.creator.dragpoint):
            	this.creator.getValuesForPixels(isc.Event.lastEvent.x-this.creator.dragpoint);

            this.creator.setEndValue(this.creator.oldEndValue+val);

            if (this.creator.scrollbar) {
                this.creator.scrollbar.moveThumb();
            }

            this.creator.fireChangedEvent();
            return true;
        },

        
        dragStop : function() {
            this.creator.isDragging = false;

            this.creator.fireChangedEvent();
        }
     },


    scrollbarDefaults: {
        thumbDragStop : function() {
            this.Super("thumbDragStop",	arguments);
            this.creator.thumbdragging = false;
            this.creator.isDragging = false;
           
            this.creator.updatePositions();
            this.creator.fireChangedEvent();
        },

        thumbDragStart : function() {
            this.Super("thumbDragStart",	arguments);
            this.creator.thumbdragging = true;
            this.creator.isDragging = true;
            
            this.creator.oldStartValue = this.creator.startValue;
            this.creator.oldEndValue = this.creator.endValue;

            this.creator.dragpoint = this.getEventCoord();
            this.creator.fireChangedEvent();
        }
    }
});

isc.RangeSlider.addMethods ({
	
    initWidget : function() {
        this.Super("initWidget", arguments);
        
        // value sanity checks
        if (this.maxValue < this.minValue) { 
            var x = this.minValue;
            this.minValue = this.maxValue;
            this.maxValue = x;
        }

        if (this.startValue < this.minValue) {
            this.startValue = this.minValue;
        }

        if (this.endValue > this.maxValue) {
            this.endValue = this.endValue;
        }
        
        // lazily initialize track defaults
        if (!this.trackDefaults) {
        	isc.RangeSlider.setInstanceProperty("trackDefaults", this.getTrackDefaults());
        }
        	
        // create the controls either vertically or horizontally
        if (this.vertical) {
            this.createControls(true);
        }
        else {
            this.createControls();
        }
    },
    
    // return the default properties for the track.
    getTrackDefaults : function() {
    	
    	return {
    		overflow:"hidden", 
	        showDisabled:true,
	        cacheImageSizes: false,
	    	_constructor:isc.Slider.getInstanceProperty("trackConstructor"),
	    	capSize:isc.Slider.getInstanceProperty("trackCapSize"),
	    	skinImgDir:isc.Slider.getInstanceProperty("skinImgDir"),
	    	imageType:isc.Slider.getInstanceProperty("trackImageType"),
	    	trackSrc : isc.Slider.getInstanceProperty("trackSrc")
    	};    
    },

    // overwrite resized so track size and thumb positions are updated on resize
    resized : function() {
        this.Super("resized", arguments);

        if (this.showTrack) {
            if (this.vertical) {
                this.track.setWidth(isc.Slider.getInstanceProperty("trackWidth"));
                this.track.setHeight(this.height);
            } else {
                this.track.setHeight(isc.Slider.getInstanceProperty("trackWidth"));
                this.track.setWidth(this.width);
            }
        }

    	this.updatePositions();
    },

    // create the child controls and set up the callback functions
    createControls : function(vertical) {
        var that =  this;
        var remaining;
        var trackWidth = isc.Slider.getInstanceProperty("trackWidth");

        if (vertical) {
            this.scrollbar = this.addAutoChild("scrollbar", { 
                _constructor:this.scrollbarConstructor, 
                vertical:true, 
                height:"100%"
            });

            if (this.scrollbar) {
                remaining = this.getWidth() - this.scrollbar.getWidth();
                this.scrollbar.setLeft(remaining);
                this.scrollbar.setTop(0);
            } else {
                remaining = this.getWidth()
            }
            
            this.labelStart = this.addAutoChild("labelStart", {
                width:remaining,
                baseStyle:this.baseStyle+"Start"
            });

            this.labelDrag = this.addAutoChild("labelDrag", { 
                width:remaining,
                baseStyle:this.baseStyle+"Selected"
            });

            this.labelEnd = this.addAutoChild("labelEnd", { 
                width:remaining,
                baseStyle:that.baseStyle+"End"
            });

            this.startThumb = this.addAutoChild("startThumb", { 
                height:this.thumbSize, 
                width:remaining,
                target:this.labelStart,
                vertical:false
            });

            this.endThumb = this.addAutoChild("endThumb", { 
                height:this.thumbSize, 
                width:remaining,
                target:this.labelEnd,
                vertical:false,
                
                makeLabel:function() {
                	this.Super("makeLabel", arguments);
                    this.label.addMethods({
                    	getCustomState : function () {
                    		// don't show closed state if showClosedGrip is set to false
                    		if (isc.Snapbar.getInstanceProperty("showClosedGrip")) {
                    			return "closed";
                    		}
                    	}
                    });
                }
            });

            this.track = this.addAutoChild("track", {
            	left:Math.round(remaining/2-trackWidth/2),
            	width:trackWidth, 
            	height:this.height,
            	vertical:this.vertical,
            	
        		src:"[SKIN]"+(this.vertical? "v" : "h")+isc.Slider.getInstanceProperty("trackSrc"),
                styleName:isc.Slider.getInstanceProperty((this.vertical ? "v" : "h") + "TrackStyle")
        	});
        } else {
            this.scrollbar = this.addAutoChild("scrollbar", { 
                _constructor:isc.Scrollbar, 
                vertical:false, 
                width:"100%"
            });

            if (this.scrollbar) {
                remaining = this.getHeight() - this.scrollbar.getHeight();
                this.scrollbar.setLeft(0);
                this.scrollbar.setTop(remaining);
            } else {
                remaining = this.getHeight();
            }

            this.labelStart = this.addAutoChild("labelStart", {
                height:remaining,
                baseStyle:this.baseStyle+"Start"
            });

            this.labelDrag = this.addAutoChild("labelDrag", { 
                height:remaining,
                baseStyle:this.baseStyle+"Selected"
            });

            this.labelEnd = this.addAutoChild("labelEnd", { 
                height:remaining,
                baseStyle:this.baseStyle+"End"
            });

            this.startThumb = this.addAutoChild("startThumb", { 
                width:this.thumbSize, 
                height:remaining,
                target:this.labelStart
            });

            this.endThumb = this.addAutoChild("endThumb", { 
                width:this.thumbSize, 
                height:remaining,
                target:this.labelEnd,
                
                makeLabel:function() {
                	this.Super("makeLabel", arguments);
                    this.label.addMethods({
                    	getCustomState : function () {
                    		// don't show closed state if showClosedGrip is set to false
                    		if (isc.Snapbar.getInstanceProperty("showClosedGrip")) {
                    			return "closed";
                    		}
                    	}
                    });
                }
            });

            this.track = this.addAutoChild("track", {
            	top:Math.round(remaining/2-trackWidth/2),
            	height:trackWidth, 
            	width:this.width,
            	vertical:this.vertical,
            	
        		src:"[SKIN]"+(this.vertical? "v" : "h")+isc.Slider.getInstanceProperty("trackSrc"),
                styleName:isc.Slider.getInstanceProperty((this.vertical ? "v" : "h") + "TrackStyle")
        	});
            
        };

        if (this.track) {
        	this.track.sendToBack();
        }

        if (this.scrollbar) {
            this.scrollbar.setScrollTarget(this);
        }
        
        this.updatePositions();

    },

    // called when mouse-up happens on the control.
    // this is not called when dragging happens
    mouseUp : function() {
        
        if (this.vertical) {
			var val = this.getOffsetY()-this.startThumb.getHeight();
		} else {
			var val = this.getOffsetX()-this.startThumb.getWidth();
		}

        this.slideSelectedRangeByPoints(val);

        this.fireChangedEvent();
    },

    // slide the selected range to so it's middle point will be at given point
    // values are clamped at  [minValue,maxValue] range
    slideSelectedRangeByPoints : function (pixels) {
        var val = this.getValuesForPixels(pixels);
        
		// compute it's middle value
		var avg = (this.endValue-this.startValue)/2;
		
		// simulate dragging so values are clamped instead of ignored if they're
		// outside of bounds
		this.isDragging = true;

		this.setValues(val-avg+this.minValue, val+avg+this.minValue);

		this.isDragging = false;
    },

    // this is called when the user drags the scrollbar
    // scroll the scrollbar to a ratio. If the scrollbar is at either one of its
    // ends, then the entire selected range is slided out - if dragging - or
    // it has both startValue and endValue 0,0 or maxValue, maxValue
    scrollToRatio : function(vertical, ratio, reason) {

    	// compute the ratio of how much was thumb dragged vs the track size, since 
    	// we need to know how much we need to move the selected range towards min or max value
        var dragratio = (this.scrollbar.getEventCoord() - this.dragpoint)/(this.scrollbar.trackSize());
        var val = this.getValueForScrollRatio(dragratio);
        this.setValues(this.oldStartValue+val,this.oldEndValue+val);

    	this.fireChangedEvent();
    },

    // scroll by a small amount (20px) when scroll buttons are clicked
    scrollByDelta : function(vertical, direction, reason) {
        var range = this.endValue-this.startValue;

        // compute the value/pixel ratio, without the width/height of the splitter bars
        if (this.vertical) {
            var w = this.getHeight()-this.startThumb.getHeight()-this.endThumb.getHeight();
            
        } else {
            var w = this.getWidth()-this.startThumb.getWidth()-this.endThumb.getWidth();
        }

        var ratio =  (this.maxValue-this.minValue)/w;
        
        // compute how much of range is 20 pixels and move the range
        var amount = 20*ratio*direction;

        var start = this.startValue + amount;
        var end = this.endValue + amount;

        // make sure when end is reached, rage is not changed
        if (start<this.minValue) {
            start = this.minValue;
            end = this.minValue + range;
        }

        if (end>this.maxValue) {
            end = this.maxValue;
            start = this.maxValue-range;
        }

        this.isDragging = true;

        this.setValues(start, end);
        this.isDragging = false;
        this.fireChangedEvent();
    },

    // scroll by a viewport (the selected range)
    scrollByPage : function(vertical, direction, reason) {
        // compute the width of viewport and either add it or remove it from 
        // the start value and end value
        var amount = Math.max((this.endValue-this.startValue),0)*direction;

        var start = this.startValue + amount;
        var end = this.endValue + amount;

        // make sure when end is reached, rage is not changed
        if (start<this.minValue) {
            start = this.minValue;
            end = this.minValue + Math.abs(amount);
        }

        if (end>this.maxValue) {
            end = this.maxValue;
            start = this.maxValue-Math.abs(amount);
        }

        this.isDragging = true;

        this.setValues(start, end);
        this.isDragging = false;
        this.fireChangedEvent();
    },

    
    getViewportRatio : function (vertical) {
        var range = this.maxValue - this.minValue,
            selectedRange = 0;
        if (this.thumbdragging) {
            selectedRange = this.oldEndValue - this.oldStartValue;
        } else {
            selectedRange = this.endValue - this.startValue;
        }
        return (Math.abs(range) < isc.RangeSlider._epsilon ? 0 : selectedRange / range);
    },

    
    getScrollRatio : function (vertical) {
        var range = this.maxValue - this.minValue,
            selectedRange = this.endValue - this.startValue;
        return (
            Math.abs(range - selectedRange) < isc.RangeSlider._epsilon
                ? 0 : (this.startValue - this.minValue) / (range - selectedRange));
    },

    // Convert a scroll ratio to a value.
    getValueForScrollRatio : function(val) {
        return val * (this.maxValue - this.minValue);
    },

    // convert a length in pixels in length in values
    getValuesForPixels : function(val) {

        if (this.vertical) {
            return val*(this.maxValue-this.minValue)/(this.getHeight());
        }
        else {
            return val*(this.maxValue-this.minValue)/(this.getWidth());
        }
    },

    // recompute the position of all components according to the current
    // minValue, maxValue, startValue, endValue
    // this will implicitly resize also the scrollbar in concordance with
    // the ratio of the selected range vs minValue-maxValue range
    updatePositions : function() {

        // compute the pixel/value ratio, without the width/height of the splitter bars
        if (this.vertical) {
            var w = this.getHeight()-this.startThumb.getHeight()-this.endThumb.getHeight();
            
        } else {
            var w = this.getWidth()-this.startThumb.getWidth()-this.endThumb.getWidth();
        }
        
        // make sure we have a valid ratio and we don't divide by 0 if this.minValue = this.maxValue
        var ratio = 0;

        if (this.maxValue-this.minValue > 0) {
            ratio =  w/(this.maxValue-this.minValue);
        }

        // compute the width of each segment
        var d1 = Math.round((this.startValue-this.minValue)*ratio);
        var d2 = Math.round((this.endValue-this.startValue)*ratio);
        var d3 = Math.round((this.maxValue-this.endValue)*ratio);
        var sum = Math.round((this.startValue-this.minValue+this.endValue-this.startValue)*ratio);
        
        if (this.vertical) {

        	if (d1 == 0) {
        		this.startThumb.target = this.labelDrag;
        	} else {
        		this.labelStart.show();
        		
                this.labelStart.setTop(0);
                this.labelStart.setHeight(d1);
        	}

        	if (d2 == 0) {
        		this.labelDrag.hide();
        	} else {
        		this.labelDrag.show();
                this.labelDrag.setHeight(d2);
                this.labelDrag.setTop(d1+this.startThumb.getHeight());       		
        	}

        	if (d3 == 0) {
        		this.labelEnd.hide();
        	} else {
        		this.labelEnd.show();
        		this.labelEnd.setTop(sum+this.startThumb.getHeight()+this.endThumb.getHeight());
                this.labelEnd.setHeight(d3);
        	}

            this.startThumb.setTop(d1);
            this.endThumb.setTop(sum+this.startThumb.getHeight());
        } 
        else {

        	if (d1 == 0) {
        		this.labelStart.hide();
        	}
        	else {
    			this.labelStart.show();
    			
        		this.labelStart.setLeft(0);
                this.labelStart.setWidth(d1);
        	}
        	
            if (d2 == 0) {
            	this.labelDrag.hide();
            }
            else {
            	this.labelDrag.show();
            	
            	this.labelDrag.setWidth(d2);
                this.labelDrag.setLeft(d1+this.startThumb.getWidth());
            } 
            	

            if (d3 == 0) {
            	this.labelEnd.hide();
            } else {
            	this.labelEnd.show();
            	
                this.labelEnd.setLeft(sum+this.startThumb.getWidth()+this.endThumb.getWidth());
                this.labelEnd.setWidth(d3);
            }

            this.startThumb.setLeft(d1);
            this.endThumb.setLeft(sum+this.startThumb.getWidth());
        }
        
        if (this.scrollbar) {
            this.scrollbar.setThumb();
        }
    },

    // clamp a given value to the allowed [minValue maxValue] range
    clampToMinMax : function (value) {
        if (value<=this.minValue) {
            value = this.minValue;
        }
        if (value >= this.maxValue) {
            value = this.maxValue;
        }

        return value;
    },

    // check if a value is in the allowed [minValue maxValue] range
    isInMinMaxRange : function(value) {
        if (value<this.minValue) {
            return false;
        }
        if (value > this.maxValue) {
            return false;
        }

        return true;
    },
    
    // set both startValue and endValue of the selected area simultaneously.
    // if change of values occurs while dragging , then we're clamping to 
    // [minValue maxValue] range
    // if no dragging occurs, then change is made by user request and we accept
    setValues : function (startv, endv) {

        if (this.isDragging) {
            startv = this.clampToMinMax(startv);
            endv = this.clampToMinMax(endv);
            
            this.startValue = startv;
            this.endValue = endv;
            
            this.updatePositions();
        } else {
            if (this.isInMinMaxRange(startv) && this.isInMinMaxRange(endv) &&
            		startv<=endv) {
                this.startValue = startv;
                this.endValue = endv;
                
                this.updatePositions();
            }

            // otherwise ignore
        }
    },
    
    // if change of startValue occurs while dragging , then we're clamping to 
    // [minValue maxValue] range
    // if no dragging occurs, then change is made by user request and we accept
    // the values only if they're legal
    setStartValue : function (value) {

        if (this.isDragging) {
            value = this.clampToMinMax(value);
            if (value >= this.endValue) {
                value =  this.endValue;
            }
            this.startValue = value;
            this.updatePositions();
        } else {
            if (this.isInMinMaxRange(value) && value <= this.endValue) {
                this.startValue = value;
                this.updatePositions();
            } else {
                isc.logWarn("Ignoring setStartValue to "+value+" (out of range).");
            }
        }
    },

    // if change of endValue occurs while dragging , then we're clamping to 
    // [minValue maxValue] range
    // if no dragging occurs, then change is made by user request and we accept
    // the values only if they're legal
    setEndValue : function( value ) {
        if (this.isDragging) {
            value = this.clampToMinMax(value);
            if (value <= this.startValue) {
                value =  this.startValue;
            }
            this.endValue = value;
            this.updatePositions();
        } else {
            if (this.isInMinMaxRange(value) && value >= this.startValue) {
                this.endValue = value;
                this.updatePositions();
            } else {
                isc.logWarn("Ignoring setEndValue to "+value+" (out of range).");
            }
        }
    },
    
    getStartValue : function() {
    	return this.startValue;
    },
    
    getEndValue : function() {
    	return this.endValue;
    },
    
    setMinValue : function(value) {
        this.minValue = value;
        this.updatePositions();
    },

    setMaxValue : function (value) {
        this.maxValue = value;
        this.updatePositions();
    },
    
    getMinValue : function() {
    	return this.minValue;
    },
    
    getMaxValue : function() {
    	return this.maxValue;
    },
    
    fireChangedEvent : function() {
        this.changed(this.startValue, this.endValue, this.isDragging);
    },

    changed : function (startValue, endValue, isDragging) {}
});
