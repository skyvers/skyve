/*
 * Isomorphic SmartClient
 * Version v10.0p_2015-01-04 (2015-01-04)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */

 
//> @class Rangebar
//  
//  @treeLocation Client Reference/Control
//<
isc.ClassFactory.defineClass("Rangebar", "Progressbar");

//----------  Define instance properties  ----------\\
isc.Rangebar.addProperties({

    value:0,						
	minValue:0,					    
	maxValue:99,

	title:"",					// title.............optional display title 
	
	vertical:true,				// vertical.........vertical rangebar if true; horizontal rangebar if false

    showTitle:true,				// showTitle........if true, display the bar's title
								
	showRange:true,				// showRange........if true, display the min and max values of the bar;

    showValue:true,			    // showValue........if true, display the bar's value

    allLabelDefaults : {
        width : 50,
        height : 20,
	    spacing : 5          // space between the label and the bar - this is used by Rangebar only
    },

    titleLabelDefaults : {
        width : 100,
        className : "rangebarTitle"
    },

    rangeLabelDefaults : {
    	className:"rangebarRange"
    },
    
    valueLabelDefaults : {
    	className:"rangebarValue"
    },

    forceOverrides : {
        _resizeWithMaster: false,
        autoDraw: false
    },
    
	// text to use for range label instead of minValue
	//minValueLabel:null,

	// text to use for range label instead of maxValue
	//maxValueLabel:null,			
	
	flipValues: false 			//XXX NOT TESTED
});				  

//!>Deferred

//----------  Define instance methods  ----------\\
isc.Rangebar.addMethods({

initWidget : function () {
    this.Super(this._$initWidget);

    this.titleLabelDefaults = isc.addProperties({}, this.allLabelDefaults,
                                                this.titleLabelDefaults);
    this.valueLabelDefaults = isc.addProperties({}, this.allLabelDefaults,
                                                this.valueLabelDefaults);
    this.rangeLabelDefaults = isc.addProperties({}, this.allLabelDefaults,
                                                this.rangeLabelDefaults);
    if (this.showRange) {
        this._minLabel = this.addPeer(this._createRangeLabel("min"));
        this._maxLabel = this.addPeer(this._createRangeLabel("max"));
    }
	if (this.showValue) this._valueLabel = this.addPeer(this._createValueLabel());
	if (this.showTitle) this._titleLabel = this.addPeer(this._createTitleLabel());
    this.setValue(this.value);
},

resized : function(deltaX, deltaY) {
    this._adjustPeerPositions();
},

_adjustPeerPositions : function() {
    if(this.showRange && this._minLabel && this._maxLabel) {
        var minProps = this._computeRangeLabelProperties("min");
        var maxProps = this._computeRangeLabelProperties("max");
        this._minLabel.moveTo(minProps.left, minProps.top);
        this._maxLabel.moveTo(maxProps.left, maxProps.top);
    }
    
    if(this.showValue && this._valueLabel) {
        var props = this._computeValueLabelProperties();
        this._valueLabel.moveTo(props.left, props.top);
    }

    if(this.showTitle && this._titleLabel) {
        var props = this._computeTitleLabelProperties();
        this._titleLabel.moveTo(props.left, props.top);
    }
},

//------  _createRangeLabel(minOrMax)
// Creates, initializes, and returns a new Label widget to be the rangebar's mix or max value
// label. minOrMax must be the string "min" or "max".
_createRangeLabel : function (minOrMax) {
    var props = this._computeRangeLabelProperties(minOrMax);    

	return isc.Label.newInstance({
        ID:this.getID()+"_"+minOrMax+"Label",
		contents:(minOrMax == "min" ?
			(this.minValueLabel ? this.minValueLabel : this.minValue) : 
			(this.maxValueLabel ? this.maxValueLabel : this.maxValue) )
	}, this.rangeLabelDefaults, props, this.forceOverrides);
},

_computeRangeLabelProperties : function (minOrMax) {
    var props = {},
        defs = this.rangeLabelDefaults,
		shouldFlip = ((minOrMax == "min" && !this.flipValues) || 
                      (minOrMax = "max" && this.flipValues));

    if (this.vertical) {
		props.left = this.left + this.width + defs.spacing,
		props.align = isc.Canvas.LEFT;
        if (shouldFlip) {
			props.top = this.getTop() + this.getHeight() - defs.height;
			props.valign = isc.Canvas.BOTTOM;
		} else {
			props.top = this.getTop();
			props.valign = isc.Canvas.TOP;
		}
	} else { // this.horizontal
		props.top = this.getTop() + this.getHeight() + defs.spacing,
		props.valign = isc.Canvas.TOP;
        if (shouldFlip) {
			props.left = this.getLeft();
			props.align = isc.Canvas.LEFT;
		} else {
			props.left = this.getLeft() + this.getWidth() - defs.width;
			props.align = isc.Canvas.RIGHT;
		}
	}
    return props;
},


//------  _createTitleLabel()
// Creates, initializes, and returns a new Label widget to be the reangebar's title label.
_createTitleLabel : function () {
    var props = this._computeTitleLabelProperties();
    
	return isc.Label.newInstance({
		ID:this.getID()+"_titleLabel",
		contents:this.title
	}, this.titleLabelDefaults, props, this.forceOverrides);
},

_computeTitleLabelProperties : function () {
    var props = {};
    var defs = this.titleLabelDefaults;

	if (this.vertical) {
    	props.left = this.left + this.width/2 - defs.width/2;
		props.top = this.top - defs.height - defs.spacing;
		props.align = isc.Canvas.CENTER;
	} else {
		props.left = this.left - defs.width - defs.spacing;
		props.top = this.top + this.getHeight()/2 - defs.height/2;
		props.align = isc.Canvas.RIGHT;
	}

    return props;
},


//------  _createValueLabel()
// Creates, initializes, and returns a new Label widget to be the rangebar's dynamic value
// label.
_createValueLabel : function () {
    var props = this._computeValueLabelProperties();
    
	return isc.Label.newInstance({
		ID:this.getID()+"_valueLabel",
		contents:this.value,
		mouseUp:"return false;",
		observes:[{source:this, message:"valueChanged", action:"observer.setContents(this.getValue())"}]
	}, this.valueLabelDefaults, props, this.forceOverrides);
},

_computeValueLabelProperties : function () {
    var props = {};
    var defs = this.valueLabelDefaults;    

	if (this.vertical) {
		props.left = this.left - defs.width - defs.spacing;
		props.top = this.top + this.getHeight()/2 - defs.height/2;
		props.align = isc.Canvas.RIGHT;
		props.valign = isc.Canvas.CENTER;
	} else {
		props.left = this.left + this.width/2 - defs.width/2;
		props.top = this.top - defs.height - defs.spacing;
		props.align = isc.Canvas.CENTER;
		props.valign = isc.Canvas.BOTTOM;
	}
    return props;
},

getValue : function () {
	return this.value;
},

// Sets this.value to the new value, moves the rangebar to the appropriate position
// for this value, and calls valueChanged() which you can observe
setValue : function (newValue) {
	// do nothing if the value hasn't actually changed
	if (this.value == newValue) return;

	// make sure the new value falls in the range allowed by this instance.
    // If the value provided is outside the range, it clamps to the appropriate
	// boundary (min/max)
	if (newValue > this.maxValue) newValue = this.maxValue;			
	else if (newValue < this.minValue) newValue = this.minValue;
	this.value = newValue;
    this.percentDone = 100 * (this.value - this.minValue) / (this.maxValue - this.minValue);
    this.markForRedraw();
	this.valueChanged();	// observable method
},

valueChanged : function () { 

}

});
//!<Deferred
