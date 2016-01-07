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
//> @class SliderItem
// FormItem that uses a +link{class:Slider} component to present an interface for picking
// from either a continuous range or a range with a small number of discrete values.
//
// @treeLocation Client Reference/Forms/Form Items
// @visibility external
// @example sliderItem
//<



isc.ClassFactory.defineClass("SliderItem", "CanvasItem");

isc.SliderItem.addProperties({

    defaultType: "float",

    // Passthroughs: certain properties are very likely to be set and so we allow setting them
    // right on the SliderItem
    // ---------------------------------------------------------------------------------------

    //> @attr sliderItem.vertical (Boolean : false : IR)
    // @include slider.vertical
    //<
    vertical:false,

    //>	@attr sliderItem.minValue (float : 1 : IRW)
    // @include slider.minValue
    // @example sliderItem
    //<
    minValue:1,

    //>	@attr sliderItem.maxValue (float : 100 : IRW)
    // @include slider.maxValue
    // @example sliderItem
    //<
    maxValue:100,

    //>	@attr sliderItem.numValues (integer : null : IRW)
    // @include slider.numValues
    // @example sliderItem
    //<
    
    //> @attr sliderItem.roundValues (Boolean : true : IR)
    // @include slider.roundValues
    // @example sliderItem
    //<
    roundValues:true,

    //> @attr sliderItem.roundPrecision (number : 1 : IR)
    // @include slider.roundPrecision
    // @example sliderItem
    //<
    roundPrecision:1,
    
    //>	@attr sliderItem.defaultValue (int : 1 : [IRW])
    // Default value for this sliderItems is 1.
    // @visibility external
    //<
    defaultValue:1,
    
    // sliderProperties: use for setting any other Slider property
    // ---------------------------------------------------------------------------------------
    
    //> @attr sliderItem.slider (AutoChild Canvas : null : R)
    // This item is an autoChild generated +link{class:Canvas} displayed by
    // the SliderItem and is an instance of +link{class:Slider} by default. It is customizable 
    // via the standard +link{autoChild} pattern, by customizing +link{sliderProperties}
    // and +link{sliderConstructor}.
    // 
    // @visibility external
    //<

    //> @attr sliderItem.sliderProperties (Slider properties : null : IR)
    // Properties to add to the automatically created +link{slider} used by this
    // FormItem.  See the +link{class:Slider} class for reference.
    //
    // @visibility external
    //<

    //> @attr sliderItem.valueStyle (CSSStyleName : "sliderValue" : IR)
    // +link{StatefulCanvas.baseStyle,Base style} for the value label.
    //<
    valueStyle: "sliderValue",

    //> @attr sliderItem.pendingValueStyle (CSSStyleName : "pendingSliderValue" : IR)
    // +link{StatefulCanvas.baseStyle,Base style} for the value label when this <code>SliderItem</code>
    // is +link{attr:showPending,pending}.
    //<
    pendingValueStyle: "pendingSliderValue",

    // ---------------------------------------------------------------------------------------

    //> @attr sliderItem.shouldSaveValue (Boolean : true : IR)
    // @include FormItem.shouldSaveValue
    //<
    shouldSaveValue:true,

    //> @attr sliderItem.sliderConstructor (Class : Slider : IR)
    // Constructor class for this item's +link{slider}.
    // @visibility external
    //<
    sliderConstructor:"Slider",

    sliderDefaults : {
        autoDraw:false,
        showTitle:false,
        hover : function () {
            var sliderItem = this.canvasItem;
            if (sliderItem != null) {
                sliderItem._handleHover();
                return false;
            }
        },
        valueChanged : function () {
            var sliderItem = this.canvasItem;
            if (sliderItem != null) sliderItem.sliderChange();
        }
    },
    autoDestroy:true
});

isc.SliderItem.addMethods({
    init : function () {
        this.Super("init", arguments);
        // used for change detection
        this._currentValue = this.getDefaultValue();
    },

    // Override _createCanvas to set up a Slider as this item's canvas, and set up appropriate
    // set of properties.
    _createCanvas : function () {
        // create our slider based on our sliderDefaults, and appropriate instance properties.

        // Use 'addAutoChild' - this will handle applying the various levels of defaults
        var slider = this.canvas = this.addAutoChild("slider", {
            canHover: this._getShowPending(),
            vertical: this.vertical,
            minValue: this.minValue,
            maxValue: this.maxValue,
            value: this.defaultValue,
            numValues: this.numValues,
            roundValues: this.roundValues,
            roundPrecision: this.roundPrecision,
            tabIndex: this.getGlobalTabIndex(),
            valueStyle: this.valueStyle
        }, this.sliderConstructor, this.container);

        if (slider != null) {
            
            this.observe(slider._track, "_focusChanged", this.sliderPartFocusChanged);
            this.observe(slider._thumb, "_focusChanged", this.sliderPartFocusChanged);
            if (slider._thumb.triggerArea != null) {
                this.observe(slider._thumb.triggerArea, "_focusChanged", this.sliderPartFocusChanged);
            }
        }

        this.Super("_createCanvas", arguments);        
    },

    sliderPartFocusChanged : function () {
        var form = this.form,
            focusCanvas = this.ns.EH.getFocusCanvas();
        if (focusCanvas != null && this.canvas.contains(focusCanvas, true)) {
            if (form.getFocusSubItem() !== this) {
                form.setFocusItem(this);
                this.elementFocus();
            }
        } else {
            this.elementBlur();
            if (form.getFocusSubItem() === this) form.setFocusItem(null);
        }
    },

    // Sliders are editable
    isEditable : function () {
        return this.getCanEdit();
    },

    //> @method sliderItem.pendingStatusChanged()
    // Notification method called when +link{FormItem.showPending,showPending} is enabled and
    // this <code>SliderItem</code> should either clear or show its pending visual state.
    // <p>
    // The default behavior is that the +link{FormItem.titleStyle,titleStyle} and
    // +link{FormItem.cellStyle,cellStyle} are updated to include/exclude the "Pending" suffix.
    // In addition, when displayed in the pending state the value label changes color.
    // Returning <code>false</code> will cancel this default behavior.
    // @include FormItem.pendingStatusChanged()
    //<
    _defaultPendingStatusChangedBehavior : function (pendingStatus) {
        this.Super("_defaultPendingStatusChangedBehavior", arguments);
        this.canvas.setValueStyle(pendingStatus ? this.pendingValueStyle : this.valueStyle);
    },

    // Override setValue to update the value on the slider
    setValue : function (value) {
        this._setValueCalled = true;

        var defaultVal;
        if (value == null) {
            defaultVal = this.getDefaultValue();
            // don't apply the default value if it's not set - this allows for the distinction 
            // between setting the value to 'null' vs 'undefined'
            if (defaultVal != null) value = defaultVal;
        }
        // update the previous value so we don't fire a change handler on 'sliderChanged'
        this._currentValue = value;
        this.canvas.setValue(value, (defaultVal != null));
        // No need to save this value - that should be handled by sliderChange
    },
    
    getValue : function () {
        return this.canvas.getValue();
    },
    
    //>@attr sliderItem.changeOnDrag (Boolean : true : IRW)
    // Should this sliderItem update its value and fire change handlers while the user is
    // actively dragging the slider.
    // Setting this attribute value to <code>false</code> will suppress any change notifications
    // from the user dragging the slider thumb until the user releases the mouse at the final
    // position.
    // This can be useful to avoid repeatedly firing expensive operations such as server fetches
    // while the user drags through a range of values.
    // @visibility external
    //<
    
    changeOnDrag:true,
    
    // Define a sliderChange function to handle value changes
    sliderChange : function() {
        
        // Note: the slider.valueChanged method doesn't inform us what the old value was, so
        // we need to track this ourselves in order to pass it to any item level change handler.
        var val = this.canvas.getValue();

        if (this._currentValue != val) {
            // if changeOnDrag is false, don't update anything until the user is done dragging
            // the slider around
            if (this.changeOnDrag || !this.canvas.valueIsChanging()) {
                this._updateValue(val);
                this._currentValue = val;
            }
            
        // catch the case where there was an explicit setValue() call, and save out the new
        // value from the slider.
        } else {
            this.saveValue(val);
        }
    },
    
    //>	@method	sliderItem.setMinValue()   ([])
    // @include slider.setMinValue()
    //<
    setMinValue : function (newValue) {
        this.canvas.setMinValue(newValue);
        // update this just in case someone checks canvasItem.minValue, since we do declare it
        // as a property on the CanvasItem.
        this.minValue = newValue; 
    },
    
    //>	@method	sliderItem.setMaxValue()   ([])
    // @include slider.setMaxValue()
    //<
    setMaxValue : function (newValue) {
        this.canvas.setMaxValue(newValue);
        this.maxValue = newValue;
    },
    
    //>	@method	sliderItem.setNumValues()   ([])
    // @include slider.setNumValues()
    //<
    setNumValues : function (newNumValues) {
        this.canvas.setNumValues(newNumValues);
        this.numValues = newNumValues;
    },
    
    _shouldAllowExpressions : function () {
        return false;
    }
});
