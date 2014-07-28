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


//>	@class	ColorItem
// FormItem for selecting a color via a pop-up ColorPicker.
//
// @treeLocation Client Reference/Forms/Form Items
// @visibility external
//<

isc.ClassFactory.defineClass("ColorItem", "TextItem");

//> @class  ColorPickerItem
//
// Form item for colors input
// @visibility external
//<
// Alias for smartgwt
isc.addGlobal("ColorPickerItem", isc.ColorItem);

isc.ColorItem.addProperties({
    // Don't update on keystrokes, as we're verifying the color on change.
    changeOnBlur:true,
    changeOnKeypress:false,
    
    // Properties for the default formItem picker handling code
    pickerConstructor: "ColorPicker",
    pickerDefaults: {
        // By default the form item 'picker' subsystem will fired pickerDataChanged in response
        // to a picker firing its dataChanged() method.
        // ColorChoosers support 'colorSelected()' rather than dataChanged, so override this
        // notification method to fire pickerColorSelected instead.
        colorSelected : function (color, opacity) { 
            this.callingFormItem.pickerColorSelected(color, opacity) 
        }
    },
    
    
    //>	@attr	colorItem.showPickerIcon    (Boolean : true : IRW)
    // Should the pick button icon be shown for choosing colors from a ColorPicker
    // @visibility external
    //<
    showPickerIcon:true,

    //>	@attr	colorItem.pickerIconWidth (Integer : 18 : IRW)
    // @include FormItem.pickerIconWidth
    // @visibility external
    //<
    pickerIconWidth:18,

    //>	@attr	colorItem.pickerIconHeight    (Integer : 18 : IRW)
    // @include FormItem.pickerIconHeight
    // @visibility external
    //<
    pickerIconHeight:18,

    //> @attr colorItem.pickerIconSrc (SCImgURL : "[SKIN]/DynamicForm/ColorPicker_icon.png" : IRW)
    // @include FormItem.pickerIconSrc
    // @visibility external
    //<
    // Note - by default this image has a transparent patch allowing the
    // background color to show through.
    pickerIconSrc:"[SKIN]/DynamicForm/ColorPicker_icon.png",

    //> @attr colorItem.pickerIconPrompt (string : "Click to select a new color" : IR)
    // @include formItem.pickerIconPrompt
    // @group i18nMessages
    // @visibility external
    //<
    pickerIconPrompt : "Click to select a new color",

    //> @attr colorItem.pickerIconProperties (object : {...} : IRWA)
    // Properties for the picker icon.
    // @visibility external
    //<
    pickerIconProperties:{
        showOver:false
    },
    
    //>@attr colorItem.defaultPickerMode (string : "simple" : IR)
    // The +link{ColorPicker} associated with this ColorItem can operate in either a "simple" mode 
    // (where it displays just the 40 basic colors and allows the user to click one), or a 
    // "complex" mode (where the user can specify a color from anywhere in the spectrum, with 
    // an optional alpha element). The defaultPickMode attribute specifies which of these two 
    // modes is in force when the picker first loads.
    // @see ColorPicker.defaultPickMode
    // @visibility external
    //<     
    defaultPickerMode: "simple",
    
    //>@attr colorItem.allowComplexMode (Boolean : true : IR)
    // Should "complex" mode be allowed for the +link{ColorPicker} window associated with  
    // this ColorItem?<p>
    // If false, no "More" button is shown on the simple picker
    // @visibility external
    //<     
    allowComplexMode: true,
    
    //> @attr   colorItem.supportsTransparency  (Boolean : false : IRW)
    // Determines whether the +link{ColorPicker} associated with this ColorItem allows the user 
    // to set transparency/opacity information whilst selecting a color. If false, no opacity
    // slider is shown and all colors are 100% opaque.<p>
    // <b>Note</b> ColorItems are representations of HTML color strings, they do not implicitly
    // support transparency.  Setting supportsTransparency to true just allows the user to 
    // set opacity with the picker; if you actually want to capture that information,  you will
    // also need to override +link{pickerColorSelected}.
    // @visibility external
    //<
    supportsTransparency : false,
    
    // Disable native spellChecking on color fields
    browserSpellCheck:false,

    type: "color"
});

isc.ColorItem.addMethods({

    init : function() {
        this.pickerDefaults.defaultPickMode = this.defaultPickerMode;
        this.pickerDefaults.allowComplexMode = this.allowComplexMode;
        this.pickerDefaults.supportsTransparency = this.supportsTransparency;
        this.Super("init", arguments);
    },

    // Override updateValue to validate the color, and update the icon color
    updateValue : function () {

        var oldValue = this._value,
            value = this.getElementValue();

        // unmap the value if necessary 
        value = this.mapDisplayToValue(value);

        if (value == this._value) return;
        
        // If the user entered an invalid color just refuse to accept it.
        if (value != null && !isc.isA.color(value)) {
            this.setElementValue(oldValue);
            return;
        }

        // Allow the superclass implementation to actually update the value
        this.Super("updateValue", arguments); 

        // Assuming the change wasn't rejected, update our icon background color.
        if (this.showPickerIcon && this._value != oldValue) {
            this.setIconBackgroundColor(this.getPickerIcon(), this._value);
        }
    },

	//>	@method	colorItem.getDefaultValue()	(A)
	//		Override getDefaultValue to guarantee that it returns a color (or null)
	//<
	getDefaultValue : function () {
		var value = this.Super("getDefaultValue", arguments);
        if (value && !isc.isA.color(value)) {
            this.logWarn("Default value:" + value + " is not a valid color identifier." + 
                        " Ignoring this default.");
            value = this.defaultValue = null;
        }
        return value;
	},

    // Override 'showPicker' to pass in supportsTransparency
    showPicker : function () {
        this.picker = isc.ColorChooser.getSharedColorPicker(this.pickerDefaults);
        var picker = this.picker;

        var oldItem = picker.callingFormItem;
        if (oldItem != this) {
            picker.callingFormItem = this;
            picker.callingForm = this.form;
            picker.setSupportsTransparency(this.supportsTransparency);
        }
        picker.setHtmlColor(this._value || "");
        if (picker.allowComplexMode) {
            if (picker._currentPickMode == 'simple') {  
                picker.modeToggleButton.setTitle(picker.moreButtonTitle);
            } else {
                picker.modeToggleButton.setTitle(picker.lessButtonTitle);
            }
        }

        if (!this.isObserving(picker, "visibilityChanged")) {
            this.observe(picker, "visibilityChanged", this.pickerVisibilityChanged);
        }

        return this.Super("showPicker", arguments);
    },

	//>	@method	colorItem.pickerColorSelected()
    //  Store the color value selected by the user from the color picker.  You will need to 
    //  override this method if you wish to capture opacity information from the +link{ColorPicker}
    //  @param  color (String)   the selected color as a string
    //  @param  opacity (int)  the selected opacity, from 0 (transparent) to 100 (opaque).
    //                            Only applicable if +link{supportsTransparency} is true.
    // @visibility external
	//<
	pickerColorSelected : function (color, opacity) {
        // If using a mask for color entry, the valuemap cannot be used.
        if (!this.mask) {
            color = this.mapValueToDisplay(color);
        }
        this.setElementValue(color);
        this.updateValue();
	},

    pickerVisibilityChanged : function (isVisible) {
        if (!isVisible) {
            this.focusInIcon("picker");
        }
    },

    // Override setValue to ensure we update the color swatch icon.
    setValue : function (newValue) {
        this.Super("setValue", arguments);
        this.setIconBackgroundColor(this.getPickerIcon(), this._value);
    }
});

