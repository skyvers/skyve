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






//> @type ColorPickerMode
// @value "simple" Display a palette of 40 basic colors from which to pick.
// @value "complex" In addition to the 40 basic colors, the user can specify a color from anywhere
// in the spectrum, with an optional alpha component.
// @visibility external
//<


//> @class ColorPicker
// The ColorPicker widget allows the user to select a color from anywhere in the 
// color spectrum. It also supports selecting the alpha (opacity) value of the 
// color.  The picker supports a simple mode - which allows for one-click selection
// from a standard palette of colors - and a complex mode which allow the user to
// define any conceivable color. It is possible for the user to switch from simple
// mode to complex by interacting with the widget.  In general, the widget provides
// very similar functionality to the color picker dialogs found in graphics packages
// and other desktop software.
// @treeLocation Client Reference/Forms
// @visibility external
//<

if (isc.Window) {

isc.ClassFactory.defineClass("ColorPicker", isc.Window);

isc.ColorPicker.addClassMethods({

//> @classMethod ColorPicker.getSharedColorPicker
// Returns the shared global ColorPicker. 
// Many applications will only need one ColorPicker instance; for such use 
// cases, it is a good idea to use the shared object for performance reasons.
// <p>
// The optional second parameter to this method indicates whether the shared picker
// should retain the state (mode, color and opacity) it was in last time it was used, 
// or revert to defaults.
// Generally, you will want the picker to revert to default state; this gives the
// same user experience as creating a new instance without incurring the overhead.
// However, some use cases will benefit from the picker remembering what the user
// did last time.
// @param properties (Object) Properties to apply to the global ColorPicker object
// @param [keepCurrentState] (boolean) Should we keep the current state?
//                                    If false (or not provided), revert to default state
// @visibility external
//<
getSharedColorPicker : function (properties, keepCurrentState) {

    properties = properties || {};
    
    if (!isc.isA.ColorPicker(this._globalColorPicker)) {
        this._globalColorPicker = isc.ColorPicker.create(properties);
    } else {
        // Ensure previous colorSelected won't fire even if properties.colorSelected is undefined 
        if (properties.colorSelected == null) delete this._globalColorPicker.colorSelected;
        if (properties.colorChanged == null) delete this._globalColorPicker.colorChanged;

        // Set properties so that RichTextEditor can assign this._globalColorPicker.creator to itself (RichTextEditor.js, chooseColor method, line 587)
        this._globalColorPicker.setProperties(properties);
    }

    if (!keepCurrentState) {
        var picker = this._globalColorPicker;

        if (picker._currentPickMode != picker.defaultPickMode) {
            picker._currentPickMode = picker.defaultPickMode;
            if (picker._currentPickMode == 'simple') {  
                picker.removeComplexElements();
                if (picker.allowComplexMode) {
                    picker.modeToggleButton.setTitle(picker.moreButtonTitle);
                }
            } else {
                if (!picker._rgbForm) {
                    picker.createComplexElements();
                }
                picker.addComplexElements();
                picker.modeToggleButton.setTitle(picker.lessButtonTitle);
            }
        }

        // Note we already copied "properties" over to "picker" via the setProperties call
        // above.
        picker.setHtmlColor(picker.defaultColor);
        picker.setOpacity(picker.defaultOpacity);
    }
    return this._globalColorPicker;
}
});

// ColorChooser was an undocumented widget used by the RichTextEditor. This component
// supersedes it.
isc.addGlobal("ColorChooser", isc.ColorPicker);

isc.ColorPicker.addProperties({
 
// Default Window properties first
canFocusInHeaderButtons: true,
autoSize: true,
keepInParentRect: true,
isModal: true,
autoCenter: true,
autoDraw: false,
showMinimizeButton: false,
closeClick: function () { this.hide(); },

layoutMargin: 2,

//> @attr colorPicker.okButton (AutoChild IButton : null : R)
// "OK" button for the ColorPicker
// @visibility external
//<

showOkButton: true,

//>    @attr colorPicker.okButtonConstructor    (Class : IButton : IRWA)
//      The class of the "OK" button. It is intended that you use either IButton or 
//      Button - other classes are unlikely to work correctly.
//      @visibility external
//<
okButtonConstructor: isc.IButton,

okButtonDefaults: {
    width: 80,
    autoParent: "buttonLayout",
    click: function () {
        if (this.creator.colorSelected) {
            this.creator.colorSelected(this.creator.getHtmlColor(), this.creator.getOpacity());
        }
        this.creator.hide();
    }
},
    
    
//> @attr colorPicker.cancelButton (AutoChild IButton : null : R)
// Cancel button for the ColorPicker
// @visibility external
//<

showCancelButton: true,
    
//>    @attr colorPicker.cancelButtonConstructor    (Class : IButton : IRWA)
//      The class of the Cancel button. It is intended that you use either IButton or 
//      Button - other classes are unlikely to work correctly.
//      @visibility external
//<
cancelButtonConstructor: isc.IButton,

cancelButtonDefaults: {
    title: "Cancel",
    width: 80,
    autoParent: "buttonLayout",
    click: function () {
        this.creator.hide();
    }
},

//> @attr colorPicker.modeToggleButton (AutoChild IButton : null : R)
// "More"/"Less" button for the ColorPicker
// @visibility external
//<

showModeToggleButton: true,
    
//>    @attr colorPicker.modeToggleButtonConstructor    (Class : IButton : IRWA)
//      The class of the mode toggle button. It is intended that you use either IButton or 
//      Button - other classes are unlikely to work correctly.
//      @visibility external
//<
modeToggleButtonConstructor: isc.IButton,

modeToggleButtonDefaults: {
    width: 80,
    autoParent: "buttonLayout",
    click: function () { 
        this.creator._togglePickMode();
    }
},

showButtonLayout: true,
buttonLayoutConstructor: "HLayout",
buttonLayoutDefaults: {
    autoParent: "contentLayout"
},

//> @attr colorPicker.defaultColor (text : #808080 : IR)
// The default color. This is the color that is selected when the picker first loads
// @visibility external
//<     
defaultColor: "#808080",

//> @attr colorPicker.colorButtonSize (number : 20 : IR)
// Width and height of the basic color boxes (they are always square, and they are
// all the same size).
// @visibility external
//<     
colorButtonSize: 20,  

//> @attr colorPicker.colorButtonBaseStyle (String : "ColorChooserCell" : IR)
// Base CSS style applied to the basic color boxes 
// @visibility external
//<     
colorButtonBaseStyle : "colorChooserCell",

//> @attr colorPicker.colorArray (array : [...]  : IR)
// Array containing 40 HTML color strings, which will be shown as the basic color 
// selection boxes.
// @visibility external
//<


colorArray: [
    "#000000",
    "#993300",
    "#333300",
    "#003300",
    "#003366",
    "#000080",
    "#333399",
    "#333333",

    "#800000",
    "#FF6600",
    "#808000",
    "#008000",
    "#008080",
    "#0000FF",
    "#666699",
    "#808080",

    "#FF0000",
    "#FF9900",
    "#99CC00",
    "#339966",
    "#33CCCC",
    "#3366FF",
    "#800080",
    "#999999",

    "#FF00FF",
    "#FFCC00",
    "#FFFF00",
    "#00FF00",
    "#00FFFF",
    "#00CCFF",
    "#993366",
    "#C0C0C0",

    "#FF99CC",
    "#FFCC99",
    "#FFFF99",
    "#CCFFCC",
    "#CCFFFF",
    "#99CCFF",
    "#CC99FF",
    "#FFFFFF"
],

numBasicColorRows: 5,
numBasicColorColumns: 8,

basicColorLayoutDefaults: {
    _constructor: "VLayout",
    autoParent: "none"
},

basicColorRowLayoutDefaults: {
    _constructor: "HLayout",
    layoutBottomMargin: 1,
    membersMargin: 1
},

basicColorSwatchDefaults: {
    _constructor: "StatefulCanvas",
    overflow: "hidden",
    title: "",
    showFocusOutline: false,
    showTitle: false,
    showFocused: true,
    showRollOver: true,
    showFocusedAsOver: true,
    canFocus: true,
    click : function () {
        var picker = this.creator;
        picker.setHtmlColor(this.backgroundColor);
        if (picker._currentPickMode == "simple") {
            picker._oneClickColorSelected(this.backgroundColor);
        }
    }
},

//> @attr colorPicker.swatchWidth (number : 170 : IR)
// Displayed width of the color swatch image. The default width is approximately
// that used by the Windows&#174; XP color picking window
// @visibility external
//<     
swatchWidth:170,

//> @attr colorPicker.swatchHeight (number : 170 : IR)
// Displayed height of the color swatch image. The default height is approximately
// that used by the Windows&#174; XP color picking window
// @visibility external
//<     
swatchHeight:170, 

//> @attr colorPicker.lumStep (number : 4 : IR)
// The Luminosity bar shows the selected color tone at numerous levels of brightness,
// from black to white. It is implemented as a stack of isc.Canvas objects. This attribute
// determines the height of each of those canvases.
//<     
lumStep:4,           // The size of the steps in the Lum indicator

//> @attr colorPicker.lumWidth (number : 15 : IR)
// Width of the Luminosity bar
// @visibility external
//<     
lumWidth:15,       // The width of the Lum indicator
    
//> @attr colorPicker.supportsTransparency (Boolean : true : IR)
// Determines whether to show the opacity slider. This allows the user to select colors
// with an alpha element (ie, semi-transparent colors). If this attribute is set to false,
// no opacity slider is shown, and all colors are completely opaque.
// @visibility external
//<     
supportsTransparency:true, 

//> @attr colorPicker.opacityText (String : "Lorem ipsum dolor sit amet, consectetuer adipiscing elit." : IR)
// The text to show underneath the selected color box, so that it can 
// be seen through semi-transparent colors. If you do not want such text, set 
// this value to blank. This value is irrelevant if 
// +link{ColorPicker.supportsTransparency} is false.
// @visibility external
//<
opacityText: "Lorem ipsum dolor sit amet, consectetuer adipiscing elit.",

//> @attr colorPicker.swatchImageURL (String : "[SKIN]ColorPicker/spectrum.png" : IR)
// The location of the color swatch image file
// @visibility external
//<     
swatchImageURL: "[SKIN]ColorPicker/spectrum.png", 

//> @attr colorPicker.crosshairImageURL (String : "[SKIN]ColorPicker/crosshair.png" : IR)
// The location of the crosshair image file
// @visibility external
//<     
crosshairImageURL: "[SKIN]ColorPicker/crosshair.png", 

//> @attr colorPicker.lessButtonTitle (String : "<< Less" : IR)
// The title for the button that switches to a less complex view.
// @group i18nMessages
// @visibility external
//<     
lessButtonTitle: "<< Less", 

//> @attr colorPicker.moreButtonTitle (String : "More >>" : IR)
// The title for the button that switches to a more complex view.
// @group i18nMessages
// @visibility external
//<
moreButtonTitle: "More >>", 

//> @attr colorPicker.basicColorLabel (String : "Basic Colors:" : IR)
// The label shown above the basic color blocks.
// @group i18nMessages
// @visibility external
//<     
basicColorLabel: "Basic Colors:", 

//> @attr colorPicker.selectedColorLabel (String : "Selected Color" : IR)
// The label shown next to the selected color box.
// @group i18nMessages
// @visibility external
//<     
selectedColorLabel: "Selected Color:", 

//> @attr colorPicker.opacitySliderLabel (String : "Opacity" : IR)
// The label shown next to the opacity slider. Ignored if 
// +link{ColorPicker.supportsTransparency} is false.
// @group i18nMessages
// @visibility external
//<     
opacitySliderLabel: "Opacity:", 

//> @attr colorPicker.defaultOpacity (number : 100 : IR)
// The initial opacity value for the component, as a percentage value between 0 and 100
// @visibility external
//<     
defaultOpacity: 100, 

//> @attr colorPicker.redFieldTitle (String : "Red" : IR)
// The title for the 'Red' field in the complex chooser.
// @group i18nMessages
// @visibility external
//<     
redFieldTitle: "Red", 

//> @attr colorPicker.redFieldPrompt (String : "The Red component of the selected color" : IR)
// The text to show when the mouse hovers over the 'Red' field in the complex chooser.
// @group i18nMessages
// @visibility external
//<     
redFieldPrompt: "The Red component of the selected color", 

//> @attr colorPicker.greenFieldTitle (String : "Green" : IR)
// The title for the 'Green' field in the complex chooser.
// @group i18nMessages
// @visibility external
//<     
greenFieldTitle: "Green", 

//> @attr colorPicker.greenFieldPrompt (String : "The Green component of the selected color" : IR)
// The text to show when the mouse hovers over the 'Green' field in the complex chooser.
// @group i18nMessages
// @visibility external
//<
greenFieldPrompt: "The Green component of the selected color", 

//> @attr colorPicker.blueFieldTitle (String : "Blue" : IR)
// The title for the 'Blue' field in the complex chooser.
// @group i18nMessages
// @visibility external
//<
blueFieldTitle: "Blue", 

//> @attr colorPicker.blueFieldPrompt (String : "The Blue component of the selected color" : IR)
// The text to show when the mouse hovers over the 'Blue' field in the complex chooser.
// @group i18nMessages
// @visibility external
//<
blueFieldPrompt: "The Blue component of the selected color", 

//> @attr colorPicker.htmlFieldTitle (String : "HTML" : IR)
// The title for the 'HTML' field in the complex chooser.
// @group i18nMessages
// @visibility external
//<
htmlFieldTitle: "HTML", 

//> @attr colorPicker.htmlFieldPrompt (String : "The selected color&#39;s HTML coding" : IR)
// The text to show when the mouse hovers over the 'HTML' field in the complex chooser.
// @group i18nMessages
// @visibility external
//<
htmlFieldPrompt: "The selected color&#39;s HTML coding", 

//> @attr colorPicker.hueFieldTitle (String : "Hue" : IR)
// The title for the 'Hue' field in the complex chooser.
// @group i18nMessages
// @visibility external
//<
hueFieldTitle: "Hue", 

//> @attr colorPicker.hueFieldPrompt (String : "The Hue (base tone) of the selected color" : IR)
// The text to show when the mouse hovers over the 'Hue' field in the complex chooser.
// @group i18nMessages
// @visibility external
//<
hueFieldPrompt: "The Hue (base tone) of the selected color", 

//> @attr colorPicker.satFieldTitle (String : "Sat" : IR)
// The title for the 'Sat' field in the complex chooser.
// @group i18nMessages
// @visibility external
//<
satFieldTitle: "Sat", 

//> @attr colorPicker.satFieldPrompt (String : "The Saturation (color purity) of the selected color" : IR)
// The text to show when the mouse hovers over the 'Saturation' field in the complex chooser.
// @group i18nMessages
// @visibility external
//<
satFieldPrompt: "The Saturation (color purity) of the selected color", 

//> @attr colorPicker.lumFieldTitle (String : "Luminosity" : IR)
// The title for the 'Luminosity' field in the complex chooser.
// @group i18nMessages
// @visibility external
//<
lumFieldTitle: "Lum", 

//> @attr colorPicker.lumFieldPrompt (String : "The Luminosity (brightness) of the selected color" : IR)
// The text to show when the mouse hovers over the 'Luminosity' field in the complex chooser.
// @group i18nMessages
// @visibility external
//<
lumFieldPrompt: "The Luminosity (brightness) of the selected color", 

//> @attr colorPicker.okButtonTitle (String : "OK" : IR)
// The title for the 'OK' button.
// @group i18nMessages
// @visibility external
//<
okButtonTitle: "OK", 

//> @attr colorPicker.cancelButtonTitle (String : "Cancel" : IR)
// The title for the 'Cancel' button.
// @group i18nMessages
// @visibility external
//<
cancelButtonTitle: "Cancel", 

//> @attr colorPicker.autoPosition (Boolean : true : IR)
// If true, causes the ColorPicker to appear near where the mouse was last clicked.
// If false, the ColorPicker is centered on first show; depending on the value of 
// +link{autoCenterOnShow}, it either reappears wherever it was last shown after hide/show(), 
// or centered regardless of where it was last shown.
// @see ColorPicker.autoCenterOnShow
// @visibility external
//<     
autoPosition: true,
    
//> @attr colorPicker.autoCenterOnShow (Boolean : true : IR)
// If +link{autoPosition} is false, this property controls whether to automatically center the
// colorPicker every time it is reshown with the show() method.
// @see ColorPicker.autoPosition
// @visibility external
//<     
autoCenterOnShow: true,

//> @attr colorPicker.defaultPickMode (ColorPickerMode : "simple" : IR)
// The <code>ColorPicker</code> can operate in either a "simple" mode (where it displays just the
// 40 basic colors and allows the user to click one), or a "complex" mode (where the
// user can specify a color from anywhere in the spectrum, with an optional alpha
// element). The <code>defaultPickMode</code> attribute specifies which of these two modes is
// in force when the picker first loads.
// @see ColorPicker.allowComplexMode
// @setter setCurrentPickMode()
// @visibility external
//<
defaultPickMode: "simple",

//> @attr colorPicker.allowComplexMode (Boolean : true : IR)
// Should the "complex" mode be allowed for this ColorPicker?
// If false, no "More" button is shown on the simple picker
// @visibility external
//<     
allowComplexMode: true,

_updateColor: true 
});
    
isc.ColorPicker.addMethods({

// Override show() to position near the mouse
show : function () {
    // place the picker immediately adjacent to the mouse pointer, keeping it onscreen
    if (this.autoPosition) {
        this.autoCenter = false;
    	var event = isc.EH.getLastEvent();
        this.placeNear(event.x, event.y);
    } else {
        // Looks like DnD unsets autoCenter - that's great for the general case, but we need 
        // the option to re-center every time after a hide/show.
        if (this.autoCenterOnShow) this.autoCenter = true;
    }

    this.Super("show", arguments);
    this.cancelButton.focus();
},

// the window title is in the language packs as "selectTitle", but we want to also support 
// title being set directly - make title null by default and then set a default selectTitle 
// value - if title is still null at init, use selectTitle
title: null,
selectTitle: "Select a Color",
initWidget : function () {
    this.title = this.title || this.selectTitle;

    this._currentPickMode = this.defaultPickMode;
    if (!this.allowComplexMode) {
        this._currentPickMode = "simple";
    }

    this.addAutoChild("basicColorLayout");

    var numBasicColorRows = this.numBasicColorRows,
        numBasicColorColumns = this.numBasicColorColumns;
    
    var basicColorRowProperties = {
        height: this.colorButtonSize
    };
    for (var i = 0; i < numBasicColorRows; ++i) {
        var wk = this.createAutoChild("basicColorRowLayout", basicColorRowProperties);
        for (var j = 0; j < numBasicColorColumns; ++j) {
            var wk2 = this.createAutoChild("basicColorSwatch", {
                baseStyle: this.colorButtonBaseStyle,
                width: this.colorButtonSize,
                height: this.colorButtonSize,
                backgroundColor: this.colorArray[i * numBasicColorColumns + j]
            });
            wk.addMember(wk2);
        }
        this.basicColorLayout.addMember(wk);
    }

    this.leftHandLayout = isc.VLayout.create({autoDraw:false});

    this.leftHandLayout.addMember(this.basicColorLayout);

    this.innerContentLayout = isc.HLayout.create({
         autoDraw:false,
         align: "center",
         members: [this.leftHandLayout]        
    });

    this.contentLayout = isc.VLayout.create({
        autoDraw:false,
        members: [ this.innerContentLayout ]
    });    
    this.addItem(this.contentLayout);
    if (this._currentPickMode == "simple") {
        this.showOkButton = false;
    }
    if (!this.allowComplexMode) {
        this.showModeToggleButton = false;
    }

    this.addAutoChild("buttonLayout");
    this.addAutoChild("okButton", {title: this.okButtonTitle});
    this.addAutoChild("cancelButton", {title: this.cancelButtonTitle});
    this.addAutoChild("modeToggleButton", {
        title: (this._currentPickMode == "simple" && this.allowComplexMode ? this.moreButtonTitle : this.lessButtonTitle)
    });

    if (this._currentPickMode != "simple") {
        this.createComplexElements();
        this.addComplexElements();
    }

    this.setHtmlColor(this.defaultColor);
    this._setLumVals();
    this.setOpacity(this.defaultOpacity);

    this.Super("initWidget", arguments);
},

createComplexElements : function () {

    if (this._currentPickMode != 'complex') {
        return;
    }        

    this._rgbForm = isc.DynamicForm.create({
        autoDraw: false,
        cellPadding:1,
        padding: 10,
        width:65,
        fields: [ 
            {name: "pickerRedVal", title:this.redFieldTitle, type: "text", 
             width: "40", defaultValue: this._pickedRed,
             prompt: this.redFieldPrompt,
             picker: this,
             changed: function (form,item,value) { this.picker.setRed(value); } },

            {name: "pickerGrnVal", title:this.greenFieldTitle, type: "text", 
             width: "40", defaultValue: this._pickedGrn,
             prompt: this.greenFieldPrompt,
             picker: this,
             changed: function (form,item,value) { this.picker.setGreen(value); } },

            {name: "pickerBluVal", title:this.blueFieldTitle, type: "text", 
             width: "40", defaultValue: this._pickedBlu,
             prompt: this.blueFieldPrompt,
             picker: this,
             changed: function (form,item,value) { this.picker.setBlue(value); } },

            {name: "pickerHtmlVal", title:this.htmlFieldTitle, type: "text", 
             width: "65", defaultValue: this._pickedHtml,
             prompt: this.htmlFieldPrompt,
             picker: this,
             changed: function (form,item,value) { this.picker.setHtmlColor(value); } }
        ]
                     
    });

    this._hslForm = isc.DynamicForm.create({
        autoDraw: false,
        cellPadding:1,
        padding: 10,
        width:65,
        fields: [ 
            {name: "pickerHueVal", title:this.hueFieldTitle, type: "text", 
             width: "40", defaultValue: this._pickedHue,
             prompt: this.hueFieldPrompt,
             picker: this,
             changed: function (form,item,value) { this.picker.setHue(value); } },

            {name: "pickerSatVal", title:this.satFieldTitle, type: "text", 
             width: "40", defaultValue: this._pickedSat,
             prompt: this.satFieldPrompt,
             picker: this,
             changed: function (form,item,value) { this.picker.setSaturation(value); } },

            {name: "pickerLumVal", title:this.lumFieldTitle, type: "text", 
             width: "40", defaultValue: this._pickedLum,
             prompt: this.lumFieldPrompt,
             picker: this,
             changed: function (form,item,value) { this.picker.setLuminosity(value); } }
        ]
                     
    });

    this._crossHair = isc.Img.create({
        autoDraw: false,
        imageWidth: 16, imageHeight: 16, src: this.crosshairImageURL,
        width: 16, height: 16, imageType: "normal",
        canDrag: true,
        canDrop: true,
        dragAppearance: "target",
        picker: this,
        dragMove: function () {
            this.picker._dragging = true;
            this.picker._crosshairMoved(
                this.parentElement.getOffsetX(), 
                this.parentElement.getOffsetY());
        }
    });

    this._colorBox = isc.Canvas.create({
        autoDraw: false, 
        width: 100, 
        height: 40, 
        backgroundColor: this.getHtmlColor()
    });
                
    this._opacityBox = isc.Canvas.create({
        autoDraw: false, width: 60, height: 40, 
        overflow: "hidden",
        border: "1px black solid",
        contents: this.opacityText,
        children: [
            this._colorBox
        ]
    });
                            
    this._lumVals = isc.VStack.create({
        lumWidth: 15, height: this.swatchHeight, margin: 5, border: "1px solid black"
    });

    for (var i = 0; i < this.swatchHeight/this.lumStep; i++) {
        this._lumVals.addMember(isc.Canvas.create({
            width: this.lumWidth, height: this.lumStep, 
            margin: 0, padding: 0, overflow: "hidden"
        }));
    }
    
    this._lumSlider = isc.Slider.create({
        minValue: 0,
        maxValue: 240,
        numValues: 240,
        margin: 5,
        length: this.swatchHeight,
        width: 10,
        showTitle: false,
        showValue: false,
        showRange: false
    });
    
    if (this.supportsTransparency) {
    
        this._opacitySlider = isc.Slider.create({
            autoDraw: false,
            vertical: false,
            margin: 5,
            minValue: 0,
            maxValue: 100,
            numValues: 100,
            length: 100,
            height: 12,
            width: 100,
            thumbThickWidth: 15,
            thumbThinWidth: 10,
            showTitle: false,
            showValue: false,
            showRange: false,
            value: 100
        });
        this._opacityLayout = isc.HLayout.create({
                                autoDraw: false,
                                layoutLeftMargin: 5,
                                layoutRightMargin: 5,
                                membersMargin: 5,
                                members: [
                                    isc.Label.create({
                                        autoDraw: false,
                                        margin: 5,
                                        contents: this.opacitySliderLabel, 
                                        width: this.swatchWidth - 105, height: 10}),
                                        
                                    this._opacitySlider
                                ]
                            });
    }

    this._rightHandLayout = isc.VLayout.create({
        autoDraw: false,
        layoutLeftMargin: 5,
        layoutRightMargin: 5,
        membersMargin: 5,
        members: [
            isc.HLayout.create({
                autoDraw: false,
                height: this.swatchHeight,
                members: [
                    isc.Img.create({
                        autoDraw: false,
                        margin: 5,
                        // Note: width and height have 12 added to them here, to allow 
                        // for the 5px margin and 1px border around the image
                        width: this.swatchWidth+12, height: this.swatchHeight+12, 
                        src: this.swatchImageURL,
                        overflow: "hidden",
                        border: "1px black solid",
                        picker: this,
                        click: function () {
                            this.picker._crosshairMoved(this.getOffsetX(), this.getOffsetY());
                        },
                        
                        children: [
                            this._crossHair
                        ]
                    }),
                    this._lumVals,
                    this._lumSlider
                ]
            }), 
            isc.HLayout.create({
                autoDraw: false,
                layoutLeftMargin: 5,
                layoutRightMargin: 5,
                membersMargin: 5,
                members: [
                    isc.Label.create({
                        autoDraw: false,
                        margin: 5,
                        contents: this.selectedColorLabel, 
                        width: this.swatchWidth - 63, height: 15}),
                        
                    this._opacityBox
                 ]
            })
        ]
    });
    
    if (this._lumSlider) this.observe(this._lumSlider, "valueChanged", "observer._lumSliderChanged()");
    if (this._opacitySlider) this.observe(this._opacitySlider, "valueChanged", "observer._opSliderChanged()");

},

initComplexElements : function () {
    this._lumSlider.setValue(this._pickedLum);
    this._setLumVals();
    this._positionCrossHair(this._pickedHue, this._pickedSat);
    // run through setHtmlColor to update the RGB and HTML forms
    if (this._pickedHtml) this.setHtmlColor(this._pickedHtml);
    
    this._colorBox.setBackgroundColor(
        isc.ColorUtils.hslToHtml( this._pickedHue,
                                  this._pickedSat,
                                  this._pickedLum ) );        
    if (this.supportsTransparency) {
        this._colorBox.setOpacity(this._pickedOpacity);
        this._opacitySlider.setValue(this._pickedOpacity);
    }
},

addComplexElements : function () {

    if (this._currentPickMode != 'complex') {
        return;
    }

    this.showOkButton = true;
    this.setAutoChild("okButton", {title: this.okButtonTitle});
    this.buttonLayout.setMembers([this.okButton, this.cancelButton, this.modeToggleButton]);

    this.basicLabel = isc.Label.create({
            autoDraw: false,
            margin: 5,
            contents: this.basicColorLabel, 
            width: 100, height: 15
        });
    
    this.formLayout = isc.HLayout.create({
            autoDraw: false,
            members: [
                this._rgbForm,
                this._hslForm
            ]
    });
        
    this.leftHandLayout.addMember(this.basicLabel, 0);
    this.leftHandLayout.addMember(this.formLayout);
    
    if (this.supportsTransparency) {
        this._rightHandLayout.addMember(this._opacityLayout);
    }

    this.innerContentLayout.addMember(this._rightHandLayout);

    // Initialise the complex elements
    this.initComplexElements();

},
    

removeComplexElements : function () {

    if (this._currentPickMode == 'complex') {
        return;
    }
     
    this.showOkButton = false;
    this.setAutoChild("okButton");
 
    if (this.formLayout) {   
        this.leftHandLayout.removeMembers([this.basicLabel, this.formLayout]);

        this.innerContentLayout.removeMember(this._rightHandLayout);
    }

},

//> @method colorPicker.setSupportsTransparency
// Set the +link{supportsTransparency} flag.
// @param transparencyFlag (boolean) Set to true to enable transparency/opacity
// @visibility external
//<
setSupportsTransparency : function (transparencyFlag) {
    this.supportsTransparency = transparencyFlag;
    if (this._currentPickMode == 'complex') {
        if (this.supportsTransparency) {
            this._rightHandLayout.addMember(this._opacityLayout);
        } else {
            this._rightHandLayout.removeMember(this._opacityLayout);            
        }
    }
},

//> @method colorPicker.getRed()
// Returns the Red element of the currently-selected color, as an integer from 0-255
// @see ColorPicker.setRed()
// @return (int) red color component
// @visibility external
//<
getRed : function () {
    return this._pickedRed;
},

//> @method colorPicker.getGreen()
// Returns the Green element of the currently-selected color, as an integer from 0-255
// @see ColorPicker.setGreen()
// @return (int) green color component
// @visibility external
//<
getGreen : function () {
    return this._pickedGrn;
},

//> @method colorPicker.getBlue()
// Returns the Blue element of the currently-selected color, as an integer from 0-255
// @see ColorPicker.setBlue()
// @return (int) blue color component
// @visibility external
//<
getBlue : function () {
    return this._pickedBlu;
},

//> @method colorPicker.getHue()
// Returns the Hue of the currently-selected color, as an integer from 0-239
// @see ColorPicker.setHue()
// @return (int) hue value
// @visibility external
//<
getHue : function () {
    return this._pickedHue;
},


//> @method colorPicker.getSaturation()
// Returns the Saturation of the currently-selected color, as an integer from 0-240
// @see ColorPicker.setSaturation()
// @return (int) saturation value
// @visibility external
//<
getSaturation : function () {
    return this._pickedSat;
},


//> @method colorPicker.getLuminosity()
// Returns the Luminosity (brightness) of the currently-selected color, as an 
// integer from 0-240
// @see ColorPicker.setLuminosity()
// @return (int) luminosity value
// @visibility external
//<
getLuminosity : function () {
    return this._pickedLum;
},


//> @method colorPicker.getHtmlColor()
// Returns the currently-selected color, in HTML color representation form, as a string.
// HTML color representation is a hash sign, followed by the red, green and blue elements
// of the color in 2-digit hex form - for example "#F17F1D"
// @see ColorPicker.setHtmlColor()
// @return (String) HTML color value
// @visibility external
//<
getHtmlColor : function () {
    return this._pickedHtml;
},


//> @method colorPicker.getOpacity()
// Returns the opacity of the currently-selected color, as an integer from 0-100. If 
// opacity is switched off, this is always 100.
// @return (int) opacity value
// @visibility external
//<
getOpacity : function () {
    return this._pickedOpacity;
},
    
//> @method colorPicker.setRed()
// Sets the Red element of the selected color
// @param newValue (Number) An integer between 0 and 255
// @see ColorPicker.getRed()
// @visibility external
//<
setRed : function (val) {
    if (val < 0) this._pickedRed = 0;
    else if (val > 255) this._pickedRed = 255;
    else this._pickedRed = val/1;
    
    if (this._currentPickMode == 'complex') {
        this._rgbForm.setValue("pickerRedVal", this._pickedRed);
    }
    if (this._updateColor === true)
        this._changeColor('rgb');
},

    
//> @method colorPicker.setGreen
// Sets the Green element of the selected color
// @param newValue (Number) An integer between 0 and 255
// @see ColorPicker.getGreen()
// @visibility external
//<
setGreen : function (val) {
    if (val < 0) this._pickedGrn = 0;
    else if (val > 255) this._pickedGrn = 255;
    else this._pickedGrn = val/1;
    
    if (this._currentPickMode == 'complex') {
        this._rgbForm.setValue("pickerGrnVal", this._pickedGrn);
    }
    if (this._updateColor === true)
        this._changeColor('rgb');
},
    
//> @method colorPicker.setBlue()
// Sets the Blue element of the selected color
// @param newValue (Number) An integer between 0 and 255
// @see ColorPicker.getBlue()
// @visibility external
//<
setBlue : function (val) {
    if (val < 0) this._pickedBlu = 0;
    else if (val > 255) this._pickedBlu = 255;
    else this._pickedBlu = val/1;
    
    if (this._currentPickMode == 'complex') {
        this._rgbForm.setValue("pickerBluVal", this._pickedBlu);
    }
    if (this._updateColor === true)
        this._changeColor('rgb');
},
            
//> @method colorPicker.setHue()
// Sets the Hue of the selected color
// @param newValue (Number) An integer between 0 and 239
// @see ColorPicker.getHue()
// @visibility external
//<
setHue : function (val) {
    if (val < 0) this._pickedHue = 0;
    else if (val > 239) this._pickedHue = 239;
    else this._pickedHue = val/1;
    
    if (this._currentPickMode == 'complex') {
        this._hslForm.setValue("pickerHueVal", this._pickedHue);
    }
    if (this._updateColor === true)
        this._changeColor('hsl');
},
    
//> @method colorPicker.setSaturation()
// Sets the Saturation of the selected color
// @param newValue (Number) An integer between 0 and 240
// @see ColorPicker.getSaturation()
// @visibility external
//<
setSaturation : function (val) {
    if (val < 0) this._pickedSat = 0;
    else if (val > 240) this._pickedSat = 240;
    else this._pickedSat = val/1;
    
    if (this._currentPickMode == 'complex') {
        this._hslForm.setValue("pickerSatVal", this._pickedSat);
    }
    if (this._updateColor === true) {
        this._changeColor('hsl');
    }
},
    
//> @method colorPicker.setLuminosity()
// Sets the Luminosity (brightness) of the selected color
// @param newValue (Number) An integer between 0 and 240
// @see ColorPicker.getLuminosity()
// @visibility external
//<
// additional dontPersist flag: If passed, we reset the luminosity to 50% when
// the user picks a new color via the swatch
// This is useful for the case where the user picks an HTML color value which
// includes luminosity implicitly.
// In this case if the user then picks from the swatch we don't want to retain this
// luminosity setting. This is most obvious if the user picks black (luminosity zero)
// - if we hung onto that setting it'd make subsequent choices in the swatch have no
// effect on the resultant color (still black!)
setLuminosity : function (val, dontPersist) {
    if (val < 0) this._pickedLum = 0;
    else if (val > 240) this._pickedLum = 240;
    else this._pickedLum = val/1;
    
    this._persistLum = !dontPersist;
    
    if (this._currentPickMode == 'complex') {
        this._hslForm.setValue("pickerLumVal", this._pickedLum);
    }
    if (this._updateColor === true)
        this._changeColor('hsl');
},
    
//> @method colorPicker.setHtmlColor()
// Changes the selected color to the one represented by the supplied HTML color 
// string. Note that ths method only accepts the parameter if it represents a 
// valid color (otherwise it is simply ignored).
// @param newValue (text) A string in HTML color representation format (#RRGGBB)
// @see ColorPicker.getHtmlColor()
// @visibility external
//<
setHtmlColor : function (val) {
    if (isc.ColorUtils.encodingIsValid(val) === true) {
        this._pickedHtml = val.toUpperCase();
        if (this._currentPickMode == 'complex') {
            this._rgbForm.setValue("pickerHtmlVal", this._pickedHtml);
        }
        if (this._updateColor === true)
            this._changeColor('html');
    }
},
    
    
//> @method colorPicker.setOpacity()
// Sets the Opacity of the selected color. Ignored if opacity is switched off.
// @param newValue (Number) An integer between 0 and 100
// @see ColorPicker.getOpacity()
// @visibility external
//<
setOpacity : function (val) {
    if (this._currentPickMode == 'complex' && this.supportsTransparency) {
        if (val < 0) this._pickedOpacity = 0;
        else if (val > 100) this._pickedOpacity = 100;
        else this._pickedOpacity = val/1;
        
        if (this._updateColor === true) {
            this._changeColor('opacity');
        }
    }
},

_changeColor : function (src) {
    
    if (src == 'rgb') {
        
        var hsl = isc.ColorUtils.rgbToHsl( this._pickedRed,
                                        this._pickedGrn,
                                        this._pickedBlu );
        this._updateColor = false;
        this.setHue(hsl.h);
        this.setSaturation(hsl.s);
        // pass in the param to forget the luminosity if the user changes positions in
        // the swatch
        this.setLuminosity(hsl.l, true);
        this.setHtmlColor(isc.ColorUtils.rgbToHtml(this._pickedRed,
                                                   this._pickedGrn,
                                                   this._pickedBlu));
        this._updateColor = true;
        // Move the crosshair 
        this._positionCrossHair( this._pickedHue,
                                 this._pickedSat);
    } else if (src == 'hsl') {
        if (!this._persistLum) this._pickedLum = 120;
        var rgb = isc.ColorUtils.hslToRgb( this._pickedHue,
                                           this._pickedSat,
                                           this._pickedLum );
        this._updateColor = false;
        this.setRed(rgb.r);
        this.setGreen(rgb.g);
        this.setBlue(rgb.b);
        this.setHtmlColor(isc.ColorUtils.rgbToHtml(this._pickedRed,
                                                   this._pickedGrn,
                                                   this._pickedBlu));
        this._updateColor = true;
        // Move the crosshair, if necessary (if only the Luminosity
        // has changed, the crosshair does not need to move. If we move 
        // it anyway, precision loss issues might cause it to choose
        // a pixel in the color swatch adjacent to the one we currently
        // have, rather than that exact pixel.  This is a problem if the
        // user is changing Lum with the slider - as they smoothly slide 
        // the value up and down, we get a very annoying "wobbly cursor"
        // effect...)
        if (this._pickedHue != this._savHue || this._pickedSat != this._savSat) {
            this._positionCrossHair( this._pickedHue,
                                      this._pickedSat);
        }
    } else if (src == 'html') {  
                                    
        var rgb = isc.ColorUtils.htmlToRgb( this._pickedHtml );

        this._updateColor = false;
        this.setRed(rgb.r);
        this.setGreen(rgb.g);
        this.setBlue(rgb.b);

        var hsl = isc.ColorUtils.rgbToHsl(  this._pickedRed,
                                        this._pickedGrn,
                                        this._pickedBlu );
        this.setHue(hsl.h);
        this.setSaturation(hsl.s);
        this.setLuminosity(hsl.l, true);
        this._updateColor = true;
        // Move the crosshair 
        this._positionCrossHair( this._pickedHue,
                                 this._pickedSat);
    }
    

    // and the slider value
    if (this._currentPickMode == 'complex') {
        this._lumSlider.setValue(this._pickedLum);
        this._hslForm.setValue("pickerLumVal", this._pickedLum);
    }

    // Now set the color box - use HSL, though we could use either as they
    // are now the same...
    if (this._currentPickMode == 'complex') {
        this._colorBox.setBackgroundColor(
                            isc.ColorUtils.hslToHtml( this._pickedHue,
                                                      this._pickedSat,
                                                      this._pickedLum ) );        
    }

    // Change the luminosity bar (for performance reasons, only do this
    // if the Hue or Sat have changed)
    if (this._pickedHue != this._savHue || this._pickedSat != this._savSat) {
        this._setLumVals();
    }
    
    // Set the opacity
    if (this._currentPickMode == 'complex' && this.supportsTransparency) {
        this._colorBox.setOpacity(this._pickedOpacity);
        if (this._opacitySlider != null) {
            var slider = this._opacitySlider,
                sliderVal = slider.getValue(),
                newSliderVal = this._pickedOpacity;
            if (newSliderVal === null) newSliderVal = 100;
            if (sliderVal != newSliderVal) this._opacitySlider.setValue(this._pickedOpacity);
        }
    }
    
    // Save the existing Hue and Saturation - we only want to reposition the 
    // crosshair if either actually changes, otherwise we get an annoying 
    // wobbling effect when we move the Luminosity slider
    this._savHue = this._pickedHue;
    this._savSat = this._pickedSat;
    
    if (this.colorChanged) this.colorChanged();
},

// Called when a color box is clicked in simple mode
_oneClickColorSelected : function (color) {
    this.hide();
    if (this.colorSelected) this.colorSelected(color);
},

_positionCrossHair : function (hue, sat) {

    if (this._currentPickMode != 'complex') {
        return;
    }
    

    if (this._dragging === true) {
        this._dragging = false;
        return;
    }
    
    var ph = hue / 239.0;
    var ps = sat / 240.0;
    
    ph *= this.swatchWidth;
    ps = this.swatchHeight - (ps * this.swatchHeight); 
    
    ph = parseInt(ph) - 8;
    ps = parseInt(ps) - 8;
            
    this._crossHair.setLeft(ph);
    this._crossHair.setTop(ps);
    
},

_crosshairMoved : function (h, s) {

    h -= 5;  // Account for margin and border. Note that I'm using 5 rather than
    s -= 5;  // 6 here because it gives a more accurate result - maybe SmartClient
             // doesn't include the border...?
    
    h /= this.swatchWidth;
    s = 1.0 - s/this.swatchHeight;
    
    this._updateColor = false;  // Just to stop it updating the screen twice
    this.setHue(Math.floor(h * 239.0 + 0.5));
    this._updateColor = true;
    this.setSaturation(Math.floor(s * 240.0 + 0.5));
},

_setLumVals : function () {
    if (this._currentPickMode != 'complex') {
        return;
    }
    
    for (var i = 0; i < this.swatchHeight/this.lumStep; i++) {
        this._lumVals.members[i].setBackgroundColor(
                        isc.ColorUtils.hslToHtml(
                               this._pickedHue,
                               this._pickedSat,
                               240-(i * 240/(this.swatchHeight/this.lumStep))
                            ));
    }
    
}, 

_lumSliderChanged : function () {
    var wk = this._lumSlider.getValue();
    if (this._pickedLum != wk) {
        this.setLuminosity(wk);
    }
},

_opSliderChanged : function () {
    this.setOpacity(this._opacitySlider.getValue());
},

//> @method colorPicker.setCurrentPickMode()
// Changes the pick mode of this <code>ColorPicker</code> to <code>pickMode</code>.
// <p>
// Note: It is not allowed to set the pick mode to
// <smartclient>"complex"</smartclient>
// <smartgwt>{@link com.smartgwt.client.types.ColorPickerMode#COMPLEX}</smartgwt>
// if +link{ColorPicker.allowComplexMode,allowComplexMode} is <code>false</code>.
// @param pickMode (ColorPickerMode) the new pick mode.
// @visibility external
//<
setCurrentPickMode : function (pickMode) {
    if (this._currentPickMode == pickMode) return;

    if (pickMode == "simple" || !this.allowComplexMode) {
        this._currentPickMode = "simple";
        this.removeComplexElements();
        if (this.allowComplexMode) {
            this.modeToggleButton.setTitle(this.moreButtonTitle);
        }
    } else {
        this._currentPickMode = "complex";
        if (! this._rightHandLayout) {
            this.createComplexElements();
        }
        this.addComplexElements();
        this.modeToggleButton.setTitle(this.lessButtonTitle);
    }
    this.modeToggleButton.setState("");
},

_togglePickMode : function () {
    this.setCurrentPickMode(this._currentPickMode == "simple" ? "complex" : "simple");
}
}); 


isc.ColorPicker.registerStringMethods({
    
    //> @method colorPicker.colorChanged
    // Override this method to be kept informed when the ColorPicker changes in real-time 
    // (for example, if you need to update your own GUI accordingly). Then use the 
    // getXxxx() methods (for example, +link{getBlue,getBlue()} or 
    // +link{getLuminosity,getLuminosity()})to obtain current state as required. 
    // @see ColorPicker.colorSelected()
    // @visibility external
    //<
    colorChanged : "",
    
    //> @method colorPicker.colorSelected
    // Override this method to be notified when the user selects a color
    // either by clicking a basic color box in simple mode, or by clicking 
    // the OK button in complex mode. It is not intended that client code 
    // call this method.
    // @param color (String)    The color selected, in HTML format
    // @param opacity (Number)  The opacity selected, from 0-100.
    // @see ColorPicker.colorChanged()
    // @visibility external
    //<
    colorSelected : "color,opacity"
});    


/*-----------------------------------------------------------------------------------*/

// The ColorUtils class contains class methods that are generally useful when you
// are working with colors - for example, conversion routines to convert between
// HTML, RGB and HSL color formats.
//
// Not documenting these for now - probably don't have much general-purpose 
// usefulness

isc.ClassFactory.defineClass("ColorUtils", isc.Class).addClassMethods({

//*******************************************************
//  hexToDec
//  Returns the decimal equivalent of the passed-in hex string
//*******************************************************
hexToDec : function (hex) {
    return parseInt(hex, 16);        
},

//*******************************************************
//  decToHex
//  Returns the hexadecimal equivalent of the passed-in decimal number
//*******************************************************
decToHex : function (dec) {
    var d = dec/1;
    var h = d.toString(16);
    if (h.length == 1) {
        h = "0" + h;
    }
    return h;
},

//*******************************************************
//  brightness
//  Returns the brightness (luminosity) of the supplied RGB values
//*******************************************************
brightness : function (r, g, b) {
    var hsl = isc.ColorUtils.rgbToHsl(r, g, b);
    return (hsl.l / 240.0);
},

//*******************************************************
//  encodingIsValid
//  Returns true if the supplied string is a valid HTML color
//*******************************************************
encodingIsValid : function (html) {
    return (html.substring(0, 1) == '#' && isc.isA.color(html));
},


//*******************************************************
//  rgbToHtml
//  Converts an RGB triplicate to an HTML color string
//*******************************************************
rgbToHtml : function (r, g, b) {
    var htmlCol = '#' + isc.ColorUtils.decToHex(r) + 
                        isc.ColorUtils.decToHex(g) + 
                        isc.ColorUtils.decToHex(b);
    return htmlCol;
},

//*******************************************************
//  hslToHtml
//  Converts an HSL triplicate to an HTML color string
//*******************************************************
hslToHtml : function (h, s, l) {
    var rgb = isc.ColorUtils.hslToRgb(h, s, l);
    var htmlCol = '#' + isc.ColorUtils.decToHex(rgb.r) + 
                        isc.ColorUtils.decToHex(rgb.g) + 
                        isc.ColorUtils.decToHex(rgb.b);
    return htmlCol;
},

//*******************************************************
//  htmlToRgb
//  Converts an HTML color string to an RGB triplicate 
//*******************************************************
htmlToRgb : function (htmlString) {
    var r = htmlString.substring(1, 3);
    var g = htmlString.substring(3, 5);
    var b = htmlString.substring(5, 7);
    return {
        r: isc.ColorUtils.hexToDec(r),
        g: isc.ColorUtils.hexToDec(g),
        b: isc.ColorUtils.hexToDec(b)
    };
},

//*******************************************************
//  htmlToHsl
//  Converts an HTML color string to an HSL triplicate 
//*******************************************************
htmlToHsl : function (htmlString) {
    var r = htmlString.substring(1, 3);
    var g = htmlString.substring(3, 5);
    var b = htmlString.substring(5, 7);
    return isc.ColorUtils.rgbToHsl( isc.ColorUtils.hexToDec(r),
                                    isc.ColorUtils.hexToDec(g),
                                    isc.ColorUtils.hexToDec(b) );
},

//*******************************************************
//  rgbToHsl
//  Converts an RGB triplicate to an HSL triplicate
//*******************************************************
rgbToHsl : function (r, g, b) {
    var wr = r / 255.0;
    var wg = g / 255.0;
    var wb = b / 255.0;
    
    var min  = Math.min(Math.min(wr, wg), wb);
    var max  = Math.max(Math.max(wr, wg), wb);
    var delta = max - min;
    
    var h = 0, s = 0, l = 0;
    
    l = (max + min) / 2.0;
    
    if (max == min) {   // ie, a grey shade
        s = 0;
        h = 0;
    } else {
        if (l < 0.5) {
            s = (max - min) / (max + min);
        } else {
            s = (max - min) / (2.0 - max - min)
        }
        
        
        // Calculate hue
        if ( wr == max )
            h = ( wg - wb ) / delta;    // between yellow & magenta
        else if( wg == max )
            h = 2 + ( wb - wr ) / delta;    // between cyan & yellow
        else
            h = 4 + ( wr - wg ) / delta;    // between magenta & cyan                
    }

    // Scale the results        
    h = Math.floor(h * 40 + 0.5);
    if (h < 0) h += 240;
    s = Math.floor(s * 240 + 0.5);
    l = Math.floor(l * 240 + 0.5);
    
    return { h: h, s: s, l: l};
},
    
//*******************************************************
//  hslToRgb
//  Converts an HSL triplicate to an RGB triplicate
//  Note that, in keeping with Microsoft's color picking tool,
//  we expect the HSL values to be between 0 and 240 (239 for
//  Hue, because of the way the maths works out).
//*******************************************************
hslToRgb : function (h, s, l) {

    var wh = h / 239.0;
    var ws = s / 240.0;
    var wl = l / 240.0;
    var t1, t2, tr3, tg3, tb3;
    
    var r = 0, g = 0, b = 0;
    
    if (ws == 0) {   // ie, a completely neutral grey shade
        r = wl;
        g = wl;
        b = wl;
    } else {
        
        if (wl < 0.5) {
            t2 = wl * (1.0 + ws);
        } else {
            t2 = (wl + ws) - (wl * ws);
        }
        
        t1 = (2.0 * wl) - t2;
        
        tr3 = wh + 0.3333;
        tg3 = wh;
        tb3 = wh - 0.3333;
        
        if (tr3 < 0) tr3 += 1.0;
        if (tg3 < 0) tg3 += 1.0;
        if (tb3 < 0) tb3 += 1.0;

        if (tr3 > 1) tr3 -= 1.0;
        if (tg3 > 1) tg3 -= 1.0;
        if (tb3 > 1) tb3 -= 1.0;
        
        if (tr3 * 6.0 < 1) 
            r = t1 + (t2-t1) * 6.0 * tr3;
        else if (tr3 * 2.0 < 1)
            r = t2;
        else if (tr3 * 3.0  < 2)
            r = t1 + (t2-t1) * (0.6667 - tr3) * 6.0;
        else 
            r = t1;
        
        if (tg3 * 6.0 < 1) 
            g = t1 + (t2-t1) * 6.0 * tg3;
        else if (tg3 * 2.0 < 1)
            g = t2;
        else if (tg3 * 3.0  < 2)
            g = t1 + (t2-t1) * (0.6667 - tg3) * 6.0;
        else 
            g = t1;
        
        if (tb3 * 6.0 < 1) 
            b = t1 + (t2-t1) * 6.0 * tb3;
        else if (tb3 * 2.0 < 1)
            b = t2;
        else if (tb3 * 3.0  < 2)
            b = t1 + (t2-t1) * (0.6667 - tb3) * 6.0;
        else 
            b = t1;
    }        
    
    // And scale...
    r = Math.floor(r * 255.0 + 0.5);
    g = Math.floor(g * 255.0 + 0.5);
    b = Math.floor(b * 255.0 + 0.5);
    
    return { r: r, g: g, b: b};
}    
});

}
