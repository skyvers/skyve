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
//>	@class	StatefulCanvas
// A component that has a set of possible states, and which presents itself differently according to
// which state it is in.  An example is a button, which can be "up", "down", "over" or "disabled".
// 
// @treeLocation Client Reference/Foundation
// @visibility external
//<
isc.ClassFactory.defineClass("StatefulCanvas", "Canvas");

	//>	@groupDef	state
	// Change of state and it's consequences for presentation.
	//<
isc.StatefulCanvas.addClassProperties({

	//>	@type	State
    // Constants for the standard states for a StatefulCanvas.  
	//			@group	state
	STATE_UP:"",				//	@value	StatefulCanvas.STATE_UP         state when mouse is not acting on this StatefulCanvas
	STATE_DOWN:"Down",			//	@value	StatefulCanvas.STATE_DOWN       state when mouse is down
    STATE_OVER:"Over",			//	@value	StatefulCanvas.STATE_OVER		state when mouse is over
    STATE_DISABLED:"Disabled",	//	@value	StatefulCanvas.STATE_DISABLED	disabled
    // @visibility external
	//<

	//>	@type	SelectionType	
    // Controls how an object changes state when clicked
	// @group	state
	// @group	event handling
    //	@value	StatefulCanvas.BUTTON	object moves to "down" state temporarily (normal button) 
    //	@value	StatefulCanvas.CHECKBOX object remains in "down" state until clicked again (checkbox)
    //	@value	StatefulCanvas.RADIO	object moves to "down" state, causing another object to go up (radio)
	BUTTON:"button",	
	CHECKBOX:"checkbox",
	RADIO:"radio",		
    // @visibility external
	//<	

	//>	@type	Selected
	//			@group	state
    FOCUSED:"Focused",          //  @value  StatefulCanvas.FOCUSED  StatefulCanvas should show
                                // focused state  
	SELECTED:"Selected",		//	@value	StatefulCanvas.SELECTED		StatefulCanvas is selected
	UNSELECTED:"",				//	@value	StatefulCanvas.UNSELECTED   StatefulCanvas is not selected
	// @visibility external
	//<	

    // Internal map of radioGroup ID's to arrays of widgets
    _radioGroups:{},

    _mirroredAlign: {
        "left": "right",
        "center": "center",
        "right": "left"
    },

    // ------------------------
    // Properties for manipulating CSS border
    
    _borderStyleCache: {},
    _borderCSSHTMLCache: {},

    _borderProperties : [ 
        "borderBottomLeftRadius",
        "borderBottomRightRadius",
        "borderTopRightRadius",
        "borderTopLeftRadius",
        "borderBottomColor", 
        "borderBottomStyle", 
        "borderBottomWidth", 
        "borderLeftColor", 
        "borderLeftStyle", 
        "borderLeftWidth", 
        "borderRightColor", 
        "borderRightStyle", 
        "borderRightWidth", 
        "borderTopColor", 
        "borderTopStyle", 
        "borderTopWidth" 
    ],
    _nRadiusBorderProperties: 4,

    _$separator: " ",

    //> @classAttr statefulCanvas.pushTableBorderStyleToDiv (boolean : ? : IR)
    // Causes border properties to be written onto containing DIV rather than
    // be applied to the internal Table TDs for Button widgets
    //<                                          
    pushTableBorderStyleToDiv: false,

    _shadowStyleCache: {},
    _shadowStyleCSSHTMLCache: {},
    
    pushTableShadowStyleToDiv: true

});


isc.StatefulCanvas.addProperties({

    //> @attr statefulCanvas.title (HTMLString : null : IRW)
    // The title HTML to display in this button.
    // @group basics
    // @visibility external
    //<

    //>@attr StatefulCanvas.hiliteAccessKey (boolean : null : [IRWA])
    // If set to true, if the +link{statefulCanvas.title, title} of this button contains the
    // specified +link{canvas.accessKey, accessKey}, when the title is displayed to the user
    // it will be modified to include HTML to underline the accessKey.<br>
    // Note that this property may cause titles that include HTML (rather than simple strings)
    // to be inappropriately modified, so should be disabled if your title string includes
    // HTML characters.
    // @visibility internal
    //<

    //> @attr statefulCanvas.ignoreRTL (boolean : false : IRWA)
    // Should horizontal alignment-related attributes +link{align,align} and +link{iconOrientation,iconOrientation}
    // be mirrored in RTL mode? This is the default behavior unless ignoreRTL is set to true.
    // @setter setIgnoreRTL()
    // @group RTL
    // @visibility external
    //<
    // iconAlign is also mirrored.
    ignoreRTL: false,

    // State-related properties
    // -----------------------------------------------------------------------------------------------

    //>	@attr	statefulCanvas.redrawOnStateChange		(Boolean : false : IRWA)
	// Whether this widget needs to redraw to reflect state change
	// @group	state
    // @visibility external
	//<

    //>	@attr	statefulCanvas.selected		(Boolean : false : IRW)
	// Whether this component is selected.  For some components, selection affects appearance.
	// @group	state
    // @visibility external
	//<

    //>	@attr	statefulCanvas.state		(State : "" : IRWA)
	// Current "state" of this widget. The state setting is automatically updated as the 
	// user interacts with the component (see +link{statefulCanvas.showRollOver}, 
	// +link{statefulCanvas.showDown}, +link{statefulCanvas.showDisabled}).
	// <P>
    // StatefulCanvases will have a different appearance based
    // on their current state. 
    // By default this is handled by changing the css className applied to
    // the StatefulCanvas - see +link{StatefulCanvas.baseStyle} and 
    // +link{StatefulCanvas.getStateSuffix()} for a description of how this is done.
    // <P>
    // For +link{class:Img} or +link{class:StretchImg} based subclasses of StatefulCanvas, the 
    // appearance may also be updated by changing the src of the rendered image. See
    // +link{Img.src} and +link{StretchImgButton.src} for a description of how the URL 
    // is modified to reflect the state of the widget in this case.
    // 
    // @see type:State
    // @see group:state
	// @group	state
    // @visibility external
	//<
	
	state:"",

    //>	@attr	statefulCanvas.showRollOver		(Boolean : false : IRW)
	// Should we visibly change state when the mouse goes over this object?
	// @group	state
    // @visibility external
	//<	

    //>	@attr	statefulCanvas.showFocus        (Boolean : false : IRW)
	// Should we visibly change state when the canvas receives focus?  Note that by default the
    // <code>over</code> state is used to indicate focus.
	// @group	state
    // @deprecated as of SmartClient version 6.1 in favor of +link{statefulCanvas.showFocused}
    // @visibility external
	//<	

    //>	@attr	statefulCanvas.showFocused        (Boolean : false : IRW)
	// Should we visibly change state when the canvas receives focus?  If
    // +link{statefulCanvas.showFocusedAsOver} is <code>true</code>, then <b><code>"over"</code></b>
    // will be used to indicate focus. Otherwise a separate <b><code>"focused"</code></b> state
    // will be used.
	// @group	state
    // @visibility external
	//<

    //> @attr statefulCanvas.showFocusedAsOver (Boolean : true : IRW) 
    // If +link{StatefulCanvas.showFocused,showFocused} is true for this widget, should the
    // <code>"over"</code> state be used to indicate the widget as focused. If set to false,
    // a separate <code>"focused"</code> state will be used.
    // @group state
    // @visibility external
    //<
    showFocusedAsOver: true,
    
    
    
	//>	@attr	statefulCanvas.showDown		(Boolean : false : IRW)
	// Should we visibly change state when the mouse goes down in this object?
	//		@group	state
    // @visibility external
	//<	

	//>	@attr	statefulCanvas.showDisabled  (Boolean : true : IRW)
	// Should we visibly change state when disabled?
	//		@group	state
    // @visibility external
	//<	
    showDisabled:true,

	//>	@attr	statefulCanvas.actionType		(SelectionType : "button": IRW)
	// Behavior on state changes -- BUTTON, RADIO or CHECKBOX
	//		@group	state
	//		@group	event handling
    //      @setter setActionType()
    //      @getter getActionType()
    // @visibility external
	//<
    actionType:"button",
    
	//>	@attr	statefulCanvas.radioGroup   (string : null : IRWA)
	// String identifier for this canvas's mutually exclusive selection group.
	//		@group	state
	//		@group	event handling
    // @visibility external
	//<

	//>	@attr	statefulCanvas.baseStyle		(CSSStyleName : null : IRW)
	// Base CSS style className applied to the component. 
    // <P>
	// As the component changes +link{statefulCanvas.state} and/or is selected, 
	// suffixes will be added to the base style. In some cases more than one suffix will 
	// be appended to reflect a combined state ("Selected" + "Disabled", for example).
    // <P>
    // See +link{statefulCanvas.getStateSuffix()} for a description of the default set
    // of suffixes which may be applied to the baseStyle
    // 
    // @visibility external
	//<							
	
	
	//>	@attr	statefulCanvas.cursor		(Cursor : Canvas.ARROW : IRW)
	//			Specifies the cursor to show when over this canvas.
	//			See Cursor type for different cursors.
	//		@group	cues
	//		@platformNotes	Nav4	Cursor changes are not available in Nav4
	//<
	cursor:isc.Canvas.ARROW,

    //> @attr statefulCanvas.iconCursor (Cursor : null : IR)
    // Specifies the cursor to display when the mouse pointer is over the icon image.
    //
    // @group cues
    // @see attr:disabledIconCursor
    //<
    //iconCursor: null,

    //> @attr statefulCanvas.disabledIconCursor (Cursor : null : IR)
    // Specifies the cursor to display when the mouse pointer is over the icon image and this
    // <code>StatefulCanvas</code> is +link{Canvas.disabled,disabled}.
    // <p>
    // If not set and the mouse pointer is over the icon image, +link{iconCursor,iconCursor}
    // will be used.
    //
    // @group cues
    //<
    //disabledIconCursor: null,

    // Image-based subclasses
    // ---------------------------------------------------------------------------------------
    capSize:0,

    //> @attr statefulCanvas.showTitle (boolean : false : [IRWA])
    // Determines whether any specified +link{statefulCanvas.getTitle(), title} will be 
    // displayed for this component.<br>
    // Applies to Image-based components only, where the title will be rendered out in a label
    // floating over the component
    // @visibility internal
    //<
    // Really governs whether a label canvas is created to contain the title.
    // Exposed on img based subclasses only as some statefulCanvas subclasses will support
    // displaying the title without a label canvas
    //showTitle:false,

    //>	@attr	statefulCanvas.align		(Alignment : isc.Canvas.CENTER : [IRW])
    // Horizontal alignment of this component's title.
    // @group appearance
    // @visibility external
    //<
	align:isc.Canvas.CENTER,

    //>	@attr	statefulCanvas.valign		(VerticalAlignment : isc.Canvas.CENTER : [IRW])
    // Vertical alignment of this component's title.
    // @group appearance
    // @visibility external
    //<
	valign:isc.Canvas.CENTER,

    //> @attr StatefulCanvas.autoFit  (boolean : null : IRW)
    // If true, ignore the specified size of this widget and always size just large
    // enough to accommodate the title.  If <code>setWidth()</code> is explicitly called on an
    // autoFit:true button, autoFit will be reset to <code>false</code>.
    // <P>
    // Note that for StretchImgButton instances, autoFit will occur horizontally only, as 
    // unpredictable vertical sizing is likely to distort the media. If you do want vertical 
    // auto-fit, this can be achieved by simply setting a small height, and having 
    // overflow:"visible"
    // @setter setAutoFit()
    // @group sizing
    // @visibility external
    //<
    //autoFit:null

    // autoFitDirection: Undocumented property determining whether we should auto-fit
    // horizontally, vertically or in both directions
    // Options are "both", "horizontal", "vertical"
    autoFitDirection:isc.Canvas.BOTH,

    //        
    // Button properties - managed here and @included from Button, ImgButton and
    // StatefulImgButton
	// =================================================================================

    // Icon (optional)
	// ---------------

    //> @attr statefulCanvas.icon           (SCImgURL : null : [IRW])
    // Optional icon to be shown with the button title text.  
    // <P>
    // Specify as the partial URL to an image, relative to the imgDir of this component.
    // @group buttonIcon
    // @visibility external
    //<

    //> @attr statefulCanvas.iconSize (int : 16 : IR)
    // Size in pixels of the icon image.
    // <P>
    // The +link{StatefulCanvas.iconWidth,iconWidth} and +link{StatefulCanvas.iconHeight,iconHeight}
    // properties can be used to configure width and height separately.
    // <P>
    // Note: When configuring the properties of a <code>StatefulCanvas</code> (or derivative)
    // +link{AutoChild,AutoChild}, it is best to set the <code>iconWidth</code> and <code>iconHeight</code>
    // to the same value rather than setting an <code>iconSize</code>. This is because certain
    // skins or customizations thereto might set the <code>iconWidth</code> and <code>iconHeight</code>,
    // making the customization of the AutoChild's <code>iconSize</code> ineffective.
    //
    // @group buttonIcon
    // @visibility external
    //<
    iconSize:16,

    //> @attr statefulCanvas.iconWidth (integer : null : IR)
    // Width in pixels of the icon image.
    // <P>
    // If unset, defaults to +link{StatefulCanvas.iconSize,iconSize}.
    //
    // @group buttonIcon
    // @visibility external
    //<

    //> @attr statefulCanvas.iconHeight (integer : null : IR)
    // Height in pixels of the icon image.
    // <P>
    // If unset, defaults to +link{StatefulCanvas.iconSize,iconSize}.
    //
    // @group buttonIcon
    // @visibility external
    //<

    //> @attr statefulCanvas.iconStyle (CSSStyleName : null : IRW)
    // Base CSS style applied to the icon image. If set, as the <code>StatefulCanvas</code> changes
    // +link{StatefulCanvas.state,state} and/or is +link{StatefulCanvas.selected,selected},
    // suffixes will be appended to <code>iconStyle</code> to form the className set on the
    // image element.
    // <p>
    // The following table lists out the standard set of suffixes which may be appended:
    // <table border=1>
    // <tr><th>CSS Class Applied</th><th>Description</th></tr>
    // <tr><td><code><i>iconStyle</i></code></td><td>Default CSS style</td></tr>
    // <tr><td><code><i>iconStyle</i>+Selected</code></td>
    //      <td>Applied when +link{StatefulCanvas.selected} and +link{StatefulCanvas.showSelectedIcon}
    //      are true.</td></tr>
    // <tr><td><code><i>iconStyle</i>+Focused</code></td>
    //      <td>Applied when the component has keyboard focus, if
    //      +link{StatefulCanvas.showFocusedIcon} is true, and
    //      +link{StatefulCanvas.showFocusedAsOver} is not true.</td></tr>
    // <tr><td><code><i>iconStyle</i>+Over</code></td>
    //      <td>Applied when +link{StatefulCanvas.showRollOverIcon} is set to true and either
    //      the user rolls over the component or +link{StatefulCanvas.showFocusedAsOver} is true
    //      and the component has keyboard focus.</td></tr>
    // <tr><td><code><i>iconStyle</i>+Down</code></td>
    //      <td>Applied when the user presses the mouse button on the component if
    //          +link{StatefulCanvas.showDownIcon} is set to true</td></tr>
    // <tr><td><code><i>iconStyle</i>+Disabled</code></td>
    //      <td>Applied when the component is +link{Canvas.disabled,disabled}
    //       if +link{statefulCanvas.showDisabledIcon} is true.</td></tr>
    // <tr><td colspan=2><i>Combined styles</i></td></tr>
    // <tr><td><code><i>iconStyle</i>+SelectedFocused</code></td>
    //      <td>Combined Selected and focused styling</td></tr>
    // <tr><td><code><i>iconStyle</i>+SelectedOver</code></td>
    //      <td>Combined Selected and rollOver styling</td></tr>
    // <tr><td><code><i>iconStyle</i>+FocusedOver</code></td>
    //      <td>Combined Focused and rollOver styling</td></tr>
    // <tr><td><code><i>iconStyle</i>+SelectedFocusedOver</code></td>
    //      <td>Combined Selected, Focused and rollOver styling</td></tr>
    // <tr><td><code><i>iconStyle</i>+SelectedDown</code></td>
    //      <td>Combined Selected and mouse-down styling</td></tr>
    // <tr><td><code><i>iconStyle</i>+FocusedDown</code></td>
    //      <td>Combined Focused and mouse-down styling</td></tr>
    // <tr><td><code><i>iconStyle</i>+SelectedFocusedDown</code></td>
    //      <td>Combined Selected, Focused and mouse-down styling</td></tr>
    // <tr><td><code><i>iconStyle</i>+SelectedDisabled</code></td>
    //      <td>Combined Selected and Disabled styling</td></tr>
    // </table>
    // <p>
    // In addition, if +link{StatefulCanvas.showRTLIcon} is true, then in RTL mode, a final
    // "RTL" suffix will be appended.
    // @setter setIconStyle()
    // @group buttonIcon
    // @visibility external
    //<

    //> @attr statefulCanvas.iconOrientation     (string : "left" : [IR])
    // If this button is showing an icon should it appear to the left or right of the title?
    // valid options are <code>"left"</code> and <code>"right"</code>.
    //
    // @group buttonIcon
    // @visibility external
    //<
    iconOrientation:"left",

    //> @attr statefulCanvas.iconAlign     (string : null : [IR])
    // If this button is showing an icon should it be right or left aligned?
    //
    // @group buttonIcon
    // @visibility internal
    //<
    // Behavior is as follows - if iconOrientation and iconAlign are both left or both right we
    // write the icon out at the extreme right or left of the button, and allow the title to 
    // aligned independently of it. (otherwise the icon and the text will be adjacent, and 
    // aligned together based on the button's "align" property.
    

    //> @attr statefulCanvas.iconSpacing   (int : 6 : [IR])
    // Pixels between icon and title text.
    //
    // @group buttonIcon
    // @visibility external
    //<
    iconSpacing:6,

    // internal: controls whether we apply any state to the icon at all 
    showIconState: true,

    //> @attr statefulCanvas.showDisabledIcon   (Boolean : true : [IR])
    // If using an icon for this button, whether to switch the icon image if the button becomes
    // disabled.
    //
    // @group buttonIcon
    // @visibility external
    //<
    showDisabledIcon:true,

    //> @attr statefulCanvas.showRollOverIcon   (Boolean : false : [IR])
    // If using an icon for this button, whether to switch the icon image on mouse rollover.
    //
    // @group buttonIcon
    // @visibility external
    //<

    //> @attr statefulCanvas.showDownIcon       (Boolean : false : [IR])
    // If using an icon for this button, whether to switch the icon image when the mouse goes
    // down on the button.
    //
    // @group buttonIcon
    // @visibility external
    //<

    //> @attr statefulCanvas.showSelectedIcon   (Boolean : false : [IR])
    // If using an icon for this button, whether to switch the icon image when the button
    // becomes selected.
    //
    // @group buttonIcon
    // @visibility external
    //<

    //> @attr StatefulCanvas.showFocusedIcon (Boolean : false : [IR])
    // If using an icon for this button, whether to switch the icon image when the button
    // receives focus.
    // <P>
    // If +link{statefulCanvas.showFocusedAsOver} is true, the <code>"Over"</code> icon will be
    // displayed when the canvas has focus, otherwise a separate <code>"Focused"</code> icon
    // will be displayed
    // @group buttonIcon
    // @visibility external
    //<

    //> @attr statefulCanvas.showRTLIcon (boolean : false : IR)
    // Is +link{Page.isRTL(),RTL} media available for the icon? If true, then in RTL mode, the
    // image's src will have "_rtl" inserted immediately before the file extension. For example,
    // if +link{icon,icon} is "myIcon.png" and showRTLIcon is true, then in RTL mode, the image's
    // src will be set to "myIcon_rtl.png".
    // @group RTL
    // @visibility external
    //<

    // ---------------------------------------------------------------------------------------

    // doc'd only on StretchImg
    gripImgSuffix:"grip",

    // ---------------------------------------------------------------------------------------
    
    //> @attr statefulCanvas.showOverCanvas (Boolean  : false : [IRWA])
    // When this property is set to true, this widget will create and show the
    // +link{StatefulCanvas.overCanvas} on user rollover.
    // @visibility external
    //<
    
    //> @attr statefulCanvas.overCanvas (AutoChild Canvas : null : [R])
    // Auto generated child widget to be shown when the user rolls over this canvas if 
    // +link{StatefulCanvas.showOverCanvas} is true. See documentation for +link{type:AutoChild}
    // for information on how to customize this canvas.
    // @visibility external
    //<
    
    //> @attr statefulCanvas.overCanvasConstructor (String : "Canvas" : [IRWA])
    // Constructor class name for this widget's +link{statefulCanvas.overCanvas,overCanvas}
    // @visibility external
    //<
    overCanvasConstructor: "Canvas",
    
    //> @attr statefulCanvas.overCanvasDefaults (Canvas : { ... } : [IRWA])
    // Default properties for this widgets +link{statefulCanvas.overCanvas,overCanvas}. To modify
    // these defaults, use +link{Class.changeDefaults()} 
    // @visibility external
    //<
    overCanvasDefaults: {
        // override mouseOut to hide this canvas if the user rolls off it and out of the
        // parent/constructor
        mouseOut:function () {
            if (isc.EH.getTarget() != this.creator) this.clear();
            return this.Super("mouseOut", arguments);
        }
    }
     

});

isc.StatefulCanvas.addMethods({

//>	@method	statefulCanvas.init()	(A)
// Initialize this StatefulCanvas. Pass in objects with properties to add or override defaults.
//
//		@param	[all arguments]	(object)	objects with properties to override from default
//<
initWidget : function () {

    
    if (this.src == null) this.src = this.vertical ? this.vSrc : this.hSrc;

    

    var stateIsDisabled = (this.state == isc.StatefulCanvas.STATE_DISABLED);
    if (stateIsDisabled) {
        if (!this.showDisabled) {
            this.logWarn("The state cannot be initialized to 'Disabled' if this.showDisabled is false. Setting to STATE_UP...");
            this.state = isc.StatefulCanvas.STATE_UP;
            stateIsDisabled = false;
        }
    }

    // the disabled property also affects the state of this object
    if (this.isDisabled()) {
        
        if (!stateIsDisabled) this._enabledState = this.state;

        if (this.showDisabled) {
            this.state = isc.StatefulCanvas.STATE_DISABLED;
            stateIsDisabled = true;
        }
    }

    // if className has been specified and baseStyle has no default, copy className to
    // baseStyle.  This is needed for the Label where you are expected to set className, not
    // baseStyle.
    // From then on the current className will be derived from the baseStyle setting plus the
    // current state, unless the widget suppresses className, which it may do if it has another
    // element the receives the baseStyle, and it leaves the handle unstyled.
    this.baseStyle = this.baseStyle || this.className;
    this.styleName = (this.suppressClassName ? null : this.getStateName());
    this.className = this.styleName;

    // If this button has a radioGroup ID specified, update the array of widgets in the
    // radiogroup to include this one.
    if (this.radioGroup != null) {

        var rg = this.radioGroup;
        // clear out the property to avoid a no-op, then add with the standard setter    
        this.radioGroup = null;
        this.addToRadioGroup(rg);
    }

    // Initialize autoFit
    this.setAutoFit(this.autoFit, true);

    if (this.showGrip) {
        // use the icon functionality of the label to show an image floated over center (this
        // is mutex with using the icon / label functionality, but most such uses don't make
        // much sense)
        this.showTitle = true;
        this.labelVPad = 0;
        this.labelHPad = 0;
        this.iconSpacing = 0;
        this.align = isc.Canvas.CENTER;
        // get the URL for a piece named "grip".  NOTE: resolve to a fully qualified URL now,
        // in the same imgDir context as the rest of the pieces, as opposed to the labels
        this.icon = this.getImgURL(this.getURL(this.gripImgSuffix));

        // NOTE: grip* sizing is intentionally null by default, so we get the image's natural
        // size, overriding the icon defaults.
        this.iconSize = this.gripSize;
        this.iconWidth = this.vertical ? this.gripBreadth : this.gripLength;
        this.iconHeight = this.vertical ? this.gripLength : this.gripBreadth;

        this.showRollOverIcon = this.showRollOverGrip;
        this.showDownIcon  = this.showDownGrip;
    }

    var showingLabel = this.shouldShowLabel();
    if (showingLabel) this.makeLabel();

    if (isc.screenReader && !showingLabel && !this.showGrip && (this.prompt || this.title)) {
        
        var label = this.getAriaLabel();

        // avoid writing out the default "Untitled Button" (or its i18n replacement)
        if (label != null) {
            //this.logWarn("aria-label set to: " + label);
            this.ariaState = isc.addProperties({}, this.ariaState, {
                label: label
            });
        }
    }
},

getAriaLabel : function () {
    var label = this.prompt || this.title;

    // avoid writing out the default "Untitled Button" (or its i18n replacement)
    if (label != null && label != "" && isc.Button.getInstanceProperty("title") != label) {
        return String.htmlStringToString(label);
    }
    return null;
},

//>	@method	statefulCanvas.getURL()
// Get the URL for an image based on this.src as modified by the piece name and state.
//			
//			eg if:		.src 		= "foo.gif"
//						pieceName 	= "start"
//						state		= "down"
//
//			url = 		foo_down_start.gif
//
// @param	[pieceName]	(string : "") 				name for part of the image
// @param	[state]		(string : this.state)		state of the image ("up", "off", etc.)
// @param	[selected]	(boolean : this.selected)	whether or not image is also in the
//                                                      "selected" state
// @param  [focused]   (boolean) 
//   Whether this image should be rendered in the "focused" state. Defaults to true if
//   this Img has focus and +link{StatefulCanvas.showFocused,this.showFocused} is true and
//   +link{StatefulCanvas.showFocusedAsOver,this.showFocusedAsOver} is false.
//
// @return (SCImgURL) URL for the image
//<
getURL : function (pieceName, state, selected, focused) {
    return isc.Img.urlForState(this.src, 
    					   selected != null ? selected : this.selected,
                           focused != null ? focused : this.getFocusedState(),
    					   state != null ? state : this.state, 
    					   pieceName,
                           this.getCustomState());
},

//> @method StatefulCanvas.shouldShowLabel()
// Should this widget create a floating label for textual content - used for image based widgets.
// Default implementation returns this.showTitle
// @return (boolean) true if the floating label should be created
//<
shouldShowLabel : function () {
    return this.showTitle;
    
},

//> @method statefulCanvas.setIgnoreRTL() (A)
// Setter for +link{ignoreRTL,ignoreRTL}.
// @param ignoreRTL (boolean) new value for ignoreRTL.
// @visibility external
//<
setIgnoreRTL : function (ignoreRTL) {
    this.ignoreRTL = !!ignoreRTL;
    if (this.isDrawn()) this.markForRedraw();
    if (this.label) this.label.setIgnoreRTL(ignoreRTL);
},

// State
// ------------------------------------------------------------------------------------------------------
// set the state for this object, and whether or not it is selected

_$visualState:"visualState",
stateChanged : function () {
    
    if (this.destroyed) return;

    if (this.logIsDebugEnabled(this._$visualState)) {
        this.logDebug("state changed to: " + this.getStateName(), "visualState");
    }

    if (this.redrawOnStateChange) {
        this.markForRedraw("state change");
    }
    // NOTE: a redraw doesn't update className
    if (!this.suppressClassName) {
        this.setClassName(this.getStateName());
    }

	// set our label to the same state (note it potentially has independent styling)
    var label = this.label;
	if (label != null) {
        label.setState(this.getState());
        label.setSelected(this.isSelected());
        label.setCustomState(this.getCustomState());
    }
},

//>	@method statefulCanvas.setBaseStyle()
// Sets the base CSS style.  As the component changes state and/or is selected, suffixes will be
// added to the base style.
// @visibility external
// @param style (CSSStyleName) new base style
//<		
setBaseStyle : function (style) {
    if (this.baseStyle == style) return;
    this.baseStyle = style;
    if (this.label && this.titleStyle == null) this.label.setBaseStyle(style);
    // fall through to stateChanged to actually update the appearance
    this.stateChanged();
},

_getIconCursor : function () {
    
    var cursor = this.iconCursor;
    if (this.isDisabled() && this.disabledIconCursor != null) cursor = this.disabledIconCursor;
    return cursor;
},


setTitleStyle : function (style) {
    if (this.titleStyle == style) return;
    this.titleStyle = style;
    if (this.label) {
        this.label.setBaseStyle(style || this.baseStyle);
    }
    this.stateChanged();
},

//> @method statefulCanvas.setState() (A)
// Sets the +link{StatefulCanvas.state,state} of this object, changing its appearance.
// Note: <code>newState</code> cannot be
// <smartclient>"Disabled"</smartclient>
// <smartgwt>{@link com.smartgwt.client.types.State#STATE_DISABLED}</smartgwt>
// if +link{StatefulCanvas.showDisabled,this.showDisabled} is <code>false</code>.
//
// @param newState (State) the new state.
// @group state
// @group appearance
// @visibility external
//<
setState : function (newState) {
    if (newState == isc.StatefulCanvas.STATE_DISABLED && !this.showDisabled) {
        this.logWarn("The state cannot be changed to 'Disabled' when this.showDisabled is false.");
        return;
    }
    if (this.state == newState) return;
    this.state = newState;
    // update the appearance - redraw if necessary
    this.stateChanged();
},

_updateChildrenTopElement : function () {
    this.Super("_updateChildrenTopElement", arguments);
    this.setHandleDisabled(this.isDisabled());
},

//>	@method	statefulCanvas.getState()	(A)
// Return the state of this StatefulCanvas
//		@group	state
//
// @visibility external
// @return (State)
//<
getState : function () {
	return this.state;
},

//>	@method	statefulCanvas.setSelected()
// Set this object to be selected or deselected.
//		@group	state
//
//		@param	newIsSelected	(boolean)	new boolean value of whether or not the object is
//                                          selected.
// @visibility external
//<
setSelected : function (newIsSelected) {
    // if this.selected is unset, we are not selected and should no-op if being set to not
    // selected
    if (this.selected == null && newIsSelected == false) {
        this.selected = false;
        return;
    } 

	if (this.selected == newIsSelected) return;

    // handle mutually exclusive radioGroups
    if (newIsSelected && this.radioGroup != null) {
        var groupArray = isc.StatefulCanvas._radioGroups[this.radioGroup];
        // catch the (likely common) case of this.radioGroup being out of synch - implies
        // a developer has assigned directly to this.radioGroup without calling the setter
        if (groupArray == null) {
            this.logWarn("'radioGroup' property set for this widget, but no corresponding group " +
                         "exists. To set up a new radioGroup containing this widget, or add this " +
                         " widget to an existing radioGroup at runtime, call 'addToRadioGroup(groupID)'");
        } else {
            for (var i = 0; i < groupArray.length; i++) {
                if (groupArray[i]!= this && groupArray[i].isSelected()) 
                    groupArray[i].setSelected(false);
            }
        }
    }

	this.selected = newIsSelected;

    if (this.label) this.label.setSelected(this.isSelected());

    this.stateChanged();
},

//>	@method	statefulCanvas.select()
// Select this object.
//		@group	state
// @visibility external
//<
select : function () {
	this.setSelected(true);
},

//>	@method	statefulCanvas.deselect()
// Deselect this object.
//		@group	state
// @visibility external
//<
deselect : function () {
	this.setSelected(false);
},

//>	@method	statefulCanvas.isSelected()
// Find out if this object is selected
//		@group	state
//		@return (Boolean)
// @visibility external
//<
isSelected : function () {
	return this.selected;
},

// actionType - determines whether the button will select / deselect on activation

//>	@method	statefulCanvas.getActionType() (A)
// Return the 'actionType' for this canvas (radio / checkbox / button)
//      @group  state
//      @group event handling
//      @visibility external
//<
getActionType : function () {
    return this.actionType;
},

//>	@method	statefulCanvas.setActionType() (A)
// Update the 'actionType' for this canvas (radio / checkbox / button)
// If the canvas is currently selected, and the passed in actionType is 'button'
// this method will deselect the canvas.
//      @group  state
//      @group event handling
//      @visibility external
//<
setActionType : function (actionType) {
    if (actionType == isc.StatefulCanvas.BUTTON && this.isSelected()) {
        this.setSelected(false);
    }
    this.actionType = actionType;
},

// radioGroups - automatic handling for mutually exclusive selection behavior between buttons

//>	@method	statefulCanvas.addToRadioGroup(groupID) (A)
// Add this widget to the specified mutually exclusive selection group with the ID
// passed in.
// Selecting this widget will then deselect any other StatefulCanvases with the same
// radioGroup ID.
// StatefulCanvases can belong to only one radioGroup, so this method will remove from 
// any other radiogroup of which this button is already a member.
//      @group  state
//      @group event handling
//      @param  groupID (string)    - ID of the radiogroup to which this widget should be added
//      @visibility external
//<
addToRadioGroup : function (groupID) {
    // Bail if groupID is null, or if we already belong to the specified group, so we don't 
    // get duplicated in the array
    if (groupID == null || this.radioGroup == groupID) return;
    
    if (this.radioGroup != null) this.removeFromRadioGroup();

    this.radioGroup = groupID;
    
    // update the widget array for the specified group (stored on the Class object)
    if (isc.StatefulCanvas._radioGroups[this.radioGroup] == null) {
        isc.StatefulCanvas._radioGroups[this.radioGroup] = [this];
    } else {
        isc.StatefulCanvas._radioGroups[this.radioGroup].add(this);
    }
    
},

//>	@method	statefulCanvas.removeFromRadioGroup(groupID) (A)
// Remove this widget from the specified mutually exclusive selection group with the ID
// passed in.
// No-op's if this widget is not a member of the groupID passed in.
// If no groupID is passed in, defaults to removing from whatever radioGroup this widget
// is a member of.
//      @group  state
//      @group event handling
//      @visibility external
//      @param  [groupID]   (string)    - optional radio group ID (to ensure the widget is removed
//                                        from the appropriate group.
//<
removeFromRadioGroup : function (groupID) {
    // if we're passed the ID of a group we're not a member of, just bail
    if (this.radioGroup == null || (groupID != null && groupID != this.radioGroup)) return;
    
    var widgetArray = isc.StatefulCanvas._radioGroups[this.radioGroup];
    
    widgetArray.remove(this);
    
    delete this.radioGroup;
    
},

// Enable/Disable
// ------------------------------------------------------------------------------------------------------
//	to have an object redraw when it's enabled, set:
//		.redrawOnDisable = true

//>	@method	statefulCanvas.setDisabled()
// Enable or disable this object
//		@group enable, state
//
//	@param	disabled (boolean) true if this widget is to be disabled
// @visibility external
//<
// actually implemented on Canvas, calls setHandleDisabled()

setHandleDisabled : function (disabled,b,c,d) {
	this.invokeSuper(isc.StatefulCanvas, "setHandleDisabled", disabled,b,c,d);

    // set the StatefulCanvas.STATE_DISABLED/StatefulCanvas.STATE_UP states.
    var handleIsDisabled = (this.state == isc.StatefulCanvas.STATE_DISABLED);
    if (handleIsDisabled == disabled) return;

	if (disabled == false) {
        var enabledState = this._enabledState || isc.StatefulCanvas.STATE_UP;
        if (enabledState == isc.StatefulCanvas.STATE_OVER) {
            var EH = this.ns.EH;
            if (!this.visibleAtPoint(EH.getX(), EH.getY())) {
                enabledState = isc.StatefulCanvas.STATE_UP;
                this.setState(enabledState);
            } else {
                this.setState(isc.StatefulCanvas.STATE_UP);
                this._doMouseOverStateChange();
            }
        } else this.setState(enabledState);
	} else {
        // hang onto the enable state so that when we're next enabled we can reset to it.
        this._enabledState = this.state;
        this._doMouseOutStateChange(true);
        if (this.showDisabled) this.setState(isc.StatefulCanvas.STATE_DISABLED);
    }

    if (this.showDisabled && this.iconCursor != null) {
        var imageHandle = this.getImage("icon");
        if (imageHandle != null) imageHandle.style.cursor = this._getIconCursor();
    }
},


// CSS Style methods
// ------------------------------------------------------------------------------------------
// methods that allow style to change according to state.

//>	@method	statefulCanvas.getStateName()	(A)
// Get the CSS styleName that should currently be applied to this component, reflecting
// <code>this.baseStyle</code> and the widget's current state.
// <P>
// NOTE: this can differ from the style currently showing if the component has not yet updated
// it's visual state after a state change.
// 
//		@group	appearance
//		@return	(CSSStyleName)	name of the style to set the StatefulCanvas to
//<
getStateName : function () {
    var modifier = this.getStateSuffix();
    if (modifier) return this.baseStyle + modifier;
    return this.baseStyle;
},

getTitleStateName : function () {
    
    if (!this.titleStyle) return null;
    return this.titleStyle + (this.showDisabled && this.isDisabled() ? isc.StatefulCanvas.STATE_DISABLED : isc.emptyString);
},

//>	@method	statefulCanvas.getStateSuffix()	
// Returns the suffix that will be appended to the +link{StateFulCanvas.baseStyle}  
// as the component changes +link{statefulCanvas.state} and/or is selected / focused.
// <P>
// The following table lists out the standard set of suffixes which may be applied 
// to the base style:
// <table border=1>
// <tr><td><b>CSS Class Applied</b></td><td><b>Description</b></td></tr>
// <tr><td><code><i>baseStyle</i></code></td><td>Default css style</td></tr>
// <tr><td><code><i>baseStyle</i>+Selected</code></td>
//      <td>Applied when +link{statefulCanvas.selected} is set to true</td></tr>
// <tr><td><code><i>baseStyle</i>+Focused</code></td>
//      <td>Applied when the component has keyboard focus, if 
//      +link{statefulCanvas.showFocused} is true, and 
//      +link{statefulCanvas.showFocusedAsOver} is not true.</td></tr>
// <tr><td><code><i>baseStyle</i>+Over</code></td>
//      <td>Applied when +link{statefulCanvas.showRollOver} is set to true and either the user
//      rolls over the component or +link{statefulCanvas.showFocusedAsOver} is true and the
//      component has keyboard focus.</td></tr>
// <tr><td><code><i>baseStyle</i>+Down</code></td>
//      <td>Applied when the user presses the mouse button on the component if
//          +link{statefulCanvas.showDown} is set to true</td></tr>
// <tr><td><code><i>baseStyle</i>+Disabled</code></td>
//      <td>Applied when the component is +link{Canvas.disabled,disabled}
//       if +link{statefulCanvas.showDisabled} is true.</td></tr>
// <tr><td colspan=2><i>Combined styles</i></td></tr>
// <tr><td><code><i>baseStyle</i>+SelectedFocused</code></td>
//      <td>Combined Selected and focused styling</td></tr>
// <tr><td><code><i>baseStyle</i>+SelectedOver</code></td>
//      <td>Combined Selected and rollOver styling</td></tr>
// <tr><td><code><i>baseStyle</i>+FocusedOver</code></td>
//      <td>Combined Focused and rollOver styling</td></tr>
// <tr><td><code><i>baseStyle</i>+SelectedFocusedOver</code></td>
//      <td>Combined Selected, Focused and rollOver styling</td></tr>
// <tr><td><code><i>baseStyle</i>+SelectedDown</code></td>
//      <td>Combined Selected and mouse-down styling</td></tr>
// <tr><td><code><i>baseStyle</i>+FocusedDown</code></td>
//      <td>Combined Focused and mouse-down styling</td></tr>
// <tr><td><code><i>baseStyle</i>+SelectedFocusedDown</code></td>
//      <td>Combined Selected, Focused and mouse-down styling</td></tr>
// <tr><td><code><i>baseStyle</i>+SelectedDisabled</code></td>
//      <td>Combined Selected and Disabled styling</td></tr>
// </table>
//
// @return (String) suffix to be appended to the baseStyle
// @visibility external
//<						
getStateSuffix : function () {
    var state = this.getState(),
        selected = this.isSelected() ? isc.StatefulCanvas.SELECTED : null,
        focused = this.getFocusedState() ? isc.StatefulCanvas.FOCUSED : null,
        customState = this.getCustomState();
    return this._getStateSuffix(state,selected,focused,customState);
},

_$SelectedFocused:"SelectedFocused",
_getStateSuffix : function (state, selected, focused, customState) {
    var modifier;
    if (selected || focused) {
        modifier = (selected && focused) ? this._$SelectedFocused : 
                                                selected ? selected : focused; 
    }
    if (!customState) {
        if (modifier) return state ? modifier + state : modifier;
        else return state;
    } else if (modifier) {
        return state ? modifier + state + customState : modifier + customState;
    } else {
        return state ? state + customState : customState;
    }
},

setCustomState : function (customState) { 
    if (customState == this.customState) return;
    this.customState = customState;
    this.stateChanged();
},
getCustomState : function () { return this.customState },

// Override getPrintStyleName to pick up the current stateName rather than this.styleName which
// may have been cleared (EG suppressClassName is true)
getPrintStyleName : function () {
    return this.printStyleName || this.getStateName();
},

// Label
// ---------------------------------------------------------------------------------------

labelDefaults : {
    _isStatefulCanvasLabel: true,
    _canFocus : function () { return this.masterElement._canFocus(); },
    focusChanged : function (hasFocus) {
        if (this.hasFocus) this.eventProxy.focus();
    },

    getContents : function () { return this.masterElement.getTitleHTML() }, 

    // override adjustOverflow to notify us when this has it's overflow changed
    // (probably due to 'setContents')
    adjustOverflow : function (a,b,c,d) {
        this.invokeSuper(null, "adjustOverflow", a,b,c,d);
        if (this.masterElement) this.masterElement._labelAdjustOverflow();
    }
},

_$label: "label",
makeLabel : function () {
    var labelClass = this.getAutoChildClass(this._$label, null, isc.Label);

    var label = labelClass.createRaw();
    label.ignoreRTL = this.ignoreRTL;
    label.clipTitle = this.clipTitle;
    // handle the clipped title hover ourselves
    label.showClippedTitleOnHover = false;
    label._canHover = false;

    if (this._getAfterPadding != null) {
        label._getAfterPadding = function () {
            return this.masterElement._getAfterPadding();
        };
    }

    label.align = this.align;
    label.valign = this.valign;

    label._resizeWithMaster = false;
    label._redrawWithMaster = (this._redrawLabelWithMaster != null ? this._redrawLabelWithMaster : false);
    label._redrawWithParent = false;
    label.containedPeer = true;

    // icon-related
    label.icon = this.icon;
    label.iconWidth = this.iconWidth;
    label.iconHeight = this.iconHeight;
    label.iconSize = this.iconSize;
    label.iconOrientation = this.iconOrientation;
    label.iconAlign = this.iconAlign;
    label.iconSpacing = this.iconSpacing;
    label.iconStyle = this.iconStyle;
    label.iconCursor = this.iconCursor;
    label.disabledIconCursor = this.disabledIconCursor;
    label.showDownIcon = this.showDownIcon;
    label.showSelectedIcon = this.showSelectedIcon;
    label.showRollOverIcon = this.showRollOverIcon;
    label.showFocusedIcon = this.showFocusedIcon;
    label.showDisabledIcon = this.showDisabledIcon;
    label.showRTLIcon = this.showRTLIcon;
    if (this.showIconState != null) label.showIconState = this.showIconState;

    // If we show 'focused' state, have our label show it too.
    label.getFocusedAsOverState = function () {
        var button = this.masterElement;
        if (button && button.getFocusedAsOverState) return button.getFocusedAsOverState();
    };
    label.getFocusedState = function () {
        var button = this.masterElement;
        if (button && button.getFocusedState) return button.getFocusedState();
    };


    // By default we'll apply our skinImgDir to the label - allows [SKIN] to be used
    // in icon src.
    label.skinImgDir = this.labelSkinImgDir || this.skinImgDir;

    
    label.baseStyle = this.titleStyle || this.baseStyle;
    label.showDisabled = this.showDisabled;
    label.state = this.getState();
    label.customState = this.getCustomState();

    // default printStyleName to this.printStyleName
    label.getPrintStyleName = function () {
        return this.masterElement.getPrintStyleName();
    }

    // if we're set to overflow:visible, that means the label should set to overflow:visible
    // and we should match its overflowed size
    label.overflow = this.overflow;

    
	label.width = this._getLabelSpecifiedWidth();
	label.height = this._getLabelSpecifiedHeight();
	label.left = this._getLabelLeft();
	label.top = this._getLabelTop();

    
    // NOTE: vertical always false where inapplicable, eg ImgButton
    label.wrap = this.wrap != null ? this.wrap : this.vertical;

    label.eventProxy = this;        
    
    label.isMouseTransparent = true; 

    label.zIndex = this.getZIndex(true) + 1;

    label.tabIndex = -1;

    // finish createRaw()/completeCreation() construction style, but allow autoChild defaults
    this._completeCreationWithDefaults(this._$label, label);

    label = this.label = isc.SGWTFactory.extractFromConfigBlock(label);

    // Because the label is a peer of this StatefulCanvas, if we are explicitly disabled, but
    // within an enabled parent, the label, when added to the parent, would be enabled if we
    // did not explicitly disable it.
    label.setDisabled(this.isDisabled());
    label.setSelected(this.isSelected());

    
    this.addPeer(label, null, null, true);
},


setLabelSkinImgDir : function (dir) {
    this.labelSkinImgDir = dir;
    if (this.label != null) this.label.setSkinImgDir(dir);
},

setSkinImgDir : function (dir) {
    this.Super("setSkinImgDir", arguments);
    if (this.labelSkinImgDir == null && this.label != null) this.label.setSkinImgDir(dir);
},

// Label Sizing Handling
// ---------------------------------------------------------------------------------------

//> @method statefulCanvas.setIconOrientation
// Changes the orientation of the icon relative to the text of the button.
//
// @param orientation ("left" or "right") The new orientation of the icon relative to the text
// of the button.
//
// @group buttonIcon
// @visibility external
//<
setIconOrientation : function (orientation) {
    
    this.iconOrientation = orientation;
    if (this.label) {
        this.label.iconOrientation = orientation;
        this.label.markForRedraw();
    } else {
        this.markForRedraw();
    }
},

//>@method statefulCanvas.setAutoFit()
// Setter method for the +link{StatefulCanvas.autoFit} property. Pass in true or false to turn
// autoFit on or off. When autoFit is set to <code>false</code>, canvas will be resized to
// it's previously specified size.
// @param autoFit (boolean) New autoFit setting.
// @visibility external
//<
setAutoFit : function (autoFit, initializing) {

    // setAutoFit is called directly from resizeTo
    // If we're resizing before the autoFit property's initial setup, don't re-set the
    // autoFit property.
    if (initializing) {
        this._autoFitInitialized = true;
        // No need to make any changes if autoFit is false
        if (!autoFit) return;
    }

    // This can happen if 'setWidth()' et-al are called during 'init' for the statefulCanvas,
    // and should not effect the autoFit setting.
    if (!this._autoFitInitialized) return;

    // Typecast autoFit to a boolean
    autoFit = !!autoFit;

    // bail if no change to autoFit, unless this is the special init-time call
    if (!initializing && (!!this.autoFit == autoFit)) return;
    
    this._settingAutoFit = true;
    this.autoFit = autoFit;
    var horizontal = (this.autoFitDirection == isc.Canvas.BOTH) || 
                      (this.autoFitDirection == isc.Canvas.HORIZONTAL),
        vertical = (this.autoFitDirection == isc.Canvas.BOTH) || 
                    (this.autoFitDirection == isc.Canvas.VERTICAL);

    // advertise that we have inherent width/height in whatever directions we are autofitting,
    // iow, a Layout should not expand us along that axis.
    this.inherentWidth = autoFit && horizontal;
    this.inherentHeight = autoFit && vertical;

    if (autoFit) {
        // record original overflow, width and height settings so we can restore them if
        // setAutoFit(false) is called
        this._explicitOverflow = this.overflow;
        this.setOverflow(isc.Canvas.VISIBLE);
                
        if (horizontal) {
            this._explicitWidth = this.width;
            this.setWidth(1);
        }
        
        if (vertical) {
            this._explicitHeight = this.height;
            this.setHeight(1);
        }
        //this.logWarn("just set autoFit to:"+ autoFit + 
        //     ", width/height/overflow:"+ [this.width, this.height, this.overflow]);

    } else {

        // If we had an explicit height before being set to autoFit true, we should reset to
        // that size, otherwise reset to default.
        var width = this._explicitWidth || this.defaultWidth,
            height = this._explicitHeight || this.defaultHeight;
            
        
        if (horizontal) this.setWidth(width);
        if (vertical) this.setHeight(height);

        if (this.parentElement && isc.isA.Layout(this.parentElement)) {
            if (horizontal && !this._explicitWidth) this._userWidth = null;
            if (vertical && !this._explicitHeight) this._userHeight = null;            
        }
        this._explicitWidth = null;
        this._explicitHeight = null;
        if (this._explicitOverflow) this.setOverflow(this._explicitOverflow);
        this._explicitOverflow = null;

    }
    delete this._settingAutoFit;
},


// override 'resizeBy()' / 'setOverflow()' - if these methods are called
// we're essentially clearing out this.autoFit
// Note we override resizeBy() as setWidth / setHeight / resizeTo all fall through to this method.
resizeBy : function (dX, dY, a,b,c,d) {

    if (this.autoFit && this._autoFitInitialized && !this._settingAutoFit) {
        var changeAutoFit = false;
        
        if (dX != null && 
            (this.autoFitDirection == isc.Canvas.BOTH || 
             this.autoFitDirection == isc.Canvas.HORIZONTAL)) 
        {        
            this._explicitWidth = (1 + dX);
            changeAutoFit = true;
            dX = null;
        }
        if (dY != null && 
            (this.autoFitDirection == isc.Canvas.BOTH || 
             this.autoFitDirection == isc.Canvas.VERTICAL)) 
        {
            this._explicitHeight = (1 + dY);
            changeAutoFit = true;
            dY = null;
        }

        // one or more of the dimensions where we're autofitting has changed.  Disable
        // autoFitting for this dimension - this will call setWidth / height to return to
        // default or pre-autoFit size
        if (changeAutoFit) this.setAutoFit(false);
        // now continue with normal resizeBy logic for other dimension, if it's non-null
    }
	return this.invokeSuper(isc.StatefulCanvas, "resizeBy", dX,dY, a,b,c,d);    
},

getLabelHPad : function () {
    if (this.labelHPad != null) return this.labelHPad;
    if (this.vertical) {
        return this.labelBreadthPad != null ? this.labelBreadthPad : 0;
    } else {
        return this.labelLengthPad != null ? this.labelLengthPad : this.capSize;
    }
},

getLabelVPad : function () {
    if (this.labelVPad != null) return this.labelVPad;
    if (!this.vertical) {
        return this.labelBreadthPad != null ? this.labelBreadthPad : 0;
    } else {
        return this.labelLengthPad != null ? this.labelLengthPad : this.capSize;
    }
},

_getLabelLeft : function () {
    var left;
    
    if (this.isDrawn()) {
        left = (this.position == isc.Canvas.RELATIVE && this.parentElement == null ? 
                this.getPageLeft() : this.getOffsetLeft());
    } else {
        left = this.getLeft();
    }

    left += this.getLabelHPad();
    
    return left;
},

_getLabelTop : function () {
    var top;
    if (this.isDrawn()) {
        top = (this.position == isc.Canvas.RELATIVE && this.parentElement == null ? 
               this.getPageTop() : this.getOffsetTop());
    } else {
        top = this.getTop();
    }

    top += this.getLabelVPad();
    return top;
},

_getLabelSpecifiedWidth : function () {
    var width = this.getInnerWidth();
    width -= 2* this.getLabelHPad();
    
    return Math.max(width, 1);
},

_getLabelSpecifiedHeight : function () {
    var height = this.getInnerHeight();
    height -= 2 * this.getLabelVPad();
    return Math.max(height, 1);
},

// if we are overflow:visible, match the drawn size of the label.
// getImgBreadth/getImgLength return the sizes for the non-stretching and stretching axes
// respectively.
// NOTE that stretching on the breadth axis won't look right with most media sets, eg a
// horizontally stretching rounded button is either going to tile its rounded caps vertically
// (totally wrong) or stretch them, which will probably degrade the media.
getImgBreadth : function () {
    if (this.overflow == isc.Canvas.VISIBLE && isc.isA.Canvas(this.label)) 
    {
        return this.vertical ? this._getAutoInnerWidth() : this._getAutoInnerHeight();
    }
    
    //return this.Super("getImgBreadth", arguments);
    // same as the Superclass behavior
    return (this.vertical ? this.getInnerWidth() : this.getInnerHeight());
},

getImgLength : function () {
    if (this.overflow == isc.Canvas.VISIBLE && isc.isA.Canvas(this.label)) 
    {
        return this.vertical ? this._getAutoInnerHeight() : this._getAutoInnerWidth();
    }
    return (this.vertical ? this.getInnerHeight() : this.getInnerWidth());
},

// get the inner breadth or height we should have if we are overflow:visible and want to size
// to the label and the padding we leave around it
_getAutoInnerHeight : function () {
    var innerHeight = this.getInnerHeight();
    // use the normal inner height if we have no label
    if (!isc.isA.Canvas(this.label)) return innerHeight;

    // if the padding for this dimension is set, use that, otherwise assume the capSize as a
    // default padding for the stretch dimension
    var padding = this.getLabelVPad();
    var labelSize = this.label.getVisibleHeight() + 2*padding;
    return Math.max(labelSize, innerHeight);
},

_getAutoInnerWidth : function () {
    var innerWidth = this.getInnerWidth();
    if (!isc.isA.Canvas(this.label)) return innerWidth;

    var padding = this.getLabelHPad();
    var labelSize = this.label.getVisibleWidth() + 2*padding;
    return Math.max(labelSize, innerWidth);
},
    

// Have getSizeTestHTML delegate to the label but add the
// labelHPad
_getSizeTestHTML : function (title) {
    if (isc.isA.Canvas(this.label)) {
        return "<table cellpadding=0 cellspacing=0><tr><td>" +
                isc.Canvas.spacerHTML(2*this.getLabelHPad(), 1) + "</td><td>" +
                this.label._getSizeTestHTML(title) + "</td></tr></table>";
    }
    return "<div style='position:absolute;" +
            (this.wrap ? "' " : "white-space:nowrap;' ") +
            "class='" + this.getStateName() + "'>" + title + "</div>";
},


// If we are matching the label size, and it changes, resize images and redraw
_$labelOverflowed:"Label overflowed.",
_labelAdjustOverflow : function () {
    if (this.overflow != isc.Canvas.VISIBLE) return;

    //this.logWarn("our innerWidth:" + this.getInnerWidth() + 
    //             ", label visible width: " + this.label.getVisibleWidth() + 
    //             " padding: " + (this.labelHPad * 2) +
    //             " resizing to width: " + this.getImgLength());
    
    // by calling our adjustOveflow, we will re-check the scrollWidth / height which
    // will adjust our size if necessary
    this.adjustOverflow(this._$labelOverflowed);
},

// Override getScrollWidth / Height - if we are overflow:"visible", and have a label we're 
// going to size ourselves according to its drawn dimensions
getScrollWidth : function (calcNewValue,B,C,D) {

    if (this.overflow != isc.Canvas.VISIBLE || !isc.isA.Canvas(this.label)) 
        return this.invokeSuper(isc.StatefulCanvas, "getScrollWidth", calcNewValue,B,C,D);

    if (this._deferredOverflow) {
        this._deferredOverflow = null;
        this.adjustOverflow("widthCheckWhileDeferred");
    }

    if (!calcNewValue && this._scrollWidth != null) return this._scrollWidth;

    // _getAutoInnerWidth() will give us back the greater of our specified size / the
    // label's visible size + our end caps.
    // This is basically our "scroll size" if overflow is visible
    var scrollWidth = this._getAutoInnerWidth()

    return (this._scrollWidth = scrollWidth);
},

getScrollHeight : function (calcNewValue,B,C,D) {

    if (this.overflow != isc.Canvas.VISIBLE || !isc.isA.Canvas(this.label)) 
        return this.invokeSuper(isc.StatefulCanvas, "getScrollHeight", calcNewValue,B,C,D);

    if (this._deferredOverflow) {
        this._deferredOverflow = null;
        this.adjustOverflow("heightCheckWhileDeferred");
    }

    if (!calcNewValue && this._scrollHeight != null) return this._scrollHeight;

    // _getAutoInnerWidth() will give us back the greater of our specified size / the
    // label's visible size + our end caps.
    // This is basically our "scroll size" if overflow is visible
    var scrollHeight = this._getAutoInnerHeight()

    return (this._scrollHeight = scrollHeight);
},

// Update the label's overflow when our overflow gets updated.
setOverflow : function (newOverflow, a, b, c, d) {

    // If we're autoFit:true, and overflow is getting set to hidden revert the autoFit property
    // to false
    if (this.autoFit && this._autoFitInitialized && !this._settingAutoFit &&
        newOverflow != isc.Canvas.VISIBLE) {
        
        this._explicitOverflow = newOverflow;
        this.setAutoFit(false);
        return;
    }    

    this.invokeSuper(isc.StatefulCanvas, "setOverflow", newOverflow, a, b, c, d);
    if (isc.isA.Canvas(this.label)) this.label.setOverflow(newOverflow, a, b, c, d);
    
},

// if the SIB is resized, resize the label
// This covers both:
// - the SIB is resized by application code and the label must grow/shrink
// - the SIB resizes itself as a result of the label changing size, in which case the call to
//   resize the label should no-op, since the sizes already agree
_$_resized:"_resized",
_resized : function (deltaX, deltaY, a,b,c) {
    this.invokeSuper(isc.StatefulCanvas, this._$_resized, deltaX,deltaY,a,b,c);
    //if (!this.label || this.overflow != isc.Canvas.VISIBLE) return;
    if (this.label) this.label.resizeTo(this._getLabelSpecifiedWidth(), 
                                        this._getLabelSpecifiedHeight());
},


draw : function (a,b,c) {
    if (isc._traceMarkers) arguments.__this = this;

    
    var returnVal = isc.Canvas._instancePrototype.draw.call(this, a,b,c);    
    //var returnVal = this.Super("draw", arguments);
    
    if (this.position != isc.Canvas.ABSOLUTE && isc.isA.Canvas(this.label)) {
        
        if (isc.Page.isLoaded()) this._positionLabel();
        else isc.Page.setEvent("load", this.getID() + "._positionLabel()");
    }

    if (this.label != null && isc.Canvas.ariaEnabled()) {
        //var labelDOMId = this.label.getCanvasName();
        //this.logWarn("setting labelledby to: " + labelDOMId);
        //this.setAriaState("labelledby", labelDOMId);
        
        var label = this.getAriaLabel();
        if (label != null) this.setAriaState("label", label);
    }

    return returnVal;        
},

_positionLabel : function () {
    if (!this.isDrawn()) return;
    this.label.moveTo(this._getLabelLeft(), this._getLabelTop());
},

// setAlign() / setVAlign() to set content alignment
// JSDoc'd in subclasses
setAlign : function (align) {
    this.align = align;
    if (this.isDrawn()) this.markForRedraw();
    if (this.label) this.label.setAlign(align);
},

setVAlign : function (valign) {
    this.valign = valign;
    if (this.isDrawn()) this.markForRedraw();
    if (this.label) this.label.setVAlign(valign);
},


// Printing
// --------------------------------------------------------------------------------------

// If we are showing a label default to printing it's text rather than 
// our standard content (images etc)
getPrintHTML : function (a,b,c,d) {
    var useLabel = this.shouldShowLabel();
    if (useLabel) {
        if (this.label == null) {
            this.makeLabel();
        }
        return this.label.getPrintHTML(a,b,c,d);
    }
    return this.Super("getPrintHTML", arguments);
    
},

// Title handling
// ---------------------------------------------------------------------------------------

//> @method statefulCanvas.shouldHiliteAccessKey()
// Should the accessKey be underlined if present in the title for this button.
// Default implementation returns +link{StatefulCanvas.hiliteAccessKey}
//<
shouldHiliteAccessKey : function () {
    return this.hiliteAccessKey;
},


// If this widget has an accessKey, it will underline the first occurance of the accessKey
// in the title (preferring Uppercase to Lowercase)
getTitleHTML : function () {

    var title = this.getTitle();

    if (!this.shouldHiliteAccessKey() || !isc.isA.String(title) || this.accessKey == null) 
        return title;

    return isc.Canvas.hiliteCharacter(title, this.accessKey);
},

//>	@method	statefulCanvas.getTitle()	(A)
// Return the title - HTML drawn inside the component.
// <p>
// Default is to simply return this.title.
// @return (HTMLString) HTML for the title.
// @visibility external
//<

getTitle : function () {
    return this.title;
},

//> @method statefulCanvas.setTitle()
// Setter for the +link{StatefulCanvas.title,title}.
// @param newTitle (HTMLString) the new title HTML.
// @group	appearance
// @visibility external
//<
setTitle : function (newTitle) {
	// remember the contents
	this.title = newTitle;
	// re-evaluation this.getTitle in case it's dynamic.
	var newTitle = this.getTitleHTML();
    // For performance, don't force a redraw / setContents, etc if the
    // title is unchanged
    if (this._titleHTML != null && this._titleHTML == newTitle) {
        return;
    } else {
        this._titleHTML = newTitle;
    }
    if (this.label) {
        
        if (this.label._redrawWithMaster && this.label.masterElement == this) this.label._dirty = true;
	    this.label.setContents(newTitle);
    	this.label.setState(this.getState());
	    this.label.setSelected(this.isSelected());
    // if we didn't have a label before, lazily create it.
    } else if (this.title != null && this.shouldShowLabel()) {
        this.makeLabel()
    }

    // Update the ariaLabel to reflect our title (we do this regardless of whether we're
    // showing a title or not.
    if (isc.Canvas.ariaEnabled()) {
        var ariaLabel = this.getAriaLabel();
        if (ariaLabel != null) this.setAriaState("label", ariaLabel);
        else this.clearAriaState("label");
    }

    // redraw even if we have a title label.
    
    this.markForRedraw("setTitle");
},

// other Label management
// ---------------------------------------------------------------------------------------

// override setZIndex to ensure that this.label is always visible.
setZIndex : function (index,b,c) {
    
    isc.Canvas._instancePrototype.setZIndex.call(this, index,b,c);    
    //this.Super("setZIndex", arguments);

    if (isc.isA.Canvas(this.label)) this.label.moveAbove(this);
},


// Override _updateCanFocus() update the focusability of the label too
_updateCanFocus : function () {
    this.Super("_updateCanFocus", arguments);
    if (this.label != null) this.label._updateCanFocus();
},

//> @method statefulCanvas.setIcon()
// Change the icon being shown next to the title text.
// @param icon (URL) URL of new icon
// @group buttonIcon
// @visibility external
//<
// NOTE: subclasses that show a Label use the label to show an icon.  Other subclasses (like
// Button) must override setIcon()
setIcon : function (icon) { 
    this.icon = icon;
    if (this.label) this.label.setIcon(icon); 
    // lazily create a label if necessary
    else if (icon && this.shouldShowLabel()) this.makeLabel();
},

//> @method statefulCanvas.setIconStyle()
// Setter for +link{StatefulCanvas.iconStyle}.
// @param iconStyle (CSSStyleName) the new <code>iconStyle</code> (may be <code>null</code> to
// remove the className on the image).
// @visibility external
//<
// NOTE: subclasses that show a Label use the label to reflect changes in the iconStyle. Other
// subclasses (like Button) must override setIcon().
setIconStyle : function (iconStyle) {
    this.iconStyle = iconStyle;
    if (this.label) this.label.setIconStyle(iconStyle);
},

// Mouse Event Handlers
// --------------------------------------------------------------------------------------------
// various mouse events will set the state of this object.

// implement mouseOver / mouseOut handlers to apply appropriate states to
// this widget.

handleMouseOver : function (event,eventInfo) {
    var rv;
    if (this.mouseOver != null) {
        rv = this.mouseOver(event, eventInfo);
        if (rv == false) return false;
    }
    this._doMouseOverStateChange();
    return rv;
},

_doMouseOverStateChange : function () {
    if (this.showDown && this.ns.EH.mouseIsDown()) {
        
        this.setState(isc.StatefulCanvas.STATE_DOWN);
    } else {
        if (this.showRollOver) {
            this.setState(isc.StatefulCanvas.STATE_OVER);
        }
        if (this.showOverCanvas) {
            if (this.overCanvas == null) {
                this.addAutoChild("overCanvas", {
                    autoDraw:false
                });
            }
            this.overCanvas.moveAbove(this);
            if (!this.overCanvas.isDrawn()) this.overCanvas.draw();
        }
    }
},

// clear rollOver styling on mouseOut
handleMouseOut : function (event,eventInfo) {
    var rv;
    if (this.mouseOut != null) {
        rv = this.mouseOut(event, eventInfo);
        if (rv == false) return rv;
    }
    this._doMouseOutStateChange();
    return rv;
},

_doMouseOutStateChange : function (disabling) {
    if (this.showRollOver) {
        this.setState(this.getFocusedAsOverState()
                      ? isc.StatefulCanvas.STATE_OVER : isc.StatefulCanvas.STATE_UP);
    } else if (this.showDown && this.ns.EH.mouseIsDown()) {
        
        this.setState(isc.StatefulCanvas.STATE_UP);
    }

    if (this.showOverCanvas && this.overCanvas != null && this.overCanvas.isVisible() &&
        (disabling || !this.overCanvas.contains(this.ns.EH.getTarget(), true)))
    {
        this.overCanvas.clear();
    }
},

// override the internal _focusChanged() method to set the state of the canvas to "over" on
// focus.  (Note - overriding this rather than the public 'focusChanged()' method so developers
// can still put functionality into that method without worrying about calling 'super').
_focusChanged : function (hasFocus,b,c,d) {

    var returnVal = this.invokeSuper(isc.StatefulCanvas, "_focusChanged", hasFocus,b,c,d);
    // don't show the over state if we don't actually have focus anymore (because onFocus
    // is delayed in IE and focus may be elsewhere by the time it fires) 
    if (!(hasFocus && isc.Browser.isIE && 
         (this.getFocusHandle() != this.getDocument().activeElement)) ) 
    {
        this.updateStateForFocus(hasFocus);
    }

    return returnVal;
},

updateStateForFocus : function (hasFocus) {
    if (!this.showFocused) return;

    if (this.showFocusedAsOver) {
        // NOTE: don't show the over state if showRollOver is false, because this is typically set
        // because there is no over state media (eg for an ImgButton)
        if (!this.showRollOver) return;

        var state = this.getState();
        if (hasFocus && !this.isDisabled()) {
            // on focus, if our state is currently 'up' set state to 'over' to indicate
            // we have focus
            if (state == isc.StatefulCanvas.STATE_UP) this.setState(isc.StatefulCanvas.STATE_OVER);
        } else {
            // on blur - clear out the 'over' state (if appropriate)
            var EH = this.ns.EH;
            if (state == isc.StatefulCanvas.STATE_OVER &&
                !this.visibleAtPoint(EH.getX(), EH.getY()))
            {
                this.setState(isc.StatefulCanvas.STATE_UP);
            }
        }
    } else {
        // just call stateChanged - it will check this.hasFocus
        this.stateChanged();
        // Note: normally label styling etc will be updated by stateChanged() - but in this case
        // the other states are all unchanged so the label would not necessarily refresh to reflect
        // the focused state.
        if (this.label) this.label.stateChanged();
    }
},

getFocusedAsOverState : function () {
    if (!this.showFocused || !this.showFocusedAsOver || this.isDisabled()) return false;
    return this.hasFocus;
},

// getFocusedState() - returns a boolean value for whether we should show the "Focused" state
getFocusedState : function () {
    if (!this.showFocused || this.showFocusedAsOver || this.isDisabled()) return false;
    return this.hasFocus;
},

//>	@method	statefulCanvas.handleMouseDown()	(A)
// MouseDown event handler -- show the button as down if appropriate
// calls this.mouseDown() if assigned
//	may redraw the button
//		@group	event
//<
handleMouseDown : function (event, eventInfo) {
    if (event.target == this && this.useEventParts) {
        if (this.firePartEvent(event, isc.EH.MOUSE_DOWN) == false) return false;
    }
    var rv;
    if (this.mouseDown) {
        rv = this.mouseDown(event, eventInfo);
        if (rv == false) return false;
    }
	if (this.showDown) this.setState(isc.StatefulCanvas.STATE_DOWN);
    return rv;
},


//>	@method	statefulCanvas.handleMouseUp()	(A)
//		@group	event
//			mouseUp event handler -- if showing the button as down, reset to the 'up' state
//          calls this.mouseUp() if assigned
//<
handleMouseUp : function (event, eventInfo) {
    if (event.target == this && this.useEventParts) {
        if (this.firePartEvent(event, isc.EH.MOUSE_UP) == false) return false;
    }

    var rv;
    if (this.mouseUp) {
        rv = this.mouseUp(event, eventInfo);    
        if (rv == false) return false;
    }

    // set the state of the button to change its appearance
    if (this.showDown) {
        var EH = this.ns.EH;
        // In desktop browsers, when the 'mouseup' event occurs on this StatefulCanvas, the mouse
        // cursor is still over the StatefulCanvas, so if showRollOver is true, go back to the
        // "Over" state. When the mouse cursor leaves this StatefulCanvas, then the 'mouseout'
        // handler will reset the state back to STATE_UP.
        //
        // On touch-enabled devices, the 'mouseout' event will not fire until the user next touches
        // something interactable on screen, possibly not for a very long time. So that we don't
        // leave the StatefulCanvas in the "Over" state, make sure the 'mouseup' event was not
        // fired in response to ending a touch event.
        this.setState(this.showRollOver &&
                      EH._handledTouch != EH._touchEventStatus.TOUCH_ENDING
                      ? isc.StatefulCanvas.STATE_OVER : isc.StatefulCanvas.STATE_UP);
    }
    return rv;
},





//>	@method	statefulCanvas.handleActivate() (A)
//      "Activate" this widget - fired from click or Space / Enter keypress.
//      Sets selection state of this widget if appropriate.
//      Calls this.activate stringMethod if defined
//      Otherwise calls this.click stringMethod if defined.
//      @group  event
//<
handleActivate : function (event, eventInfo) {
    var actionType = this.getActionType();
	if (actionType == isc.StatefulCanvas.RADIO) {
		// if a radio button, select this button
		this.select();
		
	} else if (actionType == isc.StatefulCanvas.CHECKBOX) {
		// if a checkbox, toggle the selected state
		this.setSelected(!this.isSelected());
	}

    if (this.activate) return this.activate(event, eventInfo);
    
    if (this.action) return this.action();
    if (this.click) return this.click(event, eventInfo);
},


//>	@method	statefulCanvas.handleClick()	(A)
//			click event handler -- falls through to handleActivate.
//          Note: Does not call 'this.click' directly - this is handled by handleActivate
//		@group	event
//<
handleClick : function (event, eventInfo) {
    if (isc._traceMarkers) arguments.__this = this;

    // This is required to handle icon clicks on buttons, etc
    if (event.target == this && this.useEventParts) {
        if (this.firePartEvent(event, isc.EH.CLICK) == false) return false;
    }
    return this.handleActivate(event,eventInfo);    
},

//>	@method	statefulCanvas.handleKeyPress()	(A)
//			keyPress event handler.
//          Will call this.keyPress if defined on Space or Enter keypress, falls through 
//          to this.handleActivate().
//		@group	event
//<
handleKeyPress : function (event, eventInfo) {
    if (isc._traceMarkers) arguments.__this = this;

    if (this.keyPress && (this.keyPress(event, eventInfo) == false)) return false;
    
    if (event.keyName == "Space" || event.keyName == "Enter") {
        if (this.handleActivate(event, eventInfo) == false) return false;
    }
    
    return true;
    
}, 

// ---------------------------------------------------------------------------------------

// override destroy to removeFromRadioGroup - cleans up a class level pointer to this widget.
destroy : function () {
    this.removeFromRadioGroup();
    
    return this.Super("destroy", arguments);
}



});

// Add 'activate' as a stringMethod to statefulCanvii, with the same signature as 'click'
isc.StatefulCanvas.registerStringMethods({
    activate:isc.EH._eventHandlerArgString,  //"event, eventInfo"

    //> @method statefulCanvas.action()
    // This property contains the default 'action' for the Button to fire when activated.
    //<
    // exposed on the Button / ImgButton / StretchImgButton subclasses
    action:""
});

isc.StatefulCanvas.addClassMethods({

// build a properties object representing the border for supplied CSS class name
_buildBorderStyle : function (borderRadiusOnly, className, referenceElement) {

    // for performance, use cached border style results if present
    var classNameKey = borderRadiusOnly ? "$" + className : className;

    if (this._borderStyleCache[classNameKey]) {
        return this._borderStyleCache[classNameKey];
    }

    // if no cached results are present, we must recompute

    var maxProperties,
        borderStyle = {},
        setProperties = 0;

    // if widget has border specified, we need only propagate border radius
    maxProperties = borderRadiusOnly ? isc.StatefulCanvas._nRadiusBorderProperties : 
                                       isc.StatefulCanvas._borderProperties.length;

    var styleInfo = isc.Element.getStyleDeclaration(className, true, referenceElement);
    if (styleInfo) {
        for (var i = 0; i < styleInfo.length; i++) {
            for(var j = 0; j < maxProperties; j++) {
                var prop = isc.StatefulCanvas._borderProperties[j];
                
                if (borderStyle[prop] == null && styleInfo[i][prop] != isc.emptyString) {
                    borderStyle[prop] = styleInfo[i][prop];
                    setProperties++;
                }
            }
            if (setProperties == maxProperties) break;
        }
    }
    this._borderStyleCache[classNameKey] = borderStyle;
    return borderStyle;
},

// build an HTML string representing the border for supplied CSS class name
_getBorderCSSHTML : function (borderRadiusOnly, className) {

    // for performance, use cached border CSS HTML results if present
    var classNameKey = borderRadiusOnly ? "$" + className : className;

    if (this._borderCSSHTMLCache[classNameKey]) {
        return this._borderCSSHTMLCache[classNameKey];
    }

    // if no cached results are present, we must recompute
    var borderStyle = this._buildBorderStyle(borderRadiusOnly, className);

    var cssText = isc.emptyString,
        separator = isc.StatefulCanvas._$separator;

    // build border style for each possibly different edge

    var bottom = isc.SB.concat(
        borderStyle.borderBottomWidth, separator,
        borderStyle.borderBottomStyle, separator,
        borderStyle.borderBottomColor).trim();

    var left = isc.SB.concat(
        borderStyle.borderLeftWidth, separator,
        borderStyle.borderLeftStyle, separator,
        borderStyle.borderLeftColor).trim();

    var right = isc.SB.concat(
        borderStyle.borderRightWidth, separator,
        borderStyle.borderRightStyle, separator,
        borderStyle.borderRightColor).trim();

    var top = isc.SB.concat(
        borderStyle.borderTopWidth, separator,
        borderStyle.borderTopStyle, separator,
        borderStyle.borderTopColor).trim();

    // apply border styles separately if necessary, otherwise as simple border
    if (bottom != left || bottom != right || bottom != top) {
        if (bottom != isc.emptyString) cssText += isc.semi + "BORDER-BOTTOM:" + bottom;
        if (left   != isc.emptyString) cssText += isc.semi + "BORDER-LEFT:"   + left;
        if (right  != isc.emptyString) cssText += isc.semi + "BORDER-RIGHT:"  + right;
        if (top    != isc.emptyString) cssText += isc.semi + "BORDER-TOP:"    + top;
    } else {
        if (bottom != isc.emptyString) cssText += isc.semi + "BORDER:" + bottom;
    }

    var bl = borderStyle.borderBottomLeftRadius,
        br = borderStyle.borderBottomRightRadius,
        tr = borderStyle.borderTopRightRadius,
        tl = borderStyle.borderTopLeftRadius;

    // apply border radius separately if necessary, otherwise as simple border radius
    if (bl != br || bl != tr || bl != tl) {
        if (bl != null) cssText += isc.semi + "BORDER-BOTTOM-LEFT-RADIUS:"  + bl;
        if (br != null) cssText += isc.semi + "BORDER-BOTTOM-RIGHT-RADIUS:" + br;
        if (tr != null) cssText += isc.semi + "BORDER-TOP-RIGHT-RADIUS:"    + tr;
        if (tl != null) cssText += isc.semi + "BORDER-TOP-LEFT-RADIUS:"     + tl;
    } else {
        if (bl != null) cssText += isc.semi + "BORDER-RADIUS:" + bl;
    }

    this._borderCSSHTMLCache[classNameKey] = cssText;
    return cssText;
},

// clear the cache of per-class name CSS border objects and HTML strings
clearBorderCSSCache : function () {
    if (isc.StatefulCanvas.pushTableBorderStyleToDiv) {
        this._borderStyleCache   = {};
        this._borderCSSHTMLCache = {};
    }
},

// Similar logic for drop-shadows.
// These extend past the edges of the target element, so if we
// are applying styling to a table rendered within our handle, and our handle's overflow
// is hidden, we need to explicitly apply the shadow to the outer element.
_$boxShadowRegExp: new RegExp("(?:\\([^)]*\\)|[^,])+", "g"),
_buildShadowStyle : function (className, referenceElement) {

    var classNameKey = className;

    if (this._shadowStyleCache[classNameKey]) {
        return this._shadowStyleCache[classNameKey];
    }

    // We have just a single property to worry about - "boxShadow", so we could just
    // cache a string here. Using an object just in case we find we encounter other
    // css attributes which extend outside the box as shadows do for which we need to
    // apply the same workarounds.
    var shadowStyle = {},
        property = "boxShadow";

    var styleInfo = isc.Element.getStyleDeclaration(className, true, referenceElement);
    if (styleInfo) {
        for (var i = 0; i < styleInfo.length; i++) {
            if (shadowStyle[property] == null && styleInfo[i][property] != isc.emptyString) {
                shadowStyle[property] = styleInfo[i][property];
            }
        }
    }
    // Filter out inner box shadows (those specified with the 'inset' keyword).
    if (shadowStyle.boxShadow != null && shadowStyle.boxShadow.indexOf("inset") >= 0) {
        
        var shadowDefs = shadowStyle.boxShadow.match(this._$boxShadowRegExp).map("trim"),
            numShadowDefs = shadowDefs.length;
        var k = 0;
        for (var i = 0; i < numShadowDefs; ++i) {
            var shadowDef = shadowDefs[i];
            
            if (shadowDef.startsWith("inset") || shadowDef.endsWith("inset")) {
                ++k;
            } else if (k > 0) {
                shadowDefs[i - k] = shadowDef;
            }
        }
        
        shadowDefs.setLength(numShadowDefs - k);
        shadowStyle.boxShadow = shadowDefs.join(", ");
        
    }
    this._shadowStyleCache[classNameKey] = shadowStyle;
    return shadowStyle;
},

// build an HTML string representing the shadow for supplied CSS class name
_getShadowCSSHTML : function (className) {

    // for performance, use cached border CSS HTML results if present
    var classNameKey = className;

    if (this._shadowStyleCSSHTMLCache[classNameKey]) {
        return this._shadowStyleCSSHTMLCache[classNameKey];
    }

    // if no cached results are present, we must recompute
    var shadowStyle = this._buildShadowStyle(className);

    if (shadowStyle.boxShadow == null) return isc.emptyString;

    var cssText = "box-shadow:" + shadowStyle.boxShadow + ";";
    this._shadowStyleCSSHTMLCache[classNameKey] = cssText;

    return cssText;
},

// clear the cache of per-class name CSS shadow objects and HTML strings
clearShadowCSSCache : function () {
    if (isc.StatefulCanvas.pushTableShadowStyleToDiv) {
        this._shadowStyleCache   = {};
        this._shadowStyleCSSHTMLCache = {};
    }
}

});
