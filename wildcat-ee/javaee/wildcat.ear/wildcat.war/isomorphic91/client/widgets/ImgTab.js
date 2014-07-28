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

 






//>	@class	ImgTab
// Specialized StretchImgButton used by TabSet/TabBar for tabs
//
// @treeLocation Client Reference/Foundation
// @visibility external
//<

// class for Stretchable image buttons
isc.ClassFactory.defineClass("ImgTab", "StretchImgButton");

// add properties to the class
isc.ImgTab.addProperties({
    //>	@attr	isc.ImgTab.capSize		(number : 2 : IRW)
	// How big are the end pieces by default
	// @group appearance
    // @visibility external
	//<
	capSize:2,

    //>	@attr	isc.ImgTab.skinImgDir		(URL : "images/Tab/" : IRW)
	// Base path for the images.   <B>Note</B> that when used within a TabSet, the
    // +link{tabSet.tabBarPosition} is appended as an additional path segment, yielding
    // "images/Tab/top/" et al.
    //
    // @visibility external
	//<
	skinImgDir:"images/Tab/",
	
	//> @attr isc.ImgTab.labelSkinImgDir (URL : "images/" : IRW)
	// Base path for images shown within this ImgTab's label. This will be used for
	// icons (such as the close icon) by default.
	// @visibility external
	//<
	labelSkinImgDir:"images/",
	
    //> @attr ImgTab.baseStyle (CSSStyleName : "tab" : IR)
    // @visibility external
    //<
    baseStyle:"tab",

    //> @attr ImgTab.titleStyle (CSSStyleName : null : IR)
    // Like +link{StretchImgButton.titleStyle}, can set to provide a separate style for the
    // title text.
    // <P>
    // If set and the ImgTab is +link{StretchImgButton.vertical,vertical}, a "v" will be
    // automatically prepended to the style name (hence "tabTitle" -> "vtabTitle").
    //
    // @visibility external
    //<

    //>	@attr	isc.ImgTab.src		(URL : "tab.gif" : IRW)
	// Base URL for tab images
    // @visibility external
	//<
	src:"[SKIN]tab.gif",				

    //>	@attr	isc.ImgTab.showRollOver		(Boolean : false : IRW)
	// Should we visibly change state when the mouse goes over this tab
    // @visibility external
	//<
	showRollOver:false,					

    //>	@attr	isc.ImgTab.showFocus    (boolean : true : IRW)
	// Should we visibly change state when the tab receives keyboard focus?
    // @deprecated as of SmartClient 6.1 in favor of +link{imgTab.showFocused}
    // @visibility external
	//<
    //>	@attr	isc.ImgTab.showFocused    (Boolean : true : IRW)
	// Should we visibly change state when the tab receives keyboard focus?
    // @visibility external
	//<
	showFocused:true,

    //>	@attr	isc.ImgTab.align		(Alignment : isc.Canvas.CENTER : IRW)
	// Alignment of title text
	//		@group	positioning
    // @visibility external
	//<
    // agrees with superclass
	//align:isc.Canvas.CENTER,

	//>	@attr	isc.ImgTab.valign		(VerticalAlignment : isc.Canvas.CENTER : IRW)
	// Vertical alignment of title text.
	//		@group	positioning
	//<
    // agrees with superclass
	//valign:isc.Canvas.CENTER,

    //>	@attr	isc.ImgTab.actionType		(ButtonActionType : isc.Button.BUTTON : IRWA)
	//			button behavior -- BUTTON, RADIO or CHECKBOX
	//<                                        
	actionType:isc.Button.RADIO,
    
    
    mozOutlineOffset:"0px"
});

isc.ImgTab.addProperties({

    //>EditMode 
    // needed so that we can autodiscover this method to update the pane.
    setPane : function (pane) {
        this.parentElement.parentElement.updateTab(this, pane);
    }, 
    // needed to allow a zero-parameter action for selecting a tab
    selectTab : function () {
        this.parentElement.parentElement.selectTab(this);
    },
    //<EditMode
    
    initWidget : function (a,b,c,d,e,f) {    
        if (this.vertical && this.titleStyle) this.titleStyle = "v" + this.titleStyle;
        return this.invokeSuper(isc.ImgTab, this._$initWidget, a,b,c,d,e,f);
    }
});
