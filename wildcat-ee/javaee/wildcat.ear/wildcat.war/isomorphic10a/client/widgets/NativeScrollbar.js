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

 







//>	@class NativeScrollbar
//
// The NativeScrollbar widget will render in the browser as a native 
// scrollbar, and has APIs allowing it to be applied to scroll content any another widget
// on the page. Essentially this behaves similarly to the +link{Scrollbar} class but will
// be rendered as a native browser scrollbar rather than  using media, thus providing the
// advantages of an independant scrollbar (support for rendering the scrollbar separate from the
// content it effects, support for "virtual scrolling" mechanisms where content size is unknown
// at initial render, etc), with a native look and feel and without requiring the loading of
// additional media on the page.
// <P>
// To enable this for a component simply set +link{canvas.showCustomScrollbars} to true and
// set +link{canvas.scrollbarConstructor} to <code>"NativeScrollbar"</code>
//
// @treeLocation Client Reference/Foundation
// @visibility external
//<

// Implementation:
// The NativeScrollbar is overflow:"hidden". It renders an autoChild which shows native 
// scrollbars and has overflow set to "scroll".
// The autoChild is sized, and the NativeScrollbar is itself scrolled such that the
// scrollbars will show in the NativeScrollbar's viewport (while the rest of the auto-child is
// clipped).
// The autoChild is populated with content such that the scrollbars render at the expected
// size, and we react to the css scroll event by scrolling our scroll target.
// We share the same basic set of APIs as the Scrollbar widget so to get a native scrollbar
// appearance a widget needs to just set ScrollbarConstructor to "NativeScrollbar" (and
// showCustomScrollbars:true



isc.ClassFactory.defineClass("NativeScrollbar", "Canvas");

isc.NativeScrollbar.addClassProperties({
    // NativeScrollbars will *always* render at the native browser SB size + 1px for the
    // spacer content.
    getScrollbarSize : function () {
        return isc.Element.getNativeScrollbarSize();
    }
});

isc.NativeScrollbar.addProperties({
    
    vertical:true,
    
    showCustomScrollbars:false,
    overflow:"hidden",
    
    //>	@attr nativeScrollbar.autoEnable (boolean : true : [IRWA])
    // If true, this scrollbar will automatically enable when the scrollTarget is
    // scrollable (i.e., when the contents of the scrollTarget exceed its clip size in the
    // direction relevant to this scrollbar), and automatically disable when the
    // scrollTarget is not scrollable. Set this property to false for full manual control
    // over a scrollbar's enabled state.
    // @visibility internal
    //<
    
	autoEnable:true,
	
    //scrollTarget:null,

    initWidget : function () {
        
        var scrollThickness = isc.NativeScrollbar.getScrollbarSize();
        // disallow a width/height that is not equal to native scrollbar size
        
        if (this.vertical) {
            this.setWidth(scrollThickness);
        } else {
            this.setHeight(scrollThickness);
        }
        
        this.setOverflow(isc.Canvas.HIDDEN);
        
        this.addAutoChild("scrollbarCanvas");
        this.sizeScrollbarCanvas();
        
        // initialize us for our scrollTarget
        this.setScrollTarget();
    
        // call setThumb to figure out how big and where the scrollbar thumb should be
        // note: this will enable and disable the scrollbar if autoEnable is true
        this.setThumb();
    },
    
    
    // scrollbarCanvas autoChild
    // This is a child which actually shows native scrollbars.
    // We will clip this widget in our viewport to get the appearance we want,
    // and we'll respond to the user interacting with (scrolling) this widget to give us
    // the scroll events we need.
    scrollbarCanvasDefaults:{
        overflow:"scroll",
        showCustomScrollbars:false,
        
        // Respond to a user scrolling this scrollbar by scrolling our scroll target
        _handleCSSScroll : function (waited, fromFocus) {
            this.Super("_handleCSSScroll", arguments);
            
            if (isc.Browser.isMoz && !waited && (fromFocus ||  isc.Browser.geckoVersion < 20030312))
            {
                return;
            }
            if (this._scrollingHandleDirectly) return;
            this.creator.scrollbarCanvasScrolled();
        },
        
        // On scrollbar resize, the scrollbarCanvas needs to also resize to stay at the
        // correct dimensions
        parentResized : function () {
            this.creator.sizeScrollbarCanvas();
            this.creator.adjustOverflow();
        }
        
    },
    // The scrollbarCanvas will show H and V scrollbars
    // Size it to essentially match our length and be an arbitrary width (larger than the
    // native scrollbar thickness).
    // Everything but the scrollbar we're interested in will be clipped.
    scrollbarCanvasThickness:100,
    sizeScrollbarCanvas : function () {
        var scrollThickness = isc.Element.getNativeScrollbarSize();
        // native overflow:"scroll" always shows a corner at the bottom/right edge
        // (since there are 2 sb's showing).
        // size the sb lengthwise to clip the corner if showCorner is false
        
        var width = this.vertical ? this.scrollbarCanvasThickness 
                      : this.getInnerWidth() + (this.showCorner ? 0 : scrollThickness),
            height = !this.vertical ? this.scrollbarCanvasThickness 
                      : this.getInnerHeight() + (this.showCorner ? 0 : scrollThickness);
        this.scrollbarCanvas.resizeTo(width,height);
    },
    _adjustOverflow : function () {
        this.Super("_adjustOverflow", arguments);
        // We have to keep the bottom/right edge of the scrollbarCanvas actually in view.
        // Negative top/left position on the scrollbarCanvas isn't allows so just scroll 
        // ourselves to the bottom/right
        
        if (this.vertical) {
            this.scrollToTop();
            this.scrollToRight();
        } else {
            this.scrollToLeft();
            this.scrollToBottom();
        }
    },
    
    
    //>	@method	nativeScrollbar.setScrollTarget() ([])
    //          Sets or clears the scrollbar's scrollTarget. If no argument is provided, then the
    //          scrollTarget will be set to the scrollbar itself.
    //
    //      @visibility external
    //      @group  scroll
    //		@param	[newTarget]		(Canvas)	target canvas to be scrolled
    //<
    
    setScrollTarget : function (newTarget) {
    
        // If we have been given a newTarget, stop observing the current scrollTarget that we're
        // observing.
        if (this._selfManaged && 
             this.scrollTarget != null && 
             this.isObserving(this.scrollTarget, "scrollTo")) {
            //stop observing (current) this.scrollTarget
            this.ignore(this.scrollTarget, "scrollTo");
        }
            
        // If a newTarget was specified, set the scrollTarget to it.
        // If a newTarget was not specified, we'll use the current scrollTarget. If the
        // current scrollTarget isn't set, we use the scrollBar itself to avoid
        // null pointers
        if (newTarget != null) this.scrollTarget = newTarget;
        if (this.scrollTarget == null) this.scrollTarget = this;
        
        // We now are sure that we have a scrollTarget. If the scrollTarget has been changed
        // then we re-observe it. Otherwise, we're done.
        // if we've got a scrollTarget and we weren't created by adjustOverflow in the target,
        //	we should observe the _adjustOverflow method of the target to make sure the
        //	size of the thumb matches the visible portion of the target.
        if (this._selfManaged &&
             this.scrollTarget != this &&
             this.scrollTarget != newTarget) {
            this.observe(this.scrollTarget, "scrollTo", "observer.setThumb()");
        }
    
    },

    //>	@method	nativeScrollbar.setThumb()	(A)
    // Resize the thumb so that the thumb's size relative to the track reflects the viewport size
    // relative to the overall scrollable area.
    //		@param	forceResize		(boolean)	if true, resize regardless of whether it is necessary
    //<
    setThumb : function () {
        // used when animating components for performance
        if (this._suppressSetThumb) return;
        var sbc = this.scrollbarCanvas,
        
            scrollingOn = (this._selfManaged || this.scrollTarget.canScroll(this.vertical)),
         
            spacerWidth = 1,
            spacerHeight = 1;
        if (scrollingOn) {
            
        
            // calculate size for thumb
            // NOTE: We use 'getViewportRatio()' here - this is required rather than looking at
            // the target's scroll height etc directly, for virtual scrolling
            var ratio = this.scrollTarget.getViewportRatio(this.vertical);
            
            // basically viewportHeight/scrollHeight = viewportRatio - so make content (scrollHeight)
            // into viewportHeight / ratio
            var viewportSize = (this.vertical ? sbc.getViewportHeight() : sbc.getViewportWidth()),
                spacerLength = Math.round(viewportSize / ratio);
                
            if (this.vertical) spacerHeight = spacerLength;
            else spacerWidth = spacerLength;
        }
        if (sbc.spacerLength != spacerLength) {  
            sbc.setContents(isc.Canvas.spacerHTML(spacerWidth,spacerHeight));
            sbc.spacerLength = spacerLength;
        }
        
        // now move the thumb according to the scroll
        this.moveThumb();
    },
    
    //>	@method	nativeScrollbar.setVisibility()	(A)
    // Extended to ensure thumb is placed correctly when this scrollbar is shown.
    //		@group	visibility
    //
    //		@param	newState		(boolean)	new visible state
    //<
    setVisibility : function (newState,b,c,d) {
        this.invokeSuper(isc.Scrollbar, "setVisibility", newState,b,c,d);
        if (this.isVisible()) this.setThumb();
    },
    
    //>	@method	nativeScrollbar.parentVisibilityChanged()	(A)
    // Extended to ensure thumb is placed correctly when this scrollbar is shown due to a hidden
    // ancestor being shown.
    //		@group	visibility
    //
    //		@param	newState		(boolean)	new visible state
    //<
    parentVisibilityChanged : function (newState,b,c,d) {
        this.invokeSuper(isc.Scrollbar, "parentVisibilityChanged", newState,b,c,d);
        if (this.isVisible()) this.setThumb();
    },

    
    // updates the thumb's position to match the scrollLeft / scrollTop of the scroll target
    moveThumb : function () {

        var scrollRatio = this.scrollTarget.getScrollRatio(this.vertical);
        
        var sbc = this.scrollbarCanvas;
        
        // scrollRatio is basically scrollTop / (scrollHeight - viewportHeight)
        // so to get scrollTop we need scrollRatio * (scrollHeight - viewportheight)
        var scrollableLength = this.vertical ? sbc.getScrollHeight() - sbc.getViewportHeight()
                                            : sbc.getScrollWidth() - sbc.getViewportWidth(),
                                            
            scrollPosition = Math.round(scrollRatio * scrollableLength);
            
        sbc.scrollTo(this.vertical ? 0 : scrollPosition, this.vertical ? scrollPosition : 0);
        
        
    },
    
    scrollbarCanvasScrolled : function () {
        var sbc = this.scrollbarCanvas,
            ratio = this.vertical ?
                    sbc.getScrollTop() / (sbc.getScrollHeight() - sbc.getViewportHeight()) :
                    sbc.getScrollLeft() / (sbc.getScrollWidth() - sbc.getViewportWidth());
                            
        // Use scrollToRatio rather than straight "scrollTo()"
        // This allows virtual scrolling to work on the target (in GridRenderers)
        this.scrollTarget.scrollToRatio(this.vertical, ratio);
    },      
   
    
    // For now NativeScrollbars don't support showing corners
    setShowCorner : function (showCorner) {
        this.showCorner = showCorner;
        this.sizeScrollbarCanvas();
    }

    

});


