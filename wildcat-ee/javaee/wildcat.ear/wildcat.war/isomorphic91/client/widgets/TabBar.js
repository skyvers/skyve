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

 





//>	@class	TabBar
// Shows a set of Tabs.  TabBars are automatically created by TabSets and shouldn't be used
// directly.  The TabBar is documented for skinning purposes.
// 
// @treeLocation Client Reference/Layout/TabSet
// @visibility external
//<
isc.ClassFactory.defineClass("TabBar", "Toolbar");

isc.TabBar.addProperties({
    //>	@attr	isc.TabBar.tabs		(Array of Tab Properties : null : IR)
    // Tab for this TabBar.
    // @visibility external
    //<

    //>	@attr	isc.TabBar.breadth	(number : 21 : IRW)
    // Breadth of the tabBar (including baseline breadth)
    // @visibility external
    //<
    breadth: 21,

    //>	@attr	isc.TabBar.buttonConstructor	(class: ImgTab : AIRW)
    // SmartClient component used for the tabs of the tabBar. 
    // Must be Button or Button subclass.
    // @visibility external
    //<
    // Note - if this TabBar is part of a TabSet, this constructor can be overridden by setting 
    // 'useSimpleTabs' on the TabSet - will use buttons instead, styled via CSS to look like
    // tabs.
    buttonConstructor:isc.ImgTab,

    // We want to have arrow keys, not tab-keypresses, move between tabs
    tabWithinToolbar:false,

    // override keyPress - in addition to shifting focus on arrow keypress we want to
    // actually select the new tab
    
    keyPress : function () {
        var keyName = this.ns.EH.lastEvent.keyName;
        
        
        if ((this.vertical && keyName == "Arrow_Up") || 
            (!this.vertical && keyName == "Arrow_Left")) 
        {
            this._selectNextTab(false);
        } else if ((this.vertical && keyName == "Arrow_Down") || 
                   (!this.vertical && keyName == "Arrow_Right"))
        {
            this._selectNextTab(true);
        }
    },

    _selectNextTab : function (forward, start) {
        if (start == null) start = this.getSelectedTab();
        var step = forward ? 1 : -1;
        var target = start;
        do {
            if (target < 0) {
                target = this.buttons.length;
            } else if (target >= this.buttons.length) {
                target = -1;
            }
            target += step;
        } while (target != start && 
            (this.getMember(target) == null || this.getMember(target).isDisabled()));
        if (target != start && this.getMember(target)) {
            this.selectTab(target);
            // Explicitly focus in the tab too, if selection wasn't rejected via a
            // tabDeselected handler.
            if (this.getSelectedTab() == target) {
                var button = this.getMember(target);
                button.focus();
            }
        }
    },

    //>	@attr	isc.TabBar.skinImgDir		(URL : "images/Tab/" : AIRW)
    //			base path for the tab images, if an image-based
    //			tab is being used.
    //		@group skins, files
    //<
    skinImgDir:"images/Tab/",			
										
    // --------------------------------------------------------------------------------------------
    //> @attr tabBar.showMoreTab (boolean : null : IR)
    // Should tabs exceeding +link{moreTabCount} be shown on a "more" tab?
    // <p>
    // This setting is used to emulate an iPhone-style tab bar "more" button.
    // @visibility external
    //<

    //> @attr tabBar.moreTabCount (int : 5 : IR)
    // This property defines the number tab buttons that should be shown before
    // automatically adding a "more" button to handle the remaining tabs. This
    // property is only used when +link{showMoreTab} is enabled.
    // @visibility external
    //<
    moreTabCount:5,

    //> @attr tabBar.moreTab (Tab : null : IR)
    // Tab to show as the "more" tab when +link{showMoreTab} is enabled and the number
    // of tabs to show exceeds +link{moreTabCount}.
    // @visibility external
    //<

    // Baseline
    // --------------------------------------------------------------------------------------------
    
    //> @groupDef baseLine
    // The baseLine is StretchImg that is placed along the edge of the TabBar that borders on
    // the pane, occluding the pane's actual border but matching it exactly.  The selected tab
    // is in front of the baseLine, and the rest are behind it.
    // @visibility external
    //<

    //>	@attr isc.TabBar.baseLineThickness (number : 1 : IR)
    // Thickness of the baseLine, in pixels.  This should be set to match the media specified
    // by +link{baseLineSrc}.  The baseLineThickness also determines the degree of overlap with
    // the TabSet's paneContainer when using decorative edges - see +link{TabSet.paneContainer}
    // for details.
    // 
    // @group baseLine 
    // @visibility external
    //<
    baseLineThickness:1,

    //>	@attr isc.TabBar.baseLineSrc	(SCImgURL : "[SKIN]baseline.gif" : IR)
    // Sets +link{stretchImg.src} for the +link{group:baseLine} StretchImg.
    // @group baseLine 
    // @visibility external
    //<
    baseLineSrc:"[SKIN]baseline.gif",

    //>	@attr isc.TabBar.baseLineCapSize	(number : 2 : IR)
    // Set +link{stretchImg.capSize} for the +link{group:baseLine} stretchImg.
    // @group baseLine
    // @visibility external
    //<
    baseLineCapSize:2,

    // Positioning and Alignment
    // --------------------------------------------------------------------------------------------
    //>	@attr	isc.TabBar.tabBarPosition	(Side : isc.Canvas.TOP : IRW)
    // Position of the tabBar in relation to whatever it controls.
    //<
    // Not doc'd, do via TabSet
    tabBarPosition:isc.Canvas.TOP,

    // --------------------------------------------------------------------------------------------
    //>	@attr	isc.TabBar.selectedTab		(number : 0 : IR)
    // Index of the initially selected tab.  Settable at initialization only, afterwards, call
    // +link{selectTab}.
    //<
    // Not doc'd, do via TabSet
    selectedTab:0,

    //>	@attr	isc.TabBar.defaultTabSize		(number : 80 : IR)
    // Default size (length) in pixels for tabs within this tabBar
    // @visibility external
    //<    
    defaultTabSize:80
										
    //>	@attr	TabBar.tabDefaults		(Tab : 0 : AIR)
    //			Defaults applied to tabs on init.
    //			Note that these are overlaid over the superclass property
    //			toolBar.buttonDefaults.
    //<
    // Null by default - we will set this up in initWidget - this avoids multiple tabBars
    // pointing to the same tabDefaults object
    //tabDefaults:{}

});


isc.TabBar.addMethods({
//>	@method	tabBar.initWidget()	(A)
// Initialize the TabBar object.
// <p>
// Setup special button properties and create the baseLine
//<
initWidget : function () {
    // if the name of the pane property of a tab is specified as a string, check it
    // now, and reassign.
    for (var i = 0; i < this.tabs.length; i++) {
        var pane = this.tabs[i].pane;
    	
        if (isc.isA.String(pane) && isc.isA.Canvas(window[pane])) {
            this.tabs[i].pane = window[pane];
        }
        
    }
    
    // copy tabs over to the buttons array, which is what the superclass uses.
    this.buttons = this.tabs;
    
    if (this.moreTab) {
        this._moreTabIndex = this.buttons.length;
        this.buttons[this._moreTabIndex] = this.moreTab;
    }

    // Note that the order of the tabs can be reversed by setting the 'reverseOrder' property
    // on this tabBar [can be done in tabBarDefaults] if this is zrequired.
    
    // set up the skinImgDir for the baseline
    this.skinImgDir = this.skinImgDir + this.tabBarPosition + "/";
    
    var tabDefaults = this.tabDefaults;
    if (tabDefaults == null) tabDefaults = this.tabDefaults = {};

    // tabs are created as "buttons" by Toolbar superclass code; to have tabDefaults applied to
    // each button, assign to buttonDefaults.
    // NOTE: if we add properties directly to the buttonDefaults object, we'll side effect all
    // Toolbars
    tabDefaults = this.buttonDefaults = isc.addProperties({}, this.buttonDefaults, tabDefaults);

    // tabs are mutually exclusive
    tabDefaults.actionType = isc.StatefulCanvas.RADIO;
    
    // Default tabs to defaultTabWidth
    if (this.vertical) {
        tabDefaults.defaultHeight = this.defaultTabSize;
    } else {
        tabDefaults.defaultWidth = this.defaultTabSize;
    }
    // .. and allow them to expand to fit content
    tabDefaults.overflow = isc.Canvas.VISIBLE;

    // set tab style information directly on the (presumed) ImgTab, so that it may be
    // overridden by the user in tabProperties.
    tabDefaults.vertical =
        (this.tabBarPosition == isc.Canvas.LEFT || this.tabBarPosition == isc.Canvas.RIGHT);
    
    // skinImgDir: For image based tabs, we need to update the skin img dir to account for
    // tab orientation.
    // Note that we *don't* want per-side media for standard icons like the close icon.
    // Therefore we don't want to modify the skinImgDir if we're using simple tabs.
    // In ImgTabs we handle this by having a separate labelSkinImgDir
    var buttonClass = isc.ClassFactory.getClass(this.buttonConstructor);
    if (buttonClass && buttonClass.isA("ImgTab")) {
        tabDefaults.skinImgDir = buttonClass.getInstanceProperty("skinImgDir") +
            this.tabBarPosition + "/";
    }
    
    // Channel the icon mouseDown event through to the tabIcon click handler
    // (Used for [eg] closing tabs)
    
    tabDefaults.iconMouseDown = this._tabIconClickHandler;
    
    // Override the click and doubleClick handler as necessary to implement title editing
    // Note if _editTabTitle returns false this indicates we're editing the title - in this
    // case suppress any doubleClick etc handler defined by the developer directly on the
    // tab
    tabDefaults.handleDoubleClick = function () {
        var tabSet = this.parentElement.parentElement;
        if (tabSet && tabSet.titleEditEvent == "doubleClick" && tabSet._editTabTitle(this)) return;
        return this.Super("handleDoubleClick", arguments);
    }
    tabDefaults.handleClick = function () {
        var tabSet = this.parentElement.parentElement;
        if (tabSet && tabSet.titleEditEvent == "click" && tabSet._editTabTitle(this)) return;
        return this.Super("handleClick", arguments);
    },

    tabDefaults._generated = true;
    
    var perSideStyleProperty = this.tabBarPosition + "StyleName";
    if (this[perSideStyleProperty]) this.setStyleName(this[perSideStyleProperty]);

    this.Super(this._$initWidget);

    if (this._baseLine == null) this.makeBaseLine();
},

isShowingMoreTab : function () {
    return (this.showMoreTab &&
        this.moreTab &&
        this._moreTabIndex >= 0 &&
        this.getMembers(this._moreTabIndex).isVisible &&
        this.getMembers(this._moreTabIndex).isVisible()
    );
},

// _tabIconClickHandler - method applied directly to the tabs
_tabIconClickHandler : function () {
    return this.parentElement.tabIconClick(this);
},

tabIconClick : function (tab) {
    
    var ts = this.parentElement;
    return ts._tabIconClick(tab);
   
},


_clearSgwtTabReferences : function () {
    var liveButtons = this.getMembers();
    for (var i = 0; i < liveButtons.length; i++) {
        if (window.SmartGWT.isTab(liveButtons[i].__ref)) {
            liveButtons[i].__ref = null;
            delete liveButtons[i].__module;
        }
    }
},

// Override to add "more" button and hide buttons that are now on "more" tab
setButtons : function (newButtons) {

    this.Super("setButtons", arguments);
       
    if (isc.Browser.isSGWT) this._clearSgwtTabReferences();
    
    // If "more" tab is enabled and needed, hide the tabs that will show on the "more" tab
    if (this.showMoreTab && this.buttons.length-1 > this.moreTabCount) {
        for (var i = this.moreTabCount-1; i < this.buttons.length; i++) {
            this.getMember(i).hide();
        }
        // Make "more" tab visible. It will be hidden above
        this.getMember(this._moreTabIndex).show();
    } else if (this.showMoreTab && this.moreTab) {
        this.getMember(this._moreTabIndex).hide();
    }
},

// override makeButton to show the icon for the button
// Also set up locator parent for autoTest APIs
makeButton : function (properties, a,b,c,d) {
    var canClose = this.parentElement.canCloseTab(properties),
        
        properties = isc.addProperties({}, properties, this.getCloseIconProperties(properties, canClose));
    properties.locatorParent = this.parentElement;

    return this.invokeSuper("TabBar", "makeButton", properties, a,b,c,d);
},

getCloseIconProperties : function(properties, canClose) {
    var override = {};
    if (properties.canClose == true || (properties.canClose == null && canClose)) {
        override.icon = (properties.closeIcon || this.parentElement.closeTabIcon);
        override.iconSize = (properties.closeIconSize || this.parentElement.closeTabIconSize);
        // close icon should appear at the end of the tab
        
        override.iconOrientation = "right";
        override.iconAlign = override.iconOrientation;
        
    } else {
        // Explicitly override icon-related properties to the tab properties passed in.
        // This is so we can use TabSet.setCanCloseTab() to toggle the canClose state of a 
        // live Tab without touching any other properties on the live object.
        override.icon = (properties.icon);
        override.iconSize = (properties.iconSize);
        if (properties.iconOrientation != null) override.iconOrientation = properties.iconOrientation;
        if (properties.iconAlign != null) override.iconAlign = properties.iconAlign;
    }
    return override;
},

addTabs : function (tabs, position) {
    if (!position && this.tabBarPosition == isc.Canvas.LEFT) position = 0;
    this.addButtons(tabs, position);

    if (isc.Browser.isSGWT) this._clearSgwtTabReferences();
    
    // Hide any new buttons that belong on "more" tab and show "more" if needed
    if (this.showMoreTab && this.moreTab) {
        var buttons = this.getMembers();
        if (buttons.length-1 > this.moreTabCount) {
            for (var i = this.moreTabCount-1; i < buttons.length; i++) {
                buttons[i].hide();
            }
            this._moreTabIndex = buttons.length - 1;
            buttons[this._moreTabIndex].show();
        }
    }
    // ensure the tabs initially show up behind the baseline
    if (this._baseLine != null) {
        this._baseLine.bringToFront();        
        var selectedTab = this.getButton(this.getSelectedTab());
    	if (selectedTab) selectedTab.bringToFront();
    }
},


removeTabs : function (tabs, dontDestroy) {
    // get the list of tab widgets to be removed
    if (tabs == null) return;
    if (!isc.isAn.Array(tabs)) tabs = [tabs];
    var tabWidgets = this.map("getButton", tabs);

    // remove the tabs
    this.removeButtons(tabs);
    
    if (this.showMoreTab && this.moreTab && this._moreTabIndex > 0) {

        var buttons = this.getMembers();
        for (var i = 0; i < buttons.length; i++) {
            if (i < this.moreTabCount) buttons[i].show();
            else buttons[i].hide();
        }
        if (buttons.length-1 <= this.moreTabCount) {
            // Don't need more tab anymore. Make sure all tabs are shown
            // and more tab is hidden.
            this._moreTabIndex = null;
            buttons[buttons.length-1].hide();
        } else {
            this._moreTabIndex = buttons.length-1;
        }
    }

    // destroy each of the buttons we removed; it's appropriate/okay to do this because the buttons
    // were automatically created by this tabBar
    if (!dontDestroy) {
        for (var i = 0; i < tabWidgets.length; i++) {
            if (tabWidgets[i] != null) tabWidgets[i].destroy();
        }
    }
},

reorderTab : function (tab, moveToPosition) {
    var button = this.getButton(tab);
    if (button) {
        this.removeTabs(tab, true);
        this.addTabs(tab, moveToPosition);
    }
},

//> @method tabBar.draw()	(A)
// Extended to do layout and handle the selected tab.
// @group drawing
//<
draw : function (a,b,c,d) {
    arguments.__this = this;

    this.fixLayout();

    this.invokeSuper(isc.TabBar, "draw", a,b,c,d);
    this.bringToFront();

    var selectedTab = this.getButton(this.selectedTab);  
    // now that the buttons have all drawn, bring the baseline in front of them, then count on
    // the setSelected() behavior to bring the selected tab in front of the baseLine
    if (selectedTab) {
        
        selectedTab.setSelected(true);
    }
},

//> @method tabBar.makeBaseLine()	(A)
//  The baseline exists to create the appearance that one of the tabs is part of the pane whereas
//  the other tabs are behind the pane.
//
//	The baseline is an image that runs along the edge of the TabBar that borders on the pane,
//  occluding the pane's actual border but matching it exactly.  The selected tab is in front
//  of the baseline, and the rest are behind it.
//<


makeBaseLine : function () {
    // create the baseline stretchImg and add as child.
    this._baseLine = this.addAutoChild("baseLine", {
        ID:this.getID() + "_baseLine",
        vertical:(this.tabBarPosition == isc.Canvas.LEFT || 
                  this.tabBarPosition == isc.Canvas.RIGHT),
        skinImgDir:this.skinImgDir,
        src:this.baseLineSrc,
        capSize:this.baseLineCapSize,
        imageType:isc.Img.STRETCH,
        overflow:"hidden", // since the baseline can be a Canvas if it doesn't need to display images
        addAsChild:true,
        autoDraw:false
    }, isc.StretchImg);
    this.ignoreMemberZIndex(this._baseline);
},

// when scrolling shift the baseLine so it's always inside the viewport.
scrollTo : function (x,y,a,b,c,d) {
    this.invokeSuper(isc.TabBar, "scrollTo", x,y,a,b,c,d);
    if (this._baseLine) this.fixLayout();
},


//> @method tabBar.fixLayout()	(A)
//  Places the baseLine on the side of the TabBar adjacent to the tabbedPane, according to which
//  side the tabs are on.
//<
fixLayout : function () {
    var bl = this._baseLine;

    if (bl == null) return;
	
    
    var ts = this.parentElement,
        //edge = ts ? ts._edgedCanvas : null,
        edgeOffset = 0;
        
    
    
    // Canvas.TOP
    if (this.tabBarPosition == isc.Canvas.TOP) {
        //edgeOffset = edge ? edge._rightMargin : 0; // HACK 040910
        bl.setRect(this.getScrollLeft(), this.getHeight() - this.baseLineThickness, 
               this.parentElement.getWidth()-edgeOffset, this.baseLineThickness);
	
    // Canvas.BOTTOM
    } else if (this.tabBarPosition == isc.Canvas.BOTTOM) {
        //edgeOffset = edge ? edge._leftMargin : 0; // HACK 040910
        bl.setRect(this.getScrollLeft(), 0, this.parentElement.getWidth()-edgeOffset, this.baseLineThickness);
	
    // Canvas.LEFT
    } else if (this.tabBarPosition == isc.Canvas.LEFT) {
        //edgeOffset = edge ? edge._bottomMargin : 0; // HACK 040910
        bl.setRect(this.getWidth() - this.baseLineThickness, this.getScrollTop(), 
                   this.baseLineThickness, this.parentElement.getHeight()-edgeOffset);

    // Canvas.RIGHT
    } else if (this.tabBarPosition == isc.Canvas.RIGHT) {
        //edgeOffset = edge ? edge._bottomMargin : 0; // HACK 040910
        bl.setRect(0, this.getScrollTop(), this.baseLineThickness, this.parentElement.getHeight()-edgeOffset);
    }	
}, 

// fix layout on a change of size
layoutChildren : function (a,b,c,d) {
    this.invokeSuper(isc.TabBar, "layoutChildren", a,b,c,d);
    this.fixLayout();
},

//> @method tabBar.buttonSelected()	(A)
// This method overrides the toolBar's buttonSelected method.
// Differences are as follows:
//   - assumes tab is of type "radio", as all tabBar tabs are set to be a radiogroup
//   - handles z-axis reorderings of tabs and baseLine
//   - overlaps tabs by expanding and contracting them
//
// Note: we assume here that buttonSelected is only fired when the button is initially
//       set to "selected." Repeated clicks should not fire this method.
//       This assumption can be overridden by setting allowButtonReselect:true.
//
// @param tab (tab)  tab that has been selected.
//<
buttonSelected : function (tab) {    
    this.ignoreMemberZIndex(tab);
    
    // bring tab and label to front.	
    tab.bringToFront();

    // store the currently selected tab as the lastSelectedButton.
    this.lastSelectedButton = tab;
    
    // Make sure we never tab to an unselected button
    // Note that deselection of the other tabs is handled by built in Toolbar / Radiogroup
    // behavior.
    this._updateFocusButton(this.lastSelectedButton);
        
},

// Override buttonDeselected() to send the tab to the back (behind the baseLine image)
buttonDeselected : function (tab) {
    tab.sendToBack();
    this.stopIgnoringMemberZIndex(tab);
},

//> @method tabBar.getSelectedTab()	(A)
// Get the tab object currently selected.
// @return
//    tab object
//<
getSelectedTab : function () {
    return this.getButtonNumber(this.getSelectedButton());
},

//> @method tabBar.selectTab()	
// Select a tab
// @param  tabNum  (number)    index of tab to select
// @visibility external
//<
selectTab : function (tabNum) {    
    this.selectedTab = tabNum;
    this.selectButton(tabNum);
},

// Override setupButtonFocusProperties to ensure that this.selectedTab is the initial
// focusButton (will then be selected by _updateFocusButton())
setupButtonFocusProperties : function () {
    // sync up the focus with the selection
    
    this._updateFocusButton(this.getButton(this.selectedTab));
    return this.Super("setupButtonFocusProperties", arguments);

},

_scrollForward : function (backward, animated) {
    if (this.overflow == isc.Canvas.VISIBLE || !this.members || this.members.length == 0) return;
    var nextTab, nextTabEdge;
    
    // If we're in the process of scrolling to a tab - jump straight to the one after it
    if (this._scrollingToTab != null) {
        nextTab = this.members[this._scrollingToTab + (backward ? -1 : 1)];
        if (nextTab == null) {
            return;
        }
        nextTabEdge = (backward ? (this.vertical ? nextTab.getTop() : nextTab.getLeft())
                                : (this.vertical ? nextTab.getBottom() : nextTab.getRight()));
    } else {
        
        var scrollSize = (this.vertical ? this.getScrollHeight() : this.getScrollWidth());
        if (scrollSize <= (this.vertical ? this.getViewportHeight() : this.getViewportWidth())) 
            return;
    
        var scrollStart = (this.vertical ? this.getScrollTop() : this.getScrollLeft()),
            viewSize = (this.vertical ? this.getViewportHeight() : this.getViewportWidth());
    
        
        var scrollThreshold = 5;        
        for (var i = 0; i < this.members.length; i++) {
            nextTab = (backward ? this.members[this.members.length - (i+1)] : this.members[i]);
            // Note if the member order is reversed we look at the left where normally
            // we'd look at the right, etc.
            var clipBackward = backward;
            if (this.reverseOrder) clipBackward = !clipBackward
            nextTabEdge = (clipBackward ? (this.vertical ? nextTab.getTop() : nextTab.getLeft())
                                    : (this.vertical ? nextTab.getBottom() : nextTab.getRight()));
                                    
            // RTL mode - have to map from specified left (negative value) to scroll position
            // (positive value)
            if (!this.vertical && this.isRTL()) {
                nextTabEdge = this._shiftScrollLeftOrigin(nextTabEdge, false);
            }                                   
            // Determine which tab is currently clipped
            var clipped = clipBackward ? (nextTabEdge + scrollThreshold < scrollStart) 
                                   : (nextTabEdge - scrollThreshold > (scrollStart + viewSize));
            if (clipped) break;
        }
    }  
    
    if (animated) {
        this._scrollingToTab = this.members.indexOf(nextTab);
        this.scrollTabIntoView(nextTab, backward, true, "this._completeScroll(" + this._scrollingToTab + ")");
    } else this.scrollTabIntoView(nextTab, backward);
},

_completeScroll : function (scrolledToTab) {
    if (this._scrollingToTab == scrolledToTab) delete this._scrollingToTab;
},

//> @method  tabBar.scrollTabIntoView()  
// If a tab is not currently visible for this tab-bar, scroll it into view.
// Can specify where you want the tab to appear.
// edge it was clipped on.
// @param   tab (number or ImgTab)  tab to scroll into view (or index of tab to scroll into view)
// @param   [start] (boolean) Should the tab be scrolled to the start or end of the viewport?
//                          If not specified the tab will be scrolled to whichever end it is
//                          currently clipped by.
// @param [animated] (boolean) If true, do an animated scroll.
// @param [callback] (callback) If specified this will fire when the tab has been scrolled into
//                              view. Will be synchronously fired if this is not an animated
//                              scroll, or if the tab is already in view, so no scrolling occurs.
//                              The callback takes a single argument, <code>tab</code> - the tab
//                              scrolled into view.
//<
scrollTabIntoView : function (tab, start, animated, callback) {

    
    var tabNum;
    if (isc.isA.Number(tab)) {
        tabNum = tab;
        tab = this.members[tab];
    } else {
        tabNum = this.members.indexOf(tab);
    }
    if (!tab) return;
    
    // if _layoutIsDirty or _layoutInProgress, we can't guarantee that the tab is in the right place
    // (EG the tab may be currently drawn offscreen).
    // In this case wait for the layout to complete
    if (this._layoutIsDirty || this._layoutInProgress) {
        this._scrollOnLayoutComplete = [tab,start,animated,callback];
        return;
    }
    var rect = tab.getRect(),
        xPos, yPos;

    // If not pased "start" parameter, we'll scroll the tab to the start of our viewport
    // iff its clipped to the left, otherwise to the end of our viewport.
    var vertical = this.vertical;
    if (start == null) {
        if (tabNum == 0) start = true;
        else if (tabNum == (this.members.getLength() -1)) start = false;
        else {
            if (vertical) {
                if (this.getScrollTop() > rect[1]) start = true;
                else start = false;
            } else {
                if (this.getScrollLeft() > rect[0]) start = true;
                else start = false;
            }
        }
    }
        
    if (vertical) {
        yPos = (start ? "top" : "bottom");
        // don't scroll horizontally - leave at zero
        xPos = "left";
        rect[2] = 0;
    } else {
        xPos = (start ? "left" : "right");
        // Don't scroll vertically
        yPos = "top";
        rect[3] = 0;
    }
    // When scrolling to the first tab, actually scroll to 0,0, rather than the edge of the
    // tab.
    if (tabNum == 0)  rect[0] = rect[1] = this.isRTL() ? this.getScrollWidth() : 0;
    this.scrollIntoView(rect[0], rect[1], rect[2], rect[3], xPos, yPos, animated, 
        {target:this, methodName:"scrolledTabIntoView", args:[tab, callback]});
},

// Helper to fire the explicit callback passed to scrollTabIntoView.
scrolledTabIntoView : function (tab, scrollCallback) {
    if (scrollCallback != null) {
        this.fireCallback(scrollCallback, "tab", [tab]);
    }
},

// Override _layoutChildrenDone -- if we have a pending scrollTabIntoView, allow it to proceed
_layoutChildrenDone : function (reason, a,b,c,d) {
    this.invokeSuper(isc.TabBar, "_layoutChildrenDone", reason, a,b,c,d);
    if (this._scrollOnLayoutComplete != null) {
        var args = this._scrollOnLayoutComplete;
        this.scrollTabIntoView(args[0],args[1],args[2],args[3]);
        delete this._scrollOnLayoutComplete;
    }
},

scrollForward : function (animated) {
    this._scrollForward(false, animated);
},

scrollBack : function (animated) {
    this._scrollForward(true, animated);
}

});
