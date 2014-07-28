
/*

  SmartClient Ajax RIA system
  Version v9.1p_2014-03-26/LGPL Deployment (2014-03-26)

  Copyright 2000 and beyond Isomorphic Software, Inc. All rights reserved.
  "SmartClient" is a trademark of Isomorphic Software, Inc.

  LICENSE NOTICE
     INSTALLATION OR USE OF THIS SOFTWARE INDICATES YOUR ACCEPTANCE OF
     ISOMORPHIC SOFTWARE LICENSE TERMS. If you have received this file
     without an accompanying Isomorphic Software license file, please
     contact licensing@isomorphic.com for details. Unauthorized copying and
     use of this software is a violation of international copyright law.

  DEVELOPMENT ONLY - DO NOT DEPLOY
     This software is provided for evaluation, training, and development
     purposes only. It may include supplementary components that are not
     licensed for deployment. The separate DEPLOY package for this release
     contains SmartClient components that are licensed for deployment.

  PROPRIETARY & PROTECTED MATERIAL
     This software contains proprietary materials that are protected by
     contract and intellectual property law. You are expressly prohibited
     from attempting to reverse engineer this software or modify this
     software for human readability.

  CONTACT ISOMORPHIC
     For more information regarding license rights and restrictions, or to
     report possible license violations, please contact Isomorphic Software
     by email (licensing@isomorphic.com) or web (www.isomorphic.com).

*/

if(window.isc&&window.isc.module_Core&&!window.isc.module_Containers){isc.module_Containers=1;isc._moduleStart=isc._Containers_start=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc._moduleEnd&&(!isc.Log||(isc.Log && isc.Log.logIsDebugEnabled('loadTime')))){isc._pTM={ message:'Containers load/parse time: ' + (isc._moduleStart-isc._moduleEnd) + 'ms', category:'loadTime'};
if(isc.Log && isc.Log.logDebug)isc.Log.logDebug(isc._pTM.message,'loadTime');
else if(isc._preLog)isc._preLog[isc._preLog.length]=isc._pTM;
else isc._preLog=[isc._pTM]}isc.definingFramework=true;

if (window.isc && isc.version != "v9.1p_2014-03-26/LGPL Deployment") {
    isc.logWarn("SmartClient module version mismatch detected: This application is loading the core module from "
        + "SmartClient version '" + isc.version + "' and additional modules from 'v9.1p_2014-03-26/LGPL Deployment'. Mixing resources from different "
        + "SmartClient packages is not supported and may lead to unpredictable behavior. If you are deploying resources "
        + "from a single package you may need to clear your browser cache, or restart your browser."
        + (isc.Browser.isSGWT ? " SmartGWT developers may also need to clear the gwt-unitCache and run a GWT Compile." : ""));
}








//>    @class    ImgTab
// Specialized StretchImgButton used by TabSet/TabBar for tabs
//
// @treeLocation Client Reference/Foundation
// @visibility external
//<

// class for Stretchable image buttons
isc.ClassFactory.defineClass("ImgTab", "StretchImgButton");

// add properties to the class
isc.ImgTab.addProperties({
    //>    @attr    isc.ImgTab.capSize        (number : 2 : IRW)
    // How big are the end pieces by default
    // @group appearance
    // @visibility external
    //<
    capSize:2,

    //>    @attr    isc.ImgTab.skinImgDir        (URL : "images/Tab/" : IRW)
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

    //>    @attr    isc.ImgTab.src        (URL : "tab.gif" : IRW)
    // Base URL for tab images
    // @visibility external
    //<
    src:"[SKIN]tab.gif",

    //>    @attr    isc.ImgTab.showRollOver        (Boolean : false : IRW)
    // Should we visibly change state when the mouse goes over this tab
    // @visibility external
    //<
    showRollOver:false,

    //>    @attr    isc.ImgTab.showFocus    (boolean : true : IRW)
    // Should we visibly change state when the tab receives keyboard focus?
    // @deprecated as of SmartClient 6.1 in favor of +link{imgTab.showFocused}
    // @visibility external
    //<
    //>    @attr    isc.ImgTab.showFocused    (Boolean : true : IRW)
    // Should we visibly change state when the tab receives keyboard focus?
    // @visibility external
    //<
    showFocused:true,

    //>    @attr    isc.ImgTab.align        (Alignment : isc.Canvas.CENTER : IRW)
    // Alignment of title text
    //        @group    positioning
    // @visibility external
    //<
    // agrees with superclass
    //align:isc.Canvas.CENTER,

    //>    @attr    isc.ImgTab.valign        (VerticalAlignment : isc.Canvas.CENTER : IRW)
    // Vertical alignment of title text.
    //        @group    positioning
    //<
    // agrees with superclass
    //valign:isc.Canvas.CENTER,

    //>    @attr    isc.ImgTab.actionType        (ButtonActionType : isc.Button.BUTTON : IRWA)
    //            button behavior -- BUTTON, RADIO or CHECKBOX
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







//>    @class    TabBar
// Shows a set of Tabs.  TabBars are automatically created by TabSets and shouldn't be used
// directly.  The TabBar is documented for skinning purposes.
//
// @treeLocation Client Reference/Layout/TabSet
// @visibility external
//<
isc.ClassFactory.defineClass("TabBar", "Toolbar");

isc.TabBar.addProperties({
    //>    @attr    isc.TabBar.tabs        (Array of Tab Properties : null : IR)
    // Tab for this TabBar.
    // @visibility external
    //<

    //>    @attr    isc.TabBar.breadth    (number : 21 : IRW)
    // Breadth of the tabBar (including baseline breadth)
    // @visibility external
    //<
    breadth: 21,

    //>    @attr    isc.TabBar.buttonConstructor    (class: ImgTab : AIRW)
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

    //>    @attr    isc.TabBar.skinImgDir        (URL : "images/Tab/" : AIRW)
    //            base path for the tab images, if an image-based
    //            tab is being used.
    //        @group skins, files
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

    //>    @attr isc.TabBar.baseLineThickness (number : 1 : IR)
    // Thickness of the baseLine, in pixels.  This should be set to match the media specified
    // by +link{baseLineSrc}.  The baseLineThickness also determines the degree of overlap with
    // the TabSet's paneContainer when using decorative edges - see +link{TabSet.paneContainer}
    // for details.
    //
    // @group baseLine
    // @visibility external
    //<
    baseLineThickness:1,

    //>    @attr isc.TabBar.baseLineSrc    (SCImgURL : "[SKIN]baseline.gif" : IR)
    // Sets +link{stretchImg.src} for the +link{group:baseLine} StretchImg.
    // @group baseLine
    // @visibility external
    //<
    baseLineSrc:"[SKIN]baseline.gif",

    //>    @attr isc.TabBar.baseLineCapSize    (number : 2 : IR)
    // Set +link{stretchImg.capSize} for the +link{group:baseLine} stretchImg.
    // @group baseLine
    // @visibility external
    //<
    baseLineCapSize:2,

    // Positioning and Alignment
    // --------------------------------------------------------------------------------------------
    //>    @attr    isc.TabBar.tabBarPosition    (Side : isc.Canvas.TOP : IRW)
    // Position of the tabBar in relation to whatever it controls.
    //<
    // Not doc'd, do via TabSet
    tabBarPosition:isc.Canvas.TOP,

    // --------------------------------------------------------------------------------------------
    //>    @attr    isc.TabBar.selectedTab        (number : 0 : IR)
    // Index of the initially selected tab.  Settable at initialization only, afterwards, call
    // +link{selectTab}.
    //<
    // Not doc'd, do via TabSet
    selectedTab:0,

    //>    @attr    isc.TabBar.defaultTabSize        (number : 80 : IR)
    // Default size (length) in pixels for tabs within this tabBar
    // @visibility external
    //<
    defaultTabSize:80

    //>    @attr    TabBar.tabDefaults        (Tab : 0 : AIR)
    //            Defaults applied to tabs on init.
    //            Note that these are overlaid over the superclass property
    //            toolBar.buttonDefaults.
    //<
    // Null by default - we will set this up in initWidget - this avoids multiple tabBars
    // pointing to the same tabDefaults object
    //tabDefaults:{}

});


isc.TabBar.addMethods({
//>    @method    tabBar.initWidget()    (A)
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

//> @method tabBar.draw()    (A)
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

//> @method tabBar.makeBaseLine()    (A)
//  The baseline exists to create the appearance that one of the tabs is part of the pane whereas
//  the other tabs are behind the pane.
//
//    The baseline is an image that runs along the edge of the TabBar that borders on the pane,
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


//> @method tabBar.fixLayout()    (A)
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

//> @method tabBar.buttonSelected()    (A)
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

//> @method tabBar.getSelectedTab()    (A)
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




//> @class Window
//
// A general purpose Window class for implementing dialogs, portlets, alerts, prompts, wizards
// and desktop-like windowing interfaces.
// <P>
// Windows can contain arbitrary SmartClient components, configured via the +link{window.items}
// property.  Windows may be +link{window.isModal,modal} or non-modal.
// <P>
// Windows provide a series of highly configurable and skinnable +link{AutoChild,autoChildren}
// including a header, various header controls, footer, and corner resizer.
// <P>
// The more specialized +link{Dialog} subclass of Window has additional functionality targetted
// at simple prompts and confirmations, such as buttons with default actions, and single-method
// +link{classMethod:isc.warn(),shortcuts} for common application dialogs.
//
// @treeLocation Client Reference/Layout
// @visibility external
//<

isc.ClassFactory.defineClass("Window", "Layout");

//> @groupDef body
// Things related to the body subobject of Window
// @visibility internal
//<

//> @groupDef header
// Things related to the header subobject of Window
// @visibility internal
//<

//> @groupDef headerLabel
// Things related to the headerLabel subobject of Window
// @visibility internal
//<

//> @groupDef footer
// Things related to the footer subobject of Window
// @visibility internal
//<

//> @groupDef windowItems
// Things related to the items contained in the Window body
// @title Window Items
// @visibility internal
//<

// add standard instance properties
isc.Window.addProperties({


    // Skinning
    // ---------------------------------------------------------------------------------------

    //>    @attr    window.styleName    (string : "windowBackground" : IRW)
    //            Default style for the Window background
    //        @group    appearance, header
    //<
    styleName:"windowBackground",
    printStyleName:"normal",

    //>    @attr    window.skinImgDir        (URL : "images/Window/" : IRWA)
    //        Where do 'skin' images (those provided with the class) live?
    //        This is local to the Page.skinDir
    //        @group    appearance, images
    //<
    skinImgDir:"images/Window/",

    //>    @attr    window.backgroundColor    (string : "#DDDDDD" : IRW)
    //            background color, picked up in Header, Footer, etc.
    //        @group    appearance, header
    //<
    backgroundColor:"#DDDDDD",

    layoutMargin:2,
    membersMargin:2,

    // set orientation to vertical by default
    orientation: "vertical",

    // Dragging
    // ---------------------------------------------------------------------------------------


    dragStartDistance:1,

    //> @attr window.canDragReposition (Boolean : true : IRW)
    // If true, this Window may be moved around by the user by dragging on the Window header.
    // Note that if the header is not showing, the Window can't be drag-repositioned regardless
    // of this setting.
    // @see window.showHeader
    // @group dragging
    // @visibility external
    //<
    canDragReposition:true,

    setCanDragReposition : function (canDragReposition, dragTarget) {
        if (!this.headerLabelParent) return;
        this.canDragReposition = false;

        var dragRepo = canDragReposition == null ? true : canDragReposition;
        // if the Window is moveable, make the header draggable
        this.headerLabelParent.dragTarget = dragTarget || this;
        this.headerLabelParent.canDragReposition = dragRepo;
        // HACK: for a Window, canDragReposition means you can reposition using the header.  We
        // have to turn it off for the widget as a whole or any widget that lets drag events
        // bubble will cause strange effects.
        this.canDragReposition = false;
    },

    getCanDragReposition : function () {
        if (this.headerLabelParent) return this.headerLabelParent.canDragReposition;
        return this.canDragReposition;
    },

    //>    @attr    window.keepInParentRect        (boolean or rect: null : IRWA)
    // If +link{window.canDragReposition} or +link{window.canDragResize} is true, should the
    // windows size and position be constrained such that it remains within the viewport of
    // its parent element (or for top level windows, within the viewport of the browser window)?
    // <br>
    // Can also be set to an array denoting an arbitrary rect [Left,Top,Width,Height] beyond
    // which the window cannot be moved.
    // <p>
    // Note: keepInParentRect affects only user drag interactions, not programmatic moves.
    //
    // @see window.canDragReposition
    // @group dragdrop
    // @visibility external
    //<

    dragAppearance : isc.EventHandler.OUTLINE,

    // Drag Resizing
    // ---------------------------------------------------------------------------------------

    //>    @attr    window.canDragResize    (Boolean : false : IRW)
    // Can the window be drag-resized? If true the window may be drag resized from its edges,
    // and if showing, via the resizer icon in the footer.
    // @see window.showResizer
    // @group dragging, resizing
    // @visibility external
    //<

    canDragResize:false,

    //>    @attr    window.resizeFrom    (array : ["R","B","BR"] : IRWA)
    //            which parts of the window can be clicked and
    //            dragged to resize it?
    //        @group    resizing
    //<

    resizeFrom:["R","B","BR"],

    // quick fix for odd drawing behaviors when window is too small
    minWidth:100,
    minHeight:100,

    // Internal
    // ---------------------------------------------------------------------------------------


    //> @attr Window.useBackMask (Boolean : varies : IRA)
    // By default Windows show a +link{canvas.useBackMask,backMask} in Internet Explorer
    // versions predating Internet Explorer 9. This is a workaround for a native browser
    // issue whereby certain DOM elements such as <code>IFRAME</code>s (whether rendered
    // within SmartClient components via features such as +link{htmlFlow.contentsURL,contentsURL} or
    // explicitly written into the HTML of the page) will not be properly occluded
    // by DOM elements which overlap them but have a higher z-index.
    // <P>
    // A side-effect of this is that the +link{canvas.opacity,opacity} can not be modified
    // for the entire window. Developers may disable the backmask in order to support
    // opacity in IE versions less than 9 by setting this property to false, however you
    // should be aware that in doing this there is a potential for the "burn through"
    // problem described above.
    // @visibility external
    //<
    useBackMask: isc.Browser.isIE && isc.Browser.minorVersion >= 5.5 && isc.Browser.version < 9,

    // Document opacity just so we can refer back to useBackMask
    //> @attr Window.opacity (integer : null : IRWA)
    // Renders the widget to be partly transparent. A widget's opacity property may
    // be set to any number between 0 (transparent) to 100 (opaque).
    // Null means don't specify opacity directly, 100 is fully opaque.
    // Note that heavy use of opacity may have a performance impact on some older
    // browsers.
    // <P>
    // In older versions of Internet Explorer (Pre IE9 / HTML5), opacity is achieved
    // through proprietary filters. If
    // +link{canvas.neverUseFilters,filters have been disabled} within this application
    // developers must set +link{canvas.useOpacityFilter} to true for specific components
    // on which opacity support is required.
    // <P>
    // Also note that opacity is incompatible
    // with +link{canvas.useBackMask,backMasks}, and that this property is enabled
    // by default for Window instances.
    // @visibility external
    //<

    // Modality
    // ---------------------------------------------------------------------------------------

    //>    @attr    window.isModal        (Boolean : false : [IRW])
    // If true, when shown this Window will intercept and block events to all other existing
    // components on the page.
    // <P>
    // Use +link{showModalMask} to darken all other elements on the screen when a modal dialog
    // is showing.
    // <P>
    // Chained modal windows - that is, modal windows that launch other modal windows - are
    // allowed.  You can accomplish this by simply creating a second modal Window while a modal
    // Window is showing.
    // <P>
    // Note only top-level Windows (Windows without parents) can be modal.
    //
    // @group modal
    // @visibility external
    // @example modality
    //<
    isModal : false,

    //> @attr window.modalMask (AutoChild Canvas : null : R)
    // A ScreenSpan instance used to darken the rest of a page when a modal window is
    // active. To use, set +link{window.showModalMask} to true, add a CSS style
    // "modalMask" to the active skin (generally with background-color set),
    // and adjust +link{window.modalMaskOpacity}.
    // @group modal, appearance
    // @visibility external
    //<

    //> @attr window.showModalMask (boolean : null : IR)
    // If true, displays a translucent mask over the rest of the page when a modal window
    // is displayed.
    // @group modal, appearance
    // @see window.modalMask
    // @visibility external
    //<

    //> @attr window.modalMaskOpacity (number : 50 : IR)
    // Controls the opacity of the modal mask displayed behind modal windows.
    // @group modal, appearance
    // @see window.modalMask
    // @visibility external
    //<
    modalMaskOpacity: 50,

    //> @attr window.modalMaskStyle (string : "modalMask" : IR)
    // Specifies the CSS style for the modal mask.
    // @group modal, appearance
    // @see window.modalMask
    // @visibility external
    //<
    modalMaskStyle: "modalMask",

    modalMaskConstructor: "ScreenSpan",

    //>    @attr    window.autoCenter        (Boolean : autoCenter : [IRW])
    //      If true, this Window widget will automatically be centered on the page when shown.
    //      If false, it will show up in the last position it was placed (either programmatically,
    //      or by user interaction).
    //  @group  appearance, location
    //  @visibility external
    //<
    //autoCenter : false,

    // Dismissal
    // ---------------------------------------------------------------------------------------

    //>    @attr    window.dismissOnOutsideClick        (Boolean : false : [IRW])
    //      If true, a click outside the bounds of the Window will have the same effect as
    //      pressing its cancel button.<br>
    //      <b>Note:</b> Applies only to modal windows.
    //  @visibility external
    //  @group  modal
    //  @see isModal
    //<
    dismissOnOutsideClick:false,

    //> @attr   window.dismissOnEscape  (Boolean : null : [IRW])
    // Should this window be dismissed (same effect as pressing the "Cancel" button) when the
    // user presses the "Escape" key? Behavior will only occur while the window or one of its
    // descendants has focus, and does not cancel the Escape keypress.
    // <P>
    // If unset default behavior depends on whether a close / cancel button is visible for
    // this item.
    // @visibility external
    // @see window.shouldDismissOnEscape()
    //<
    //dismissOnEscape:null,

    // Body
    // ----------------------------------------------------------------------------------------

    //> @attr window.body (AutoChild Canvas : null : R)
    // Body of the Window, where +link{items,contained components} or +link{src,loaded content}
    // is shown.
    // @visibility external
    //<

    //>    @attr    window.showBody        (Boolean : true : IRWA)
    //      If true, draw the body contents when this Window is drawn.
    //  @visibility external
    //  @group  appearance, body
    //<
    showBody:true,

    //>    @attr    window.bodyStyle    (CSSStyleName : "windowBody" : [IRW])
    //      Style of the Window body.
    //  @visibility external
    //  @group  appearance, body
    //<
    bodyStyle:"windowBody",

    //> @attr window.printBodyStyle (CSSStyleName : "printHeader" : [IR])
    // Style for the Window body in printed output.
    //  @visibility external
    //<

    printBodyStyle:"printHeader",

    //>    @attr    window.bodyColor        (string : "#FFFFFF" : [IRW])
    //      Color of the Window body. Overrides the background color specified in the style.
    //  @visibility external
    //  @group  appearance, body
    //  @see    flash()
    //<
    bodyColor:"#FFFFFF",

    //>    @attr    window.hiliteBodyColor        (string : "#EEEEEE" : [IRW])
    //      Highlight color for the Window body (shown when the body is flashed).
    //  @visibility external
    //  @group  appearance, body
    //  @see    flash()
    //<
    hiliteBodyColor:"#EEEEEE",

    //>    @attr    window.items        (Array of Canvas | Canvas | String : null : [IR])
    //      The contents of the Window body. Can be specified three different ways:
    //      <ul><li>an Array of Canvases that will become the children of the Window's body when it
    //      is initialized; the canvases in this array should be created, but not drawn (autodraw:
    //      false).
    //      <li>a single canvas that will become a child of the Window body.
    //      <li>a string that will be set as the body's contents.</ul>
    //  @see body
    //  @visibility external
    //  @group  appearance, body
    //<

    //>    @attr window.src (string : null : [IRW])
    // A URL to load as content for the Window's body.  If specified, this
    // attribute will take precedence over the items attribute.
    // <P>
    // Note that setting window.src is essentially a shortcut for setting +link{window.items}
    // to a single HTMLflow with a specified +link{htmlFlow.contentsURL,contentsURL}.
    //
    // @see window.contentsType
    // @group  appearance, body
    // @visibility external
    //<

    //> @attr window.contentsType (string : "page" : IR)
    // If this window has +link{window.src} specified, this property can be used to indicate
    // whether the source is a standalone HTML page or an HTML fragment.
    // <P>
    // This is similar to the +link{htmlFlow.contentsType} property - be sure to read the
    // HTMLFlow documentation to understand circumstances where contentsType:"page" is
    // <b>unsafe and not recommended</b>.
    //
    // @see window.src
    // @visibility external
    // @group appearance, body
    //<

    contentsType:"page",


    //>    @attr    window.bodyConstructor (string : null : IRWA)
    // The name of the widget class (as a string) to use for the body. If unset the appropriate
    // constructor type will be determined as follows:<br>
    // - if +link{window.items} is defined as an array of widgets, and +link{window.contentLayout}
    //   is not set to <code>"none"</code>, bodyConstructor defaults to a +link{class:VLayout}<br>
    // - if +link{window.src} is set, bodyConstructor defaults to an +link{class:HTMLFlow}<br>
    // - otherwise bodyConstructor will default to a simple +link{class:Canvas}<br>
    // Note that if this property is overridden for some window, the specified constructor
    // should be a subclass of one of these defaults to ensure the window renders out as
    // expected.
    //
    // @group    appearance, body
    // @visibility external
    //<


    //>    @attr    window.bodyDefaults        (object : ... : IRWA)
    // Default properties for the body of the Window<br>
    // You can change the class-level bodyDefaults for all Windows by changing this item
    // or set  instance.body to be another object of properties to override for your instance only
    // @group    appearance, body
    // @visibility external
    //<
    bodyDefaults:{
        layoutMargin:0,
        printStyleName:"printHeader"
    },

    // Layout
    // ----------------------------------------------------------------------------------------------

    //>    @attr    window.contentLayout (string : "vertical" : [IRWA])
    // The layout policy that should be used for widgets within the Window body.
    // <P>
    // Valid values are "vertical", "horizontal", "none".  If the body is a Layout, this
    // controls how the items are stacked in the body by setting +link{layout.vertical}.
    // See +link{bodyConstructor} for details.
    //
    //  @visibility external
    //    @group    appearance
    //<
    contentLayout:"vertical",

    //>    @attr    window.autoSize (Boolean : false : [IRW])
    //            If true, the window is resized automatically to accommodate the contents
    //            of the body, if they would otherwise require scrolling.
    //      @visibility external
    //        @group    appearance
    //      @example windowAutosize
    //<
    autoSize:false,


    // Header and Header Components
    // ----------------------------------------------------------------------------------------------

    //> @attr window.header (AutoChild HLayout : null : R)
    // Header for the Window, based on an HLayout. The header contains the title and some standard
    // controls for the window, which may be configured via +link{window.headerControls}.
    // @visibility external
    //<

    //>    @attr    window.showHeader        (Boolean : true : IRA)
    // If true, show a +link{window.header} for this Window.
    // <P>
    // Note that in certain Smartclient skins +link{window.showHeaderBackground} may be set to
    // <code>false</code> and the header's appearance implemented as part of the
    // window's +link{canvas.showEdges,edge media}. In this case suppressing the header can be achieved
    // by overriding the edge media as well as setting this property to false. For example, to
    // create a headerless window with a similar appearance to a +link{Menu} in the
    // <code><i>TreeFrog</i></code> skin, the following attributes could be used:
    // <pre>
    //      showHeader:false,
    //      edgeImage:"[SKIN]/Menu/m.png",
    //      edgeSize:10, edgeTop:17, edgeBottom:17,
    //      edgeCenterBackgroundColor:"#F7F7F7"
    // </pre>
    //
    //      @visibility external
    //        @group  appearance, header
    //<
    showHeader:true,

    headerConstructor:"HLayout",

    //> @attr window.headerBackground (AutoChild Img : null : R)
    // Img background component for the header, for gradient or image-based display
    // @visibility external
    //<

    //>@attr    window.showHeaderBackground (Boolean : varies : IRA)
    // Should the window header show a background image? Default value is true for all browsers
    // except for Internet Explorer.<br>
    // If set to true the image source is derived from +link{window.headerSrc} and
    // +link{window.hiliteHeaderSrc}, otherwise the background will be styled according to
    // +link{window.headerStyle} / +link{window.hiliteHeaderStyle}.
    // @group appearance, header
    // @visibility external
    //<
    // By default, we assume CSS will be used in recent IE, and media otherwise, since the
    // typical presentation is a gradient.
    showHeaderBackground :
        !(isc.Browser.isIE && !isc.Browser.isStrict && isc.Browser.minorVersion >= 5.5),

    headerBackgroundConstructor: "Img",

    headerBackgroundDefaults : {
        width:"100%",
        height:"100%",
        // background is a non-member child of the header, which is a Layout
        addAsChild:true,
        // applicable to StretchImgs only
        vertical:false,
        capSize:10,
        shouldPrint:false
    },

    //>    @attr    window.headerStyle    (CSSStyleName : "WindowHeader" : [IRWA])
    //          Style for the Window header.
    //      @visibility external
    //      @group    appearance, header
    //<
    headerStyle:"windowHeader",

    //> @attr window.printHeaderStyle (CSSStyleName : "printHeader" : [IR])
    // CSS Style for header in printed output
    // @visibility external
    //<
    printHeaderStyle:"printHeader",

    //>    @attr    window.headerSrc (SCImgURL : "[SKIN]Window/headerGradient.gif" | null : [IRWA])
    // If +link{window.showHeaderBackground} is <code>true</code>, this property provides
    // the URL of the background image for the header.
    // @group  appearance, header
    // @visibility external
    //<
    headerSrc:(!(isc.Browser.isIE && !isc.Browser.isStrict && isc.Browser.minorVersion >= 5.5) ?
                        "[SKIN]Window/headerGradient.gif" : null),

    headerDefaults:{
        // Note - other defaults applied in Window.makeHeader()
        height:18,
        layoutMargin:1,
        membersMargin:2,
        overflow:isc.Canvas.HIDDEN,
        // Turn off printFillWidth for the header - we don't want to render the
        // print header in a 100% sized table as this causes the icon's cell to have
        // a bunch more space than it needs and so you get a big gap between icon and
        // title
        printFillWidth:false
    },

    //> @attr window.headerControls (Array of String : (see below) : IR)
    // Array of members to show in the Window header.
    // <P>
    // The default value of <code>headerControls</code> is an Array of Strings listing the
    // standard header controls in their default order:
    // <pre>
    //    headerControls : ["headerIcon", "headerLabel",
    //                      "minimizeButton", "maximizeButton", "closeButton"]
    // </pre>
    // You can override <code>headerControls</code> to change the order of standard controls in
    // the header.  You can also omit standard controls this way, although it more efficient to
    // use the related "show" property if available (eg +link{showMinimizeButton}).
    // <P>
    // By embedding a Canvas directly in this list you can add arbitrary additional controls to
    // the header, for example, an additional button (eg return to dock) or a DynamicForm with
    // various kinds of input controls.
    // <P>
    // Note that having added controls to headerControls, you can still call APIs directly on
    // those controls to change their appearance, and you can also show() and hide() them if
    // they should not be shown in some circumstances.
    // <P>
    // Tip: custom controls need to set layoutAlign:"center" to appear vertically centered.
    //
    // @visibility external
    // @example windowHeaderControls
    //<
    headerControls : ["headerIcon", "headerLabel",
                      "minimizeButton", "maximizeButton", "closeButton"],

    // Flashing
    // ---------------------------------------------------------------------------------------
    //>    @attr    window.hiliteHeaderStyle    (CSSStyleName : "WindowHeader" : [IRWA])
    // Highlight style for the Window header. Displayed when a window
    // is +link{window.flash(), flashed}
    //      @group    appearance, header
    //      @visibility external
    //<
    hiliteHeaderStyle:"windowHeaderHilite",

    //>    @attr    window.hiliteHeaderSrc (SCImgURL : "[SKIN]Window/headerGradient_hilite.gif" | null : [IRWA])
    // If +link{Window.showHeaderBackground} is true, this governs the URL of the image to
    // use in the header's highlighted state when the window is +link{window.flash(), flashed}
    // @group  appearance, header
    // @visibility external
    //<
    hiliteHeaderSrc:(!(isc.Browser.isIE && isc.Browser.minorVersion >= 5.5) ?
                        "[SKIN]Window/headerGradient_hilite.gif" : null),


    // HeaderLabel settings
    // --------------------------------------------------------------------------------------------

    //> @attr window.headerLabel (AutoChild Label : null : R)
    // Label that shows Window title in header.
    // @visibility external
    //<

    //>    @attr    window.showTitle        (Boolean : true : [IRW])
    //        Show a title (typically just text) on the header for this window.
    //      @visibility external
    //      @group    appearance, headerLabel
    //<
    showTitle:true,

    //>    @attr window.title        (HTMLString : "Untitled Window" : [IRW])
    //          title for this Window, shown in the header (if drawn)
    //      @visibility external
    //      @group    appearance, headerLabel, i18nMessages
    //<
    title:"Untitled Window",

    //>    @attr    window.headerLabelConstructor    (Class : Label : IRWA)
    //            The headerLabel for a Window, if shown, will be an instance of this class.
    //        @group    appearance, headerLabel
    //<
    headerLabelConstructor:"Label",

    headerLabelParentDefaults : {
        autoDraw:false,
        _generated:true,


        _constructor: "StatefulCanvas",
        showTitle: true,
        getTitle : function () {
            return isc.Canvas.blankImgHTML(1000, 100);
        },
        // delegate 'getPrintHTML' to the actual label so we don't render out a big spacer
        getPrintHTML : function (a,b,c,d) {
            return this.creator.headerLabel.getPrintHTML(a,b,c,d);
        },

        overflow:"hidden"
    },

    //> @attr window.headerLabelDefaults (Object : see below : IRWA)
    //
    // This is an object literal property block specifying various properties of the header
    // label that displays the +link{Window.title}.  Overrideable defaults are as follows:
    // <ul>
    // <li>styleName- defaults to <code>"windowHeaderText"</code> and specifies the css style
    // that is used  to render the +link{Window.title} text.
    // </ul>
    // You can override the the above properties by calling +link{Class.changeDefaults()}.
    //
    // @group appearance, headerLabel
    // @visibility external
    //<
    //
    // Default properties for the headerLabel of the Window you can change the class-level
    // headerLabelDefaults for all Windows by changing this item or set  instance.headerLabel to be
    // another object of properties to override for your instance only
    headerLabelDefaults:{
        wrap:false,
        align:isc.Canvas.LEFT,
        styleName:"windowHeaderText",
        width:10,
        inherentWidth:true,
        layoutAlign: isc.Page.isRTL() ? isc.Canvas.RIGHT : isc.Canvas.LEFT
    },

    // Header icon
    // --------------------------------------------------------------------------------------------

    //> @attr window.headerIcon (AutoChild Img : null : R)
    // Header icon shown at left end of header by default.
    // @visibility external
    //<

    //>    @attr    window.showHeaderIcon        (Boolean : true : [IRW])
    //          If true, we show an icon on the left in the header.
    //      @visibility external
    //      @group  appearance, header
    //<
    showHeaderIcon:true,

    //>    @attr    window.headerIconConstructor    (Class : Img : IRWA)
    //            The headerIcon for a Window, if shown,
    //            will be an instance of this class.
    //        @group    appearance, header
    //<
    headerIconConstructor:"Img",

    //>    @attr    window.headerIconDefaults        (object : ... : IRWA)
    //
    // This is an object literal property block specifying the various properties of the
    // headerIcon - the icon that appears at the top left of the window and is by default the
    // Isomorphic logo.  Overrideable defaults are as follows:
    // <ul>
    // <li>width - default to <code>16</code> and specifies the width of the headerIcon.
    // <li>height - default to <code>14</code> and specifies the height of the headerIcon.
    // <li>src - defaults to <code>"[SKIN]/Window/minimize.gif"</code> and specifies the image
    // for the headerIcon.
    // </ul>
    // You can override the the above properties by calling +link{Class.changeDefaults()}.
    //
    //    @group    appearance, header
    //  @visibility external
    //<
    headerIconDefaults:{
        width:16,
        height:16,
        layoutAlign:"center",
        src:"[SKIN]/Window/headerIcon.gif"
    },

    // Header buttons
    // --------------------------------------------------------------------------------------------

    //>    @attr    window.canFocusInHeaderButtons (Boolean : false : [IRWA])
    //      If true, the user can give the header buttons keyboard focus (by clicking on
    //      them and including them in the tabOrder)
    //  @visibility external
    //  @group    focus, header
    //<
    // Note: this property is applied to the header buttons when they are initialized.
    canFocusInHeaderButtons:false,

    // Close button
    // --------------------------------------------------------------------------------------------

    //> @attr window.closeButton (AutoChild ImgButton : null : R)
    // Button show in the header that will close this Window by calling +link{closeClick()}.
    // @visibility external
    //<

    //>    @attr    window.showCloseButton        (Boolean : true : [IRW])
    // If true, show a close button in the header, which will dismiss this window by
    // calling +link{closeClick()}.
    // @group  appearance, header
    // @visibility external
    //<
    showCloseButton:true,

    closeButtonConstructor:"ImgButton",

    closeButtonDefaults:{
        width:16,
        height:14,
        layoutAlign:"center",
        src:"[SKIN]/Window/close.gif",
        click: function () {return this.creator._closeButtonClick()}
    },

    // MinimizeButton (same button as for restoring)
    // --------------------------------------------------------------------------------------------

    //> @attr window.minimizeButton (AutoChild ImgButton : null : R)
    // ImgButton shown in the header that will minimize this Window by calling +link{minimize()}.
    // @visibility external
    //<

    //>    @attr    window.showMinimizeButton        (Boolean : true : [IRW])
    // If true, show a minimize button in the header--clicking it minimizes the Window.
    //      @visibility external
    //      @group  appearance, header
    //<
    showMinimizeButton:true,

    minimizeButtonConstructor:"ImgButton",

    minimizeButtonDefaults:{
        width:16,
        height:14,
        layoutAlign:"center",
        src:"[SKIN]/Window/minimize.gif",
        click:function () {
            // If onMinimizeClick exists, allow it to cancel default behavior

            if (!this.creator.onMinimizeClick || (this.creator.onMinimizeClick() != false)) {
                this.creator.minimize();
            }
            return false
        }
    },

    //> @attr   window.minimized    (Boolean : false : [IRW])
    // Is this window minimized. If true at init time, the window will be drawn minimized.
    // To set this property at runtime use +link{Window.minimize()} or +link{Window.restore()}.
    // @visibility external
    // @group  appearance, header
    //<
    minimized:false,

    //> @attr   window.defaultMinimizeHeight    (number : 16 : [IRWA])
    // If +link{window.minimizeHeight} is unset, by the window will shrink to the height of the
    // header when minimized.
    // <BR>
    // If there is no header, the <code>defaultMinimizeHeight</code> will be used instead.
    // @visibility external
    // @group  appearance, header
    //<
    defaultMinimizeHeight:16,

    //> @attr window.minimizeHeight (number : null : [IRWA])
    // Height for the window when minimized.
    // If unset the window will shrink to the height of the header, if present, otherwise
    // +link{window.defaultMinimizeHeight,this.defaultMinimizeHeight}
    // @visibility external
    // @group appearance, minimize
    //<

    //>Animation
    //> @attr   window.animateMinimize    (boolean: null : [IRWA])
    // Should this window minimize, maximize, and restore as an animation, or as a
    // simple 1-step transition?
    // @visibility animation
    // @group  appearance, header, animation
    // @example windowMinimize
    //<
    //animateMinimize:false,

    //> @attr   window.minimizeTime     (number : null : [IRWA])
    // If this window is minimizeable, and animateMinimize is true, what should the duration of
    // the minimize / maximize be (in ms)? If unset defaults to <code>canvas.animationTime</code>.
    // @visibility animation
    // @group  appearance, header, animation
    // @example windowMinimize
    //<
    //minimizeTime : null,

    //> @attr window.minimizeAcceleration (AnimationAcceleration : null : IRWA)
    // Default acceleration function for performing an animated minimize / maximize.  If unset,
    // <code>this.animateAcceleration</code> will be used by default instead
    // @visibility animation
    // @group appearance, header, animation
    //<
    //<Animation

    // RestoreButton - properties to make the minimize button a restore button.
    // --------------------------------------------------------------------------------------------

    //> @attr window.restoreButton (AutoChild ImgButton : null : R)
    // ImgButton that restores the Window via +link{restore()}.
    // @visibility external
    //<

    // Note: we currently actually apply these to the minimizeButton to change it on the fly,
    // we should probably just create both and hide/show
    restoreButtonDefaults:{
        width:16,
        height:14,
        src:"[SKIN]/Window/restore.gif",
        layoutAlign:"center",
        click:function () {
            if (!this.creator.onRestoreClick || (this.creator.onRestoreClick() != false)) {
                this.creator.restore();
            }
            return false
        }
    },

    // MaximizeButton
    // --------------------------------------------------------------------------------------------

    //> @attr   window.maximized (Boolean : false : [IRW])
    // Is this window maximized. If true at init time, the window will be drawn maximized.
    // To set this property at runtime use +link{window.maximize()} or +link{window.restore()}.
    // @visibility external
    // @group  appearance, header
    //<
    minimized:false,

    //> @attr window.maximizeButton (AutoChild ImgButton : null : R)
    // Button that will make this Window fill the browser via +link{maximize()}.
    // @visibility external
    //<

    //>    @attr    window.showMaximizeButton        (Boolean : false : [IRW])
    // If true, show a maximize button in the header - clicking it maximizes the Window
    //      @visibility external
    //      @group  appearance, header
    //<
    showMaximizeButton:false,

    maximizeButtonConstructor:"ImgButton",

    maximizeButtonDefaults:{
        width:16,
        height:14,
        src:"[SKIN]/Window/maximize.gif",
        layoutAlign:"center",
        click:function () {
            if (!this.creator.onMaximizeClick || (this.creator.onMaximizeClick() != false)) {
                this.creator.maximize();
            }
            return false
        }
    },


    // Footer and Footer Components
    // ------------------------------------------------------------------------------------------

    //> @attr window.footer (AutoChild HLayout : null : R)
    // Optional footer for the window, providing space for controls such as the resizer and
    // status bar.
    // @visibility external
    //<

    //>    @attr    window.showFooter        (Boolean : true : [IRW])
    // If true, show a footer for this Window, including resizer, statusBar, etc.
    // This setting is commonly overridden for skinning purposes.
    //      @visibility external
    //      @group  appearance, footer
    // @example windowFooter
    //<
    showFooter:true,

    footerConstructor:"HLayout",

    //>    @attr    window.footerHeight        (number : 18 : IR)
    //
    // The height of the footer, in pixels.
    //
    // @group  appearance, footer
    // @visibility external
    //<
    footerHeight:18,

    //> @attr window.footerControls (Array of String : (see below) : IR)
    // Array of members to show in the Window footer.
    // <P>
    // The default value of <code>footerControls</code> is an Array of Strings listing the
    // standard footer controls in their default order:
    // <pre>
    //    footerControls : ["spacer", "resizer"]
    // </pre>
    // As with +link{Window.headerControls}, you can override <code>footerControls</code>
    // to change the order of standard controls in the footer. <code>"spacer"</code> is a special
    // value which will create a +link{LayoutSpacer} in the footer bar. <code>"resizer"</code>
    // will show the +link{window.resizer} in the footer.
    // <P>
    // By embedding a Canvas directly in this list you can add arbitrary additional controls to
    // the footer.
    // <P>
    // Note that the +link{window.statusBar} is not part of the set of footer controls - it is a
    // separate canvas rendered behind all footer controls. If you include some custom status bar
    // directly in the footerControls you may want to set +link{window.showFooter} to false.
    // <P>
    // Tip: custom controls need to set layoutAlign:"center" to appear vertically centered.
    //
    // @visibility external
    //<
    footerControls:["spacer", "resizer"],

    // StatusBar settings
    // ----------------------------------------------------------------------------------------

    //> @attr window.statusBar (AutoChild Canvas : null : R)
    // Simple Canvas-based status bar, shown in the footer.  +link{setStatus()} can be used to
    // show text here.
    // @visibility external
    //<

    //>    @attr    window.showStatusBar        (Boolean : true : [IRW])
    // If true, show a statusBar for this Window, including resizer.
    //      @visibility external
    //      @group  appearance, footer
    //<
    showStatusBar:true,

    statusBarConstructor:"Canvas",

    statusBarDefaults:{
        overflow:isc.Canvas.HIDDEN,
        styleName:"windowStatusBar",
        addAsChild:true,
        width:"100%",
        wrap:false,
        leftPadding:5
    },


    // Resizer
    // --------------------------------------------------------------------------------------------

    //> @attr window.resizer (AutoChild ImgButton : null : R)
    // ImgButton-based resizer, shown in the footer.
    // @visibility external
    //<

    //>    @attr    window.showResizer        (Boolean : true : [IRW])
    // If true, show a button in the lower right corner that allows users to resize the Window.
    // Note that the resizer will only be displayed if the footer is showing for the window
    // (+link{window.showFooter}) and +link{window.canDragResize} is true.
    // @group  appearance, dragging
    // @visibility external
    //<
    showResizer:true,

    resizerConstructor:"Img",

    resizerDefaults:{
        canDragResize:true,
        getEventEdge:function(){
            if (this.creator.resizeFrom.contains("BR")) {
                return "BR";
            } else if (this.creator.resizeFrom.contains("B")) {
                return "B";
            } else if (this.creator.resizeFrom.contains("R")) {
                return "R";
            }
        },
        src:"[SKIN]/Window/resizer.gif",
        width:16,
        height:16
    },

    // Toolbar
    // ----------------------------------------------------------------------------------------------

    // NOTE: only documented on Dialog

    showToolbar:false,

    toolbarConstructor:"Toolbar",

    toolbarDefaults:{
        height:40,
        layoutMargin:10,
        membersMargin:5,
        overflow:"visible"
    },

    // Edges
    // ---------------------------------------------------------------------------------------
    // show custom edges as a child Canvas, top and bottom only by default.
    customEdges:["T", "B"],

    // alternate mode where we create the edgedCanvas as a child and use layoutMargins to place
    // members.  No known advantages.
    //edgesAsChild:true,

    // ---------------------------------------------------------------------------------------

    // set overflow to hidden; nothing should ever overflow the Window.  We need to be overflow
    // "hidden" even if the body clips, since the Window can be minimized.
    overflow:"hidden"

});    // END    Window.addProperties()



//!>Deferred

isc.Window.addMethods({
//>    @method    Window.initWidget()    (A)
//            Initialize this window.
//<
initWidget : function () {

    if (this.minimized && this.maximized) {
        this.logWarn("Window initialized with maximized and minimized both set to true. " +
                     "This is unsupported. The Window will be rendered minimized.");
        this.maximized = false;
    }

    // If this.minimized is true, call this.minimize() to set up minimized height, etc.
    if (this.minimized) {
        // clear out the property to avoid any confusion (currently we don't have a no-op check
        // in there but we may introduce one at some point)
        this.minimized = null;
        this.minimize();
    } else if (this.maximized) {
        this.maximized = null;
        this.maximize();
    }

    /*
    // edges as child mode.  Currently, no known advantages.
    if (this.showEdges) {
        var edge = this._createEdges(),
            edgesAsChild = this.edgesAsChild;

        // if edgesAsChild is true, we have no automatically created native margins, so we need
        // to use the Layout margin settings to cause our members (eg the header) not to
        // overlap the edges we are configured to show.
        if (edgesAsChild) {
            this.layoutLeftMargin = edge._leftMargin;
            this.layoutRightMargin = edge._rightMargin;
            this.layoutTopMargin = edge._topMargin;
            this.layoutBottomMargin = edge._bottomMargin;
        }
    }
    */

    if (this.autoSize) {
        this.vPolicy = "none";
        this.overflow = "visible";
    }

    this.Super(this._$initWidget);

    // if we're not drawn, clear any specified items that are currently drawn
    // (Note: we could use 'addItems' to achieve this too)
    if (!this._isInitialized && this.items != null) {
        for (var i = 0; i < this.items.length; i++) {
            if (isc.isA.Canvas(this.items[i]) && this.items[i].isDrawn()) this.items[i].clear();
        }
    }

},

createChildren : function () {

    this.makeHeader();

    // make the body of the Window, and set up the items in the Window as children of the body
    this.makeBody();

    this.makeToolbar();

    this.makeFooter();

    this._isInitialized = true;
},

makeToolbar : function () {
    this.addAutoChild("toolbar", {
            buttons:this.toolbarButtons,
            // hide initially if we're minimized
            visibility : this.minimized ? isc.Canvas.HIDDEN : isc.Canvas.INHERIT
    });
},

setShowToolbar : function (showToolbar) {
    if (showToolbar == this.showToolbar) return;
    this.showToolbar = showToolbar;
    if (!this._isInitialized) return;
    if (showToolbar) {
        if (!this.toolbar) this.makeToolbar();
        else if (!this.toolbar.isDrawn()) this.toolbar.draw();
    } else {
        if (this.toolbar && this.toolbar.isDrawn()) this.toolbar.clear();
    }

},

//>    @method    Window.draw()    (A)
//        @group    drawing
//            Override draw to create the various sub-components and arrange their zOrder appropriately
//
//        @return    (boolean)    true if drawn successfully; false if not drawn for some reason
//<
draw : function (a,b,c,d) {
    if (isc._traceMarkers) arguments.__this = this;

    if (!this.readyToDraw()) return this;

    // create children (unless we've been clear()d and are being drawn for a second time)
    if (!this._isInitialized) this.createChildren();

    // call the superclass draw to actually draw the components, including the body
    return this.invokeSuper(isc.Window, "draw", a,b,c,d);
},


// Because we lazily add our items as children on draw, if we've never been drawn we will have
// to explicitly destroy members of our items array.
destroy : function () {
    if (!this._isInitialized) {
        var items = this.items;
        if (!isc.isAn.Array(items)) items = [items];
        for (var i = 0; i < items.length; i++) {
            if (isc.isA.Canvas(items[i])) items[i].destroy();
        }
    }

    this.items = null;
    this.destroyModalMask();

    return this.Super("destroy", arguments);
},

// Bring Windows to front on mouseUp.

mouseUp : function () {
    this.bringToFront(true);
    this.Super("mouseUp", arguments);
},

// Header Methods
// -----------------------------------------------------------------------------------------------
// These are methods that construct the header. Window.setHeader() is the main method.
// It calls the make() methods of its constituent components to set them up and lay
// them out.
//

// declare relationships of children of header
autoChildParentMap : {
    resizer : "footer",
    statusBar : "footer",
    headerBackground : "header",
    headerIcon : "header",
    headerLabel : "header",
    minimizeButton : "header",
    maximizeButton : "header",
    closeButton : "header",
    toolbar : "body"
},

//>    @method    window.setHeader()
//        @group    appearance
//            initialize the header and its components.
//            if placement parameters are given, then lay out the header.
//
//        @param left        (number) left position of header
//        @param top        (number) right position of header
//        @param width    (number) width of header
//        @param height    (number) height of header
//<
makeHeader : function () {
    // the header is first created, then its children.
    var header = this.addAutoChild(
                    "header",
                    {width:"100%", styleName:this.headerStyle,
                     printStyleName:this.printHeaderStyle}
                 );

    if (header == null) return; // not showing a header

    // create children once the header has been created
    if (header != null) {
        var headerBackground = this.addAutoChild("headerBackground", {
            // src will be picked up only by an Img/StretchImg
            src:this.headerSrc
        });
        if (headerBackground) headerBackground.sendToBack();

        // if the window is in minimized state before we draw, swap in the restore button
        // autoChild defaults so we get the restore button to draw instead.
        if (this.minimized) {
            this._minimizeButtonDefaults = this.minimizeButtonDefaults;
            this._minimizeButtonProperties = this.minimizeButtonProperties;
            this.minimizeButtonDefaults = this.restoreButtonDefaults;
            this.minimizeButtonProperties = this.restoreButtonProperties;

        // Ditto with the maximize button
        } else if (this.maximized) {
            this._maximizeButtonDefaults = this.maximizeButtonDefaults;
            this._maximizeButtonProperties = this.maximizeButtonProperties;
            this.maximizeButtonDefaults = this.restoreButtonDefaults;
            this.maximizeButtonProperties = this.restoreButtonProperties;
        }

        // instantiate the header buttons in left-> right order so the tab order is appropriate
        // when we allow tabbing through them.
        this.addAutoChildren(this.headerControls, this.header);
        if (this.minimized) {
            this.minimizeButtonDefaults = this._minimizeButtonDefaults;
            this.minimizeButtonProperties = this._minimizeButtonProperties;
            this._minimizeButtonDefaults = this._minimizeButtonProperites = null;
        } else if (this.maximized) {
            this.maximizeButtonDefaults = this._maximizeButtonDefaults;
            this.maximizeButtonProperties = this._maximizeButtonProperties;
            this._maximizeButtonDefaults = this._maximizeButtonProperites = null;
        }
    }
},

setHeaderControls : function (headerControls) {
    if (this.headerControls == headerControls) return;

    var oldHeaderControls = this.headerControls,
        oldControls = [];

    this.headerControls = headerControls;

    if (this.header == null) return;

    for (var i = i ; i < oldHeaderControls.length; i++) {
        // map from auto child names ('minimizeButton' etc) to live widgets
        if (isc.isA.String(oldHeaderControls[i])) oldControls[i] = this[oldHeaderControls[i]]
        else oldControls[i] = oldHeaderControls[i];
    }
    this.header.removeMembers(oldControls);
    this.header.addMembers(headerControls);


},

// The way auto-children work is that if show[childName] is false, they aren't created as
// part of addAutoChild()
// This means that simply setting the show[headerControl] attributes after initial call to
// makeHeader will fail to ever create / show these controls.
// Therefore have a method to explicitly create and show the header controls at runtime
setShowHeaderControl : function (controlName, show, showControlAttrName) {
    var headerControls = this.headerControls;
    if (!headerControls.contains(controlName)) {
        this.logWarn("request to show/hide header control with name:" + controlName +
                     ". No such control is present in this.headerControls - ignoring.");
        return;
    }
    if (!showControlAttrName)
        showControlAttrName = "show" +
                              controlName.substring(0,1).toUpperCase() + controlName.substring(1);

    if (this[showControlAttrName] == show) return;
    this[showControlAttrName] = show;

    // If we've never created our header - don't worry - our headerControls will be updated
    // if/when we do create the header
    if (this.header == null) return;

    if (this[controlName]) {
        if (show) this[controlName].show();
        else this[controlName].hide();
    } else if (show) {
        var slot = 0;
        for (var i = 0; i < headerControls.length; i++) {
            if (headerControls[i] == controlName) break;
            if (this[headerControls[i]]) slot++;
        }
        this.addAutoChild(controlName, null, null, this.header, slot);
        this[controlName].show();
    }
},

//> @method Window.setShowCloseButton()
// Dynamically update +link{window.showCloseButton} to show / hide the closeButton
// @see window.headerControls
// @see window.showCloseButton
// @visibility external
//<
setShowCloseButton : function (show) {
    this.setShowHeaderControl("closeButton", show, "showCloseButton");
},

//> @method Window.setShowMinimizeButton()
// Dynamically update +link{window.showMinimizeButton} to show / hide the minimizeButton
// @see window.headerControls
// @see window.showMinimizeButton
// @visibility external
//<
setShowMinimizeButton : function (show) {
    this.setShowHeaderControl("minimizeButton", show, "showMinimizeButton");
},


//> @method Window.setShowMaximizeButton()
// Dynamically update +link{window.showMaximizeButton} to show / hide the maximizeButton
// @see window.headerControls
// @see window.showMaximizeButton
// @visibility external
//<
setShowMaximizeButton : function (show) {
    this.setShowHeaderControl("maximizeButton", show, "showMaximizeButton");
},

//> @method Window.setShowHeaderIcon()
// Dynamically update +link{window.showHeaderIcon} to show / hide the headerIcon
// @see window.headerControls
// @see window.showHeaderIcon
// @visibility external
//<
setShowHeaderIcon : function (show) {
    this.setShowHeaderControl("headerIcon", show, "showHeaderIcon");
},



getDynamicDefaults : function (childName) {
    if (isc.endsWith(childName, isc.Button.Class)) {
        return { canFocus : this.canFocusInHeaderButtons };
    }
},

// custom autoChild maker function for the headerLabel, because it is currently wrapped inside
// a Canvas used for clipping
headerLabel_autoMaker : function () {
    // if we're not showing a headerLabel,
    if (!this.showTitle) {
        // clear the headerLabel property
        this.headerLabelParent = null;
        this.headerLabel = null;
        // and get outta dodge
        return;
    }


    var headerLabelParent = this.headerLabelParent = this.createAutoChild("headerLabelParent");


    if (this.headerLabelParent.label) {
        this.headerLabelParent.label.sendToBack();
    }

    this.setCanDragReposition(this.canDragReposition);

    var headerLabel = this.headerLabel = this.createAutoChild(
                        "headerLabel",
                        {
                            height:"100%",
                            contents:this.title,
                            dragTarget:this,
                            // Override getCurrentCursor so we show the drag reposition cursor
                            // rather than the default pointer.
                            getCurrentCursor : function () {
                                if (this.parentElement)
                                    return this.parentElement.getCurrentCursor();
                                return this.Super("getCurrentCursor", arguments);
                            },
                            eventProxy: this.headerLabelParent
                        });

    // Add the headerLabel to an HStack layout to allow the label to have
    // a layoutAlign "right" in RTL mode (set in headerLabelDefaults).
    var rtlFix = isc.HStack.create({
        autoDraw: false,
        width: "100%",
        height: "100%",
        members: [headerLabel],
        // Override getCurrentCursor so we show the drag reposition cursor
        // rather than the default pointer.
        getCurrentCursor : function () {
            if (this.parentElement)
                return this.parentElement.getCurrentCursor();
            return this.Super("getCurrentCursor", arguments);
        }
    });

    this.headerLabelParent.addChild(rtlFix);
    this.header.addMember(headerLabelParent);
},

//>    @method    Window.setTitle()   ([])
//          Sets the title text that appears in the window header; the header will be redrawn
//          if necessary.
//      @visibility external
//        @group    header
//        @param    newTitle    (string : null)    new title
//<
setTitle : function (newTitle) {
    if (newTitle) this.title = newTitle;
    if (!this.header) return;
    // if a header label exists, set the title on that, otherwise set it on the header
    if (this.headerLabel) this.headerLabel.setContents(this.title);
    else this.header.setContents(this.title);
},

// Toolbar Methods
// -----------------------------------------------------------------------------------------------
// These are methods that construct the toolbar.
//

//>    @method    dialog.setButtons()
// Set the buttons for the toolbar displayed in this dialog.
// Synonym for +link{dialog.setToolbarButtons()}
// @param    newButtons    (array of Buttons: null) buttons for the toolbar
// @visibility external
//<

setButtons : function (newButtons) {
    return this.setToolbarButtons(newButtons);
},

// Exposed at the Dialog level, where we also expose the toolbarButtons attribute
//>    @method    dialog.setToolbarButtons()
// Set the +link{dialog.toolbarButtons} for this dialog.
// Synonym for +link{dialog.setButtons()}.
// @param    newButtons    (array of Buttons: null) buttons for the toolbar
// @visibility external
//<
setToolbarButtons : function (newButtons) {
    this.toolbarButtons = newButtons;
    if (!this._isInitialized) return;
    if (newButtons) {
        if (!this.toolbar) this.setShowToolbar(true);
        this.toolbar.setButtons(newButtons);
    } else {
        this.toolbar.setButtons(newButtons);
        if (this.showToolbar) this.setShowToolbar(false);
    }
},


// Footer Methods
// -----------------------------------------------------------------------------------------------
// These are methods that construct the footer. Window.setFooter() is the main method.
// It calls the make() methods of its constituent components to set them up and lay
// them out.
//

//>    @method    window.setFooter()
//        @group    appearance
//            initialize the footer and its components.
//            if placement parameters are given, then lay out the footer.
//
//        @param left        (number) left position of footer
//        @param top        (number) right position of footer
//        @param width    (number) width of footer
//        @param height    (number) height of footer
//<
makeFooter : function () {
    // if not showing a footer, bail
    if (!this.showFooter) return;

    this.addAutoChild("footer", {height:this.footerHeight});

    if (!this.footer) return;
    var controls = [];
    for (var i = 0; i < this.footerControls.length; i++) {
        var control = this.footerControls[i], properties = {};

        if (control == "spacer") control = isc.LayoutSpacer.create();
        if (control == "resizer") {
            if (!this.canDragResize) continue;
            properties.dragTarget = this;
        }
        properties.visibility = this.minimized ? isc.Canvas.HIDDEN : isc.Canvas.INHERIT;

        if (isc.isA.String(control)) {
            this.addAutoChild(control, properties, null, this.footer);
        } else {
            if (isc.isA.Canvas(control)) control.setProperties(properties);
            else isc.addProperties(control, properties);

            this.footer.addMember(control);
        }
    }


    // status bar fills entire width (not a member: extends under resizer)
    // Note that this means the resizer may obscure the borders of the statusBar. This is
    // currently handled by the resizer media
    this.addAutoChild("statusBar", {
        height: this.footer.getHeight(),
        visibility : this.minimized ? isc.Canvas.HIDDEN : isc.Canvas.INHERIT
    });

    if (this.status != null) this.setStatus(this.status);
    if (this.statusBar) this.statusBar.sendToBack();

},

//> @attr Window.status (string : null : IRW)
// Text to show in the status bar of the window (if one is visible)
// @group appearance
// @visibility external
//<

//>    @method    Window.setStatus()  ([])
//            Sets the text in the status bar of the window, redrawing if necessary.
//        @param statusString (string) new text for the status bar
//        @group    appearance
//      @visibility external
//<
setStatus : function (statusString) {
    this.status = statusString;
    if (this.statusBar == null) return;
    if (statusString == null) statusString = "";
    var leftPadding = (this.statusBar.leftPadding ? isc.Canvas.spacerHTML(this.statusBar.leftPadding,1) : "");
    this.statusBar.setContents(leftPadding + statusString);

},


//>    @method    Window.setSrc() ([])
// Sets the URL of the contents to display in the body of the window, redrawing if
// necessary.
//      @visibility external
//        @group    appearance, body
//        @param url (string) URL of new contents to be displayed in the window body
//<
setSrc : function (url) {
    this.src = url;
    if (this.body) this.body.setContentsURL(url);
},



// Misc Make Methods
// -----------------------------------------------------------------------------------------------
// make methods for the body
//


//>    @method    Window.makeBody()    (A)
//        @group    appearance, body
//         make the body of the Window
//<
makeBody : function() {
    // if not showing the body, bail
    if (!this.showBody) return;

    // Body contents can be assigned using the following methods:
    // - The src property can be set to a URL. this will be assigned to the body
    //     canvas' contentsURL property.
    // - The items property can be set to a string. this will be assigned to the
    //     contents property of the body canvas.
    // - The items property can be set to an existing canvas or an array of canvases.
    //     These will be assigned as the body canvas' children.
    var children, contents, contentsURL;
    if (this.src) {
        contentsURL = this.src;
    } else {
        // determine whether to display the window contents as contents or children of the body
        // canvas
        var items = this.items;

        if (isc.isA.Array(items)) {
            // contents are Canvii - duplicate the Array to keep body.children as a distinct
            // array from this.items.
            children = items.duplicate();
        } else if (isc.isA.Canvas(items)) {
            // contents is a single Canvas
            children = items;
        } else {
            // contents is HTML content
            contents = items;
        }

        // For the AutoTest module, mark each item as a locationChild of the window
        // (This could also be achieved via a call to addItems or similar
        if (!isc.isAn.Array(items)) items = [items];
        for (var i = 0; i < items.length; i++) {

            if (isc.isAn.Object(items[i])) {
                items[i].locatorParent = this;
                items[i]._containerID = this.ID;
            }
        }
    }

    // if the bodyConstructor hasn't been set, use the appropriate constructor based on
    // the kind of content we have:
    // - contentsURL: use an HTMLFlow
    // - contents (as a string): use a Canvas
    // - children
    //      - if autoSizing, or explicit contentLayout, use a Layout
    //      - otherwise use a Canvas
    if (!this.bodyConstructor) {
        if (contentsURL) {
            // body will be a normal Canvas (containing an IFrame if contentsURL specified)
            this.bodyConstructor = "HTMLFlow";
        } else if (contents) {
            this.bodyConstructor = "Canvas";

        } else if (!this.autoSize) {
            // if the Window dictates body size, and contentLayout hasn't been set to none, use
            // a Layout
            if (this.contentLayout != "none") this.bodyConstructor = "Layout";
            // if contentLayout is set to none, use a Canvas
            else this.bodyConstructor = "Canvas";
        } else {
            // use a Layout with a none/none policy for autoSize:true
            // so that contents will not be resized when they're first drawn
            // when the window is drag resized, the body's policy will be set to fill/fill
            this.bodyConstructor = "Layout";
            var policyProps = {vPolicy:"none", hPolicy:"none"};
            if (!this.bodyProperties) this.bodyProperties = policyProps;
            else isc.addProperties(this.bodyProperties, policyProps);
        }
    }


    // NOTE: create items instead of allowing it to happen as the body initializes its children
    // array, so that any autoChildren are created in the context of the Window itself, not the
    // body
    this.createCanvii(children);


    if (isc.Browser.isMoz && contentsURL != null) {
        if (!this.body) this.body = {};
        this.body.useClipDiv = false;
    }

    // create the body canvas
    var bodyProps = ("body", {
            contents : contents || "&nbsp;",

            // XXX watch tab can't handle showing non-generated children of generated components.
            // We should fix that.  For now, just flag the body as non-generated
            _generated: false,
            defaultHeight : this.autoSize ? 50 : 100,

            contentsURL : contentsURL,
            contentsType : this.contentsType,

            hideUsingDisplayNone: (isc.Browser.isMoz && contentsURL ? true : false),

            styleName : this.bodyStyle,
            printStyleName : this.printBodyStyle,
            backgroundColor : this.bodyColor,
            // hide initially if we're minimized
            visibility : this.minimized ? isc.Canvas.HIDDEN : isc.Canvas.INHERIT,

            // for when the body is a Layout/Stack
            vertical : (this.contentLayout == isc.Canvas.VERTICAL),

            // when Window size dictates body size, scroll as needed.  Otherwise, expand to body
            // contents
            overflow:this.autoSize ? "visible" : "auto"
    });

    // should the window.items become members or children of the body?
    var bodyClass = isc.ClassFactory.getClass(this.bodyConstructor);
    if (bodyClass && bodyClass.isA("Layout")) {
        bodyProps.members = children;
    } else {
        bodyProps.children = children;
    }

    this.addAutoChild("body", bodyProps);
},

setBodyColor : function (color) {
    this.bodyColor = color;
    if (this.body) this.body.setBackgroundColor(color)
},

hasInherentHeight : function () { return this.autoSize; },
hasInherentWidth : function () { return this.autoSize; },

//>    @method    Window.addItem()    ([A])
// Adds a widget to the body area of the window.
//      @visibility external
//        @group    windowItems
//      @param  item    (Canvas)    the widget to be added
//      @return (array) array of widgets added
//<
addItem : function (item, position) {
    return this.addItems([item], position);
},

//>    @method    Window.removeItem() ([A])
// Removes a widget from the body area of the window.
//      @visibility external
//        @group    windowItems
//      @param  item    (Canvas)    the widget to be removed
//      @return (array) the array of widgets removed
//<
removeItem : function (item) {
    return this.removeItems([item]);
},

//>    @method    Window.addItems([A])
//            Adds an array of widgets to the window.
//      @visibility external
//        @group    windowItems
//      @param  items    (Array of Canvas)    an array of widgets to be added
//      @return (array)  array of widgets added
//<
addItems : function (items, position) {
    if (!isc.isAn.Array(items)) items = [items];

    if (!this.items) this.items = [];

    for (var i =0; i < items.length; i++) {

        // handle calling code that passes null or undefined
        if (!items[i]) continue;

        // Skip any items we already have
        if (this.items.contains(items[i])) continue;

        // add each item to this.items
        if (position != null) this.items.addAt(items[i], position+i);
        else this.items.add(items[i]);

        // Explicitly flag this widget as the locatorParent of the widget - used by the
        // AutoTest module
        items[i].locatorParent = this;

        items[i]._containerID = this.ID;

        // if the body hasn't been created yet, ensure any drawn items are clear()'d, and return
        if (!this._isInitialized) {
            if (isc.isA.Canvas(items[i]) && items[i].isDrawn()) items[i].clear();

        // If the body has been drawn - add the items to it as members/children
        } else {
            // Depending on the contentLayout property the body may be a layout or a straight
            // canvas.  Use addMember if it's there, otherwise just addChild.
            if (this.body.addMember) {
                this.body.addMember(items[i], position != null ? position+i : null);
            } else {
                this.body.addChild(items[i]);
            }
        }

    }

    return items;

},

//>    @method    Window.removeItems([A])
//            Removes an array of widgets from the window.
//      @visibility external
//        @group    windowItems
//      @param  items   (array of canvases) an array of widgets to be removed
//      @return (array) the array of widgets removed
//<
removeItems : function (items) {

    if (!isc.isAn.Array(items)) items = [items];
    for (var i = 0; i < items.length; i++) {
        delete items[i].locatorParent;
    }

    if (this._isInitialized) {
        if (this.body.removeMembers) this.body.removeMembers(items);
        else {
            for (var i=0; i<items.length; i++) {
                if (items[i].parentElement == this.body) items[i].deparent();
            }
        }
    }
    // Remove from this.items
    this.items.removeList(items);
    return items;

},

// we're explicitly marked as the "locatorParent" of items - use the 'locatorChildDestroyed' method
// to clean up the items array on items' destroy() calls
locatorChildDestroyed : function (canvas) {
    if (this.items && this.items.contains(canvas)) this.items.remove(canvas);
},


replaceItem : function (oldItem, newItem) {
    if (oldItem == newItem) return oldItem;
    if (newItem == null) return this.removeItem(oldItem);
    if (oldItem == null) return this.addItem(newItem);

    for (var i =0; i < this.items.length; i++) {
        if (this.items[i] == oldItem) {

            this.items[i] = newItem;

            // if the body hasn't been created yet, ensure any drawn items are clear()'d, and return
            if (!this._isInitialized) {
                if (isc.isA.Canvas(newItem) && newItem.isDrawn()) newItem.clear();
            // If the body has been drawn - add the items to it as members/children
            } else {
                // Depending on the contentLayout property the body may be a layout or a straight
                // canvas.  Use addMember if it's there, otherwise just addChild.

                if (this.body.addMember) {
                    var oldPos = this.body.getMemberNumber(oldItem);
                    this.body.removeMember(oldItem);
                    this.body.addMember(newItem, oldPos);
                } else {
                    this.body.removeChild(oldItem);
                    this.body.addChild(newItem);
                }
            }


            break;
        }
    }
},

//> @method window.addMember() [A]
// Same as +link{layout.addMember()}.  Note that in order to add items to +link{window.body},
// you use +link{window.addItem()} rather than <code>addMember</code>.  Adding a member to
// a Window adds the member as a sibling to the header, body and other built-in Window
// subcomponents.
// @include layout.addMember()
// @visibility external
//<

//> @method window.addMembers() [A]
// Same as +link{layout.addMembers()}.  Note that in order to add items to +link{window.body},
// you use +link{window.addItem()} rather than <code>addMembers</code>.  Adding a member to
// a Window adds the member as a sibling to the header, body and other built-in Window
// subcomponents.
// @include layout.addMembers()
// @visibility external
//<


// Resizing / Layout
// ---------------------------------------------------------------------------------------

// override to handle autoSize:true: make the Window match the body's size
layoutChildren : function (a,b,c,d) {
    if (this.body == null) return;

    if (this._disableAutoSize) {
        this._disableAutoSize = null;
        this.disableAutoSize();
    }

    if (this.autoSize) this._matchBodyWidth();

    this.invokeSuper(isc.Window, "layoutChildren", a,b,c,d);

    // overflow set to visible -- similar to autoSize except content expands to fill
    // specified space as a minimum (but we'll then react to it overflowing)
    if (this.header != null &&
        this.body.overflow == isc.Canvas.VISIBLE && this.overflow == isc.Canvas.VISIBLE)
    {
        // Ensure the header fills the available space.
        // Note that to support shrinking we'll need to shrink to our default specified
        // width, then re-expand to fit available space.
        this.header.setWidth(this.body.getVisibleWidth());
    }


    var edge = this.edgesAsChild ? this._edgedCanvas : null;
    if (edge) edge.setHeight(this.getVisibleHeight(true));
},

_matchBodyWidth : function () {
    if (this.minimized) return;

    if (this._matchingWidth) return;
    this._matchingWidth = true;

    var edge = this.edgesAsChild ? this._edgedCanvas : null;

    if (!this.body.isDrawn()) this.body.draw();

    // if autoSizing, once the body has received an initial width from the Window, don't have
    // the Window's layout code manage the width of the body.  Otherwise, the first time we
    // autoSize to an overflowed body, we'll size the body to it's overflow'd size,
    // establishing the overflowed size as a minimum from then on.
    this.body.inherentWidth = true;

    // the window should be larger than the body by styling width (margin/border/padding) on
    // Window as a whole, plus the layoutMargin, which is interior to styling.
    var edgeWidth = (this.getWidth() - this.getInnerWidth()) +
            this._leftMargin + this._rightMargin;

    // plus the width of rounded edges, if edges are done as a child (otherwise these are
    // already factored in as native margins)
    if (edge) edgeWidth += edge._leftMargin + edge._rightMargin;

    var windowWidth = this.body.getVisibleWidth() + edgeWidth;
    this.logInfo("edgeWidth is: " + edgeWidth + ", setting window width to: " + windowWidth,
                 "layout");

    // setting the Window's width to match the body means all other children (eg the header)
    // will be sized to match the Window's width

    if (this.getWidth() != windowWidth) this.setWidth(windowWidth);

    this._matchingWidth = null;
},

disableAutoSize : function () {
    this.setAutoSize(false);
},

//> @method window.setAutoSize()
// Setter for +link{window.autoSize}
// @param autoSize (boolean) true if the window should auto-size to its content
// @visibility external
//<
setAutoSize : function (autoSize) {
    this.autoSize = autoSize;

    if (autoSize) {
        if (this.body) {
            // set the body to apply a "fill" policy if its a Layout (in autoSize mode we just
            // stack the body items by default)
            if (isc.isA.Layout(this.body)) this.body.vPolicy = this.body.hPolicy = "none";
            // set the body to start scrolling
            this.body.setOverflow("visible");
        }
        // change the policy of the Window as a whole to start setting the size of the body
        // based on the Window size instead of vice versa
        this.vPolicy = "none";
        this.setOverflow("visible");
    } else {
        if (this.body) {
            // set the body to apply a "fill" policy if its a Layout (in autoSize mode we just
            // stack the body items by default)
            if (isc.isA.Layout(this.body)) this.body.vPolicy = this.body.hPolicy = "fill";
            // set the body to start scrolling
            this.body.setOverflow("auto");
            this.body.inherentWidth = false;
        }
        // change the policy of the Window as a whole to start setting the size of the body
        // based on the Window size instead of vice versa
        this.vPolicy = "fill";
        this.setOverflow("hidden");

        if (this.header != null) delete this.header._userWidth;
    }
},

// if we are dragResized, disable autoSizing
dragResizeStart : function () {
    if (this.Super("dragResizeStart", arguments) == false) return;
    // set a flag to disable autoSizing the next time we do layout.
    // NOTE: technically, we should only do this on a successful drag, and for the special case
    // of dragAppearance target, be able to disable autoSizing but re-enable it on drag
    // cancellation.
    if (this.autoSize && isc.EH.dragTarget == this) {
        this.autoSize = false;
        this._disableAutoSize = true;
    }
},

// ---------------------------------------------------------------------------------------

//>    @method    Window.returnValue()
//        @group    data
//             return a value to the callback function
//                and hide the Window
//
//        @param    value    (any)    return value for the Window
//<
returnValue : function (value) {
    if (this.isVisible()) this.hide();
    if (this.callback) {
        //this.fireCallback(this.callback, "value", [value]);
        // the above call needs to be delayed to prevent a bug where subsequent dialogs shown
        // from the callback aren't shown when the cancel button is pressed.
        this.delayCall("fireCallback", [this.callback, "value", [value]], 50);
    }
    return value;
},


// event handling
// -------------------------------------------------------------------------------------------------
//

//>    @method    Window.show()
// Show the Window.
// <P>
// When a modal window is shown, it will begin blocking input to any components on the screen other
// than the modal window.
//        @group    appearance
//<
show : function (a,b,c,d) {
    if (isc._traceMarkers) arguments.__this = this;

    if (this.isModal) {

        // 2 kinds of modality:
        // - modalTarget: mask everything within the modal target, link visibility to modalTarget
        // - no modal target: show global clickMask and bring us above it.
        if (this.modalTarget) {
            if (!isc.isA.Canvas(this.modalTarget) || this.modalTarget.contains(this)) {
                this.logWarn("Invalid modalTarget:" + this.modalTarget +
                             ". Should be a canvas, and not an ancestor of this Window.");
                delete this.modalTarget;
                this.isModal = false;
            } else {

                this.modalTarget.showComponentMask(
                    this.showModalMask ?
                        {styleName: this.modalMaskStyle, opacity: this.modalMaskOpacity } :
                        null
                );
                this.observeModalTarget();
            }

        // Explicitly catch the case of a developer specifying isModal on a non top-level window
        // this will be clearer than a log message about clickMasks.
        } else if (this.topElement != null) {
            this.logWarn("Window specified with 'isModal' set to true, but this window has a " +
                         "parentElement. Only top level Windows can be shown modally.");
            this.isModal = false;
        } else {
            this.showClickMask(
                    {
                        target: this,
                        methodName: (this.dismissOnOutsideClick ? "handleCloseClick"
                                                                : "flash")
                    },
                    false,
                    // Don't mask ourselves

                    [this]);
            this.makeModalMask();
        }
    }

    // If we're going to be auto-centered, draw offscreen before centering

    if (this.autoCenter && !this.parentElement) {

        this._centering = true;

        this.moveTo(0, -1000);
        this._centering = false;
    }

    this.invokeSuper(isc.Window, "show", a,b,c,d);

    if (this.autoCenter) {
        // if we're supposed to autoCenter, center in the page
        this.centerInPage();

        // set up an event to keep autoCentering on page resize
        if (!this.parentElement) {
            isc.Page.setEvent(this._$resize, this, null, "parentResized");
        }
    }

    this.bringToFront(true);
},


makeModalMask : function () {
    if (!this.showModalMask) return;

    if (!this.modalMask) this.modalMask = this.createAutoChild( "modalMask",
        { styleName: this.modalMaskStyle, opacity: this.modalMaskOpacity } );
    this.modalMask.show();
},

hideModalMask : function () {
    if (this.modalMask) this.modalMask.hide();
},

destroyModalMask : function () {
    if (this.modalMask) {
        this.modalMask.destroy();
        this.modalMask = null;
    }
},

// Modal target behavior
// ----------------------------

// if we have a modal target, hide and show / draw and clear with it
observeModalTarget : function () {
    if (this._modalTargetVisibilityChange) return;
    this.observe(this.modalTarget, "show",
                            "observer.modalTargetVisibilityChanged(observed)");
    this.observe(this.modalTarget, "hide",
                            "observer.modalTargetVisibilityChanged(observed)");
    this.observe(this.modalTarget, "clear",
                            "observer.modalTargetVisibilityChanged(observed)");
    this.observe(this.modalTarget, "draw",
                            "observer.modalTargetVisibilityChanged(observed)");
    this.observe(this.modalTarget, "parentVisibilityChanged",
                            "observer.modalTargetVisibilityChanged(observed)");
},

ignoreModalTarget : function () {
    if (this._modalTargetVisibilityChange) return;
    this.ignore(this.modalTarget, "show");
    this.ignore(this.modalTarget, "hide");
    this.ignore(this.modalTarget, "draw");
    this.ignore(this.modalTarget, "clear");
    this.ignore(this.modalTarget, "parentVisibilityChanged");
},

modalTargetVisibilityChanged : function (modalTarget) {
    // set special flag b/c we don't want the hide()/show() calls resulting from our call here
    // to to unregister the observations.
    this._modalTargetVisibilityChange = true;
    if (modalTarget.isVisible() && modalTarget.isDrawn()) this.show();
    else this.hide();
    delete this._modalTargetVisibilityChange;
},

//> @method window.shouldDismissOnEscape()
// Should this window be dismissed (same effect as pressing the "Cancel" button) when the
// user presses the "Escape" key?<br>
// Default behavior: if +link{window.dismissOnEscape} is set, just return it. Otherwise return
// true if this window is showing a "close" control in the header
// (see +link{window.headerControls}).
// @return  (Boolean) true if the window should be dismissed when the user hits escape
// @visibility external
//<
shouldDismissOnEscape : function () {
    if (this.dismissOnEscape != null) return this.dismissOnEscape;
    // If we're showing a close button in our header, return true
    return this.showHeader && this.headerControls &&
            this.showCloseButton && this.headerControls.contains("closeButton");

},

// Implement dismissOnEscape via the bubbled keyPress event.
// This means that if we don't contain focus, or if "Escape" has meaning to a child (EG LG Editing/
// modal windows) we won't get the event and kill the window.
handleKeyPress : function () {
    var keyName = isc.EH.getKey();
    if (keyName == "Escape" && this.shouldDismissOnEscape()) {
        this.handleEscape();
        return false;
    }
    return this.Super("handleKeyPress", arguments);
},

// handleEscape() - fired when the user hits escape if 'dismissOnEscape' is true.
// Fires closeClick() to dismiss the window. Can potentially be overridden for other
// behavior.
handleEscape : function () {
    // If we're under a clickMask, don't dismiss.
    // This handles the case where we're showing 2 windows, the top one of which is modal
    // In this case we want the user to have to interact with the top window before
    // dismissing the window underneath it.
    if (this.isMasked()) return;
    this.handleCloseClick();
},

resized : function (a,b,c,d) {
    this.invokeSuper(isc.Window, "resized", a,b,c,d);
    if (this.autoCenter) this.centerInPage();
},

//>    @method    Window.hide()
//            Hide the Window.  Hides the clickMask for modal Windows.
//        @group    appearance
//<
hide : function (a,b,c,d) {
    //>Animation
    if (this._animatingMinimize) isc.Animation.finishAnimation(this._animatingMinimize);
    //<Animation

    this.invokeSuper(isc.Window, "hide", a,b,c,d);
    if (this.isDrawn() && this.isModal) {
        if (this.modalTarget) {
            this.modalTarget.hideComponentMask();
            this.ignoreModalTarget();
        } else {
            this.hideClickMask();
            this.hideModalMask();
        }
    }
},


//> @method Window.clear()
//  When clearing a modal window, also clear the clickMask
// @group appearance
//<
clear : function (a,b,c,d) {
    //>Animation
    if (this._animatingMinimize) isc.Animation.finishAnimation(this._animatingMinimize);
    //<Animation

    this.invokeSuper(isc.Window, "clear", a,b,c,d);
    if (!this.clearingWithModalTarget && this.isVisible() && this.isModal) {
        if (this.modalTarget) {
            this.ignoreModalTarget();
            this.modalTarget.hideComponentMask();
        } else {
            this.hideClickMask();
            this.hideModalMask();
        }
    }
},

// AutoCenter
// ---------------------------------------------------------------------------------------

// if our parent (or the Page) resizes, autoCenter if configured to do so
parentResized : function () {
    this.Super("parentResized", arguments);
    // auto center, only if we're still set to
    if (this.autoCenter) this.centerInPage();
},

// stop centering if we are moved other than by the autoCentering code itself
handleMoved : function () {
    this.Super("handleMoved", arguments);
    if (this.isDrawn() &&
        !this._centering &&
        // In RTL mode, handleMoved() may be called from adjust overflow. We don't want to switch
        // off auto-centering in that case.
        !this._inAdjustOverflow)
    {
        this.autoCenter = false;
    }
},

//>    @method    Window.centerInPage()   ([A])
// Centers the Window in the page. This is called automatically in window.show() if
// Window.autoCenter is true.
// Note - if the Window is a child of another widget, we center in the parent widget
// rather than centering in the page.
//      @visibility external
//        @group    appearance
//      @see    autoCenter
//<
centerInPage : function () {
    var width = this.getVisibleWidth(),
        height = this.getVisibleHeight(),
        parent = this.parentElement ? this.parentElement : isc.Page,
        left = ((parent.getWidth() - width) / 2) + parent.getScrollLeft(),
        top = ((parent.getHeight() - height) / 2) + parent.getScrollTop();
    // Don't try to apply decimal positions, don't position top of window off-screen
    left = Math.round(left);
    top = Math.max(Math.round(top),0);

    this._centering = true;
    this.moveTo(left, top);
    this._centering = null;
},

// Miscellaneous methods
// -------------------------------------------------------------------------------------------------
//

//>    @method    Window.flash()    ([A])
//          Makes the window header flash if it's visible; if there's no header, or the header
//          is hidden, makes the window body flash instead.
//          <p>
//            This method is executed when users click outside the bounds of a modal window
//            so they'll notice that they have to do something with the window.
//      @visibility external
//        @group    modal
//<

flash : function (step) {
    var showHeader = this.showHeader;

    if (step == null) {
        // kick off a new flashing cycle

        // Set a 'isFlashing' flag, so we don't attempt to start a new flashing cycle in the
        // middle of a running one.
        if (this._isFlashing) return false; // return false to cancel the click
        this._isFlashing = true;

        step = 0;

        // store off the starting styleNames/backgroundColors/Img sources.  NOTE:
        if (showHeader) {
            this._headerStyle = this.header.getStateName();
            if (this.headerBackground) {
                this._headerBGStyle = this.headerBackground.getStateName();
                this._headerBGSrc = this.headerBackground.src;
            }
        } else {
            this._bodyColor = this.body.backgroundColor;
        }
    }
    if (showHeader) {
        // apply the original or flash styles / sources in alternation
        var newStyle = (step % 2 == 0 ? this.hiliteHeaderStyle : this._headerStyle),
            newSrc = (step % 2 == 0 ? this.hiliteHeaderSrc : this._headerBGSrc),
            newBGStyle = (step % 2 == 0 ? this.hiliteHeaderStyle : this._headerBGStyle);

        this.header.setStyleName(newStyle)
        var background = this.headerBackground;
        if (background) {
            this.headerBackground.setStyleName(newBGStyle)
            if (background.setSrc) background.setSrc(newSrc);
        }
    } else {
        // if there's no header, flash the body
        var newColor = (step % 2 == 0 ? this.hiliteBodyColor : this._bodyColor);
        this.body.setBackgroundColor(newColor);
    }

    step++;

    if (step < 4) this.delayCall("flash", [step], 100);
    else this._isFlashing = false;   // clear the isFlashing flag

    return false; // return false to cancel the click
},


//>    @method    Window.minimize()
// Minimize the window. Fired when the user clicks the minimize button if
// +link{window.showMinimizeButton, this.showMinimizeButton} is true.<br>
// Default implementation shrinks the window to just the height of the header bar, hiding the
// body. If +link{window.animateMinimize, animateMinimize} is true, the resize will be animated.
// A restore button will be displayed in place of the minimize button when the window is
// minimized.
// @visibility external
//<
minimize : function () {
    // This will hide everything except the header, and size the window to match the header's
    // height.

    //>Animation
    if (this._animatingMinimize) isc.Animation.finishAnimation(this._animatingMinimize);
    //<Animation
    // Put this AFTER the finish animation call so the interrupted animation will have had
    // time to set this.minimized
    if (this.minimized) return;

    // remember the height (and specified height) that we were before we were minimized
    // (If currently maximized, assume the 'restore' size has already been stored)
    if (!this.maximized) {
        this._restoreHeight = this.getHeight();
        this._restoreVisibleHeight = this.getVisibleHeight();
        // see comments in 'restore' for explanation of '_userHeight' / '_restoreUserHeight'.
        this._restoreUserHeight = this._userHeight;

        // disable drag resize while minimized
        // (No need to do this if we're maximized - already disabled)
        this._canResizeAfterRestore = this.canDragResize;
        this.canDragResize = false;

    // If we're maximized, re-set the maximize button properties
    // (currently it'll be acting as a restore button)
    } else {
        if (this.maximizeButton) {
            this.maximizeButton.addProperties(this.maximizeButtonDefaults);

            this.maximizeButton.redraw();
        }
    }

    var minimizeHeight;
    if (this.minimizeHeight) {
        minimizeHeight = this.minimizeHeight;
    // the minimize height defaults to the header height (plus margins) if present
    } else if (this.showHeader) {
        var headerHeight;
        // header may not be created yet if we are initializing minimized
        if (this.header) {
            headerHeight = this.header.getHeight();
        } else {
            var defaults = this.headerDefaults;
            headerHeight = defaults.height || defaults.defaultHeight;
        }
        minimizeHeight = headerHeight + (this.layoutMargin*2) + this.getVMarginBorderPad();
    } else {
        minimizeHeight = this.defaultMinimizeHeight;
    }

    // enable clipping if not enabled, since otherwise if the body or another component is
    // taller than our minimized size, it will show
    if (this.overflow == isc.Canvas.VISIBLE) {
        this.setHeight(this.getVisibleHeight());
    }

    this._restoreOverflow = this.overflow;
    this.setOverflow("hidden");

    // switch minimize button to a restore button before any animation occurs,
    // but disable it until the minimize is complete
    var minButton = this.minimizeButton;
    if (minButton) {
        minButton.addProperties(this.restoreButtonDefaults);
        minButton.markForRedraw();
    }

    //>Animation
    this._minimizeHeight = minimizeHeight;
    if (this.animateMinimize && this.isDrawn() && this.isVisible()) {
        if (minButton) {
            minButton.disable();
            minButton.redraw();
        }

        // Remember the sizing / overflow of the body for when we're done minimizing
        this._storeContentRestoreStats();
        // Note: we use the same flag for minimize and restore animation IDs since the same
        // actions (show/hide/resize/minimize/restore) all kill the animation
        this._animatingMinimize =
            isc.Animation.registerAnimation(this.animateMinimizeStep,
                                            (this.minimizeTime||this.animateTime),
                                            this.minimizeAcceleration || this.animateAcceleration,
                                            this);
    } else {
    //<Animation
        this.completeMinimize(minimizeHeight);
    //>Animation
    }//<Animation
},

// Helper method to store / reset the sizing (etc) of the body for animated minimize and restore
_storeContentRestoreStats : function () {
    if (this.body) {
        this._bodyRestoreScrollTop = this.body.getScrollTop();
        this._bodyRestoreOverflow = this.body.overflow;
        this._bodyRestoreHeight = this.body.getHeight();
        this._bodyRestoreWidth = this.body.getWidth();
        this._bodyRestoreUserHeight = this.body._userHeight;
        this._bodyRestoreUserWidth = this.body._userWidth;

        if (this._bodyRestoreOverflow == isc.Canvas.VISIBLE) {
            this.body.resizeTo(this.body.getVisibleWidth(), this.body.getVisibleHeight());
        }
        this.body.setOverflow(isc.Canvas.HIDDEN);
    }

    // Footer is overflow visible by default - also make it hidden so we can scroll it
    // and the status / resizer will pop out of view
    // (No need to store the specified size of the footer - this is picked up from the
    // footerHeight property);
    if (this.footer) {
        this._footerRestoreOverflow = this.footer.overflow;
        if (this._footerRestoreOverflow == isc.Canvas.VISIBLE) {
            this.footer.setHeight(this.footer.getVisibleHeight());
        }
        this.footer.setOverflow(isc.Canvas.HIDDEN);
    }

},

_resetContentRestoreStats : function () {
    if (this.body) {
        this.body.scrollTo(null, this._bodyRestoreScrollTop, "restore");
        this.body.resizeTo(this._bodyRestoreWidth, this._bodyRestoreHeight);
        // Resetting _userHeight means that the body doesn't have an explicitly specified height
        // so ensures the layout manages its height as it should.
        this.body._userHeight = this._bodyRestoreUserHeight;
        this.body._userWidth = this._bodyRestoreUserWidth;
        this.body.setOverflow(this._bodyRestoreOverflow);
    }
    if (this.footer) {
        this.footer.scrollTo(null, 0, "restore");
        this.footer.setHeight(this.footerHeight);
        this.footer.setOverflow(this._footerRestoreOverflow);
    }

    delete this._bodyRestoreScrollTop;
    delete this._bodyRestoreHeight;
    delete this._bodyRestoreUserHeight;
    delete this._bodyRestoreWidth;
    delete this._bodyRestoreUserWidth;
    delete this._bodyRestoreOverflow;
    delete this._footerRestoreOverflow;
},

// Used for minimize, restore and maximize animations.
animateMinimizeStep : function (ratio, ID, earlyFinish, restore, maximize) {

    var minimizing = (!restore && !maximize);

    // If we're currently maximized, recalculate the maximizeHeight / width once at the
    // beginning of the animation. This is required to catch the case where the stored height
    // represents the size of a different parent / a parent that has changed sizes etc
    if (this.maximized && !this._recalculatedMaxSize) {
        this._maximizeHeight = (this.parentElement ? this.parentElement.getInnerHeight()
                                                   : isc.Page.getHeight());
        this._maximizeWidth = (this.parentElement ? this.parentElement.getInnerWidth()
                                                    : isc.Page.getWidth());
        this._recalculatedMaxSize = true;
    }


    var initialHeight = this.minimized ? this._minimizeHeight
                                       : this.maximized ? this._maximizeHeight
                                                        : this._restoreVisibleHeight,

        finalHeight = restore ? this._restoreVisibleHeight
                              : maximize ? this._maximizeHeight : this._minimizeHeight,

        initialWidth = this.maximized ? this._maximizeWidth : this._restoreVisibleWidth,
        finalWidth  = maximize ? this._maximizeWidth : this._restoreVisibleWidth;

    var targetHeight = Math.round(initialHeight + (ratio * (finalHeight - initialHeight))),
        targetWidth = (finalWidth == initialWidth ? finalWidth
                        : Math.round(initialWidth + (ratio * (finalWidth - initialWidth))));

    var targetInnerHeight = targetHeight - this.getVMarginBorder() - (2*this.layoutMargin) -
                            (this.showHeader? this.header.getHeight() + this.membersMargin : 0),

        body = (this.showBody ? this.body : null),
        footer = (this.showFooter ? this.footer : null),
        footerSize = 0,
        bodySize = 0,
        footerMax = (footer ? this.footerHeight : 0),
        membersMargin = this.membersMargin || 0;

    // At any point during animation (either direction), if there's just, or less than enough
    // room for the footer, it will be showing and nothing else...
    // If there's enough room for the footer the body will start to show too.
    // So calculate sizes first, then resize / show/hide the necessary widgets
    // NOTE: the toolbar is a child of the body so we don't need to calculate its size
    // and separately show/hide here
    if (footer != null) {
        if (targetInnerHeight <= footerMax) {
            footerSize = targetInnerHeight
        } else {
            footerSize = footerMax;
        }
    }

    var footerMaxSpace = footer ? footerMax + membersMargin : 0;
    if (body != null && (targetInnerHeight > footerMaxSpace)) {
        bodySize = targetInnerHeight - footerMaxSpace;
    }

    //this.logWarn("animation step - size of window:"+ targetHeight +
    //             ", size of body,footer:"+ [bodySize, footerSize]);

    // Actually resize the parts, and if they're clipped, scroll them so they appear to get
    // slid up out of sight.
    if (footer) {
        if (footerSize > 0) {
            if (footer.getHeight() != footerSize) {
                var scrollBottom = footer.getScrollTop() + footer.getViewportHeight();
                footer.resizeTo(null, footerSize);
                footer.scrollTo(null, scrollBottom - footer.getViewportHeight(), "animateMinimize");
            }
            if (!footer.isVisible()) footer.show();

        } else if (footer.isVisible()) {
            footer.hide();
        }
    }

    if (body) {
        if (bodySize > 0) {
            if (body.getHeight() != bodySize) {
                var scrollBottom = body.getScrollTop() + body.getViewportHeight();
                body.resizeTo(null, bodySize);
                body.scrollTo(null, scrollBottom - body.getViewportHeight(), "animateMinimize");
            }
            if (!body.isVisible()) body.show();
        } else if (body.isVisible()) {
            body.hide();
        }
    }

    // Move logic - required for maximizing (where we always move the window to zero/zero so it can
    // take up all available space in the page or parent)
    if (maximize || this.maximized) {
        var initialLeft = (maximize ? this._restoreLeft : 0),
            initialTop = (maximize ? this._restoreTop : 0),
            finalLeft = (maximize ? 0 : this._restoreLeft),
            finalTop = (maximize ? 0 : this._restoreTop);

        this.moveTo(
            Math.round(initialLeft + (ratio * (finalLeft - initialLeft))),
            Math.round(initialTop + (ratio * (finalTop - initialTop))),
            true
        );
    }

    // Call resizeBy directly so we can pass in a special extra param to let it know we're
    // doing an animateMinimize (so don't loop back to 'finish' this animation)
    this.resizeBy((targetWidth - this.getWidth()), (targetHeight - this.getHeight()),
                  null, null, true);

    if (ratio == 1) {

        delete this._recalculatedMaxSize;

        // clean up the strange overflow / sizing properties we had to set during the animation
        this._resetContentRestoreStats();

        // Animation is complete - no need to track the ID
        delete this._animatingMinimize

        if (restore) this.completeRestore(true);
        else if (maximize) this.completeMaximize(true);
        else this.completeMinimize(this._minimizeHeight, true);
    }
},

animateRestoreStep : function (ratio, ID, earlyFinish) {
    this.animateMinimizeStep(ratio, ID, earlyFinish, true);
},

animateMaximizeStep : function (ratio, ID, earlyFinish) {
    this.animateMinimizeStep(ratio, ID, earlyFinish, null, true);
},

//>Animation
// Override 'isAnimating()' to return true if we're doing an animated minimize/maximize
// Return true for type 'minimize' or type 'rect', since we are changing our size.

isAnimating : function (types, a,b,c,d) {
    if (this.invokeSuper(isc.Window, "isAnimating", types, a,b,c,d)) return true;
    if (types && !isc.isAn.Array(types)) types = [types];
    if (this._animatingMinimize &&
        ((types == null) || (types.contains("minimize")) || (types.contains("rect")))) return true;

    return false;
},
//<Animation

// method fired when minimizing is complete
// Split into a separate function to support animated resize
completeMinimize : function (minimizeHeight, animated) {
    this.minimized = true;
    this.maximized = false;

    // Hide everything except the header
    // (If this was an animated minimize they may already be hidden)
    // Note: toolbar is a child of the body
    // Resizer / status bar etc are children of the footer
    if (this.body && this.body.isVisible()) this.body.hide();
    if (this.footer && this.footer.isVisible()) this.footer.hide();



    // make sure the minimize height is respected as a user-specified height for when a Window
    // is being managed by a Layout.  If a Window in a Layout draws normally and then is
    // minimized, the Layout will automatically pick up the minimize size as a user-specified
    // size, but this wouldn't happen if the Window is *initialized minimized*, so we set
    // _userHeight explicitly.
    this._userHeight = minimizeHeight;

    // If this._restoreWidth is set, we were previously maximized - ensure we shrink to the
    // appropriate width
    if (this._restoreWidth != null) {
        if (!animated) this.setWidth(this._restoreWidth);
        this._userWidth = this._restoreWidth;
    }

    if (!animated) {
        this.setHeight(minimizeHeight);
        if (this._restoreLeft != null) this.setLeft(this._restoreLeft);
        if (this._restoreTop != null) this.setTop(this._restoreTop);
    }
    if (this._restoreShowShadow != null) this.setShowShadow(this._restoreShowShadow);

    if (this._canMoveAfterRestore != null && this.headerLabel)
        this.headerLabel.parentElement.canDragReposition = this._canMoveAfterRestore;

    delete this._canMoveAfterRestore;
    // position and shadow are the same for minimized and restored windows
    // clear these out now - they will be re-set if required as part of maximize()
    delete this._restoreTop;
    delete this._restoreLeft;
    delete this._restoreShowShadow;
    delete this._restoreWidth;
    if (this.minimizeButton) this.minimizeButton.enable();
},

//>    @method    Window.restore()
// Restores the window to its specified height and width after a call to +link{window.minimize()} or
// +link{window.maximize()}. Called from a click on the restore button shown in place of the
// minimize or maximize button when the window is minimized or maximized.<br>
// Resizing will occur as an animation if +link{window.animateMinimize} is true.
// @visibility external
//<
restore : function () {

    //>Animation
    // Calling restore() during a minimize (or maximize/restore) animation must kill it right
    // away
    if (this._animatingMinimize) {
        isc.Animation.finishAnimation(this._animatingMinimize);
    }
    //<Animation
    // If we're already 'restored', return
    // Put this AFTER the finish animation call so the interrupted animation will have had
    // time to set this.minimized / this.maximized
    if (!this.minimized && !this.maximized) return;

    if (!this._restoreVisibleHeight) this._restoreVisibleHeight = this.getVisibleHeight();

    // switch minimize button back to a minimize button before any animation occurs,
    // but disable it until the restore is complete
    var restoreButton = (this.minimized ? this.minimizeButton : this.maximizeButton);
    if (restoreButton) {
        restoreButton.addProperties(this.minimized ? this.minimizeButtonDefaults
                                                   : this.maximizeButtonDefaults);
        restoreButton.markForRedraw();
    }

    //>Animation
    if (this.animateMinimize && this.isDrawn() && this.isVisible()) {
        if (restoreButton) {
            restoreButton.disable();
            restoreButton.redraw();
        }
        // Note: before either animated minimize or restore we remember the 'restore' size
        // of the components (the normal drawn size) and at the end of the animation reset them
        // This is cleaner than remembering them before minimize, then resetting them after
        // restore as it should work for the case where a window is drawn initially minimized
        // then restored, etc.
        // This method stores the size / specified size / overflow etc of the footer and body
        this._storeContentRestoreStats();

        // Note: we use the same flag for minimize and restore animation IDs since the same
        // actions (show/hide/resize/minimize/restore) all kill the animation
        this._animatingMinimize =
            isc.Animation.registerAnimation(this.animateRestoreStep,
                                        (this.minimizeTime || this.animateTime),
                                        this.minimizeAcceleration || this.animateAcceleration,
                                        this);
    } else {
    //<Animation
        this.completeRestore();
    //>Animation
    } //<Animation
},


// Finishes a restore - if this is an animated restore this will be fired as the callback when
// the resize to the specified size completes.
completeRestore : function (animated) {


    if (this._restoreOverflow != null) this.setOverflow(this._restoreOverflow);
    if (this._restoreHeight != null) this.setHeight(this._restoreHeight);
    if (this._restoreWidth != null) this.setWidth(this._restoreWidth);

    if (!animated) {
        if (this._restoreLeft != null) this.setLeft(this._restoreLeft);
        if (this._restoreTop != null) this.setTop(this._restoreTop);
    }
    if (this._userHeight != null) this._userHeight = this._restoreUserHeight;
    if (this._userWidth != null) this._userWidth = this._restoreUserWidth;
    if (this._restoreShowShadow != null) this.setShowShadow(this._restoreShowShadow);

    // restore resizability
    if (this._canResizeAfterRestore != null) this.canDragResize = this._canResizeAfterRestore;
    if (this._canMoveAfterRestore != null && this.headerLabel)
        this.headerLabel.parentElement.canDragReposition = this._canMoveAfterRestore;

    var restoreButton = this.minimized ? this.minimizeButton : this.maximizeButton;

    this.minimized = false;
    this.maximized = false;

    // show components
    // If this was an animated restore, or a restore (shrink) from maximized they may already
    // be showing
    this._showComponents();

    // If we were autoSized, reset to autoSize:true (automatically gets set to false when
    // maximized).
    if (this._restoreAutoSize) {
        // reflow before calling 'setAutoSize' - this gives the body width a chance to adjust
        // to match the available space in the window
        this.reflowNow();
        this.setAutoSize(true);
    }

    delete this._restoreHeight;
    delete this._restoreUserHeight;
    delete this._restoreVisibleHeight;
    delete this._canResizeAfterRestore;
    delete this._canMoveAfterRestore;
    delete this._restoreOverflow;
    delete this._restoreWidth;
    delete this._restoreUserWidth;
    delete this._restoreShowShadow;
    delete this._restoreLeft;
    delete this._restoreTop;
    delete this._restoreAutoSize;

    if (restoreButton) restoreButton.enable();
},

// Helper for showing the various parts of the window
_showComponents : function () {
    // show non-header components
    // Note we if we're not drawn we can call 'show()' and 'hide()' on our children to set their
    // visibility property - so when they get drawn they'll be in the right state.
    // (show() / hide() will not effect their visibility on the screen if we are hidden or
    // undrawn)
    if (this.body && !this.body.isVisible()) this.body.show();
    if (this.footer && !this.footer.isVisible()) this.footer.show();
},

//>    @method    Window.maximize()
// Maximize the window. Fired when the user clicks the maximize button if
// +link{window.showMaximizeButton, this.showMaximizeButton} is true.<br>
// Default implementation moves the window to <code>0, 0</code> and resizes the window to
// <code>"100%"</code> on both axes, so it will fill the browser window (or the parent
// of the Window instance, if appropriate).<br>
// If +link{window.animateMinimize, animateMinimize} is true, the maximize will be animated.
// A restore button will be displayed in place of the maximize button when the window is
// maximized.
//
// @visibility external
//<
maximize : function () {

    //>Animation
    if (this._animatingMinimize) isc.Animation.finishAnimation(this._animatingMinimize);
    //<Animation
    // Put this AFTER the finish animation call so the interrupted animation will have had
    // time to set this.minimized
    if (this.maximized) return;

    // If we're already minimized restore size will have been stored already.
    if (!this.minimized) {
        this._restoreHeight = this.getHeight();
        this._restoreVisibleHeight = this.getVisibleHeight();
        this._restoreUserHeight = this._userHeight;

        this._canResizeAfterRestore = this.canDragResize;
        this.canDragResize = false;

    } else {
        // If we're minimized, re-set the minimize button to actually minimize the window
        // (currently it'll be acting as a restore button)
        if (this.minimizeButton) {
            this.minimizeButton.addProperties(this.minimizeButtonDefaults);
            this.minimizeButton.redraw();
        }
    }

    // Remember the position so we can move to 0/0
    this._restoreLeft = this.getLeft();
    this._restoreTop = this.getTop();

    this._restoreWidth = this.getWidth();
    this._restoreVisibleWidth = this.getVisibleWidth();
    this._restoreUserWidth = this._userWidth;

    // we also disable drag-repositioning of maximized windows.
    if (this.headerLabel) {
        this._canMoveAfterRestore = this.headerLabel.parentElement.canDragReposition;
        this.headerLabel.parentElement.canDragReposition = false;
    }

    // the shadow takes up space - we'll hide it when maximized so we don't have unnecessary
    // scrollbars etc.
    this._restoreShowShadow = this.showShadow;
    this.setShowShadow(false);

    // If this is an auto-size window, disable autoSizing while maximized, but re-set when
    // restored
    if (this.autoSize) {
        this._restoreAutoSize = true;
        this.setAutoSize(false);
    }

    // switch maximize button to a restore button
    // but disable it until the maximize is complete
    var maxButton = this.maximizeButton;
    if (maxButton) {
        maxButton.addProperties(this.restoreButtonDefaults);
        maxButton.markForRedraw();
    }

    //>Animation
    if (this.animateMinimize && this.isDrawn() && this.isVisible()) {
        if (maxButton) {
            maxButton.disable();
            maxButton.redraw();
        }
        // maximize height and width are 100% / 100%.
        // We'll animate to the explicit size this resolves to (then set to 100% to support
        // parent resizing, etc)
        this._maximizeHeight = (this.parentElement ? this.parentElement.getInnerHeight()
                                                   : isc.Page.getHeight());
        this._maximizeWidth = (this.parentElement ? this.parentElement.getInnerWidth()
                                                    : isc.Page.getWidth());

        // Note: before either animated minimize or restore we remember the 'restore' size
        // of the components (the normal drawn size) and at the end of the animation reset them
        // This is cleaner than remembering them before minimize, then resetting them after
        // restore as it should work for the case where a window is drawn initially minimized
        // then restored, etc.
        // This method stores the size / specified size / overflow etc of the footer and body
        this._storeContentRestoreStats();

        // Note: we use the same flag for minimize and restore animation IDs since the same
        // actions (show/hide/resize/minimize/restore) all kill the animation
        this._animatingMinimize =
            isc.Animation.registerAnimation(this.animateMaximizeStep,
                                        (this.minimizeTime || this.animateTime),
                                        this.minimizeAcceleration || this.animateAcceleration,
                                        this);
    } else {
    //<Animation
        this.completeMaximize();
    //>Animation
    } //<Animation
},


completeMaximize : function (animated) {

    if (!animated) this.moveTo(0,0);
    // always resize to the percentage value so we resize with our parent.
    this.resizeTo("100%", "100%");

    // show components - required if this was minimized
    // If not previously minimized, or if this was an animated maximize they may already be
    // showing
    this._showComponents();

    this.minimized = false;
    this.maximized = true;

    if (this.maximizeButton) this.maximizeButton.enable();
},

//>Animation
// We must override methods that would cut a minimize / maximize animation short
// This includes:
// - minimize() [above]
// - restore() [above]
// - hide() [later in this file]
// - clear() [later in this file]
// - resizeBy() and resizeTo()

resizeTo : function (width, height, animatingRect, suppressHandleUpdate, animatingMinimize) {

    if (!animatingMinimize && this._animatingMinimize) {
        isc.Animation.finishAnimation(this._animatingMinimize);
    }
    return this.invokeSuper(isc.Window, "resizeTo",
                        width, height, animatingRect, suppressHandleUpdate, animatingMinimize);
},
resizeBy : function (deltaX, deltaY, animatingRect, suppressHandleUpdate, animatingMinimize) {
    if (!animatingMinimize && this._animatingMinimize) {
        isc.Animation.finishAnimation(this._animatingMinimize);
    }
    return this.invokeSuper(isc.Window, "resizeBy",
                        deltaX, deltaY, animatingRect, suppressHandleUpdate, animatingMinimize);
},
//<Animation


// Click Handlers for buttons
// ---------------------------------------------------------------------------------------


_closeButtonClick : function () { return this.handleCloseClick() },

handleCloseClick : function () {
    if (this.onCloseClick && this.onCloseClick() == false) return;
    return this.closeClick();
},

//>    @method    Window.closeClick() ([])
// Handles a click on the close button of this window. The default implementation
// calls +link{window.close(),close()} and returns false to prevent bubbling of the click event.
// <P>
// <smartclient>Override this method if you want
// other actions to be taken.</smartclient>
// <smartgwt>Developers may use <code>addCloseClickHandler()</code> to provide custom
// handling when the user clicks this button.</smartgwt>
// Custom implementations may call <code>close()</code> to trigger the default behavior.
// @return (Boolean) Return false to cancel bubbling the click event
// @group    buttons
// @visibility external
//<
closeClick : function () {
    this.close();
    // cancel the mouseClick
    return false;
},

//> @method window.close()
// Close this window.
// This method is fired by the default +link{closeClick()} implementation.
// Default implementation will hide the window.
// @visibility external
//<
close : function () {
    this.returnValue(null);
    // NOTE: if this Window is going to be reused, it's best to hide() it, otherwise it would be
    // best to destroy() it.
    this.hide();
}

});    // END  Window.addMethods();

isc.Window.registerStringMethods({
    //> @method window.onMaximizeClick()
    // Notification method fired when the user clicks the 'maximize' button.
    // @return (boolean) return false to cancel the default maximize behavior
    // @visibility sgwt
    //<

    onMaximizeClick:"",
    //> @method window.onMinimizeClick()
    // Notification method fired when the user clicks the 'minimize' button.
    // @return (boolean) return false to cancel the default minimize behavior
    // @visibility sgwt
    //<

    onMinimizeClick:"",
    //> @method window.onRestoreClick()
    // Notification method fired when the user clicks the 'restore' button.
    // @return (boolean) return false to cancel the default restore behavior
    // @visibility sgwt
    //<

    onRestoreClick:"",

    //> @method window.onCloseClick()
    // Notification method fired when the user attempts to close the window via a click on the
    // 'close' button, click outside the window if +link{window.dismissOnOutsideClick} is true,
    // or on escape keypress if +link{window.dismissOnEscape} is true.
    // @return (Boolean) return false to cancel the default behavior
    //    (firing +link{window.closeClick()})
    // @visibility sgwt
    //<

    onCloseClick:""
})

// If we set up the 'definePrintWindow()' method, call it now to set up the PrintWindow class
if (isc.definePrintWindow) isc.definePrintWindow();

//!<Deferred

isc.Window.registerDupProperties("items");



//> @object PortalPosition
//
// Represents the position of a +link{Portlet} within a +link{PortalLayout}, indicating the
// column, row, and position within the row.
//
// @visibility external
// @treeLocation Client Reference/Layout/PortalLayout
//<

//> @attr portalPosition.colNum (int : 0 : IR)
//
// The column number occupied by a +link{Portlet} within a +link{PortalLayout}.
//
// @visibility external
//<

//> @attr portalPosition.rowNum (int : 0 : IR)
//
// The row number occupied by a +link{Portlet} within a +link{PortalLayout} column.
//
// @visibility external
//<

//> @attr portalPosition.position (int : 0 : IR)
//
// The position occupied by a +link{Portlet} within a +link{PortalLayout} row
// (generally 0, unless there is more than one Portlet in the row).
//
// @visibility external
//<

//> @class Portlet
// Custom subclass of Window configured to be embedded within a PortalLayout.
// @visibility external
// @treeLocation Client Reference/Layout/PortalLayout
//<
isc.defineClass("Portlet", "Window").addProperties({
    showShadow:false,

    // enable predefined component animation
    animateMinimize:true,

    // Window is draggable with "outline" appearance by default.
    // "target" is the solid appearance.
    dragAppearance:"outline",

    //>@attr portlet.dragType (string : "Portlet" : IRWA)
    // <p>By default, +link{portalLayout.portletDropTypes} is set so that any component can be
    // dragged into a +link{PortalLayout}. If the component is not a +link{Portlet},
    // it will be automatically be wrapped in a newly created +link{Portlet}.</p>
    //
    // <p>If you prefer to only allow real +link{Portlet,Portlets} to be dragged into a
    // +link{PortalLayout}, then you can set +link{portalLayout.portletDropTypes} to
    // <code>["Portlet"]</code>, since <code>Portlet.dragType</code> defaults to
    // <code>"Portlet"</code>.</p>
    //
    // <p>if you want to allow some +link{Portlet,Portlets} to be dropped on a +link{PortalLayout}
    // but not others, then you can specify a custom <code>dragType</code> for +link{Portlet,Portlets},
    // and then set +link{PortalLayout.portletDropTypes} as appropriate.</p>
    //
    // @group dragdrop
    // @see canvas.dragType
    // @see portalLayout.portletDropTypes
    // @visibility external
    //<
    dragType: "Portlet",

    //>@attr portlet.canDrop (boolean : true : IRW)
    // Portlets have canDrop set to true to enable drag/drop reposition within the portalLayout
    // @visibility external
    // @example repositionPortlets
    //<
    canDrop:true,

    // We don't want to allow the Portlet itself to overflow
    overflow: "hidden",

    //>@attr portlet.minHeight (Number : 60 : IRW)
    // Specifies a minimum height for the Portlet. The height of rows within a +link{PortalLayout}
    // will be adjusted to take into account the minHeight of all the Portlets in that row.
    // @see Canvas.minHeight
    // @visibility external
    //<
    minHeight: 60,

    setMinHeight : function (height) {
        this.minHeight = height;
        if (this.portalRow) this.portalRow._checkPortletHeights();
    },

    //>@attr portlet.minWidth (Number : 70 : IRW)
    // Specifies a minimum width for the Portlet.
    // @see Canvas.minWidth
    // @visibility external
    //<
    minWidth: 70,

    setMinWidth : function (width) {
        if (this.minWidth == width) return;
        this.minWidth = width;
        if (this.portalRow) this.portalRow.reflow("Portlet minWidth changed");
    },

    //>@attr portlet.height (Number or String : null : IRW)
    //
    // If you initialize the height of a Portlet, then that height will be used as the
    // Portlet's +link{rowHeight,rowHeight} (if no rowHeight is set).
    // <p>
    // After initialization, the +link{PortalLayout} manages the height of Portlets. If you
    // want to change the height, use +link{setRowHeight()}.
    //
    // @see rowHeight
    // @see setRowHeight()
    // @visibility external
    //<

    //>@method portlet.setHeight()
    //
    // The height of a Portlet is managed by the +link{PortalLayout}. If you want to change
    // the Portlet's height, use +link{setRowHeight()} instead.
    //
    // @group sizing
    // @param height (number) new height
    // @see setRowHeight()
    // @see rowHeight
    // @visibility external
    //<

    //>@attr portlet.rowHeight (Number or String : null : IRW)
    // If you set the rowHeight of a Portlet before adding it to a +link{PortalLayout}, then
    // the height will be used when creating the new row. If adding the Portlet
    // to an existing row (or dragging it there), the Portlet's rowHeight will be used if
    // the row's height has not already been specified. However, if you
    // set the rowHeight of a Portlet after adding it to the PortalLayout, then the height
    // of the entire row will always be adjusted to match.
    // <p>
    // You can also just specify +link{Canvas.height,height} when initializing a Portlet, and it
    // will be applied to the rowHeight when added to a PortalLayout. However, changing the Portlet's
    // height after initialization will not affect the row.
    // <p>
    // Note that getting the rowHeight will indicate the rowHeight specified for this Portlet,
    // not the actual height of the row it is in.
    // @group sizing
    // @example portalLayoutColumnHeight
    // @visibility external
    //<

    // Also see the code in portalRow.addPortlets to see how the rowHeight and _userHeight are applied there.
    // Note that we do not keep track of changes to the real row's height -- we are only responding
    // to explicitly setting rowHeight on the portlet. Tracking the real row's height may sometimes make some
    // sense, but the user's intentions aren't necessarily easy to model -- for instance, the user may have
    // shrunk a row but want the portlet to automatically expand if dragged to an empty column.

    //>@method portlet.setRowHeight()
    // Sets the height of the Portlet's row (and, thus, indirectly sets the Portlet's own height).
    // Use this instead of using +link{setHeight()} directly.
    // @param height (number) new height
    // @group sizing
    // @visibility external
    //<
    setRowHeight : function (height) {
        this.rowHeight = height;
        if (this.portalRow) this.portalRow.setHeight(height);
    },

    //> @method portlet.getPortalLayout()
    // Gets the +link{PortalLayout} which encloses this Portlet (or null, if none).
    // @return (PortalLayout) the PortalLayout enclosing this Portlet
    // @visibility external
    //<
    getPortalLayout : function () {
        if (this.portalRow) {
            return this.portalRow.portalLayout;
        } else {
            return null;
        }
    },

    //> @method portlet.getPortalPosition()
    // Gets the position of the Portlet within its +link{PortalLayout}. Returns null
    // if the Portlet is not in a PortalLayout.
    // @return (PortalPosition) the position of the Portlet
    // @visibility external
    //<
    getPortalPosition : function () {
        var layout = this.getPortalLayout();
        if (layout) {
            return layout.getPortalPosition(this);
        } else {
            return null;
        }
    },

    // Resize from any edge except corners -- the resize may be "grabbed" by a parent,
    // and it's simpler if we don't have to deal with resizing two directions at once.
    resizeFrom: ["T", "B", "L", "R"],

    edgeCursorMap: {
        "L": "col-resize",
        "R": "col-resize",
        "T": "row-resize",
        "B": "row-resize"
    },

    // customize the appearance and order of the controls in the window header
    // (could do this in load_skin.js instead)
    showMaximizeButton: true,
    headerControls:["headerLabel", "minimizeButton", "maximizeButton", "closeButton"],

    // show either a shadow, or translucency, when dragging a portlet
    // (could do both at the same time, but these are not visually compatible effects)
    //showDragShadow:true,
    dragOpacity:30,

    //>@attr portlet.showCloseConfirmationMessage (Boolean : true : IRW)
    // If true, +link{closeConfirmationMessage} will be displayed before portlets are closed
    // @visibility external
    //<
    showCloseConfirmationMessage:true,

    //>@attr portlet.closeConfirmationMessage (string : "Close portlet?" : IRW)
    // Confirmation message to show the user when closing portlets if
    // +link{showCloseConfirmationMessage} is true.
    // @visibility external
    // @group i18nMessages
    //<
    closeConfirmationMessage:"Close portlet?",

    //>@attr portlet.destroyOnClose (Boolean : false : IRW)
    // Whether to call +link{Canvas.destroy,destroy()} when closing the Portlet.
    // @visibility external
    //<

    //>@method portlet.closeClick()
    // Handles a click on the close button of this portlet.  The default implementation
    // calls +link{close()}.
    // <P>
    // <smartclient>Override this method if you want
    // other actions to be taken.</smartclient>
    // <smartgwt>Developers may use <code>addCloseClickHandler()</code> to provide custom
    // handling when the user clicks this button.</smartgwt>
    // Custom implementations may call <code>close()</code> to trigger the default behavior.
    // @visibility external
    //<

    //> @method portlet.close()
    // <code>close()</code> method overridden to show
    // +link{portlet.closeConfirmationMessage} to the user before
    // removing the portlet from the PortalLayout via +link{portalLayout.removePortlet()}
    // @visibility external
    //<
    close : function () {
        var portalLayout = this.getPortalLayout();
        if (portalLayout && portalLayout.willClosePortlet) {
            var proceed = portalLayout.willClosePortlet(this);
            // Require explicit return of false to cancel
            if (proceed === false) return;
        }

        if (this.showCloseConfirmationMessage) {
            isc.confirm(this.closeConfirmationMessage,
                    {target:this, methodName:"confirmedClosePortlet"});
        } else {
            this.confirmedClosePortlet(true);
        }
    },

    confirmedClosePortlet : function (value) {
        if (!value) return;

        // If we have an editContext, we'll do the removal from there whether or not
        // we are in editMode
        if (this.editContext && this.editNode) {
            this.editContext.removeNode(this.editNode);
        } else {
            if (this.portalRow) {
                this.portalRow.removePortlets(this);
            } else {
                this.clear();
            }
        }

        if (this.destroyOnClose) this.markForDestroy();
    },

    onMaximizeClick : function () {
        var portalLayout = this.getPortalLayout();
        if (portalLayout && portalLayout.willMaximizePortlet) {
            // Require explicit return of false to cancel
            return portalLayout.willMaximizePortlet(this) === false ? false : true;
        } else {
            return true;
        }
    },

    onMinimizeClick : function () {
        var portalLayout = this.getPortalLayout();
        if (portalLayout && portalLayout.willMinimizePortlet) {
            // Require explicit return of false to cancel
            return portalLayout.willMinimizePortlet(this) === false ? false : true;
        } else {
            return true;
        }
    },

    onRestoreClick : function () {
        var portalLayout = this.getPortalLayout();
        if (portalLayout && portalLayout.willRestorePortlet) {
            // Require explicit return of false to cancel
            return portalLayout.willRestorePortlet(this) === false ? false : true;
        } else {
            return true;
        }
    },

    _createPlaceholder : function () {
        if (this._portletPlaceholder) return;

        // First we record the absolute coordinates of the portlet, so that
        // when we remove it from the layout, we can draw it at the same place ...
        // that will make the animation look right.
        var width = this.getVisibleWidth(),
            height = this.getVisibleHeight(),
            pageLeft = this.getPageLeft(),
            pageTop = this.getPageTop(),
            userHeight = this._userHeight,
            userWidth = this._userWidth
        ;

        this._portletPlaceholder = isc.Canvas.create({
            width: width,
            height: height,
            minHeight: this.getMinHeight(),
            minWidth: this.getMinWidth(),
            minimized: this.minimized,
            _userHeight: this._userHeight,
            _userWidth: this._userWidth,
            _portlet: this
        });

        if (this.parentElement) {
            this.masterLayout = this.parentElement;
            this.masterLayout.portletMaximizing = true;
            this.masterLayout.replaceMember(this, this._portletPlaceholder, false);
            this.masterLayout.portletMaximizing = false;
        }

        // Now that we've swapped it out of the layout, redraw it in the same place
        this.setWidth(width);
        this.setHeight(height);

        // But, give it the same _userHeight and _userWidth so they get restored.
        this._userHeight = userHeight;
        this._userWidth = userWidth;

        this.moveTo(pageLeft, pageTop);
        this.bringToFront();
        this.draw();
    },

    _destroyPlaceholder : function () {
        if (!this._portletPlaceholder) return;

        if (this.masterLayout && this.masterLayout.hasMember(this._portletPlaceholder)) {
            this.masterLayout.portletMaximizing = true;
            this.masterLayout.replaceMember(this._portletPlaceholder, this);
            this.masterLayout.portletMaximizing = false;
        }
        this._portletPlaceholder._portlet = null;
        this._portletPlaceholder.destroy();

        delete this._portletPlaceholder;
        delete this.masterLayout;
    },

    maximize : function () {
        this._createPlaceholder();

        // Now that we've redrawn it outside the layout, do the regular maximizing
        this.delayCall("doMaximize");
    },

    restore : function () {
        // If we're restoring the portlet, make sure that its row is restored
        // first, to make space for it. We don't need to check other portlets,
        // since if one is restored then the row needs to be restored. Note
        // that this is a no-op if the row is already restored.
        if (this.portalRow) this.portalRow.restore();

        this.Super("restore", arguments);
    },

    completeRestore : function () {
        this.Super("completeRestore", arguments);
        this._destroyPlaceholder();
        if (this.portalRow) this.portalRow._checkPortletHeights();

        var portalLayout = this.getPortalLayout();
        if (portalLayout && portalLayout.portletRestored) {
            portalLayout.portletRestored(this);
        }
    },

    doMaximize : function () {
        this.Super("maximize", arguments);
    },

    completeMaximize : function () {
        this.Super("completeMaximize", arguments);
        var portalLayout = this.getPortalLayout();
        if (portalLayout && portalLayout.portletMaximized) {
            portalLayout.portletMaximized(this);
        }
    },

    completeMinimize : function () {
        this.Super("completeMinimize", arguments);
        this._percent_height = null;
        this._percent_width = null;
        this._destroyPlaceholder();
        if (this.portalRow) this.portalRow._checkPortletHeights();

        var portalLayout = this.getPortalLayout();
        if (portalLayout && portalLayout.portletMinimized) {
            portalLayout.portletMinimized(this);
        }
    },

    resized : function (deltaX, deltaY, reason) {
        this.Super("resized", arguments);

        // If we're maximizing or restoring, then ignore the resize
        if (this.masterLayout) return;

        var portalLayout = this.getPortalLayout();
        if (portalLayout) portalLayout._portletsResized();
    }
});

// A resizeBar for Portlets and PortalRows. We don't actually want to show a
// visible resizeBar between Portlets and between PortalRows, because the UI
// becomes visually cluttered with the edges of the Portlet (which are
// themselves draggable). As far as spacing goes, simply using membersMargin
// would be sufficient. However, using membersMargin creates a zone in which
// drags cannot easily be initiated, because the membersMargin is not literally
// a margin ... it merely affects the location of the next member. So there is
// nothing in the "margin" to initiate a drag. Thus, we need a specialized
// resizeBar in order to have both spacing and resizing by initiating a drag
// in that space. Essentially, this is an invisible resizeBar.
isc.defineClass("PortalResizeBar", isc.Canvas);

isc.PortalResizeBar.addProperties({
    dragStartDistance: 1,
    isMouseTransparent: true,
    overflow: "hidden",
    _redrawWithMaster: false,
    _resizeWithMaster: false,

    // We'll resize the target ...
    canDragResize: true,

    initWidget : function () {
        this.Super("initWidget", arguments);

        this.dragTarget = this.target;
    },

    edgeCursorMap: {
        "R": "col-resize",
        "B": "row-resize"
    },

    // Return the event edge relative to the target
    getEventEdge : function () {
        return this.vertical ? "R" : "B";
    }
});

// provides a menu for adding a remove columns
isc.defineClass("PortalColumnHeader", "HLayout").addProperties({
    height: 20,
    noResizer: true,

    border:"1px solid #CCCCCC",

    // allow dragging by the header
    canDragReposition: true,

    initWidget : function () {
        this.Super("initWidget", arguments);

        // header drags the portalColumn
        this.dragTarget = this.creator;

        this.addMember(isc.LayoutSpacer.create());

        this.menu = this.getMenuConstructor().create({
            width: 150,
            portalColumn: this.creator,
            data: [{
                title: "Remove Column",
                click: "menu.portalColumn.removeSelf()",
                // Don't offer to remove the last column.
                enableIf: function (target, menu, item) {
                    return menu.portalColumn.portalLayout.getMembers().length > 1;
                }
            },{
                title: "Add Column",
                click: "menu.portalColumn.addNewColumn()"
            }]
        });

        this.addMember(isc.MenuButton.create({
            title: "Column Properties",
            width: 150,
            menu: this.menu
        }));

        this.addMember(isc.LayoutSpacer.create());
    }
});

// Manages horizontal vs vertical drag and drop such that a drop to the sides is a drop within
// this PortalRow and a drop above or below is a drop within the parent, before or after this
// PortalRow.
// Created whenever a drop occurs in a PortalColumn (even if it's the first drop).
// Note that you can drop just about anything on a PortalRow -- it will be wrapped in a Portlet
// if necessary.
isc.defineClass("PortalRow", "Layout").addProperties({
    vertical : false,

    // This makes VisualBuilder generate code inline for PortalRows, rather than
    // creating standalone.
    _generated: true,

    // To apply the minWidth of Portlets
    respectSizeLimits: true,

    // Note that by default, we stretch column widths so that the scroll bars will not appear
    // at the row level ... see portalLayout.canStretchColumnWidths ... thus this setting should
    // only matter when canStretchColumnWidths is false.
    overflow: "auto",

    // leave some space between portlets
    resizeBarClass: "PortalResizeBar",
    resizeBarSize: 3,
    defaultResizeBars: "middle",

    // enable drop handling
    canAcceptDrop:true,

    // change appearance of drag placeholder and drop indicator
    dropLineThickness:2,
    dropLineProperties:{backgroundColor:"blue"},

    edgeCursorMap: {
        "T": "row-resize",
        "B": "row-resize"
    },

    // Will accept a portlets attribute and add it (or them, if an array) to the portalRow
    initWidget : function () {
        this.Super("initWidget", arguments);
        if (this.portlets) this.addPortlets(this.portlets);
        this.portlets = null;
    },

    // When creating resizeBars for Portlets, intialize canDragResize
    createResizeBar : function () {
        var resizeBar = this.Super("createResizeBar", arguments);
        resizeBar.canDragResize = this.canResizePortlets;
        return resizeBar;
    },

    setCanResizePortlets : function (canResize) {
        this.canResizePortlets = canResize;

        // If the row has a resizeBar, update it
        if (this._resizeBar) this._resizeBar.canDragResize = canResize;

        // And update portlets and their resizeBars
        this.getPortlets().map(function (portlet) {
            portlet.canDragResize = canResize;
            if (portlet._resizeBar) portlet._resizeBar.canDragResize = canResize;
        });
    },

    // Check whether we want to grab the resize started by a Portlet
    prepareForDragging : function () {
        var EH = this.ns.EH;

        // If no one has already set the dragTarget, then do the usual thing to set it
        if (!EH.dragTarget) this.Super("prepareForDragging", arguments);

        // Make certain adjustments if the dragTarget is one of our Portlets
        if (this.hasMember(EH.dragTarget) && EH.dragOperation == EH.DRAG_RESIZE) {
            switch (EH.resizeEdge) {
                case "B":
                case "T":
                    // Don't resize the Portlet -- resize me instead!
                    EH.dragTarget = this;
                    break;

                case "L":
                    var index = this.getMemberNumber(EH.dragTarget);
                    if (index > 0) {
                        // If we're resizing from the left edge, and there is a previous portlet
                        // then switch to it, and switch edges -- this makes the layout reflow
                        // in a way that makes more sense to the user.
                        EH.dragTarget = this.getMember(index - 1);
                        EH.resizeEdge = "R";
                    } else {
                        // If we're resizing from the left edge, and there is no previous portlet,
                        // then the user essentially wants to move this portlet left. The only
                        // way to accomplish that is to move the column to the left, so we actually
                        // need to resize the column. Note that the prepareForDragging event will
                        // bubble up to the PortalColumn, which may make further adjustments.
                        EH.dragTarget = this.portalColumn;
                    }
                    break;

                case "R":
                    var index = this.getMemberNumber(EH.dragTarget);
                    var peers = this.getMembers().length;

                    if (index == peers - 1) {
                        // If we're resizing the last Portlet in the row, then the user likely
                        // means to be resizing the column instead.
                        EH.dragTarget = this.portalColumn;
                    }
                    break;
            }
        }

        // Check whether we are the dragTarget ... either natively or because
        // we took over the drag.
        if (EH.dragTarget == this && EH.resizeEdge == "T" && EH.dragOperation == EH.DRAG_RESIZE) {
            // If we're resizing from the top edge, then resize fhe
            // previous row from the bottom edge instead (unless there
            // is no previous row, in which case we cancel). The reason
            // is that the Layout will then reflow in ways that seem
            // more natural to the user. We could just disallow
            // resizing from the top edge instead, but this seems to
            // work well.
            var index = this.parentElement.getMemberNumber(this);

            if (index > 0) {
                EH.dragTarget = this.parentElement.getMember(index - 1);
                EH.resizeEdge = "B";
            } else {
                EH.dragTarget = null;
            }
        }
    },

    // Reset each portlet's resizeFrom so that we don't advertise resizes which
    // we would end up canceling.
    //
    // Note that we enforce PortalLayout.canResizeColumns in cases where
    // resizing a Portlet would be delegated up to resize the column. Thus, if
    // PortalLayout.canResizeColumns is false, then resizing Portlets from the
    // left or right edge will only be allowed where we are really just
    // resizing Portlets (i.e. where there are multiple Portlets in the row).
    _updatePortletResizeFrom : function () {
        // Bail if we have no members or no parent
        if (this.members.length == 0 || !this.parentElement || !this.portalLayout) return;

        var rowIndex = this.parentElement.getMemberNumber(this);
        var columnIndex = this.portalLayout.getPortalColumnNumber(this.portalColumn);

        // For the row itself.
        this.resizeFrom = rowIndex > 0 ? ["T", "B"] : ["B"];

        for (var portletIndex = 0; portletIndex < this.members.length; portletIndex++) {
            // We always allow "B" (assuming that canResizePortlets is true -- otherwise,
            // it doesn't matter). This allows the bottom-most Portlet to be enlarged,
            // which would make sense to force overflow, if that is what the user wants
            // (i.e. vertical scrolling).
            var resizeFrom = ["B"];

            // We allow "T" except in the top row, since resizing the top Portlet from
            // the top edge doesn't make sense (it's pinned to the top anyway).
            if (rowIndex > 0) resizeFrom.add("T");

            // We allow "L" if there is a Portlet to our left, since in that
            // case we really would be resizing Portlets. If not, we check for
            // a column to our left (since dragging the left-most column to the
            // left doesn't make sense). If there is a column to our left, the
            // drag would be delegated up to the column, so we check whether
            // canResizeColumns is set.
            if (portletIndex > 0 || (columnIndex > 0 && this.portalLayout.canResizeColumns)) {
                resizeFrom.add("L");
            }

            // We allow "R" if there is a Portlet to our right, since in that
            // case we really would be resizing Portlets. If not, then the drag
            // would be delegated up to the column, so we consult
            // canResizeColumns. This allows dragging the right-most column to
            // the right, since that can make sense to force overflow if that
            // is what the user wants (i.e. horizontal scrolling).
            if (portletIndex != this.members.length - 1 || this.portalLayout.canResizeColumns) {
                resizeFrom.add("R");
            }

            var portlet = this.getMember(portletIndex);
            portlet.resizeFrom = resizeFrom;
        }
    },

    // number of pixels you have to be within the left or right border of a portlet for us to
    // show a drop to the left or right of this portlet.  If not within this margin, drop is
    // indicated above or below instead.
    hDropOffset: 15,
    isHDrop : function () {
        var dropPosition = this.getDropPosition();
        var dropOverTarget = this.getMember(dropPosition == 0 ? 0 : dropPosition - 1);
        if (!dropOverTarget.containsEvent() && dropPosition < this.members.length) {
            dropOverTarget = this.getMember(dropPosition);
        }

        var targetOffsetX = dropOverTarget.getOffsetX();
        if (targetOffsetX < this.hDropOffset || targetOffsetX > dropOverTarget.getVisibleWidth() - this.hDropOffset) {
            return true;
        } else {
            return false;
        }
    },

    // We pass through the drop if it is a PortalColumn, since it doesn't make sense to drop
    // a PortalColumn on a PortalRow -- the PortalLayout will handle it.
    isPortalColumnDrop : function () {
        var dragTarget = this.ns.EH.dragTarget;
        var type = dragTarget.getDragType();
        if (type == "PortalColumn") return true;

        //>EditMode
        if (dragTarget.isA("Palette")) {
            var data = dragTarget.getDragData(),
                component = (isc.isAn.Array(data) ? data[0] : data);
            if (component.type == "PortalColumn" || component.className == "PortalColumn") return true;
        }
        //<EditMode

        return false;
    },

    // EventHandler calls this before doing the actual drop (but not for calling dropMove or dropOver)
    // If we answer 'false', then the drop won't occur. Thus, we can't just answer for ourselves and
    // let the drop bubble up -- there won't be any bubbling if we answer 'false'.
    // So, we have to delegate and give the right answer (or not be the dropTarget).
    willAcceptDrop : function() {
        delete this.dropTarget;

        // If we're dropping a PortalColumn, then the dropTarget should be the PortalLayout
        if (this.isPortalColumnDrop()) {
            this.dropTarget = this.portalLayout;
            return this.portalLayout.willAcceptDrop();
        }

        // If we're not near a horizontal edge, then change the dropTarget
        // so that the rowLayout will handle it
        if (!this.isHDrop()) {
            this.dropTarget = this.parentElement;
            return this.dropTarget.willAcceptDrop();
        }

        // By default, portalLayout.willAcceptPortletDrop just calls back to our
        // superclass. But subclasses of portalLayout could do something different.
        return this.portalLayout.willAcceptPortletDrop(
            this.ns.EH.dragTarget,
            this.portalLayout.getPortalColumnNumber(this.portalColumn),
            this.portalColumn.getPortalRowNumber(this),
            this.getDropPosition()
        );
    },

    drop : function () {
        // There is a small chance that we've just changed the dropTarget, since EventHandler
        // has just called willAcceptDrop(). So, make sure that we use the right dropTarget.
        if (this.dropTarget) {
            this.dropTarget.drop();
        } else {
            this.Super("drop", arguments);
        }

        // In either case, return false to stop bubbling -- if we've got this far, then we've
        // handled the drop and shouldn't bubble it.
        return false;
    },

    dropMove : function () {
        // Note that willAcceptDrop may change the dropTarget
        if (this.willAcceptDrop()) {
            if (this.dropTarget) {
                // If we're not the actual dropTarget, then hide our drop line and delegate
                this.hideDropLine();
                this.dropTarget.dropMove();
            } else {
                // If we are the actual dropTarget, then hide our parent's drop line and show ours
                this.parentElement.hideDropLine();
                this.showDropLine();
            }
            // In either case, we've handled it, so stop the bubbling
            return false;
        } else {
            // If we don't think anyone is accepting the drop, then hide the drop line and let
            // the event bubble.
            this.hideDropLine();
            return true;
        }
    },

    dropOver : function () {
        // Note that willAcceptDrop may change the dropTarget
        if (this.willAcceptDrop()) {
            if (this.dropTarget) {
                // If we're not the actual dropTarget, then hide our drop line and delegate
                this.hideDropLine();
                this.dropTarget.dropOver();
            } else {
                // If we are the actual dropTarget, then hide our parent's drop line and show ours
                this.parentElement.hideDropLine();
                this.showDropLine();
            }
            // In either case, we've handled it, so stop the bubbling
            return false;
        } else {
            // If we don't think anyone is accepting the drop, then hide the drop line and let
            // the event bubble.
            this.hideDropLine();
            return true;
        }
    },

    getDropComponent : function (dragTarget, dropPosition) {
        var dropComponent = this.portalLayout.getDropPortlet(
            dragTarget,
            this.portalLayout.getPortalColumnNumber(this.portalColumn),
            this.portalColumn.getPortalRowNumber(this),
            dropPosition
        );

        //>EditMode
        if (this.handleDroppedEditNode) dropComponent = this.handleDroppedEditNode(dropComponent, dropPosition);
        //<EditMode

        if (dropComponent) {
            // Check if the dropComponent is a Portlet (or subclass) ... if not, wrap it in one.
            if (!isc.isA.Portlet(dropComponent)) {
                dropComponent = isc.Portlet.create({
                    autoDraw: false,
                    title: "",
                    items: dropComponent,
                    destroyOnClose: true
                });
            }
        }

        return dropComponent;
    },

    setMinHeight : function (height) {
        if (this.minHeight == height) return;
        this.minHeight = height;
        if (this.portalColumn) this.portalColumn.rowLayout.reflow("PortalRow minHeight changed");
    },

    shouldAlterBreadth : function (member) {
        // Don't change height of minimized members
        if (member.minimized) return false;

        return this.Super("shouldAlterBreadth", arguments);
    },

    _checkPortletHeights : function () {
        // Check if all the portlets are minimized ... in that case, minimize
        // the row. Otherwise, restore the row. Note that both minimize and
        // restore are no-ops if we're already in the desired state.
        if (this.members.map(function (portlet) {
            return portlet.minimized;
        }).and()) {
            this.minimize();
        } else {
            this.setMinHeight(
                this.members.map("getMinHeight").max() +
                this._getBreadthMargin() +
                this.getVMarginBorder()
            );
            this.restore();
        }
    },

    minimize : function () {
        if (this.minimized) return;

        this.setMinHeight(0);

        this._restoreHeight = this.getHeight();
        this._restoreUserHeight = this._userHeight;

        this.setHeight(
            this.members.map("getHeight").max() +
            this._getBreadthMargin() +
            this.getVMarginBorder()
        );

        this.minimized = true;
    },

    restore : function () {
        if (!this.minimized) return;

        this.setHeight(this._restoreHeight);
        this._userHeight = this._restoreUserHeight;

        delete this._restoreHeight;
        delete this._restoreUserHeight;

        this.minimized = false;
    },

    // We always reflow the PortalLayout when reflowing a row, because the layout may need
    // to resize a column. This could possibly be optimized so that we don't need to reflow
    // the PortalLayout every time.
    reflow : function () {
        if (this.portalLayout) this.portalLayout.reflow("portalRow reflowed");
        this.Super("reflow", arguments);
    },

    // The sizing policy we want is a bit special. We want to manage percent sizes (or * sizes)
    // like a Layout, but we want to be able to force overflow (though minWidth or simply
    // manually setting widths) like a stack. The easiest way seems to be to manipulate
    // getTotalMemberSpace to ensure that we're big enough. We implement _getDesiredMemberSpace
    // separately so that the PortalColumn and PortalLayout can access it.
    _getDesiredMemberSpace : function () {
        return this.members.map(function (member) {
            if (isc.isA.Number(member._userWidth)) {
                return Math.max(member._userWidth, member.minWidth);
            } else {
                // If the _userWidth is a percentage, then we reserve enough room
                // for its minWidth. That way, we'll always have enough room to
                // let the percentage resolve to the minWidth, but the percentage
                // can resolve to more if more space is available.
                return member.minWidth;
            }
        }).sum();
    },

    // Returns the part of the width not accounted for in desiredMemberSpace or totalMemberSpace
    _getWidthOverhead : function () {
        return this.getMarginSpace() + this.getHMarginBorder();
    },

    getTotalMemberSpace : function () {
        var normalMemberSpace = this.Super("getTotalMemberSpace", arguments);
        var desiredMemberSpace = this._getDesiredMemberSpace();
        if (normalMemberSpace < desiredMemberSpace) {
            return desiredMemberSpace;
        } else {
            // If we have enough member space, return what the parent would give us,
            // so we'll fill it.
            return normalMemberSpace;
        }
    },

    // Called when the PortalColumn is resized to be smaller ... force each
    // row's width to be smaller if necessary, by shrinking portlets from the
    // right, respecting only the portlet's minWidth.
    _applyMaxWidth : function (width) {
        var length = this.members.length;
        if (length == 0) return;

        var excessWidth = this._getDesiredMemberSpace() + this._getWidthOverhead() - width;
        if (excessWidth <= 0) return;

        // Iterate backwards through the members while we still have excessWidth to remove
        for (var i = length - 1; i >= 0 && excessWidth > 0; i--) {
            var portlet = this.getMember(i);
            var currentWidth = portlet.getWidth();
            var subtract = Math.min(currentWidth - portlet.minWidth, excessWidth);
            if (subtract > 0) {
                portlet.setWidth(currentWidth - subtract);
                excessWidth -= subtract;
            }
        }
    },

    membersChanged : function () {
        // Ignore if we're just swapping placeholders in and out
        if (this.portletMaximizing) return;

        if (this.members.length == 0) {
            //>EditMode
            if (this.editContext && this.editNode) this.editContext.removeNode(this.editNode);
            //<EditMode
            this.destroy();
        } else {
            this._checkPortletHeights();
            this._updatePortletResizeFrom();
        }

        if (this.portalLayout && this.portalLayout.portletsChanged) this.portalLayout.portletsChanged();

        // No need to invoke Super - membersChanged is undefined by default
    },

    // Apply portlet logic when adding portlets as members. This allows smoother interaction
    // with existing drag and drop code in Layout.js, since that code ultimately calls
    // addMembers and removeMembers
    addMembers : function (portlets, index) {
        // Don't do anything special if we're just swapping placeholders in and out
        if (this.portletMaximizing) return this.Super("addMembers", arguments);

        if (!isc.isAn.Array(portlets)) portlets = [portlets];

        var self = this;
        portlets.map(function (portlet) {
            // Don't deal with portlet placeholders here
            if (portlet._portlet) return;

            // Apply canResizePortlets
            portlet.canDragResize = self.canResizePortlets;
            if (portlet._resizeBar) portlet._resizeBar.canDragResize = self.canResizePortlets;

            // Check if the portlet has a specified height
            var userHeight = portlet._userHeight;
            if (userHeight) {
                // The portlet's height should always be 100%, but we specify null here
                // because otherwise this sets _userHeight to 100% as well, and that
                // will get picked up the *next* time we move the portlet.
                portlet._userHeight = null;
                portlet._percent_height = null;

                // If there is no explicit rowHeight, then use the userHeight
                if (!portlet.rowHeight) portlet.rowHeight = userHeight;
            }

            // Apply the rowHeight if specified
            if (portlet.rowHeight) {
                // We only apply the rowHeight when adding a portlet if the row's height has
                // not already been explicity indicated
                if (!self._userHeight) {
                    self.setHeight(portlet.rowHeight);
                    // This is needed if the row is still being initialized ... otherwise,
                    // the height gets reset later.
                    self._userHeight = isc.NumberUtil.parseIfNumeric(portlet.rowHeight);
                }
            }
        });

        this.Super("addMembers", arguments);

        // Need to do this after Super, since the addMembers may imply a removeMembers
        // which would set portalRow to null
        portlets.map(function (portlet) {
            // Add a reference back, since a maximized portlet cannot get to us
            // via parentElement
            portlet.portalRow = self;
        });

        //>EditMode
        // If we have an editContext and we aren't coming from addPortlets, then we check
        // whether the portlets have an editNode ... if so, we should add it
        if (this.editContext && !this._addingPortlets) {
            for (var i = 0; i < portlets.length; i++) {
                var portlet = portlets[i];
                if (portlet.editNode) {
                    this.editContext.addNode(portlet.editNode, this.editNode, index + i, null, true);
                }
            }
        }
        //<EditMode
    },

    addPortlets : function (portlets, index) {
        //>EditMode
        // We keep track of whether we're calling addMembers from here in order to know whether
        // to check for editNodes. The assumption is that if we're calling from here, we have
        // already dealt with editNodes appropriately.
        //<EditMode
        this._addingPortlets = true;
        this.addMembers(portlets, index);
        delete this._addingPortlets;
    },

    addPortlet : function (portlet, index) {
        this.addPortlets(portlet, index);
    },

    // Catch removeMembers which occurs via drag & drop
    removeMembers : function (portlets) {
        this.Super("removeMembers", arguments);

        if (!this.portletMaximizing) {
            if (!isc.isAn.Array(portlets)) portlets = [portlets];

            var self = this;
            portlets.map(function (portlet) {
                if (portlet.portalRow) portlet.portalRow = null;

                //>EditMode
                // If we have an editContext, then we check whether we got here via
                // removePortlets. If we did, then we assume that the code has done
                // the right thing re: editContext. If not, then we're probably doing
                // a drag & drop from Layout.js, so we should remove the component
                if (self.editContext && portlet.editNode && !self._removingPortlets) {
                    // Note that we skip live removal, since we'll have just done that
                    self.editContext.removeNode(portlet.editNode, true);
                }
                //<EditMode
            });
        }
    },

    removePortlets : function (portlets) {
        if (!isc.isAn.Array(portlets)) portlets = [portlets];

        var self = this;
        portlets.map(function (portlet) {
            var placeholder = portlet._portletPlaceholder;
            if (placeholder) {
                self.removeMembers(placeholder);
                delete placeholder._portlet;
                delete portlet._portletPlaceholder;
                placeholder.destroy();
                portlet.deparent();
                portlet.clear();
                portlet.portalRow = null;
            } else {
                //>EditMode
                // We keep track of whether we are calling removeMembers from here in order
                // to know whether to check for editNodes. The assumption is that if we are
                // calling from here, we have already done the appropriate thing with any
                // editNodes.
                //<EditMode
                self._removingPortlets = true;
                self.removeMembers(portlet);
                delete self._removingPortlets;
            }
        });
    },

    removePortlet : function (portlet) {
        this.removePortlets(portlet);
    },

    getPortlet : function (id) {
        return this.getMember(id);
    },

    getPortlets : function () {
        return this.getMembers().map(function (member) {
            // If the member has a _portlet, then it is really a placeholder and we
            // return the _portlet
            if (member._portlet) {
                return member._portlet;
            } else {
                return member;
            }
        });
    },

    getPortalPosition : function (portlet) {
        var position = this.getPortlets().indexOf(portlet);
        if (position < 0) {
            // Return null if it wasn't found
            return null;
        } else {
            // Otherwise, start building up the PortalPosition object ... the callers
            // will supply what they know.
            return {position: position};
        }
    }
});

isc.PortalRow.addClassMethods({
    // Check for the case where all the sizes are pixels. In that case, we won't be able to fill
    // the totalSize, because there won't be anything to stretch. If so, make the last size a *, so
    // it will get the space. The net effect is a bit of a cross between a Layout and a Stack, since
    // you can resize to more than the totalSize, but not less.
    applyStretchResizePolicy : function (sizes, totalSize, minSize, modifyInPlace, propertyTarget) {
        if (propertyTarget.portalLayout && propertyTarget.portalLayout.preventRowUnderflow) {
            if (sizes && sizes.length > 0) {
                var allNumeric = sizes.map(function (size) {
                    return isc.isA.Number(size);
                }).and();

                if (allNumeric) {
                    var totalNumericSizes = sizes.sum();
                    if (totalNumericSizes < totalSize) {
                        sizes[sizes.length - 1] = "*";
                    }
                }
            }
        }

        return this.Super("applyStretchResizePolicy", arguments);
    }
});

// A vertical layout rendered within a PortalColumn, containing the PortalRows.
// A separate object for the PortalColumnBody avoids having to adjust for whether the
// PortalColumn's columnHeader is showing.
// Note that you can drop just about anything on a PortalColumnBody -- it will be wrapped
// with a Portlet and a PortalRow if necessary.
isc.defineClass("PortalColumnBody", "Layout").addProperties({
    vertical: true,
    layoutMargin: 3,

    resizeBarClass: "PortalResizeBar",
    resizeBarSize: 3,
    defaultResizeBars: "middle",

    canAcceptDrop: true,
    canDrag: false,
    dropLineThickness: 2,
    dropLineProperties: {backgroundColor: "blue"},
    width: "100%",

    // Handles resizes from the edge of the column, where there are no resizeBars.
    canDragResize: true,
    dragTarget: "parent",
    edgeCursorMap: {
        "L": "col-resize",
        "R": "col-resize"
    },

    // When creating resizeBars for rows, intialize canDragResize
    createResizeBar : function () {
        var resizeBar = this.Super("createResizeBar", arguments);
        resizeBar.canDragResize = this.creator.canResizePortlets;
        return resizeBar;
    },

    // To apply the minHeight of rows.
    respectSizeLimits: true,

    membersChanged : function () {
        this.members.map(function (row) {
            row._updatePortletResizeFrom();
        });
    },

    // The sizing policy we want is a bit special. We want to manage percent sizes (or * sizes)
    // like a Layout, but we want to be able to force overflow (though minHeight or simply
    // manually setting heights) like a stack. The easiest way seems to be to manipulate
    // getTotalMemberSpace to ensure that we're big enough.
    getTotalMemberSpace : function () {
        var normalMemberSpace = this.Super("getTotalMemberSpace", arguments);
        var specialMemberSpace = this.members.map(function (row) {
            if (isc.isA.Number(row._userHeight)) {
                return Math.max(row._userHeight, row.minHeight);
            } else {
                // If the _userHeight is a percentage, we reserve enough space for the minHeight.
                // That way, there will always be enough space for the percentage to resolve
                // to the minHeight -- if more space is available, it can resolve to more.
                return row.minHeight;
            }
        }).sum();
        return Math.max(normalMemberSpace, specialMemberSpace);
    },

    // We pass through the drop if it is a PortalColumn, since it doesn't make sense to drop
    // a PortalColumn on another PortalColumn -- the PortalLayout will handle it.
    isPortalColumnDrop : function () {
        var dragTarget = this.ns.EH.dragTarget;
        var type = dragTarget.getDragType();
        if (type == "PortalColumn") return true;

        //>EditMode
        if (dragTarget.isA("Palette")) {
            var data = dragTarget.getDragData(),
                component = (isc.isAn.Array(data) ? data[0] : data);
            if (component.type == "PortalColumn" || component.className == "PortalColumn") return true;
        }
        //<EditMode

        return false;
    },

    // EventHandler calls this before doing the actual drop (but not for calling dropMove or dropOver)
    // If we answer 'false', then the drop won't occur. Thus, we can't just answer for ourselves and
    // let the drop bubble up -- there won't be any bubbling if we answer 'false'.
    // So, we have to delegate and give the right answer (or not be the dropTarget).
    willAcceptDrop : function () {
        delete this.dropTarget;

        // If this is a portalColumn drop, then the portalLayout should be the target
        if (this.isPortalColumnDrop()) {
            this.dropTarget = this.creator.portalLayout;
            return this.dropTarget.willAcceptDrop();
        }

        // By default, portalLayout.willAcceptPortletDrop just calls back to our
        // superclass. But subclasses of portalLayout could do something different.
        return this.creator.portalLayout.willAcceptPortletDrop(
            this.ns.EH.dragTarget,
            this.creator.portalLayout.getPortalColumnNumber(this.creator),
            this.getDropPosition(),
            null
        );
    },

    // We can share drop, dropMove and dropOver with PortalRow, since they need to do the same thing here
    drop     : isc.PortalRow.getInstanceProperty("drop"),
    dropMove : isc.PortalRow.getInstanceProperty("dropMove"),
    dropOver : isc.PortalRow.getInstanceProperty("dropOver"),

    getDropComponent : function (dragTarget, dropPosition) {
        var dropComponent = this.creator.portalLayout.getDropPortlet(
            dragTarget,
            this.creator.portalLayout.getPortalColumnNumber(this.creator),
            dropPosition,
            null
        );

        //>EditMode
        if (this.handleDroppedEditNode) dropComponent = this.handleDroppedEditNode(dropComponent, dropPosition);
        //<EditMode

        if (dropComponent) {
            // Check if the dropComponent is a Portlet (or subclass) ... if not, wrap it in one.
            if (!isc.isA.Portlet(dropComponent)) {
                dropComponent = isc.Portlet.create({
                    autoDraw: false,
                    title: "",
                    items: dropComponent,
                    destroyOnClose: true
                });
            }

            // We need to check whether the dropComponent is already the only portlet
            // in an existing row. If so, we can simplify by just dropping
            // the row -- that is what the user will have meant.
            var currentRow = dropComponent.portalRow;
            if (currentRow && currentRow.parentElement == this && currentRow.getMembers().length == 1) {
                return currentRow;
            } else {
                this.creator.addPortlet(dropComponent, dropPosition);
                return null;
            }
        }
    }
});

isc.PortalColumnBody.addClassMethods({
    // Check for the case where all the sizes are pixels. In that case, we won't be able to fill
    // the totalSize, because there won't be anything to stretch. If so, make the last size a *, so
    // it will get the space. The net effect is a bit of a cross between a Layout and a Stack, since
    // you can resize to more than the totalSize, but not less.
    applyStretchResizePolicy : function (sizes, totalSize, minSize, modifyInPlace, propertyTarget) {
        var portalLayout = propertyTarget.creator.portalLayout;
        if (portalLayout && portalLayout.preventColumnUnderflow) {
            if (sizes && sizes.length > 0) {
                var allNumeric = sizes.map(function (size) {
                    return isc.isA.Number(size);
                }).and();

                if (allNumeric) {
                    var totalNumericSizes = sizes.sum();
                    if (totalNumericSizes < totalSize) {
                        // Give the last non-minmized portal row the extra space ...
                        var lastNonMinimizedRow = propertyTarget.members.findNextIndex(
                            sizes.length - 1, function (row) {
                                return !row.minimized;
                            }, true, 0
                        );

                        if (lastNonMinimizedRow != -1) {
                            sizes[lastNonMinimizedRow] = "*";
                        }
                    }
                }
            }
        }

        return this.Super("applyStretchResizePolicy", arguments);
    }
});


// Vertical layout based container rendered within a PortalLayout.
// PortalColumns are automatically constructed by the PortalLayout class and will not typically
// be directly instantiated.
//
// The only reason to expose this would be to allow customization of appearance - and it makes
// more sense to do that via attributes on the PortalLayout itself.
isc.defineClass("PortalColumn", "Layout").addProperties({
    vertical:true,
    minWidth: 80,

    // This makes VisualBuilder generate code inline for PortalColumns, rather than
    // creating standalone.
    _generated: true,

    // Can drag the PortalColumn, but it does not handle drops ... the PortalColumnBody
    // manages that.
    dragAppearance: "outline",
    canAcceptDrop: false,
    canDrop: true,
    dragType: "PortalColumn",

    // Handles a drag initiated right on the border, which won't be picked up elsewhere
    canDrag: false,
    canDragResize: true,
    edgeCursorMap: {
        "L": "col-resize",
        "R": "col-resize"
    },

    // The columnHeader is handled as an AutoChild
    showColumnHeader: true,
    columnHeaderConstructor: "PortalColumnHeader",
    columnHeaderDefaults: {
        title: "Column"
    },

    // Make showColumnHeader updatable
    setShowColumnHeader : function (show) {
        if (show) {
            if (this.showColumnHeader) return;
            this.showColumnHeader = show;
            this.addAutoChild("columnHeader", {autoParent: "none"});
            this.addMember(this.columnHeader, 0);
        } else {
            if (!this.showColumnHeader) return;
            this.showColumnHeader = show;
            this.removeMember(this.columnHeader);
        }
    },

    // The rowLayout is where the actual rows go ... this avoids having to
    // continually adjust the code for whether the columnHeader is
    // showing or not.
    //
    // These Autochild settings are referenced in PortalLayout as well, since that is where they
    // are exposed as an API. Changing rowLayoutDefaults there will also change it
    // here, but that is fine, since people should be using changeDefaults() anyway. The
    // settings are then copied dynamically from PortalLayout when it creates columns.
    rowLayoutDefaults: {
        _constructor: "PortalColumnBody"
    },

    // Will accept a portalRows attribute, containing portalRows to insert,
    // or properties to be used to construct them.
    initWidget : function () {
        this.Super("initWidget", arguments);
        this.addAutoChild("columnHeader");
        this.addAutoChild("rowLayout");

        // Only apply portletDropTypes if it was supplied. Otherwise, we would clobber the
        // autoChild settings for dropTypes (if any) -- we only do that if portletDropTypes
        // was specified.
        if (this.portletDropTypes) this.rowLayout.dropTypes = this.portletDropTypes;

        if (this.portalRows) this.addPortalRows(this.portalRows);
        this.portalRows = null;
    },

    // This computes the width that we want, given the _userWidth of all of our Portlets.
    // PortalLayout.getTotalMemberSpace and PortalLayout.applyStretchResizePolicy use
    // this as part of the sizing process.
    _getDesiredWidth : function () {
        var rows = this.getPortalRows();
        if (rows.length == 0) {
            return this.minWidth;
        } else {
            var desiredWidth = rows.map(function (row) {
                return row._getDesiredMemberSpace() + row._getWidthOverhead();
            }).max() + this._getWidthOverhead();

            return Math.max(desiredWidth, this.minWidth);
        }
    },

    // Returns the width overhead that a PortalRow doesn't know about -- that is, the width
    // added at the PortalColumnBody and PortalColumn levels.
    _getWidthOverhead : function () {
        var overhead = this._getBreadthMargin() +
                       this.getHMarginBorder() +
                       this.rowLayout._getBreadthMargin() +
                       this.rowLayout.getHMarginBorder();

        if (this.rowLayout.vscrollOn) overhead += this.rowLayout.getScrollbarSize();

        return overhead;
    },

    prepareForDragging : function () {
        var EH = this.ns.EH;

        if (EH.dragTarget) {
            // If someone has already claimed a dragTarget, check whether it is us ... if
            // so, we'll continue on to the adjustment below. Otherwise, not.
            if (EH.dragTarget != this) return;
        } else {
            // If no one has claimed a dragTarget, then do the standard checks
            this.Super("prepareForDragging", arguments);
        }

        // Then, make some adjustments if we are the target and we're dragging the left edge
        if (EH.dragTarget == this && EH.dragOperation == EH.DRAG_RESIZE && EH.resizeEdge == "L") {
            var index = this.portalLayout.getMemberNumber(this);
            if (index > 0) {
                // If we're resizing from the left edge, and there is a previous column
                // then switch to it, and switch edges -- this makes the layout reflow
                // in a way that makes more sense to the user.
                EH.dragTarget = this.portalLayout.getMember(index - 1);
                EH.resizeEdge = "R";
            } else {
                // If we're resizing from the left edge, and there is no previous column,
                // then cancel.
                EH.dragTarget = null;
            }
        }
    },

    dragResized : function (deltaX, deltaY) {
        if (deltaX < 0) {
            // If we're drag-resized to be smaller, then adjust any rows which
            // would otherwise force us to be bigger.
            var maxRowWidth = this.getWidth() - this._getWidthOverhead();
            this.getPortalRows().map(function (row) {
                row._applyMaxWidth(maxRowWidth);
            });
        }

        this.Super("dragResized", arguments);
    },

    resized : function (deltaX, deltaY, reason) {
        this.Super("resized", arguments);

        // Only catch width changes, since height is managed
        if (!deltaX) return;

        if (this.portalLayout) this.portalLayout._portletsResized();
    },

    addNewColumn : function () {
        this.portalLayout.addColumnAfter(this);
    },

    removeSelf : function () {
        this.portalLayout.removeColumn(this.portalLayout.getMemberNumber(this));
    },

    // See comment on rowLayoutDefaults re: the reference in PortalLayout
    rowConstructor: "PortalRow",

    // Creates rows via AutoChild logic, or modifies existing rows
    // to prepare them to be added to the column. Note that rowConstructor,
    // rowDefaults and rowProperties will have been copied from the portalLayout.
    makePortalRow : function (props) {
        if (props == null) props = {};

        var dynamicProperties = {
            portalLayout: this.portalLayout,
            portalColumn: this,
            canResizePortlets: this.canResizePortlets
        };

        // Only supply portletDropTypes if it was specified. This avoids clobbering
        // the autoChild settings for dropTypes -- we only clobber them if portletDropTypes
        // was specified.
        if (this.portletDropTypes) dynamicProperties.dropTypes = this.portletDropTypes;

        var portalRow;
        if (isc.isA.PortalRow(props)) {
            // If we're given an already created PortalRow, then use setProperties
            // to add the dynamicProperties.
            props.setProperties(dynamicProperties);
            portalRow = props;
        } else {
            // Otherwise, construct it as an autoChild
            isc.addProperties(props, dynamicProperties);
            portalRow = this.createAutoChild("row", props);
        }
        return portalRow;
    },

    setCanResizePortlets : function (canResize) {
        this.canResizePortlets = canResize;
        this.getPortalRows().map(function (row) {
            row.setCanResizePortlets(canResize);
        });
    },

    setPortletDropTypes : function (portletDropTypes) {
        this.portletDropTypes = portletDropTypes;
        this.rowLayout.dropTypes = portletDropTypes;
        this.getPortalRows().map(function (row) {
            row.dropTypes = portletDropTypes;
        });
    },

    addPortalRows : function (rows, position) {
        if (!isc.isAn.Array(rows)) rows = [rows];
        var self = this;
        rows = rows.map(function (row) {
            return self.makePortalRow(row);
        });
        this.rowLayout.addMembers(rows, position);
    },

    addPortalRow : function (row, position) {
        this.addPortalRows(row, position);
    },

    removePortalRows : function (rows) {
        this.rowLayout.removeMembers(rows);
    },

    removePortalRow : function (row) {
        this.removePortalRows(row);
    },

    getPortalRows : function () {
        return this.rowLayout.getMembers();
    },

    getPortalRowNumber : function (id) {
        return this.rowLayout.getMemberNumber(id);
    },

    getPortalRow : function (rowID) {
        return this.rowLayout.getMember(rowID);
    },

    // Returns flat list of portlets
    getPortlets : function () {
        var portlets = [];
        this.getPortalRows().map(function (row) {
            portlets.addList(row.getPortlets());
        });
        return portlets;
    },

    // Returns portlets in array of arrays, corresponding to rows
    getPortletArray : function () {
        return this.getPortalRows().map(function (row) {
            return row.getPortlets();
        });
    },

    getPortalPosition : function (portlet) {
        var rows = this.getPortalRows();

        for (var rowNum = 0; rowNum < rows.length; rowNum++) {
            // Ask each row to get the portal position. If it's found,
            // add the rowNum ... otherwise, return null.
            var position = rows[rowNum].getPortalPosition(portlet);
            if (position) {
                position.rowNum = rowNum;
                return position;
            }
        }

        return null;
    },

    getPortlet : function (id) {
        var rows = this.getPortalRows();
        for (var x = 0; x < rows.length; x++) {
            var portlet = rows[x].getPortlet(id);
            if (portlet) return portlet;
        }
        return null;
    },

    // Adds portlets, auto-wrapping them in rows
    addPortlets : function (portlets, position) {
        if (!isc.isAn.Array(portlets)) portlets = [portlets];

        var self = this;
        var rows = portlets.map(function(portlet) {
            return self.makePortalRow({
                portlets: portlet
            });
        });

        this.addPortalRows(rows, position);
    },

    addPortlet : function (portlet, position) {
        this.addPortlets(portlet, position);
    },

    addPortletToExistingRow : function (portlet, rowNum, rowOffset) {
        var rows = this.rowLayout.getMembers();

        if (rows == null || rows.length <= rowNum) {
            if (this.editContext && this.editNode && portlet.editNode) {
                this.addNode(portlet.editNode, this.editNode, rows.length);
            } else {
                this.addPortlet(portlet, rows.length);
            }
        } else {
            var portalRow = this.rowLayout.getMember(rowNum);
            if (portalRow.editContext && portalRow.editNode && portlet.editNode) {
                portalRow.editContext.addNode(portlet.editNode, portalRow.editNode, rowOffset);
            } else {
                portalRow.addPortlets(portlet, rowOffset);
            }
        }
    }
});


//>    @class    PortalLayout
// A PortalLayout is a special subclass of Layout designed to display +link{Portlet} windows.
// A PortalLayout displays Portlets in columns and supports drag-drop interaction for moving
// Portlets around within the PortalLayout. Portlets may be drag-reordered within columns, dragged
// into other columns, or dragged next to other Portlets to sit next to them horizontally
// within a column.
//
// @visibility external
// @treeLocation Client Reference/Layout
//<
isc.defineClass("PortalLayout", "Layout").addProperties({
    vertical:false,

    //> @attr portalLayout.overflow (Overflow : "auto" : IRW)
    //
    // Controls how the PortalLayout reacts when column widths or +link{Portlet} widths
    // overflow the width of the PortalLayout. By default, the PortalLayout scrolls
    // when necessary. You can also use overflow: visible or overflow: hidden, with the
    // usual results -- see +link{canResizePortlets} for a further explanation of column widths.
    // <p>
    // Note that overflowing height is also affected by +link{columnOverflow,columnOverflow}.
    // By default, each column will scroll individually -- you can change
    // columnOverflow to "auto" to scroll the whole PortalLayout instead.
    //
    // @see canResizePortlets
    // @see columnOverflow
    // @see Canvas.overflow
    // @example portalLayoutColumnHeight
    // @group sizing
    // @visibility external
    //<
    overflow: isc.Canvas.AUTO,

    // Vertically, it seems to make most sense to scroll each column individually (when
    // required). Imagine a left-hand column with a tall Portlet and several others
    // (forcing scrolling), and a right-hand column with a single Portlet. If the scrolling
    // were at the PortalLayout level, then the right-hand column would be enlarged and the
    // single Portlet would have to scroll -- which means that it would not be entirely visible.
    // That seems undesirable, so (by default) we scroll each column vertically, rather than the whole
    // PortalLayout. However, this is configurable here.

    //> @attr portalLayout.columnOverflow (Overflow : "auto" : IRWA)
    //
    // Controls the +link{Canvas.overflow,overflow} setting for each column. If set to "auto" (the
    // default) then each column will scroll individually (if its +link{Portlet,Portlets} overflow
    // the column height). You can also use "hidden" to clip overflowing heights, or "visible" to
    // show the overflow. The effect of "visible" will depend on the setting for +link{PortalLayout.overflow}
    // -- by default, the PortalLayout as a whole will scroll when necessary.
    //
    // @see overflow
    // @see Canvas.overflow
    // @example portalLayoutColumnHeight
    // @group sizing
    // @visibility external
    //<
    columnOverflow: isc.Canvas.AUTO,

    //> @method portalLayout.setColumnOverflow()
    // Sets +link{columnOverflow} and updates existing columns to reflect the new setting.
    // @param overflow (Overflow) Overflow setting for columns
    // @see columnOverflow
    // @example portalLayoutColumnHeight
    // @visibility external
    //<
    setColumnOverflow : function (overflow) {
        this.columnOverflow = overflow;
        this.rowLayoutDefaults.overflow = overflow;
        this.getPortalColumns().map(function (column) {
            column.rowLayout.setOverflow(overflow);
        });
    },

    // Horizontally, we could scroll each row individually, each column, or the PortalLayout
    // as a whole. Permitting all three is undesirable, as in the worst-case scenario one
    // would end up with three horizontal scroll-bars adjacent to each other (one for the row,
    // one for the column and one for the PortalLayout as a whole -- I've seen it, and it's
    // not pretty).
    //
    // Now, we're ultimately going to have to allow for the PortalLayout as a whole to scroll
    // horizontally, because the user can keep adding columns until the PortalLayout overflows.
    // So, it seems simplest to arrange things so that it is only the PortalLayout as a whole
    // that ever scrolls horizontally -- that way, we only ever have the one set of scrollbars.
    //
    // To accomplish this, by default, the portalLayout.overflow property is "auto" by default,
    // so we'll scroll the whole layout if necessary.
    //
    // To ensure that we don't scroll the rows horizontally, we turn on portalLayout.canStretchColumnWidths
    // by default. This stretchs a column to accomodate the _userWidth specified for its
    // Portlets. However, this does not affect the _userWidth of the column itself, so
    // the column "remembers" its intrinsic width, and will snap back to it when it no
    // longer needs to be stretched (e.g. when you drag a wide Portlet to a different column).
    //
    // If one column needs to stretch, and portalLayout.canShrinkColumnWidths is set, then
    // portalLayout.applyStretchResizePolicy will check
    // to see if another PortalColumn can shrink -- that is, whether its intrinsic width
    // is wider than the width of its Portlets requires. If so, shrinking the other column
    // will avoid forcing the PortalLayout as a whole to scroll, which is desirable.
    //
    // If canStretchColumnWidths is off, then the overflow for the rows is set to "auto", since
    // individual rows may need to scroll if the column cannot stretch.

    //> @attr portalLayout.canStretchColumnWidths (Boolean : true : IRWA)
    //
    // Controls whether the PortalLayout will stretch column widths, if needed to accommodate the
    // width of +link{Portlet,Portlets}. If set, columns will overflow their widths in order to accomodate
    // the widths of their Portlets.
    // <p>
    // With the default setting of +link{overflow}: auto, the PortalLayout as a whole will scroll
    // horizontally if needed. Depending on the setting of +link{canShrinkColumnWidths,canShrinkColumnWidths},
    // other columns may shrink to avoid overflow on the PortalLayout as a whole.
    // <p>
    // If <code>canStretchColumnWidths</code> is turned off, then individual rows will scroll
    // horizontally in order to accommodate Portlets that are wider than their column width allows.
    //
    // @see canShrinkColumnWidths
    // @see canResizePortlets
    // @see overflow
    // @group sizing
    // @visibility external
    //<
    canStretchColumnWidths: true,

    //> @method portalLayout.setCanStretchColumnWidths()
    // Sets +link{canStretchColumnWidths} and reflows to reflect the new setting.
    // @param canStretch (boolean) Whether columns can stretch to accommodate +link{Portlet} widths.
    // @see canStretchColumnWidths
    // @see canShrinkColumnWidths
    // @group sizing
    // @visibility external
    //<
    setCanStretchColumnWidths : function (canStretch) {
        this.canStretchColumnWidths = canStretch;
        this.reflow("canStretchColumnWidths changed");
    },

    //> @attr portalLayout.canShrinkColumnWidths (Boolean : true : IRWA)
    //
    // Controls whether the PortalLayout will shrink column widths to avoid overflowing the PortalLayout
    // horizontally. If the PortalLayout would otherwise overflow its width, it will check each column
    // to see whether it is wider than necessary to accommodate its +link{Portlet,Portlets}. If so,
    // the column may shrink to avoid having to scroll the PortalLayout.
    //
    // @see canStretchColumnWidths
    // @group sizing
    // @visibility external
    //<
    canShrinkColumnWidths: true,

    //> @method portalLayout.setCanShrinkColumnWidths()
    // Sets +link{canShrinkColumnWidths} and reflows to reflect the new setting.
    // @param canShrink (boolean) Whether columns can shrink to avoid overflowing the PortalLayout's width.
    // @see canShrinkColumnWidths
    // @see canStretchColumnWidths
    // @group sizing
    // @visibility external
    //<
    setCanShrinkColumnWidths : function (canShrink) {
        this.canShrinkColumnWidths = canShrink;
        this.reflow("canShrinkColumnWidths changed");
    },

    //> @attr portalLayout.stretchColumnWidthsProportionally (Boolean : false : IRWA)
    //
    // When +link{canStretchColumnWidths,stretching column widths}, should we stretch all column widths proportionally,
    // or just stretch the columns that need extra width?
    // <p>
    // Note that this implies turning off +link{canShrinkColumnWidths,canShrinkColumnWidths}.
    //
    // @see canStretchColumnWidths
    // @see canShrinkColumnWidths
    // @group sizing
    // @visibility external
    //<

    //> @method portalLayout.setStretchColumnWidthsProportionally()
    // Sets +link{stretchColumnWidthsProportionally} and reflows to reflect the new setting.
    // @param stretchProportionally (boolean) Whether to stretch column widths proportionally
    // @see stretchColumnWidthsProportionally
    // @group sizing
    // @visibility external
    //<
    setStretchColumnWidthsProportionally : function (stretchProportionally) {
        this.stretchColumnWidthsProportionally = stretchProportionally;
        this.reflow("stretchColumnWidthsProportionally changed");
    },

    //> @attr portalLayout.preventUnderflow (Boolean : true : IRW)
    //
    // Controls whether the last column will be stretched to fill the PortalLayout's width,
    // or left at its specified width.
    //
    // @group sizing
    // @example portalColumnWidth
    // @visibility external
    //<
    preventUnderflow: true,

    //> @method portalLayout.setPreventUnderflow()
    // Sets +link{preventUnderflow,preventUnderflow} and reflows the layout to implement it.
    // @param preventUnderflow (boolean) Whether to stretch the last column to fill the PortalLayout's width.
    // @group sizing
    // @example portalColumnWidth
    // @visibility external
    //<
    setPreventUnderflow : function (preventUnderflow) {
        if (this.preventUnderflow == preventUnderflow) return;
        this.preventUnderflow = preventUnderflow;
        this.reflow("preventUndeflow changed");
    },

    //> @attr portalLayout.preventColumnUnderflow (Boolean : true : IRW)
    //
    // Controls whether the last +link{Portlet} in a column will be stretched to fill the column's height,
    // or left at its specified height.
    //
    // @group sizing
    // @example portletHeight
    // @visibility external
    //<
    preventColumnUnderflow: true,

    //> @method portalLayout.setColumnPreventUnderflow()
    // Sets +link{preventColumnUnderflow,preventColumnUnderflow} and reflows the layout to implement it.
    // @param preventColumnUnderflow (boolean) Whether to stretch the last +link{Portlet} in a column to
    // fill the column's height.
    // @group sizing
    // @example portletHeight
    // @visibility external
    //<
    setPreventColumnUnderflow : function (preventColumnUnderflow) {
        if (this.preventColumnUnderflow == preventColumnUnderflow) return;
        this.preventColumnUnderflow = preventColumnUnderflow;
        this.getPortalColumns().map(function (column) {
            column.rowLayout.reflow("preventColumnUnderflow changed");
        });
    },

    //> @attr portalLayout.preventRowUnderflow (Boolean : true : IRW)
    //
    // Controls whether the last +link{Portlet} in a row will be stretched to fill the row's width,
    // or left at its specified width.
    //
    // @group sizing
    // @example portletWidth
    // @visibility external
    //<
    preventRowUnderflow: true,

    //> @method portalLayout.setPreventRowUnderflow()
    // Sets +link{preventRowUnderflow,preventRowUnderflow} and reflows the layout to implement it.
    // @param preventRowUnderflow (boolean) Whether to stretch the last +link{Portlet} in a row to
    // to fill the row's width.
    // @group sizing
    // @example portletWidth
    // @visibility external
    //<
    setPreventRowUnderflow : function (preventRowUnderflow) {
        if (this.preventRowUnderflow == preventRowUnderflow) return;
        this.preventRowUnderflow = preventRowUnderflow;
        this.getPortalColumns().map(function (column) {
            column.getPortalRows().map(function (row) {
                row.reflow("preventRowUnderflow changed");
            });
        });
    },

    //> @attr portalLayout.portlets (Array of Portlet : null : I)
    // A convenience attribute which you can use to populate a PortalLayout with +link{Portlet,Portlets}
    // on initialization. After initialization, use +link{addPortlet(),addPortlet()} or drag-and-drop to add
    // Portlets, and +link{getPortlets(),getPortlets()} or +link{getPortletArray(),getPortletArray()}
    // to get Portlets.
    // <p>
    // To create one column, you can provide an array of Portlets.
    // <p>
    // To create multiple columns, provide an array of arrays (where the first level represents columns,
    // and the second represents Portlets).
    // <p>
    // To put multiple portlets in the same row, provide a third level to the array.
    // <p>
    // Note that +link{numColumns,numColumns} is ignored if you provide the portlets attribute, since
    // the array will indicate how many columns to create. You can provide an empty second-level
    // array to create a blank column, if needed.
    // @see getPortlets()
    // @see getPortletArray()
    // @see addPortlet()
    // @see numColumns
    // @example repositionPortlets
    // @visibility external
    //<

    //> @attr portalLayout.numColumns (int : 2 : IR)
    // Initial number of columns to show in this PortalLayout. Note that after initialization
    // columns should be added / removed via +link{addColumn()} and +link{removeColumn}.
    // numColumns is ignored if you initialize the +link{portlets} attribute, since the portlets
    // attribute will imply how many columns to create.
    // @see portlets
    // @getter getNumColumns
    // @visibility external
    //<
    //

    numColumns:2,

    //> @method portalLayout.getNumColumns()
    // Returns the current number of columns displayed in this PortalLayout.
    // @return numColumns (Integer)
    // @visibility external
    //<
    // Overridden to return this.getMembers.length. Will have been set up at initialization time.
    getNumColumns : function () {
        return this.getMembers().length;
    },

    //> @attr portalLayout.showColumnMenus (Boolean : true : IRW)
    // Should a menu be shown within each column with options to add / remove columns?
    // @example addRemovePortalColumn
    // @visibility external
    //<
    showColumnMenus:true,

    //> @method portalLayout.setShowColumnMenus()
    // Sets +link{showColumnMenus} and updates existing columns to reflect the new setting.
    // @param showMenus (boolean) Whether to show column menus
    // @visibility external
    //<
    setShowColumnMenus : function (show) {
        if (this.showColumnMenus == show) return;
        this.showColumnMenus = show;
        this.getPortalColumns().map(function (column) {
            column.setShowColumnHeader(show);
        });
    },

    //> @attr portalLayout.columnBorder (string : "1px solid gray" : IRW)
    // Border to show around columns in this PortalLayout
    // @visibility external
    //<
    columnBorder:"1px solid gray",


    //> @method portalLayout.setColumnBorder()
    // Sets the columnBorder for to the specified value and updates any drawn columns to reflect
    // this.
    // @param columnBorder (string) New border to show around columns
    // @visibility external
    //<
    setColumnBorder : function (columnBorder) {
        this.columnBorder = columnBorder;
        var members = this.members || [];
        for (var i = 0; i < members.length; i++) {
            members[i].setBorder(columnBorder);
        }
    },

    //> @attr portalLayout.canResizeColumns (Boolean : true : IRW)
    // Are columns in this PortalLayout drag-resizeable?
    // <p>
    // Note that the <u>displayed</u> width of a column will automatically shrink and stretch
    // to accomodate the width of +link{Portlet,Portlets} -- see +link{canStretchColumnWidths,canStretchColumnWidths}
    // and +link{canShrinkColumnWidths,canShrinkColumnWidths} for an explanation.
    // This setting affects the <u>intrinsic</u> width of a column --
    // that is, the width it will try to return to when not necessary to stretch or shrink
    // to accomodate Portlet widths.
    // @see canStretchColumnWidths
    // @see canShrinkColumnWidths
    // @setter setCanResizeColumns()
    // @group sizing
    // @example portalColumnWidth
    // @visibility external
    //<
    canResizeColumns: true,

    //> @method portalLayout.setCanResizeColumns()
    // Set whether columns in this portalLayout are drag-resizable, and update any
    // drawn columns to reflect this.
    // @param canResize (Boolean) Whether columns are drag-resizable
    // @see canResizeColumns
    // @group sizing
    // @visibility external
    //<
    setCanResizeColumns : function (resizeColumns) {
        this.canResizeColumns = resizeColumns;

        // Also update the resizeFrom of columns, and portlets, since that
        // depends in part on canResizeColumns
        this._updateResizeFrom();
    },

    //>!BackCompat 2011.07.11 renamed canResizeRows to canResizePortlets
    //> @attr portalLayout.canResizeRows (Boolean : false : IRW)
    // Should vertical drag-resize of portlets within columns be allowed?
    // @deprecated Use +link{canResizePortlets,canResizePortlets} instead.
    // @visibility external
    //<

    //> @method portalLayout.setCanResizeRows()
    // Set whether vertical drag-resize of portlets within columns is allowed, and
    // update any drawn columns to reflect this.
    // @param canResize (Boolean) Whether drag-resize of portlets within columns is allowed
    // @deprecated Use +link{setCanResizePortlets(),setCanResizePortlets()} instead.
    // @visibility external
    //<
    setCanResizeRows : function (resizeRows) {
        this.setCanResizePortlets(resizeRows);
    },
    //<!BackCompat

    //> @attr portalLayout.canResizePortlets (Boolean : false : IRW)
    // Should the height and width of +link{Portlet,Portlets} be drag-resizable?
    // <p>
    // Note that changing the <b>height</b> of a Portlet will change the height of all
    // the Portlets in the same row to match.
    // <p>
    // If the height of Portlets causes a column to overflow, that column will scroll vertically
    // (independent of other columns), depending on the +link{columnOverflow,columnOverflow} setting.
    // <p>
    // Changing the <b>width</b> of a Portlet will potentially cause columns to stretch
    // and shrink their <u>displayed</u> widths in order to accomodate the Portlets,
    // depending on the value of +link{portalLayout.canStretchColumnWidths,canStretchColumnWidths} and
    // +link{portalLayout.canShrinkColumnWidths,canShrinkColumnWidths}.
    // <p>
    // However, the <u>instrinsic</u> width of the columns will remain the same,
    // so that the columns will resume their former widths when no longer necessary
    // to stretch or shrink to accomodate the widths of Portlets.
    // To allow drag-resizing of the intrinsic width of columns, see +link{canResizeColumns,canResizeColumns}.
    // <p>
    // The net effect is that (by default) PortalLayouts behave like layouts when Portlet sizes do
    // not cause overflow, but behave more like stacks when overflow occurs.
    //
    // @setter setCanResizePortlets
    // @see canResizeColumns
    // @see canStretchColumnWidths
    // @see canShrinkColumnWidths
    // @see columnOverflow
    // @group sizing
    // @example resizingPortlets
    // @visibility external
    //<

    //> @method portalLayout.setCanResizePortlets()
    // Set whether the height and width of +link{Portlet,Portlets} should be drag-resizable, and
    // update any drawn Portlets to reflect this.
    // @param canResize (Boolean) Whether drag-resizing the height and width of portlets is allowed
    // @see canResizePortlets
    // @group sizing
    // @visibility external
    //<
    setCanResizePortlets : function (resizePortlets) {
        this.canResizePortlets = resizePortlets;
        this.getPortalColumns().map(function (column) {
            column.setCanResizePortlets(resizePortlets);
        });
    },

    //> @attr portalLayout.portletDropTypes (Array of String : null : IRW)
    // <p>The +link{Canvas.dropTypes,dropTypes} to be applied when dropping +link{Portlet,Portlets}
    // on this <code>PortalLayout</code>, or when dropping other components to be auto-wrapped in
    // a +link{Portlet}. If you set this, then you will need to set an equivalent +link{Canvas.dragType} on
    // anything to be dragged into this <code>PortalLayout</code> (including +link{Portlet,Portlets}).</p>
    //
    // <p>As a convenience, +link{Portlet.dragType} defaults to <code>"Portlet"</code>. Thus, if you want
    // to allow +link{Portlet,Portlets} to be dropped on this <code>PortalLayout</code>, but disable
    // auto-wrapping of other components, you can set <code>portletDropTypes</code> to
    // <code>["Portlet"]</code>.</p>
    //
    // <p>If you want to allow some +link{Portlet,Portlets} to be dropped on this <code>PortalLayout</code> but
    // not others, then set a custom +link{portlet.dragType,dragType} for the +link{Portlet,Portlets}, and
    // set <code>portletDropTypes</code> to match.</p>
    //
    // <p>If you want to have different <code>dropTypes</code> for +link{row,rows} and
    // +link{rowLayout,rowLayouts}, you can specify <code>dropType</code> on the +link{row,row}
    // or +link{rowLayout,rowLayout} autochildren instead.</p>
    //
    // <p>For more control over what can be dropped, you can also implement
    // +link{willAcceptPortletDrop(),willAcceptPortletDrop()}.
    //
    // @setter setPortletDropTypes
    // @see Canvas.dropTypes
    // @group dragdrop
    // @visibility external
    //<

    //> @method portalLayout.setPortletDropTypes()
    // <p>Sets the +link{portletDropTypes,portletDropTypes} to be applied when dropping +link{Portlet,Portlets}
    // on this <code>PortalLayout</code>, or when dropping other components to be auto-wrapped in
    // a +link{Portlet}.</p>
    //
    // @param portletDropTypes (Array of String) dropTypes to apply when dropping +link{Portlet,Portlets}
    // @see portletDropTypes
    // @group dragdrop
    // @visibility external
    //<
    setPortletDropTypes : function (portletDropTypes) {
        this.portletDropTypes = portletDropTypes;
        this.getPortalColumns().map(function (column) {
            column.setPortletDropTypes(portletDropTypes);
        });
    },

    //> @method portalLayout.willAcceptPortletDrop() [A]
    // <p>Returns true if the dragged +link{Portlet}, or other component, can be dropped onto
    // this <code>PortalLayout</code> (other components would be auto-wrapped in a <code>Portlet</code>).</p>
    //
    // <p>The default implementation acts like +link{Canvas.willAcceptDrop}, except applying
    // +link{portalLayout.portletDropTypes,portletDropTypes} rather than +link{portalLayout.dropTypes,dropTypes}.
    // You can subclass to apply other (or additional) criteria</p>
    //
    // @return    (boolean)    true if the +link{Portlet} or other component being dragged can be dropped on this PortalLayout,
    //                      false otherwise
    //
    // @param dragTarget (Canvas) The +link{Portlet}, or other component, being dragged
    // @param colNum (int) indicates which column the portlet would be dropped on.
    // @param rowNum (int) indicates the row number being dropped on.
    // @param [dropPosition] (int) Drop position within an existing row. If the dropPosition
    //  is null, then that means that a new row will be created.
    // @see Canvas.dragType
    // @see PortalLayout.portletDropTypes
    // @see portletsChanged()
    // @group dragdrop
    // @visibility external
    //<
    willAcceptPortletDrop : function(dragTarget, colNum, rowNum, dropPosition) {
        // By default, we call back to the Super of the dropTarget. But, we get
        // it by colNum and rowNum, since that is more useful information as an API for
        // possible sub-classes. (And, it is easier to implement the SmartGWT wrapper that way).
        var dropTarget = this.getPortalColumn(colNum).rowLayout;

        // If the dropPosition is specified, then the dropTarget is actually the row
        if (dropPosition != null) dropTarget = dropTarget.getMember(rowNum);

        // Call the super of dropTarget.willAcceptDrop, since it is willAcceptDrop itself
        // which will have called us. Thus, by default, this is all a no-op ... it exists
        // only as an override point.
        return dropTarget.Super("willAcceptDrop", arguments);
    },

    // This allows drag/drop reordering within the portal layout
    canAcceptDrop: true,

    //> @attr portalLayout.dropTypes (Array of String : ["PortalColumn"] : IR)
    // <code>dropTypes</code> is set to <code>["PortalColumn"]</code>
    // in order to allow the dragging of columns within the <code>PortalLayout</code>.
    // To control <code>dropTypes</code> when +link{Portlet,Portlets} or other components are dragged
    // into the <code>PortalLayout</code>, use +link{portalLayout.portletDropTypes,portletDropTypes} instead.
    //
    // @group dragdrop
    // @see portalLayout.portletDropTypes
    // @visibility external
    //<
    dropTypes: ["PortalColumn"],

    // change appearance of drag placeholder and drop indicator
    dropLineThickness:2,
    dropLineProperties:{backgroundColor:"blue"},

    initWidget : function () {
        this.Super("initWidget", arguments);

        this.setCanResizeColumns(this.canResizeColumns);
        this.setColumnOverflow(this.columnOverflow);

        //>!BackCompat 2011.07.11 Renamed canResizeRows to canResizePortlets
        if (this.canResizeRows != null) this.setCanResizePortlets(this.canResizeRows);
        //<!BackCompat

        if (this.portalColumns) {
            this.addPortalColumns(this.portalColumns);

            // Don't hang on to the initialization value
            delete this.portalColumns;
        } else if (this.portlets) {
            var self = this;

            // If we've been given a single Portlet, make it an array
            if (!isc.isAn.Array(this.portlets)) this.portlets = [this.portlets];

            // If the first element in the array is a Portlet (not another array), then
            // we can only have a single column -- so we create the multi-array structure
            if (!isc.isAn.Array(this.portlets[0])) this.portlets = [this.portlets];

            // Now we're guaranteed that the first level is an array representing columns
            this.portlets.map(function (column) {
                var portalColumn = self.makePortalColumn();
                self.addPortalColumn(portalColumn);

                // If the column is a single portlet (not an array), then make it an array
                if (!isc.isAn.Array(column)) column = [column];

                // Now, we're guaranteed that the column is an array of rows
                column.map(function (row) {
                    var portalRow = portalColumn.makePortalRow();
                    portalColumn.addPortalRow(portalRow);

                    // Now, we should have either a Portlet or an array of Portlets, so we can just add them
                    portalRow.addPortlets(row);
                });
            });

            // We don't need to hang on to the initialization value
            delete this.portlets;
        } else {
            if (this.numColumns) {
                for (var x = 0; x < this.numColumns; x++) {
                    this.addColumn();
                }
            }
        }
    },

    //> @method portalLayout.getDropPortlet()
    // This method is called when the user drops components into the rows or columns of this
    // PortalLayout.
    // <P>
    // Overriding this method allows you to modify drop behaviour when creating or reordering
    // portlets via drag & drop. You can return the dragTarget for the standard behavior,
    // or null to cancel the drop.
    // <P>
    // Otherwise, return the component you want to be dropped (as for +link{layout.getDropComponent}).
    // You will generally want to return a +link{Portlet} or subclass. However, you can return
    // any +link{Canvas}, and it will automatically be wrapped in a Portlet if necessary.
    // @param dragTarget (Canvas) drag target
    // @param colNum (int) indicates which column the portlet is being dropped on.
    // @param rowNum (int) indicates the row number being dropped on.
    // @param [dropPosition] (int) Drop position within an existing row. If the dropPosition
    //  is null, then that means that a new row will be created.
    // @return (Canvas) drop-component or custom Portlet to embed in the portalLayout. Returning
    //  null will cancel the drop.
    // @example portletContentsDragging
    // @example portalCrossWindowDrag
    // @see willAcceptPortletDrop()
    // @see portletsChanged()
    // @visibility external
    //<
    // This is called from portalColumnBody.getDropComponent and portalRow.getDropComponent.
    // Note that return type is documented as Canvas but overrides would probably
    // always return a Portlet.
    getDropPortlet : function (dragTarget, colNum, rowNum, dropPosition) {
        return dragTarget;
    },

    //> @attr portalLayout.row (MultiAutoChild Layout : null : A)
    // Automatically generated horizontal +link{Layout} used to create rows of +link{Portlet,Portlets} via
    // +link{Class.createAutoChild,createAutoChild()}. Since this is an +link{AutoChild}, you can use
    // rowDefaults and rowProperties to customize the rows.
    // <p>
    // Rows are created inside +link{rowLayout,rowLayouts}, which in turn are inside +link{column,columns}.
    // @see portalLayout.column
    // @see portalLayout.rowLayout
    // @visibility external
    //<
    // Note that the actual call to createAutoChild is in PortalColumn. We expose the property
    // here instead because we're trying to avoid exposing PortalColumn and PortalRow.
    rowConstructor: isc.PortalColumn.getInstanceProperty("rowConstructor"),

    //> @attr portalLayout.rowLayout (MultiAutoChild Layout : null : A)
    // Automatically generated vertical +link{Layout} used to create columns of +link{Portlet,Portlets} via
    // +link{Class.createAutoChild,createAutoChild()}. Since this is an +link{AutoChild}, you can use
    // rowLayoutDefaults and rowLayoutProperties to customize the layout used to contain the rows.
    // <p>
    // The rowLayout is the actual container for +link{row,rows} of +link{Portlet,Portlets}. See +link{column,column} for
    // the column as a whole, which may include a menu as well (depending on +link{showColumnMenus,showColumnMenus}).
    // If you want to style the columns as a whole,
    // use columnDefaults or columnProperties, but if you want to style the layout that actually contains the
    // rows, use rowLayoutDefaults or rowLayoutProperties.
    // @see portalLayout.rowLayout
    // @see portalLayout.row
    // @visibility external
    //<
    // Note that the actual call to addAutoChild is in PortalColumn. We expose teh property
    // here instead because we're trying to avoid exposing PortalColumn and PortalRow.
    rowLayoutDefaults: isc.PortalColumn.getInstanceProperty("rowLayoutDefaults"),

    //> @attr portalLayout.column (MultiAutoChild Layout : null : A)
    // Automatically generated vertical +link{Layout} used to create columns of +link{Portlet,Portlets} via
    // +link{Class.createAutoChild,createAutoChild()}. Since this is an +link{AutoChild}, you can use
    // columnDefaults and columnProperties to customize the columns.
    // <p>
    // The column includes a menu, if +link{showColumnMenus,showColumnMenus} is true, and a +link{rowLayout,rowLayout} which
    // actually contains the +link{row,rows}. Therefore, if you want to style the columns as a whole,
    // use columnDefaults or columnProperties, but if you want to style the layout that contains the
    // rows, use rowLayoutDefaults or rowLayoutProperties.
    // @see portalLayout.rowLayout
    // @see portalLayout.row
    // @visibility external
    //<
    columnConstructor: "PortalColumn",

    // Make columns using autoChild logic, or apply this PortalLayout's
    // settings to an existing PortalColumn. Note that we copy rowConstructor etc. in order
    // to pass the AutoChild logic down for rows.
    makePortalColumn : function (props) {
        if (props == null) props = {};

        var dynamicProperties = {
            portalLayout: this,
            showColumnHeader: this.showColumnMenus,
            border: this.columnBorder,
            canResizePortlets: this.canResizePortlets,
            rowConstructor: this.rowConstructor,
            rowDefaults: this.rowDefaults,
            rowProperties: this.rowProperties,
            rowLayoutDefaults: this.rowLayoutDefaults,
            rowLayoutProperties: this.rowLayoutProperties
        }

        // Only pass on portletDropTypes if it has been specified -- otherwise, we'll
        // clobber any autoChild settings for dropTypes
        if (this.portletDropTypes) dynamicProperties.portletDropTypes = this.portletDropTypes;

        var portalColumn;
        if (isc.isA.PortalColumn(props)) {
            // If we're given an already created PortalColumn, then use setProperties
            // to make it conform to the PortalLayout settings here.
            props.setProperties(dynamicProperties);
            portalColumn = props;
        } else {
            // Otherwise, construct it as an autoChild
            isc.addProperties(props, dynamicProperties);
            portalColumn = this.createAutoChild("column", props);
        }
        return portalColumn;
    },

    // Apply portalColumn logic when adding PoralColumns as members. This allows smoother interaction
    // with existing drag and drop code in Layout.js, since that code ultimately calls
    // addMembers and removeMembers
    addMembers : function (columns, index) {
        if (!isc.isAn.Array(columns)) columns = [columns];

        var self = this;
        columns = columns.map(function (column) {
            return self.makePortalColumn(column);
        });

        this.Super("addMembers", arguments);

        //>EditMode
        // If we have an editContext and we aren't coming from addPortalColumns, then we check
        // whether the columns have an editNode ... if so, we should add it
        if (this.editContext && !this._addingPortalColumns) {
            for (var i = 0; i < columns.length; i++) {
                var column = columns[i];
                if (column.editNode) {
                    this.editContext.addNode(column.editNode, this.editNode, index + i, null, true);
                }
            }
        }
        //<EditMode
    },

    addPortalColumns : function (columns, index) {
       this._addingPortalColumns = true;
       this.addMembers(columns, index);
       delete this._addingPortalColumns;
    },

    addPortalColumn : function (column, index) {
        this.addPortalColumns(column, index);
    },

    // Catch removeMembers which occurs via drag & drop
    removeMembers : function (portalColumns) {
        this.Super("removeMembers", arguments);

        //>EditMode
        // If we have an editContext, then we check whether we got here via
        // removePortalColumns. If we did, then we assume that the code has done
        // the right thing re: editContext. If not, then we're probably doing
        // a drag & drop from Layout.js, so we should remove the component
        if (this.editContext && !this._removingPortalColumns) {
            if (!isc.isAn.Array(portalColumns)) portalColumns = [portalColumns];
            var self = this;
            portalColumns.map(function (column) {
                if (column.editNode) {
                    // Note that we skip live removal, since we'll have just done that
                    self.editContext.removeNode(column.editNode, true);
                }
            });
        }
        //<EditMode
    },

    removePortalColumns : function (columns) {
        this._removingPortalColumns = true;
        this.removeMembers(columns);
        delete this._removingPortalColumns;
    },

    removePortalColumn : function (column) {
        this.removePortalColumns(column);
    },

    //> @method portalLayout.addColumn()
    // Adds a new portal column to this layout at the specified position
    // @param index (int) target position for the new column
    // @visibility external
    //<
    addColumn : function (index) {
        //>EditMode
        // PortalLayout is a little special with respect to EditMode, since the
        // whole purpose of PortalLayout is to be editable ... thus, it makes
        // more sense to integrate the editing code here, rather than relying
        // on separate code in EditMode.js. For instance, it makes more sense
        // to rely on the standard PortalLayout interface for adding columns,
        // rather than forcing the user to drag a column from a palette.
        //
        // Thus, when adding a Column, we note whether there is an edit context
        // around and, if so, ask it to do it. That will also eventually run
        // through addPortalColumn, given the standard sequence of events.
        //<EditMode
        if (this.editContext) {
            var columnNode = this.editContext.makeEditNode({
                type: this.columnConstructor
            });
            this.editContext.addNode(columnNode, this.editNode, index);
        } else {
            //>EditMode
            // This is a bit hackish to generate nice ID's in cases where we
            // will soon be put into an editContext
            //<EditMode
            var autoId = "";
            var typeCount = 0;
            while (window[(autoId = "PortalColumn" + typeCount++)]) {}

            this.addPortalColumn({ID: autoId}, index);
        }
    },

    //> @method portalLayout.removeColumn()
    // Removes the specified column from this layout.
    // All portlets displayed within this column will be destroyed when the column is removed.
    // @param index (int) column number to remove
    // @visibility external
    //<
    removeColumn : function (index) {
        var column = this.members[index];
        if (column != null) {
            if (this.editContext && column.editNode) {
                this.editContext.removeNode(column.editNode);
            } else {
                column.destroy();
            }
        }
    },

    // addColumnAfter is used by the header menus shown within columns if appropriate
    addColumnAfter : function (portalColumn) {
        var targetIndex = this.getMemberNumber(portalColumn) + 1;
        this.addColumn(targetIndex);
    },

    //>@method portalLayout.getPortlets()
    // Returns a flat array of all the +link{Portlet,Portlets} in this PortalLayout.
    // @return portlets (Array of Portlet)
    // @visibility external
    // @see getPortletArray()
    //<
    getPortlets : function () {
        var portlets = [];
        this.getPortalColumns().map(function (column) {
            portlets.addList(column.getPortlets());
        });
        return portlets;
    },

    //>@method portalLayout.getPortletArray()
    // Returns a multi-level array of the +link{Portlet,Portlets} in this PortalLayout,
    // where the first level corresponds to columns, the second to rows, and the third
    // to Portlets within rows.
    // @return portlets (Array of Array of Array of Portlet)
    // @visibility external
    // @see getPortlets()
    //<
    getPortletArray : function () {
        return this.getPortalColumns().map(function (column) {
            return column.getPortletArray();
        });
    },

    //> @method portalLayout.getPortalPosition()
    // Gets the position of the +link{Portlet} within this PortalLayout. Returns null
    // if the Portlet is not in this PortalLayout.
    // @param portlet (Portlet) the Portlet for which to get the position
    // @return (PortalPosition) the position of the Portlet
    // @visibility external
    //<
    getPortalPosition : function (portlet) {
        var columns = this.getPortalColumns();

        for (var colNum = 0; colNum < columns.length; colNum++) {
            // We ask each column to find the portlet and fill in the rowNum
            // and position ... if it's found, we add the colNum and return.
            var position = columns[colNum].getPortalPosition(portlet);
            if (position) {
                position.colNum = colNum;
                return position;
            }
        }

        // If it wasn't found, then return null
        return null;
    },

    //>@method portalLayout.addPortlet()
    // Adds a +link{Portlet} instance to this portalLayout in the specified position.
    // @param portlet (Portlet) Portlet to add to this layout.
    // @param [colNum] (integer) Column in which the Portlet should be added. If unspecified,
    //  defaults to zero.  If specified, but the specified column does not exist, a column is
    //  automatically added at the specified colNum index.
    // @param [rowNum] (integer) Row within the column for the Portlet.
    // @param [rowOffset] (integer) Offset within the row. If you specify a
    //   rowOffset, then the Portlet will be added to the existing row. If not, then a new row
    //   will be created at rowNum.
    // @visibility external
    //<
    //>EditMode in EditMode users can drag/drop from paletteNodes to add portlets to columns.
    // This will never run through this method so this is not a valid override point to catch every
    // newly added portlet //<EditMode
    addPortlet : function (portlet, colNum, rowNum, rowOffset) {
        if (rowNum == null) rowNum = 0;
        if (colNum == null) colNum = 0;

        var column = this.getMember(colNum);
        if (column == null) {
           this.addColumn(colNum);
           column = this.getMember(colNum);
        }
        if (rowOffset == null) {
            if (column.editContext && column.editNode && portlet.editNode) {
                column.editContext.addNode(portlet.editNode, column.editNode, rowNum);
            } else {
                column.addPortlet(portlet, rowNum);
            }
         } else {
            column.addPortletToExistingRow(portlet, rowNum, rowOffset);
        }
    },

    // The sizing policy we want is a bit special. We want to manage percent sizes (or * sizes)
    // like a Layout, but we want to be able to force overflow (though minWidth or simply
    // manually setting widths) like a stack. The easiest way seems to be to manipulate
    // getTotalMemberSpace to ensure that we're big enough.
    getTotalMemberSpace : function () {
        var normalMemberSpace = this.Super("getTotalMemberSpace", arguments);
        var specialMemberSpace = this.members.map(function (member) {
            if (isc.isA.Number(member._userWidth)) {
                return Math.max(member._userWidth, member.minWidth);
            } else {
                return member.minWidth;
            }
        }).sum();
        return Math.max(normalMemberSpace, specialMemberSpace);
    },

    //>@method portalLayout.setColumnWidth()
    // Sets the width of a column in the PortalLayout.
    // <p>
    // Note that this sets the intrinsic width of the column. Columns may also
    // automatically stretch and shrink to accomodate the width of
    // +link{Portlet,Portlets}.
    // @param colNumber (Integer) Which column's width to set.
    // @param width (Number or String) How wide to make the column
    // @see Canvas.setWidth()
    // @visibility external
    //<
    setColumnWidth : function (columnNumber, width) {
        var column = this.getPortalColumn(columnNumber);
        if (!column) return;
        // This automatically adjusts the editNode if present
        if (column.editContext && column.editNode) {
            column.editContext.setNodeProperties(column.editNode, {
                width: width
            });
        } else {
            column.setWidth(width);
        }
    },

    //>@method portalLayout.getColumnWidth()
    // Gets the width of a column in the PortalLayout.
    // @param colNumber (int) Which column's width to get
    // @return width (int)
    // @see Canvas.getWidth()
    // @visibility external
    //<
    getColumnWidth : function (columnNumber) {
        var column = this.getPortalColumn(columnNumber);
        if (column) {
            return column.getWidth();
        } else {
            return null;
        }
    },

    getPortalColumns : function () {
        return this.getMembers();
    },

    getPortalColumn : function (columnID) {
        return this.getMember(columnID);
    },

    getPortalColumnNumber : function (columnID) {
        return this.getMemberNumber(columnID);
    },

    getColumn : function (colNum) {
        return this.getPortalColumn(colNum);
    },

    //>@method portalLayout.removePortlet()
    // Removes a +link{Portlet} which is currently rendered in this PortalLayout.
    // Portlet will not be destroyed by default - if this is desired the calling code should
    // do this explicitly.
    // @param portlet (Portlet) portlet to remove
    // @visibility external
    //<
    //>EditMode We *DO* auto-destroy portlets on closeclick in editMode if they were dragged in
    // from a paletteNode //<EditMode
    removePortlet : function (portlet) {
        if (this.editContext && portlet.editNode) {
            this.editContext.removeNode(portlet.editNode);
        } else {
            if (portlet.portalRow) portlet.portalRow.removePortlets(portlet);
            // Note: the row will self-destruct if appropriate -- see membersChanged handler
        }
    },

    _updateResizeFrom : function () {
        var firstColumn  = true;
        var self = this;

        this.getPortalColumns().map(function (column) {
            if (self.canResizeColumns) {
                // If canResizeColumns is on, then we always allow resizing
                // from the right edge. Even if this is the right-most column,
                // we allow it to be enlarged, since the user may want to force
                // overflow (i.e. horizontal scrolling).
                //
                // We allow resizing from the left edge if this is not the first
                // column.
                column.rowLayout.resizeFrom = column.resizeFrom = firstColumn ? ["R"] : ["L", "R"];
            } else {
                // If canResizeColumns is off, then don't allow any resizes
                column.rowLayout.resizeFrom = column.resizeFrom = [];
            }

            // And update the resizeFrom for each portlet in the column
            column.getPortalRows().map(function (row) {
                row._updatePortletResizeFrom();
            });

            firstColumn = false;
        });
    },

    membersChanged : function () {
        this._updateResizeFrom();
    },

    //>@method portalLayout.willMaximizePortlet()
    // Method called when a +link{Portlet} in this PortalLayout is about to be
    // maximized. Note that this method is only called when the user explicitly
    // clicks on the portlet's +link{window.showMaximizeButton, maximize button} --
    // it is not called when programmatically maximizing a portlet via
    // +link{window.maximize(),maximize()}.
    // <p>
    // Return false to cancel the action.
    //
    // @param portlet (Portlet) the Portlet which will be maximized
    // @return (boolean) whether the action should proceed
    // @see portletMaximized()
    // @visibility external
    //<

    //>@method portalLayout.portletMaximized()
    // Notification method called after a portlet has been maximized (whether by
    // user action or programmatically).
    //
    // @param portlet (Portlet) the Portlet which was maximized
    // @see willMaximizePortlet()
    // @visibility external
    //<

    //>@method portalLayout.willMinimizePortlet()
    // Method called when a +link{Portlet} in this PortalLayout is about to be
    // minimized. Note that this method is only called when the user explicitly
    // clicks on the portlet's +link{window.showMinimizeButton, minimize button} --
    // it is not called when programmatically minimizing a portlet via
    // +link{window.minimize(),minimize()}.
    // <p>
    // Return false to cancel the action.
    //
    // @param portlet (Portlet) the Portlet which will be minimized
    // @return (boolean) whether the action should proceed
    // @see portletMinimized()
    // @visibility external
    //<

    //>@method portalLayout.portletMinimized()
    // Notification method called after a portlet has been minimized (whether by
    // user action or programmatically).
    //
    // @param portlet (Portlet) the Portlet which was minimized
    // @see willMinimizePortlet()
    // @visibility external
    //<

    //>@method portalLayout.willRestorePortlet()
    // Method called when a +link{Portlet} in this PortalLayout is about to be
    // restored to its normal place (after having been
    // +link{portletMaximized(),maximized}. Note that this method is only
    // called when the user explicitly clicks on the portlet's
    // +link{window.restoreButton, restore button} -- it is not called when
    // programmatically restoring a portlet via +link{window.restore(),restore()}.
    // <p>
    // Return false to cancel the action.
    //
    // @param portlet (Portlet) the Portlet which will be restored
    // @return (boolean) whether the action should proceed
    // @see portletRestored()
    // @visibility external
    //<

    //>@method portalLayout.portletRestored()
    // Notification method called after a portlet has been restored to its normal place
    // (after having been maximized). The method is called whether the restore is
    // via user action or done programmatically.
    //
    // @param portlet (Portlet) the Portlet which was restored
    // @see willRestorePortlet()
    // @visibility external
    //<

    //>@method portalLayout.willClosePortlet()
    // Method called when a +link{Portlet} in this PortalLayout is about to be closed.
    // This method is called before +link{portlet.showCloseConfirmationMessage} is applied.
    // Note that this method is called only when the user explicitly closes a Portlet.
    // It is not called when programmatically removing a Portlet via +link{removePortlet()}.
    // <p>
    // Return false to cancel the action.
    //
    // @param portlet (Portlet) the Portlet which will be closed
    // @return (boolean) whether the action should proceed
    // @see portlet.showCloseConfirmationMessage
    // @see portletsChanged()
    // @visibility external
    //<

    //>@method portalLayout.portletsChanged()
    // Fires at initialization if the PortalLayout has any initial
    // +link{Portlet,portlets}, and then fires whenever portlets are added,
    // removed or reordered.
    //
    // @see layout.membersChanged()
    // @visibility external
    //<


    //>@method portalLayout.portletsResized()
    // Fires when +link{Portlet,portlets} or columns in this PortalLayout are resized.
    // Note that this fires on a short delay -- otherwise, it would fire multiple times
    // for each change, since most portlet size changes will affect multiple portlets.
    // Does not fire when a portlet is +link{portletMaximized(),maximized} or
    // +link{portletRestored(),restored}.
    // @visibility external
    //<

    _portletsResized : function () {
        if (this.portletsResized) {
            this.fireOnPause("portletsResized", "portletsResized", 100);
        }
    }
});

isc.PortalLayout.addClassMethods({
    // Like PortalRow & others, we're using this to make sure that the members fill the space
    // if they all have numeric sizes -- otherwise, there would be nothing to stretch.
    // Then, we check to see whether any columns need to stretch to avoid horizontal overflow. If
    // extra width is required, we check other columns to see if they can shrink (to avoid PortalLayout overflow).
    applyStretchResizePolicy : function (sizes, totalSize, minSize, modifyInPlace, propertyTarget) {
        if (propertyTarget.preventUnderflow) {
            if (sizes && sizes.length > 0) {
                var allNumeric = sizes.map(function (size) {
                    return isc.isA.Number(size);
                }).and();

                if (allNumeric) {
                    var totalNumericSizes = sizes.sum();
                    if (totalNumericSizes < totalSize) {
                        sizes[sizes.length - 1] = "*";
                    }
                }
            }
        }

        var newSizes = this.Super("applyStretchResizePolicy", arguments);
        if (modifyInPlace) newSizes = sizes;

        var desiredWidths = propertyTarget.getPortalColumns().map("_getDesiredWidth");
        var extraWidth = 0;

        if (propertyTarget.canStretchColumnWidths) {
            if (propertyTarget.stretchColumnWidthsProportionally) {
                // If we're maintaining the relative size of column widths, we figure out
                // the maximum percentage stretch and then apply it everywhere
                var maxStretchRatio = 1;

                for (var i = 0; i < newSizes.length; i++) {
                    var stretchRatio = (desiredWidths[i] / newSizes[i]);
                    maxStretchRatio = Math.max(maxStretchRatio, stretchRatio);
                }

                if (maxStretchRatio > 1) {
                    for (var i = 0; i < newSizes.length; i++) {
                        newSizes[i] = newSizes[i] * maxStretchRatio;
                    }
                }
            } else {
                // If we're not maintain the relative size of column widths, then just stretch
                // the columns that need it, and shrink other columns if allowed (and possible)
                for (var i = 0; i < newSizes.length; i++) {
                    if (desiredWidths[i] > newSizes[i]) {
                        extraWidth += desiredWidths[i] - newSizes[i];
                        newSizes[i] = desiredWidths[i];
                    } else if (extraWidth && propertyTarget.canShrinkColumnWidths) {
                        var excess = newSizes[i] - desiredWidths[i];
                        var difference = Math.min(extraWidth, excess);
                        newSizes[i] -= difference;
                        extraWidth -= difference;
                    }
                }

                // Check if there is any extraWidth remaining at the end
                if (extraWidth && propertyTarget.canShrinkColumnWidths) {
                    for (var i = 0; i < newSizes.length; i++) {
                        if (desiredWidths[i] < newSizes[i]) {
                            var excess = newSizes[i] - desiredWidths[i];
                            var difference = Math.min(extraWidth, excess);
                            newSizes[i] -= difference;
                            extraWidth -= difference;
                            if (extraWidth == 0) break;
                        }
                    }
                }
            }
        }

        return newSizes;
    }
});







//>    @class    Dialog
// Dialogs are a specialized version of +link{Window} used for small windows that contain just a text
// message or a text mesage with some standard buttons.
// <P>
// Many typical modal dialogs such as alerts and confirmations are built into the system with convenience
// APIs - see +link{classMethod:isc.say()}, +link{classMethod:isc.warn()} and +link{classMethod:isc.askForValue}.
// <P>
// Dialogs can be modal or non-modal according to +link{Window.isModal,isModal}.
// <P>
// NOTE: If you are building a dialog that will involve more than just buttons and a message, consider
// starting from the +link{Window} class instead, where arbitrary components can be added to the body
// area via +link{Window.addItem()}.
// This is an example of creating a custom dialog:
// <smartclient>
// <pre>
//  isc.Dialog.create({
//      message : "Please choose whether to proceed",
//      icon:"[SKIN]ask.png",
//      buttons : [
//          isc.Button.create({ title:"OK" }),
//          isc.Button.create({ title:"Cancel" })
//      ],
//      buttonClick : function (button, index) {
//          this.hide();
//      }
//  });
// </pre>
// </smartclient>
// <smartgwt>
// <pre>
// final Dialog dialog = new Dialog();
// dialog.setMessage("Please choose whether to proceed");
// dialog.setIcon("[SKIN]ask.png");
// dialog.setButtons(new Button("OK"), new Button("Cancel"));
// dialog.addButtonClickHandler(new ButtonClickHandler() {
//     public void onButtonClick(ButtonClickEvent event) {
//         dialog.hide();
//     }
// });
// dialog.draw();
// </pre>
// </smartgwt>
//
//  @treeLocation Client Reference/Control
//  @visibility external
//<
isc.ClassFactory.defineClass("Dialog", "Window");

// add class properties
isc.Dialog.addClassProperties({
    //>    @classAttr    Dialog._openModalDialogs        (array : [] : IRWA)
    //             list of open modal Dialogs so we can keep track as we open them
    //        @group    modal
    //        @see    Dialog.show()
    //<
    _openModalDialogs : [],

    //> @classAttr  Dialog.OK_BUTTON_TITLE  (HTML : "OK" : IRW)
    // Title for the <code>"OK"</code> button.
    // @see type:DialogButtons
    // @group i18nMessages
    // @visibility external
    //<
    OK_BUTTON_TITLE:"OK",
    //> @classAttr  Dialog.APPLY_BUTTON_TITLE  (HTML : "Apply" : IRW)
    // Title for the <code>"Apply"</code> button.
    // @see type:DialogButtons
    // @group i18nMessages
    // @visibility external
    //<
    APPLY_BUTTON_TITLE:"Apply",
    //> @classAttr  Dialog.YES_BUTTON_TITLE  (HTML : "Yes" : IRW)
    // Title for the <code>"Yes"</code> button.
    // @see type:DialogButtons
    // @group i18nMessages
    // @visibility external
    //<
    YES_BUTTON_TITLE:"Yes",
    //> @classAttr  Dialog.NO_BUTTON_TITLE  (HTML : "No" : IRW)
    // Title for the <code>"No"</code> button.
    // @see type:DialogButtons
    // @group i18nMessages
    // @visibility external
    //<
    NO_BUTTON_TITLE:"No",
    //> @classAttr  Dialog.CANCEL_BUTTON_TITLE  (HTML : "Cancel" : IRW)
    // Title for the <code>"Cancel"</code> button.
    // @see type:DialogButtons
    // @group i18nMessages
    // @visibility external
    //<
    CANCEL_BUTTON_TITLE:"Cancel",
    //> @classAttr  Dialog.DONE_BUTTON_TITLE  (HTML : "Done" : IRW)
    // Title for the <code>"Done"</code> button.
    // @see type:DialogButtons
    // @group i18nMessages
    // @visibility external
    //<
    DONE_BUTTON_TITLE:"Done",

    // Default Titles for the prompt windows themselves

    //> @classAttr  Dialog.CONFIRM_TITLE    (HTML : "Confirm" : IRW)
    // Default title for the dialog displayed in response to the +link{classMethod:isc.confirm()} method.
    // Note that a custom title can be specified as the <code>title</code> attribute of the
    // <code>properties</code> parameter passed to that method.
    // @group i18nMessages
    // @visibility external
    //<
    CONFIRM_TITLE:"Confirm",

    //> @classAttr  Dialog.SAY_TITLE    (HTML : "Note" : IRW)
    // Default title for the dialog displayed in response to the +link{classMethod:isc.say()} method.
    // Note that a custom title can be specified as the <code>title</code> attribute of the
    // <code>properties</code> parameter passed to that method.
    // @group i18nMessages
    // @visibility external
    //<
    SAY_TITLE:"Note",

    //> @classAttr  Dialog.WARN_TITLE    (HTML : "Warning" : IRW)
    // Default title for the dialog displayed in response to the +link{classMethod:isc.warn()} method.
    // Note that a custom title can be specified as the <code>title</code> attribute of the
    // <code>properties</code> parameter passed to that method.
    // @group i18nMessages
    // @visibility external
    //<
    WARN_TITLE:"Warning",

    //> @classAttr  Dialog.ASK_TITLE    (HTML : "Question" : IRW)
    // Default title for the dialog displayed in response to the +link{classMethod:isc.ask()} method.
    // Note that a custom title can be specified as the <code>title</code> attribute of the
    // <code>properties</code> parameter passed to that method.
    // @group i18nMessages
    // @visibility external
    //<
    ASK_TITLE:"Question",

    //> @classAttr  Dialog.ASK_FOR_VALUE_TITLE    (HTML : "Please enter a value" : IRW)
    // Default title for the dialog displayed by +link{classMethod:isc.askForValue()}.
    // A custom title can alternatively be specified as the <code>title</code> attribute of the
    // <code>properties</code> parameter passed to that method.
    // @group i18nMessages
    // @visibility external
    //<
    ASK_FOR_VALUE_TITLE:"Please enter a value",

    //> @classAttr  LoginDialog.LOGIN_TITLE (HTML : "Please log in" : IRW)
    // Default title for the dialog displayed by +link{classMethod:isc.showLoginDialog()}.
    // A custom title can alternatively be specified as the <code>title</code> attribute of the
    // <code>properties</code> parameter passed to that method.
    // @group i18nMessages
    // @visibility external
    //<

    LOGIN_TITLE:"Please log in",

    //> @classAttr  LoginDialog.USERNAME_TITLE (HTML : "Username" : IRW)
    // Default title for the +link{loginDialog.usernameItem,"usernameItem"} field in the
    // dialog displayed by +link{classMethod:isc.showLoginDialog()}.
    // @group i18nMessages
    // @visibility external
    //<

    USERNAME_TITLE:"Username",

    //> @classAttr  LoginDialog.PASSWORD_TITLE (HTML : "Password" : IRW)
    // Default title for the +link{loginDialog.passwordItem,"passwordItem"} field in the
    // dialog displayed by +link{classMethod:isc.showLoginDialog()}.
    // @group i18nMessages
    // @visibility external
    //<

    PASSWORD_TITLE:"Password",

    //> @classAttr  LoginDialog.LOGIN_BUTTON_TITLE (HTML : "Log in" : IRW)
    // Default title for login button in the dialog displayed by
    // +link{classMethod:isc.showLoginDialog()}.
    // @group i18nMessages
    // @visibility external
    //<

    LOGIN_BUTTON_TITLE:"Log in",

    //> @classAttr  LoginDialog.LOGIN_ERROR_MESSAGE (HTML : "Invalid username or password" : IRW)
    // Default error message displayed on failed login in the dialog shown by
    // +link{classMethod:isc.showLoginDialog()}.
    // @group i18nMessages
    // @visibility external
    //<

    LOGIN_ERROR_MESSAGE:"Invalid username or password",

    //>    @type   DialogButtons
    // Default buttons that you can use in your Dialogs.
    // <P>
    // On click these call canonical methods that you can override in your Dialog.
    // <P>
    // Refer to these buttons via the syntax <code>isc.Dialog.OK</code> when passing them into
    // +link{dialog.buttons} or into the <code>properties</code> argument of helper
    // methods such as +link{classMethod:isc.say()}.
    //
    // @value   OK  Button object to fire dialog's "okClick()" method on click.
    //              Title derived from +link{Dialog.OK_BUTTON_TITLE}.
    OK         : {getTitle:function () {return isc.Dialog.OK_BUTTON_TITLE},
                width:75, click: function () { this.topElement.okClick() } },
    // @value   APPLY Button object to fire dialog's "applyClick()" method on click.
    //              Title derived from +link{Dialog.APPLY_BUTTON_TITLE}.
    APPLY     : {getTitle:function () {return isc.Dialog.APPLY_BUTTON_TITLE},
                width:75, click: function () { this.topElement.applyClick() } },
    // @value   YES Button object to fire dialog's "yesClick()" method on click
    //              Title derived from +link{Dialog.YES_BUTTON_TITLE}.
    YES     : {getTitle:function () {return isc.Dialog.YES_BUTTON_TITLE},
                width:75, click: function () { this.topElement.yesClick() } },
    // @value   NO  Button object to fire dialog's "noClick()" method on click.
    //              Title derived from +link{Dialog.NO_BUTTON_TITLE}.
    NO         : {getTitle:function () {return isc.Dialog.NO_BUTTON_TITLE},
                width:75, click: function () { this.topElement.noClick() } },
    // @value   CANCEL  Button object to fire dialog's "cancelClick()" method on click.
    //                  Title derived from +link{Dialog.CANCEL_BUTTON_TITLE}.
    CANCEL     : {getTitle:function () {return isc.Dialog.CANCEL_BUTTON_TITLE},
                width:75, click: function () { this.topElement.cancelClick() } },
    // @value   DONE  Button object to fire dialog's "doneClick()" method on click.
    //                  Title derived from +link{Dialog.DONE_BUTTON_TITLE}.
    DONE    : {getTitle:function () {return isc.Dialog.DONE_BUTTON_TITLE},
                width:75, click: function () { this.topElement.doneClick() } }
    // @visibility external
    //<




});

// add standard instance properties
isc.Dialog.addProperties({

    //> @attr dialog.defaultWidth (int: 360 : IR)
    // @group appearance
    // @visibility external
    //<
    defaultWidth:360,

    title:"Dialog",

    //>    @attr    dialog.styleName    (CSSStyleName: "dialogBackground" : IRW)
    //            Style of the Dialog background
    //        @group    appearance
    //      @visibility external
    //<
    styleName:"dialogBackground",

    skinImgDir:"images/Dialog/",

    canDragReposition : false,
    canDragResize:false,

    //>    @attr dialog.autoCenter (boolean : true : IRW)
    // If true, this dialog will automatically be centered on the page when shown
    // If false, it will show up wherever you (or the user) last put it
    //        @group    appearance, location
    //        @see    dialog.show()
    //<
    autoCenter : true,

    // Body Settings
    // ----------------------------------------------------------------------------------------
    //>    @attr    dialog.bodyStyle    (string : "dialogBody" : IA)
    // Style of the Window body
    //        @group    appearance, header
    //        @see    Window.makeBody()
    //<
    bodyStyle:"dialogBody",

    //>    @attr    dialog.bodyColor        (CSSColor : "#DDDDDD" : IA)
    //            Color of the Window body.
    //            Overrides the background color specified in the style.
    //        @group    appearance, header
    //        @see    Window.makeBody()
    //      @see    Window.flash()
    //<
    bodyColor:"#DDDDDD",

    //>    @attr    dialog.hiliteBodyColor        (CSSColor : "#DDDDDD" : IA)
    // Highlight color for the Window body (shown when the body is flashed).
    //        @group    appearance, header
    //        @see    Window.makeBody()
    //      @see    Window.flash()
    //<
    hiliteBodyColor:"#FFFFFF",

    bodyDefaults: isc.addProperties({}, isc.Window.getInstanceProperty("bodyDefaults"),
    {
        layoutTopMargin:15,
        layoutLeftMargin:15,
        layoutRightMargin:15,
        layoutBottomMargin:5
    }),

    // Message & Icon
    // ---------------------------------------------------------------------------------------

    //> @attr dialog.message (HTMLString : null : IR)
    // Message to show in this dialog.
    // <P>
    // If a message is set the primary purpose of the dialog will be assumed to be to show a message with
    // buttons - auto-sizing to the message text will be enabled, and, if +link{dialog.icon} has also
    // been set, the +link{Dialog.messageLabel,messageLabel} and +link{Dialog.messageIcon,messageIcon}
    // AutoChildren will be created and placed together in the +link{Dialog.messageStack,messageStack}
    // AutoChild, with the toolbar underneath as usual.  If any of these behaviors are inconvenient or
    // you want more precise control over a message and some custom widgets, start from the superclass
    // +link{Window} instead, and add controls via +link{Window.addItem()}.
    // <P>
    // The message string may contain "${loadingImage}", if so, the standard loading spinner will appear at
    // that location in the text (see +link{Canvas.loadingImageSrc}).
    // <P>
    // The message will be styled with the +link{messageStyle}.
    //
    // @visibility external
    //<

    //> @attr dialog.messageStyle (CSSStyle : "normal" : IR)
    // Style to apply to the message text shown in the center of the dialog
    // @visibility external
    //<
    messageStyle:"normal",

    //> @attr dialog.messageLabel (Label AutoChild : null : IR)
    // AutoChild that shows +link{dialog.message}.
    // @visibility external
    //<
    messageLabelDefaults : {width : "100%", canSelectText: true},

    //> @attr dialog.messageIcon (Img AutoChild : null : IR)
    // AutoChild that shows +link{dialog.icon}.
    // @visibility external
    //<
    messageIconDefaults : { width:32, height:32 },

    //> @attr dialog.messageStack (Layout AutoChild : null : IR)
    // AutoChild that combines +link{dialog.message} and +link{dialog.icon}.
    // @visibility external
    //<
    messageStackDefaults : {height : 1, layoutMargin : 10, layoutBottomMargin:5, membersMargin:10},

    autoChildParentMap : isc.addProperties({}, isc.Window.getInstanceProperty("autoChildParentMap"),
    {
        messageStack : "body",
        messageIcon  : "messageStack",
        messageLabel : "messageStack"
    }),

    //> @attr dialog.icon (SCImgURL : null : IR)
    // Icon to show in this dialog - see +link{Dialog.message}.
    // @visibility external
    //<

    //> @attr dialog.iconSize (int : 32 : IR)
    // Size of the icon to show in this dialog.
    // @visibility external
    //<

    // Header
    // ----------------------------------------------------------------------------------------
    //>    @attr    dialog.headerStyle    (string : "DialogHeader" : IA)
    // Style of the Dialog header
    //        @group    appearance, header
    //        @see    Dialog.makeHeader()
    //<
    headerStyle:"dialogHeader",

    //>    @attr    dialog.windowHeaderHilite    (string : "WindowHeader" : IA)
    //            Highlight style for the Dialog header
    //        @group    appearance, header
    //        @see    Window.makeHeader()
    //<
    hiliteHeaderStyle:"dialogHeaderHilite",

    //>    @attr    dialog.headerLabelTextStyle    (string : "dialogHeaderText" : IA)
    //            Style of the Dialog headerLabel text
    //        @group    appearance, headerLabel
    //        @see    Dialog.makeHeaderLabel()
    //<

    headerLabelDefaults : isc.addProperties({},
                                            isc.Window.getInstanceProperty("headerLabelDefaults"),
                                            {styleName:"dialogHeaderText"}),

    // Header Icon
    // ----------------------------------------------------------------------------------------
    //>    @attr    dialog.showHeaderIcon        (boolean : false : IRW)
    //            should we show a headerIcon in the header,
    //            clicking it dismisses the Dialog
    //        @group    appearance, header
    //        @see    Dialog.makeHeaderIcon()
    //<
    showHeaderIcon:false,

    // Buttons
    // ----------------------------------------------------------------------------------------
    //>    @attr    Dialog.showMinimizeButton        (boolean : false : IRW)
    // Should we show a minimizeButton in the header, clicking it dismisses the Dialog
    //        @group    appearance, header
    //        @see    Dialog.makeMinimizeButton()
    //<
    showMinimizeButton:false,

    //>    @attr    Dialog.showMaximizeButton        (boolean : false : IRW)
    // Should we show a maximizeButton in the header, clicking it dismisses the Dialog
    //        @group    appearance, header
    //        @see    Dialog.makeMaximizeButton()
    //<
    showMaximizeButton:false,

    // Footer
    // ----------------------------------------------------------------------------------------
    //>    @attr    Dialog.showFooter        (boolean : false : IRW)
    // Should we show a footer for this Dialog, including resizer, statusBar, etc?
    //        @group    appearance, footer
    //<
    showFooter:false,

    // Toolbar
    // ----------------------------------------------------------------------------------------
    //>    @attr    Dialog.showToolbar        (boolean : null : IR)
    // Whether to show a toolbar of buttons at the bottom of the Dialog.
    // Default of null will cause the value to be resolved automatically to true or
    // false when the Dialog is first drawn according as +link{Dialog.toolbarButtons}
    // contains buttons or not.
    //        @group    appearance, toolbar
    // @visibility external
    //<
    showToolbar: null,

    //>    @attr dialog.autoFocus (Boolean : true : IR)
    // If a toolbar is showing, automatically place keyboard focus in the first button.
    // @group appearance, toolbar
    // @visibility external
    //<
    autoFocus :true,

    //> @attr Dialog.toolbar (AutoChild Toolbar : null : IR)
    // +link{AutoChild} of type Toolbar used to create the +link{toolbarButtons}.
    // @visibility external
    //<

    //> @attr Dialog.buttons (Array of Button or Button Properties : null : IR)
    // Array of Buttons to show in the +link{showToolbar,toolbar}, if shown.
    // <P>
    // The set of buttons to use is typically set by calling one of the shortcuts such as
    // +link{classMethod:isc.say()} or +link{classMethod:isc.confirm()}.  A custom set of buttons can be passed to
    // these shortcuts methods via the "properties" argument, or to a directly created Dialog.
    // <P>
    // In both cases, a mixture of +link{type:DialogButtons,built-in buttons}, custom buttons,
    // and other components (such as a +link{LayoutSpacer}) can be passed.  Built-in buttons
    // can be referred to as <code>isc.Dialog.OK</code>, for example:
    // <smartclient>
    // <pre>
    // isc.Dialog.create({
    //    buttons:[
    //       isc.Dialog.OK,
    //       isc.Dialog.CANCEL,
    //       isc.LayoutSpacer.create({width:50}),
    //       { title:"Not now", click:"doSomething()" }
    //    ]
    // })
    // </pre>
    // </smartclient>
    // <smartgwt>
    // <pre>
    // Dialog dialog = new Dialog();
    // Canvas layoutSpacer = new LayoutSpacer();
    // layoutSpacer.setWidth(50);
    // Button notNowButton = new Button("Not now");
    // notNowButton.addClickHandler(new ClickHandler() {
    //     public void onClick(ClickEvent event) {
    //         doSomething();
    //     }
    // });
    // dialog.setButtons(Dialog.OK, Dialog.CANCEL, layoutSpacer, notNowButton);
    // dialog.draw();
    // </pre>
    // </smartgwt>
    // Built-in buttons will call standard methods on the Dialog itself, such as
    // +link{dialog.cancelClick()}, as explained in the
    // +link{type:DialogButtons,list of built-in buttons}.
    //
    // @visibility external
    //<

    //> @attr Dialog.toolbarButtons (Array of Button or Button Properties : null : IR)
    // This is a synonym for +link{Dialog.buttons}
    //
    // @visibility external
    //<

    // Body Icons
    // ---------------------------------------------------------------------------------------
    askIcon:"[SKIN]ask.png",
    sayIcon:"[SKIN]say.png",
    warnIcon:"[SKIN]warn.png",
    confirmIcon:"[SKIN]confirm.png",

    // media exists, but no global helper, you have to call eg showMessage(message, "error")
    notifyIcon:"[SKIN]notify.png",
    errorIcon:"[SKIN]error.png",
    stopIcon:"[SKIN]stop.png",

    toolbarDefaults: isc.addProperties({}, isc.Window.getInstanceProperty("toolbarDefaults"),
    {
        // be minimum width and centered
        layoutAlign:"center", width:20,
        // batch bubbled clicks that hit buttons and report them via buttonClick
        click : function (item, itemNum) {
            this.Super("click", arguments);

            var target = isc.EH.getTarget(),
                index = this.getMemberNumber(target);
            if (index == -1) return;
            this.topElement.buttonClick(target, index);
        }
    })

});    // END    isc.Dialog.addProperties()

//!>Deferred

isc.Dialog.addMethods({

initWidget : function () {
    if (this.message != null) {
        this.autoSize = true;
    }

    // call the superclass function
    this.Super("initWidget",arguments);

    if (this.buttons) {
        this.toolbarButtons = this.buttons;
    }
},

createChildren : function () {

    // HACK: prevent toolbar from being created, since we want it placed in "messageStack", which
    // we can't create until Super.createChildren() creates the "body", which is "messageStack"'s
    // parent.
    var origSetting = this.showToolbar;
    this.showToolbar = false;
    this.Super("createChildren");
    this.showToolbar = origSetting;

    if (this.message != null) {
        // can't be done via defaults because policy and direction are dynamically determined
        this.body.hPolicy = "fill";

        this.addAutoChild("messageStack", null, isc.HStack);
        if (this.icon != null) {
            this.addAutoChild("messageIcon", {
                    // call getImgURL so we're in the Dialog's imgDir
                    src:this.getImgURL(this.icon)
                },
                isc.Img
            );
        }

        var message = this.message.evalDynamicString(this, {
            loadingImage: this.imgHTML(isc.Canvas.loadingImageSrc,
                                       isc.Canvas.loadingImageSize,
                                       isc.Canvas.loadingImageSize)
            });
        this.addAutoChild("messageLabel", {
            contents:message,
            baseStyle:this.messageStyle
        }, isc.Label);
    //} else {
    //    this.logWarn("this.message is null, not creating messageIcon...");
    }
    // automatically resolve showToolbar to true or false based on toolbarButtons
    if (this.showToolbar == null) {
        this.showToolbar = this.toolbarButtons != null && this.toolbarButtons.length > 0;
    }
    if (this.showToolbar) {
        this.makeToolbar();
    }
},

draw : function () {
    if (!this.readyToDraw()) return this;
    this.Super("draw", arguments);
    if (this.toolbar && this.autoFocus) {
        var firstButton = this.toolbar.getMember(0);
        if (firstButton) firstButton.focus();
    }
    return this;
},

//>    @method    Dialog.saveData()    (A)
// Method to save this Dialog's data. Called from <code>okClick()</code>,
// <code>applyClick()</code>.
// No default implementation - override to perform some action if required.
//
//        @group    buttons
//      @visibility external
//      @see okClick()
//      @see applyClick()
//<
saveData : function () {},

//> @method Dialog.closeClick()
// @include Window.closeClick()
//<

//>    @method    Dialog.cancelClick()
// Handle a click on the 'cancel' button of this Dialog.
// Default implementation is to return null and hide the Dialog.
// Override to do something else.
//        @group    buttons
//      @visibility external
//      @see type:DialogButtons
//<
cancelClick : function () {
    return this.closeClick();
},
// reroute the close button to call cancelClick
// (This way overrides to cancelClick will get fired - still falls through to closeClick())
_closeButtonClick : function () { return this.cancelClick() },

//>    @method    Dialog.okClick()    ()
// Handle a click on the 'ok' button of this Dialog.
// Default implementation is to call <code>saveData()</code>, hide the Dialog, then return
// <code>true</code>.
// Override to do something else.
//        @group    buttons
//      @visibility external
//      @see type:DialogButtons
//<
okClick : function () {
    this.saveData();

    this.clear();
    this.returnValue(true);
},


//>    @method    Dialog.applyClick()
// Handle a click on the 'apply' button of this Dialog.
// Default implementation is to call <code>saveData()</code>, but NOT close the Dialog.
//        @group    buttons
//      @visibility external
//      @see type:DialogButtons
//<
applyClick: function () {
    this.saveData();
},

//>    @method    Dialog.yesClick()
// Handle a click on the 'yes' button of this Dialog.
// Default implementation is to return <code>true</code>.
// Override to do something else
//        @group    buttons
//      @visibility external
//      @see type:DialogButtons
//<
yesClick : function () {
    this.returnValue(true);
},

//>    @method    Dialog.noClick()
// Handle a click on the 'no' button of this Dialog.
// Default implementation is to return <code>false</code>.
// Override to do something else.
//        @group    buttons
//      @visibility external
//      @see type:DialogButtons
//<
noClick : function () {
    this.returnValue(false);
},

//>    @method    Dialog.doneClick()
// Handle a click on the 'done' button of this Dialog.
// Default implementation is to hide the dialog then return <code>true</code>.
// Override to do something else.
//        @group    buttons
//      @visibility external
//      @see type:DialogButtons
//<
doneClick : function () {
    // refer to comment in okClick
    this.clear();
    this.returnValue(true);
},

//> @method Dialog.buttonClick(button)
// Fires when any button in this Dialog's toolbar is clicked.  Default implementation does nothing.
//
// @param button (Button) button that was clicked
// @param index (int) index of the button that was clicked
// @group  buttons
// @visibility external
//<
buttonClick : function (button, index) {
},

// for Autotest APIs
namedLocatorChildren:[
    "okButton", "applyButton", "yesButton", "noButton", "cancelButton", "doneButton"
]
});

isc.Dialog.changeDefaults("toolbarDefaults",
{

    makeButton : function (button) {
        var config = button,
            button = this.Super("makeButton", arguments);

        switch (config)
        {
        case isc.Dialog.OK:
            this.creator.okButton = button;
            button.locatorParent = this.creator;
            break;

        case isc.Dialog.APPLY:
            this.creator.applyButton = button;
            button.locatorParent = this.creator;
            break;

        case isc.Dialog.YES:
            this.creator.yesButton = button;
            button.locatorParent = this.creator;
            break;

        case isc.Dialog.NO:
            this.creator.noButton = button;
            button.locatorParent = this.creator;
            break;

        case isc.Dialog.CANCEL:
            this.creator.cancelButton = button;
            button.locatorParent = this.creator;
            break;

        case isc.Dialog.DONE:
            this.creator.doneButton = button;
            button.locatorParent = this.creator;
            break;
        }
        return button;
    }
});



//!<Deferred

//
//    Default Dialogs that we create
//


//>    @groupDef Prompting
//    Objects / methods used for displaying prompts and warnings to the user via (possibly modal)
//  isc Dialog objects.
// @treeLocation Client Reference/Control
//<


//>    @classAttr    Dialog.Prompt   (Dialog Properties : dialog instance properties : A)
//
//  The "Prompt" object on the dialog class is a singleton Dialog instance.
//  The Prompt is used to show text to the user in a modal fashion - it will expand to show
//  all the text that you put into it.
//  By default this Dialog has no end-user controls and is expected to be programmatically
//  dismissed.<br>
//  Common use-case: During server-interactions, the Prompt will be used to display a suitable
//  wait message, and suppress user input.<br><br>
//
// Notes:<br>
//  Because this is a singleton object, properties set on the Prompt directly will persist each
//  time it is shown.<br>
//  Developers should use the <code>showPrompt()</code> and <code>clearPrompt()</code> methods
//  to show and hide the prompt rather than manipulating the prompt directly.
//
// @group Prompting
// @visibility external
// @see classMethod:isc.showPrompt
// @see classMethod:isc.clearPrompt
//<
isc.Dialog.Prompt = {
    ID:"isc_globalPrompt",
    _generated:true,
    width:400,
    height:90,

    autoDraw:false,
    autoSize:true,
    isModal:true,
    showHeader:false,
    showFooter:false,
    showToolbar:false,

    dismissOnEscape:false,

    bodyStyle:"promptBody", // no border-top, since there is no header
                            // TODO autogenerate border in Window based on header visibility

    bodyDefaults: isc.addProperties({}, isc.Dialog.getInstanceProperty("bodyDefaults"), {height:"100%"}),

    // Message & Icon

    message:"Loading...&nbsp;${loadingImage}",

    messageStackDefaults: isc.addProperties({}, isc.Dialog.getInstanceProperty("messageStackDefaults"),
    {
        height: "100%",
        width: "100%",
        layoutAlign: "center"
    }),
    messageLabelDefaults: isc.addProperties({}, isc.Dialog.getInstanceProperty("messageLabelDefaults"),
    {
        width:"100%", align:isc.Canvas.CENTER, valign:isc.Canvas.CENTER
    }),
    layoutMargin:0,

    //>    @method    Prompt.showMessage()
    //    Show a message in the Dialog
    //
    //    Dialog will redraw and resize to show the entire message
    //    any properties in attributes will get applied and may be visibily changed
    //
    //    @param    newMessage    (string)    message to display
    //    @param    properties (Dialog Properties)    object of name:value pairs to apply to the object
    //                                    properties are applied before the redraw
    //<
    showMessage : function (newMessage, properties) {

        // first add the properties specified
        this.setProperties(properties);
        if (newMessage == null) newMessage = "&nbsp;"
        this.message = newMessage.evalDynamicString(this, {
            loadingImage: this.imgHTML(isc.Canvas.loadingImageSrc,
                                       isc.Canvas.loadingImageSize,
                                       isc.Canvas.loadingImageSize)
            });
        // Note: we lazily create children on draw, so verify that the items have been
        // initialized before manipulating the label
        if (!this._isInitialized) this.createChildren();

        this.messageLabel.setContents(this.message == null ? "&nbsp;" : this.message);

        this.show();
    },

    // clear the prompt message -- just clear the prompt

    clearMessage : function () {
        if (this.pendingFade) {
            isc.Timer.clearTimeout(this.pendingFade);
            delete this.pendingFade;
        }
        if (this.isAnimating(this._$hide)) {
            this.finishAnimation(this._$hide);
        }
        this.clear();
        if (this._clearPromptCallback) {
            this.fireCallback(this._clearPromptCallback);
            delete this._clearPromptCallback;
        }
    },

    fadeDuration:2000,
    fadeMessage : function () {
        delete this.pendingFade;
        this.animateHide("fade", {target:this, methodName:"clearMessage"});
    },

    // If the prompt gets destroyed, remove the pointer to it.

    destroy : function () {
        isc.Dialog.Prompt = this._originalProperties;
        return this.Super("destroy", arguments);
    }
};



//>    @classMethod isc.showPrompt()
//
//    Method available on the isc object to show a modal prompt to the user.
//  This method will display the message using the Dialog.Prompt singleton object.<br>
//  Note: if this prompt is to be shown to the user during some slow JavaScript logic, we
//  advise calling this method, then using +link{Class.delayCall()} or +link{Timer.setTimeout}
//  to kick off the slow logic in a separate thread. This ensures that the prompt is showing
//  before the lengthy execution begins.
//  <p/>Use <code>"\${loadingImage}"</code> to include +link{Canvas.loadingImageSrc,a loading image}.
//
//
//    @param    message            (string)    message to display
//    @param    [properties]    (Dialog Properties)    additional properties for the Dialog, applied before
//                                       the Dialog is shown
//
// @visibility external
// @see Dialog.Prompt
// @group Prompting
//<
isc.addGlobal("showPrompt", function (message, properties) {
    var prompt = isc.Dialog.Prompt;
    if (!isc.isA.Dialog(prompt)) {
        var props = prompt;
        prompt = isc.Dialog.Prompt = isc.Dialog.create(prompt);
        // If we destroy() the prompt, this allows us to essentially 'reset' ourselves to a
        // state where calling this method again will create a new prompt from the original
        // set of properties.

        prompt._originalProperties = props;
    }
    isc.Dialog.Prompt.showMessage(message, properties);
});

//>    @classMethod    isc.clearPrompt()
//
//    Clear the modal prompt being shown to the user.
//
//  @group Prompting
//  @visibility external
//  @see Dialog.Prompt
//<
isc.addGlobal("clearPrompt", function () {
    if (!isc.isA.Dialog(isc.Dialog.Prompt)) return; // prompt has never been shown
    isc.Dialog.Prompt.clearMessage();
});



//>    @classMethod isc.showFadingPrompt()
//
//    Method available on the isc object to show a temporary modal prompt to the user.
//  This method will display the message using the Dialog.Prompt singleton object, then hide it
//  using a fade animation effect.<br>
//  Note: if this prompt is to be shown to the user during some slow JavaScript logic, we
//  advise calling this method, then using +link{Class.delayCall()} or +link{Timer.setTimeout}
//  to kick off the slow logic in a separate thread. This ensures that the prompt is showing
//  before the lengthy execution begins.
//  <P>
//  The prompt may be cleared before the duration has elapsed via a call to +link{isc.clearPrompt()}
//  and any callback specified will still be fired even if the prompt is dismissed early.
//
//    @param    message            (string)    message to display
//  @param  duration        (number)    how long the message should appear for in milliseconds before
//    fading from view.
//  @param  [callback]      (callback) When the prompt is hidden, callback will be fired.
//    @param    [properties]    (Dialog Properties)    additional properties for the Dialog, applied before
//                                       the Dialog is shown
//
// @visibility external
// @see Dialog.Prompt
// @group Prompting
//<


isc.addGlobal("showFadingPrompt", function (message, duration, callback, properties) {
    if (isc.isA.Canvas(isc.Dialog.Prompt) && isc.Dialog.Prompt.isDrawn()) {
        isc.clearPrompt();
    }
    isc.showPrompt(message, properties);
    var prompt = isc.Dialog.Prompt;
    // this defaults to 2000 ms (though undocumented as such right now)
    if (duration == null) duration = prompt.fadeDuration;

    prompt._clearPromptCallback = callback;
    prompt.pendingFade = prompt.delayCall("fadeMessage", [], duration);
});


////////////////////////////////////////////////////////////////////////////////////////////


//>    @classAttr    Dialog.Warn (Dialog Properties : dialog instance properties : A)
//
// A singleton Dialog instance that will show text to the user and provide buttons for their
// response.  The Dialog will expand to show all the text that you put into it. This dialog
// is shown in response to calls to +link{classMethod:isc.say()}, +link{classMethod:isc.warn()}, +link{classMethod:isc.ask} and
// +link{classMethod:isc.confirm()}
// <P>
// This can be used in cases where a developer would alternatively make use of the native
// JavaScript <code>alert()</code> and <code>confirm()</code> methods.  The main differences
// between those methods and using the Warn object are:<br>
// - The Warn object can be customized by modifying which buttons are visible, the style
//   applied to it, etc.<br>
// - The <code>isc.ask()</code> and <code>isc.warn()</code> methods are asynchronous - rather
//   than returning a value indicating the user's response, a callback method will be fired
//   when the user interacts with the dialog.<br><br>
//
// Notes:<br>
//  Because this is a singleton object, properties set on the Warn object directly will persist
//  each time it is shown.<br>
//  Developers should use the <code>warn()</code> or <code>ask()</code> methods to show and
//  hide this object rather than manipulating the Dialog directly.
//  @group  Prompting
//  @visibility external
//  @see classMethod:isc.warn
//  @see classMethod:isc.ask
//<
isc.Dialog.Warn = {
    ID:"isc_globalWarn",
    _generated:true,
    width:360,
    height:60,

    isModal:true,
    canDragReposition:true,
    keepInParentRect:true,

    autoDraw:false,
    autoSize:true,
    autoCenter:true,

    buttons:[isc.Dialog.OK],
    message:"Your message here!",

    //>    @method    Warn.showMessage()
    // Show a message in the Dialog
    //
    // Dialog will redraw and resize to show the entire message
    // any properties in attributes will get applied and may be visibily changed
    //
    //    @param    newMessage    (string)    message to display
    //    @param    attributes    (Dialog Properties)    object of name:value pairs to apply to the object
    //                                    properties are applied before the redraw
    //<
    showMessage : function (newMessage, properties) {
        if (newMessage == null) newMessage = "&nbsp;";

        this.message = newMessage;

        if (!this.icon && properties.icon) this.icon = properties.icon;
        this.setProperties(properties);
        // if no callback was specified, clear the Dialog callback
        if (properties.callback == null) delete this.callback;

        // Note: we lazily create children on draw, so verify that the items have been
        // initialized before manipulating the label
        if (!this._isInitialized) this.createChildren();

        // Update the label in the body
        this.messageLabel.setContents(this.message == null ? "&nbsp;" : this.message);

        if (this.icon) {
            if (this.messageIcon) {
                this.messageIcon.setSrc(this.getImgURL(this.icon));
                this.messageIcon.show();
            }
        } else if (this.messageIcon) this.messageIcon.hide();

        // do immediate relayout so we don't wait for timers before we draw the new buttons,
        // especially because the destroy is immediate but the new draw is delayed, and in the
        // interim things react to the empty toolbar.
        this.toolbar.layoutChildren();
        // since we're going to try to autoCenter on show(), we go ahead and get all relayout
        // done now
        if (this.messageLabel.isDirty()) this.messageLabel.redraw();
        if (this.isDrawn()) {
            this.messageStack.layoutChildren();
            this.body.layoutChildren();
            this.layoutChildren();
        }

        this.show();

        // focus in the first button so you can hit Enter to do the default thing
        if (this.toolbar) {
            var firstButton = this.toolbar.getMember(0);
            /*
            this.logWarn("focusing on first button: " + firstButton +
                         ", drawn: " + firstButton.isDrawn() +
                         ", disabled: " + firstButton.isDisabled() +
                         ", visible: " + firstButton.isVisible() +
                         ", canFocus: " + firstButton._canFocus());
            */
            firstButton.focus();
        }
    }
};

//> @classMethod isc.showMessage()
// Show a modal dialog with a message, icon, and response buttons.
//<
isc.addGlobal("showMessage", function (message, messageType, callback, properties) {

    if ((isc.isA.String(properties) || isc.isA.Function(properties)) ||
        (properties == null && isc.isAn.Object(callback) && callback.methodName == null &&
         callback.action == null && callback.method == null))
    {
        // swap arguments
        var realCallback = properties;
        properties = callback;
        callback = realCallback;
    }

    if (!isc.isA.Dialog(isc.Dialog.Warn) ||
            (isc.isA.Function(isc.Dialog.Warn.initialized) && !isc.Dialog.Warn.initialized()))
    {
        isc.Dialog.Warn = isc.Dialog.create(isc.Dialog.Warn);
    }
    if (!properties) properties = {};

    // We support toolbarButtons and buttons - copy across to "buttons" attr so we can
    // easily check if they were specified on the object passed in and otherwise apply defaults.
    if (properties.toolbarButtons != null) {
        properties.buttons = properties.toolbarButtons;
        delete properties.toolbarButtons;
    }
    // messageType is one of
    // "confirm" (confirm dialog)
    // "ask" (ask dialog)
    // "say", "warn" (info / warn dialog)
    if (!properties.buttons) {
        if (messageType == "confirm") {
            properties.buttons = [isc.Dialog.OK, isc.Dialog.CANCEL];
        } else if (messageType == "ask") {
            properties.buttons = [isc.Dialog.YES, isc.Dialog.NO];
        } else {
            properties.buttons = [isc.Dialog.OK];
        }
    }


    // Title: If specified in properties, respect it, otherwise show the
    // appropriate default title based on the dialog type
    if (!properties.title) {
        if (messageType == "confirm") properties.title = isc.Dialog.CONFIRM_TITLE;
        else if (messageType == "ask") properties.title = isc.Dialog.ASK_TITLE;
        else if (messageType == "warn") properties.title = isc.Dialog.WARN_TITLE;
        else properties.title = isc.Dialog.SAY_TITLE;
    }

    isc._applyDialogHandlers(properties);

    if (!properties.icon) properties.icon = isc.Dialog.getInstanceProperty(messageType+"Icon");
    if (callback) properties.callback = callback;

    isc.Dialog.Warn.showMessage(message, properties);
});

//> @classMethod isc.getLastDialog()
// Returns the last-shown isc.say/ask/warn/confirm dialog.  Do not document externally.
//<
isc.addGlobal("getLastDialog", function () {
    return isc.Dialog.Warn;
});

//> @classMethod isc.dismissLastDialog()
// Dismisses the last-shown isc.say/ask/warn/confirm dialog.  Do not document externally.
//<
isc.addGlobal("dismissLastDialog", function () {
    if (isc.Dialog.Warn) isc.Dialog.Warn.hide();
});

// shared with askForValue()
isc._applyDialogHandlers = function (properties) {

    var defaultHandlers = this._defaultHandlers =
        this._defaultHandlers || ["okClick", "yesClick", "noClick",
                                  "cancelClick", "closeClick", "applyClick"];
    for (var i = 0; i < defaultHandlers.length; i++) {
        var handlerName = defaultHandlers[i];
        if (!properties[handlerName]) {
            properties[handlerName] = isc.Dialog.getInstanceProperty(handlerName);
        }
    }
}

//>    @classMethod    isc.warn()
// Show a modal dialog with a message, icon, and "OK" button.
// <P>
// The callback will receive boolean true for an OK button click, or null if the Dialog is
// dismissed via the close button.
//
//    @param    message            (string)    message to display
//  @param  [callback]      (Callback)  Optional Callback to fire when the user
//                                      dismisses the dialog. This has the single parameter
//                                      'value', indicating the value returned by the Warn
//                                      dialog from 'okClick()' etc.
//    @param    [properties]    (Dialog Properties)    additional properties for the Dialog.
//                                      To set +link{Dialog.buttons,custom buttons} for
//                                      the Dialog, set properties.buttons to
//                                      an array of buttons
//                                        eg:    { buttons : [Dialog.OK, Dialog.CANCEL] }
// @group Prompting
// @visibility external
// @see classAttr:Dialog.Warn
// @see classMethod:isc.say()
// @see classMethod:isc.ask()
// @see method:Dialog.okClick()
// @see classAttr:Dialog.WARN_TITLE
//<
isc.addGlobal("warn", function (message, callback, properties) {
    isc.showMessage(message, "warn", callback, properties);
});

//>    @classMethod    isc.say()
// Show a modal dialog with a message, icon, and "OK" button.  Intended for notifications which
// are not really warnings (default icon is less severe).
// <P>
// The callback will receive boolean true for an OK button click, or null if the Dialog is
// dismissed via the close button.
//
//    @param    message            (string)    message to display
//  @param  [callback]      (Callback)  Optional Callback to fire when the user
//                                      dismisses the dialog. This has the single parameter
//                                      'value', indicating the value returned by the Warn
//                                      dialog from 'okClick()' etc.
//    @param    [properties]    (Dialog Properties)    additional properties for the Dialog.
//                                      To set +link{Dialog.buttons,custom buttons} for
//                                      the Dialog, set properties.buttons to an array of
//                                      buttons
//                                        eg:    { buttons : [Dialog.OK, Dialog.CANCEL] }
// @group Prompting
// @visibility external
// @see classAttr:Dialog.Warn
// @see classMethod:isc.warn()
// @see classMethod:isc.ask()
// @see method:Dialog.okClick()
// @see classAttr:Dialog.SAY_TITLE
//<
isc.addGlobal("say", function (message, callback, properties) {
    isc.showMessage(message, "say", callback, properties);
});


//>    @classMethod    isc.ask()
// Show a modal dialog with a message, icon, and "Yes" and "No" buttons.
// <P>
// The callback will receive boolean true for an OK button click, boolean false for a No button
// click, or null if the Dialog is dismissed via the close button.
//
//    @param    message            (string)    message to display
//  @param  [callback]      (Callback)  Callback to fire when the
//                                      user clicks a button to dismiss the dialog.
//                                      This has the single parameter 'value', indicating the
//                                      value returned by the Warn dialog from 'okClick()' etc.
//    @param    [properties]    (Dialog Properties)    additional properties for the Dialog.
//                                      To set +link{Dialog.buttons,custom buttons} for
//                                      the Dialog, set properties.buttons to an array
//                                      of buttons
//                                        eg:    { buttons : [Dialog.OK, Dialog.CANCEL] }
//
// @group Prompting
// @visibility external
// @see Dialog.Warn
// @see classMethod:isc.warn()
// @see method:Dialog.yesClick()
// @see method:Dialog.noClick()
// @see classAttr:Dialog.ASK_TITLE
// @example dialogs
//<
isc.addGlobal("ask", function (message, callback, properties) {
    isc.showMessage(message, "ask", callback, properties);
});

//>    @classMethod    isc.confirm()
// Show a modal dialog with a message, icon, and "OK" and "Cancel" buttons.
// <P>
// The callback will receive boolean true for an OK button click, or null for a Cancel click or
// if the Dialog is dismissed via the close button.
// <P>
// Note: this does not override the native window.confirm() method.
//
//    @param    message            (string)    message to display
//  @param  [callback]      (Callback)  Callback to fire when the
//                                      user clicks a button to dismiss the dialog.
//                                      This has the single parameter 'value', indicating the
//                                      value returned by the Warn dialog from 'okClick()' etc.
//    @param    [properties]    (Dialog Properties)    additional properties for the Dialog.
//                                      To set +link{Dialog.buttons,custom buttons} for
//                                      the Dialog, set properties.buttons to an array of
//                                      buttons
//                                        eg:    { buttons : [Dialog.OK, Dialog.CANCEL] }
//
// @group Prompting
// @visibility external
// @see Dialog.Warn
// @see classMethod:isc.warn()
// @see method:Dialog.okClick()
// @see method:Dialog.cancelClick()
// @see classAttr:Dialog.CONFIRM_TITLE
// @example dialogs
//<
isc.confirm = function (message, callback, properties) {
    isc.showMessage(message, "confirm", callback, properties);
}

//>    @classAttr    Dialog.Ask (Dialog Properties : dialog instance properties : A)
//
// A singleton Dialog instance that will be shown in response to a +link{isc.askForValue()} call.
//
// Notes:<br>
//  Because this is a singleton object, properties set on the Ask object directly will persist
//  each time it is shown.<br>
//  Developers should use the <code>askForValue()</code> method to show this object rather than
//  manipulating the Dialog directly.
//  @group  Prompting
//  @visibility external
//  @see classMethod:isc.askForValue
//<

//> @classMethod isc.askForValue()
// Show a modal dialog with a text entry box, asking the user to enter a value.
// <P>
// As with other convenience methods that show Dialogs, such as +link{classMethod:isc.warn()},
// the dialog is shown and the function immediately returns.  When the user responds, the
// provided callback is called.
// <P>
// If the user clicks OK, the value typed in is passed to the callback (including the empty
// string ("") if nothing was entered.  If the user clicks cancel, the value passed to the
// callback is null.
// <P>
// A default value for the text field can be passed via <code>properties.defaultValue</code>.
// <P>
// Keyboard focus is automatically placed in the text entry field, and hitting the enter key is
// the equivalent of pressing OK.
//
//    @param    message            (string)    message to display
//  @param  [callback]      (Callback)  Callback to fire when the
//                                      user clicks a button to dismiss the dialog.
//                                      This has the single parameter 'value', indicating the
//                                      user entry, or null if cancel was pressed or the window
//                                      closed
//    @param    [properties]    (Dialog Properties)    additional properties for the Dialog.
//                                      To set +link{Dialog.buttons,custom buttons} for
//                                      the Dialog, set properties.buttons to an array of
//                                      buttons
//                                        eg:    { buttons : [Dialog.OK, Dialog.CANCEL] }
//
// @see method:Dialog.okClick()
// @see method:Dialog.cancelClick()
// @see classAttr:Dialog.ASK_FOR_VALUE_TITLE
// @group Prompting
// @visibility external
//<
isc.askForValue = function (message, callback, properties) {
    properties = properties || isc.emptyObject;

    var askDialog = isc.Dialog.Ask;
    if (!askDialog) {
        var askForm = isc.DynamicForm.create({
            numCols:1,
            padding:3,
            items: [
                { name:"message", type:"blurb" },
                { name:"value", showTitle:false, width:"*" }
            ],
            // fire okClick on enter
            saveOnEnter:true,
            submit : function () { this.askDialog.okClick(); }
        });
        askDialog = isc.Dialog.Ask = isc.Dialog.create({
            items : [ askForm ],
            askForm: askForm,
            canDragReposition:true,
            isModal:true,
            // accomplishes vertical autoSizing
            bodyProperties : {overflow:"visible"},
            overflow:"visible"
        });
        askForm.askDialog = askDialog;

        // return the form value to the callback on okClick
        askDialog._okClickFunction = function () {
            this.clear();
            this.returnValue(this.askForm.getValue("value"));
        }
    }
    // If we were given explicit left/top coords, auto-center, otherwise respect them
    var explicitPosition = properties.left != null || properties.top != null;

    if (properties.toolbarButtons != null) {
        properties.buttons = properties.toolbarButtons;
        delete properties.toolbarButtons;
    }

    // copy properties and install defaults
    properties = isc.addProperties({
        callback: callback,
        title: properties.title || isc.Dialog.ASK_FOR_VALUE_TITLE,
        autoCenter:!explicitPosition,
        left: (explicitPosition ? properties.left || "10%" : null),
        top: (explicitPosition ? properties.top || "20%" : null),
        width: properties.width || "80%",
        height: properties.height || 20,
        buttons: properties.buttons || [ isc.Dialog.OK, isc.Dialog.CANCEL ],
        okClick : properties.okClick || askDialog._okClickFunction
    }, properties);

    // have standard handlers added to properties
    isc._applyDialogHandlers(properties);

    askDialog.setProperties(properties);

    askDialog.askForm.setValues({
        message : message || "Please enter a value:",
        value : properties.defaultValue || ""
    });
    askDialog.show();
    askDialog.askForm.focusInItem("value");
}

//> @classMethod isc.showLoginDialog()
// Handle a complete login interaction with a typical login dialog asking for username and
// password credentials using the +link{LoginDialog} class.
// <P>
// As with other convenience methods that show Dialogs, such as +link{classMethod:isc.warn()},
// the dialog is shown and the function immediately returns.  When the user responds, the
// provided callback function is called.
// <P>
// If the user clicks the "Log in" button, the credentials entered by the user are passed to
// the provided "loginFunc" as an Object with properties "username" and "password" (NOTE: both
// property names are all lowercase), as the variable "credentials".  For example:
// <pre>{ username: "barney", password: "rUbbL3" }</pre>
// <P>
// The "loginFunc" should then attempt to log in by whatever means is necessary.  The second
// parameter to the loginFunc, "dialogCallback", is a function, which must be called <i>whether
// login succeeds or fails</i> with a true/false value indicating whether login succeeded.
// <P>
// If the login dialog is dismissable (settable as properties.dismissable, default false) and
// the user dismisses it, the loginFunc will be fired with null for the credentials.
// <P>
// The following code shows typical usage.  This code assumes you have created a global
// function sendCredentials() that send credentials to some authentication system and fires a
// callback function with the result:
// <pre>
// isc.showLoginDialog(function (credentials, dialogCallback) {
//     if (credentials == null) return; // dismissed
//
//     // send credentials
//     sendCredentials(credentials, function (loginSucceeded) {
//         // report success or failure
//         dialogCallback(loginSucceeded);
//     })
// })
// </pre>
// The login dialog has several built-in behaviors:
// <ul>
// <li> keyboard focus is automatically placed in the username field
// <li> hitting enter in the username field proceeds to the password field
// <li> hitting enter in the password field submits (fires the provided callback)
// </ul>
// In addition to normal properties supported by Dialog/Window, the following special
// properties can be passed:
// <ul>
// <li><code>username</code>: initial value for the username field
// <li><code>password</code>: initial value for the password field
// <li><code>usernameTitle</code>: title for the username field
// <li><code>passwordTitle</code>: title for the password field
// <li><code>errorMessage</code>: default error message on login failure
// <li><code>loginButtonTitle</code>: title for the login button
// <li><code>dismissable</code>: whether the dialog can be dismissed, default false
// <li><code>errorStyle</code>: CSS style for the error message, if shown
// </ul>
// See below for links to the default values for these properties.
//
//  @param  loginFunc       (Callback)  Function to call to attempt login.  Receives parameters
//                                      "credentials" and "dialogCallback", described above
//    @param    [properties]    (LoginDialog Properties)    additional properties for the Dialog
//
// @see classAttr:LoginDialog.LOGIN_TITLE
// @see classAttr:LoginDialog.USERNAME_TITLE
// @see classAttr:LoginDialog.PASSWORD_TITLE
// @see classAttr:LoginDialog.LOGIN_BUTTON_TITLE
// @see classAttr:LoginDialog.LOGIN_ERROR_MESSAGE
// @group Prompting
// @visibility external
//<

//>    @class    LoginDialog
// Handle a complete login interaction with a typical login dialog asking for username and
// password credentials. Use this
// class to quickly present a traditional username/password authentication mechanism in a
// SmartClient window.
// <p>
// To adapt this class to your requirements, first implement LoginDialog.loginFunc to submit
// the username and password to the authentication mechanism of your choice, calling
// dialogCallback once the authentication process completes.
//
// @see classMethod:isc.showLoginDialog
// @treeLocation Client Reference/Control
// @group Prompting
// @visibility external
//<

isc.ClassFactory.defineClass("LoginDialog", "Window");
isc.LoginDialog.registerStringMethods({
    //> @method loginDialog.register()
    // Called if the user clicks on the +link{loginDialog.registrationItem,registration link}
    // on the login form. Implement this method to allow the user to register for a
    // new account.
    // @param values (Object) Current values of form fields
    // @param form (DynamicForm) Form on which the link was clicked
    // @visibility external
    //<
    register:"values, form",

    //> @method loginDialog.lostPassword()
    // Called if the user clicks on the +link{loginDialog.lostPasswordItem,"Lost Password"} link
    // on the login form. Implement this method to allow the user to request the password
    // be resent or reset.
    // @param values (Object) Current values of form fields
    // @param form (DynamicForm) Form on which the link was clicked
    // @visibility external
    //<
    lostPassword:"values, form"
});
isc.LoginDialog.addClassProperties({
    firstTimeInit: true
});
isc.LoginDialog.addProperties({
    //> @method loginDialog.loginFunc()
    // User-supplied callback function to process login transactions.
    // <p>If the user clicks the "Log in" button, the credentials entered by the user are passed to
    // loginFunc as an Object with properties "username" and "password" (NOTE: both
    // property names are all lowercase), as the variable "credentials".  For example:
    // <pre>{ username: "barney", password: "rUbbL3" }</pre>
    // <P>
    // This function should then attempt to log in by whatever means is necessary.  The second
    // parameter to the loginFunc, "dialogCallback", is a function, which must be called <i>whether
    // login succeeds or fails</i> with a true/false value indicating whether login succeeded.
    // <P>
    // If the login dialog is dismissable (settable as properties.dismissable, default false) and
    // the user dismisses it, loginFunc will be fired with null for the credentials.
    // <P>
    // The following code shows typical usage.  This code assumes you have created a global
    // function sendCredentials() that send credentials to some authentication system and fires a
    // callback function with the result:
    // <pre>
    // ...
    // loginFunc : function (credentials, dialogCallback) {
    //     if (credentials == null) return; // dismissed
    //
    //     // send credentials
    //     sendCredentials(credentials, function (loginSucceeded) {
    //         // report success or failure
    //         dialogCallback(loginSucceeded);
    //     })
    // })
    // ...
    // </pre>
    // @param credentials (Object) Login credentials supplied by the user
    // @param dialogCallback (Function) Function that must be called once the login transaction
    // completes
    // @visibility external
    //<

    //> @attr loginDialog.dismissable (Boolean : false : [IR])
    // Whether the user should be able to dismiss the login dialog without entering
    // credentials.  Set to true if logging in is optional.  When set, a close button will be
    // present, and hitting escape will also dismiss the dialog.
    // <p>
    // If the Dialog is dismissed, +link{LoginDialog.loginFunc} is called with null arguments.
    // <p>
    // Note that this attribute overrides the dismissOnEscape and showCloseButton attributes.
    // @visibility external
    //<
    dismissable: false,

    //> @attr   loginDialog.dismissOnEscape  (boolean : null : [IRW])
    // Do not set LoginDialog.dismissOnEscape; it is controlled by the
    // +link{LoginDialog.dismissable}
    // property.
    // @visibility external
    //<

    //>    @attr    loginDialog.showCloseButton        (boolean : true : [IRW])
    // Do not set LoginDialog.showCloseButton; it is controlled by the
    // +link{LoginDialog.dismissable}
    // property.
    // @visibility external
    //<

    //> @attr loginDialog.allowBlankPassword (Boolean : false : IR)
    // If true, the login form will allow blank passwords to be submitted. Otherwise
    // the form fails to be validated until the user enters at least one character into
    // the password field.
    // @visibility external
    //<
    allowBlankPassword: false,

    //> @attr loginDialog.showLostPasswordLink (Boolean : false : IR)
    // If true, display a +link{LinkItem} (+link{LoginDialog.lostPasswordItem})
    // meant for the user to click if the account's
    // credentials are forgotten. The text of the link is controlled by
    // +link{loginDialog.lostPasswordItemTitle}. If clicked, the link will fire
    // +link{loginDialog.lostPassword()}.
    // @visibility external
    //<
    showLostPasswordLink: false,

    //> @attr loginDialog.showRegistrationLink (Boolean : false : IR)
    // If true, display a +link{LinkItem} (+link{LoginDialog.registrationItem})
    // meant for the user to click if the user wishes to register a new account.
    // The text of the link is controlled by
    // +link{loginDialog.registrationItemTitle}. If clicked, the link will fire
    // +link{loginDialog.register()}.
    // @visibility external
    //<
    showRegistrationLink: false,

    //> @attr loginDialog.title (String : Dialog.LOGIN_TITLE : IR)
    // Specifies the title of the dialog box.
    // @visibility external
    //<

    //> @attr loginDialog.errorStyle (String : "formCellError" : IR)
    // Specifies the CSS style of the error text shown for a login failure.
    // @visibility external
    //<
    errorStyle: "formCellError",

    //> @attr loginDialog.usernameItemTitle (String : Dialog.USERNAME_TITLE : IR)
    // Specifies the title of the "usernameItem" field of the +link{loginForm}.
    // @visibility external
    //<

    //> @attr loginDialog.passwordItemTitle (String : Dialog.PASSWORD_TITLE : IR)
    // Specifies the title of the "passwordItem" field of the +link{loginForm}.
    // @visibility external
    //<

    //> @attr loginDialog.loginButtonTitle (String : Dialog.LOGIN_BUTTON_TITLE : IR)
    // Specifies the contents of the login submission button of the +link{loginForm}.
    // @visibility external
    //<

    //> @attr loginDialog.lostPasswordItemTitle (String : LoginDialog.lostPasswordItemTitle : IR)
    // Specifies the contents of the password request button (if configured) on
    // the +link{loginForm}.
    // @visibility external
    //<
    lostPasswordItemTitle: "Lost Password?",

    //> @attr loginDialog.registrationItemTitle (String : LoginDialog.registrationItemTitle : IR)
    // Specifies the contents of the registration link (if configured) on
    // the +link{loginForm}.
    // @visibility external
    //<
    registrationItemTitle: "Register",

    //> @attr loginDialog.errorMessage (String : Dialog.LOGIN_ERROR_MESSAGE : IR)
    // Specifies the default error message displayed on the login form when
    // authentication fails.
    // @visibility external
    //<

    autoCenter: true,
    autoSize: true,
    isModal: true,
    showMinimizeButton:false,

    //> @attr loginDialog.items (Array of String : ["autoChild:loginForm"] : IR)
    // Specifies the dialog contents. By default, the dialog only contains
    // +link{LoginDialog.loginForm}. If desired, additional widgets may be placed before/after
    // the loginForm. To specify these widgets as +link{group:autoChildren}, use the syntax
    // "autoChild:<i>childName</i>" +link{group:autoChildren,as used for panes/items of
    // Tabs/SectionStacks}.
    // @visibility external
    //<
    items: [ "autoChild:loginForm" ],


    //> @attr loginDialog.formFields (Array of FormItem Properties : null : IR)
    // Customizes the fields present in the dialog, or specifies new fields to be
    // present, in the same manner as with +link{DynamicForm.fields}.
    //
    // @see DataBoundComponent.fields
    // @visibility external
    //<

    //> @attr loginDialog.loginFailureItem ( AutoChild BlurbItem : null : [IR] )
    // Field item containing login error message (if required) in +link{LoginDialog.loginForm}.
    // @visibility external
    //<

    //> @attr loginDialog.usernameItem ( AutoChild TextItem : null : [IR] )
    // Username field item in +link{LoginDialog.loginForm}.
    // @visibility external
    //<

    //> @attr loginDialog.lostPasswordItem ( AutoChild LinkItem : null : [IR] )
    // +link{linkItem} to page requesting forgotten password in +link{LoginDialog.loginForm}.
    // <p>To handle user clicks on this link, implement +link{loginDialog.lostPassword}.
    // <p>To handle a user click as a physical link to another page, set
    // +link{formItem.defaultValue,defaultValue} via loginDialog.lostPasswordItemProperties:
    // <code>
    // lostPasswordItemProperties: {
    //     defaultValue: "register.html"
    // },
    // </code>
    // @see loginDialog.showLostPasswordLink
    // @see loginDialog.lostPasswordItemTitle
    // @visibility external
    //<

    //> @attr loginDialog.registrationItem ( AutoChild LinkItem : null : [IR] )
    // +link{linkItem} to page requesting new user registration in +link{LoginDialog.loginForm}.
    // <p>To handle user clicks on this link, implement +link{loginDialog.register}.
    // <p>To handle a user click as a physical link to another page, set
    // +link{formItem.defaultValue,defaultValue} via loginDialog.registrationItemProperties:
    // <code>
    // registrationItemProperties: {
    //     defaultValue: "register.html"
    // },
    // </code>
    // @see loginDialog.showRegistrationLink
    // @see loginDialog.registrationItemTitle
    // @visibility external
    //<

    //> @attr loginDialog.passwordItem ( AutoChild PasswordItem : null : [IR] )
    // Password field item in +link{LoginDialog.loginForm}.
    // @see loginDialog.allowBlankPassword
    // @see loginDialog.passwordItemTitle
    // @visibility external
    //<

    //> @attr loginDialog.loginButton ( AutoChild ButtonItem : null : [IR] )
    // Login submission button in +link{LoginDialog.loginForm}.
    // @see loginDialog.loginButtonTitle
    // @visibility external
    //<

    //> @attr loginDialog.defaultValues ( Object : null : [IR] )
    // Adds default values to +link{loginDialog.loginForm}.
    // @visibility internal
    //<

    //> @attr loginDialog.loginForm ( AutoChild DynamicForm : null : R )
    // Form used to request login credentials from the user.
    // @see loginDialog.formFields
    // @visibility external
    //<

    loginFormConstructor: "DynamicForm",
    loginFormDefaults: {
        numCols: 2,
        padding: 4,
        autoDraw: false,
        saveOnEnter:true,

        submit : function () {
            var loginForm = this,
                params = [{
                    username : this.getValue("usernameItem"),
                    password : this.getValue("passwordItem")
                }];

            params[1] = function (success, errorMessage) {
                if (success) {
                    loginForm.complete(); // report success
                } else {
                    // failed login attempt - indicate failure, remain visible
                    if (errorMessage != null)
                        loginForm.setValue("loginFailureItem", errorMessage)
                    loginForm.showItem("loginFailureItem");
                    loginForm.focusInItem("passwordItem");
                }
            };

            this.fireCallback(this.loginDialog.loginFunc, "credentials,dialogCallback", params);
        },
        complete : function (dismissed) {
            this.loginDialog.hide();
            this.setValue("loginFailureItem", this.loginDialog.errorMessage);
            // reset for next time
            this.setValue("usernameItem", "");
            this.setValue("passwordItem", "");
            this.hideItem("loginFailureItem");

            // if this was a dismissal, tell the loginFunc
            if (dismissed) {
                this.fireCallback(this.loginFunc, "credentials,dialogCallback");
            } else {
                // if the server provided a loginRedirect, use it. This will be set
                // if an Authentication login page is visited without having credentials.

                var loginRedirect = isc.Cookie.get("loginRedirect");
                if (loginRedirect) window.location.replace(loginRedirect);
            }
        }
    },

    formDSDefaults: {
        clientOnly: true,
        useAllDataSourceFields: true
    },
    formDefaultFields: [
        { name: "loginFailureItem", type:"blurb", colSpan: 2, visible:false },
        { name: "usernameItem", required:true,
            // Diable spell checking etc where supported
            browserSpellCheck:false,
            browserAutoCorrect:false,
            browserAutoCapitalize:false,

            keyPress : function (item, form, keyName) {
                if (keyName == "Enter") {
                    form.focusInItem("passwordItem");
                    return false;
        }}},
        { name: "passwordItem", type: "password", required: true },
        { name: "loginButton", type:"button", type:"submit" },
        { name: "lostPasswordItem", type: "link", target: "javascript", canEdit:false,
            endRow: true, numCols:2, colSpan:2, showTitle: false,
            click: "form.loginDialog.lostPassword(form.getValues(), form)"
        },
        { name: "registrationItem", type: "link", target: "javascript", canEdit:false,
            endRow: true, numCols: 2, colSpan: 2, showTitle: false,
            click: "form.loginDialog.register(form.getValues(), form)"
        }
    ],

    getDynamicDefaults : function (child) {
        switch (child) {
        case "loginForm":
            var ret = {
                loginDialog: this,
                values: {
                    usernameItem: this.username || "",
                    passwordItem: this.password || "",
                    loginFailureItem: this.errorMessage
                },
                fields: this.formFields
            };

            // Bind form to datasource containing internally specified FormItem
            // properties. This datasource is updated with properties slurped up
            // from LoginDialog itself, ie usernameItemTitle, etc.
            // The user manipulates the form items either through
            // <item name>Properties (which ultimately affects the datasource
            // fields) or through formFields (which ultimately affects
            // form.fields itself).

            // safe to clone - not manipulated yet
            var updatedFields = isc.clone(this.formDefaultFields);

            // Build fields from this.<fieldName>Defaults + this.<fieldName>Properties +
            // this.<fieldName>Title. However, LinkItem fields need special treatment
            // as their titles specifically map to linkTitle if showTitle:false...
            for (var j=0; j<updatedFields.length; j++) {
                var field = updatedFields[j], name = field.name;

                isc.addProperties(field, this[name+"Defaults"], this[name+"Properties"]);

                if (null != this[name + "Title"]) {
                    field.title = this[name + "Title"];
                    if (field.type == 'link' && !field.showTitle)
                        field.linkTitle = this[name + "Title"];
                }

                // Go through some extra contortions so that eg "showMyField" maps to
                // field of name "myField".

                var showField = this["show" + name.substr(0, 1).toUpperCase() +
                    name.substr(1)];
                if (null != showField) field.visible = showField;

                // custom logic needed for some fields
                switch (name) {
                case "registrationItem": field.visible = this.showRegistrationLink; break;
                case "lostPasswordItem": field.visible = this.showLostPasswordLink; break;
                case "loginFailureItem": field.cellStyle = this.errorStyle; break;
                case "passwordItem": field.required = !this.allowBlankPassword; break;
                }
                updatedFields[j] = field;
            }
            ret.dataSource = isc.DataSource.create(this.formDSDefaults,{fields:updatedFields});

            // Note that LoginDialog.init controls initialization of some field attributes,
            // like errorStyle and values, which are controlled from uniquely named
            // LoginDialog attributes rather than <fieldName>Defaults etc.
            return ret;
        }
        return null;
    },
    cancelClick : function () { this.loginForm.complete(true) },
    init : function () {

        if (isc.LoginDialog.firstTimeInit) {
            isc.LoginDialog.firstTimeInit = false;
            isc.LoginDialog.addProperties({
                title: isc.Dialog.LOGIN_TITLE,
                usernameItemTitle: isc.Dialog.USERNAME_TITLE,
                passwordItemTitle: isc.Dialog.PASSWORD_TITLE,
                loginButtonTitle: isc.Dialog.LOGIN_BUTTON_TITLE,
                errorMessage: isc.Dialog.LOGIN_ERROR_MESSAGE
            });
        }
        this.dismissOnEscape = this.showCloseButton = this.dismissable;
        this.Super("init", arguments);
        this.loginForm.focusInItem("usernameItem");
        // handle initial values
        // this functionality was lost in the merge into mainline from 70RC
        if (this.username) this.loginForm.setValue("usernameItem", this.username);
        if (this.password) this.loginForm.setValue("passwordItem", this.password);
    }
});

isc.showLoginDialog = function (loginFunc, properties) {
    return isc.LoginDialog.create(isc.addProperties({}, properties, { autoDraw:true, loginFunc: loginFunc }));
}


// NOTE: unfinished dialog to confirm save when closing / exiting an application, or otherwise
// dropping edits.
// Typical Windows buttons: [*Yes*, No, Cancel]
// Typical Mac buttons: [Don't Save, separator, Cancel, *Save*]
/*
isc.confirmSave = function (message, callback, properties) {
    isc.confirm(message || isc.Dialog.saveChangesMessage, {
                    buttons:[isc.Dialog.OK,
                             {title:"Save", width:75,
                              click:"this.hide();this.topElement.returnValue('save');"},
                             isc.Dialog.CANCEL]
                }
                );
}
*/






//> @object SortSpecifier
// A Javascript object defining the details of a single sort operation.
// <P>
// You can convert between SortSpecifiers and the string format required by
// +link{dsRequest.sortBy} by calling +link{DataSource.getSortBy()} and
// +link{DataSource.getSortSpecifiers()}.
//
// @treeLocation Client Reference/Data Binding
// @visibility external
//<

//> @attr sortSpecifier.property (String : null : IR)
// The property name, eg a +link{ListGridField, field name}, to which this sortSpecifier applies.
//
// @visibility external
//<

//> @attr sortSpecifier.direction (SortDirection : null : IR)
// The direction in which this specifier should sort.
//
// @visibility external
//<

//> @attr sortSpecifier.normalizer (Function : null : IR)
// A normalizer function which this sortSpecifier will use to sort.
//
// @visibility external
//<

//> @attr sortSpecifier.context (DataBoundComponent : null : IR)
// A DataBoundComponent providing the context for the sort-normalizer.
//
// @visibility external
//<


//> @class MultiSortPanel
// A widget that allows the user to set up complex sorting arrangements by defining a group of
// +link{SortSpecifier}s.
// <P>
// Each +link{SortSpecifier} applies to a single property and direction - so, for instance, in
// a grid with two columns, <code>year</code> and <code>monthNumber</code>, you could sort first
// by <code>year</code> in descending order and then by <code>monthNumber</code> in ascending
// order.  This would producing a grid sorted by year from largest (most
// recent) to smallest (least recent) and, within each year, by monthNumber from smallest
// (January) to largest (December).
//
// @treeLocation Client Reference/Data Binding
// @visibility external
//<
isc.defineClass("MultiSortPanel", "Layout");

isc.MultiSortPanel.addProperties({
    vertical: true,
    overflow: "visible",

    //> @attr multiSortPanel.fields (Array of Field : null : IR)
    // The list of fields which the user can choose to sort by.
    // @visibility external
    //<

    // i18n text constants
    //> @attr multiSortPanel.addLevelButtonTitle (String : "Add Level" : IR)
    // The title-text to appear on the addLevelButton
    // @visibility external
    // @group i18nMessages
    //<
    addLevelButtonTitle: "Add Level",
    //> @attr multiSortPanel.deleteLevelButtonTitle (String : "Delete Level" : IR)
    // The title-text to appear on the deleteLevelButton
    // @visibility external
    // @group i18nMessages
    //<
    deleteLevelButtonTitle: "Delete Level",
    //> @attr multiSortPanel.copyLevelButtonTitle (String : "Copy Level" : IR)
    // The title-text to appear on the copyLevelButton
    // @visibility external
    // @group i18nMessages
    //<
    copyLevelButtonTitle: "Copy Level",

    //> @attr multiSortPanel.invalidListPrompt (String : "Columns may only be used once: '\${title}' is used multiple times." : IR)
    // This is a dynamic string - text within <code>\${...}</code> will be evaluated as JS code
    // when the message is displayed.
    // <P>
    // Default value returns <P>
    // <code>
    // <i>Columns may only be used once: <code>[some field's title]</code> is used multiple times</i>
    // </code>
    // @visibility external
    // @group i18nMessages
    //<
    invalidListPrompt: "Columns may only be used once: '\${title}' is used multiple times.",

    //> @attr multiSortPanel.propertyFieldTitle (String : "Column" : IR)
    // The title-text to appear in the header of the "property" field.
    // @visibility external
    // @group i18nMessages
    //<
    propertyFieldTitle: "Column",

    //> @attr multiSortPanel.directionFieldTitle (String : "Order" : IR)
    // The title-text to appear in the header of the "direction" field.
    // @visibility external
    // @group i18nMessages
    //<
    directionFieldTitle: "Order",

    //> @attr multiSortPanel.ascendingTitle (String : "Ascending" : IR)
    // The title-text to appear in the "direction" field's SelectItem for an "ascending" sort
    // @visibility external
    // @group i18nMessages
    //<
    ascendingTitle: "Ascending",
    //> @attr multiSortPanel.descendingTitle (String : "Descending" : IR)
    // The title-text to appear in the "direction" field's SelectItem for a "descending" sort
    // @visibility external
    // @group i18nMessages
    //<
    descendingTitle: "Descending",

    //> @attr multiSortPanel.firstSortLevelTitle (String : "Sort by" : IR)
    // The title-text to appear in the first column for the first sort-level.
    // @visibility external
    // @group i18nMessages
    //<
    firstSortLevelTitle: "Sort by",
    //> @attr multiSortPanel.otherSortLevelTitle (String : "Then by" : IR)
    // The title-text to appear in the first column for all sort-levels other than the first.
    // @visibility external
    // @group i18nMessages
    //<
    otherSortLevelTitle: "Then by",

    topLayoutDefaults: {
        _constructor: "HLayout",
        overflow: "visible",
        height: 22,
        align: "left",
        membersMargin: 5,
        extraSpace: 5
    },

    //> @attr multiSortPanel.addLevelButton (AutoChild IButton : null : RA)
    // Automatically generated +link{class:IButton} providing a mechanism for adding new levels
    // to the sort configuration.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via
    // <code>multiSortPanel.addLevelButtonProperties</code> and
    // <code>multiSortPanel.addLevelButtonDefaults</code>.
    //
    // @visibility external
    //<
    addLevelButtonDefaults: {
        _constructor: "IButton",
        icon: "[SKINIMG]actions/add.png",
        autoFit: true,
        height: 22,
        showDisabled: false,
        autoParent: "topLayout",
        click: "this.creator.addLevel()"
    },

    //> @attr multiSortPanel.deleteLevelButton (AutoChild IButton : null : RA)
    // Automatically generated +link{class:IButton} providing a mechanism for deleting levels
    // from the sort configuration.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via
    // <code>multiSortPanel.deleteLevelButtonProperties</code> and
    // <code>multiSortPanel.deleteLevelButtonDefaults</code>.
    //
    // @visibility external
    //<
    deleteLevelButtonDefaults: {
        _constructor: "IButton",
        icon: "[SKINIMG]actions/remove.png",
        autoFit: true,
        height: 22,
        showDisabled: false,
        autoParent: "topLayout",
        click: "this.creator.deleteSelectedLevel()"
    },

    //> @attr multiSortPanel.copyLevelButton (AutoChild IButton : null : RA)
    // Automatically generated +link{class:IButton} providing a mechanism for duplicating levels
    // in the sort configuration.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via
    // <code>multiSortPanel.copyLevelButtonProperties</code> and
    // <code>multiSortPanel.copyLevelButtonDefaults</code>.
    //
    // @visibility external
    //<
    copyLevelButtonDefaults: {
        _constructor: "IButton",
        icon: "[SKINIMG]RichTextEditor/copy.png",
        autoFit: true,
        height: 22,
        showDisabled: false,
        autoParent: "topLayout",
        click: "this.creator.copySelectedLevel()"
    },

    //> @attr multiSortPanel.levelUpButtonTitle (String : "Move Level Up" : IR)
    // The hover-prompt for the Level Up button.
    // @visibility external
    // @group i18nMessages
    //<
    levelUpButtonTitle: "Move Level Up",

    //> @attr multiSortPanel.levelUpButton (AutoChild ImgButton : null : RA)
    // Automatically generated +link{class:ImgButton} providing a mechanism for moving existing
    // sort-levels up in the sort configuration.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via
    // <code>multiSortPanel.levelUpButtonProperties</code> and
    // <code>multiSortPanel.levelUpButtonDefaults</code>.
    //
    // @visibility external
    //<
    levelUpButtonDefaults: {
        _constructor: "ImgButton",
        src: "[SKINIMG]common/arrow_up.gif",
        height: 22,
        width: 20,
        imageType: "center",
        showDisabled: false,
        showRollOver: false,
        showDown: false,
        showFocused: false,
        autoParent: "topLayout",
        click: "this.creator.moveSelectedLevelUp()"
    },

    //> @attr multiSortPanel.levelDownButtonTitle (String : "Move Level Down" : IR)
    // The hover-prompt for the Level Down button.
    // @visibility external
    // @group i18nMessages
    //<
    levelDownButtonTitle: "Move Level Down",

    //> @attr multiSortPanel.levelDownButton (AutoChild ImgButton : null : RA)
    // Automatically generated +link{class:ImgButton} providing a mechanism for moving existing
    // sort-levels down in the sort configuration.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via
    // <code>multiSortPanel.levelDownButtonProperties</code> and
    // <code>multiSortPanel.levelDownButtonDefaults</code>.
    //
    // @visibility external
    //<
    levelDownButtonDefaults: {
        _constructor: "ImgButton",
        src: "[SKINIMG]common/arrow_down.gif",
        height: 22,
        width: 20,
        imageType: "center",
        showDisabled: false,
        showRollOver: false,
        showDown: false,
        showFocused: false,
        autoParent: "topLayout",
        click: "this.creator.moveSelectedLevelDown()"
    },

    //> @attr multiSortPanel.optionsGrid (AutoChild ListGrid : null : IR)
    // Automatically generated +link{class:ListGrid} allowing the user to configure a set of
    // +link{SortSpecifier}s.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via
    // <code>multiSortPanel.optionsGridProperties</code> and
    // <code>multiSortPanel.optionsGridDefaults</code>.
    //
    // @visibility external
    //<
    optionsGridDefaults: {
        _constructor: "ListGrid",
        width:"100%",
        height: "*",
        canSort: false,
        canReorderFields: false,
        canResizeFields: false,
        canEdit: true,
        canEditNew: true,
        selectionType: "single",
        selectionProperty: "_selection_1",

//        alwaysShowEditors: true,
        fields: [
            { name: "sortSequence", title: "&nbsp;", showTitle: false, canEdit: false, width: 80, canHide: false,
                showDefaultContextMenu: false,
                formatCellValue : function (value, record, rowNum, colNum, grid) {
                    return rowNum == 0 ? grid.creator.firstSortLevelTitle :
                        grid.creator.otherSortLevelTitle;
                }
            },
            { name: "property", title: " ", type: "select",
                defaultToFirstOption: true,
                showDefaultContextMenu: false,
                changed: "item.grid.creator.fireChangeEvent()"
            },
            { name: "direction",  title: " ", type: "select", width: 100,
                showDefaultContextMenu: false,
                defaultToFirstOption: true,
                changed: "item.grid.creator.fireChangeEvent()"
            }
        ],
        selectionUpdated: function (record, recordList) {
            this.creator.setButtonStates();
        },
        bodyKeyPress: function (event) {
            if (event.keyName == "Delete" && this.anySelected()) this.removeSelectedData();
            else this.Super("bodyKeyPress", arguments);
        },
        extraSpace: 5
    },

    propertyFieldNum: 1,
    directionFieldNum: 2,

    topAutoChildren: ["topLayout", "addLevelButton", "deleteLevelButton", "copyLevelButton"]

    //> @attr multiSortPanel.initialSort (Array of SortSpecifier : null : IR)
    // The initial sort configuration to show in the
    // +link{multiSortPanel.optionsGrid, optionsGrid}.
    //
    // @visibility external
    //<

    //> @attr multiSortPanel.maxLevels (number : null : IR)
    // The maximum number of levels of sorting that can be applied.  Since each sort-property or
    // field-name can be used only once in a given multi-sort operation, if no maxLevels value
    // or a value larger than the total number of available properties is specified, it will
    // default to the total number of available properties.
    //
    // @visibility external
    //<

});

isc.MultiSortPanel.addMethods({
    //> @method multiSortPanel.getNumLevels()
    // Return the number of levels of sorting that have been configured.
    //
    // @return (number) The number of levels of sorting that have been configured
    // @visibility external
    //<
    getNumLevels : function () {
        return this.optionsGrid.data.length;
    },

    //> @method multiSortpanel.getSortLevel()
    // Return a +link{SortSpecifier} object for the requested levelNum.
    //
    // @param levelNum (number) The index of the level to return a SortSpecifier for
    // @return (SortSpecifier) A SortSpecifier representing the requested levelNum
    // @visibility external
    //<
    getSortLevel : function (levelNum) {
        return this.getSortSpecifier(this.data.get(levelNum));
    },

    //> @method multiSortPanel.getSort()
    // Returns all configured sorting levels, as an array of +link{SortSpecifier}s.
    //
    // @return (Array of SortSpecifier) the SortSpecifiers for all configured sorting levels
    // @visibility external
    //<
    getSort : function () {
        var grid = this.optionsGrid,
            data = grid.data.duplicate(),
            editRowNum = grid.getEditRow(),
            editValues = isc.isA.Number(editRowNum) ? grid.getEditValues(editRowNum) : null
        ;

        if (editValues) data[editRowNum] = isc.addProperties(data[editRowNum], editValues);
        return this.getSortSpecifiers(data);
    },

    //> @method multiSortPanel.setSort()
    // Sets the sort configuration being displayed after initialization
    //
    // @param sortSpecifiers (Array of SortSpecifier) The sort configuration to set in the +link{optionsGrid}
    //<
    setSort : function (sortSpecifiers) {
        this.optionsGrid.setData(sortSpecifiers);
    },


    //> @method multiSortPanel.validate()
    // Validate that no two +link{SortSpecifier}s sort on the same
    // +link{sortSpecifier.property, property}.
    //
    // @return (boolean) True if validation succeeds, false if any property is used twice
    // @visibility external
    //<
    validate : function () {
        var grid = this.optionsGrid,
            data = grid.data,
            specifiers = []
        ;

        for (var i = 0; i<data.length; i++) {
            var item = data.get(i);
            if (specifiers.contains(item.property)) {
                var _this = this,
                    title = this.optionsGrid.getField("property").valueMap[item.property],
                    message = this.invalidListPrompt.evalDynamicString(this, { title: title });
                isc.warn(message,
                    function () {
                        _this.recordFailedValidation(item, i);
                    }
                );
                return false;
            }
            specifiers.add(item.property);
        }

        return true;
    },

    recordFailedValidation : function (record) {
        var grid = this.optionsGrid,
            recordIndex = (isc.isA.Number(record) ? record : grid.getRecordIndex(record)),
            record = (!isc.isA.Number(record) ? record : grid.data.get(record))
        ;
        grid.selectSingleRecord(record);
        grid.startEditing(recordIndex, 1);
    },

    getSortSpecifier : function (record) {
        if (isc.isA.Number(record)) record = this.optionsGrid.data.get(record);
        return this.optionsGrid.removeSelectionMarkers(record);
    },

    getSortSpecifiers : function (data) {
        return this.optionsGrid.removeSelectionMarkers(data);
    },

    setSortSpecifiers : function (data) {
        this.optionsGrid.setData(data);
    },

    initWidget : function () {
        this.Super("initWidget", arguments);

        // store the maxLevels for use with runtime calls to setFields()
        this._maxLevels = this.maxLevels;

        this.addAutoChildren(this.topAutoChildren);

        this.addAutoChild("levelUpButton", { prompt: this.levelUpButtonTitle });
        this.addAutoChild("levelDownButton", { prompt: this.levelDownButtonTitle });

        this.addAutoChild("optionsGrid");
        this.setSortFields();
        this.setSortDirections();
        this.setButtonTitles();

        this.addMember(this.topLayout);
        this.addMember(this.optionsGrid);

        this.setButtonStates();

        if (this.initialSort) this.setSortSpecifiers(this.initialSort);
        else this.addLevel();
    },

    setButtonTitles : function (enable) {
        if (this.addLevelButton) this.addLevelButton.setTitle(this.addLevelButtonTitle);
        if (this.deleteLevelButton) this.deleteLevelButton.setTitle(this.deleteLevelButtonTitle);
        if (this.copyLevelButton) this.copyLevelButton.setTitle(this.copyLevelButtonTitle);
    },

    setButtonStates : function () {
        var numLevels = this.getNumLevels(),
            maxLevels = this.maxLevels,
            grid = this.optionsGrid,
            anySelected = grid.anySelected(),
            selectedIndex = grid.getRecordIndex(grid.getSelectedRecord())
        ;
        if (this.addLevelButton) this.addLevelButton.setDisabled(numLevels >= maxLevels);
        if (this.deleteLevelButton) this.deleteLevelButton.setDisabled(!anySelected);
        if (this.copyLevelButton) this.copyLevelButton.setDisabled(!anySelected || numLevels >= maxLevels);
        if (this.levelUpButton) this.levelUpButton.setDisabled(!anySelected || selectedIndex == 0);
        if (this.levelDownButton) this.levelDownButton.setDisabled(!anySelected || selectedIndex == numLevels-1);
    },

    // support setting the fields array after creation-time
    setFields : function (fields) {
        if (isc.DataSource && isc.isA.DataSource(fields)) fields = isc.getValues(fields.getFields());
        this.fields = isc.shallowClone(fields);
        this.setSortFields();
        this.optionsGrid.refreshFields();
        this.setButtonStates();
    },

    setSortFields : function () {
        var fields = [];

        this.optionsGrid.getField("property").title = this.propertyFieldTitle;

        if (!this.fields) return;

        // parse the fields array removing any canSort: false fields
        for (var i=0; i<this.fields.length; i++) {
            var field = this.fields[i];
            if (field.canSort != false) fields.add(field);

        }
        this.fields = fields;
        var grid = this.optionsGrid,
            fieldMap = this.fields ? this.fields.getValueMap(grid.fieldIdProperty, "title") :
                { none: "No fields" },
            keyCount = isc.getKeys(fieldMap).length
        ;

        for (var key in fieldMap) {
            // if there's no title, use DS.getAutoTitle() (!value seems to detect empty strings
            // too, but checking it seperately just to be safe)
            if (isc.DataSource && (!fieldMap[key] || isc.isAn.emptyString(fieldMap[key])))
                fieldMap[key] = isc.DataSource.getAutoTitle(key);
        }

        if (this.creator.headerSpans && this.creator.showHeaderSpanTitles) {
            this.applyHeaderSpans(this.creator.headerSpans, fieldMap, "");
        }

        this.optionsGrid.setValueMap("property", fieldMap);
        if (!this._maxLevels || this.maxLevels > keyCount) this.maxLevels = keyCount;
    },

    applyHeaderSpans : function (spans, fieldMap, paramTitle) {
        // This method prepends header span titles to field titles.  This can be important for
        // disambiguation - for example, a grid that shows "Q1" and "Q2" columns under header
        // spans of "North", "South" "East" and "West".  The user sees, eg, "East - Q1" in the
        // list of fields, rather than a bunch of unqualified "Q1" and "Q2" references.
        for (var i = 0; i < spans.length; i++) {
            var title = paramTitle;
            var span = spans[i];
            title += span.title + this.creator.spanTitleSeparator;
            if (span.spans) {
                this.applyHeaderSpans(span.spans, fieldMap, title);
            } else {
                for (var j = 0; j < span.fields.length; j++) {
                    var fieldName = span.fields[j];
                    // skip fields not present in the valueMap (canSort:false)
                    if (fieldMap[fieldName] == null) {
                    } else {
                    fieldMap[fieldName] = title + fieldMap[fieldName];
                }
            }
        }
        }
    },

    setSortDirections : function () {
        this.optionsGrid.getField("direction").title = this.directionFieldTitle;
        this.optionsGrid.getField("direction").valueMap = {
            "ascending" : this.ascendingTitle,
            "descending" : this.descendingTitle
        };
    },

    addLevel : function () {
        var grid = this.optionsGrid,
            selectedIndex = grid.getRecordIndex(grid.getSelectedRecord()),
            columnField = grid.getField("property"),
            orderField = grid.getField("direction"),
            rowNum = selectedIndex >=0 ? selectedIndex+1 : grid.data.length,
            record = {
                property: isc.firstKey(columnField.valueMap),
                direction: isc.firstKey(orderField.valueMap)
            }
        ;

        grid.data.addAt(record, rowNum);
        this.editRecord(rowNum);
        this.setButtonStates();
        this.fireChangeEvent();
    },

    deleteSelectedLevel : function () {
        var grid = this.optionsGrid,
            selectedIndex = grid.getRecordIndex(grid.getSelectedRecord())
        ;
        if (selectedIndex >= 0) {
            grid.data.removeAt(selectedIndex);
            this.setButtonStates();
            this.fireChangeEvent();
        }
    },

    copySelectedLevel : function () {
        var grid = this.optionsGrid,
            selectedIndex = grid.getRecordIndex(grid.getSelectedRecord()),
            record = isc.shallowClone(grid.getRecord(selectedIndex))
        ;
        if (selectedIndex >= 0) {
            grid.data.addAt(record, selectedIndex+1);
            this.editRecord(selectedIndex+1);
            this.setButtonStates();
            this.fireChangeEvent();
        }
    },

    editRecord : function (rowNum) {
        // select and edit a record
        this.optionsGrid.selectSingleRecord(rowNum);
        this.optionsGrid.startEditing(rowNum, this.propertyFieldNum);
    },

    moveSelectedLevelUp : function () {
        var grid = this.optionsGrid,
            selectedIndex = grid.getRecordIndex(grid.getSelectedRecord())
        ;
        if (selectedIndex>0) {
            grid.data.slide(selectedIndex, selectedIndex-1);
            this.fireChangeEvent();
            this.optionsGrid.selectSingleRecord(selectedIndex-1);
        }
    },

    moveSelectedLevelDown : function () {
        var grid = this.optionsGrid,
            selectedIndex = grid.getRecordIndex(grid.getSelectedRecord())
        ;
        if (selectedIndex >= 0 && selectedIndex < grid.data.length-1) {
            grid.data.slide(selectedIndex, selectedIndex+1);
            this.fireChangeEvent();
            this.optionsGrid.selectSingleRecord(selectedIndex+1);
        }
    },

    fireChangeEvent : function () {
        this.sortChanged(isc.shallowClone(this.getSort()));
    },

    //> @method multiSortPanel.sortChanged()
    // Fired whenever the sort configuration changes.  The single parameter is an array of
    // +link{SortSpecifier}s that represent the list of sort-levels as they appear after
    // whatever change has occurred.
    //
    // @param sortLevels (Array of SortSpecifier) The current sort configuration, after any changes
    // @visibility external
    //<
    sortChanged : function (sortLevels) {
    }

});




//> @class MultiSortDialog
// A dialog that allows the user to set up complex sorting arrangements by defining a group of
// +link{SortSpecifier}s.
// <P>
// Each +link{SortSpecifier} applies to a single property and direction - so, for instance, in
// a grid with two columns, <code>year</code> and <code>monthNumber</code>, you could sort first
// by <code>year</code> in descending order and then by <code>monthNumber</code> in ascending
// order.  This would producing a grid sorted by year from largest (most
// recent) to smallest (least recent) and, within each year, by monthNumber from smallest
// (January) to largest (December).
//
// @treeLocation Client Reference/Data Binding
// @visibility external
//<
isc.defineClass("MultiSortDialog", "Window");

isc.MultiSortDialog.addClassMethods({
    //> @classMethod multiSortDialog.askForSort()
    // Launches a MultiSortDialog and obtains a sort-definition from the user.
    //
    // @param fieldSource (Array of Field or DataSource or DataBoundComponent) A source for Fields which the user can choose to sort by
    // @param initialSort (Array of SortSpecifier) The initial sort definition.
    // @param callback (Callback) Called when the user defines and accepts one or more
    // +link{SortSpecifier}s.  Single parameter <code>sortLevels</code> is an Array of
    // SortSpecifier or null if the user cancelled the dialog.
    // @visibility external
    //<
    askForSort : function (fieldSource, initialSort, callback) {
        var fields = isc.isAn.Array(fieldSource) ? fieldSource :
                isc.DataSource && isc.isA.DataSource(fieldSource) ? isc.getValues(fieldSource.getFields()) :
                isc.isA.DataBoundComponent(fieldSource) ? fieldSource.getAllFields() : null
        ;
        if (!fields) return;
        var props = {
            autoDraw:true,
            fields: fields,
            initialSort: initialSort,
            callback: callback
        };
        if (isc.ListGrid && isc.isA.ListGrid(fieldSource) && fieldSource.headerSpans) {
            props.headerSpans = fieldSource.headerSpans;
            props.showHeaderSpanTitles = fieldSource.showHeaderSpanTitlesInSortEditor;
            props.spanTitleSeparator = fieldSource.sortEditorSpanTitleSeparator;
        }
        isc.MultiSortDialog.create(props);

    }
});

isc.MultiSortDialog.addProperties({
    isModal: true,
    width: 500,
    height: 250,
    vertical: true,
    autoCenter: true,
    showMinimizeButton: false,

    mainLayoutDefaults: {
        _constructor: "VLayout",
        width: "100%",
        height: "100%",
        layoutMargin: 5
    },

    multiSortPanelDefaults: {
        _constructor: "MultiSortPanel",
        width: "100%",
        height: "*",
        autoParent: "mainLayout"
    },

    // i18n text constants - passthrough to this.multiSortPanel
    //> @attr multiSortDialog.title (String : "Sort" : IR)
    // The title-text to appear in this Dialog's Header-bar.
    //
    // @visibility external
    // @group i18nMessages
    //<
    title: "Sort",

    //> @attr multiSortDialog.addLevelButtonTitle (String : "Add Level" : IR)
    // The title-text to appear on the addLevelButton.
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the
    // +link{class:MultiSortPanel, MultiSortPanel} contained in this dialog.  You only need to
    // consider the properties on the MultiSortPanel for i18n.
    //
    // @visibility external
    // @group i18nMessages
    //<

    //> @attr multiSortDialog.deleteLevelButtonTitle (String : "Delete Level" : IR)
    // The title-text to appear on the deleteLevelButton
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the
    // +link{class:MultiSortPanel, MultiSortPanel} contained in this dialog.  You only need to
    // consider the properties on the MultiSortPanel for i18n.
    //
    // @visibility external
    // @group i18nMessages
    //<

    //> @attr multiSortDialog.copyLevelButtonTitle (String : "Copy Level" : IR)
    // The title-text to appear on the copyLevelButton
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the
    // +link{class:MultiSortPanel, MultiSortPanel} contained in this dialog.  You only need to
    // consider the properties on the MultiSortPanel for i18n.
    //
    // @visibility external
    // @group i18nMessages
    //<

    //> @attr multiSortDialog.levelUpButtonTitle (String : "Move Level Up" : IR)
    // The hover-prompt for the Level Up button.
    // @visibility external
    // @group i18nMessages
    //<

    //> @attr multiSortDialog.levelDownButtonTitle (String : "Move Level Down" : IR)
    // The hover-prompt for the Level Down button.
    // @visibility external
    // @group i18nMessages
    //<

    //> @attr multiSortDialog.invalidListPrompt (String : "Columns may only be used once: '\${title}' is used multiple times." : IR)
    // This is a dynamic string - text within <code>\${...}</code> will be evaluated as JS code
    // when the message is displayed.
    // <P>
    // Default value returns <P>
    // <code>
    // <i>Columns may only be used once: <code>[some field's title]</code> is used multiple times</i>
    // </code>
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the
    // +link{class:MultiSortPanel, MultiSortPanel} contained in this dialog.  You only need to
    // consider the properties on the MultiSortPanel for i18n.
    //
    // @visibility external
    // @group i18nMessages
    //<

    //> @attr multiSortDialog.propertyFieldTitle (String : "Column" : IR)
    // The title-text to appear in the header of the "property" field.
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the
    // +link{class:MultiSortPanel, MultiSortPanel} contained in this dialog.  You only need to
    // consider the properties on the MultiSortPanel for i18n.
    //
    // @visibility external
    // @group i18nMessages
    //<

    //> @attr multiSortDialog.directionFieldTitle (String : "Order" : IR)
    // The title-text to appear in the header of the "direction" field.
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the
    // +link{class:MultiSortPanel, MultiSortPanel} contained in this dialog.  You only need to
    // consider the properties on the MultiSortPanel for i18n.
    //
    // @visibility external
    // @group i18nMessages
    //<

    //> @attr multiSortDialog.ascendingTitle (String : "Ascending" : IR)
    // The title-text to appear in the "direction" field's SelectItem for an "ascending" sort
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the
    // +link{class:MultiSortPanel, MultiSortPanel} contained in this dialog.  You only need to
    // consider the properties on the MultiSortPanel for i18n.
    //
    // @visibility external
    // @group i18nMessages
    //<

    //> @attr multiSortDialog.descendingTitle (String : "Descending" : IR)
    // The title-text to appear in the "direction" field's SelectItem for a "descending" sort
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the
    // +link{class:MultiSortPanel, MultiSortPanel} contained in this dialog.  You only need to
    // consider the properties on the MultiSortPanel for i18n.
    //
    // @visibility external
    // @group i18nMessages
    //<

    //> @attr multiSortDialog.firstSortLevelTitle (String : "Sort by" : IR)
    // The title-text to appear in the first column for the first sort-level.
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the
    // +link{class:MultiSortPanel, MultiSortPanel} contained in this dialog.  You only need to
    // consider the properties on the MultiSortPanel for i18n.
    //
    // @visibility external
    // @group i18nMessages
    //<

    //> @attr multiSortDialog.otherSortLevelTitle (String : "Then by" : IR)
    // The title-text to appear in the first column for all sort-levels other than the first.
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the
    // +link{class:MultiSortPanel, MultiSortPanel} contained in this dialog.  You only need to
    // consider the properties on the MultiSortPanel for i18n.
    //
    // @visibility external
    // @group i18nMessages
    //<

    //> @attr multiSortDialog.initialSort (Array of SortSpecifier : null : IR)
    // The initial sort configuration to show in the
    // +link{multiSortPanel.optionsGrid, optionsGrid}.
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the
    // +link{class:MultiSortPanel, MultiSortPanel} contained in this dialog.
    //
    // @visibility external
    //<

    //> @attr multiSortDialog.maxLevels (number : null : IR)
    // The maximum number of levels of sorting that can be applied.  Since each sort-property or
    // field-name can be used only once in a given multi-sort operation, if no maxLevels value
    // or a value larger than the total number of available properties is specified, it will
    // default to the total number of available properties.
    // <P>
    // Note, this is a passthrough property which, when set, is passed through to the
    // +link{class:MultiSortPanel, MultiSortPanel} contained in this dialog.
    //
    // @visibility external
    //<


    //> @attr multiSortDialog.applyButtonTitle (String : "Apply" : IR)
    // The title-text to appear on the applyButton
    // @visibility external
    // @group i18nMessages
    //<
    applyButtonTitle: "Apply",
    //> @attr multiSortDialog.cancelButtonTitle (String : "Cancel" : IR)
    // The title-text to appear on the cancelButton
    // @visibility external
    // @group i18nMessages
    //<
    cancelButtonTitle: "Cancel",

    bottomLayoutDefaults: {
        _constructor: "HLayout",
        width: "100%",
        height: 22,
        align: "right",
        membersMargin: 5,
        autoParent: "mainLayout"
    },

    //> @attr multiSortDialog.applyButton (AutoChild IButton : null : RA)
    // Automatically generated +link{class:IButton} providing the mechanism for accepting
    // the current sort configuration.  Fires the passed callback with a single parameter,
    // sortLevels, representing the current sort configuration as an array of
    // +link{SortSpecifier}s.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via
    // <code>multiSortDialog.applyButtonProperties</code> and
    // <code>multiSortDialog.applyButtonDefaults</code>.
    //
    // @visibility external
    //<
    applyButtonDefaults: {
        _constructor: "IButton",
        autoFit: true,
        height: 22,
        autoParent: "bottomLayout",
        click: "this.creator.apply()"
    },

    //> @attr multiSortDialog.cancelButton (AutoChild IButton : null : RA)
    // Automatically generated +link{class:IButton} providing the mechanism for closing this
    // Dialog without accepting the current sort configuration.  The passed callback is fired
    // with a single null parameter, indicating that the operation was cancelled.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via
    // <code>multiSortDialog.cancelButtonProperties</code> and
    // <code>multiSortDialog.cancelButtonDefaults</code>.
    //
    // @visibility external
    //<
    cancelButtonDefaults: {
        _constructor: "IButton",
        autoFit: true,
        height: 22,
        autoParent: "bottomLayout",
        click: "this.creator.cancel()"
    },

    bottomAutoChildren: ["bottomLayout", "applyButton", "cancelButton"]

    //> @attr multiSortDialog.addLevelButton (AutoChild IButton : null : RA)
    // @include multiSortPanel.addLevelButton
    //<

    //> @attr multiSortDialog.deleteLevelButton (AutoChild IButton : null : RA)
    // @include multiSortPanel.deleteLevelButton
    //<

    //> @attr multiSortDialog.copyLevelButton (AutoChild IButton : null : RA)
    // @include multiSortPanel.copyLevelButton
    //<

    //> @attr multiSortDialog.levelUpButton (AutoChild ImgButton : null : RA)
    // @include multiSortPanel.levelUpButton
    //<

    //> @attr multiSortDialog.levelDownButton (AutoChild ImgButton : null : RA)
    // @include multiSortPanel.levelDownButton
    //<

    //> @attr multiSortDialog.fields (Array of Field : null : IR)
    // @include multiSortPanel.fields
    //<

    //> @attr multiSortDialog.optionsGrid (AutoChild ListGrid : null : IR)
    // @include multiSortPanel.optionsGrid
    //<

});

isc.MultiSortDialog.addMethods({
    initWidget : function () {
        this.Super("initWidget", arguments);

        this.addAutoChild("mainLayout");
        this.addAutoChild("multiSortPanel", this.getPassthroughProperties());

        this.addAutoChildren(this.bottomAutoChildren);
        this.addItem(this.mainLayout);
        // grab a local copy of the panel's optionsGrid
        this.optionsGrid = this.multiSortPanel.optionsGrid;
        // setup the button-states
        this.setButtonStates();
    },

    _passthroughs: [ "initialSort", "maxLevels", "invalidListPrompt",
        // autoChildren & i18nMessages
        "addLevelButtonTitle", "addLevelButtonDefaults", "addLevelButtonProperties",
        "deleteLevelButtonTitle", "deleteLevelButtonDefaults", "deleteLevelButtonProperties",
        "levelUpButtonTitle", "levelDownButtonTitle",
        "copyLevelButtonTitle", "copyLevelButtonDefaults", "copyLevelButtonProperties",
        // grid properties and titles
        "optionsGridDefaults", "optionsGridProperties",
        "firstSortLevelTitle", "propertyFieldTitle", "directionFieldTitle",
        "descendingTitle", "ascendingTitle", "otherSortLevelTitle"
    ],

    getPassthroughProperties : function () {
        var propNames = this._passthroughs,
            props = {};

        for (var i = 0; i < propNames.length; i++) {
            var name = propNames[i];
            if (this[name] != null) props[name] = this[name];
        }

        if (this.fields) props.fields = isc.shallowClone(this.fields);

        return props;
    },

    setButtonStates : function () {
        this.multiSortPanel.setButtonStates();
        this.applyButton.setTitle(this.applyButtonTitle);
        this.cancelButton.setTitle(this.cancelButtonTitle);
    },

    //> @method multiSortDialog.getNumLevels()
    // @include multiSortPanel.getNumLevels
    //<
    getNumLevels : function () {
        return this.multiSortPanel.getNumLevels();
    },

    //> @method multiSortDialog.getSortLevel()
    // @include multiSortPanel.getSortLevel
    //<
    getSortLevel : function (levelNum) {
        return this.multiSortPanel.getSortLevel(levelNum);
    },

    //> @method multiSortDialog.getSort()
    // @include multiSortPanel.getSort
    //<
    getSort : function () {
        return this.multiSortPanel.getSort();
    },

    //> @method multiSortDialog.validate()
    // @include multiSortPanel.validate
    //<
    validate : function () {
        return this.multiSortPanel.validate();
    },

    closeClick : function () {
        this.cancel();
        return false;
    },

    cancel : function () {
        if (this.callback)
            this.fireCallback(this.callback, ["sortLevels"], [null]);

        this.hide();
        this.markForDestroy();
    },

    apply : function () {
        // end the current edit, if there is one
        if (this.optionsGrid.getEditRow() != null) this.optionsGrid.endEditing();
        if (!this.validate()) return;
        if (this.callback) {
            // get the array of SortSpecifiers and fire the callback is one was provided
            var specifiers = isc.shallowClone(this.getSort());
            this.fireCallback(this.callback, ["sortLevels"], [specifiers]);
        }
        this.hide();
        this.markForDestroy();
    }

});









//>    @class    TabSet
//
// The TabSet class allows components on several panes to share the same space. The tabs at
// the top can be selected by the user to show each pane.
// <P>
// Tabs are configured via the <code>tabs</code> property, each of which has a
// <code>pane</code> property which will be displayed in the main pane when that tab is
// selected.
//
//  @treeLocation Client Reference/Layout
//  @visibility external
//<

isc.ClassFactory.defineClass("TabSet", "Canvas");

isc.TabSet.addProperties({

    // NOTE: Setting both the paneContainer and TabSet to overflow:"visible" results in an
    // auto-expanding TabSet.  This may be appropriate as a top-level page layout when an
    // application is more web-style than desktop-style, eg, allows and utilizes browser-level
    // scrolling.
    overflow:"hidden",

    // TabBar
    // ----------------------------------------------------------------------------------------
    //>    @attr    tabSet.tabs        (Array of Tab : null : IRW)
    //
    // An array of tab objects, specifying the title and pane contents of each tab in the
    // TabSet.  When developing in JavaScript, tabs are specified as an array of object
    // literals, not instances - see +link{Tab}.
    // <p>
    // You can add and remove tabs after creating the TabSet by calling +link{TabSet.addTab}
    // @visibility external
    // @example tabsOrientation
    //<

    //> @object Tab
    // Tabs are specified as objects, not class instances.  For example, when
    // developing in JavaScript, a typical initialization block for a TabSet would look like
    // this:
    // <pre>
    // TabSet.create({
    //     tabs: [
    //         {title: "tab1", pane: "pane1"},
    //         {title: "tab2"}
    //     ]
    // });
    // </pre>
    // And in XML:
    // <pre>
    // &lt;TabSet&gt;
    //    &lt;tabs&gt;
    //        &lt;Tab title="tab1" pane="pane1"/&gt;
    //        &lt;Tab title="tab2"/&gt;
    //    &lt;/tabs&gt;
    // &lt;/TabSet&gt;
    // </pre>
    //
    // @treeLocation Client Reference/Layout/TabSet
    // @visibility external
    //<

    //> @attr tab.title (HTML : null : IRW)
    //
    // Specifies the title of the this tab.  To change the title after the TabSet has been
    // created, call +link{TabSet.setTabTitle}.
    //
    // @see TabSet.setTabTitle
    // @visibility external
    //<

    //> @attr tab.canEditTitle (boolean : null : IRW)
    //
    // If specified, overrides the +link{TabSet.canEditTabTitles} setting, for this one tab
    // only.
    // <p>
    // Note that the TabSet's +link{TabSet.titleEditEvent,titleEditEvent} must be set to a
    // supported +link{TabTitleEditEvent} in order for users to be able to edit this tab's
    // title.
    //
    // @see TabSet.canEditTabTitles
    // @visibility external
    // @example userEditableTitles
    //<

    //> @attr tab.prompt (string : null : IRW)
    //
    // Specifies the prompt to be displayed when the mouse hovers over the tab.
    // @visibility external
    //<

    //> @attr tab.pickerTitle   (HTML : null : IRW)
    // If +link{tabSet.showTabPicker} is true for this TabSet, if set this property will determine
    // the title of the picker menu item for this tab. If unset, +link{tab.title} will be used
    // instead
    // @see TabSet.showTabPicker
    // @see tab.title
    // @group tabBarControls
    // @visibility external
    //<

    //> @attr tab.pane (ID or Canvas: null : IRW)
    //
    // Specifies the pane associated with this tab.  You have two options for the value of
    // the pane attribute:
    // <ul>
    // <li><b>ID</b> - The global ID of an already created Canvas (or subclass).
    // <li><b>Canvas</b> - A live instance of a Canvas (or subclass).
    // </ul>
    // You can change the pane associated with a given tab after the TabSet has been created by
    // calling +link{TabSet.updateTab}.
    //
    // @see TabSet.updateTab
    // @visibility external
    //<

    //> @attr tab.paneMargin (int : null : IR)
    // Space to leave around the pane within this Tab.
    // If specified, this property takes precedence over +link{tabSet.paneMargin}
    // @visibility external
    //<

    //> @attr tab.ID (identifier : null : IRW)
    // Optional ID for the tab, which can later be used to reference the tab.
    // APIs requiring a reference to a tab will accept the tab's ID
    // [including  +link{tabSet.selectTab()}, +link{tabSet.updateTab()}, +link{tabSet.removeTab()}].<br>
    // The ID will also be passed to the +link{tabSet.tabSelected()} and +link{tabSet.tabDeselected()}
    // handler functions, if specified.
    // <p>
    // Note that if you provide an ID, it must be globally unique.  If you do not want a
    // globally unique identifier, set +link{tab.name} instead.
    //
    // @visibility external
    //<

    //> @attr tab.name (identifier : null : IRW)
    // Optional name for the tab, which can later be used to reference the tab.
    // APIs requiring a reference to a tab will accept the tab's name
    // [including  +link{tabSet.selectTab()}, +link{tabSet.updateTab()}, +link{tabSet.removeTab()}].<br>
    // This name will also be passed to the +link{tabSet.tabSelected()} and +link{tabSet.tabDeselected()}
    // handler functions, if specified.
    // <p>
    // This identifier is requred to be locally unique to the TabSet and cannot be used to get
    // a global reference to the Tab.  If you want a global reference, set +link{tab.ID} instead.
    //
    // @visibility external
    //<

    //> @attr tab.width (number : 100 : IRW)
    // You can specify an explicit width for the tab using this property.  Note that tabs
    // automatically size to make room for the full title, but if you want to e.g. specify a
    // uniform width for all tabs in a TabSet, this property enables you to do so.
    //
    // @visibility external
    //<

    //> @attr tab.disabled (boolean : null : IRW)
    // If specified, this tab will initially be rendered in a disabled state. To enable or
    // disable tabs on the fly use the +link{tabSet.enableTab()}, and +link{tabSet.disableTab()}
    // methods.
    // @visibility external
    //<

    //> @attr tab.icon (SCImgURL : null : IRW)
    // If specified, this tab will show an icon next to the tab title.
    // <p>
    // <b>NOTE:</b> if you enable +link{tabSet.canCloseTabs,closeable tabs},
    // <code>tab.icon</code> is used for the close icon.  +link{tabSet.canCloseTabs} describes
    // a workaround to enable both a <code>closeIcon</code> and a second icon to be shown.
    // <p>
    // Use +link{tabSet.tabIconClick} to add an event handler specifically for clicks on the icon.
    // <p>
    // If a tab +link{tab.disabled,becomes disabled}, a different icon will be loaded by adding
    // a suffix to the image name (see +link{Button.icon}).
    // <p>
    // You should specify a size for the icon via +link{tab.iconSize} or +link{tab.iconWidth}
    // and +link{tab.iconHeight}. Without an explicitly specified size, tabs may be drawn
    // overlapping or with gaps the first time a page is loaded, because the icon is not cached
    // and therefore its size isn't known.
    //
    // @visibility external
    // @example tabsOrientation
    // @see tabSet.tabIconClick
    //<

    //> @attr tab.iconSize (integer : 16 : IRW)
    // If +link{tab.icon} is specified, this property may be used to specify a size for the icon.
    // Per side sizing may be specified instead via +link{tab.iconWidth} and +link{tab.iconHeight}.
    // @visibility external
    //<
    defaultTabIconSize: 16,

    //> @attr tab.iconWidth (integer : null : IRW)
    // If +link{tab.icon} is specified, this property may be used to specify a size for the icon
    // @visibility external
    //<

    //> @attr tab.iconHeight (integer : null : IRW)
    // If +link{tab.icon} is specified, this property may be used to specify a size for the icon
    // @visibility external
    //<

    //> @attr tab.canReorder (Boolean : null : IR)
    // If +link{tabSet.canReorderTabs} is set to <code>true</code>, setting <code>canReorder</code>
    // explicitly to <code>false</code> for some tab will disallow drag-reordering of
    // this tab. Has no effect if <code>canReorderTabs</code> is not true at the tabSet level.
    // <P>
    // Note that this setting also disallows a reorder of another tab into the slot before
    // or following this tab. This means for tabs located at the beginning or end of the
    // tab-bar, users cannot changing the index of the tab by dropping another
    // before or after it. However if you have a <i><code>canReorder:false</code></i>
    // tab which is not at the beginning or end of the tab bar, users can
    // drag reorder other tabs around it which may ultimately change its position.
    // @visibility external
    // @see TabSet.canReorderTabs
    //<

    //> @attr tab.canClose (boolean : null : IRW)
    // Determines whether this tab should show a close icon allowing the user to dismiss the tab
    // by clicking on the close icon directly. The URL for the close icon's image will be derived from
    // +link{tabSet.closeTabIcon} by default, but may be overridden by explicitly specifying
    // +link{tab.closeIcon}.
    // <p>
    // If unset or null, this property is derived from +link{tabSet.canCloseTabs}.
    // <p>
    // Note that setting <code>canClose</code> means that +link{tab.icon} cannot be used,
    // because it's used for the +link{tab.closeIcon,closeIcon} - see
    // +link{tabSet.canCloseTabs} for a workaround.
    // <p>
    // After the TabSet has been created, you can change a tab's canClose property by calling
    // +link{TabSet.setCanCloseTab()}.
    //
    // @visibility external
    // @example closeableTabs
    // @see TabSet.closeClick()
    //<

    //> @attr tab.closeIcon (SCImgURL : null : IRW)
    // Custom src for the close icon for this tab to display if it is closeable.
    // See +link{tab.canClose} and +link{tabSet.canCloseTabs}.
    // @visibility external
    //<

    //> @attr tab.closeIconSize (number : null :IRW)
    // Size of the +link{tab.closeIcon} for this tab. If unspecified the icon will be sized
    // according to +link{tabSet.closeTabIconSize}
    // @visibility external
    //<

    // ---------------------------------------------------------------------------------------

    //> @attr tabSet.tabBar (AutoChild TabBar : null : R)
    // TabBar for this TabSet, an instance of +link{TabBar}.
    // @visibility external
    //<
    // NOTE: tabBar is actually not created via autoChild system, but supports the same
    // defaults.

    //>    @attr tabSet.tabProperties (Tab Properties : null : IR)
    // Properties to apply to all Tabs created by this TabSet.
    // @visibility external
    //<
    tabProperties:{},

    //> @attr tabSet.defaultTabWidth (number : null : IR)
    // If set, is passed as "width" to all tabs when +link{tabBarPosition} is set to
    // <code>"top"</code> or <code>"bottom"</code>.
    // <P>
    // If unset, width will be picked up from
    // the Tab constructor class defaults. Tabs expand to fit their content, so
    // this width acts as a minimum.
    // Setting width:1 will result in tabs that are
    // only as wide as their titles. May be customized by individual
    // +link{group:skinning,skins}.
    // @visibility external
    //<

    //> @attr tabSet.defaultTabHeight (number : null : IR)
    // If set, is passed as "height" to all tabs when +link{tabBarPosition} is set to
    // <code>"left"</code> or <code>"right"</code>.
    // <P>
    // If unset, height will be picked up from
    // the Tab constructor class defaults. Note that tabs expand to fit their content so
    // this height acts as a minimum. May be customized by individual
    // +link{group:skinning,skins}.
    // @visibility external
    //<

    // Simple Tabs
    // ---------------------------------------------------------------------------------------

    //>    @attr    isc.TabSet.useSimpleTabs    (Boolean : false : IRA)
    // Should we use simple button based tabs styled with CSS rather than
    // image based +link{class:ImgTab} tabs?
    // <P>
    // <smartclient>
    // If set to true the +link{tabSet.simpleTabButtonConstructor} will be used and tabs will
    // by styled according to +link{tabSet.simpleTabBaseStyle}.
    // </smartclient>
    // <smartgwt>
    // If set to true tabs will instances of +link{class:Button}, styled according to the
    // +link{tabSet.simpleTabBaseStyle}.
    // </smartgwt>
    // @visibility external
    //<
    //useSimpleTabs:false,

    //> @attr   isc.TabSet.simpleTabBaseStyle   (CSSStyleName : "tabButton" : [IRW])
    //  If this.useSimpleTabs is true, simpleTabBaseClass will be the base style used to
    //  determine the css style to apply to the tabs.<br>
    //  This property will be suffixed with the side on which the tab-bar will appear, followed
    //  by with the tab's state (selected, over, etc), resolving to a className like
    //  "tabButtonTopOver"
    // @visibility external
    //<
    simpleTabBaseStyle:"tabButton",

    // TabBar placement and sizing
    // ---------------------------------------------------------------------------------------

    //> @attr tabSet.tabBarPosition (Side : isc.Canvas.TOP : IR)
    // Which side of the TabSet the TabBar should appear on.
    // @group tabBar
    // @visibility external
    // @example tabsOrientation
    //<
    tabBarPosition:isc.Canvas.TOP,

    //> @attr tabSet.tabBarAlign (Side | Alignment : see below : IR)
    // Alignment of the tabBar.
    // <P>
    // If the +link{tabSet.tabBarPosition, tabBarPosition} is "top" or "bottom", then
    // this attribute may be set to "left", "right" or "center".  The default is "left", or
    // "right" in +link{isc.Page.isRTL,RTL mode}.
    // <P>
    // If the +link{tabSet.tabBarPosition, tabBarPosition} is "left" or "right", then this
    // attribute may be set to "top", "bottom" or "center".  The default is "top".
    //
    // @group tabBar
    // @visibility external
    // @example tabsAlign
    //<


    //> @attr tabSet.tabBarThickness (number : 21 : IRW)
    // Thickness of tabBar, applies to either orientation (specifies height for horizontal,
    // width for vertical orientation).  Note that overriding this value for TabSets that are
    // skinned with images generally means providing new media for the borders.
    // @group tabBar
    // @visibility external
    //<
    tabBarThickness:21,

    // ---------------------------------------------------------------------------------------

    //>    @attr    tabSet.selectedTab        (number : 0 : IRW)
    // Specifies the index of the initially selected tab.
    // @group tabBar
    // @visibility external
    //<
    selectedTab:0,

    // ---------------------------------------------------------------------------------------

    //> @attr tabSet.canCloseTabs (boolean : null : IRW)
    // Should tabs in this tabSet show an icon allowing the user to dismiss the tab by
    // clicking on it directly. May be overridden for individual tabs by setting
    // +link{tab.canClose}.
    // <P>
    // The URL for this icon's image will be derived from  +link{tabSet.closeTabIcon} by
    // default, but may be overridden by explicitly specifying +link{tab.closeIcon}.
    // <P>
    // <b>Note</b>: Currently, tabs can only show a single icon, so a closable tab will show
    // the close icon only even if +link{tab.icon} is set.  To work around this, add the icon
    // as an HTML &lt;img&gt; tag to the +link{tab.title} property, for example:
    // <smartclient>
    // <pre>
    //    title : "&lt;span&gt;" + isc.Canvas.imgHTML("path/to/icon.png") + " Tab Title&lt;/span&gt;"
    // </pre>
    // </smartclient>
    // <smartgwt>
    // <pre>
    //    tab.setTitle("&lt;span&gt;" + Canvas.imgHTML("path/to/icon.png") + " Tab Title&lt;/span&gt;");
    // </pre>
    // </smartgwt>
    //
    // @see TabSet.closeClick()
    // @visibility external
    //<

    //> @attr tabSet.closeTabIcon (SCImgURL : [SKIN]/TabSet/close.png : IR)
    // Default src for the close icon for tabs to display if +link{tabSet.canCloseTabs} is true.
    // @visibility external
    //<
    closeTabIcon:"[SKIN]/TabSet/close.png",

    //> @attr tabSet.closeTabIconSize (int : 16 : IR)
    // Size in pixels of the icon for closing tabs, displayed when +link{canCloseTabs} is true.
    // @visibility external
    //<
    closeTabIconSize:16,

    //> @attr tabSet.canReorderTabs (boolean : null : IR)
    // If true, tabs can be reordered by dragging on them.
    // <P>
    // To disallow drag-reorder of a specific tab, see +link{tab.canReorder}.
    // @group dragdrop
    // @visibility external
    //<

    //> @attr tabSet.showMoreTab (boolean : null : IR)
    // @include tabBar.showMoreTab
    // @visibility external
    //<

    //> @attr tabSet.moreTabCount (number : 5 : IR)
    // @include tabBar.moreTabCount
    // @visibility external
    //<
    moreTabCount:5,

    //> @attr tabSet.moreTabTitle (String : "More" : IR)
    // Title for the "More" tab.
    // @visibility external
    //<
    moreTabTitle:"More",

    //> @attr tabSet.moreTabImage (SCImgURL : "[SKINIMG]/iOS/more.png" : IR)
    // If +link{showMoreTab} is enabled this property determines the image to display on
    // the "More" tab button.
    // @visibility external
    //<
    moreTabImage:"[SKINIMG]/iOS/more.png",

    //> @attr tabSet.moreTab (AutoChild Tab : null : R)
    // +link{object:Tab} to be shown when +link{showMoreTab} is enabled
    // more than +link{moreTabCount} tabs are provided.
    // @visibility external
    //<

    moreTabDefaults: { ariaRole:"tab" },

    //>    @attr tabSet.moreTabProperties (Tab Properties : null : IR)
    // Properties to apply to the "more" tab created by this TabSet.
    // @visibility external
    //<
    moreTabProperties:{},

    //> @attr tabSet.moreTabPane (AutoChild VLayout : null : R)
    // Pane contents for the "more" tab based on a VLayout. Typically contains
    // a +link{NavigationBar} and +link{TableView}.
    // @visibility external
    //<

    //>    @attr tabSet.moreTabPaneProperties (Canvas Properties : null : IR)
    // Properties to apply to the "more" tab's pane created by this TabSet.
    // @visibility external
    //<
    moreTabPaneProperties:{},

    //>    @attr tabSet.moreTabPaneDefaults (Canvas Properties : null : IR)
    // Default properties for the "more" tab's pane.
    // <p>
    // Currently constructs a VLayout with a +link{NavigationBar} and +link{TableView}.
    // @visibility external
    //<
    moreTabPaneDefaults:{
        _constructor: "VLayout",
        width: "100%",
        height: "100%",
        setData : function (newData) {
            this.creator.moreTabPaneTable.setData(newData);
        }
    },

    moreTabPaneNavBarDefaults:{
        _constructor: "NavigationBar",
        controls: ["titleLabel"],
        autoParent: "moreTabPane"
    },

    moreTabPaneTableDefaults:{
        _constructor: "TableView",
        width: "100%",
        height: "100%",
        recordNavigationClick : function (record) {
            this.creator._tabSelected(record.button);
        },
        autoParent: "moreTabPane"
    },

    // -----------------------------------------------------------
    // Tab bar controls

    //> @attr tabSet.tabBarControls       (Array : ["tabScroller", "tabPicker"] : [IRA])
    // This property determines what controls should show up after the tabBar for this TabSet.
    // Standard controls can be included using the strings <code>"tabScroller"</code> and
    // <code>"tabPicker"</code>. These correspond to the +link{TabSet.scroller} and +link{TabSet.tabPicker}
    // AutoChildren, respectively. The <code>"tabScroller"</code> standard control shows two
    // buttons for scrolling through the tabs in order and the <code>"tabPicker"</code> standard
    // control allows tabs to be picked directly from a menu. The standard controls show up only if
    // +link{tabSet.showTabScroller} or +link{tabSet.showTabPicker} is true and there is not
    // enough space available to show all of the tabs in the tabBar.
    // <P>
    // +explorerExample{layout_tabs_custom_controls, This sample} illustrates the usage of this property
    // <P>
    // Additional controls can be included by adding any widget to this array.  Controls will
    // show up in the order in which they are specified.  For example, the following code would
    // add a button in the tabBar area, while preserving the normal behavior of the tabScroller
    // and tabPicker:
    // <smartclient>
    // <pre>
    // isc.TabSet.create({
    //     width:300,
    //     tabs : [
    //         { title: "Tab one" }
    //     ],
    //     tabBarControls : [
    //         isc.ImgButton.create({
    //             src:"[SKINIMG]/actions/add.png",
    //             width:16, height:16,
    //             layoutAlign:"center"
    //         }),
    //         "tabScroller", "tabPicker"
    //     ]
    // });
    // </pre>
    // </smartclient>
    // <smartgwt>
    // <pre>
    //        ImgButton addButton = new ImgButton();
    //        addButton.setSrc("[SKINIMG]/actions/add.png");
    //        addButton.setTitle("Add");
    //        addButton.setWidth(16);
    //        addButton.setHeight(16);
    //        addButton.setAlign(Alignment.CENTER);
    //        TabSet ts = new TabSet();
    //        ts.setWidth(300);
    //        ts.setHeight(32);
    //        ts.setTabs(new Tab("Tab one"));
    //        ts.setTabBarControls(addButton, TabBarControls.TAB_SCROLLER, TabBarControls.TAB_PICKER);
    //        contentLayout.addMember(ts);
    // </pre>
    // </smartgwt>
    // You can also refer to the default tabPicker/tabScroll controls
    // from Component XML:
    // <pre>
    // <TabSet width="300">
    //    <tabBarControls>
    //          <Button title="Custom Button"/>
    //       <value xsi:type="string">tabPicker</value>
    //       <value xsi:type="string">tabScroller</value>
    //       </tabBarControls>
    //    <tabs>
    //       <tab title="Foo"/>
    //       <tab title="Bar"/>
    //    </tabs>
    // </TabSet>
    // </pre>
    //
    // @group tabBarControls
    // @visibility external
    //<
    tabBarControls : ["tabScroller", "tabPicker"],


    //> @attr   tabSet.showTabScroller  (Boolean : true : [IR])
    // If there is not enough space to display all the tab-buttons in this tabSet, should
    // scroller buttons be displayed to allow access to tabs that are clipped?
    // @visibility external
    // @group tabBarControls
    //<
    showTabScroller:true,

    //> @attr   tabSet.showTabPicker    (Boolean : true : [IR])
    // If there is not enough space to display all the tab-buttons in this tabSet, should
    // a drop-down "picker" be displayed to allow selection of tabs that are clipped?
    // @visibility external
    // @group tabBarControls
    //<
    showTabPicker:true,

    //> @attr tabSet.tabBarControlLayout (AutoChild Layout : null : IR)
    // +link{AutoChild} of type +link{Layout} that holds the +link{tabBarControls} as well as
    // the built-in controls such as the +link{showTabPicker,tab picker menu}.
    // @visibility external
    //<
    tabBarControlLayoutConstructor:"Layout",
    tabBarControlLayoutDefaults:{},

    //>Animation
    //> @attr   tabSet.animateTabScrolling  (Boolean : true : [IR])
    // If +link{tabSet.showTabScroller} is true, should tabs be scrolled into view via an
    // animation when the user interacts with the scroller buttons?
    // @visibility external
    // @group tabBarControls
    //<
    animateTabScrolling:true,
    //<Animation

    //> @attr tabSet.scroller (AutoChild StretchImgButton : null : R)
    // A component containing back and forward buttons for scrolling through all of the tabs
    // of the TabSet. The scroller is created automatically when needed and when <code>"tabScroller"</code>
    // is specified in the +link{TabSet.tabBarControls}.
    // <p>
    // By default, the scroller constructor is +link{StretchImgButton}. Note that the scroller
    // +link{StretchImg.items,items} are determined automatically, so any items set in
    // scrollerProperties will be ignored.
    // @group tabBarControls
    // @visibility external
    //<
    // @see TabSet.getScrollerBackImgName()
    // @see TabSet.getScrollerForwardImgName()
    scrollerConstructor:isc.StretchImgButton,
    scrollerDefaults: {
        // set noDoubleClicks - this means if the user clicks repeatedly on the
        // scroller we'll move forward 1 tab for each click rather than appearing
        // to swallow every other click
        noDoubleClicks: true,

        // Disable normal over/down styling as that would style both buttons at once
        showRollOver: false,
        showDown: false,

        mouseMove : function () {
            if (!this.creator.showScrollerRollOver) return;
            var currPart = this.inWhichPart();
            var otherPart = currPart == this.backPartName ? this.forwardPartName : this.backPartName;
            this.setState(isc.StatefulCanvas.STATE_UP, otherPart);
            this.setState(isc.StatefulCanvas.STATE_OVER, currPart);
        },
        mouseOut : function () {
            if (!this.creator.showScrollerRollOver) return;
            this.setState(isc.StatefulCanvas.STATE_UP, this.forwardPartName);
            this.setState(isc.StatefulCanvas.STATE_UP, this.backPartName);
        },
        mouseDown : function () {
            this.clickPart = this.inWhichPart();
            this.setState(isc.StatefulCanvas.STATE_DOWN, this.clickPart);
        },
        mouseUp : function () {
            this.setState(isc.StatefulCanvas.STATE_UP, this.clickPart);
        },
        mouseStillDown : function () {
            this.click();
        },
        click : function () {
            var back = this.clickPart == this.backPartName;

            if (this.isRTL()) back = !back;
            // figure out which part they clicked in and remember it
            if (back) this.creator.scrollBack();
            else this.creator.scrollForward();

            return false;
        }
    },

    //> @attr   tabSet.scrollerButtonSize   (number : 16 : [IR])
    // If +link{tabSet.showTabScroller} is true, this property governs the size of scroller
    // buttons. Applied as the width of buttons if the tabBar is horizontal, or the height
    // if tabBar is vertical. Note that the other dimension is determined by
    // +link{tabBarThickness,this.tabBarThickness}
    // @group tabBarControls
    // @visibility external
    //<
    scrollerButtonSize:16,

    //> @attr tabSet.tabPicker (AutoChild ImgButton : null : R)
    // A button control that allows tabs to be picked directly from a popup menu. The tabPicker
    // is created automatically when needed and when <code>"tabPicker"</code> is specified in
    // the +link{TabSet.tabBarControls}.
    // @group tabBarControls
    // @visibility external
    //<
    // @see TabSet.getTabPickerSrc()
    tabPickerConstructor: isc.ImgButton,
    tabPickerDefaults: {
        showRollOver: false,

        click : function () {
            this.creator.showTabPickerMenu();
        }
    },

    //> @attr   tabSet.pickerButtonSize   (number : 16 : [IR])
    // If +link{tabSet.showTabPicker} is true, this property governs the size of tab-picker
    // button. Applied as the width of buttons if the tabBar is horizontal, or the height
    // if tabBar is vertical. Note that the other dimension is determined by
    // +link{tabBarThickness,this.tabBarThickness}
    // @group tabBarControls
    // @visibility external
    //<
    pickerButtonSize:16,

    //> @attr   tabSet.skinImgDir (string : "images/TabSet/" : [IR])
    // @include Canvas.skinImgDir
    //<
    skinImgDir:"images/TabSet/",

    //> @attr tabSet.symmetricScroller (Boolean : true : [IR])
    // If this TabSet is showing +link{tabSet.showTabScroller,tab scroller buttons}, this property
    // determines whether the +link{tabSet.scrollerHSrc} and +link{tabSet.scrollerVSrc} media
    // will be used for vertical and horizontal tab-bar scroller buttons, or whether separate
    // media should be used for each possible +link{tabSet.tabBarPosition,tabBarPosition} based
    // on the +link{tabSet.scrollerSrc} property for this tabSet.
    // @group tabBarScrolling
    // @visibility external
    //<
    symmetricScroller:true,

    //> @attr   tabSet.scrollerSrc (SCImgURL : "[SKIN]/scroll.gif" : [IR])
    // If this TabSet is showing +link{tabSet.showTabScroller,tab scroller buttons}, and
    // +link{tabSet.symmetricScroller,symmetricScroller} is false, this property governs the base
    // URL for the tab bar back and forward scroller button images.
    // <P>
    // Note that if +link{tabSet.symmetricScroller,symmetricScroller} is true,
    // +link{tabSet.scrollerHSrc} and +link{tabSet.scrollerVSrc} will be used instead.
    // <P>
    // To get the path to the image to display, this base URL will be modified as follows:
    // <ul>
    // <li>If appropriate a state suffix of <code>"Down"</code> or <code>"Disabled"</code> will be
    //     appended.</li>
    // <li>The +link{tabSet.tabBarPosition,tabBarPosition} for this tabSet will be appended.</li>
    // <li>A suffix of <code>"forward"</code> or <code>"back"</code> will be appended for the
    //     forward or backward scrolling button.</li>
    // </ul>
    // For example - if the scrollerSrc is set to <code>"[SKIN]scroll.gif"</code>, the image
    // displayed for the back-scroller button on a tabSet with <code>tabBarPosition</code> set to
    // "top" and <code>symmetricScroller</code> set to false would be one of
    // <code>"[SKIN]scroll_top_back.gif"</code>, <code>"[SKIN]scroll_Down_top_back.gif"</code>,
    // and <code>"[SKIN]scroll_Disabled_top_back.gif"</code>.
    // <P>
    // Note that for best results the media should be sized to match the scroller button sizes,
    // determined by +link{tabSet.tabBarThickness} and +link{tabSet.scrollerButtonSize}.
    // @see tabSet.symmetricScroller
    // @group tabBarScrolling
    // @visibility external
    //<
    scrollerSrc:"[SKIN]/scroll.gif",

    //> @attr   tabSet.scrollerHSrc (SCImgURL :"[SKIN]hscroll.gif" : [IR])
    // If this TabSet is showing +link{tabSet.showTabScroller,tab scroller buttons}, and
    // +link{tabSet.symmetricScroller,symmetricScroller} is true, this property governs the base
    // URL for the tab bar back and forward scroller button images for horizontal tab bars [IE for
    // tab sets with +link{tabSet.tabBarPosition,tabBarPosition} set to "top" or "bottom"].
    // <P>
    // Note that if +link{tabSet.symmetricScroller,symmetricScroller} is false,
    // +link{tabSet.scrollerSrc} will be used instead.
    // <P>
    // To get the path to the image to display, this base URL will be modified as follows:
    // <ul>
    // <li>If appropriate a state suffix of <code>"Down"</code> or <code>"Disabled"</code> will be
    //     appended.</li>
    // <li>A suffix of <code>"forward"</code> or <code>"back"</code> will be appended for the
    //     forward or backward scrolling button.</li>
    // </ul>
    // For example - if the scrollerHSrc is set to <code>"[SKIN]hscroll.gif"</code>, the image
    // displayed for the back-scroller button on a tabSet with <code>tabBarPosition</code> set to
    // "top" and <code>symmetricScroller</code> set to true would be one of
    // <code>"[SKIN]hscroll_back.gif"</code>, <code>"[SKIN]hscroll_Down_back.gif"</code>,
    // and <code>"[SKIN]hscroll_Disabled_back.gif"</code>.
    // <P>
    // Note that for best results the media should be sized to match the scroller button sizes,
    // determined by +link{tabSet.tabBarThickness} and +link{tabSet.scrollerButtonSize}.
    // @see tabSet.symmetricScroller
    // @group tabBarScrolling
    // @visibility external
    //<
    scrollerHSrc:"[SKIN]hscroll.gif",

    //> @attr   tabSet.scrollerVSrc (SCImgURL :"[SKIN]vscroll.gif" : [IR])
    // If this TabSet is showing +link{tabSet.showTabScroller,tab scroller buttons}, and
    // +link{tabSet.symmetricScroller,symmetricScroller} is true, this property governs the base
    // URL for the tab bar back and forward scroller button images for vertical tab bars [IE for
    // tab sets with +link{tabSet.tabBarPosition,tabBarPosition} set to "left" or "right"].
    // <P>
    // Note that if +link{tabSet.symmetricScroller,symmetricScroller} is false,
    // +link{tabSet.scrollerSrc} will be used instead.
    // <P>
    // To get the path to the image to display, this base URL will be modified as follows:
    // <ul>
    // <li>If appropriate a state suffix of <code>"Down"</code> or <code>"Disabled"</code> will be
    //     appended.</li>
    // <li>A suffix of <code>"forward"</code> or <code>"back"</code> will be appended for the
    //     forward or backward scrolling button.</li>
    // </ul>
    // For example - if the scrollerVSrc is set to <code>"[SKIN]vscroll.gif"</code>, the image
    // displayed for the back-scroller button on a tabSet with <code>tabBarPosition</code> set to
    // "left" and <code>symmetricScroller</code> set to true would be one of
    // <code>"[SKIN]vscroll_back.gif"</code>, <code>"[SKIN]vscroll_Down_back.gif"</code>,
    // and <code>"[SKIN]vscroll_Disabled_back.gif"</code>.
    // <P>
    // Note that for best results the media should be sized to match the scroller button sizes,
    // determined by +link{tabSet.tabBarThickness} and +link{tabSet.scrollerButtonSize}.
    // @see tabSet.symmetricScroller
    // @group tabBarScrolling
    // @visibility external
    //<
    scrollerVSrc:"[SKIN]vscroll.gif",

    //> @attr tabSet.showScrollerRollOver (boolean : false : [IR])
    // set this to true to show scroller rollover images when the mouse is over the scroller
    // buttons
    // @group tabBarScrolling
    //<

    //> @attr tabSet.scrollerProperties (Object : null : [IR])
    // Properties set here override those supplied by default when creating
    // the scroller control.
    // @group tabBarScrolling
    //<


    //> @attr tabSet.symmetricPickerButton (Boolean : true : [IR])
    // If this TabSet is showing a +link{tabSet.showTabPicker,tab picker button}, this
    // property determines whether the +link{tabSet.pickerButtonHSrc} and
    // +link{tabSet.pickerButtonVSrc} media will be used for vertical and horizontal tab-bar
    // picker buttons, or whether separate media should be used for each possible
    // +link{tabSet.tabBarPosition,tabBarPosition} based on the +link{tabSet.pickerButtonSrc}
    // property  for this tabSet.
    // @group tabBarScrolling
    // @visibility external
    //<
    symmetricPickerButton:true,

    //> @attr   tabSet.pickerButtonSrc (SCImgURL : "[SKIN]/picker.gif" : [IR])
    // If +link{tabSet.showTabPicker} is true, this property governs the base URL for the picker
    // button image, when +link{tabSet.symmetricPickerButton} is set to false
    // <P>
    // Note that if <code>symmetricPickerButton</code> is true, the +link{tabSet.pickerButtonHSrc}
    // and +link{tabSet.pickerButtonVSrc} properties will be used instead.
    // <P>
    // To get the path to the image to display, this base URL will be modified as follows:
    // <ul>
    // <li>If appropriate a state suffix of <code>"Down"</code> or <code>"Disabled"</code> will be
    //     appended.</li>
    // <li>The +link{tabSet.tabBarPosition,tabBarPosition} for this tabSet will be appended.</li>
    // </ul>
    // @see tabSet.symmetricPickerButton
    // @group tabBarScrolling
    // @visibility external
    //<
    pickerButtonSrc:"[SKIN]/picker.gif",

    //> @attr   tabSet.pickerButtonHSrc (SCImgURL : "[SKIN]hpicker.gif" : [IR])
    // If +link{tabSet.showTabPicker} is true, and +link{tabSet.symmetricPickerButton} is
    // set to true, this property governs the base URL for the picker
    // button image, when displayed in a horizontal tab-bar [IE +link{tabSet.tabBarPosition} is
    // set to <code>"top"</code> or <code>"bottom"</code>].
    // <P>
    // Note that if <code>symmetricPickerButton</code> is false, the +link{tabSet.pickerButtonSrc}
    // property will be used instead.
    // <P>
    // This base URL will have a suffix of <code>"Down"</code> appended when the user holds the
    // mouse down over the button, and <code>"Disabled"</code> if the tabset as a whole is
    // disabled.
    // @see tabSet.symmetricPickerButton
    // @group tabBarScrolling
    // @visibility external
    //<
    pickerButtonHSrc:"[SKIN]hpicker.gif",

    //> @attr   tabSet.pickerButtonVSrc (SCImgURL : "[SKIN]vpicker.gif" : [IR])
    // If +link{tabSet.showTabPicker} is true, and +link{tabSet.symmetricPickerButton} is
    // set to true, this property governs the base URL for the picker
    // button image, when displayed in a verricaL tab-bar [IE +link{tabSet.tabBarPosition} is
    // set to <code>"LEFT"</code> or <code>"right"</code>].
    // <P>
    // Note that if <code>symmetricPickerButton</code> is false, the +link{tabSet.pickerButtonSrc}
    // property will be used instead.
    // <P>
    // This base URL will have a suffix of <code>"Down"</code> appended when the user holds the
    // mouse down over the button, and <code>"Disabled"</code> if the tabset as a whole is
    // disabled.
    // @see tabSet.symmetricPickerButton
    // @group tabBarScrolling
    // @visibility external
    //<
    pickerButtonVSrc:"[SKIN]vpicker.gif",

    // PaneContainer
    // ----------------------------------------------------------------------------------------

    //> @attr tabSet.paneContainer (AutoChild VLayout : null : R)
    // Container where the component specified by +link{tab.pane} is shown.
    // <P>
    // Note: paneContainer and showEdges:true for rounded tabsets: you can enable decorative
    // image-based edges on the paneContainer by setting +link{Canvas.showEdges,showEdges:true}
    // via paneContainerDefaults (to skin all tabsets) or paneContainerProperties (to use
    // edges on one instance).  In this structure, the +link{group:baseLine} should use media
    // that matches the appearance of the decorative edges and fully overlaps the edge of the
    // paneContainer that it is adjacent to.  In the most typical appearance (symmetric edges
    // on all 4 sides), both +link{tabBar.baseLineCapSize} and +link{tabBar.baseLineThickness}
    // match the +link{canvas.edgeSize,edgeSize} set on the paneContainer.  See the
    // load_skin.js file for the "SmartClient" skin for an example of setting all relevant
    // properties.
    // <P>
    // To disable edges for a particular TabSet, which you may want to do for a TabSet that
    // is already within a clearly defined container, configure the paneContainer to show only
    // it's top edge:
    // <pre>
    //      paneContainerProperties : { customEdges:["T"] },
    // </pre>
    // To completely flatten even the top edge of the TabSet:
    // <pre>
    //      paneContainerProperties : { customEdges:["T"] },
    //      tabBarProperties :{ baseLineCapSize:0 },
    // </pre>
    // This "flattens" the baseLine so that only the center image is used.
    //
    // @visibility external
    //<
    // XXX: advice above suboptimal:
    // - in general, the StretchImg baseline is using different media names for the same media.
    //   Could be fixed by passing custom sib.items to the baseline
    // - when we "flatten" as above, the paneContainer is still rendering a top edge and still
    //   using 3 pieces of media, it's just occluded by the baseline.  Ideally, we'd turn the
    //   edges off entirely, but by default this would cause the baseline to actually overlap
    //   widgets show in the paneContainer, so a margin would need to be set in CSS to
    //   compensate - more complicated to explain

    paneContainerConstructor:"PaneContainer",

    //>    @attr    tabSet.paneContainerClassName        (CSSStyleName : null : IRW)
    // CSS style used for the paneContainer.
    // @visibility external
    //<
    paneContainerClassName:"tabSetContainer",

    //>    @attr    tabSet.paneContainerOverflow    (Overflow : isc.Canvas.AUTO : IRWA)
    // Specifies the overflow of the pane container (the component that holds the pane contents
    // for all tabs).  By default this is set to "auto", meaning the pane container will
    // automatically introduce scrolling when the pane contents exceed the TabSet's specified
    // size.
    // <p>
    // For other values and their meaning, see +link{Overflow}
    //
    // @visibility external
    //<
    paneContainerOverflow:isc.Canvas.AUTO,

    //> @method tabSet.setPaneContainerOverflow()
    // Update +link{paneContainerOverflow} after creation.
    //
    // @param newOverflow (Overflow) new overflow setting
    // @visibility external
    //<
    setPaneContainerOverflow : function (newOverflow) {
        this.paneContainerOverflow = newOverflow;
        if (this.paneContainer) this.paneContainer.setOverflow(newOverflow);
    },

    //> @attr tabSet.symmetricEdges (Boolean : true : IR)
    // If this tabSet will +link{tabSet.showPaneContainerEdges,show edges} for the paneContainer,
    // this property determines whether the same edge media will be used regardless of the tab
    // bar position, or whether different media should be used (necessary if the edge appearance is
    // not symmetrical on all sides).
    // <P>
    // If this property is set to false the paneContainer edge image URLs will be prefixed with
    // the tabBarPosition of the tabSet - for example <code>"[SKIN]edge_top_T.gif"</code> rather
    // than just <code>"[SKIN]edge_T.gif"</code>.
    // <P>
    // When <code>symmetricEdges</code> is false, custom edge sizes for the pane container may be
    // specified via +link{tabSet.topEdgeSizes} et al, and custom edge offsets via
    // +link{tabSet.topEdgeOffsets} et al.
    // @visibility external
    //<
    symmetricEdges:true,

    //> @type EdgeSizes
    // Object used to specify custom edge sizes or offsets.
    // Specified as an object where <code>defaultSize</code> will map to the default edge size or
    // offset for the canvas (+link{canvas.edgeSize}, or +link{canvas.edgeOffset} and
    // <code>top</code>, <code>left</code>, <code>right</code> and
    // <code>bottom</code> will map to the
    // +link{edgedCanvas.edgeTop,edgeTop}/+link{edgedCanvas.edgeOffsetTop,edgeOffsetTop},
    // +link{edgedCanvas.edgeLeft,edgeLeft}/+link{edgedCanvas.edgeOffsetLeft,edgeOffsetLeft},
    // +link{edgedCanvas.edgeRight,edgeRight}/+link{edgedCanvas.edgeOffsetRight,edgeOffsetRight},
    // and +link{edgedCanvas.edgeBottom,edgeBottom}/+link{edgedCanvas.edgeOffsetBottom,edgeOffsetBottom}
    // attributes on the paneContainer respectively. Note that not all these properties have to be
    // set - if unset standard edge sizing rules will apply.
    // @visibility external
    //<

    //> @attr tabSet.leftEdgeSizes (EdgeSizes : null : IR)
    // If this tabSet will +link{tabSet.showPaneContainerEdges,show edges} for the paneContainer,
    // and +link{tabSet.symmetricEdges} is set to false, the <code>leftEdgeSizes</code>,
    // <code>rightEdgeSizes</code>, <code>topEdgeSizes</code> and <code>bottomEdgeSizes</code>
    // properties allow the sizes of edges for the paneContainer to be customized depending on
    // the +link{tabSet.tabBarPosition}.
    // <P>
    // The attribute should be specified an +link{type:EdgeSizes,edgeSizes map}, specifying the
    // desired edge sizes where for the appropriate +link{tabSet.tabBarPosition}.
    // @visibility external
    //<

    //> @attr tabSet.topEdgeSizes (EdgeSizes : null : IR)
    // @include tabSet.leftEdgeSizes
    // @visibility external
    //<

    //> @attr tabSet.bottomEdgeSizes (EdgeSizes : null : IR)
    // @include tabSet.leftEdgeSizes
    // @visibility external
    //<

    //> @attr tabSet.rightEdgeSizes (EdgeSizes : null : IR)
    // @include tabSet.leftEdgeSizes
    // @visibility external
    //<

    //> @attr tabSet.leftEdgeOffsets (EdgeSizes : null : IR)
    // If this tabSet will +link{tabSet.showPaneContainerEdges,show edges} for the paneContainer,
    // and +link{tabSet.symmetricEdges} is set to false, the <code>leftEdgeOffsets</code>,
    // <code>rightEdgeOffsets</code>, <code>topEdgeOffsets</code> and <code>bottomEdgeOffsets</code>
    // properties allow the offsets of edges for the paneContainer to be customized depending on
    // the +link{tabSet.tabBarPosition}.
    // <P>
    // The attribute should be specified an +link{type:EdgeSizes,edgeSizes map}, specifying the
    // desired edge offsets where for the appropriate +link{tabSet.tabBarPosition}.
    // @visibility external
    //<

    //> @attr tabSet.rightEdgeOffsets (EdgeSizes : null : IR)
    // @include tabSet.leftEdgeOffsets
    // @visibility external
    //<

    //> @attr tabSet.topEdgeOffsets (EdgeSizes : null : IR)
    // @include tabSet.leftEdgeOffsets
    // @visibility external
    //<

    //> @attr tabSet.bottomEdgeOffsets (EdgeSizes : null : IR)
    // @include tabSet.leftEdgeOffsets
    // @visibility external
    //<

    //>    @attr    tabSet.showPaneContainerEdges (boolean : null : IRWA)
    // Should the paneContainer for this tabset show +link{Canvas.showEdges,edges}.
    //
    // @visibility external
    //<
    // set to null not false by default so we pick up the value from paneContainerDefaults
    // for backCompat (pre 6.1)

    //> @attr tabSet.paneMargin (int : 0 : IR)
    // Space to leave around the panes in our paneContainer
    // <P>
    // Note that this property may be specified on a per-tab basis via +link{tab.paneMargin}.
    // @visibility external
    //<
    //paneMargin:0

    //>    @attr tabSet.canEditTabTitles (Boolean : false : IRW)
    // If true, users can edit the titles of tabs in this TabSet when the
    // +link{titleEditEvent,titleEditEvent} fires.  You can override this behavior per tab
    // with the +link{Tab.canEditTitle} property.
    // <p>
    // Note that this TabSet's +link{TabSet.titleEditEvent,titleEditEvent} must be set to a
    // supported +link{TabTitleEditEvent} in order for users to be able to edit the titles of
    // tabs.
    // @visibility external
    // @example userEditableTitles
    //<

    //>    @attr tabSet.titleEditEvent (TabTitleEditEvent : null : IRW)
    // The event that triggers title editing on this TabSet.
    // @see canEditTabTitles
    // @see Tab.canEditTitle
    // @visibility external
    // @example userEditableTitles
    //<

    //> @type TabTitleEditEvent
    // An event that triggers title editing in a TabSet.
    // @value "click"       Start editing when the user single-clicks a tab title
    // @value "doubleClick" Start editing when the user double-clicks a tab title
    // @visibility external
    //<

    //> @attr tabSet.titleEditor (AutoChild TextItem : null : R)
    // TextItem we use to edit tab titles in this TabSet.  You can override this property
    // using the normal +link{AutoChild} facilities.
    // @see canEditTabTitles
    // @see Tab.canEditTitle
    // @see TabSet.editTabTitle
    // @visibility external
    //<

    // Explicitly call out titleEditorProperties as TextItem config so it gets
    // picked up in SGWT
    //> @attr tabSet.titleEditorProperties (TextItem properties : null : IR)
    // Properties for the auto-generated +link{tabSet.titleEditor}. This is the text item
    // we use to edit tab titles in this tabSet.
    // @see tabSet.titleEditor
    // @see canEditTabTitles
    // @visibility external
    //<

    //>    @attr tabSet.titleEditorLeftOffset (Integer : null : IRW)
    // If set, offsets the tab title editor further in from the left-hand edge of the tab, by
    // the number of pixels set in this property.  Note that the editor is always offset to
    // avoid overlapping the endcaps of the tab; this property is applied on top of that
    // default offset.
    // @see titleEditorRightOffset
    // @see titleEditorTopOffset
    // @visibility external
    //<

    //>    @attr tabSet.titleEditorRightOffset (Integer : null : IRW)
    // If set, offsets the tab title editor further in from the right-hand edge of the tab, by
    // the number of pixels set in this property.  Note that the editor is always offset to
    // avoid overlapping the endcaps of the tab; this property is applied on top of that
    // default offset.
    // @see titleEditorLeftOffset
    // @see titleEditorTopOffset
    // @visibility external
    //<

    //>    @attr tabSet.titleEditorTopOffset (Integer : null : IRW)
    // If set, offsets the tab title editor further down from the top edge of the tab, by the
    // number of pixels set in this property.  You can use this property, together with the
    // left and right offset properties, to fine tune positioning of the editor within or
    // around the tab button.<p>
    // <b>Note:</b> The height of the editor is an attribute of the editor itself, and can be
    // set by specifying a "height" property in +link{titleEditor,titleEditorDefaults}.
    // @see titleEditorLeftOffset
    // @see titleEditorRightOffset
    // @visibility external
    //<

    titleEditorDefaults: {
        name: "title", type: "text",
        showTitle: false
    },

    //> @attr tabSet.useIOSTabs (boolean : null : IRW)
    //
    // Setting this to true turns on a different appearance for tabs, similar to iOS tabs from
    // the "Music" app, where the tab.icon is enlarged and shown as a black and white mask.
    // This mode does not support a clickable icon - clicking the enlarged icon just switches
    // tabs.
    // <P>
    // This attribute only has an effect when +link{TabSet.canCloseTabs, canCloseTabs} is
    // false, and only for Mobile WebKit, by default.
    //
    // @visibility external
    //<
    useIOSTabs: isc.Browser.isWebKit && isc.Browser.isMobile

});

// Have an explicit subclass of Button for tabs when useSimpleTabs is true.
// This allows us to include "pane" in the schema - required for the visual builder.
// It also would allow for simpler skinning customizations.

//> @class SimpleTabButton
// Simple subclass of +link{Button} used for tabs in a +link{TabSet} if +link{tabSet.useSimpleTabs}
// is true. See also +link{tabSet.simpleTabButtonConstructor}.
// @treeLocation Client Reference/Layout/TabSet
// @visibility external
//<
isc.defineClass("SimpleTabButton", "Button");

isc.SimpleTabButton.addProperties({

    // Override the default width of 100 set on button

    width:null,
    height:null,

    setIcon : function (icon) {
        var tabset = this.parentElement ? this.parentElement.parentElement : null;
        if (tabset && !tabset.canCloseTabs && tabset.useIOSTabs) {
            // Make sure a previous icon is replaced
            this.iOSIcon = null;
        }
        this.Super("setIcon", arguments);
    },
    getTitle : function () {
        var tabset = this.parentElement ? this.parentElement.parentElement : null;
        if (tabset && !tabset.canCloseTabs && tabset.useIOSTabs) {
            if (!this.iOSIcon && this.icon) {
                this.iOSIcon = this.icon;
                this.icon = null;
            }
            var imgHTML = (tabset.iOSIcon == null
                ? "<span style='height: 30px'>&nbsp;</span>"
                : isc.Canvas.imgHTML("[SKIN]blank.gif", 30, 30, null,
                      "style='-webkit-mask-box-image: url(" +
                            isc.Page.getImgURL(this.iOSIcon) +
                      ");",
                      null, null)),
                titleHTML = "<span>" + this.title + "</span>"
            ;
            return imgHTML + titleHTML;
        }
        return this.Super("getTitle", arguments);
    },


    //>EditMode
    // needed so that we can autodiscover this method to update the pane.
    setPane : function (pane) {
        this.parentElement.parentElement.updateTab(this, pane);
    },
    // needed to allow a zero-parameter action for selecting a tab
    selectTab : function () {
        this.parentElement.parentElement.selectTab(this);
    }
    //<EditMode

});

isc.TabSet.addMethods({

//> @attr tabSet.simpleTabButtonConstructor (Class : SimpleTabButton : IRA)
// Tab button constructor if +link{tabSet.useSimpleTabs} is true.
// @visibility external
//<
simpleTabButtonConstructor: isc.SimpleTabButton,

//>    @method    tabSet.initWidget()    (A)
// Initialize the TabSet object
//<
initWidget : function () {

    // disallow 'showEdges:true' on tabSets - this is an effect the user essentially never wants
    // as edges would encompass the tab-bar as well as the (rectangular) pane container.

    this.showEdges = false;

    // call the superclass function
    this.Super("initWidget",arguments);

    if (this.tabs == null) this.tabs = [];
    if (this.tabBarDefaults == null) this.tabBarDefaults = {};
    // NOTE: tabInstanceDefaults is old name
    this.tabProperties = this.tabProperties || this.tabInstanceDefaults || {};
    // Set up some dynamic defaults to apply to all tabs (without modifying the
    // tabProperties object directly, which is shared across all TabSets!)
    this.dynamicTabProperties = {};

    var pos = this.tabBarPosition;
    var  vTabs = (pos == "left") || (pos == "right");

    // if tabBarAlign is unset, set default based on tabBarPosition
    if (this.tabBarAlign == null) {
        this.tabBarAlign = (vTabs ? "top"
                            : (this.isRTL() ? "right" : "left"));
    }

    // If this has the 'useSimpleTabs' property set to true, create buttons rather than imgTabs
    // as tabs in the tab bar.  Saves on creating a number of widgets for performance.

    if (this.useSimpleTabs) {
        // also update the styling
        this.tabBarDefaults.buttonConstructor = this.simpleTabButtonConstructor;
        // eg base + "Right" (derived from "right")
        this.dynamicTabProperties.baseStyle = this.simpleTabBaseStyle +
                pos.substring(0,1).toUpperCase() + pos.substring(1);

        var verticalTabs = (this.tabBarPosition == isc.Canvas.LEFT ||
                            this.tabBarPosition == isc.Canvas.RIGHT);

        this.dynamicTabProperties.ariaRole = "tab";
    }

    // defaultTabWidth / Height only apply on the "length" axis of tabs
    // since the thickness is determined by the tab-bar width.
    if (this.defaultTabWidth && !vTabs) {
        this.dynamicTabProperties.width = this.defaultTabWidth;
    }
    if (this.defaultTabHeight && vTabs) {
        this.dynamicTabProperties.height = this.defaultTabHeight;
    }

    if (this.defaultTabIconSize) {
        this.dynamicTabProperties.iconSize = this.defaultTabIconSize;
    }

    this.makeTabBar();

    this.makePaneContainer();

    this.createPanes();
},


tabBarConstructor:isc.TabBar,

//> @attr tabSet.tabBarProperties (TabBar Properties : null : IR)
// This attribute allows developers to specify custom properties for this tabset's
// +link{tabset.tabBar}
//
// @visibility external
//<

//>    @method    tabSet.makeTabBar()    (A)
//    Instantiates a tabBar for this tabSet, and then adds it as a child of
//    the tabSet. starts with tabBarDefaults and adds additional, tabSet-specific properties
// @visibility internal
//<
makeTabBar : function () {
    if (this.tabs == null) return;


    var tabBarIsVertical = (this.tabBarPosition == isc.Canvas.LEFT ||
                            this.tabBarPosition == isc.Canvas.RIGHT),
        align = this.tabBarAlign;


    var tabs = this.tabs.duplicate(),
        undef;
    var tabProperties = isc.addProperties({}, this.tabProperties, this.dynamicTabProperties);
    for (var i = 0; i < tabs.length; i++) {
        for (var j in tabProperties) {
            if (tabs[i][j] === undef) tabs[i][j] = tabProperties[j];
        }
    }

    // assemble tabBar properties
    var tabBarProperties = isc.addProperties({
        // selectTabOnContextClick: we suppress this behavior by default - this is an undocumented
        // flag to allow selection of tabs on context click

        selectTabOnContextClick:this.selectTabOnContextClick,

        ID:this.getID() + "_tabBar",

        // see "fixLayout" method for where this gets updated dynamically at runtime.
        width: (tabBarIsVertical ? this.tabBarThickness : "100%"),
        height: (tabBarIsVertical ? "100%" : this.tabBarThickness),

        // Default the tab bar to having the same accessKey as the tabSet
        accessKey: this.accessKey,

        // If the user has specified a tabIndex for the tabSet, apply it to the tabBar as well
        tabIndex: this.tabIndex,

        // Passes in the user-specified tabs array.
        // This is a simple way for the developer to specify title / size / etc. for each tab
        // Note - we copy the tabs array rather than pointing at the same array.
        // the tabSet should manage the tabs and call the appropriate actions on the tabBar.
        tabs:tabs,

        align:this.tabBarAlign,

        // tabBar is set vertical or not depending on the value of tabBarPosition.
        vertical: tabBarIsVertical ? true : false,

        // the initially selectedTab is passed in.
        selectedTab:this.selectedTab,

        // More tab settings
        showMoreTab:this.showMoreTab,
        moreTabCount:this.moreTabCount,
        moreTab:this.createMoreTab(),
        // When showing a "more" button, allow buttons to be re-selected.
        allowButtonReselect: this.showMoreTab ? true : false,


        // Override buttonSelected() to fire _tabSelected() on this widget
        // Note: this method is only fired on actual selection change - repeated clicks on
        // the buttons should not fire these methods.
        // _tabSelected will handle firing the public tabSelected/tabDeselected handlers
        // as well as hiding/showing panes.
        // Note that standard TabBar buttonSelected/deselected already handles moving deselected
        // tab behind the baseline image, etc.
        buttonSelected : function (button) {

            this.Super("buttonSelected", arguments);

            //call _tabSelected() on this tabSet to trigger any selection actions
            if (this.parentElement != null) {
                this.parentElement._tabSelected(button);
            }
        },

        // notify the tabset if a tab resizes
        childResized : function (child, deltaX, deltaY, reason) {
            this.Super("childResized", arguments);
            // Don't run 'tabResized' if we're in mid layout.

            if (reason == "Overflow on initial draw") {

                return;
            }

            if (this.parentElement != null) {
                this.parentElement._tabResized();
            }
        },

        // Override showContextMenu -- if this event was bubbled up  a right click on one of our tabs,
        // fire the special showTabContextMenu method
        showContextMenu : function () {
            var target = isc.EH.getTarget();
            if (this.getButtons().contains(target)) {
                var tabSet = this.parentElement,
                    tabObj = tabSet.getTabObject(target);
                if (tabSet.showTabContextMenu(tabSet, tabObj) == false) return false;
            }
            return this.Super("showContextMenu", arguments);

        },

        // If drag reordering of tabs is enabled configure the tabbar and
        // trap the notification so we handle the actual reordering
        canReorderItems: this.canReorderTabs,
        reorderOnDrop : !this.canReorderTabs,
        itemDragReordered : function (startPosition, currentPosition) {
            if (this.parentElement != null) {
                this.parentElement.reorderTab(startPosition, currentPosition);
            }
        },

        // other properties
        tabBarPosition:this.tabBarPosition,
        tabBarAlign:this.tabBarAlign,
        autoDraw:false

    }, this.tabBarDefaults, this.tabBarProperties);

    // create tabBar and add as child.  NOTE: make available as this.tabBar as well since it's
    // declared as an autoChild.  For the same reason, add a "creator" property
    tabBarProperties.creator = this;
    this.tabBar = this._tabBar = isc.ClassFactory.newInstance(this.tabBarConstructor, tabBarProperties);
    this.addChild(this._tabBar);
},
// Documented under registerStringMethods
showTabContextMenu:function () {},

createMoreTab : function () {
    if (!this.showMoreTab) return null;

    // Hold onto pane independently of the tab because the pane will change
    // to show tab panes of the selected "more" tab.
    this.moreTabPane = this.createAutoChild("moreTabPane", this.moreTabPaneProperties);
    this.addAutoChild("moreTabPaneNavBar", {title: this.moreTabTitle});
    this.moreTabPaneTable = this.addAutoChild("moreTabPaneTable");

    var moreTab = isc.addProperties({
        title: this.moreTabTitle,
        icon: this.moreTabImage,
        pane: this.moreTabPane,
        // Mark more tab so it can be recognized in the tabbar
        moreTab: true
    }, this.moreTabDefaults, this.moreTabProperties);


    var undef;
    var tabProperties = isc.addProperties({}, this.tabProperties, this.dynamicTabProperties);

    for (var j in tabProperties) {
        if (moreTab[j] === undef) moreTab[j] = tabProperties[j];
    }
    this.moreTab = moreTab;

    return moreTab;
},

rebuildMorePane : function () {
    this.moreTabPane.setData(this.getMorePaneRecords());
},

getMorePaneRecords : function () {
    var tabSet = this,
        records = []
    ;
    for (var i = 0; i < this.tabs.length; i++) {
        var tabButton = this.getTab(this.tabs[i]);
        if (tabButton.isVisible()) continue;
        var tabObject = this.getTabObject(tabButton);

        var icon = (tabObject.icon != null ? isc.Page.getImgURL(tabObject.icon) : null);
        records[records.length] = {
            icon: icon,
            title: tabObject.title,
            pane: tabObject.pane,
            button: tabButton
        };
    }
    return records;
},

// override setAccessKey and setTabIndex to manage the accessKey / tabIndex of the
// tab-bar

setTabIndex : function (index) {
    this.Super("setTabIndex", arguments)

    if (this._tabBar != null) this._tabBar.setTabIndex(index);
},

// setAccessKey()
// apply the accessKey to the tabBar, which will in turn apply it to the focus-tab.
setAccessKey : function (accessKey) {
    this.Super("setAccessKey", arguments);
    if (this._tabBar != null) this._tabBar.setAccessKey(accessKey);
},


//>    @method    tabSet.createPanes()
//      converts any tab.pane object literals to canvii
// @visibility internal
//<
createPanes : function () {
    for (var i = 0; i < this.tabs.length; i++) {
        var tab = this.tabs[i],
            pane = tab.pane
        ;
        if (pane == null) continue;

        tab.pane = this.createPane(pane, tab);

    }
},

//> @attr tabSet.disablePaneWithTab (boolean : true : IRW)
// If true when a tab is enabled or disabled it's pane will also be enabled / disabled.
// @visibility internal
//<

disablePaneWithTab:true,

//>    @method    tabSet.createPane()
//      (Internal method)
//      Given a pane object, create a canvas from it, and prepare it to be made a pane of this
//      object.
//      Creates canvas from properties object.
//      Ensures canvas is deparented / hidden.
//      Returns canvas.
//  @param  pane (object | canvas) object literal / canvas to be made into a pane
// @param tab (object | ImgTab) tab to which the pane is being applied
// @visibility internal
//<
createPane : function (pane, tab) {
    if (pane == null) return pane;

    // handle string name, autoChild, props object
    if (!isc.isA.Canvas(pane)) pane = this.createCanvas(pane);

    if (pane == null) return pane;

    // make sure the pane is hidden before we add it to the pane container - otherwise it will
    // draw before the tab is actually selected
    pane.hide();

    // If the tab is disabled, disable the pane (if appropriate)
    if (this.disablePaneWithTab && tab && tab.disabled) {
        pane.setDisabled(tab.disabled);
    }

    // add the pane as a member to the paneContainer right away.
    //
    // Note: previously we did the addMember in updateTab() and _showTab().  Now we also do it
    // here - the reason is that it immediately establishes the parent-child relationship that
    // the ExampleViewer relies on to correctly render a view.  In the ExampleViewer we scan
    // for top-level Canvases and add them to a view Canvas - if this addMember isn't here,
    // we'll mistakenly add panes declared inline in a TabSet constructor block as top-level
    // canvases.
    //
    // We still must do the addMember in updateTab() and _showTab() because tabSelected() may
    // be overridden to provide a new pane.
    this.paneContainer.addMember(pane);
    pane._containerID = this.ID;
    return pane;
},

makePaneContainer : function () {

    var props = {
            ID: this.getID() + "_paneContainer",
            _generated: false,
            className:this.paneContainerClassName,
            layoutMargin:(this.paneMargin || 0),
            overflow:this.paneContainerOverflow,

            _createEdgedCanvas : function () {
                var edgedCanvas = this.Super("_createEdgedCanvas", arguments);
                edgedCanvas.addMethods({
                    _asymmetricEdgePrefixes:{top:"_top",left:"_left",bottom:"_bottom",right:"_right"},
                    getEdgePrefix : function (edgeName) {
                        var pc = this.eventProxy,
                            tabSet = pc ? pc.creator : null;
                        if (tabSet && !tabSet.symmetricEdges) {
                            return this._asymmetricEdgePrefixes[tabSet.tabBarPosition];
                        }
                    }
                });
                return edgedCanvas;
            }
        };
    // NOTE: these dynamic defaults will override any static defaults defined in
    // this.paneContainerDefaults, (but may be overridden by attributes in
    // this.paneContainerProperties)
    // For back-compat, if showPaneContainerEdges / getPaneContainerCustomEdges() don't have
    // an explicit value, don't apply them to this object so we continue to pick up
    // showEdges/customEdges from the paneContainerDefaults block
    if (this.showPaneContainerEdges != null) props.showEdges = this.showPaneContainerEdges;
    if (this.getPaneContainerEdges && this.getPaneContainerEdges() != null) {
        props.customEdges = this.getPaneContainerEdges();
    }
    // asymmetricEdges needs support for asymmetric edge sizes and offsets

    if (!this.symmetricEdges) {
        var sizes = this[this._asymmetricEdgeSizePropertyMap[this.tabBarPosition]];
        if (sizes && sizes.defaultSize != null) props.edgeSize = sizes.defaultSize;
        if (sizes && sizes.bottom != null) props.edgeBottom = sizes.bottom;
        if (sizes && sizes.top != null) props.edgeTop = sizes.top;
        if (sizes && sizes.left != null) props.edgeLeft = sizes.left;
        if (sizes && sizes.right != null) props.edgeRight = sizes.right;

        var offsets = this[this._asymmetricEdgeOffsetPropertyMap[this.tabBarPosition]];
        if (offsets && offsets.defaultSize != null) props.edgeOffset = offsets.defaultSize;
        if (offsets && offsets.bottom != null) props.edgeOffsetBottom = offsets.bottom;
        if (offsets && offsets.top != null) props.edgeOffsetTop = offsets.top;
        if (offsets && offsets.left != null) props.edgeOffsetLeft = offsets.left;
        if (offsets && offsets.right != null) props.edgeOffsetRight = offsets.right;

    }

    this.addAutoChild("paneContainer", props);
},

// For efficiency avoid assembling asymmetric edge size / offset property names on the fly
_asymmetricEdgeSizePropertyMap : {
    top:"topEdgeSizes", bottom:"bottomEdgeSizes", left:"leftEdgeSizes", right:"rightEdgeSizes"
},
_asymmetricEdgeOffsetPropertyMap : {
    top:"topEdgeOffsets", bottom:"bottomEdgeOffsets", left:"leftEdgeOffsets",
    right:"rightEdgeOffsets"
},

//> @attr tabSet.showPartialEdges (Boolean : false : [IRA])
// If the paneContainer for this tab set is showing +link{Canvas.showEdges,edges}, setting this
// attribute to <code>true</code> will set the paneContainer to show
// +link{canvas.customEdges,customEdges} for the three sides opposing the tabBarPosition.
// @visibility external
//<

//>    @method tabSet.getPaneContainerEdges() [A]
// If the paneContainer for this tab set is showing +link{Canvas.showEdges,edges}, this
// method can be used to specify (dynamically) which +link{canvas.customEdges,customEdges} to
// show. Called when the pane creator is created.
// <P>
// Default implementation will return null unless +link{tabSet.showPartialEdges,showPartialEdges}
// is true, in which case it will return the three edges opposite the
// +link{tabSet.tabBarPosition,tabBarPosition}.
// @return (array) array of custom edges to show
// @visibility external
//<
getPaneContainerEdges : function () {
    if (this.showPartialEdges) {
                if (this.tabBarPosition == "bottom") return ["T","L","R"];
                else if (this.tabBarPosition == "left") return ["T","B","R"];
                else if (this.tabBarPosition == "right") return ["T","B","L"];
                else return ["B","L","R"];
    }
    return null;
},

// override draw to make sure we have a tab selected, and to fire 'tabSelected()' on the tab
draw : function (a,b,c,d) {
    if (this.tabs && this.tabs.length > 0) {
        var selectedTab = this.getSelectedTabNumber();
        // Don't allow a bad selectedTab value to persist.
        if (!isc.isA.Number(selectedTab) || selectedTab < 0) selectedTab = this.selectedTab = 0;
        // Ensure it's selected in the tab-bar - will no op if already selected, otherwise
        // will perform selection and fire our handlers
        this._tabBar.selectTab(selectedTab);
    }
    this.invokeSuper(isc.TabSet, "draw", a,b,c,d);
    this.fixLayout();
},

//>    @method    tabSet.setTabTitle()    (A)
// Changes the title of a tab
// @param    tab      (Tab | number | ID | name)
// @param    title    (HTML)  new title
// @visibility external
// @example titleChange
//<
setTabTitle : function (tab, title) {
    this.getTabObject(tab).title = title;
    this.getTab(tab).setTitle(title);
    // reset the menu to pick up the new title
    this.resetTabPickerMenu();
},

//>    @method    tabSet.setTabIcon() (A)
// Changes the icon for a tab
// @param tab (Tab | number | ID | name) tab to update
// @param icon (SCImgURL) new icon
// @visibility external
//<
setTabIcon : function (tab, icon) {
    this.setTabProperties(tab, {icon:icon});
},

//>@method tabSet.enableTab()
// If the specified tab is disabled, enable it now.
// @param   tab (Tab | number | ID | name)
// @see tab.disabled
// @visibility external
//<
enableTab : function (tab) {
    this.setTabDisabled(tab, false);
},

//>@method tabSet.disableTab()
// If the specified tab is enabled, disable it now.
// @param   tab (Tab | number | ID | name)
// @see tab.disabled
// @visibility external
//<
disableTab : function (tab) {
    this.setTabDisabled(tab, true);
},

//>@method tabSet.setTabProperties() (A)
// Apply properties to an existing tab in a tabSet.
// @param tab (Tab | number | ID | name) Identifier for the tab to be modified
// @param properties (object) Javascript object containing the set of properties to be applied
//  to the tab.
// @visibility external
//<
setTabProperties : function (tab, properties) {
    if (!properties) return;

    if (properties.ID != null) {
        this.logWarn("setTabProperties(): Unable to modify ID for an existing tab - ignoring " +
                    "this property");
        delete properties.ID;
    }

    // A couple of properties require special APIs
    if (properties.pane != null) {
        this.updateTab(tab, properties.pane);
        delete properties.pane;
    }
    if (properties.disabled != null) {
        this.setTabDisabled(tab, properties.disabled);
        delete properties.disabled;
    }

    var tabObject = this.getTabObject(tab),
        tab = this.getTab(tab);
    if (!tabObject) return;
    isc.addProperties(tabObject, properties);

    if (tab) {
        tab.setProperties(properties);
    }

    // If we have a pickerMenu, destroy it so it gets rebuilt when next required
    // Ensures we pick up title / icon etc changes
    this.resetTabPickerMenu();
},

// Actually set the disabled property on a tab. Handled by just disabling the button.
setTabDisabled : function (tab, disabled) {
    var tabObject = this.getTabObject(tab);
    if (tabObject) tabObject.disabled = disabled;

    var tab = this.getTab(tab);
    if (tab) {
        // disable the tab so you can't access it.
        tab.setDisabled(disabled);
        // Also disable the pane in case it's showing.
        // Alternative approach would be to deselect the tab, if selected. The problem with
        // this is we may only have one tab in the tabSet.
        var pane = tab.pane;
        if (pane && this.disablePaneWithTab) {
            if (isc.isA.Canvas(pane)) pane.setDisabled(disabled);
            else pane.disabled = disabled;
        }
    }
    // rebuild the picker menu so the item in question shows up disabled
    this.resetTabPickerMenu();
},

//>    @method    tabSet.addTab()    (A)
// Add a tab
// @param    tab      (Tab)   new tab
// @param    [position] (number)  position where tab should be added
// @see TabSet.addTabs
// @visibility external
// @example tabsAddAndRemove
//<
addTab : function (tab, position) {
    return this.addTabs(tab, position);
},

//>    @method    tabSet.addTabs()    (A)
// Add one or more tabs
// @param    tabs      (Tab or Array of Tab)   new tab or tabs
// @param    position (number)  position where tab should be added (or array of positions)
// @see TabSet.addTab
// @visibility external
//<
addTabs : function (newTabs, position) {
    if (!isc.isAn.Array(newTabs)) newTabs = [newTabs];
    var oldSelectedTab = this.getTabObject(this.getSelectedTabNumber()),
        forceSelection = (this.getSelectedTabNumber() == -1);

    if (position == null || position > this.tabs.length) position = this.tabs.length;
    for (var i = 0; i < newTabs.length; i++) {
        // use 'createPane' to turn the pane into a hidden, deparented canvas.
        newTabs[i].pane = this.createPane(newTabs[i].pane, newTabs[i]);

        // apply tabProperties (see comment in makeTabBar)
        var undef;
        var tabProperties = isc.addProperties({}, this.tabProperties, this.dynamicTabProperties);

        for (var propName in tabProperties) {
            if (newTabs[i][propName] === undef) {
                newTabs[i][propName] = tabProperties[propName];
            }
        }

        // Actually add the tab to the config
        this.tabs.addAt(newTabs[i], (position + i))
    }
    this._tabBar.addTabs(newTabs, position);

    // If we have a pickerMenu, destroy it so it gets rebuilt when next required
    this.resetTabPickerMenu();

    // call fixLayout on a delay
    // Necessary in case the new tabs introduced clipping of the tab-bar
    // Delay required as layout reflow is asynch
    this.delayCall("fixLayout");

    if (forceSelection) {
        // If we didn't have a selected tab at the start of this method, ensure we select the
        // first of the new tabs
        this.selectTab(0);
    } else {
        // otherwise, update this.selectedTab (an index) in case tabs were added before the old
        // selected tab
        this.selectedTab = this.getTabNumber(oldSelectedTab);
    }

    //>EditMode
    this.addTabsEditModeExtras(newTabs);
    //<EditMode


    return position;
},

//> @method tabSet.setTabPane()
// Apply a new +link{tab.pane,pane} to an existing tab in this tabSet
// @param tab (number | string | Tab) Tab to update (may be referenced by ID or index)
// @param pane (Canvas) new Pane for the tab
// @visibility external
//<
setTabPane : function (tab, pane) {
    return this.updateTab(tab,pane);
},

//> @attr tabSet.destroyPanes (boolean : null : IR)
// Whether +link{canvas.destroy,destroy()} should be called on +link{tab.pane} when it a tab is
// removed via +link{removeTab()}}.
// <P>
// An application might set this to false in order to re-use panes in different tabs or in
// different parts of the application.
//
// @visibility external
//<

//>    @method    tabSet.removeTab()    (A)
// Remove a tab.
// <P>
// The pane associated with the removed tab is automatically destroyed when you
// call this method.  To avoid this, call +link{updateTab()} with <code>null</code> as the new
// pane immediately before removing the tab, or set +link{tabSet.destroyPanes} to false.
//
// @param    tabs      (Tab | ID | name | number | Array of Tab)  list of tabs, tabIDs, or tab numbers
//
// @see TabSet.removeTabs
// @visibility external
// @example tabsAddAndRemove
//<
removeTab : function (tab, dontDestroy) {
    return this.removeTabs(tab, dontDestroy);
},

//>    @method    tabSet.removeTabs()    (A)
// Remove one or more tabs.  The pane(s) associated with the removed tab(s) is automatically
// destroyed when you call this method.
//
// @param    tabs      (Tab | ID | name | number)   list of tabs, tabIDs, tab names, or tab numbers
//
// @see TabSet.removeTab
// @visibility external
//<
removeTabs : function (tabs, dontDestroy) {
    if (!isc.isAn.Array(tabs)) tabs = [tabs];

    // get the actual tab button object from whatever was passed in.
    // We can pass this to tabBar.removeTabs()
    tabs = this.map("getTab", tabs);

    var removedSelected = false,
        selectedTab = this.getSelectedTab(),
        autoSelectTab = 0;

    for (var i = 0; i < tabs.length; i++) {

        // remove the tab from the config
        var tab = tabs[i],
            index = this.getTabNumber(tab)
        ;
        if (index == -1) continue; // can't find specified tab

        var tabObject = this.tabs[index];

        // if we remove the selected tab we want to just select another one near it
        if (tabObject == selectedTab) {
            removedSelected = true;
            // auto-select the next tab to the left if there is one, or the current
            // index otherwise
            if (index > 0) autoSelectTab = index - 1;
            else if (index < this.tabs.length + 1) autoSelectTab = index;

        // otherwise we may need to update our internal 'selectedTab' index value
        // to reflect the new position of the already selected tab
        } else {
            if (index < this.selectedTab) {
                this.selectedTab -= 1;
            }
        }

        this.tabs.removeAt(index);

        if (tabObject) {
            // remove the pane
            var pane = tabObject.pane;
            if (pane && pane.parentElement == this.paneContainer) {
                this.paneContainer.removeChild(pane);
                if (!dontDestroy && this.destroyPanes !== false) {
                    pane.destroy();
                }
            }
        }
        // remove the tab button
        this._tabBar.removeTabs(tab);
    }

    // if the selected tab was removed, select the first tab if we have any
    if (removedSelected && this.tabs.length > 0) {
        // if the new selected-tab index is beyond the tab-count, select the last tab
        if (autoSelectTab >= this.tabs.length) autoSelectTab = this.tabs.length - 1;
        this.selectTab(autoSelectTab);
    }
    // If we have a pickerMenu, destroy it so it gets rebuilt when next required
    this.resetTabPickerMenu();

    // call fixLayout on a delay
    // Necessary in case the removed tabs get rid of clipping of the tab-bar
    // Delay required as layout reflow is asynch
    this.delayCall("fixLayout", 0);

    //>EditMode
    this.removeTabsEditModeExtras();
    //<EditMode

},

//>    @method    tabSet.reorderTab()
// Move a tab to another location in the tabset.
// @param tab (Tab | ID | name | number) tab to move
// @param [moveToPosition] (number) the index to move the tab to - defaults to the end of the
//                                  tabset if not passed
// @visibility external
//<
reorderTab : function (tab, moveToPosition) {
    if (moveToPosition == null || moveToPosition > this.tabs.length) moveToPosition = this.tabs.length;

    var tab = this.getTab(tab);
    if (tab) {
        var index = this.getTabNumber(tab);
        if (index == moveToPosition) return;

        var tabObject = this.getTabObject(tab),
            selectedTab = this.getSelectedTab()
        ;

        // Move the tab button
        this._tabBar.reorderTab(tab, moveToPosition);

        // Resync our matching tab list and selected tab
        this.tabs.removeAt(index);
        this.tabs.addAt(tabObject, moveToPosition);
        if (this.selectedTab == index) {
            this.selectedTab = moveToPosition;
        } else if (index < this.selectedTab && this.selectedTab <= moveToPosition) {
            this.selectedTab--;
        } else if (index > this.selectedTab && this.selectedTab >= moveToPosition) {
            this.selectedTab++;
        }

        // If we have a pickerMenu, destroy it so it gets rebuilt when next required
        this.resetTabPickerMenu();

        // call fixLayout on a delay
        // Necessary in case the new tabs introduced clipping of the tab-bar
        // Delay required as layout reflow is asynch
        this.delayCall("fixLayout");

        //>EditMode
        this.reorderTabsEditModeExtras(index, moveToPosition);
        //<EditMode

        this.tabsReordered();
    }

},

//> @method tabSet.canCloseTab()
// Returns true if this tab is closeable. Determined by checking +link{tab.canClose} and
// +link{tabSet.canCloseTabs}.
// @param tab (int | ID | name | Tab) tab to check
// @return (boolean) true if tab is closeable
//<
canCloseTab : function (tab) {
    tab = this.getTabObject(tab);
    if (tab && tab.canClose != null) return tab.canClose;
    return !!this.canCloseTabs;
},

//> @method tabSet.setCanCloseTab()
// Sets the given tab's +link{tab.canClose,canClose} property to the boolean parameter canClose.
// If canClose is null, this will have the effect of causing the tab to fall back on +link{tabSet.canCloseTabs}.
// @param tab (Tab | ID | name | number) tab to change
// @param canClose (boolean) new value for the tab's canClose property, or null to clear it
// @visibility external
//<
setCanCloseTab : function (tab, canClose) {
    tab = this.getTabObject(tab);
    var liveTab = this.getTab(tab);
    tab.canClose = canClose;
    if (liveTab) {
        liveTab.setProperties(this.getTabBar().getCloseIconProperties(tab, this.canCloseTab(tab)));
    }
},

//> @method tabSet.setCanCloseTabs()
// Changes this TabSet's +link{TabSet.canCloseTabs,canCloseTabs} property.
// @param canCloseTabs (boolean) the new value for canCloseTabs.
// @visibility external
//<
setCanCloseTabs : function (canCloseTabs) {
    canCloseTabs = !!canCloseTabs;
    this.canCloseTabs = canCloseTabs;

    var tabs = this.tabs;
    if (!tabs) return;

    // Go through each tab, updating the tab buttons whose corresponding tab object has an
    // unspecified or null canClose property.
    var tb = this.getTabBar();
    for (var i = 0, len = tabs.length; i < len; ++i) {
        var tab = tabs[i];
        if (tab.canClose != null) continue;

        var liveTab = this.getTab(tab);
        if (liveTab) {
            liveTab.setProperties(tb.getCloseIconProperties(tab, canCloseTabs));
        }
    }
},

setCanReorderTabs : function (canReorderTabs) {
    this.canReorderTabs = canReorderTabs;
    this.tabBar.canReorderItems = canReorderTabs;
    this.tabBar.reorderOnDrop = !canReorderTabs;
},

_tabIconClick : function(tab) {
    var shouldClose = this.canCloseTab(tab);
    if (shouldClose) {
        this.closeClick(tab);
        return false;
    } else return this.tabIconClick(tab);

},

//> @method tabSet.closeClick()
// When +link{canCloseTabs} is set, method fired when the user clicks the "close" icon for a
// tab.
// <P>
// Default implementation will remove the tab from the tabSet via +link{removeTab()}.
//
// @param tab (Tab) tab to close
// @visibility external
//<
closeClick : function (tab) {
    // if "onCloseClick" exists, allow it to cancel the default behavior

    if (this.onCloseClick && (this.onCloseClick(tab) == false)) {
        return;
    }
    this.removeTab(tab);
},

//> @method tabSet.tabIconClick()
// Method fired when the user clicks the icon for a tab, as specified via +link{tab.icon}.
// <P>
// Default behavior will fire <code>icon.click()</code> if specified, with two parameters
// <code>tab</code> (a pointer to the tab object and <code>tabSet</code> a pointer to the tabSet
// instance.
// @param tab (Tab) with click handler being fired
// @visibility external
//<
tabIconClick : function (tab) {
    var icon = tab.icon;
    if (icon && icon.click) return this.fireCallback(icon.click, 'tab,tabSet', [tab,this]);
},

//> @method tabSet.getTabObject()
// Get the tab Object originally passed to +link{tabSet.tabs}, by index, name or ID.
// If passed a tab Object, just returns it.
//
// @param    tab   (int | ID | name | Tab)
// @return (Tab) the tab, or null if not found
// @visibility external
//<
// NOTE: this returns the tab configuration object, not the button, since there may not be a
// Button.
getTabObject : function (tab) {
    // passed the tab button - determine it's index (use this below)
    tab = this.getTabNumber(tab);
    if (tab >= this.tabs.length) {
        var button = this.tabBar.getButton(tab);
        if (button && button.moreTab) return this.moreTab;
    }
    return this.tabs[tab];
},

//> @method tabSet.getTab()
// Get the live Canvas representing a tab by index, ID, reference, or name.
// If passed a tab Canvas, just returns it.
// <P>
// Note that live Tab instances are not available until +link{Canvas.draw,draw()}.
// <P>
// The returned Tab is considered an internal component of the TabSet.  In order to maximize
// forward compatibility, manipulate tabs through APIs such as a +link{setTabTitle()} instead.
// Also note that a super-lightweight TabSet implementation may not use a separate Canvas per
// Tab, and code that accesses an manipulates Tabs as Canvases won't be compatible with that
// implementation.
//
// @param    tab   (int | ID | name | Canvas)
// @return (Tab) the tab Canvas, or null if not found or TabSet not drawn yet
//
// @visibility external
//<
getTab : function (tab) {

    // already the tab button, return it
    if (isc.isAn.Canvas(tab)) return tab;

    if (!this.tabs) return null;

    // if we have a tab-config block, convert it to an index, since the tabBar doesn't see our
    // 'tabs' array
    if (this.tabs.contains(tab)) tab = this.tabs.indexOf(tab);

    // getButton on the tabBar handles the various possible types of the tab identifier passed in
    tab = this.getTabBar().getButton(tab);

    return tab;
},

//> @method tabSet.getTabPane()
// Returns the pane for a given tab.
//
// @param    tab   (object | number | ID | name | Tab)
// @return (Canvas) the tab pane
// @visibility external
//<
getTabPane : function (tab) {
    return this.getTabObject(tab).pane;
},

//> @method tabSet.findTab()
// Returns a the first tab in the list that matches the user-passed property name/value pair.
//
// @param    propertyName   (String) name of the property to look for
// @param    propertyValue  (Any) value of the property
//<
findTabObject : function (propertyName, propertyValue) {
    return this.tabs.find(propertyName, propertyValue);
},

//> @method tabSet.getTabNumber()
// Get the index of a tab, from the tab, tab ID or tab name.  If passed a number, just returns it.
// @param    tab   (number | ID | name | tab)
// @return (number) the index of the tab, or -1 if not found
// @visibility external
//<
// Note - we don't call this 'getTabIndex', even though it is an index, because of the conflict
// with the 'tabIndex' of the widget as a whole
getTabNumber : function (tab) {
    if (isc.isA.Number(tab)) return tab;
    if (!this.tabs) return null;
    var index = this.tabs.indexOf(tab);
    if (index != -1) return index;


    if (isc.isA.String(tab)) {
        var index = this.tabs.findIndex("name", tab);
        if (index == -1) index = this.tabs.findIndex("ID", tab);
        return index;
    }

    // At this point it must be a pointer to the tab button, so fall through to
    // tabBar.getButtonNumber()
    return this.getTabBar().getButtonNumber(this.getTab(tab));
},

//> @method tabSet.updateTab()
// Set the pane for a tab.
// <P>
// Pass in the index of a tab (or a tab object), and a new pane.
// <P>
// NOTE: the old pane for the tab is not destroy()d
//
// @param    tab   (number | ID | name | Tab) tab to update
// @param    pane  (Canvas | ID) new pane for the tab
// @visibility external
//<
updateTab : function (tab, pane) {
    // if we were passed a tab init block, for a new tab, call addTabs instead
    if (isc.isAn.Object(tab) && !isc.isA.Canvas(tab) &&
        this.tabs.indexOf(tab) == -1)
    {
        if (pane != null) tab.pane = pane;
        return this.addTabs(tab);
    }

    // get the index for the tab (whatever way the "tab" is passed)
    var tabIndex = this.getTabNumber(tab);
    // bad tab specification
    if (tabIndex == -1) {
        this.logWarn("no such tab: " + this.echo(tab));
        return;
    }

    // get rid of the old pane
    var tabObject = this.getTabObject(tabIndex),
        oldPane = tabObject ? tabObject.pane : null;

    if (tabObject && tabObject.pane == pane) return; // no-op

    if (oldPane != null) {
        oldPane.hide();
        oldPane.deparent();
    }

    // NOTE: keep tabCanvas.pane and tabObject.pane in sync for EditMode where the Tab needs to
    // be able to respond to getProperty("pane")
    var tabCanvas = this.getTab(tab);

    // if the new pane is null, we're done
    if (pane == null) {
        if (tabCanvas != null) tabCanvas.pane = null;
        return tabObject.pane = null;
    }

    // add the new pane to init block (Using createPane to instantiate as a Canvas if necessary)
    // this makes sure the pane is hidden and not a child of anything except the paneContainer
    pane = tabObject.pane = this.createPane(pane, tabObject);

    // tabCanvas won't exist if we're not drawn yet
    if (tabCanvas != null) tabCanvas.pane = pane;

    // if the currently visible tab is being updated, ensure the new pane is
    // a member of the paneContainer with the appropriate visibility
    // (If undrawn it'll show up when the tabSet as a whole gets drawn)
    if (this.getSelectedTabNumber() == tabIndex) {
        if (!this.paneContainer.hasMember(pane)) this.paneContainer.addMember(pane);
        pane.setVisibility(isc.Canvas.INHERIT);
    }
},

//>    @method    tabSet.fixLayout()    (A)
//            lay out the children of the tabSet.
//            this method takes into account the position of the tabBar in the tabSet,
//            and lays out the tabBar and the paneContainer accordingly.
//<
fixLayout : function () {
    // abbreviations
    var tb = this._tabBar,
        // round corners: for layout only, manipulate the edgedCanvas instead of the
        // paneContainer
        pc = this._edgedCanvas || this.paneContainer
    ;

    // check for nulls, and exit if found.
    // this method requires that both the tabBar and the paneContainer be instantiated before
    // it is called.
    if (tb == null || pc == null) return;

    // make sure paneContainer is below tabBar
    if (pc.getZIndex(true) >= tb.getZIndex(true)) pc.moveBelow(tb);


    var tbOverlap = this._firstNonNull(this.tabBarOverlap, tb.borderThickness,
                                       tb.baseLineThickness);

    // lay out the tabBar and paneContainer, depending on where the tabBar is.
    var vertical;
    switch (this.tabBarPosition) {
        case isc.Canvas.TOP :
            vertical = false;
            pc.setRect(0,
                       tb.getHeight() - tbOverlap,
                       this.getWidth(),
                       this.getHeight() - tb.getHeight() + tbOverlap
                      );
            break;
        case isc.Canvas.BOTTOM :
            vertical = false;
            tb.setTop(this.getHeight() - tb.getHeight());
            pc.setRect(0,
                       0,
                       this.getWidth(),
                       this.getHeight() - tb.getHeight() + tbOverlap
                      );
            break;
        case isc.Canvas.LEFT :
            vertical = true;
            pc.setRect(tb.getWidth() - tbOverlap,
                       0,
                       this.getWidth() - tb.getWidth() + tbOverlap,
                       this.getHeight()
                      );
            break;
        case isc.Canvas.RIGHT :
            vertical = true;
            tb.setLeft(this.getWidth() - tb.getWidth());
            pc.setRect(0,
                       0,
                       this.getWidth() - tb.getWidth() + tbOverlap,
                       this.getHeight()
                      );
            break;
    }

    // showControls will show (or hide) the control layout, and return true if showing.
    var showControls = this.showControls();

    // If we're showing the control layout adjust our tab-bar size to take it into account
    if (showControls) {
        // Force clipping so we can scroll the tb as expected
        // Required even if we were already showing the scroller - we may have resized
        if (vertical) tb.setHeight(this.getViewportHeight() - this.tabBarControlLayout.getHeight());
        else {
            tb.setWidth(this.getViewportWidth() - this.tabBarControlLayout.getWidth());
            if (this.isRTL()) tb.setLeft(this.tabBarControlLayout.getWidth());
        }
        this.tabBarControlLayout.bringToFront();
    } else {
        tb.resizeTo(vertical ? null : "100%", vertical ? "100%" : null);
        if (this.isRTL() && !vertical) {
            tb.setLeft(0);
        }
    }
    // If the tab bar is currently scrolled, but there is enough space to display all its
    // tabs, force a scroll back to zero/zero

    var totalTabs = this._getTabSizes();
    if (vertical) {
        if (tb.getScrollTop() > 0 && totalTabs <= tb.getViewportHeight()) tb.scrollTo(null,0,"descrollTabs");
    } else {
        if (tb.getScrollLeft() > 0 && totalTabs <= tb.getViewportWidth()) tb.scrollTo(0,null,"descrollTabs");
    }
},

//>@method  tabSet.shouldShowControl()
// Should a specific control as specified in +link{tabSet.tabBarControls} be displayed?
// Default implementation will evaluate the +link{Canvas.showIf()} property for custom controls
// included as canvases. Standard controls for scrolling the tabBar will be included if
// the relevant +link{tabSet.showTabScroller} or +link{tabSet.showTabPicker} property is not
// false, and there is not enough space in the tab-bar to display all the tabs.
// @parameter (control) control from the +link{tabSet.tabBarControls} array
// @return  (boolean)   true if the control shoudl be displayed
// @group tabBarControls
//<

shouldShowControl : function (control) {
    // The standard controls only show if the tabs are clipped
    if ((control == "tabScroller") || (control == "tabPicker")) {
        if (this.showMoreTab) return false;
        if (!this.showTabScroller && control == "tabScroller") return false;
        if (!this.showTabPicker && control == "tabPicker") return false;
        // If the member width exceeds the available space for the tab-bar we need to show
        // scroller buttons
        var contentSize = this._getTabSizes();
        if (contentSize == 0) return false;


        var otherControlSize=0;
        for (var i = 0; i < this.tabBarControls.length; i++) {
            var otherControl = this.tabBarControls[i];
            if (otherControl == "tabScroller" || otherControl == "tabPicker") continue;
            if (this.shouldShowControl(otherControl)) {
                if (!isc.isA.Canvas(otherControl)) otherControl = this.getControl(otherControl);
                otherControlSize += vertical ? otherControl.getVisibleHeight() : otherControl.getVisibleWidth();
            }
        }

        var vertical = (this._tabBar.orientation == isc.Layout.VERTICAL),
            clipTabs = (contentSize > (vertical ? (this.getViewportHeight() - otherControlSize)
                                                : (this.getViewportWidth() - otherControlSize)));
        return clipTabs;
    }

    var control = this.getControl(control);

    if (isc.isA.Canvas(control) &&
        !this.tabBarControlLayout._shouldIgnoreMember(control))
    {
        return true;
    }
    return false;
},

_getTabSizes : function () {
    if (!this._tabBar) return 0;
    var contentSize = this._tabBar.getMemberSizes(),
        vertical = this._tabBar.vertical;
    if (contentSize == null || contentSize.length == 0) return 0;

    contentSize = contentSize.sum();


    var sizeAdjustment = (vertical ? (this._tabBar._topMargin || 0) + (this._tabBar._bottomMargin || 0)
                                  : (this._tabBar._leftMargin || 0) + (this._tabBar._rightMargin || 0));
    return contentSize + sizeAdjustment;
},

//> @method tabSet.getScrollerBackImgName() (A)
// Returns the +link{StretchItem.name} to use for the back button part of the <code>"tabScroller"</code>
// standard control.
// @return (String) scrollerBackImg name
// @see TabSet.scroller
//<
getScrollerBackImgName : function () {
    return this.symmetricScroller ? "back" : this.tabBarPosition + "_back";
},

//> @method tabSet.getScrollerForwardImgName() (A)
// Returns the +link{StretchItem.name} to use for the forward button part of the <code>"tabScroller"</code>
// standard control.
// @return (String) scrollerForwardImg name
// @see TabSet.scroller
//<
getScrollerForwardImgName : function () {
    return this.symmetricScroller ? "forward" : this.tabBarPosition + "_forward";
},

//> @method tabSet.getTabPickerSrc() (A)
// Returns the +link{ImgButton.src} to use for the +link{TabSet.tabPicker} button.
// @return (SCImgURL) URL of the tabPicker's src.
//<
getTabPickerSrc : function () {
    var vertical = (this._tabBar.orientation == isc.Layout.VERTICAL);
    if (this.symmetricPickerButton) {
        return vertical ? this.pickerButtonVSrc : this.pickerButtonHSrc;
    } else {
        return this.pickerButtonSrc;
    }
},

//>@method  tabSet.getControl()
// Given an entry in the +link{tabSet.tabBarControls} array, this method will return a pointer
// to the actual widget to display in the control layout.<br>
// If passed a canvas, it will be returned intact.<br>
// Will also map the special strings <code>"tabPicker"</code> and <code>"tabScroller"</code> to
// standard tab picker and scroller controls.
// @param control (string or canvas)    Control from +link{tabSet.tabBarControls} array.
// @return (canvas) Control widget to include in the control layout for this tabset
// @group tabBarControls
//<

getControl : function (control) {
    if (isc.isA.Canvas(control)) return control;
    var vertical = (this._tabBar.orientation == isc.Layout.VERTICAL);

    if (control == "tabScroller") {
        if (!this.scroller) {

            // Make the scroller a stretchImgButton with 2 "buttons"
            var sbsize = this.scrollerButtonSize;

            var scrollerSrc;
            if (this.symmetricScroller) {
                scrollerSrc = vertical ? this.scrollerVSrc : this.scrollerHSrc;
            } else {
                scrollerSrc = this.scrollerSrc;
            }
            var backName = this.getScrollerBackImgName(),
                forwardName = this.getScrollerForwardImgName();

            this.scroller = this.createAutoChild("scroller", {
                vertical:vertical,
                width:vertical ? (this.tabBarThickness - this._tabBar.baseLineThickness) : (2*sbsize),
                height:vertical ? (2*sbsize) : (this.tabBarThickness - this._tabBar.baseLineThickness),
                items:[isc.addProperties({name:backName,
                                          width:vertical ? null : sbsize,
                                          height:vertical ? sbsize : null}, this.scrollerBackImg),
                       isc.addProperties({name:forwardName,
                                          width:vertical ? null : sbsize,
                                          height:vertical ? sbsize : null}, this.scrollerForwardImg)],
                scrollerPosition:this.tabBarPosition,
                skinImgDir:this.skinImgDir,

                src:scrollerSrc,

                backPartName:backName,
                forwardPartName:forwardName
            }, this.scrollerProperties);
        }

        return this.scroller;

    } else if (control == "tabPicker") {
        var tabPickerSize = this.pickerButtonSize;
        if (!this.tabPicker) {
            var tabSrc = this.getTabPickerSrc();
            this.tabPicker = this.createAutoChild("tabPicker", {
                // use customState to append the tab bar position if necessary
                customState:this.symmetricPickerButton ? null : this.tabBarPosition,
                pickerPosition:this.tabBarPosition,
                skinImgDir:this.skinImgDir,
                src:tabSrc,
                height:(vertical ? tabPickerSize : (this.tabBarThickness - this._tabBar.baseLineThickness)),
                width:(vertical ? (this.tabBarThickness - this._tabBar.baseLineThickness) : tabPickerSize)
            });
        }

        return this.tabPicker;
    }

    // If the control is a string, check for it being a widget's global ID
    if (isc.isA.String(control) && isc.isA.Canvas(window[control])) return window[control];

    // At this point we don't recognize the controller - log a warning and bail
    this.logWarn("Unable to resolve specified tabBarControl:" + isc.Log.echo(control) +
                   " to a valid control. Not displaying.");
    return null;
},

// For autoTest: if we are showing tabBarControlLayout, access it directly by name
namedLocatorChildren:["tabBarControlLayout"],

// Method to actually show the controlLayout if required.
// If no controls are to be displayed this method falls through to hideControls()
// Returns true if any controls are displayed, false otherwise
showControls : function () {
    var controlSet = this.tabBarControls,
        controlSize = 0,
        barPos = this.tabBarPosition,
        vertical = barPos == isc.Canvas.RIGHT || barPos == isc.Canvas.LEFT,
        visibleControlIndex = 0;

    var controlLayout = this.tabBarControlLayout;
    // controls should all be housed in a layout
    if (!controlLayout) {
        // create the tabBarControls as an autoChild
        this.tabBarControlLayout = controlLayout =
                                   this.createAutoChild("tabBarControlLayout",
                                   {styleName:this.tabBarControlLayoutDefaults.styleName ||
                                              this.tabBar.styleName,
                                    _shouldIgnoreMember : function (control) {
                                        if (this.Super("_shouldIgnoreMember", arguments)) return true;
                                        if (control.showIf) return !control.fireCallback(control.showIf, [control]);
                                        return false;
                                    },
                                    // if a control is resized while visible, ensure the tabSet
                                    // is notified so it can keep us right-aligned in the tab-bar
                                    childResized : function () {
                                        this.Super("childResized", arguments);
                                        this.creator._controlLayoutChildResized();
                                    },
                                    // if the visibility of a tabBar control changes, re-layout
                                    // the tabBarControlLayout
                                    childVisibilityChanged : function (child) {
                                        this.Super("childVisibilityChanged", arguments);
                                        this.creator._controlLayoutChildResized();
                                    },
                                    vertical:vertical

                                    // For autoTest APIs
                                    ,locatorParent:this
                                   });
    }

    for (var i = 0; i< controlSet.length; i++) {
        var control = controlSet[i],
            shouldShowControl = this.shouldShowControl(control);
        // Turn the control identifier into a pointer to a Canvas if necessary
        control = this.getControl(control);
        if (!control) continue;

        if (!shouldShowControl && (control == this.scroller || control == this.tabPicker)) {
            continue;
        }

        // At this point the control should be a pointer to a canvas -
        // Ensure the layout is showing, and that the control shows up in the right spot
        if (controlLayout.getMemberNumber(control) != visibleControlIndex) {
            controlLayout.addMember(control, visibleControlIndex);
        }
        visibleControlIndex ++;

        if (shouldShowControl) {
            // Remember how much space the controls take up
            controlSize += vertical ? control.getVisibleHeight() : control.getVisibleWidth();
        }
    }

    // remove any members of the controlLayout beyond the end of the current set of visible
    // controls
    var membersToRemove = [];
    for (var i = visibleControlIndex; i < controlLayout.members.length; i++) {
        membersToRemove.add(i);
    }
    controlLayout.removeMembers(membersToRemove);
    // Note: we're not destroying these members, just deparenting them

    // If we are NOT showing any controls, hide the layout and return false
    if (controlSize == 0) {
        this.hideControls();
        return false;
    }

    this.placeControlLayout(controlSize);

    // TabBar baseline: We're truncating the tabBar in order to draw the controlLayout after
    // it.
    // The controlLayout is as thick as the tabs, excluding the baseLine (this is appropriate -
    // we want control buttons to appear above the baseLine). However since the baseLine
    // is written into the tabBar rather than being a direct child of the tabSet, it will be
    // truncated along with the tabs, so the space under the control layout will be empty (the
    // baseLine will not extend underneath the controls).
    // Therefore if we are showing the controlLayout, create a new baseLine image to
    // sit below it so the baseLine extends beyond the (truncated) tabs in the tab-bar.
    // Note that we're not destroying the existing tab-bar baseline
    // (set up via tabBar.makeBaseline) - we're essentially duplicating it with some different
    // defaults and adding it to a different position in the DOM.


    if (!this._tabBarBaseLine) {
        var tb = this._tabBar;
        this._tabBarBaseLine = this._tabBar.createAutoChild("baseLine", {

            vertical:(barPos == isc.Canvas.LEFT ||
                      barPos == isc.Canvas.RIGHT),
            _generated:true,
            skinImgDir:tb.skinImgDir,
            src:tb.baseLineSrc,
            capSize:tb.baseLineCapSize,
            imageType:isc.Img.STRETCH,
            overflow:"hidden", // since the baseline can be a Canvas if it doesn't need to display images
            autoDraw:false
        });
        this.addChild(this._tabBarBaseLine);
    }

    var tb = this._tabBar,
        tbThickness = (this.tabBarThickness - tb.baseLineThickness);

    // Position the tabBarBaseline under the controls.

    if (barPos == isc.Canvas.LEFT) {
        this._tabBarBaseLine.setRect(tbThickness, 0, tb.baseLineThickness, this.getHeight());
    } else if (barPos == isc.Canvas.RIGHT) {
        this._tabBarBaseLine.setRect(this.getWidth() -this.tabBarThickness, 0,
                                     tb.baseLineThickness, this.getHeight());
    } else if (barPos == isc.Canvas.TOP) {
        this._tabBarBaseLine.setRect(0, tbThickness, this.getWidth(), tb.baseLineThickness);
    } else if (barPos == isc.Canvas.BOTTOM) {
        this._tabBarBaseLine.setRect(0, this.getHeight() - this.tabBarThickness,
                                        this.getWidth(), tb.baseLineThickness);
    }

    if (!controlLayout.isDrawn()) {
        if (this.getDrawnState()          != isc.Canvas.UNDRAWN &&
            controlLayout.getDrawnState() == isc.Canvas.UNDRAWN) controlLayout.draw();
    } else if (!controlLayout.isVisible()) controlLayout.show();
    // Always position the baseLine behind the tabBar so we only see the edge that protrudes
    // past the end of the tabs.
    this._tabBarBaseLine.moveBelow(tb);
    if (!this._tabBarBaseLine.isVisible()) this._tabBarBaseLine.show();

    return true;
},

placeControlLayout : function (controlSize) {

    // Now figure out the desired sizing / position of the controlLayout and put it in the right
    // place
    var left,top,width,height,
        // Ensure that we don't cover the baseline
        tb = this._tabBar,
        // TabBar.getBreadth() != tabBarThickness if an app explicitly sets the tabbar's height
        // differently in properties/defaults. Notably, this occurs in the Feature Explorer,
        // where the skin switcher is thicker than the tabBarThickness under Enterprise and
        // related skins. getBreadth() is the more accurate distance
        tbThickness = tb.getBreadth() - tb.baseLineThickness,
        barPos = this.tabBarPosition;

    if (barPos == isc.Canvas.LEFT) {
        left = 0;
        top = this.getHeight() - controlSize;
        width = tbThickness;
        height = controlSize;
    } else if (barPos == isc.Canvas.RIGHT) {
        left = this.getWidth() - tbThickness;
        top = this.getHeight() - controlSize;
        width = tbThickness;
        height = controlSize;
    } else if (barPos == isc.Canvas.BOTTOM) {
        width = controlSize;
        left = this.isRTL() ? 0 : (this.getWidth() - controlSize);
        top = this.getHeight() - tbThickness;
        height = tbThickness;
    // Last possibility is TOP
    } else {
        width = controlSize;
        left = this.isRTL() ? 0 : this.getWidth() - controlSize;
        top = 0;
        height = tbThickness;
    }

    this.tabBarControlLayout.setRect(left, top, width, height);
    if (!this.children.contains(this.tabBarControlLayout)) this.addChild(this.tabBarControlLayout);

},

_controlLayoutChildResized : function () {
    var layout = this.tabBarControlLayout;
    if (!layout) return;
    this.showControls();

    var tb = this.tabBar;
    if (tb) {
        var vertical = (this.tabBarPosition == isc.Canvas.LEFT ||
                        this.tabBarPosition == isc.Canvas.RIGHT);
        if (vertical) {
            tb.setHeight(this.getViewportHeight() - this.tabBarControlLayout.getVisibleHeight());
        } else {
            tb.setWidth(this.getViewportWidth() - this.tabBarControlLayout.getVisibleWidth());
        }
    }
},

// Hide the controlLayout and special tabBarBaseLine that displayes underneath it.
hideControls : function () {
    if (this.tabBarControlLayout && this.tabBarControlLayout.isVisible()) this.tabBarControlLayout.hide();
    if (this._tabBarBaseLine && this._tabBarBaseLine.isVisible()) this._tabBarBaseLine.hide();
},

//>@method  tabSet.scrollForward()
// If there is not enough space to display all the tabs in this tabSet, this method will
// scroll the next tab (that first tab that is clipped at the end of the tab-bar) into view.
// @visibility external
//<
scrollForward : function () {
    this._tabBar.scrollForward(this.animateTabScrolling);
},

//>@method  tabSet.scrollBack()
// If there is not enough space to display all the tabs in this tabSet, this method will
// scroll the previous tab (that first tab that is clipped at the beginning of the tab-bar)
// into view.
// @visibility external
//<
scrollBack : function () {
    this._tabBar.scrollBack(this.animateTabScrolling);
},

// Called from click on the tabPicker control. Displays a menu with options to select
// a tab from the tabSet
showTabPickerMenu : function () {

    if (!this._pickerMenu) {
        var tabs = this.tabs,
            items = [];
        for (var i = 0; i < tabs.length; i++) {
            items[i] = {index:i,
                        enabled:!this.tabs[i].disabled,
                        checkIf:"menu.tabSet.getSelectedTabNumber() == " + i,
                        title:tabs[i].pickerTitle || tabs[i].title,
                        // Note: We show the tab's icon in the menu, if there is one.
                        // This will show instead of the check-mark which we normally use to
                        // indicate selection

                        icon:(this.canCloseTab(tabs[i]) ? null : tabs[i].icon),

                        // Calling selectTab will automagically scroll the tab into view if
                        // necessary
                        click:"menu.tabSet.selectTab(item.index)"}
        }
        this._pickerMenu = this.getMenuConstructor().create({tabSet:this, data:items})
    }

    // Show it under the button

    this._pickerMenu._showOffscreen();
    this._pickerMenu.placeNear(this.tabPicker.getPageLeft(), this.tabPicker.getPageBottom())
    this._pickerMenu.show();
},

// resetTabPickerMenu - helper to destroy the tab picker menu so it will be rebuilt when next shown
// This ensures it picks up new details from the current set of tabs.
resetTabPickerMenu : function () {
    if (this._pickerMenu) {
        this._pickerMenu.destroy();
        delete this._pickerMenu;
    }
},

// fix layout on a change of size
layoutChildren : function (reason,b,c,d) {
    this.invokeSuper(isc.TabSet, "layoutChildren", reason,b,c,d);
    this.fixLayout();
},

_tabResized : function () {
    this.fixLayout();
},

// NOTE: this is internal because it only shows a new tab, it does not hide the previous tab.
// The external API is selectTab();
_showTab : function (tab) {
    // Ensure we're working with a tab object rather than a tabButton instance
    // (We're keeping this.tabs up to date rather than working with the buttons directly)
    if (isc.isA.Canvas(tab)) tab = this.getTabObject(tab);

    if (tab == this.moreTab) {
        this.rebuildMorePane();
    }
    this.paneContainer.scrollTo(0,0,"showTab");

    if (tab && tab.pane) {
        if (!this.paneContainer.hasMember(tab.pane)) this.paneContainer.addMember(tab.pane);
        var paneMargin = ((tab.paneMargin != null ? tab.paneMargin : this.paneMargin) || 0);
        this.paneContainer.setLayoutMargin(paneMargin);
        tab.pane.show();
    }


    this.paneContainer.adjustOverflow();
},

//>    @method    tabSet._tabSelected(tab)    (A)
// Perform actions when a tab is selected.
// This method is "bound" to the tabBar's buttonSelected method, so that is will fire
// whenever a button on the tabBar is seleced. it performs the following functions:
//              - show the associated pane
//              - scroll to (0,0)
//
//        @see this.tabBar.buttonSelected
//        @param    tab    (tab) tab that has been selected.
//<

_tabSelected : function (tab) {
    // fire handler (fire it first so it has an opportunity to alter the tab, eg add a pane on
    // the fly)

    var cancelSelection;

    var currentTabObject = this.getSelectedTab(),
        currentTabNum = this.getSelectedTabNumber(),
        tabNum = this._tabBar.getButtonNumber(tab),
        tabObject = this.getTabObject(tabNum),
        tabDeselected = (currentTabObject != null) && (tabObject != currentTabObject);


    // currentTabNum may already be set to the tab being selected, before this
    // method has run.
    // This can occur on initial selection when tab is added/drawn and
    // on selection due to other tab being removed.
    // Therefore store another flag "_selectedTabObj" to indicate we've actually run
    // our tabSelected handlers and shown the pane.
    // If this flag is set to the tab passed in, no-op.

    var isMoreTab = this.showMoreTab && this.tabBar.isShowingMoreTab() && tabObject == this.moreTab;
    if (!isMoreTab) {
        if (tabObject == this._selectedTabObj) return;
        this._selectedTabObj = tabObject;
    }

    if (tabDeselected && !this._suppressTabSelectedHandlers) {
        // fire deselected and selected handlers.
        // Note: If this is the first time the thing is drawn we'll have tabSelected being
        // fired on the initially selected tab but the "currentTabObject" will also point to that
        // tab -- in this case don't fire the deselected handler
        // Also note: if a tab is removed programmatically it is deselected. In this case
        // currentTabObject can be expected to be unset at this point.
        if (currentTabObject.tabDeselected != null) {
            if (this.fireCallback(

                    currentTabObject.tabDeselected,
                    "tabSet,tabNum, tabPane, ID, tab, newTab, name",

                    [   this,
                        // deselected tab details
                        this.selectedTab, currentTabObject.pane, currentTabObject.ID,
                        currentTabObject,
                        // new tab
                        tabObject,
                        currentTabObject.name
                    ]
                ) == false)
            {
                cancelSelection = true;
            }
        }

        if (!cancelSelection && this.tabDeselected != null) {
            cancelSelection = (this.tabDeselected(this.selectedTab,
                                currentTabObject.pane, currentTabObject.ID, currentTabObject,
                                tabObject, currentTabObject.name) == false)
        }
        if (!cancelSelection && currentTabObject.pane) {
            currentTabObject.pane.hide();
        }
    }

    // force the tab to go back to selected state but don't fire any handlers / show or hide
    // tabs, etc.
    if (cancelSelection) {
        this._suppressTabSelectedHandlers = true;

        var cancelledTabObject = tabObject;
        var tab = this.getSelectedTab();
        this.selectTab(tab);
        var tabButton = this.getTab(this.getTabNumber(tab));
        // If this came from a click on the new tab,
        // explicitly focus back in the tab we just re-selected. This is just better UI - if
        // someone clicked a tab and it didn't select, but focus went there we don't really
        // want a dotted outline on the clicked, but not selected button.
        if (isc.EH.mouseDownTarget() == this.getTab(cancelledTabObject)) {
            // If a clickMask went up (most likely as part of the 'tabDeselected' handler showing a
            // prompt), ensure that on its dismissal focus goes to this tab, not the last clicked
            // tab!
            if (isc.EH.clickMaskUp() && isc.EH.targetIsMasked(tabButton)) {
                var topMask = isc.EH.clickMaskRegistry.last();
                isc.EH.setMaskedFocusCanvas(tabButton, topMask);
            } else {
                tabButton.focus();
            }
        }
        delete this._suppressTabSelectedHandlers;
        return;
    }

    // If pane has been destroyed drop our reference
    var pane = tabObject.pane;
    if (pane && (pane.destroyed || pane.destroying || pane.isPendingDestroy())) {
        tabObject.pane = null;
    }

    // Remember the selected tabNum - used by this.getSelectedTabNumber() etc.
    this.selectedTab = tabNum;
    if (!this._suppressTabSelectedHandlers) {
        var handlerChangedTab;
        if (tabObject.tabSelected != null) {
            this.fireCallback(
                tabObject.tabSelected,
                "tabSet,tabNum,tabPane,ID,tab",
                [this,tabNum,tabObject.pane,tabObject.ID,tabObject]
            );

            // If this tab is no longer marked as selected, tabSelected() may have shown a
            // different tab.  In this case don't call _showTab!
            if (this.getSelectedTabNumber() != tabNum) {
                return;
            }
        }

        // fire the notification functions
        if (this.tabSelected) {

            this.tabSelected(tabNum, tabObject.pane, tabObject.ID, tabObject, tabObject.name);

            // Once againk, if this tab is no longer marked as selected, tabSelected()
            // may have shown a different tab.  In this case don't call _showTab!
            if (this.getSelectedTabNumber() != tabNum) {
                return;
            }
        }
    }
    this._showTab(tab);

    // ensure the tab button is scrolled into view
    var tb = this._tabBar;
    // leave the second param as null - tab bar will automatically scroll to appropriate
    // position
    var tabSet = this;
    tb.scrollTabIntoView(tabNum, null, this.animateTabScrolling,
        function() {

            if (isc.isA.Function(tabSet.tabScrolledIntoView)) tabSet.tabScrolledIntoView();
        });
},


//> @method tab.tabSelected()
// Optional handler to fire when a tab is selected. As with +link{TabSet.tabSelected()} this
// method only fires when the tabset is drawn.
//
// @param tabSet (TabSet) the tabSet containing the tab.
// @param tabNum (integer) the index of the newly selected tab
// @param tabPane (Canvas) the newly selected tab's pane if set
// @param ID (String) the ID of the newly selected tab
// @param tab (tab) pointer to the selected tab object
// @param name (String) the name of the newly selected tab
//
// @see tab.tabDeselected
// @visibility external
//<

//> @method tab.tabDeselected()
// Optional handler to fire when a tab is deselected. Returning false will cancel the
// new selection, leaving this tab selected. As with +link{TabSet.tabSelected()} this
// method only fires when the tabset is drawn.
//
// @param tabSet (TabSet) the tabSet containing the tab.
// @param tabNum (integer) the index of the deselected tab
// @param tabPane (Canvas) the deselected tab's pane if set
// @param ID (String) the ID of the deselected tab
// @param tab (tab) pointer to the tab being deselected
// @param newTab (tab) pointer to the new tab being selected
// @param name (String) the name of the deselected tab
//
// @return (boolean) return <code>false</code> to cancel the tab selection
//
// @see tab.tabSelected
// @visibility external
//<


//>    @method    tabSet.getSelectedTab() ([A])
// Returns the currently selected tab object.  This is the object literal used to configure the
// tab, rather than the tab button widget.
// @return (Tab) the currently selected Tab object
// @visibility external
//<
getSelectedTab : function () {
    if (this.selectedTab >= this.tabs.length) return this.moreTab;
    return this.tabs[this.selectedTab];
},

//>    @method    tabSet.getSelectedTabNumber() ([A])
// Returns the index of the currently selected tab object.
// @return (number) the index of the currently selected tab object
// @visibility external
//<
getSelectedTabNumber : function () {
    if (!isc.isA.Number(this.selectedTab)) this.selectedTab = this.getTabNumber(this.selectedTab);
    // If the specified selectedTabNum doesn't correspond to a tab don't return it.
    if (!this.tabs || !this.tabs[this.selectedTab]) return -1;
    return this.selectedTab;
},



//>    @method    tabSet.selectTab()    ([])
//    Select a tab
// @param    tab   (number | ID | name | Tab) tab to select
// @visibility external
// @example tabsOrientation
//<
selectTab : function (tab) {
    var tabIndex = this.getTabNumber(tab);
    if (tabIndex != -1) {
        // calling 'selectTab()' on the tab bar will actually select the button.
        // this handles firing our tabSelected() notification functions
        if (this._tabBar) {
            this._tabBar.selectTab(tabIndex);
        }

        // TabBar (subclass of Toolbar) initializes its members (buttons) lazily on draw()
        // We won't get any _tabSelected notifications until after this has happened.
        // Therefore if the tab bar hasn't initialized yet, simply record this.selected tab
        // so methods like this.getSelectedTabNum() / getselectedTabObject() work
        //
        // Note that we explicitly call tabBar.selectTab(this.selectedTab) on draw() to ensure
        // the tab-bar stays in synch
        if (this._tabBar == null || !this._tabBar._buttonsInitialized) {
            this.selectedTab = tabIndex;
        }

    }
},

//> @method tabSet.tabForPane()
//Search for a tab that contains a pane.
//@param pane (Canvas) pane to show
//@return (Tab) tab that contains passed pane
//@visibility external
//<
tabForPane : function (pane) {
    if (this.tabs) {
        for (var i = 0; i < this.tabs.length; i++) {
            if (this.tabs[i].pane == pane) {
                return this.tabs[i];
            }
        };
    }
},

//>    @method    tabSet.getTabBar()
// Returns handle to the TabBar used by this tabset
// @return (TabBar) the tab bar
//<
getTabBar : function () {
    return this._tabBar;
},

_editTabTitle : function (tab) {
    tab = this.getTab(tab);

    var canEdit;

    if (this.canEditTabTitles) {
        if (tab.canEditTitle !== false) {
            canEdit = true;
        }
    } else {
        if (tab.canEditTitle === true) {
            canEdit = true;
        }
    }

    if (canEdit) this.editTabTitle(tab);
    return canEdit;
},

//>    @method    tabSet.editTabTitle()
// Places an editor in the title of the parameter tab and allows the user to edit the title.
// Note that this programmatic method will <b>always</b> allow editing of the specified tab's
// title, regardless of the settings of +link{canEditTabTitles} or +link{Tab.canEditTitle}.
// @param    tab      (Tab | String | integer)   The tab whose title should be edited (may be
//   specified by ID or index)
// @see TabSet.canEditTabTitles
// @see Tab.canEditTitle
// @visibility external
//<
editTabTitle : function (tab) {
    tab = this.getTab(tab);

    if (tab == null || !this.tabBar) return;

    if (!isc.isA.DynamicForm(this.titleEditorForm)) {
        var titleEditorConfig =  isc.addProperties(
                {}, this.titleEditorDefaults,
                this.titleEditorProperties, {
                     handleKeyPress : function (event,eventInfo) {

                        var rv = this.Super("handleKeyPress", arguments);

                        var keyName = event.keyName;

                        if (keyName == "Escape") {
                            this.form.targetTabSet.cancelTabTitleEditing();
                        } else if (keyName == "Enter") {
                            this.form.targetTabSet.saveTabTitle();
                        }
                        return rv;
                    }
                }
        );


        titleEditorConfig.name = "title";

        this.titleEditorForm = isc.DynamicForm.create({
            autoDraw: false,
            margin: 0, padding: 0, cellPadding: 0,
            fields: [
                titleEditorConfig
            ]
        });

        // Make the item directly available as a read-only form item (as documented)
        this.titleEditor = this.titleEditorForm.getItem("title");
    }

    var editor = this.titleEditorForm;
    editor.setProperties({targetTabSet: this, targetTab: tab});

    var item = editor.getItem("title");
    var title = tab.title;
    item.setValue(title);

    // Always scroll the tab into view before showing the editor.

    this.tabBar.scrollTabIntoView(tab, null, this.animateTabScrolling,
                {target:this, methodName:"showTitleEditor"});
},

//> @method tabSet.cancelTabTitleEditing()
// If the user is currently editing a tab title (see +link{tabSet.canEditTabTitles}), dismiss
// the editor and discard the edit value entered by the user.
// @visibility external
//<
// We'll fire this from standard end edit event (Escape keypress) too
cancelTabTitleEditing : function () {
    if (this.titleEditorForm != null) {
        this.clearTitleEditorForm();
    }
},

//> @method tabSet.saveTabTitle()
// If the user is currently editing a tab title (see +link{tabSet.canEditTabTitles}), save
// the edited tab title and hide the editor.
// @visibility external
//<
// Also fired internally from standard end edit event (click outside / enter keypress);
saveTabTitle : function () {
    if (this.titleEditorForm != null && this.titleEditorForm.isVisible()
        && this.titleEditorForm.isDrawn())
    {
        var cancelEdit = false,
            form = this.titleEditorForm,
            tab = form.targetTab,
            newTitle = form.getValue("title")
        ;
        if (newTitle != tab.title && (this.titleChanged != null)) {
            if (this.fireCallback(
                    this.titleChanged,
                    "newTitle, oldTitle, tab",
                    [newTitle, tab.title,tab]
                ) == false)
            {
                cancelEdit = true;
            }
        }
        if (!cancelEdit) this.setTabTitle(form.targetTab, newTitle);
    }
    // Dismiss the editor even if the titleChanged callback returned false, cancelling the
    // edit.
    // If we leave the editor up we're likely to get into tricky situations where
    // for example the developer can change tab with the editor still showing on another tab,

    this.clearTitleEditorForm();
},

clearTitleEditorForm : function () {
    if (this.titleEditorForm == null) return;
    this.titleEditorForm.clear();
    if (this.titleEditorForm._titleEditClickEvent != null) {
        isc.Page.clearEvent(this._titleEditClickEvent);
        delete this._titleEditClickEvent;
    }
    // Clear the 'targetTab' flag. This will allow us to avoid performing asyncronous "show"
    // due to pending animations etc. after this method has fired
    this.titleEditorForm.targetTab = null;
},

showTitleEditor : function () {
    var editor = this.titleEditorForm,
        tab = editor ? editor.targetTab : null;

    // This could happen a tab was removed, or clearTitleEditor() was called while waiting
    // for a tab to scroll (animatedly) into view, etc.
    if (tab == null || !this.getTabObject(tab)) {
        return;
    }
    // the editor will be a peer of the TabSet (shares the same parentElement)
    // The tab is a child of the TabBar
    // so left top should be tab left/top within the tabBar + tabBar left + tabBar border/margin

    var left = this.tabBar.getLeft() + this.tabBar.getLeftMargin() - this.tabBar.getScrollLeft()
            + this.tabBar.getLeftBorderSize() + tab.getLeft() + tab.capSize,
        width = tab.getVisibleWidth() - tab.capSize * 2;

    if (this.titleEditorLeftOffset) {
        left += this.titleEditorLeftOffset;
        width -= this.titleEditorLeftOffset;
    }

    if (this.titleEditorRightOffset) {
        width -= this.titleEditorRightOffset;
    }

    var item = editor.getItem("title");
    item.setWidth(width);

    // Editor form will be a peer of the tabSet - needs to float over the content of
    // the tab (nested inside the tabBar).
    var top = this.getTop() +
              this.tabBar.getTop() + this.tabBar.getTopMargin() - this.tabBar.getScrollTop()
                + this.tabBar.getTopBorderSize() + tab.getTop();
    if (this.titleEditorTopOffset) {
        top += this.titleEditorTopOffset;
    }

    editor.setTop(top);
    editor.setLeft(left);

    var item = editor.getItem("title");

    // make the editor a peer so it moves with us.
    // This will also handle showing / hiding / clearing / drawing with us - however
    // we'll also need to clear up the click-outside event on clear/hide so we'll
    // explicitly cancel title editing when we hide / clear instead of relying on this.
    if (editor.masterElement != this) {
        editor._moveWithMaster = true;
        editor._resizeWithMaster = false;
        editor._showWithMaster = false;
        this.addPeer(editor);

    } else {
        editor.draw();
    }
    item.focusInItem();
    item.delayCall("selectValue", [], 100);

    // Save edits on click outside title editor

    if (this._titleEditClickEvent == null) {
        var tabSet = this;
        var mouseDownHandler = function () {
            if (!tabSet.destroyed) {
                tabSet._clickOutsideDuringTitleEdit();
            }
        }
        this._titleEditClickEvent = isc.Page.setEvent("mouseDown", mouseDownHandler);
    }
},

_clickOutsideDuringTitleEdit : function () {
    if (isc.EH.getTarget() == this.titleEditorForm) return;
    this.saveTabTitle();
},

// On clear / hide / parent visibility change cancel title editing

// Clear is called recursively so this'll pick up parents clearing too
clear : function (a,b,c,d) {
    if (this.titleEditorForm != null && this.titleEditorForm.isDrawn()) {
        this.cancelTitleEditing();
    }
    this.invokeSuper("TabSet", "clear", a,b,c,d);
},

setVisibility : function (newVisibility, a,b,c,d) {
    this.invokeSuper("TabSet", "setVisibility", newVisibility, a,b,c,d);
    if (!this.isVisible() && this.titleEditorForm != null && this.titleEditorForm.isDrawn()) {
        this.cancelTitleEditing();
    }
},

parentVisibilityChanged : function (newVisibility, a,b,c,d) {
    this.invokeSuper("TabSet", "parentVisibilityChanged", newVisibility, a,b,c,d);
     if (!this.isVisible() && this.titleEditorForm != null && this.titleEditorForm.isDrawn()) {
        this.cancelTitleEditing();
    }
},

// documented where the string method is registered
tabsReordered : function () {}

});


isc.TabSet.registerStringMethods({
    //>    @method    tabSet.tabSelected()
    // Notification fired when a tab is selected. Note that this will only fire if
    // this tabSet is drawn. If a tab is selected before <code>TabSet.draw()</code>
    // is called, the <code>tabSelected()</code> notification will fire on
    // <code>draw()</code>
    // @param tabNum (number) number of the tab
    // @param tabPane (Canvas) pane for this tab
    // @param ID (id) id of the tab
    // @param tab (tab) the tab object (not tab button instance)
    // @param name (String) the name of the newly selected tab
    // @visibility external
    //<

    tabSelected:"tabNum,tabPane,ID,tab,name",

    //>    @method    tabSet.tabDeselected()
    //  Notification fired when a tab is deselected.
    // @param tabNum (number) number of the deselected tab
    // @param tabPane (Canvas) pane for this deselected tab
    // @param ID (id) id of the deselected tab
    // @param tab (tab) the deselected tab object (not tab button instance)
    // @param newTab (tab) the tab object being selected
    // @return (boolean) return false to cancel the tab deselection
    // @visibility external
    //<
    tabDeselected:"tabNum,tabPane,ID,tab,newTab,name",


    // getPaneContainerEdges - documented by default implementation
    getPaneContainerEdges:"",

    //> @method tabSet.onCloseClick()
    // When +link{canCloseTabs} is set, this notification method fired when the user clicks
    // the "close" icon for a tab.
    // Return false to cancel default behavior of removing the tab from the TabSet
    // @param tab (Tab) the tab to be removed
    // @return (boolean) return false to suppress removal of the tab
    // @visibility sgwt
    //<

    onCloseClick : "tab",

    //> @method tabSet.titleChanged()
    // This notification method fired when the user changes the title of a tab in this TabSet.
    // This can happen either through user interaction with the UI if
    // +link{canEditTabTitles,canEditTabTitles} is set, or programmatically if application
    // code calls +link{editTabTitle,editTabTitle}.<p>
    // Return false from this method to cancel the change.
    // @param newTitle (String) the new title
    // @param oldTitle (String) the old title
    // @param tab      (Tab)    the tab whose title has changed
    // @return (boolean) return false to suppress the title change
    // @visibility external
    //<
    titleChanged : "newTitle,oldTitle,tab",

    //> @method tabSet.showTabContextMenu()
    // Notification fired when the user right-clicks on a tab.
    // Event may be cancelled by returning false
    // @param tabSet (TabSet) This tabset
    // @param tab (Tab) the tab object that recieved the context click event
    // @return (boolean) return false to cancel default right-click behavior
    // @visibility external
    //<
    showTabContextMenu : "tabSet,tab",

    //> @method tabSet.tabsReordered
    // Noficiation method executed when one or more tabs in the TabSet are reordered.
    // @visibility external
    //<
    tabsReordered : ""

});

isc.defineClass("PaneContainer", "VLayout").addMethods({
    // override handleKeyPress to allow for navigation between tabs when focus'd on the
    // pane container or its children (via bubbled handleKeyPress events)
    // ctrl+tab - move one pane forward (or back to the first pane)
    // ctrl+shift+tab - move one pane back
    // (This is the Windows behavior - see Windows control panel)

    handleKeyPress : function (event, eventInfo) {
        if (event.keyName == "Tab" && event.ctrlKey) {
            var tabSet = this.parentElement,
                lastTabIndex = tabSet.tabs.length-1,
                currentSelection = tabSet.getSelectedTabNumber();

            if (event.shiftKey) {
                if (currentSelection > 0) currentSelection -=1;
                else currentSelection = lastTabIndex;
            } else {
                if (currentSelection < lastTabIndex) currentSelection +=1;
                else currentSelection = 0;
            }

            tabSet.selectTab(currentSelection);
            tabSet.getTabBar().getButton(currentSelection).focus();
            return false;
        }
        if (this.convertToMethod("keyPress")) return this.keyPress(event, eventInfo)
    }
});

// Register "tabs" as duplicate properties
// This means if a tabset subclass is created with tabs explicitly set to a bunch of config
// objects they'll be duplicated on instances rather than copied across directly.
// Ditto if <childName>Defaults is used in the autoChild subsystem.
// Also register the 'pane' sub property so if tab.pane is set it will be duplicated
// rather than shared across tabs
isc.TabSet.registerDupProperties("tabs", ["pane"]);

isc._debugModules = (isc._debugModules != null ? isc._debugModules : []);isc._debugModules.push('Containers');isc.checkForDebugAndNonDebugModules();isc._moduleEnd=isc._Containers_end=(isc.timestamp?isc.timestamp():new Date().getTime());if(isc.Log&&isc.Log.logIsInfoEnabled('loadTime'))isc.Log.logInfo('Containers module init time: ' + (isc._moduleEnd-isc._moduleStart) + 'ms','loadTime');delete isc.definingFramework;}else{if(window.isc && isc.Log && isc.Log.logWarn)isc.Log.logWarn("Duplicate load of module 'Containers'.");}

/*

  SmartClient Ajax RIA system
  Version v9.1p_2014-03-26/LGPL Deployment (2014-03-26)

  Copyright 2000 and beyond Isomorphic Software, Inc. All rights reserved.
  "SmartClient" is a trademark of Isomorphic Software, Inc.

  LICENSE NOTICE
     INSTALLATION OR USE OF THIS SOFTWARE INDICATES YOUR ACCEPTANCE OF
     ISOMORPHIC SOFTWARE LICENSE TERMS. If you have received this file
     without an accompanying Isomorphic Software license file, please
     contact licensing@isomorphic.com for details. Unauthorized copying and
     use of this software is a violation of international copyright law.

  DEVELOPMENT ONLY - DO NOT DEPLOY
     This software is provided for evaluation, training, and development
     purposes only. It may include supplementary components that are not
     licensed for deployment. The separate DEPLOY package for this release
     contains SmartClient components that are licensed for deployment.

  PROPRIETARY & PROTECTED MATERIAL
     This software contains proprietary materials that are protected by
     contract and intellectual property law. You are expressly prohibited
     from attempting to reverse engineer this software or modify this
     software for human readability.

  CONTACT ISOMORPHIC
     For more information regarding license rights and restrictions, or to
     report possible license violations, please contact Isomorphic Software
     by email (licensing@isomorphic.com) or web (www.isomorphic.com).

*/

