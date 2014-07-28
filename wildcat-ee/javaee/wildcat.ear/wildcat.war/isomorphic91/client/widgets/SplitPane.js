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



//> @type CurrentPane
// Possible values for the current pane showing in a +link{SplitPane}.  See
// +link{SplitPane.currentPane} for details.
// @value "navigation" +link{SplitPane.navigationPane} is the most recently shown
// @value "list" +link{SplitPane.listPane} is the most recently shown
// @value "detail" +link{SplitPane.detailPane} is the most recently shown
// @visibility external
//<



isc.defineClass("SplitPanePagedPanel", "Canvas").addProperties({
    overflow: "hidden",
    backgroundColor: "#ffffff",
    skinUsesCSSTransitions: false,
    animateScrollDuration: 350,

    // All of the pages of this `SplitPanePagedPanel' are added as children to `pagesContainer'.
    // The `pagesContainer' has overflow:"visible" so that all of the pages are visible, but
    // clipped by the `SplitPanePagedPanel' handle. The animated page change transitions are
    // implemented by translating the entire `pagesContainer' if CSS3 transitions are supported;
    // otherwise, animateMove() is used as a fall-back.
    pagesContainerBaseStyle: "splitPanePagedPanelPagesContainer",
    pagesContainerDefaults: {
        width: "100%",
        height: "100%",
        overflow: "visible",

        getTransformCSS : function () {
            var creator = this.creator;
            if (!isc.Browser._supportsCSSTransitions || !creator.skinUsesCSSTransitions) {
                return null;
            } else {
                var currentPage = creator.currentPage,
                    left;
                if (currentPage >= 0) {
                    left = -(creator.isRTL() ? creator.pages.length - 1 - currentPage : currentPage) * creator.getWidth();
                } else {
                    left = 0;
                }
                return ";" + isc.Element._transformCSSName + ": translateX(" + left + "px);";
            }
        },

        handleTransitionEnd : function (event, eventInfo) {
            // Since 'transitionend' bubbles, need to make sure that it's our transition that
            // ended, not a descendant's.
            if (eventInfo.target === this) {
                
                this._enableOffsetCoordsCaching();
            }
        }
    },

    //> @attr splitPanePagedPanel.pages (Array of Canvas : : IRW)
    //<
    //pages: [],

    //> @attr splitPanePagedPanel.currentPage (int : -1 : IRW)
    // Index of the current page.
    //<
    currentPage: -1,

autoChildren: ["pagesContainer"],
initWidget : function () {
    this.Super("initWidget", arguments);
    this.addAutoChild("pagesContainer", {
        styleName: this.pagesContainerBaseStyle
    });

    if (this.pages == null) this.pages = [];
    else this._addPagesToPagesContainer(this.pages);

    this.currentPage = Math.min(Math.max(0, this.currentPage), this.pages.length - 1);
    this._scrollToPage(this.currentPage);
},

_scrollToPage : function (currentPage, immediate) {
    if (this.pagesContainer == null) return;

    var left;
    if (currentPage >= 0) {
        left = -(this.isRTL() ? this.pages.length - 1 - currentPage : currentPage) * this.getWidth();
    } else {
        left = 0;
    }
    if (!isc.Browser._supportsCSSTransitions || !this.skinUsesCSSTransitions) {
        if (currentPage >= 0 && !immediate) {
            this.pagesContainer.animateMove(left, 0, null, this.animateScrollDuration);
        } else {
            if (this.moveAnimation != null) this.finishAnimation(this._$move);
            this.pagesContainer.setLeft(left);
        }
    } else {
        var pagesContainer = this.pagesContainer;
        if (currentPage >= 0 && !immediate) {
            pagesContainer._disableOffsetCoordsCaching();

            pagesContainer.setStyleName(this.pagesContainerBaseStyle + "Animated");
            isc.Element._updateTransformStyle(pagesContainer, "translateX(" + left + "px)");
        } else {
            // The 'transitionend' event will not fire if the transition is removed before completion.
            // https://developer.mozilla.org/en-US/docs/Web/Reference/Events/transitionend
            pagesContainer._enableOffsetCoordsCaching();

            
            isc.Element._updateTransformStyle(pagesContainer, "translateX(0px)");
            pagesContainer.setStyleName(this.pagesContainerBaseStyle); // disable animations temporarily
            isc.Element._updateTransformStyle(pagesContainer, "translateX(" + left + "px)");
        }
    }
},

//> @method splitPanePagedPanel.setCurrentPage()
// Setter for +link{SplitPanePagedPanel.currentPage,currentPage}.
//
// @param currentPage (int) the new <code>currentPage</code>.
//<
// `immediate' (boolean) internal parameter for whether the `SplitPanePagedPanel' should jump
// immediately to the page rather than animating the page into view.
setCurrentPage : function (currentPage, immediate) {
    var prevCurrentPage = this.currentPage;
    currentPage = this.currentPage = Math.min(Math.max(0, currentPage), this.pages.length - 1);
    this._scrollToPage(this.currentPage, immediate);
},

_addPagesToPagesContainer : function (pages) {
    var isRTL = this.isRTL();
    for (var i = 0, len = pages.length; i < len; ++i) {
        var page = pages[i];
        page.setWidth("100%");
        page.setHeight("100%");
        page.setLeft(((isRTL ? len - 1 - i : i) * 100) + "%");
        page.setTop(0);
        this.pagesContainer.addChild(page);
    }
},

setPages : function (pages) {
    this.pages.map("deparent");
    if (pages == null) {
        this.pages.setLength(0);
    } else {
        this.pages.setArray(pages);
        this._addPagesToPagesContainer(pages);
    }
    this.setCurrentPage(this.currentPage);
},

resized : function (deltaX, deltaY) {
    // Fix up the translation of the pagesContainer if we were resized horizontally. Note: The
    // `SplitPanePagedPanel' can be resized vertically when, for example, the navigation bar
    // title is set to an overly-long string, causing the navigation bar to increase in height
    // and this `SplitPanePagedPanel' to decrease in height. If resized only vertically, then
    // we do not want to jump immediately to the new translation on the pagesContainer.
    if (deltaX > 0) this._scrollToPage(this.currentPage, true);
}

});



isc.defineClass("SplitPaneSidePanel", "VLayout").addProperties({
    width: "42%",
    height: "100%",
    overflow: "hidden",
    baseStyle: "splitPaneSidePanel",
    skinUsesCSSTransitions: false,
    animateShowTime: 225,
    animateShowEffectConfig: {
        effect: "slide",
        startFrom: "L"
    },
    animateHideTime: 200,
    animateHideEffectConfig: {
        effect: "slide",
        endAt: "L"
    },

    //> @attr splitPaneSidePanel.navigationBar (AutoChild NavigationBar : : R)
    //<
    navigationBarDefaults: {
        _constructor: "NavigationBar",
        width: "100%"
    },

    //> @attr splitPaneSidePanel.pagedPanel (AutoChild SplitPanePagedPanel : : IRW)
    //<
    pagedPanelDefaults: {
        _constructor: "SplitPanePagedPanel",
        width: "100%",
        height: "*"
    },

    //> @attr splitPaneSidePanel.onScreen (boolean : false : R)
    //<
    onScreen: false,

autoChildren: ["navigationBar", "pagedPanel"],
initWidget : function () {
    this.Super("initWidget", arguments);
    if (this.isRTL()) this.setLeft("100%");
    this.addAutoChildren(this.autoChildren);

    this._offScreenStyleName = this.baseStyle + "OffScreen";
    this._onScreenStyleName = this.baseStyle + "OnScreen";

    if (!isc.Browser._supportsCSSTransitions || !this.skinUsesCSSTransitions) {
        this.setStyleName(this.baseStyle);
        this.hide();
    } else {
        this.setStyleName(this._offScreenStyleName);
        this.show(); // an element needs to be visible in order for transitions to have an effect
        
    }
},

setPagedPanel : function (pagedPanel) {
    if (this.pagedPanel !== pagedPanel) {
        if (this.pagedPanel != null) this.removeMember(this.pagedPanel);

        this.pagedPanel = pagedPanel;
        if (pagedPanel != null) {
            this.addMember(pagedPanel);
        }
    }
},

getTransformCSS : function () {
    if (!isc.Browser._supportsCSSTransitions || !this.skinUsesCSSTransitions || this.styleName === this._onScreenStyleName) {
        return null;
    } else {
        var dX;
        if (this.onScreen) {
            dX = this.isRTL() ? "-100%" : "0";
        } else {
            dX = this.isRTL() ? "0" : "-100%";
        }
        return ";" + isc.Element._transformCSSName + ": translateX(" + dX + ");";
    }
},

slideIn : function () {
    if (this.onScreen) return;

    this.showClickMask(
            {
                target: this,
                methodName: "slideOut"
            },
            false,
            [this]);

    if (!isc.Browser._supportsCSSTransitions || !this.skinUsesCSSTransitions) {
        this.animateShow(this.animateShowEffectConfig);
    } else {
        this.show();
        this.setStyleName(this._onScreenStyleName);
        var dX = this.isRTL() ? "-100%" : "0";
        isc.Element._updateTransformStyle(this, "translateX(" + dX + ")");
    }

    this.onScreen = true;
},

slideOut : function () {
    if (!this.onScreen) return;

    this.hideClickMask();

    if (!isc.Browser._supportsCSSTransitions || !this.skinUsesCSSTransitions) {
        this.animateHide(this.animateHideEffectConfig);
    } else {
        this.setStyleName(this._offScreenStyleName);
        if (this.isDrawn()) {
            var dX = this.isRTL() ? "0" : "-100%";
            isc.Element._updateTransformStyle(this, "translateX(" + dX + ")");
        }
    }

    this.onScreen = false;
}

});


//> @class SplitPane
// A device- and orientation-sensitive layout that implements the common pattern of rendering
// two panes side-by-side on desktop devices and on tablets in landscape orientation,
// while switching to showing a single pane for handset-sized devices or tablets in portrait
// orientation (this type of behavior is sometimes called "responsive design").
// <p>
// A <code>SplitPane</code> can manage either two or three panes &mdash; a
// +link{SplitPane.navigationPane,navigationPane} and the
// +link{SplitPane.detailPane,detailPane} are required, and a
// +link{SplitPane.listPane,listPane} can also be provided which appears in the same place as
// the navigation pane, with built-in navigation between the panes based on
// +link{NavigationBar}.  An example of 3-pane usage would be an email application:
// <ul>
// <li> <code>navigationPane</code>: +link{TreeGrid} of folders
// <li> <code>listPane</code>: +link{ListGrid} showing messages in a folder
// <li> <code>detailPane</code>: message detail view (perhaps a +link{DetailViewer} over an
//      +link{HTMLFlow} or similar arrangement)
// </ul>
// <p>
// The placement of the panes is by default sensitive to whether the device is detected as a
// handset (phone), tablet or desktop device (see +link{DeviceMode}) and to the current
// +link{PageOrientation}.  You can also configure a <code>SplitPane</code> with a fixed
// +link{SplitPane.pageOrientation} or +link{SplitPane.deviceMode}.
// <p>
// Beyond providing the panes listed above, typical usage is simply to call
// +link{SplitPane.showListPane(),showListPane()} and +link{SplitPane.showDetailPane(),showDetailPane()} when the
// <code>SplitPane</code> should navigate to a new pane.  For example, in an email application,
// clicking on a folder in the <code>navigationPane</code> should cause the
// <code>listPane</code> to show messages from the folder, then
// <code>showListPane(<em>"folder name"</em>)</code> would be called to show the
// <code>listPane</code> and give it a new title reflecting the name of the folder.
// <p>
// Similarly, clicking on a message in the <code>listPane</code> should show the message
// details in the <code>detailPane</code> and call
// <code>showDetailPane(<em>"message title"</em>)</code> to reveal the <code>detailPane</code>
// and give it an appropriate title.
// <p>
// <h3>Automatic control placement</h3>
// <p>
// +link{SplitPane.detailToolButtons} allows you to define a set of controls that are specially
// placed based on the <code>deviceMode</code> and <code>pageOrientation</code>.  See
// +link{SplitPane.detailToolButtons} for details.
// <p>
// <h3>NavigationBar and ToolStrips</h3>
// <p>
// By default, a +link{SplitPane.navigationBar} is created in all modes, and in some modes
// additional bars are created as follows:
// <ul>
// <li> in <code>deviceMode:"desktop"</code> and <code>deviceMode</code> "tablet" with
//      <code>pageOrientation</code> "landscape", the +link{SplitPane.detailToolStrip} shown
//      <em>above</em> the <code>detailPane</code>.
// <li> in <code>deviceMode:"handset"</code>, the +link{SplitPane.detailToolStrip} is created
//      <strong>only</strong> if <code>detailToolButtons</code> are specified, and is placed
//      <em>underneath</em> the <code>detailPane</code>.
// <li> +link{SplitPane.listToolStrip} - separate bar for the <code>listPane</code>, only present
//      for <code>deviceMode:"desktop"</code> when a <code>listPane</code> is provided.
// </ul>
// All of these bars are +link{group:autoChildUsage,AutoChildren} and hence completely
// optional.  You can omit them entirely, or, if you want navigation bars or tool strips but
// want to customize them more than the AutoChild system allows, you can prevent the built-in
// bars from being created and place your own +link{NavigationBar}s either <em>inside</em> your
// navigation, list or detail panes, or <em>outside</em> the <code>SplitPane</code> as a whole.
// This allows you to completely customize your navigation but still use <code>SplitPane</code>
// to handle device- and orientation-aware layout.
// <p>
// Note that while the +link{SplitPane.navigationBar,navigationBar} is named after the +link{NavigationBar}
// class, the other automatically created bars are also instances of
// <code>NavigationBar</code> despite the "toolStrip" naming convention.  These controls will
// not generally contain navigation elements; the <code>NavigationBar</code> class is used for
// consistent styling, since the <code>navigationBar</code> appears adjacent to the toolstrips
// in some modes and orientations, so they should have the same height and styling.
//
// @inheritsFrom VLayout
// @visibility external
// @treeLocation Client Reference/Layout
//<
isc.defineClass("SplitPane", "VLayout");

isc.SplitPane.addProperties({

    //> @attr splitPane.addHistoryEntries (boolean : true : IRW)
    // Should default history-tracking support be enabled? If <code>true</code>, then a history
    // management scheme utilizing +link{History.addHistoryEntry()} and +link{History.registerCallback}
    // is enabled. The history callback is registered as an additive callback, allowing other
    // history callbacks including the primary callback to be registered.
    // <p>
    // The default history management scheme is as follows:
    // <ul>
    // <li>History entries are only added after +link{Page.isLoaded(),page load} and when the
    // <code>SplitPane</code> is drawn.</li>
    // <li>A history entry is added for a pane that is hidden by +link{showNavigationPane()},
    // +link{showListPane()}, or +link{showDetailPane()} for the current +link{SplitPane.deviceMode,deviceMode}
    // and +link{SplitPane.pageOrientation,pageOrientation} settings.
    // <p>
    // Example 1: When <code>deviceMode</code> is "desktop", all 3 panes are shown simultaneously,
    // so no history entries are added.
    // <p>
    // Example 2: When <code>deviceMode</code> is "handset", calling +link{showDetailPane()}
    // will hide the current pane (the +link{listPane} if present, otherwise the +link{navigationPane}).
    // A history entry is added for the pane that was hidden</li>
    // </ul>
    // <p>
    // The default history management scheme can be supplemented with application-specific
    // history management. For example, when <code>deviceMode</code> is "tablet", the +link{detailPane}
    // is always visible, but changes to the content of the <code>detailPane</code> are transparent
    // to the <code>SplitPane</code>. The application can add history entries of its own when
    // the user causes new information to be displayed in the <code>detailPane</code>.
    // @setter setAddHistoryEntries()
    // @requiresModules History
    // @visibility external
    //<
    addHistoryEntries: true,

    //> @attr splitPane.deviceMode (DeviceMode : null : IR)
    // UI layout mode used for this <code>SplitPane</code>.
    // <p>
    // A <code>SplitPane</code> can be configured with up to 3 panes: the +link{navigationPane},
    // +link{listPane} and +link{detailPane}.  Both +link{deviceMode} and +link{PageOrientation}
    // influence the placement of these panes as follows:
    // <ul>
    // <li>"handset" <code>deviceMode</code>: only a single pane is shown at a time.  Not
    //      orientation sensitive
    // <li>"tablet" <code>deviceMode</code> with <code>pageOrientation</code>:"landscape": the
    //      <code>detailPane</code> is shown side by side with either the
    //      <code>navigationPane</code> or <code>listPane</code>
    // <li>"tablet" <code>deviceMode</code> with <code>pageOrientation</code>:"portrait": the
    //      <code>detailPane</code> is shown only.  End user navigation that would show the
    //      <code>listPane</code> or <code>navigationPane</code> shows those panes on top of the
    //      <code>detailPane</code> (temporarily covering part of its content)
    // <li>"desktop" <code>deviceMode</code>: all 3 panes are shown simultaneously.  Not
    //      orientation sensitive
    // </ul>
    // The <code>listPane</code> is optional; if not present, wherever the <code>listPane</code>
    // is mentioned above, the <code>navigationPane</code> is shown instead.
    //
    // @visibility external
    //<
    //deviceMode: null,

    //> @attr splitPane.pageOrientation (PageOrientation : null : IRW)
    // Current +link{PageOrientation}.  The default behavior of the <code>SplitPane</code> is to
    // register for orientation change notifications from the device (see
    // +link{Page.getOrientation()}) and automatically change orientation based on the
    // +link{SplitPane.deviceMode,type of device}.
    // <p>
    // You can instead set a specific value for <code>pageOrientation</code> if you only want to
    // use a specific layout, and not respond to orientation information from the device.
    //
    // @setter setPageOrientation()
    // @visibility external
    //<
    //pageOrientation: null,

    portraitSidePanelDefaults: {
        _constructor: "SplitPaneSidePanel"
    },

    handsetPagedPanelDefaults: {
        _constructor: "SplitPanePagedPanel",
        width: "100%",
        height: "*"
    },

    showSidePanelButtonDefaults: {
        _constructor: "NavigationButton",
        click : function () {
            var creator = this.creator;
            
            var currentPane = this.creator.currentPane;
            if (currentPane === "list" ||
                (currentPane === "detail" && creator._hasListPane()))
            {
                creator.showListPane(null, null, false, true);
            } else {
                
                creator.showNavigationPane(false, true);
            }
        }
    },

    mainLayoutDefaults: {
        _constructor: "HLayout"
    },

    leftLayoutDefaults: {
        _constructor: "VLayout",
        width: 320
    },

    rightLayoutDefaults: {
        _constructor: "VLayout"
    },

    navigationLayoutDefaults: {
        _constructor: "VLayout"
    },

    listLayoutDefaults: {
        _constructor: "VLayout"
    },

    detailLayoutDefaults: {
        _constructor: "VLayout",
        overflow:"hidden"
    },

    spacerDefaults: {
        backgroundColor: "black",
        overflow: "hidden",
        width: 1,
        height: "100%"
    },

    //> @attr splitPane.showResizeBars (boolean : true : IR)
    // If enabled, the <code>SplitPane</code> will add resize bars between the
    // +link{SplitPane.navigationPane,navigationPane} and +link{SplitPane.detailPane,detailPane}
    // when these panes are shown side-by-side, and between the +link{SplitPane.listPane,listPane}
    // and +link{SplitPane.detailPane,detailPane} in +link{SplitPane.deviceMode,deviceMode:"desktop"}.
    //
    // @visibility external
    //<
    showResizeBars: true,

    //> @attr splitPane.navigationBar (AutoChild NavigationBar : null : IR)
    // The AutoChild +link{class:NavigationBar} managed by this widget.
    // <p>
    // The following +link{group:autoChildUsage,passthroughs} apply:
    // <ul>
    // <li>+link{SplitPane.showRightButton,showRightButton} for NavigationBar.showRightButton.</li>
    // <li>+link{SplitPane.showLeftButton,showLeftButton} for NavigationBar.showLeftButton.</li>
    // </ul>
    //
    // @visibility external
    //<
    
    navigationBarDefaults: {
        _constructor: "NavigationBar",
        autoParent: "none",
        hieght: 44,
        rightPadding: 5,
        leftPadding: 5,
        defaultLayoutAlign: "center",
        overflow: "hidden",
        navigationClick : function (direction) {
            var creator = this.creator;
            if (creator.navigationClick != null) creator.navigationClick(direction);
        }
    },

    //> @attr splitPane.backButton (AutoChild NavigationButton : null : IR)
    // The back button shown in the +link{SplitPane.navigationBar,navigationBar} depending on
    // the current UI configuration. The back button's +link{Button.title,title} is determined
    // by the <code>SplitPane</code>.
    //
    // @visibility external
    //<
    
    backButtonDefaults: {
        _constructor: "NavigationButton",
        direction: "back",
        click : function () {
            var creator = this.creator;
            if (creator.currentPane === "detail" && creator._hasListPane() &&
                creator.currentUIConfig !== "landscape")
            {
                creator.showListPane();
            } else {
                creator.showNavigationPane();
            }
            return false;
        }
    },

    //> @attr splitPane.currentPane (CurrentPane : "navigation" : IRW)
    // The most recently shown pane.  In handset +link{DeviceMode}, the
    // <code>currentPane</code> is the only pane that is actually visible to the user.  In other
    // modes more than one pane can be simultaneously visible, so the <code>currentPane</code> is
    // the most recent pane that was brought into view via a call to +link{setCurrentPane()} or
    // +link{showNavigationPane()}.
    // <p>
    // The default value of <code>currentPane</code> is "navigation".
    //
    // @visibility external
    //<
    currentPane: "navigation",

    //> @attr splitPane.navigationTitle (HTMLString : null : IRW)
    // The title for the +link{SplitPane.navigationPane,navigationPane}.
    //
    // @setter setNavigationTitle()
    // @visibility external
    //<

    //> @attr splitPane.navigationPane (Canvas : null : IRW)
    // The left-hand of the two panes managed by this widget, used for navigation.
    //
    // @visibility external
    //<

    //> @attr splitPane.listTitle (HTMLString : null : IRW)
    // The title for the +link{SplitPane.listPane,listPane}.
    //
    // @setter setListTitle()
    // @visibility external
    //<

    listTitleLabelDefaults: {
        _constructor: "Label",
        align: "center",
        valign: "center",
        width: "*",
        height: "100%"
    },

    //> @attr splitPane.listPane (Canvas : null : IRW)
    // An optional list pane displayed in the left-hand of the panes or in a side panel
    // according to the pane layout.
    //
    // @visibility external
    //<

    //> @attr splitPane.listToolStrip (AutoChild NavigationBar : null : IR)
    // Bar displayed above the +link{SplitPane.listPane,listPane}, if a <code>listPane</code> is present,
    // <b>only</b> for +link{SplitPane.deviceMode,deviceMode} "desktop".
    //
    // @visibility external
    //<
    listToolStripDefaults: {
        _constructor: "NavigationBar",
        rightPadding: 5,
        leftPadding: 5,
        defaultLayoutAlign: "center",
        overflow: "hidden",
        showLeftButton: false,
        showRightButton: false
    },

    //> @attr splitPane.detailTitle (HTMLString : null : IRW)
    // The title for the +link{SplitPane.detailPane,detailPane}.
    //
    // @setter setDetailTitle()
    // @visibility external
    //<

    detailTitleLabelDefaults: {
        _constructor: "Label",
        align: "center",
        valign: "center",
        width: "*",
        height: "100%"
        
        //,snapTo: "C"
    },

    //> @attr splitPane.detailPane (Canvas : null : IRW)
    // The right-hand of the two panes managed by this widget, used for viewing details.
    //
    // @visibility external
    //<

    //> @attr splitPane.detailToolStrip (AutoChild NavigationBar : null : IR)
    // Toolstrip servicing the +link{SplitPane.detailPane,detailPane}.
    // <p>
    // This is not used when +link{SplitPane.deviceMode,deviceMode} is
    // <smartclient>"handset";</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.DeviceMode#HANDSET};</smartgwt>
    // in handset mode, the +link{SplitPane.navigationBar,navigationBar} is used exclusively.
    //
    // @visibility external
    //<
    detailToolStripDefaults: {
        _constructor: "NavigationBar",
        rightPadding: 5,
        leftPadding: 5,
        defaultLayoutAlign: "center",
        overflow: "hidden"
    },

    //> @attr splitPane.detailToolButtons (Array of Canvas : null : IRW)
    // <code>detailToolButtons</code> allows you to specify a set of controls that are specially
    // placed based on the +link{SplitPane.deviceMode,deviceMode} and +link{SplitPane.pageOrientation,pageOrientation}.
    // This is generally useful for a compact strip of +link{ImgButton} controls, approximately
    // 5 of which will fit comfortably using typical size icons and in the most space-constricted
    // modes.
    // <p>
    // These controls are placed as follows:
    // <ul>
    // <li> in <code>deviceMode:"desktop"</code> and <code>deviceMode</code> "tablet" with
    //      <code>pageOrientation</code> "landscape", <code>detailToolButtons</code> appear in the
    //      +link{splitPane.detailToolStrip} shown <i>above</i> of the <code>detailPane</code>.
    // <li> in <code>deviceMode:"handset"</code>, <code>detailToolButtons</code> appear in a
    //       +link{splitPane.detailToolStrip} <i>underneath</i> the detailPane.  This toolstrip
    //      is only created in "handset" mode if <code>detailToolButtons</code> are provided.
    // <li> in <code>deviceMode:"tablet"</code> and <code>pageOrientation:"portrait"</code>,
    //      <code>detailToolButtons</code> appear in <code>splitPane.navigationBar</code>.
    // </ul>
    //
    // @visibility external
    //<

    autoChildren: ["mainLayout", "leftLayout", "rightLayout", "navigationLayout", "listLayout",
                   "listToolStrip", "detailLayout", "detailToolStrip"],
    

    

    isHandset : function () {
        return this.deviceMode == null ? isc.Browser.isHandset : this.deviceMode === "handset";
    },

    isTablet : function () {
        return this.deviceMode == null ? isc.Browser.isTablet : this.deviceMode === "tablet";
    },

    getPageOrientation : function () {
        return this.pageOrientation || isc.Page.getOrientation();
    },

    initWidget : function () {
        this.Super("initWidget", arguments);

        this.addAutoChildren(this.autoChildren, "none");
        // Create the navigationBar AutoChild with the passthroughs applied.
        this.addAutoChild("navigationBar", {
            showRightButton: this.showRightButton,
            showLeftButton: this.showLeftButton
            
        });

        // On tablets, we need to create the side panel right away
        if (this.isTablet()) {
            this.portraitSidePanel = this.createAutoChild("portraitSidePanel");
            this._pagedPanel = this.portraitSidePanel.pagedPanel;

        // On handsets, create a paged panel to host the navigation, list, and detail panes.
        } else if (this.isHandset()) {
            this._pagedPanel = this.createAutoChild("handsetPagedPanel");
        }

        this.addMember(this.mainLayout);

        if (this.navigationPane != null) this._setNavigationPane(this.navigationPane);
        if (this.detailPane != null) this._setDetailPane(this.detailPane);

        // Set an 'orientationChange' event handler if this.pageOrientation is null, as per documentation.
        if (this.pageOrientation == null) {
            this._orientationChangeEventID = isc.Page.setEvent("orientationChange", this);
        }

        this._historyIDPrefix = this.getID() + "_";
        if (this.addHistoryEntries) this._setUpDefaultHistoryManagement();

        this.pageOrientationChange();
    },

    destroy : function () {
        if (this._historyCallbackID != null) {
            
            isc.History.unregisterCallback(this._historyCallbackID);
            delete this._historyCallbackID;
        }
        if (this._orientationChangeEventID != null) {
            isc.Page.clearEvent("orientationChange", this._orientationChangeEventID);
            delete this._orientationChangeEventID;
        }
        this.Super("destroy", arguments);
    },

    draw : function () {
        this.Super("draw", arguments);
        this._maybeAddHistoryEntry();
    },

    _setUpDefaultHistoryManagement : function () {
        
        if (!isc.History) {
            this.logError("addHistoryEntries is true, but the History module is not loaded.");
        } else if (this._historyCallbackID == null) {
            this._historyCallbackID = isc.History.registerCallback({ target: this, methodName: "historyCallback" }, true, true);
        }
        this._maybeAddHistoryEntry();
    },

    //> @method splitPane.setAddHistoryEntries()
    // Setter for +link{SplitPane.addHistoryEntries}.
    // @param addHistoryEntries (boolean) the new setting.
    // @visibility external
    //<
    setAddHistoryEntries : function (addHistoryEntries) {
        

        this.addHistoryEntries = addHistoryEntries;
        if (addHistoryEntries) this._setUpDefaultHistoryManagement();
        else if (this._historyCallbackID != null) {
            
            isc.History.unregisterCallback(this._historyCallbackID);
            delete this._historyCallbackID;
        }

        
    },

    _maybeAddHistoryEntry : function () {
        if (!this.addHistoryEntries || !isc.History) return;

        if (!this.isDrawn()) return;

        var currentPane = this.currentPane;
        var id = this._historyIDPrefix + currentPane;
        var title = String(this.navigationTitle);
        var data = {
            _overriddenBackButtonTitle: this._overriddenBackButtonTitle
        };
        if (currentPane === "navigation") {
            // In "desktop" layout mode, the navigation pane is always visible, so don't add a
            // history entry for it.
            if (!(this.isTablet() || this.isHandset())) return;
            data.title = this.navigationTitle;
        } else if (currentPane === "list") {
            // In "desktop" layout mode, the list pane is always visible, so don't add a history
            // entry for it.
            if (!(this.isTablet() || this.isHandset())) return;
            title += " > " + (this.listTitle == null ? "" : String(this.listTitle));
            data.title = this.listTitle;
        } else if (currentPane === "detail") {
            // In "tablet" and "desktop" layout modes, the detail pane is always visible, so don't
            // add a history entry for it.
            if (!this.isHandset()) return;
            if (this._hasListPane()) {
                title += " > " + (this.listTitle == null ? "" : String(this.listTitle));
            }
            title += " > " + (this.detailTitle == null ? "" : String(this.detailTitle));
            data.title = this.detailTitle;
        }

        // History entries can only be added after the page has loaded. If not loaded yet, set
        // a one-time 'load' event.
        if (!isc.Page.isLoaded()) {
            isc.Page.setEvent("load", function () {
                
                isc.History.addHistoryEntry(id, title, data);
            }, isc.Page.FIRE_ONCE);
        } else {
            
            isc.History.addHistoryEntry(id, title, data);
        }
    },

    //> @method splitPane.historyCallback() (A)
    // @param id (String)
    // @param [data] (String)
    //<
    historyCallback : function (id, data) {
        if (this.destroyed || id == null) return;

        

        var historyIDPrefix = this._historyIDPrefix;
        if (id.startsWith(historyIDPrefix)) {
            var oldPane = id.substring(historyIDPrefix.length);
            if (oldPane === "navigation") {
                this.setNavigationTitle(data.title);
                this.showNavigationPane(true);
            } else if (oldPane === "list") {
                this.showListPane(data.title, data._overriddenBackButtonTitle, true);
            } else {
                this.showDetailPane(data.title, data._overriddenBackButtonTitle, true);
            }
        }
    },

    //> @method splitPane.setCurrentPane()
    // Reveals the pane indicated by the <code>newPane</code> parameter.
    // <p>
    // This has different effects based on the +link{deviceMode} and +link{pageOrientation}.  For
    // example, in "handset" mode, the new pane will be the only one showing.  In other modes such
    // as "desktop", this method may do nothing, but should still be called in order to ensure
    // correct behavior with other +link{type:DeviceMode} settings.
    //
    // @param newPane (CurrentPane) new pane to show.
    // @visibility external
    //<
    setCurrentPane : function (newPane) {
        if (newPane === "navigation") this.showNavigationPane();
        else if (newPane === "list") this.showListPane();
        else this.showDetailPane();
    },

    //> @method splitPane.setDetailToolButtons()
    // Updates the +link{SplitPane.detailToolButtons,detailToolButtons} at runtime.
    //
    // @param buttons (Array of Canvas) new controls for the toolstrip.
    // @visibility external
    //<
    setDetailToolButtons : function (buttons) {
        
        this.detailToolButtons = buttons;
        this.updateDetailToolStrip();
    },

    //> @method splitPane.setPageOrientation()
    // Explicitly sets the page orientation to a fixed value instead of being responsive to device
    // orientation changes.  Pass <code>null</code> to return to responding automatically to device
    // orientation.
    // <p>
    // See +link{PageOrientation} for details of how page orientation affects layout.
    //
    // @param newOrientation (PageOrientation) new orientation to use.
    // @visibility external
    //<
    setPageOrientation : function (newOrientation) {
        

        if (this.pageOrientation !== newOrientation) {
            this.pageOrientation = newOrientation;

            // Set an 'orientationChange' event handler if null.
            if (newOrientation == null) {
                if (this._orientationChangeEventID == null) {
                    this._orientationChangeEventID = isc.Page.setEvent("orientationChange", this);
                }

            // Otherwise, an explicit pageOrientation was set, so clear the 'orientationChange'
            // event handler if set.
            } else if (this._orientationChangeEventID != null) {
                isc.Page.clearEvent("orientationChange", this._orientationChangeEventID);
                delete this._orientationChangeEventID;
            }
        }

        

        this.pageOrientationChange();
    },

    pageOrientationChange : function () {
        

        this.updateUI();
    },

    updateUI : function (forceRefresh) {

        var prevConfig = this.currentUIConfig,
            prevPane = this._lastPane;

        // Possible UI configurations:
        // - handset (one pane at a time)
        // - portrait (detailPane always visible, navigation and list panes are shown in a side panel)
        // - landscape (two panes are visible as columns)
        var config = this.currentUIConfig = this.getUIConfiguration(),
            pane = this._lastPane = this.currentPane;

        if (!forceRefresh && config === prevConfig && pane === prevPane) {
            if (config === "handset") {
                this._pagedPanel._scrollToPage(this._pagedPanel.currentPage, true);
            }
            return;
        }

        this.updateNavigationBar();
        // NOTE: `this.navigationBar' might be null at this point if showNavigationBar:false

        if (config === "handset") {
            var pages;
            if (prevConfig !== "handset") {
                pages = [];
                if (this.navigationPane != null) pages.add(this.navigationPane);
                if (this.listPane != null) pages.add(this.listPane);
                if (this.detailPane != null) pages.add(this.detailPane);
                this._pagedPanel.setPages(pages);
            } else {
                pages = this._pagedPanel.pages;
            }

            if (pane === "navigation") {
                this._pagedPanel.setCurrentPage(0, prevConfig !== "handset");
            } else if (pane === "list") {
                this._pagedPanel.setCurrentPage(1, prevConfig !== "handset");
            } else {
                this._pagedPanel.setCurrentPage((this._hasListPane() ? 2 : 1), prevConfig !== "handset");
            }

            var members = [];
            if (this.navigationBar != null) members.add(this.navigationBar);
            members.add(this._pagedPanel);
            this.navigationLayout.setMembers(members);
            members.setLength(0);
            members.add(this.navigationLayout);
            this.mainLayout.setMembers(members);

        } else if (config === "portrait") {
            // Detail toolstrip across the top
            //  - first button to show navigation or list view
            //  - detail nav control as 2nd button
            //  - detail title
            //  - detail tool buttons
            // Detail pane shows
            // If currentPane is "list":
            //  - side panel showing the list pane
            // If currentPane is "navigation":
            //  - side panel showing the navigation pane

            this.leftLayout.removeMembers(this.leftLayout.members);

            this.portraitSidePanel.setPagedPanel(this._pagedPanel);

            this.updateDetailToolStrip();
            this.detailLayout.setMembers([this.detailToolStrip]);
            if (this.detailPane != null) this.detailLayout.addMember(this.detailPane);
            this.mainLayout.setMembers([this.detailLayout]);

            var pages;
            if (prevConfig !== "portrait") {
                pages = [];
                if (this.navigationPane != null) pages.add(this.navigationPane);
                if (this.listPane != null) pages.add(this.listPane);
                this.portraitSidePanel.pagedPanel.setPages(pages);
            } else {
                pages = this.portraitSidePanel.pagedPanel.pages;
            }

            if (pane === "navigation") {
                this._pagedPanel.setCurrentPage(0, !this.portraitSidePanel.onScreen);
                this.portraitSidePanel.slideIn();

            } else if (pane === "list") {
                
                this._pagedPanel.setCurrentPage(1, !this.portraitSidePanel.onScreen);
                this.portraitSidePanel.slideIn();

            } else {
                if (this.portraitSidePanel.onScreen) {
                    this.portraitSidePanel.slideOut();
                }
            }

        } else if (config === "landscape") {
            // Landscape view
            //   This mode is used for landscape tablet views.
            //   Only 2 panes are shown at once.
            //
            // With list:
            //     Left side:
            //       Nav bar with button to show navigation view
            //         List title
            //       List pane
            //     Right side:
            //       Toolstrip with prev/next buttons
            //         No title
            //       Detail pane
            //
            // NO list:
            //     Left side:
            //       Nav bar
            //       Nav title
            //       Nav pane
            //     Right side:
            //       Detail Toolstrip
            //         Detail title
            //         Detail tool buttons

            this.portraitSidePanel.setPagedPanel(null);

            if (this.portraitSidePanel.onScreen) {
                this.portraitSidePanel.slideOut();
            }

            this.updateDetailToolStrip();
            var members = [];
            if (this.detailToolStrip != null) members.add(this.detailToolStrip);
            if (this.detailPane != null) members.add(this.detailPane);
            this.detailLayout.setMembers(members);

            var pages;
            if (prevConfig !== "landscape") {
                pages = [];
                if (this.navigationPane != null) pages.add(this.navigationPane);
                if (this.listPane != null) pages.add(this.listPane);
                this._pagedPanel.setPages(pages);
            } else {
                pages = this._pagedPanel.pages;
            }

            if (pane === "navigation") {
                this._pagedPanel.setCurrentPage(0, prevConfig !== "landscape");

            } else if (pane === "list") {
                
                this._pagedPanel.setCurrentPage(1, prevConfig !== "landscape");
            }

            members.setLength(0);
            if (this.navigationBar != null) members.add(this.navigationBar);
            members.add(this._pagedPanel);
            this.leftLayout.setMembers(members);

            this.rightLayout.setMembers([this.detailLayout]);
            
            members.setLength(0);
            members.add(this.leftLayout);
            if (this.showResizeBars) {
                this.leftLayout.setShowResizeBar(true);
            } else {
                this.leftLayout.setShowResizeBar(false);
                if (this.spacer == null) {
                    this.spacer = this.createAutoChild("spacer");
                }
                members.add(this.spacer);
            }
            members.add(this.rightLayout);
            this.mainLayout.setMembers(members);

        } else {
            // Desktop view
            //   This mode is used for the desktop view.
            //   All 3 panes may be shown.
            //
            // With list:
            //     Left side:
            //       Nav bar
            //       Nav title
            //       Nav pane
            //     Right side:
            //       Detail Toolstrip
            //         List title
            //         List tool buttons
            //       Detail pane
            //
            // NO list:
            //     Left side:
            //       Nav bar
            //       Nav title
            //       Nav pane
            //     Right side:
            //       Detail Toolstrip
            //         Detail title
            //         Detail tool buttons

            var members = [];
            if (this.navigationBar != null) members.add(this.navigationBar);
            if (this.navigationPane != null) members.add(this.navigationPane);
            this.navigationLayout.setMembers(members);
            // In desktop mode, the detail toolstrip goes with the listPane.
            this.updateListToolStrip();
            this.updateDetailToolStrip();
            if (this._hasListPane()) {
                members.setLength(0);
                if (this.listToolStrip != null) members.add(this.listToolStrip);
                
                members.add(this.listPane);
                this.listLayout.setMembers(members);
                this.listLayout.setShowResizeBar(this.showResizeBars);
                members.setLength(0);
                if (this.detailPane != null) {
                    if (this.detailToolStrip != null) members.add(this.detailToolStrip);
                    members.add(this.detailPane);
                }
                this.detailLayout.setMembers(members);
            } else {
                members.setLength(0);
                if (this.detailToolStrip != null) members.add(this.detailToolStrip);
                members.add(this.detailPane);
                this.detailLayout.setMembers(members);
            }

            this.leftLayout.setMembers([this.navigationLayout]);
            this.leftLayout.setShowResizeBar(this.showResizeBars);
            members.setLength(0);
            if (this._hasListPane()) members.add(this.listLayout);
            members.add(this.detailLayout);
            this.rightLayout.setMembers(members);
            this.mainLayout.setMembers([this.leftLayout, this.rightLayout]);
        }
        
    },

    updateListToolStrip : function () {
        if (this.listToolStrip == null) return;
        if (this.currentUIConfig === "desktop") {
            this.updateListTitleLabel();
            var members = [];
            if (this.listTitleLabel != null) members.add(this.listTitleLabel);
            this.listToolStrip.setMembers(members);
        }
    },

    updateListTitleLabel : function () {
        if (this.showListTitleLabel == false) return;
        if (this.listTitleLabel == null) {
            this.listTitleLabel = this.createAutoChild("listTitleLabel");
        }
        this.listTitleLabel.setContents(this.listTitle);
    },

    updateDetailToolStrip : function () {
        if (this.detailToolStrip == null) return;
        // handset mode - only shows on detail view and contains just the detailToolButtons,
        // centered
        if (this.currentUIConfig === "handset") {
            var members = [isc.LayoutSpacer.create({width:"*"})];
            members.addList(this.detailToolButtons);
            members.add(isc.LayoutSpacer.create({width:"*"}));

            // Probably not required - could happen if switching from 'portrait' to 
            // 'handset' - so only possible with an explicit override to the
            // ui config since the device won't change!
            if (this.detailTitleLabel && this.detailTitleLabel.isDrawn()) {
                this.detailTitleLabel.deparent();
            }

            this.detailToolStrip.setMembers(members);

        // portrait mode - always visible at top
        // Contains 
        // - show side panel button
        // - detailNavigationControl (if set)
        // - detail tool buttons on right
        // Float title across center
        } else if (this.currentUIConfig === "portrait") {
            var showSidePanelButtonTitle = (this.currentPane !== "navigation" && this.listPane
                                            ? this.listTitle
                                            : this.navigationTitle);
            if (this.showSidePanelButton == null) {
                this.showSidePanelButton = this.createAutoChild(
                    "showSidePanelButton",
                    {
                        title: showSidePanelButtonTitle
                    }
                );
            } else {
                this.showSidePanelButton.setTitle(showSidePanelButtonTitle);
            }

            this.updateDetailTitleLabel();

            var members = [
                this.showSidePanelButton,
                this.detailNavigationControl
            ];
            if (this.detailTitleLabel != null) members.add(this.detailTitleLabel);
            if (this.detailToolButtons != null) members.addList(this.detailToolButtons);
            members.removeEmpty();
            this.detailToolStrip.setMembers(members);

        } else {
            // Default view (tablet landscape or desktop)
            //      - detail title
            //      - detail tool buttons
            this.updateDetailTitleLabel();
            var members = [];
            if (this.detailTitleLabel != null) members.add(this.detailTitleLabel);
            if (this.detailToolButtons != null) members.addList(this.detailToolButtons);
            this.detailToolStrip.setMembers(members);
        }
    },

    updateDetailTitleLabel : function () {
        if (this.showDetailTitleLabel == false) return;
        if (this.detailTitleLabel == null) {
            this.detailTitleLabel = this.createAutoChild("detailTitleLabel");
        }
        this.detailTitleLabel.setContents(this.detailTitle);
    },

    updateNavigationBar : function () {
        var navigationBar;
        if (this.currentUIConfig === "portrait") {
            navigationBar = this.portraitSidePanel.navigationBar;
        } else {
            navigationBar = this.navigationBar;
        }
        if (navigationBar == null) return;

        //>DEBUG
        this.logInfo("updateNavigationBar, currentPane: " + this.currentPane +
                     ", currentUI: " + this.currentUIConfig);
        //<DEBUG

        // When showing detail view on a handset we show the navigation bar
        // but repurpose it as a detail navigation bar.
        //      - custom left button to return to navigation pane
        //      - Detail pane title
        //      - custom right button based on 'detail nav control'  
        if ((this.currentUIConfig === "handset" && this.currentPane !== "navigation") ||
            (this.currentUIConfig === "portrait" && this.currentPane !== "navigation") ||
            (this.currentUIConfig === "landscape" && this.currentPane !== "navigation" && this._hasListPane()))
        {
            // In portrait mode we show the nav or list pane
            // and the detail pane at the same time
            // In this case the title should reflect the current pane visible on the left
            var title;
            if (this.currentUIConfig === "landscape") {
                title = (this._hasListPane() && this.listPane.isVisible() ? this.listTitle
                                                                          : this.navigationTitle);
            } else if (this.currentUIConfig === "portrait") {
                title = (this._hasListPane() && this.currentPane !== "navigation" ? this.listTitle
                                                                                  : this.navigationTitle);
            } else {
                title = (this.currentPane === "detail"
                         ? this.detailTitle 
                         : (this.currentPane === "list"
                            ? this.listTitle
                            : this.navigationTitle));
            }
            if (!title) title = "&nbsp;";

            navigationBar.setTitle(title);

            var backButtonTitle;
            if (this._overriddenBackButtonTitle != null) {
                backButtonTitle = this._overriddenBackButtonTitle;
            } else {
                backButtonTitle =
                    this.isHandset() && this.currentPane === "detail" && this._hasListPane()
                        ? this.listTitle
                        : this.navigationTitle;
            }
            if (this.backButton == null) {
                this.backButton = this.createAutoChild("backButton", {
                    title: backButtonTitle
                });
            } else {
                this.backButton.setTitle(backButtonTitle);
            }

            var controls = [];
            if (this.currentUIConfig !== "portrait" || this._hasListPane()) {
                controls.add(this.backButton);
            }
            controls.add("leftButton");
            controls.add("titleLabel");
            if (this.detailNavigationControl != null) {
                controls.add(this.detailNavigationControl);
            }
            controls.add("rightButton");
            navigationBar.setControls(controls);

        // default behavior - navigation bar shows navigation title and controls
        // specified by the developer (so update title, icons, visibility)
        } else {
            navigationBar.setTitle(this.navigationTitle || "&nbsp;");

            navigationBar.setControls(["leftButton", "titleLabel", "rightButton"]);
        }

        navigationBar.setLeftButtonTitle(this.leftButtonTitle);
        navigationBar.setRightButtonTitle(this.rightButtonTitle);

        navigationBar.setShowLeftButton(this.showLeftButton);
        navigationBar.setShowRightButton(this.showRightButton);

        if (this.currentUIConfig === "portrait") {
        } else if (this.currentUIConfig === "landscape") {
            var styleName = (this.navigationBarProperties && this.navigationBarProperties.styleName) ||
                            (this.navigationBarDefaults && this.navigationBarDefaults.styleName) ||
                            this.navigationBar.getClass().getInstanceProperty("styleName");
            navigationBar.setStyleName(styleName);
        }
    },

    
    getUIConfiguration : function () {
        if (this.isHandset()) return "handset";
        else if (this.isTablet() && this.getPageOrientation() === "portrait") return "portrait";
        else if (this.isTablet() && this.getPageOrientation() === "landscape") return "landscape";
        else return "desktop";
    },

    //> @attr splitPane.showLeftButton (boolean : false : IRW)
    // Should the +link{NavigationBar.leftButton,leftButton} be shown in the
    // +link{SplitPane.navigationBar,navigationBar}?
    // <p>
    // The default behavior is to automatically create and show a +link{SplitPane.backButton,back button}
    // as appropriate that allows transitioning back to the +link{SplitPane.navigationPane,navigationPane}
    // (tablet and handset mode) or the +link{SplitPane.listPane,listPane} (handset mode). If
    // <code>showLeftButton</code> is true, then the left button is shown <em>in addition</em>
    // to the automatically-created back button.
    //
    // @visibility external
    //<
    showLeftButton:false,

    //> @method splitPane.setShowLeftButton()
    // Show or hide the +link{NavigationBar.leftButton,leftButton} of the
    // +link{SplitPane.navigationBar,navigationBar}.
    //
    // @param visible (boolean) if <code>true</code>, the button will be shown, otherwise hidden.
    // @visibility external
    //<
    setShowLeftButton : function (show) {
        this.showLeftButton = show;
        this.updateNavigationBar();
    },

    //> @method splitPane.setLeftButtonTitle()
    // Setter for the +link{NavigationBar.leftButtonTitle,leftButtonTitle} of the
    // +link{SplitPane.navigationBar,navigationBar}.
    //
    // @param newTitle (HTMLString) new title for the left button.
    // @visibility external
    //<
    setLeftButtonTitle : function (newTitle) {
        this.leftButtonTitle = newTitle;
        this.updateNavigationBar();
    },

    

    //> @attr splitPane.showRightButton (boolean : false : IRW)
    // Should the +link{NavigationBar.rightButton,rightButton} be shown in the
    // +link{SplitPane.navigationBar,navigationBar}?
    //
    // @visibility external
    //<
    showRightButton:false,

    //> @method splitPane.setShowRightButton()
    // Show or hide the +link{NavigationBar.rightButton,rightButton} of the
    // +link{SplitPane.navigationBar,navigationBar}.
    //
    // @param visible (boolean) if <code>true</code>, the button will be shown, otherwise hidden.
    // @visibility external
    //<
    setShowRightButton : function (show) {
        this.showRightButton = show;
        this.updateNavigationBar();
    },

    //> @method splitPane.setRightButtonTitle()
    // Setter for the +link{NavigationBar.rightButtonTitle,rightButtonTitle} of the
    // +link{SplitPane.navigationBar,navigationBar}.
    //
    // @param newTitle (HTMLString) new title for the right button.
    // @visibility external
    //<
    setRightButtonTitle : function (newTitle) {
        this.rightButtonTitle = newTitle;
        this.updateNavigationBar();
    },

    

    _setNavigationPane : function (pane) {
        if (this.navigationPane != null) {
            delete this.navigationPane.splitPane;
        }
        this.navigationPane = pane;
        this.navigationPane.setWidth("100%");
        this.navigationPane.setHeight("100%");
        this.navigationPane.splitPane = this;

        if (this.isTablet() || this.isHandset()) {
            var pages = [];
            if (pane != null) pages.add(pane);
            if (this.listPane != null) pages.add(this.listPane);
            if (this.isHandset() && this.detailPane != null) pages.add(this.detailPane);
            this._pagedPanel.setPages(pages);
        }
    },

    //> @method splitPane.setNavigationPane()
    // Update the +link{SplitPane.navigationPane,navigationPane} at runtime.
    // @param pane (Canvas) new navigation pane for this widget.
    // @visibility external
    //<
    setNavigationPane : function (pane) {
        this._setNavigationPane(pane);
        if (this.currentView === "navigation") {
            this.updateUI(true);
        }
    },

    //> @method splitPane.setNavigationTitle()
    // Sets the title for the +link{SplitPane.navigationPane,navigationPane}.
    // @param title (HTMLString) new title for the navigation pane.
    // @visibility external
    //< 
    setNavigationTitle : function (title) {
        this.navigationTitle = title;
        this.updateNavigationBar();
    },

    //> @method splitPane.showNavigationPane()
    // Causes a transition to the +link{SplitPane.navigationPane,navigationPane}.
    // @visibility external
    //<
    showNavigationPane : function (fromHistoryCallback, forceUIRefresh) {
        var changed = this.currentPane != null && this.currentPane !== "navigation";
        this.currentPane = "navigation";
        // If coming from the history callback, then we need to refresh the UI because the
        // navigation title might be different.
        this.updateUI(fromHistoryCallback || forceUIRefresh);

        if (changed) {
            if (!fromHistoryCallback) {
                this._maybeAddHistoryEntry();
            }
            delete this._overriddenBackButtonTitle;
            if (this.paneChanged != null && this.isDrawn()) this.paneChanged("navigation");
        }
    },

    _hasListPane : function () {
        return this.listPane != null;
    },

    _setListPane : function (pane) {
        if (this._hasListPane()) {
            delete this.listPane.splitPane;
        }

        this.listPane = pane;
        if (pane != null) {
            this.listPane.setWidth("100%");
            this.listPane.setHeight("100%");
            this.listPane.splitPane = this;
        }

        if (this.isTablet() || this.isHandset()) {
            var pages = [];
            if (this.navigationPane != null) pages.add(this.navigationPane);
            if (pane != null) pages.add(pane);
            if (this.isHandset() && this.detailPane != null) pages.add(this.detailPane);
            this._pagedPanel.setPages(pages);
        }
    },

    //> @method splitPane.setListPane()
    // Sets a new +link{SplitPane.listPane,listPane} at runtime.
    // @param pane (Canvas) new list pane for this widget.
    // @visibility external
    //<
    setListPane : function (pane) {
        this._setListPane(pane);
        this.updateUI(true);
    },

    //> @method splitPane.showListPane()
    // Causes a transition to the +link{SplitPane.listPane,listPane}, optionally updating the
    // +link{SplitPane.setListTitle(),list title}.
    // <p>
    // If, based on the +link{SplitPane.deviceMode,deviceMode} and +link{SplitPane.pageOrientation,pageOrientation},
    // this causes the +link{SplitPane.navigationPane,navigationPane} to be hidden, the
    // +link{SplitPane.backButton,back button} will be updated with the current title of the
    // <code>navigationPane</code>, or the <code>backButtonTitle</code> passed to this method.
    // When +link{SplitPane.addHistoryEntries} is enabled and <code>backButtonTitle</code> is passed,
    // then <code>backButtonTitle</code> will be used for the back button title if the user goes
    // back to the <code>listPane</code>.
    //
    // @param [listPaneTitle] (HTMLString) optional new list title.
    // @param [backButtonTitle] (HTMLString) optional new title for the +link{SplitPane.backButton,back button}.
    // @visibility external
    //<
    showListPane : function (listPaneTitle, backButtonTitle, fromHistoryCallback, forceUIRefresh) {
        if (!this._hasListPane()) {
            this.logWarn("Attempted to show the list pane, but this SplitPane does not have a list pane. Ignoring.");
            return;
        }

        var changed = (this.currentPane !== "list");
        if (listPaneTitle != null) this.listTitle = listPaneTitle;
        if (backButtonTitle != null) this._overriddenBackButtonTitle = backButtonTitle;
        this.currentPane = "list";
        // If coming from the history callback, then we need to refresh the UI because the
        // list title might be different or there might be an overridden back button title.
        this.updateUI(fromHistoryCallback || forceUIRefresh);

        if (changed) {
            if (!fromHistoryCallback) {
                this._maybeAddHistoryEntry();
            }
            delete this._overriddenBackButtonTitle;
            if (this.paneChanged != null && this.isDrawn()) this.paneChanged("list");
        }
    },

    //> @method splitPane.setListTitle()
    // Sets the title for the +link{SplitPane.listPane,listPane}.
    // @param title (HTMLString) new title for the list pane.
    // @visibility external
    //<
    setListTitle : function (title) {
        this.listTitle = title;

        
        this.updateNavigationBar();
        this.updateListToolStrip();
        this.updateDetailToolStrip();
    },

    _setDetailPane : function (pane) {
        if (this.detailPane != null) {
            delete this.detailPane.splitPane;
        }
        this.detailPane = pane;
        this.detailPane.setWidth("100%");
        this.detailPane.setHeight("100%");
        this.detailPane.splitPane = this;

        if (this.isHandset()) {
            var pages = [];
            if (this.navigationPane != null) pages.add(this.navigationPane);
            if (this.listPane != null) pages.add(this.listPane);
            if (pane != null) pages.add(pane);
            this._pagedPanel.setPages(pages);
        }
    },

    //> @method splitPane.setDetailPane()
    // Sets a new +link{SplitPane.detailPane,detailPane} at runtime.
    // @param pane (Canvas) new detail pane for this widget.
    // @visibility external
    //<
    setDetailPane : function (pane) {
        this._setDetailPane(pane);
        this.updateUI(true);
    },

    //> @method splitPane.showDetailPane()
    // Causes a transition to the +link{SplitPane.detailPane,detailPane}, optionally updating
    // the +link{SplitPane.setDetailTitle(),detail title}.
    // <p>
    // If, based on the +link{SplitPane.deviceMode,deviceMode} and +link{SplitPane.pageOrientation,pageOrientation},
    // this causes the +link{SplitPane.navigationPane,navigationPane} or +link{SplitPane.listPane,listPane}
    // to be hidden, the +link{SplitPane.backButton,back button} will be updated
    // with the current title of the <code>navigationPane</code> or <code>listPane</code>, or the
    // <code>backButtonTitle</code> passed to this method. When +link{SplitPane.addHistoryEntries}
    // is enabled and <code>backButtonTitle</code> is passed, then <code>backButtonTitle</code>
    // will be used for the back button title if the user goes back to the <code>detailPane</code>.
    //
    // @param [detailPaneTitle] (HTMLString) optional new detail title.
    // @param [backButtonTitle] (HTMLString) optional new title for the +link{SplitPane.backButton,back button}.
    // @visibility external
    //<
    showDetailPane : function (detailPaneTitle, backButtonTitle, fromHistoryCallback, forceUIRefresh) {
        var changed = (this.currentPane !== "detail");
        if (detailPaneTitle != null) this.detailTitle = detailPaneTitle;
        if (backButtonTitle != null) this._overriddenBackButtonTitle = backButtonTitle;
        this.currentPane = "detail";
        // If coming from the history callback, then we need to refresh the UI because the
        // detail title might be different or there might be an overridden back button title.
        this.updateUI(fromHistoryCallback || forceUIRefresh);

        if (changed) {
            if (!fromHistoryCallback) {
                this._maybeAddHistoryEntry();
            }
            delete this._overriddenBackButtonTitle;
            if (this.paneChanged != null && this.isDrawn()) this.paneChanged("detail");
        }
    },

    //> @method splitPane.setDetailTitle()
    // Sets the title for the +link{SplitPane.detailPane,detailPane}.
    // @param title (HTMLString) new title for the detail pane.
    // @visibility external
    //<
    setDetailTitle : function (title) {
        this.detailTitle = title;
        // In handset mode we need to update the navigation bar
        // otherwise we'll update the detailToolStrip
        if (this.currentUIConfig === "handset") {
            if (this.currentPane === "detail") this.updateNavigationBar();
        } else {
            this.updateDetailToolStrip();
        }
    },

    //> @attr splitPane.detailNavigationControl (Canvas : null : IRWA)
    // Navigation control that appears only when the navigation pane is not showing (showing detail
    // pane on handset, or portrait mode on tablet).
    // @visibility external
    //<

    //> @method splitPane.setDetailNavigationControl()
    // Navigation control that appears only when the navigation pane is not showing (showing detail
    // pane on handset, or portrait mode on tablet).
    // @param control (Canvas) 
    // @visibility external
    //<
    setDetailNavigationControl : function (canvas) {
        this.detailNavigationControl = canvas;
        var updateUI = this.currentUIConfig !== "landscape" && this.currentPane === "detail";
        if (updateUI) this.updateUI(true);
    }
});

isc.SplitPane.registerStringMethods({
    //> @method splitPane.navigationClick()
    // Notification method fired when the user clicks the default back / forward buttons
    // on the navigation bar for this <code>SplitPane</code>.
    // @param direction (NavigationDirection) direction in which the user is attempting to navigate.
    // @visibility external
    //<
    navigationClick : "direction",

    //> @method splitPane.paneChanged()
    // Notification fired when the +link{SplitPane.currentPane} changes, either due to end-user
    // action or due to a programmatic call to +link{SplitPane.setCurrentPane(),setCurrentPane()}
    // or other APIs that can change the pane.
    // <p>
    // Note that depending on the +link{DeviceMode}, this event may not signal that any pane has
    // actually been shown or hidden, since in some modes multiple panes are shown simultaneously.
    // <p>
    // Never fires while the <code>SplitPane</code> is not drawn.
    //
    // @param newPane (CurrentPane) new +link{SplitPane.currentPane} value.
    // @visibility external
    //<
    
    paneChanged : "pane"
});
