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
    // animations brutally slow on high end (circa Q2 2014) WinPhone hardware
    animateTransitions: !isc.Browser.isMobileIE,
    skinUsesCSSTransitions: false,
    animateScrollDuration: 350,

    // All of the pages of this SplitPanePagedPanel are added as children to pagesContainer.
    // The pagesContainer has overflow:"visible" so that all of the pages are visible, but
    // clipped by the SplitPanePagedPanel handle. The animated page change transitions are
    // implemented by translating the entire pagesContainer if CSS3 transitions are supported;
    // otherwise, animateMove() is used as a fall-back.
    pagesContainerBaseStyle: "splitPanePagedPanelPagesContainer",
    pagesContainerDefaults: {
        width: "100%",
        height: "100%",
        overflow: "visible",

        getTransformCSS : function () {
            var creator = this.creator;
            if (!creator.animateTransitions || !isc.Browser._supportsCSSTransitions || !creator.skinUsesCSSTransitions) {
                return null;
            } else {
                var currentPage = creator.currentPage,
                    left;
                if (currentPage >= 0) {
                    left = -(creator.isRTL() ? creator.pages.length - 1 - currentPage : currentPage) * creator.getInnerWidth();
                } else {
                    left = 0;
                }
                // Android 2.x does not support 3D transforms.
                // http://caniuse.com/transforms3d
                return ";" + isc.Element._transformCSSName + ": translateX(" + left + "px);";
            }
        },

        _transitionEnded : function (transitionRemoved) {
            var creator = this.creator;

            if (!transitionRemoved) delete creator._animating;
            this._enableOffsetCoordsCaching();

            
            var pages = creator.pages,
                currentPage = creator.currentPage;
            for (var i = 0, len = pages.length; i < len; ++i) {
                if (i != currentPage) pages[i].setVisibility(isc.Canvas.HIDDEN);
            }

            var scrollFinishedCallback = creator._scrollFinishedCallback;
            if (scrollFinishedCallback != null) {
                delete creator._scrollFinishedCallback;
                creator.fireCallback(scrollFinishedCallback);
            }

            delete creator._animating;

            
        },

        handleTransitionEnd : function (event, eventInfo) {
            // Since 'transitionend' bubbles, need to make sure that it's our transition that
            // ended, not a descendant's.
            if (eventInfo.target === this) {
                
                this._transitionEnded(false);
            }
        },

        transitionsRemoved : function () {
            var creator = this.creator;
            if (isc.Browser._supportsCSSTransitions && creator.skinUsesCSSTransitions && creator._animating) {
                this._transitionEnded(true);
            }
        }
    },

    //> @attr splitPanePagedPanel.pages (Array of Canvas : [] : IRW)
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
    
    if (isc.Browser.isIPhone && this.animateTransitions && isc.Browser._supportsCSSTransitions && this.skinUsesCSSTransitions) {
        this._orientationChangeEventID = isc.Page.setEvent("orientationChange", this);
    }

    if (this.pages == null) this.pages = [];
    else this._addPagesToPagesContainer(this.pages);

    this.currentPage = Math.min(Math.max(0, this.currentPage), this.pages.length - 1);
    this._scrollToPage(-1);
},

destroy : function () {
    if (this._orientationChangeEventID != null) {
        isc.Page.clearEvent("orientationChange", this._orientationChangeEventID);
        delete this._orientationChangeEventID;
    }
    this.Super("destroy", arguments);
},

pageOrientationChange : function () {
    
    if (isc.Browser.isIPhone && this.animateTransitions && isc.Browser._supportsCSSTransitions && this.skinUsesCSSTransitions) {
        var pagesContainer = this.pagesContainer;
        
        if (pagesContainer.isDrawn() && pagesContainer.isVisible()) {
            pagesContainer.markForRedraw();
        }
    }
},


_needHideUsingDisplayNone : function () {
    return (isc.Browser.isAndroid && isc.Browser.isChrome) || this.Super("_needHideUsingDisplayNone", arguments);
},

_scrollToPage : function (prevCurrentPage, immediate, scrollFinishedCallback) {
    

    if (this.pagesContainer == null) return;
    immediate = !this.animateTransitions || immediate || prevCurrentPage < 0 || !this.isVisible();

    var currentPage = this.currentPage;

    var pages = this.pages;
    if (currentPage < 0 || immediate) {
        for (var i = 0; i < pages.length; ++i) {
            if (i == currentPage) pages[i].setVisibility(isc.Canvas.INHERIT);
            else pages[i].setVisibility(isc.Canvas.HIDDEN);
        }
    } else {
        // Show all pages in between the previous page and the new current page, inclusive.
        var minI = Math.min(currentPage, prevCurrentPage),
            maxI = Math.max(currentPage, prevCurrentPage);
        
        var i = 0;
        for (; i < minI; ++i) {
            pages[i].setVisibility(isc.Canvas.HIDDEN);
        }
        for (; i <= maxI; ++i) {
            pages[i].setVisibility(isc.Canvas.INHERIT);
        }
        for (; i < pages.length; ++i) {
            pages[i].setVisibility(isc.Canvas.HIDDEN);
        }
    }

    var left;
    if (currentPage >= 0) {
        left = -(this.isRTL() ? this.pages.length - 1 - currentPage : currentPage) * this.getInnerWidth();
    } else {
        left = 0;
    }

    var pagesContainer = this.pagesContainer;
    if (!this.animateTransitions || !isc.Browser._supportsCSSTransitions || !this.skinUsesCSSTransitions) {
        if (currentPage >= 0 && !immediate) {
            pagesContainer.animateMove(left, 0, {
                target: this,
                method: function (earlyFinish) {
                    // If earlyFinish is true, then a new scrollToPage animation has already started (and the
                    // appropriate pages have been shown), so don't hide them here.
                    if (!earlyFinish) {
                        // The _animating flag is reset if this is not an early finish before
                        // the callback is called to mirror handleTransitionEnd().
                        delete this._animating;

                        
                        var pages = this.pages,
                            currentPage = this.currentPage;
                        for (var i = 0, len = pages.length; i < len; ++i) {
                            if (i != currentPage) pages[i].setVisibility(isc.Canvas.HIDDEN);
                        }
                    }

                    if (scrollFinishedCallback != null) this.fireCallback(scrollFinishedCallback);

                    // Reset the _animating flag unconditionally. This covers the case where, if
                    // this is an early finish, we want to preserve the current _animating flag
                    // value while calling the callback.
                    delete this._animating;
                }
            }, this.animateScrollDuration);
            this._animating = true;
        } else {
            if (this.moveAnimation != null) this.finishAnimation(this._$move);
            pagesContainer.setLeft(left);
            if (scrollFinishedCallback != null) this.fireCallback(scrollFinishedCallback);
        }

    } else if (pagesContainer.isDrawn()) {
        var oldScrollFinishedCallback = this._scrollFinishedCallback;
        delete this._scrollFinishedCallback;
        if (oldScrollFinishedCallback != null) this.fireCallback(oldScrollFinishedCallback);

        if (currentPage >= 0 && !immediate) {
            var computedTranslateX = isc.Element._getComputedTranslateX(pagesContainer);

            // If the computed translateX (the "used" value - normally a matrix) is different,
            // then there will be a transition. Disable offset coords caching so that we don't
            // cache offsets in the middle of the transition.
            if (computedTranslateX != left) {
                pagesContainer._disableOffsetCoordsCaching();

            // Otherwise, the transition of the transform is going to be canceled (or won't start).
            // In this case, need to make sure that offset coordinate caching is re-enabled if
            // previously disabled.
            } else {
                pagesContainer._enableOffsetCoordsCaching();
            }

            pagesContainer.setStyleName(this.pagesContainerBaseStyle + "Animated");
            isc.Element._updateTransformStyle(pagesContainer, "translateX(" + left + "px)");

            if (computedTranslateX != left) {
                this._animating = true;
                this._scrollFinishedCallback = scrollFinishedCallback;
            } else {
                delete this._animating;
                if (scrollFinishedCallback != null) this.fireCallback(scrollFinishedCallback);
            }
        } else {
            // The 'transitionend' event will not fire if the transition is removed before completion.
            // https://developer.mozilla.org/en-US/docs/Web/Reference/Events/transitionend
            delete this._animating;
            pagesContainer._enableOffsetCoordsCaching();

            
            isc.Element._updateTransformStyle(pagesContainer, "translateX(0px)");
            pagesContainer.setStyleName(this.pagesContainerBaseStyle); // disable transitions
            isc.Element._updateTransformStyle(pagesContainer, "translateX(" + left + "px)");
            if (scrollFinishedCallback != null) this.fireCallback(scrollFinishedCallback);
        }

    // Otherwise, we're using CSS transitions, but the pagesContainer is not drawn yet. In this case,
    // the pagesContainer clip handle will be written out with the correct CSS transform styling.
    // Call the scrollFinishedCallback if one was provided, as effectively the scroll has finished
    // (the current state now matches what it would have been if the pagesContainer were drawn
    // and the CSS transition had completed).
    } else {
        if (scrollFinishedCallback != null) this.fireCallback(scrollFinishedCallback);
    }

    var splitPane = this._splitPane;
    if (splitPane != null && splitPane.currentUIConfig != null) {
        var activeNavigationBar = splitPane._getActiveNavigationBar();
        if (activeNavigationBar != null && isc.Browser._supportsCSSTransitions &&
            activeNavigationBar.skinUsesCSSTransitions)
        {
            activeNavigationBar._animateStateChange(true);
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
setCurrentPage : function (currentPage, immediate, scrollFinishedCallback) {
    var prevCurrentPage = this.currentPage;
    currentPage = this.currentPage = Math.min(Math.max(0, currentPage), this.pages.length - 1);
    this._scrollToPage(prevCurrentPage, immediate, scrollFinishedCallback);
},

_addPageToContainer : function (page, i, len) {
    
    this.pagesContainer.addChild(page);
    page.setRect(((this.isRTL() ? len - 1 - i : i) * 100) + "%", 0, "100%", null);
},

_addPagesToPagesContainer : function (pages) {
    for (var i = 0, len = pages.length; i < len; ++i) {
        this._addPageToContainer(pages[i], i, len);
    }
},

setPages : function (pages) {
    if (pages == null) {
        this.pages.map("deparent");
        this.pages.setLength(0);
    } else {
        var pagesToRemove,
            currentPages = this.pages;

        if (currentPages.equals(pages)) return;

        // If in screen reader mode, remove all current pages and then re-add them to ensure
        // that the DOM order matches the new order.
        if (isc.screenReader) {
            pagesToRemove = currentPages;

        } else {
            pagesToRemove = [];
            for (var i = 0, len = currentPages.length; i < len; ++i) {
                var currentPage = currentPages[i];
                if (!pages.contains(currentPage)) pagesToRemove.add(currentPage);
            }
        }
        pagesToRemove.map("deparent");
        currentPages.setArray(pages);
        this._addPagesToPagesContainer(pages);
    }
    this.setCurrentPage(this.currentPage, true);
},

resized : function (deltaX, deltaY) {
    // Fix up the translation of the pagesContainer if we were resized horizontally. Note: The
    // SplitPanePagedPanel can be resized vertically when, for example, the navigation bar
    // title is set to an overly-long string, causing the navigation bar to increase in height
    // and this SplitPanePagedPanel to decrease in height. If resized only vertically, then
    // we do not want to jump immediately to the new translation on the pagesContainer.
    if (!!deltaX) {
        this._scrollToPage(this.currentPage, true);
    }
}



});



isc.defineClass("SplitPaneSidePanel", "VLayout").addProperties({
    width: "42%",
    height: "100%",
    overflow: "hidden",
    baseStyle: "splitPaneSidePanel",
    skinUsesCSSTransitions: false,
    // animations brutally slow on high end (circa Q2 2014) WinPhone hardware
    animate: !isc.Browser.isMobileIE,
    animateShowTime: 300,
    animateShowEffectConfig: {
        effect: "slide",
        startFrom: "L"
    },
    animateHideTime: 250,
    animateHideEffectConfig: {
        effect: "slide",
        endAt: "L"
    },

    //> @attr splitPaneSidePanel.navigationBar (AutoChild NavigationBar : AutoChild : R)
    //<
    navigationBarDefaults: {
        _constructor: "NavigationBar",
        width: "100%"
    },

    //> @attr splitPaneSidePanel.pagedPanel (AutoChild SplitPanePagedPanel : AutoChild : IRW)
    //<
    pagedPanelDefaults: {
        _constructor: "SplitPanePagedPanel",
        width: "100%",
        height: "*"
    },

    //> @attr splitPaneSidePanel.onScreen (boolean : false : R)
    //<
    onScreen: false,

autoChildren: ["navigationBar"],
initWidget : function () {
    this.Super("initWidget", arguments);
    this.addAutoChildren(this.autoChildren);
    this.addAutoChild("pagedPanel", {
        _splitPane: this._splitPane
    });

    var isRTL = this.isRTL();
    this._offScreenStyleName = this.baseStyle + (isRTL ? "OffScreenRTL" : "OffScreen");
    this._onScreenStyleName = this.baseStyle + (isRTL ? "OnScreenRTL" : "OnScreen");

    this.hide();
    if (!this.animate) {
        if (isRTL) {
            this._pageResizeEvent = isc.Page.setEvent("resize", this, null, "pageResized");
        } else {
            this.setLeft(0);
        }
        this.setStyleName(isRTL ? this.baseStyle + "RTL" : this.baseStyle);
    } else {
        if (!isc.Browser._supportsCSSTransitions || !this.skinUsesCSSTransitions) {
            this.setStyleName(isRTL ? this.baseStyle + "RTL" : this.baseStyle);
        } else {
            if (isRTL) this.setLeft("100%");
            this.setStyleName(this._offScreenStyleName);
        }
    }
    this.onScreen = false;
},

destroy : function () {
    if (this._pageResizeEvent != null) {
        isc.Page.clearEvent("resize", this._pageResizeEvent);
        delete this._pageResizeEvent;
    }
    if (this._slideInTimer != null) {
        isc.Timer.clear(this._slideInTimer);
        delete this._slideInTimer;
    }

    this.Super("destroy", arguments);
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
    if (!this.animate || !isc.Browser._supportsCSSTransitions || !this.skinUsesCSSTransitions) {
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

    if (!this.animate) {
        this.show();
    } else {
        if (!isc.Browser._supportsCSSTransitions || !this.skinUsesCSSTransitions) {
            
            this.animateShow(this.animateShowEffectConfig);

        } else {
            this.setStyleName(this._onScreenStyleName);
            this.show();
            
            if (this._slideInTimer != null) {
                isc.Timer.clear(this._slideInTimer);
                this._slideInTimer = null;
            }
            if (this.isDrawn()) {
                this._slideInTimer = this.delayCall("_slideIn");
            }
        }
    }

    this.onScreen = true;
    if (isc.Canvas.ariaEnabled()) this.setAriaState("hidden", false);
},

_slideIn : function () {
    delete this._slideInTimer;
    if (this.isDrawn() && this.isVisible()) { // if undrawn, then _disableOffsetCoordsCaching() and _updateTransformStyle() are no-ops
        var clipHandle = this.getClipHandle();

        var dX,
            left;
        if (this.isRTL()) {
            dX = "-100%";
            // percentages are interpreted relative to the offsetWidth in this case.
            left = -clipHandle.offsetWidth;
        } else {
            dX = "0";
            left = 0;
        }

        var computedTranslateX = isc.Element._getComputedTranslateX(this);
        if (computedTranslateX != left) {
            this._disableOffsetCoordsCaching();
        } else {
            this._enableOffsetCoordsCaching();
        }

        isc.Element._updateTransformStyle(this, "translateX(" + dX + ")");
    }
},

slideOut : function () {
    if (!this.onScreen) return;

    if (!this.animate) {
        this.hide();
    } else {
        if (!isc.Browser._supportsCSSTransitions || !this.skinUsesCSSTransitions) {
            this.animateHide(this.animateHideEffectConfig);
        } else {
            if (this._slideInTimer != null) {
                isc.Timer.clear(this._slideInTimer);
                delete this._slideInTimer;
            }
            this.setStyleName(this._offScreenStyleName);
            if (this.isDrawn() && this.isVisible()) {
                var clipHandle = this.getClipHandle();

                var dX,
                    left;
                if (this.isRTL()) {
                    dX = "0";
                    left = 0;
                } else {
                    dX = "-100%";
                    // percentages are interpreted relative to the offsetWidth in this case.
                    left = -clipHandle.offsetWidth;
                }

                var computedTranslateX = isc.Element._getComputedTranslateX(this);
                if (computedTranslateX != left) {
                    this._disableOffsetCoordsCaching();
                } else {
                    this._enableOffsetCoordsCaching();
                }
                isc.Element._updateTransformStyle(this, "translateX(" + dX + ")");
            }
        }
    }

    this.onScreen = false;
    if (isc.Canvas.ariaEnabled()) this.setAriaState("hidden", true);
},


handleTransitionEnd : function (event, eventInfo) {
    // Since 'transitionend' bubbles, need to make sure that it's our transition that
    // ended, not a descendant's.
    if (eventInfo.target === this) {
        
        this._enableOffsetCoordsCaching();

        if (!this.onScreen) this.hide();
    }
},

onDraw : function () {
    if (!this.animate && this.isRTL()) {
        this.setLeft(isc.Page.getWidth() - this.getVisibleWidth());
    }
},

pageResized : function () {
    if (this.isDrawn() && !this.animate && this.isRTL()) {
        this.setLeft(isc.Page.getWidth() - this.getVisibleWidth());
    }
},

resized : function (deltaX, deltaY) {
    if (this.isDrawn() && !this.animate && this.isRTL()) {
        this.setLeft(isc.Page.getWidth() - this.getVisibleWidth());
    }
}

});


//> @class SplitPane
// A device- and orientation-sensitive layout that implements the common pattern of rendering
// two panes side-by-side on desktop machines and on tablets in landscape orientation,
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
// By default, bars are created as follows:
// <ul>
// <li> in <code>deviceMode:"tablet"</code> and <code>deviceMode</code> "handset", the
//      +link{SplitPane.navigationBar} is always created.
// <li> in <code>deviceMode:"desktop"</code>, the <code>navigationBar</code> is created by
//      default only if the +link{SplitPane.navigationTitle} is specified and non-empty or
//      +link{SplitPane.showRightButton} and/or +link{SplitPane.showLeftButton} is <code>true</code>,
//      or +link{SplitPane.showNavigationBar} is <code>true</code>.
// <li> in <code>deviceMode:"desktop"</code> and <code>deviceMode</code> "tablet", the
//      +link{SplitPane.detailToolStrip} is shown <em>above</em> the <code>detailPane</code>.
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
// to handle device- and orientation-aware layout. See +link{SplitPane.showNavigationBar},
// +link{SplitPane.showListToolStrip}, and +link{SplitPane.showDetailToolStrip}.
// <p>
// Note that in addition to the +link{SplitPane.navigationBar,navigationBar}, the other automatically
// created bars are also instances of +link{NavigationBar} despite the "toolStrip" naming convention.
// These controls will not generally contain navigation elements; the <code>NavigationBar</code>
// class is used for consistent styling, since the <code>navigationBar</code> appears adjacent
// to the toolstrips in some modes and orientations, so they should have the same height and
// styling.
//
// @inheritsFrom Layout
// @visibility external
// @example layoutSplitPane
// @treeLocation Client Reference/Layout
//<
isc.defineClass("SplitPane", "Layout");

isc.SplitPane.addProperties({
    overflow: "hidden",
    vertical: true,

    //> @attr splitPane.addHistoryEntries (boolean : false : IRW)
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
    addHistoryEntries: false,

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
    // @example layoutSplitPane
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

    portraitClickMaskDefaults: {
        _constructor: "Canvas",
        width: "100%",
        height: "100%",
        overflow: "hidden",

        click : function () {
            this.creator._dismissPortraitSidePanel();
        }
    },
    portraitSidePanelDefaults: {
        _constructor: "SplitPaneSidePanel",

        navigationBar_autoMaker : function (dynamicProperties) {
            var splitPane = this.creator;
            dynamicProperties = isc.addProperties({}, dynamicProperties, {
                animateStateChanges: splitPane.animateNavigationBarStateChanges,
                showLeftButton: splitPane.showBackButton,
                leftButtonConstructor: splitPane.backButtonConstructor,
                leftButtonDefaults: splitPane.backButtonDefaults,
                leftButtonProperties: splitPane.backButtonProperties
            });
            return splitPane.createAutoChild("portraitSidePanelNavigationBar", dynamicProperties);
        }
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
                creator.showListPane(null, null, null, false, true);
            } else {
                
                creator.showNavigationPane(null, false, true);
            }
        }
    },

    leftLayoutDefaults: {
        _constructor: "VLayout",
        width: 320
    },

    rightLayoutDefaults: {
        _constructor: "VLayout",
        width: "*"
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

    //> @attr splitPane.desktopNavigationBarHeight (int : 30 : IR)
    // The height of all navigation bars when +link{SplitPane.deviceMode,deviceMode} is
    // <smartclient>"desktop".</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.DeviceMode#DESKTOP}.</smartgwt>
    //
    // @visibility internal
    //<
    desktopNavigationBarHeight: 30,

    //> @attr splitPane.navigationBar (AutoChild NavigationBar : null : IR)
    // A <code>NavigationBar</code> instance managed by this <code>SplitPane</code> that is
    // placed above the +link{SplitPane.navigationPane,navigationPane}.
    // <p>
    // The following +link{group:autoChildUsage,passthroughs} apply:
    // <ul>
    // <li>+link{SplitPane.animateNavigationBarStateChanges,animateNavigationBarStateChanges}
    //     for +link{NavigationBar.animateStateChanges}
    // <li>+link{SplitPane.showRightButton,showRightButton} for +link{NavigationBar.showRightButton}
    // </ul>
    // <p>
    // Note that in +link{SplitPane.deviceMode,deviceMode}
    // <smartclient>"desktop"</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.DeviceMode#DESKTOP}</smartgwt>
    // with +link{SplitPane.showNavigationBar,showNavigationBar} unset,
    // the <code>navigationBar</code> is automatically hidden when it would be empty
    // (+link{SplitPane.navigationTitle,navigationTitle} is an empty string and
    // <code>showRightButton</code> and <code>showLeftButton</code> are both <code>false</code>).
    // The <code>navigationBar</code> will be shown if the <code>navigationTitle</code>
    // +link{SplitPane.setNavigationTitle(),is set} to a non-empty string, or
    // <code>showRightButton</code> or <code>showLeftButton</code> is set to <code>true</code>.
    // @see SplitPane.showNavigationBar
    // @visibility external
    //<
    
    navigationBarDefaults: {
        _constructor: "NavigationBar",
        autoParent: "none",
        rightPadding: 5,
        leftPadding: 5,
        defaultLayoutAlign: "center",
        

        leftButton_autoMaker : function (dynamicProperties) {
            if (this.showLeftButton == false) return null;
            return this.creator.createAutoChild("backButton", dynamicProperties);
        },
        navigationClick : function (direction) {
            var creator = this.creator;
            if (creator.navigationClick != null) creator.navigationClick(direction);
        },
        upClick : function () {
            var creator = this.creator,
                upClickFun = creator.upClick;
            if (upClickFun != null) {
                return upClickFun.apply(creator, arguments);
            }
        },
        downClick : function () {
            var creator = this.creator,
                downClickFun = creator.downClick;
            if (downClickFun != null) {
                return downClickFun.apply(creator, arguments);
            }
        }
    },

    portraitSidePanelNavigationBarDefaults: {
        _constructor: "NavigationBar",
        

        leftButton_autoMaker : function (dynamicProperties) {
            if (this.showLeftButton == false) return null;
            return this.creator.createAutoChild("backButton", dynamicProperties);
        }
    },

    //> @attr splitPane.showNavigationBar (Boolean : null : IR)
    // If set to <code>false</code>, the +link{SplitPane.navigationBar,navigationBar}
    // will not be shown. If set to <code>true</code>, the <code>navigationBar</code> will
    // always be shown, even when the +link{SplitPane.deviceMode,deviceMode} is
    // <smartclient>"desktop"</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.DeviceMode#DESKTOP}</smartgwt>
    // and the <code>navigationBar</code> would be empty.
    //
    // @visibility external
    //<
    //showNavigationBar: null,

    //> @attr splitPane.animateNavigationBarStateChanges (boolean : true : IR)
    // Whether to animate state changes of the +link{SplitPane.navigationBar,navigationBar}.
    // Enabled by default except when the browser is known to have poor animation
    // performance.
    // @see NavigationBar.animateStateChanges
    // @visibility external
    //<
    animateNavigationBarStateChanges: ((isc.Browser._supportsCSSTransitions &&
                                        !isc.Browser.isMobileIE) ||
                                       isc.Browser.isMoz),

    //> @attr splitPane.backButton (AutoChild NavigationButton : null : IR)
    // A +link{class:NavigationButton} shown to the left of the 
    // +link{splitPane.navigationTitle, title} 
    // in the +link{splitPane.navigationBar,navigationBar}. 
    // <P>
    // In +link{SplitPane.deviceMode,deviceModes} other than "desktop", this button is 
    // automatically created and allows transitioning back to the 
    // +link{SplitPane.navigationPane,navigationPane} (in tablet and handset modes) or the 
    // +link{SplitPane.listPane,listPane} (in handset mode).  In these 
    // +link{splitPane.deviceMode, deviceModes}, setting 
    // +link{splitPane.showLeftButton, showLeftButton} to true shows the 
    // +link{splitPane.leftButton, leftButton} <em>in addition to</em> the 
    // automatically-created back button.
    // <P>
    // When +link{splitPane.deviceMode, deviceMode} is "desktop", this button is never shown.
    // See +link{splitPane.showLeftButton, showLeftButton} for more information.
    // <P>
    // This button's +link{Button.title,title} is determined automatically by the 
    // <code>SplitPane</code>.  See +link{splitPane.listTitle, listTitle} and 
    // +link{splitPane.detailTitle, detailTitle}.
    //
    // @visibility external
    //<
    
    // NOTE: The SplitPane.backButton MultiAutoChild is used to create the leftButton of the
    // main navigationBar as well as the leftButton of the portraitSidePanel's navigationBar.
    // This is a bit confusing because the SplitPane leftButton is different from the leftButton's
    // of the various NavigationBar instances.
    //
    // This is done so that the SplitPane backButtons are the "leftButton" instance for the
    // purpose of automatic NavigationBar title fitting.
    backButtonDefaults: {
        _constructor: "NavigationButton",
        direction: "back",
        click : function () {
            
            if (this.parentElement._animating) return;
            
            var creator = this.creator;
            if (creator.currentPane === "detail" && creator._hasListPane() &&
                creator.currentUIConfig !== "landscape")
            {
                creator.showListPane(null, null, "back");
            } else {
                creator.showNavigationPane("back");
            }
            // Always fire the navigationClick handler if defined
            if (creator.navigationClick != null) {
                creator.navigationClick(this.direction);
            }
            return false;
        }
    },

    //> @attr splitPane.leftButton (AutoChild NavigationButton : null : IR)
    // An additional +link{NavigationButton} which may be shown to the left of the 
    // +link{SplitPane.navigationTitle, title} in the
    // +link{SplitPane.navigationBar, navigation bar}.
    // <P>
    // <b>Important note:</b> by default, this button has no 
    // +link{navigationButton.direction, direction} and does not fire the 
    // +link{splitPane.navigationClick, navigationClick} notification.  You can provide a 
    // <code>direction</code> and apply a click handler via the autoChild system.
    // @see splitPane.showLeftButton
    // @see splitPane.backButton
    // @visibility external
    //<
    leftButtonDefaults: {
        _constructor: "NavigationButton",
        direction: null,
        click : function () {
            
            if (this.parentElement._animating) return;
            
            // Always fire the navigationClick handler if defined
            if (this.creator.navigationClick != null) {
                this.creator.navigationClick(this.direction);
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
    // The title for the +link{SplitPane.navigationPane,navigationPane}, displayed in the
    // +link{SplitPane.navigationBar,navigationBar} and also used for the title of a back
    // button in some configurations.
    //
    // @setter setNavigationTitle()
    // @visibility external
    //<

    //> @attr splitPane.navigationPane (Canvas : null : IRW)
    // An arbitrary widget that is visible in all configurations when the
    // +link{SplitPane.currentPane,currentPane} is
    // <smartclient>"navigation"</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.CurrentPane#NAVIGATION}</smartgwt>
    // (it may also be visible when the <code>currentPane</code> is
    // <smartclient>"list"</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.CurrentPane#LIST}</smartgwt>
    // or
    // <smartclient>"detail").</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.CurrentPane#DETAIL}).</smartgwt>
    // <p>
    // The <code>navigationPane</code> is typically used for navigation, to initialize the
    // content of the +link{SplitPane.listPane,listPane} (when using a <code>listPane</code>)
    // or the +link{SplitPane.detailPane,detailPane}. For example, in an email application
    // the <code>navigationPane</code> pane widget could be a +link{TreeGrid} of the inboxes
    // and folders.
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

    //> @attr splitPane.showListToolStrip (Boolean : null : IR)
    // If set to <code>false</code>, the +link{SplitPane.listToolStrip,listToolStrip}
    // will not be shown.
    //
    // @visibility external
    //<
    //showListToolStrip: null,

    //> @attr splitPane.detailTitle (HTMLString : null : IRW)
    // The title for the +link{SplitPane.detailPane,detailPane}.
    //
    // @setter setDetailTitle()
    // @visibility external
    //<

    detailTitleLabelDefaults: {
        _constructor: "Label",
        height: "100%",
        align: "center",
        valign: "center",
        clipTitle: true,
        wrap: false,
        overflow: "hidden"
    },

    //> @attr splitPane.detailPane (Canvas : null : IRW)
    // The right-hand of the two panes managed by this widget, used for viewing details.
    //
    // @visibility external
    //<

    detailPaneContainerDefaults: {
        _constructor: "VLayout",
        height: "100%"
    },

    //> @attr splitPane.detailToolStrip (AutoChild NavigationBar : null : IR)
    // Toolstrip servicing the +link{SplitPane.detailPane,detailPane}.
    // <p>
    // In +link{SplitPane.deviceMode,deviceMode}
    // <smartclient>"desktop"</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.DeviceMode#DESKTOP}</smartgwt>
    // and <code>deviceMode</code>
    // <smartclient>"tablet",</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.DeviceMode#TABLET},</smartgwt>
    // the <code>detailToolStrip</code> is shown <em>above</em> the <code>detailPane</code>.
    // In +link{SplitPane.deviceMode,deviceMode}
    // <smartclient>"handset",</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.DeviceMode#HANDSET},</smartgwt>
    // the <code>detailToolStrip</code> is created <strong>only</strong> if
    // +link{SplitPane.detailToolButtons,detailToolButtons} are specified, and is placed
    // <em>underneath</em> the <code>detailPane</code>.
    //
    // @visibility external
    //<
    detailToolStripDefaults: {
        _constructor: "NavigationBar",
        rightPadding: 5,
        leftPadding: 5,
        leftButtonIcon: null,
        defaultLayoutAlign: "center"
    },

    //> @attr splitPane.showDetailToolStrip (Boolean : null : IR)
    // If set to <code>false</code>, the +link{SplitPane.detailToolStrip,detailToolStrip}
    // will not be shown.
    //
    // @visibility external
    //<
    //showDetailToolStrip: null,

    //> @attr splitPane.detailToolButtons (Array of Canvas : null : IRW)
    // <code>detailToolButtons</code> allows you to specify a set of controls that are specially
    // placed based on the +link{SplitPane.deviceMode,deviceMode} and +link{SplitPane.pageOrientation,pageOrientation}.
    // This is generally useful for a compact strip of +link{ImgButton} controls, approximately
    // 5 of which will fit comfortably using typically-sized icons and in the most space-constricted
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

    autoChildren: ["leftLayout", "rightLayout", "navigationBar", "listToolStrip", "detailToolStrip"],

    //> @attr splitPane.showMiniNav (Boolean : false : IR)
    // If true, a +link{MiniNavControl} will be shown:
    // <ul>
    // <li>In the +link{attr:navigationBar,navigationBar} when the device mode is
    //     <smartclient>"handset"</smartclient>
    //     <smartgwt>{@link com.smartgwt.client.types.DeviceMode#HANDSET}</smartgwt>
    //     and the +link{attr:currentPane,currentPane} is
    //     <smartclient>"detail".</smartclient>
    //     <smartgwt>{@link com.smartgwt.client.types.CurrentPane#DETAIL}.</smartgwt>
    // <li>In the +link{attr:detailToolStrip,detailToolStrip} when the device mode is
    //     <smartclient>"tablet"</smartclient>
    //     <smartgwt>{@link com.smartgwt.client.types.DeviceMode#TABLET}</smartgwt>
    //     and the +link{attr:pageOrientation,pageOrientation} is
    //     <smartclient>"portrait".</smartclient>
    //     <smartgwt>{@link com.smartgwt.client.types.PageOrientation#PORTRAIT}.</smartgwt>
    // </ul>
    // <p>
    // @see attr:detailNavigationControl
    // @visibility external
    //<
    showMiniNav: false,

    

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
        if (this.detailToolStrip != null) this.detailTitleLabel = this.detailToolStrip.titleLabel;

        // On tablets, we need to create the side panel right away
        if (this.isTablet()) {
            var portraitClickMask = this.portraitClickMask = this.createAutoChild("portraitClickMask", {
                visibility: "hidden"
            });
            this.addChild(portraitClickMask);
            var portraitSidePanel = this.portraitSidePanel = this.createAutoChild("portraitSidePanel", {
                _splitPane: this,
                showNavigationBar: this.showNavigationBar
            });
            this._pagedPanel = portraitSidePanel.pagedPanel;
            this.addChild(portraitSidePanel);

        // On handsets, create a paged panel to host the navigation, list, and detail panes.
        } else if (this.isHandset()) {
            this._pagedPanel = this.createAutoChild("handsetPagedPanel", {
                _splitPane: this
            });
        }

        // If initialized with the navigationPane and/or listPane and/or detailPane, resize the
        // width to "100%". If the pane's _userHeight is null, then this means that the application
        // did not specify an initial height; in such cases, default the height to "100%" as well.
        if (this.navigationPane != null) {
            this.navigationPane.resizeTo("100%", this.navigationPane._userHeight != null ? null : "100%");
            this.navigationPane.splitPane = this;
            if (this.autoNavigate && isc.isA.DataBoundComponent(this.navigationPane)) {
                this.observe(this.navigationPane, "selectionUpdated", function () {
                    this.navigateListPane();
                });
            }
        }
        if (this.listPane != null) {
            this.listPane.resizeTo("100%", this.listPane._userHeight != null ? null : "100%");
            this.listPane.splitPane = this;
            if (this.autoNavigate && isc.isA.DataBoundComponent(this.listPane)) {
                this.observe(this.listPane, "selectionUpdated", function () {
                    this.navigateDetailPane();
                });
            }
        }
        if (this.detailPane != null) {
            this.detailPane.resizeTo("100%", this.detailPane._userHeight != null ? null : "100%");
            this.detailPane.splitPane = this;
        }

        // Set an 'orientationChange' event handler if this.pageOrientation is null, as per documentation.
        if (this.pageOrientation == null) {
            this._orientationChangeEventID = isc.Page.setEvent("orientationChange", this);
        }

        this._historyIDPrefix = this.getID() + "_";
        if (this.addHistoryEntries) this._setUpDefaultHistoryManagement();

        this.pageOrientationChange();
    },

    navigationBar_autoMaker : function (dynamicProperties) {
        // Create the navigationBar AutoChild with the passthroughs applied.
        dynamicProperties = isc.addProperties({}, dynamicProperties, {
            animateStateChanges: this.animateNavigationBarStateChanges,
            showLeftButton: this.showBackButton,
            leftButtonConstructor: this.backButtonConstructor,
            leftButtonDefaults: this.backButtonDefaults,
            leftButtonProperties: this.backButtonProperties,

            showRightButton: this.showRightButton,
            // pass the rightButtonTitle through
            rightButtonTitle: this.rightButtonTitle,

            showMiniNavControl: this.showMiniNav
            
        });
        if (!this.isTablet() && !this.isHandset()) {
            
            dynamicProperties.height = this.desktopNavigationBarHeight;
            dynamicProperties.visibility = "hidden";
        }
        return this.createAutoChild("navigationBar", dynamicProperties);
    },

    listToolStrip_autoMaker : function (dynamicProperties) {
        dynamicProperties = isc.addProperties({}, this.listToolStripProperties, dynamicProperties);
        if (!this.isTablet() && !this.isHandset()) {
            
            dynamicProperties.height = this.desktopNavigationBarHeight;
        }
        return this.createAutoChild("listToolStrip", dynamicProperties);
    },

    detailToolStrip_autoMaker : function (dynamicProperties) {
        dynamicProperties = isc.addProperties({}, this.detailToolStripProperties, dynamicProperties, {
            titleLabelConstructor: this.detailTitleLabelConstructor,
            titleLabelDefaults: this.detailTitleLabelDefaults,
            titleLabelProperties: this.detailTitleLabelProperties
        });
        var uiConfiguration;
        if (this.isTablet()) {
            dynamicProperties.showMiniNavControl = this.showMiniNav;
            dynamicProperties.leftButton_autoMaker = function (dynamicProperties) {
                return (this.creator.showSidePanelButton = this.creator.createAutoChild("showSidePanelButton", dynamicProperties));
            };
        } else if (!this.isHandset()) {
            
            dynamicProperties.height = this.desktopNavigationBarHeight;
        }
        return this.createAutoChild("detailToolStrip", dynamicProperties);
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
                if (isc.History.readyForAnotherHistoryEntry()) isc.History.addHistoryEntry(id, title, data);
                else isc.Class.delayCall("addHistoryEntry", [id, title, data], 0, isc.History);
            }, isc.Page.FIRE_ONCE);
        } else {
            if (isc.History.readyForAnotherHistoryEntry()) isc.History.addHistoryEntry(id, title, data);
            else isc.Class.delayCall("addHistoryEntry", [id, title, data], 0, isc.History);
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
                this.showListPane(data.title, data._overriddenBackButtonTitle, null, true);
            } else {
                this.showDetailPane(data.title, data._overriddenBackButtonTitle, null, true);
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

    _getDetailPaneContainer : function () {
        
        var detailPaneContainer = this.detailPaneContainer;
        if (detailPaneContainer == null) {
            detailPaneContainer = this.detailPaneContainer = this.createAutoChild("detailPaneContainer");
        }
        var members = [];
        if (this.detailPane != null) {
            members.add(this.detailPane);
        }
        if (this.detailToolButtons != null && !this.detailToolButtons.isEmpty()) {
            this.updateDetailToolStrip();
            members.add(this.detailToolStrip);
        }
        detailPaneContainer.setMembers(members);
        return detailPaneContainer;
    },

    updateUI : function (forceRefresh, direction) {
        // AutoChild & pane structure:
        // setMembers() is used to change the hierarchy according to:
        //
        // "desktop" hierarchy:
        // - SplitPane (H)
        //   - leftLayout (V)
        //     - navigationBar
        //     - navigationPane
        //   - rightLayout (V)
        //     - listToolStrip
        //     - listPane
        //     - detailToolStrip
        //     - detailPane
        //
        // "landscape" hierarchy:
        // - SplitPane (H)
        //   - leftLayout (V)
        //     - navigationBar
        //     - portraitSidePanel_pagedPanel (contains either navigationPane or listPane)
        //   - rightLayout (V)
        //     - detailToolStrip
        //     - detailPane
        //
        // "portrait" hierarchy:
        // - SplitPane (V)
        //   - detailToolStrip (contains a button to reveal the side panel)
        //   - detailPane
        //
        // "handset" hierarchy:
        // - SplitPane (V)
        //   - navigationBar
        //   - handsetPagedPanel (contains either navigationPane, listPane, or detailPaneContainer)

        var prevConfig = this.currentUIConfig,
            prevPane = this._lastPane,
            config = this.currentUIConfig = this.getUIConfiguration(),
            pane = this._lastPane = this.currentPane;

        if (!forceRefresh && config === prevConfig && pane === prevPane) {
            if (config === "handset") {
                this._pagedPanel._scrollToPage(this._pagedPanel.currentPage, true);
            }
            return;
        }

        this.updateNavigationBar(direction);
        // NOTE: this.navigationBar might be null at this point if showNavigationBar is false.

        if (config === "handset") {
            this.setProperty("vertical", true);

            var pages;
            if (prevConfig !== "handset") {
                pages = [];
                if (this.navigationPane != null) pages.add(this.navigationPane);
                if (this.listPane != null) pages.add(this.listPane);
                pages.add(this._getDetailPaneContainer());
                this._pagedPanel.setPages(pages);
            } else {
                pages = this._pagedPanel.pages;
            }

            var members = [];
            if (this.navigationBar != null) members.add(this.navigationBar);
            members.add(this._pagedPanel);
            if (pane === "navigation") {
                this._pagedPanel.setCurrentPage(0, prevConfig !== "handset");
            } else if (pane === "list") {
                this._pagedPanel.setCurrentPage(1, prevConfig !== "handset");
            } else {
                
                this._pagedPanel.setCurrentPage((this._hasListPane() ? 2 : 1), prevConfig !== "handset");
            }
            this.setMembers(members);

        } else if (config === "portrait") {
            this.setProperty("vertical", true);

            this.leftLayout.removeMembers(this.leftLayout.members);

            this.portraitSidePanel.setPagedPanel(this._pagedPanel);

            this.updateDetailToolStrip();
            this.setMembers([this.detailToolStrip]);
            if (this.detailPane != null) this.addMember(this.detailPane);

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
                if (this.isDrawn() && this.isVisible()) {
                    this._engagePortraitSidePanel();
                }

            } else if (pane === "list") {
                
                this._pagedPanel.setCurrentPage(1, !this.portraitSidePanel.onScreen);
                if (this.isDrawn() && this.isVisible()) {
                    this._engagePortraitSidePanel();
                }

            } else {
                if (this.portraitSidePanel.onScreen) {
                    this._dismissPortraitSidePanel();
                }
            }

        } else if (config === "landscape") {
            this.setProperty("vertical", false);

            this.portraitSidePanel.setPagedPanel(null);

            if (this.portraitSidePanel.onScreen) {
                this.portraitSidePanel.slideOut();
            }

            this.updateDetailToolStrip();
            var members = [];
            if (this.detailToolStrip != null) members.add(this.detailToolStrip);
            if (this.detailPane != null) members.add(this.detailPane);
            this.rightLayout.setMembers(members);

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
            this.setMembers(members);

        } else {
            

            this.setProperty("vertical", false);

            var members = [];
            if (this.navigationBar != null) members.add(this.navigationBar);
            if (this.navigationPane != null) members.add(this.navigationPane);
            this.leftLayout.setMembers(members);
            this.leftLayout.setShowResizeBar(this.showResizeBars);

            this.updateListToolStrip();
            this.updateDetailToolStrip();
            members.setLength(0);
            if (this._hasListPane()) {
                if (this.listToolStrip != null) members.add(this.listToolStrip);
                
                members.add(this.listPane);
                this.listPane.setShowResizeBar(this.showResizeBars);
            }
            if (this.detailPane != null) {
                if (this.detailToolStrip != null) members.add(this.detailToolStrip);
                members.add(this.detailPane);
            }
            this.rightLayout.setMembers(members);

            this.setMembers([this.leftLayout, this.rightLayout]);
        }

        var newReverseOrder = this.isRTL() && !this.vertical;
        if (this.reverseOrder != newReverseOrder) {
            this.reverseOrder = newReverseOrder;
            this.reflow();
        }

        
    },

    _engagePortraitSidePanel : function () {
        this.portraitClickMask.show();
        this.portraitSidePanel.slideIn();
    },

    _dismissPortraitSidePanel : function () {
        this.portraitSidePanel.slideOut();
        this.portraitClickMask.hide();
    },

    updateListToolStrip : function () {
        if (this.listToolStrip == null) return;
        if (this.currentUIConfig === "desktop") {
            this.updateListTitleLabel();
            var members = [];
            if (this.listToolStrip.leftButton) {
                members.add(this.listToolStrip.leftButton);
            }
            if (this.listTitleLabel != null) members.add(this.listTitleLabel);
            if (this.listToolStrip.rightButton) {
                members.add(this.listToolStrip.rightButton);
            }
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

        var currentUIConfig = this.currentUIConfig;
        

        var newViewState = {
            showLeftButton: false,
            leftButtonTitle: null,
            title: null,
            controls: []
        };
        var controls = newViewState.controls;

        if (currentUIConfig === "handset") {
            controls.addList(this.detailToolButtons);

            this.detailToolStrip.setProperty("align", "center");

        } else if (currentUIConfig === "portrait") {
            

            newViewState.showLeftButton = true;
            newViewState.leftButtonTitle = (this.currentPane !== "navigation" && this.listPane
                                            ? this.listTitle
                                            : this.navigationTitle);
            // Use the same shortLeftButtonTitle so that the title of the showSidePanelButton
            // will not be shortened (by default, to "Back", which is not a good title for the
            // showSidePanelButton).
            newViewState.shortLeftButtonTitle = newViewState.leftButtonTitle;

            controls.add(this.showSidePanelButton);
            if (this.detailNavigationControl != null) controls.add(this.detailNavigationControl);
            controls.add("titleLabel");
            if (this.showDetailTitleLabel != false) newViewState.title = this.detailTitle;
            if (this.detailToolButtons != null) controls.addList(this.detailToolButtons);
            if (this.showMiniNav) controls.add("miniNavControl");

            this.detailToolStrip.setProperty("align", "left");

        } else {
            

            controls.add("titleLabel");
            if (this.showDetailTitleLabel != false) newViewState.title = this.detailTitle;
            if (this.detailToolButtons != null) controls.addList(this.detailToolButtons);

            this.detailToolStrip.setProperty("align", "left");
        }

        this.detailToolStrip.setViewState(newViewState);
    },

    _getActiveNavigationBar : function () {
        
        if (this.currentUIConfig === "portrait") {
            return this.portraitSidePanel.navigationBar;
        } else {
            return this.navigationBar;
        }
    },

    updateNavigationBar : function (direction) {
        var navigationBar = this._getActiveNavigationBar();
        if (navigationBar == null) return;

        var undef;
        var newViewState = {
            showLeftButton: undef,
            leftButtonTitle: undef,
            shortLeftButtonTitle: undef,
            alwaysShowLeftButtonTitle: undef,
            title: undef,
            controls: []
        };
        var controls = newViewState.controls;

        //>DEBUG
        this.logInfo("updateNavigationBar, currentPane: " + this.currentPane +
                     ", currentUI: " + this.currentUIConfig);
        //<DEBUG

        if (this.showLeftButton) {
            if (this.leftButton == null) {
                this.leftButton = this.createAutoChild("leftButton", {
                    title: this.leftButtonTitle
                    
                });
            } else {
                this.leftButton.setTitle(this.leftButtonTitle);
                
            }
        }

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
                title = (this._hasListPane() ? this.listTitle
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

            newViewState.title = title;

            var backButtonTitle;
            if (this._overriddenBackButtonTitle != null) {
                backButtonTitle = this._overriddenBackButtonTitle;
            } else {
                backButtonTitle =
                    this.isHandset() && this.currentPane === "detail" && this._hasListPane()
                        ? this.listTitle
                        : this.navigationTitle;
            }
            newViewState.leftButtonTitle = backButtonTitle;

            newViewState.showLeftButton = (this.currentUIConfig !== "portrait" || this._hasListPane());

            controls.add("leftButton");

        // default behavior - navigation bar shows navigation title and controls
        // specified by the developer (so update title, icons, visibility)
        } else {
            if (this.currentUIConfig === "desktop" && this.showNavigationBar == null &&
                !this.navigationTitle && !this.showRightButton && !this.showLeftButton)
            {
                navigationBar.hide();
                newViewState.title = isc.nbsp;
            } else {
                navigationBar.show();
                if (!navigationBar.isDrawn() && (navigationBar.parentElement == null ||
                                                 navigationBar.parentElement.isDrawn()))
                {
                    navigationBar.draw();
                }
                newViewState.title = (this.navigationTitle || isc.nbsp);
            }

            newViewState.showLeftButton = false;
            newViewState.leftButtonTitle = null;
        }

        if (this.showLeftButton) {
            controls.add(this.leftButton);
        }
        controls.add("titleLabel");
        if (this.detailNavigationControl != null) {
            controls.add(this.detailNavigationControl);
        }
        if (this.showMiniNav && this.currentUIConfig === "handset" && this.currentPane === "detail") {
            controls.add("miniNavControl");
        }
        if (this.showRightButton) {
            controls.add("rightButton");
        }

        newViewState.rightButtonTitle = this.rightButtonTitle;
        newViewState.showRightButton = this.showRightButton;

        if (this.currentUIConfig === "portrait") {
        } else if (this.currentUIConfig === "landscape") {
            var styleName = (this.navigationBarProperties && this.navigationBarProperties.styleName) ||
                            (this.navigationBarDefaults && this.navigationBarDefaults.styleName) ||
                            this.navigationBar.getClass().getInstanceProperty("styleName");
            navigationBar.setStyleName(styleName);
        }

        var navigationBarUsingCSSTransitions = (isc.Browser._supportsCSSTransitions &&
                                                navigationBar.skinUsesCSSTransitions);
        navigationBar.setViewState(newViewState, direction, navigationBarUsingCSSTransitions);
    },

    
    getUIConfiguration : function () {
        if (this.isHandset()) return "handset";
        else if (this.isTablet() && this.getPageOrientation() === "portrait") return "portrait";
        else if (this.isTablet() && this.getPageOrientation() === "landscape") return "landscape";
        else return "desktop";
    },

    //> @attr splitPane.showLeftButton (boolean : false : IRW)
    // Should the +link{splitPane.leftButton} be shown in the 
    // +link{splitPane.navigationBar, navigation bar}?
    // <p>
    // When set to true, the +link{splitPane.leftButton} is displayed to the left of the 
    // +link{splitPane.navigationTitle}, and to the right of the +link{splitPane.backButton},
    // when +link{splitPane.deviceMode} is not "desktop".
    // <P>
    // @see splitPane.leftButton
    // @see splitPane.backButton
    //
    // @visibility external
    //<
    showLeftButton:false,

    //> @method splitPane.setShowLeftButton()
    // Show or hide the +link{SplitPane.leftButton,leftButton} in the navigation bar.
    // @param show (boolean) if <code>true</code>, the <code>leftButton</code> will be shown,
    // otherwise hidden.
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
        var oldNavigationPane = this.navigationPane;

        if (oldNavigationPane != null) {
            if (oldNavigationPane === pane) return;

            delete oldNavigationPane.splitPane;
            this.ignore(oldNavigationPane, "selectionUpdated"); // will no-op if not observing
        }

        this.navigationPane = pane;
        if (pane != null) {
            pane.resizeTo("100%", pane._userHeight != null ? null : "100%");
            pane.splitPane = this;
            if (this.autoNavigate && isc.isA.DataBoundComponent(pane)) {
                this.observe(pane, "selectionUpdated", function () {
                    this.navigateListPane();
                });
            }
        }

        if (this.isTablet() || this.isHandset()) {
            var pages = [];
            if (pane != null) pages.add(pane);
            if (this.listPane != null) pages.add(this.listPane);
            if (this.isHandset()) pages.add(this._getDetailPaneContainer());
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
        this.updateUI(true);
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
    // @param [direction] (NavigationDirection) when +link{attr:animateNavigationBarStateChanges}
    // is <code>true</code>, this is the direction passed to +link{NavigationBar.setViewState()}.
    // @visibility external
    //<
    showNavigationPane : function (direction, fromHistoryCallback, forceUIRefresh) {
        var changed = this.currentPane != null && this.currentPane !== "navigation";
        this.currentPane = "navigation";
        // If coming from the history callback, then we need to refresh the UI because the
        // navigation title might be different.
        this.updateUI(fromHistoryCallback || forceUIRefresh, direction);

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
            var oldListPane = this.listPane;
            if (oldListPane === pane) return;

            delete oldListPane.splitPane;
            this.ignore(oldListPane, "selectionUpdated"); // will no-op if not observing
        }

        this.listPane = pane;
        // Since the listPane is optional, it may be set to null.
        if (pane != null) {
            pane.resizeTo("100%", pane._userHeight != null ? null : "100%");
            pane.splitPane = this;
            if (this.autoNavigate && isc.isA.DataBoundComponent(pane)) {
                this.observe(pane, "selectionUpdated", function () {
                    this.navigateDetailPane();
                });
            }
        }

        if (this.isTablet() || this.isHandset()) {
            var pages = [];
            if (this.navigationPane != null) pages.add(this.navigationPane);
            if (pane != null) pages.add(pane);
            if (this.isHandset()) pages.add(this._getDetailPaneContainer());
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
    // @param [direction] (NavigationDirection) when +link{attr:animateNavigationBarStateChanges}
    // is <code>true</code>, this is the direction passed to +link{NavigationBar.setViewState()}.
    // @visibility external
    //<
    showListPane : function (listPaneTitle, backButtonTitle, direction, fromHistoryCallback, forceUIRefresh) {
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
        this.updateUI(listPaneTitle != null || backButtonTitle != null || fromHistoryCallback || forceUIRefresh, direction);

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
        if (pane) {
            pane.resizeTo("100%", pane._userHeight != null ? null : "100%");
            pane.splitPane = this;
        }

        if (this.isHandset()) {
            var pages = [];
            if (this.navigationPane != null) pages.add(this.navigationPane);
            if (this.listPane != null) pages.add(this.listPane);
            pages.add(this._getDetailPaneContainer());
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
    // @param [detailPaneTitle] (HTMLString) optional new +link{SplitPane.detailTitle,detail title}.
    // @param [backButtonTitle] (HTMLString) optional new title for the +link{SplitPane.backButton,back button}.
    // @param [direction] (NavigationDirection) when +link{attr:animateNavigationBarStateChanges}
    // is <code>true</code>, this is the direction passed to +link{NavigationBar.setViewState()}.
    // @visibility external
    //<
    showDetailPane : function (detailPaneTitle, backButtonTitle, direction, fromHistoryCallback, forceUIRefresh) {
        var changed = (this.currentPane !== "detail");
        if (detailPaneTitle != null) this.detailTitle = detailPaneTitle;
        if (backButtonTitle != null) this._overriddenBackButtonTitle = backButtonTitle;
        this.currentPane = "detail";
        // If coming from the history callback, then we need to refresh the UI because the
        // detail title might be different or there might be an overridden back button title.
        this.updateUI(detailPaneTitle != null || backButtonTitle != null || fromHistoryCallback || forceUIRefresh, direction);

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
    // <p>
    // See also +link{showMiniNav} for a way to enable a built-in control.
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
    },

    _parsePaneTitleTemplate : function(template, pane) {
        if (!isc.isA.DataBoundComponent(pane)) return "";

        var selectedRecord = pane.getSelectedRecord();

        var variables = {
            titleField: selectedRecord == null ? "" : selectedRecord[pane.getTitleField()],
            index: selectedRecord == null ? -1 : pane.getRecordIndex(selectedRecord),
            totalRows: pane.getTotalRows(),
            record: selectedRecord
        };

        return template.evalDynamicString(this, variables);
    },

    //> @attr splitPane.listPaneTitleTemplate (HTMLString : "${titleField}" : IRW)
    // Default value chosen for +link{splitPane.setListTitle,listPaneTitle} when +link{navigateListPane()} is called.
    // <p>
    // Available variables are:
    // <ul>
    // <li> "titleField" - the value of the +link{DataSource.titleField} in the selected record from
    // the +link{navigationPane}
    // <li> "index" - position of the selected record
    // <li> "totalRows" - total number of rows in the component where the record is selected
    // <li> "record" - the entire selected Record
    // </ul>
    // @see SplitPane.detailPaneTitleTemplate
    // @example layoutSplitPane
    // @group i18nMessages
    // @visibility external
    //<
    listPaneTitleTemplate: "${titleField}",

    //> @method splitPane.setListPaneTitleTemplate()
    // Sets a new +link{SplitPane.listPaneTitleTemplate,listPaneTitleTemplate} at runtime.
    // <p>
    // By calling this method it is assumed you want the list pane title to change to the new
    // template.
    //
    // @param template (HTMLString) new template, can use HTML to be styled.
    // @visibility external
    //<
    setListPaneTitleTemplate : function (template) {
        this.listPaneTitleTemplate = template;
        this.setListTitle(this._parsePaneTitleTemplate(this.listPaneTitleTemplate, this.navigationPane));
    },

    //> @attr splitPane.detailPaneTitleTemplate (HTMLString : "${titleField}" : IRW)
    // Default value chosen for +link{SplitPane.setDetailTitle,detailPaneTitle} when +link{navigateDetailPane()} is called.
    // <p>
    // Available variables are the same as for +link{listPaneTitleTemplate}.
    // @see SplitPane.listPaneTitleTemplate
    // @example layoutSplitPane
    // @group i18nMessages
    // @visibility external
    //<
    detailPaneTitleTemplate: "${titleField}",

    //> @method splitPane.setDetailPaneTitleTemplate()
    // Sets a new +link{SplitPane.detailPaneTitleTemplate,detailPaneTitleTemplate} at runtime.
    // <p>
    // By calling this method it is assumed you want the detail pane title to change to the new
    // template.
    //
    // @param template (HTMLString) new template, can use HTML to be styled.
    // @visibility external
    //<
    setDetailPaneTitleTemplate : function (template) {
        this.detailPaneTitleTemplate = template;
        this.setDetailTitle(this._parsePaneTitleTemplate(this.detailPaneTitleTemplate, this.listPane));
    },

    //> @attr splitPane.autoNavigate (boolean : null : IR)
    // If set, the <code>SplitPane</code> will automatically monitor selection changes in the
    // +link{navigationPane} or +link{listPane}, and call +link{navigateListPane()} or
    // +link{navigateDetailPane()} when selections are changed.
    // <p>
    // If any configured panes lack DataSources or there is no DataSource relationship declared
    // between panes, <code>autoNavigate</code> does nothing.
    // @example layoutSplitPane
    // @visibility external
    //<
    autoNavigate: null,

    //> @method splitPane.navigatePane()
    // Causes the target pane component to load data and update its title based on the current
    // selection in the source pane.
    // <p>
    // Both the source pane and target pane must have a +link{DataSource}, and either:
    // <ul>
    // <li> the two DataSources must have a Many-To-One relationship declared via
    // +link{dataSourceField.foreignKey}, so that +link{listGrid.fetchRelatedData()} can be
    // used on the target pane.
    // <li> the two DataSources must be the same, so that the record selected in the source pane can
    // be displayed in the target pane via simply calling +link{detailViewer.setData(),setData()}.
    // </ul>
    // The default <code>target</code> is
    // <smartclient>"list"</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.CurrentPane#LIST}</smartgwt>
    // if the +link{SplitPane.listPane,listPane} is present,
    // otherwise
    // <smartclient>"detail".</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.CurrentPane#DETAIL}.</smartgwt>
    // <p>
    // The title applied to the target pane is based on +link{listPaneTitleTemplate} if the target
    // pane is the <code>listPane</code>, otherwise +link{detailPaneTitleTemplate}.
    // <p>
    // The source pane usually does not need to be specified: if the
    // target pane is the <code>detailPane</code>, the default source pane
    // is the <code>listPane</code> if present, otherwise the +link{navigationPane}.  If the
    // target pane is the <code>listPane</code>, the source pane is always
    // the <code>navigationPane</code>.
    //
    // @param [target] (CurrentPane) pane that should navigate
    // @param [title] (HTMLString) optional title to use for target pane. If not specified, the
    // title is based on +link{listPaneTitleTemplate} if the target pane is the <code>listPane</code>,
    // otherwise +link{detailPaneTitleTemplate}.
    // @param [source] (CurrentPane) source pane used for selection
    // @visibility external
    //<
    navigatePane : function (target, title, source) {
        var targetPane;
        if (isc.isA.Canvas(target)) {
            if (target === this.navigationPane) {
                targetPane = target;
                target = "navigation";
            } else if (target === this.listPane) {
                targetPane = target;
                target = "list";
            } else if (target === this.detailPane) {
                targetPane = target;
                target = "detail";
            } else {
                this.logWarn("Unknown target pane:" + isc.echoLeaf(target) + ". Will use the default target pane.");
                target = null;
            }

        } else {
            if (target === "navigation") {
                targetPane = this.navigationPane;
            } else if (target === "list") {
                targetPane = this.listPane;
                if (targetPane == null) {
                    this.logWarn("The listPane cannot be the target because there isn't a listPane set. Will default to the detailPane.");
                }
            } else if (target === "detail") {
                targetPane = this.detailPane;
            }
            
        }

        if (targetPane == null) {
            if (this._hasListPane()) {
                target = "list";
                targetPane = this.listPane;
            } else {
                target = "detail";
                targetPane = this.detailPane;
            }
        }

        if (targetPane == null) return;

        var sourcePane;
        if (isc.isA.Canvas(source)) {
            if (source === this.navigationPane) {
                sourcePane = source;
                source = "navigation";
            } else if (source === this.listPane) {
                sourcePane = source;
                source = "list";
            } else if (source === this.detailPane) {
                sourcePane = source;
                source = "detail";
            } else {
                this.logWarn("Unknown source pane:" + isc.echoLeaf(source) + ". Will use the default source pane.");
                source = null;
            }

        } else {
            if (source === "navigation") {
                sourcePane = this.navigationPane;
            } else if (source === "list") {
                sourcePane = this.listPane;
                if (sourcePane == null) {
                    this.logWarn("The listPane cannot be the source because there isn't a listPane set. Will use the default source pane.");
                }
            } else if (source === "detail") {
                sourcePane = this.detailPane;
            }
            
        }

        if (sourcePane == null) {
            if (target === "detail" && this._hasListPane()) {
                source = "list";
                sourcePane = this.listPane;
            } else {
                source = "navigation";
                sourcePane = this.navigationPane;
            }
        }

        if (sourcePane == null) return;

        if (!isc.isA.DataBoundComponent(targetPane) || !targetPane.getDataSource()) {
            this.logWarn("Can't navigate SplitPane without a DataSource on the target pane.");
            return;
        }

        if (!isc.isA.DataBoundComponent(sourcePane) || !sourcePane.getDataSource()) {
            this.logWarn("Can't navigate SplitPane without a DataSource on the source pane.");
            return;
        }

        var splitPane = this;
        targetPane.fetchRelatedData(sourcePane.getSelectedRecord(), sourcePane, function () {
            var titleToSet = title;

            if (target === "list") {
                if (titleToSet == null && splitPane.listPaneTitleTemplate != null) {
                    titleToSet = splitPane._parsePaneTitleTemplate(splitPane.listPaneTitleTemplate, sourcePane);
                }

                splitPane.showListPane(titleToSet, null, "forward");

            } else if (target === "detail") {
                if (titleToSet == null && splitPane.detailPaneTitleTemplate != null) {
                    titleToSet = splitPane._parsePaneTitleTemplate(splitPane.detailPaneTitleTemplate, sourcePane);
                }

                splitPane.showDetailPane(titleToSet, null, "forward");
            }
        });
    },

    //> @method splitPane.navigateListPane()
    // Calls +link{navigatePane} with the +link{listPane} as the target pane.
    // @param [title] (HTMLString) optional title to use instead of the automatically chosen one
    //
    // @visibility external
    //<
    navigateListPane : function (title) {
        this.navigatePane("list", title, "navigation");
    },

    //> @method splitPane.navigateDetailPane()
    // Calls +link{navigatePane} with the +link{detailPane} as the target pane.
    // @param [title] (HTMLString) optional title to use instead of the automatically chosen one
    //
    // @visibility external
    //<
    navigateDetailPane : function (title) {
        this.navigatePane("detail", title, "list");
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
    // @param pane (CurrentPane) new +link{SplitPane.currentPane} value.
    // @visibility external
    //<
    
    paneChanged : "pane",

    //> @method splitPane.upClick()
    // Notification method fired when the +link{SplitPane.showMiniNav,miniNav is showing} and the
    // up button on the +link{SplitPane.navigationBar,navigationBar}'s +link{MiniNavControl} is
    // clicked.
    //
    // @include NavigationBar.upClick()
    //<
    upClick : "",

    //> @method splitPane.downClick()
    // Notification method fired when the +link{SplitPane.showMiniNav,miniNav is showing} and the
    // down button on the +link{SplitPane.navigationBar,navigationBar}'s +link{MiniNavControl} is
    // clicked.
    //
    // @include NavigationBar.downClick()
    //<
    downClick : ""
});
