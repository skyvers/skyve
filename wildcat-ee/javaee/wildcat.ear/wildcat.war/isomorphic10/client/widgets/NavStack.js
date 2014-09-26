/*
 * Isomorphic SmartClient
 * Version v10.0p_2014-09-10 (2014-09-10)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */


isc.defineClass("NavStackPagedPanel", "SplitPanePagedPanel").addProperties({
    animateScrollDuration: 300,
    pagesContainerBaseStyle: "navStackPagedPanelPagesContainer",

    push : function (widget, scrollFinishedCallback) {
        
        this.pages.add(widget);
        var newLength = this.pages.length;
        var i = newLength - 1;
        this._addPageToContainer(widget, i, newLength);
        this.setCurrentPage(i, false, scrollFinishedCallback);
        

        
    },

    pop : function (scrollFinishedCallback) {
        this.setCurrentPage(this.pages.length - 2, false, {
            target: this,
            method: function () {
                this.pages[this.pages.length - 1].deparent();
                this.pages.setLength(this.pages.length - 1);
                if (scrollFinishedCallback != null) this.fireCallback(scrollFinishedCallback);
            }
        });
    },

    setSinglePanel : function (singlePanel, scrollFinishedCallback) {
        this.setPages([singlePanel]);
        if (scrollFinishedCallback != null) this.fireCallback(scrollFinishedCallback);

        
    }
});

isc.defineClass("NavStack", "VLayout");

isc.NavStack.addProperties({

    navStackPagedPanelConstructor: "NavStackPagedPanel",

    navStackPagedPanelDefaults: {
        width: "100%",
        height: "*"
    },

    navigationBarConstructor: "NavigationBar",

    navigationBarDefaults: {
        autoParent: "none",
        hieght: 44,
        rightPadding: 5,
        leftPadding: 5,
        defaultLayoutAlign: "center",
        overflow: "hidden",
        showLeftButton: false,
        
        navigationClick : function (direction) {
            if ("back" == direction) {
                this.creator.pop();
            }
        }
    },

    initWidget : function () {
        this.Super("initWidget", arguments);
        if (this.navigationBar == null) {
            this.navigationBar = this.createAutoChild("navigationBar");
        }
        this.navStackPagedPanel = this.createAutoChild("navStackPagedPanel");
        this.setMembers([this.navigationBar, this.navStackPagedPanel]);
    },

    push : function (widget, scrollFinishedCallback) {
        if (this._isAnimating()) return;
        this.navigationBar.push(widget);
        this.navStackPagedPanel.push(widget, scrollFinishedCallback);
        if (this.navStackPagedPanel.pages.length > 1) {
            this.navigationBar.setShowLeftButton(true);
        }
    },

    pop : function (scrollFinishedCallback) {
        if (this._isAnimating()) return;
        var widget = this.navigationBar.pop();
        if (this.navStackPagedPanel.pages.length <= 2) {
            this.navigationBar.setShowLeftButton(false);
        }
        this.navStackPagedPanel.pop(scrollFinishedCallback);
    },

    setSinglePanel : function (singlePanel, scrollFinishedCallback) {
        this.navigationBar.setSinglePanel(singlePanel);
        this.navStackPagedPanel.setSinglePanel(singlePanel, scrollFinishedCallback);
        this.navigationBar.setShowLeftButton(false);
    },

    _isAnimating : function () {
        return !!this.navStackPagedPanel._animating;
    }
});
