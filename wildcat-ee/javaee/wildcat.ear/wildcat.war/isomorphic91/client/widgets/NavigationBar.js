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



//> @type NavigationDirection
// Navigation direction.
// @value "back" Back
// @value "forward" Forward
// @value "none" none
// @visibility external
//<


//> @class NavigationButton
// Specially styled Button subclass used by the +link{NavigationBar} class.
// @inheritsFrom Button
// @visibility external
// @treeLocation Client Reference/Layout/NavigationBar
//<
isc.defineClass("NavigationButton", "Button");

isc.NavigationButton.addProperties({
    height: "100%",
    padding: 5,
    autoFit: true,

    //> @attr navigationButton.baseStyle (CSSStyleName : "navButton" : IRW)
    // Default baseStyle for navigation buttons. Note that the special +link{backBaseStyle} and
    // +link{forwardBaseStyle} are applied if +link{navigationButton.direction} is set.
    // @visibility external
    //<
    baseStyle: "navButton",

    //> @attr navigationButton.backBaseStyle (CSSStyleName : "navBackButton" : IRW)
    // Base style for navigation buttons where +link{direction} is set to <code>"back"</code>
    // @visibility external
    //<
    backBaseStyle: "navBackButton",

    //> @attr navigationButton.forwardBaseStyle (CSSStyleName : "navForwardButton" : IRW)
    // Base style for navigation buttons where +link{direction} is set to <code>"forward"</code>
    // @visibility external
    //<
    forwardBaseStyle: "navForwardButton",

    //> @attr navigationButton.direction (NavigationDirection : "none" : IRW)
    // Navigation direction for this button. If set to <code>"forward"</code> or
    // <code>"back"</code> the special +link{forwardBaseStyle} or +link{backBaseStyle}
    // will be applied.
    //
    // @visibility external
    //<
    direction: "none",


    initWidget : function () {
        this.setBaseStyle(this.getBaseStyleName());
    },

    setNavigationDirection : function (direction) {
        this.direction = direction;
        this.setBaseStyle(this.getBaseStyleName());
    },

    getNavigationDirection : function () {
        return this.direction;
    },

    getBaseStyleName : function () {
        if (this.direction == "back") {
            return this.backBaseStyle;
        }
        if (this.direction == "forward") {
            return this.forwardBaseStyle;
        }
        return this.baseStyle;
    }
});


//> @class NavigationBar
// Navigation control implemented as a horizontal layout showing back and forward controls 
// and a title.
// @example navigationBar
// @inheritsFrom HLayout
// @visibility external
// @treeLocation Client Reference/Layout
//<
isc.defineClass("NavigationBar", "HLayout");

isc.NavigationBar.addProperties({
    width: "100%",
    height: 44,
    styleName:"navToolbar",

    //> @attr navigationBar.leftButtonTitle (HTMLString : "&nbsp;" : IRW)
    // +link{Button.title,Title} for the +link{NavigationBar.leftButton,leftButton}.
    // @visibility external
    //<
    leftButtonTitle:"&nbsp;",

    //> @attr navigationBar.leftButtonIcon (SCImgURL : null : IRW)
    // +link{button.icon,Icon} for the +link{NavigationBar.leftButton,leftButton}.
    // @visibility external
    //<

    //> @attr navigationBar.leftButton (AutoChild NavigationButton : null : IR)
    // The button displayed to the left of the title in this NavigationBar. By default this
    // will be a +link{NavigationButton} with +link{navigationButton.direction,direction} set
    // to <code>"back"</code>.
    //
    // @visibility external
    //<
    leftButtonDefaults: {
        _constructor: "NavigationButton",
        direction: "back",
        layoutAlign: "center"
    },

    //> @attr navigationBar.title (HTMLString : null : IRW)
    // The title to display centered in this <code>NavigationBar</code>.
    //
    // @visibility external
    //<

    //> @attr navigationBar.titleLabel (AutoChild Label : null : IR)
    // The AutoChild label used to display the +link{navigationBar.title, title} in this
    // NavigationBar.
    //
    // @visibility external
    //<
    titleLabelDefaults: {
        _constructor: "Label",
        width: "*",
        styleName:"navBarHeader",
        align: "center",
        valign: "center"
    },

    //> @attr navigationBar.rightButtonTitle (HTMLString : "&nbsp;" : IRW)
    // +link{Button.title,Title} for the +link{NavigationBar.rightButton,rightButton}.
    // @visibility external
    //<
    rightButtonTitle:"&nbsp;",

    //> @attr navigationBar.rightButtonIcon (SCImgURL : null : IRW)
    // +link{button.icon,Icon} for the +link{NavigationBar.rightButton,rightButton}.
    // @visibility external
    //<

    //> @attr navigationBar.rightButton (AutoChild NavigationButton : null : IR)
    // The button displayed to the right of the title in this NavigationBar. By default this
    // will be a +link{NavigationButton} with +link{navigationButton.direction,direction} set
    // to <code>"forward"</code>.
    //
    // @visibility external
    //<
    rightButtonDefaults: {
        _constructor: "NavigationButton",
        direction: "forward",
        layoutAlign: "center"
    },
    showRightButton:false,

    autoChildren: ["leftButton", "titleLabel", "rightButton"],

    //> @attr navigationBar.controls (Array of string or canvas : null : IRW)
    // Controls to show in the navigation bar. The auto children names
    // "leftButton", "titleLabel", "rightButton" may be used to show the standard
    // navigation bar controls, as well as any Canvases (which will be embedded directly
    // in the navigation bar).
    // @visibility internal
    //<
    // When we expose this we'll also need to update SGWT wrapper code to handle it
    controls:["leftButton", "titleLabel", "rightButton"],

    //> @method navigationBar.setControls()
    // Setter to update the set of displayed +link{navigationBar.controls} at runtime.
    // @param controls (Array of string or canvas)
    // @visibility internal
    //<
    setControls : function (controls) {
        this.controls = controls;
        var members = [];
        for (var i = 0; i < controls.length; i++) {
            var control = controls[i];
            // translate from autoChild name to live autoChild widget
            if (isc.isA.String(control)) control = this[control];
            members[i] = control;
        }
        this.setMembers(members);
    },

    initWidget : function () {
        this.Super("initWidget", arguments);

        var leftButtonDefaults = {
            click:function () {
                if (this.creator.navigationClick) this.creator.navigationClick(this.direction);
            }
        };
        if (this.leftButtonTitle != null) leftButtonDefaults.title = this.leftButtonTitle;
        if (this.leftButtonIcon != null) leftButtonDefaults.icon = this.leftButtonIcon;

        this.leftButton = this.createAutoChild("leftButton", leftButtonDefaults);
        this.setShowLeftButton(this.showLeftButton != false);

        this.titleLabel = this.createAutoChild("titleLabel", { contents: this.title });

        var rightButtonDefaults = {
            click:function () {
                if (this.creator.navigationClick) this.creator.navigationClick(this.direction);
            }
        };
        if (this.rightButtonTitle != null) rightButtonDefaults.title = this.rightButtonTitle;
        if (this.rightButtonIcon != null) rightButtonDefaults.icon = this.rightButtonIcon;
        this.rightButton = this.createAutoChild("rightButton", rightButtonDefaults);
        this.setShowRightButton(this.showRightButton != false);

        this.setControls(this.controls);
    },

    //> @method navigationBar.setTitle()
    // Updates the title for this <code>NavigationBar</code>.
    // @param newTitle (HTMLString) new title HTML.
    // @visibility external
    //<
    setTitle : function (newTitle) {
        this.title = newTitle;
        this.titleLabel.setContents(this.title);
    },

    //> @method navigationBar.setLeftButtonTitle()
    // Setter for +link{NavigationBar.leftButtonTitle,leftButtonTitle}.
    // @param newTitle (HTMLString) new title HTML for the left button.
    // @visibility external
    //<
    setLeftButtonTitle : function (newTitle) {
        this.leftButtonTitle = newTitle;
        if (this.leftButton) this.leftButton.setTitle(newTitle);
    },

    //> @method navigationBar.setLeftButtonIcon()
    // Setter for +link{NavigationBar.leftButtonIcon,leftButtonIcon}.
    // @param newIcon (SCImgURL) new icon for left button.
    // @visibility external
    //<
    setLeftButtonIcon : function (newIcon) {
        this.leftButtonIcon = newIcon;
        if (this.leftButton) this.leftButton.setIcon(newIcon);
    },

    //> @method navigationBar.setShowLeftButton()
    // Show or hide the +link{NavigationBar.leftButton,leftButton}.
    // @param visible (boolean) if true, the button will be shown, otherwise hidden.
    // @visibility external
    //<
    setShowLeftButton : function (show) {
        if (this.leftButton == null) return;
        var visible = (this.leftButton.visibility != isc.Canvas.HIDDEN);
        if (show == visible) return;
        // Calling setVisibility rather than show/hide so if the button is
        // created but not currently in our members array we don't draw it on 'show'
        this.leftButton.setVisibility(show ? isc.Canvas.INHERIT : isc.Canvas.HIDDEN);
    },

    //> @method navigationBar.setRightButtonTitle()
    // Setter for +link{NavigationBar.rightButtonTitle,rightButtonTitle}.
    // @param newTitle (HTMLString) new title HTML for the right button.
    // @visibility external
    //<
    setRightButtonTitle : function (newTitle) {
        if (this.rightButton) this.rightButton.setTitle(newTitle);
    },

    //> @method navigationBar.setRightButtonIcon()
    // Setter for +link{NavigationBar.rightButtonIcon,rightButtonIcon}.
    // @param newIcon (SCImgURL) new icon for the right button.
    // @visibility external
    //<
    setRightButtonIcon : function (newIcon) {
        this.rightButtonIcon = newIcon;
        if (this.rightButton) this.rightButton.setIcon(newIcon);
    },

    //> @method navigationBar.setShowRightButton()
    // Show or hide the +link{NavigationBar.rightButton,rightButton}.
    // @param visible (boolean) if true, the button will be shown, otherwise hidden.
    // @visibility external
    //<
    setShowRightButton : function (show) {
        if (this.rightButton == null) return;
        var visible = (this.rightButton.visibility != isc.Canvas.HIDDEN);
        if (show == visible) return;
        this.rightButton.setVisibility(show ? isc.Canvas.INHERIT : isc.Canvas.HIDDEN);
    }
});

isc.NavigationBar.registerStringMethods({
    //> @method navigationBar.navigationClick()
    // Notification method fired when the user clicks the +link{leftButton} or +link{rightButton}
    // @param direction (NavigationDirection) direction in which the user is attempting to 
    //   navigate
    // @visibility external
    //<
    navigationClick : "direction"
});
