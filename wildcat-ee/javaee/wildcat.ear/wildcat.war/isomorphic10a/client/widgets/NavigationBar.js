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
        this.Super("initWidget", arguments);
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
    },

    _explicitlySizeTable : function () {
        return true;
    }
});

//> @class MiniNavControl
// Compact control for up/down navigation that roughly looks like an up arrowhead next to a
// down arrowhead.
//
// @inheritsFrom StretchImgButton
// @visibility external
// @treeLocation Client Reference/Layout/NavigationBar
//<
isc.defineClass("MiniNavControl", "StretchImgButton");

isc.MiniNavControl.addProperties({

    //> @attr miniNavControl.skinImgDir (URL : "images/NavigationBar" : IR)
    // @visibility external
    //<
    skinImgDir:"images/NavigationBar/",

    showDisabled: false,
    showDown: false,
    showRollOver: false,

    // suppress the label that ends up obscuring the buttons
    showTitle: false,

    //> @attr miniNavControl.upButtonSrc (SCImgURL : "[SKIN]/up.png" : IR)
    // Image used for the up arrowhead.
    //
    // @visibility external
    //<
    upButtonSrc: "[SKIN]/up.png",

    upButtonWidth: 20,
    upButtonHeight: 22,

    //> @attr miniNavControl.downButtonSrc (SCImgURL : "[SKIN]/down.png" : IR)
    // Image used for the down arrowhead.
    //
    // @visibility external
    //<
    downButtonSrc: "[SKIN]/down.png",

    downButtonWidth: 20,
    downButtonHeight: 22,

    height: 22,
    width: 66,

    _$preUp: "blank0",
    _$up: "up",
    _$postUp: "blank1",
    _$preDown: "blank2",
    _$down: "down",
    _$postDown: "blank3",
    initWidget : function () {
        this.Super("initWidget", arguments);

        this.items = [ 
            {name: this._$preUp, width: 5, height: 5},
            {name: this._$up, src: this.upButtonSrc, width: "upButtonWidth", height: "upButtonHeight"},
            {name: this._$postUp, width: "*", height: "*"},
            {name: this._$preDown, width: "*", height: "*"},
            {name: this._$down, src: this.downButtonSrc, width: "downButtonWidth", height: "downButtonHeight"},
            {name: this._$postDown, width: 5, height: 5}
        ]
    },

    click : function () {
        var partName = this.inWhichPart();
        if (partName == null) return;
        var part = this.getPart(partName);
        

        // because these images are so small, count the pre and post padding as part of the
        // click region
        if (partName === this._$preUp || partName === this._$up || this.partName === this._$postUp) {
            var upPart = (partName === this._$up ? part : this.getPart(this._$up));
            if (upPart.state !== isc.StatefulCanvas.STATE_DISABLED) {
                if (this.upClick) this.upClick();
            }
        } else {
            var downPart = (partName === this._$down ? part : this.getPart(this._$down));
            if (downPart.state !== isc.StatefulCanvas.STATE_DISABLED) {
                if (this.downClick) this.downClick();
            }
        }
    }
});

isc.MiniNavControl.registerStringMethods({
    //> @method miniNavControl.upClick()
    // Notification method fired when the up button is clicked.
    //
    // @visibility external
    //<
    upClick : "",

    //> @method miniNavControl.downClick()
    // Notification method fired when the down button is clicked.
    //
    // @visibility external
    //<
    downClick : ""
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
    overflow: "hidden",
    styleName:"navToolbar",

    //> @attr navigationBar.leftButtonTitle (HTMLString : "&nbsp;" : IRW)
    // +link{Button.title,Title} for the +link{NavigationBar.leftButton,leftButton}.
    //
    // @visibility external
    //<
    leftButtonTitle:"&nbsp;",

    //> @attr navigationBar.shortLeftButtonTitle (HTMLString : "Back" : IRW)
    // Short title to display for the left button title if there is not enough room to show the title
    // for the navigation bar.  Setting to null or an empty string ("") will avoid a shortened
    // title ever being used.  See +link{NavigationBar.title} for a full description.
    //
    // @visibility external
    //<
    
    shortLeftButtonTitle: "Back",

    //> @attr navigationBar.alwaysShowLeftButtonTitle (boolean : false : IRW)
    // If set, the left button title will never be omitted in an attempt to fit the full title.
    // See the documentation of +link{NavigationBar.title} for details.
    //
    // @visibility external
    //<
    alwaysShowLeftButtonTitle: false,

    //> @attr navigationBar.leftButtonIcon (SCImgURL : "[SKIN]back_arrow.png" : IRW)
    // +link{button.icon,Icon} for the +link{NavigationBar.leftButton,leftButton}.
    //
    // @visibility external
    //<
    leftButtonIcon: "[SKIN]back_arrow.png",

    //> @attr navigationBar.leftButton (AutoChild NavigationButton : null : IR)
    // The button displayed to the left of the title in this NavigationBar. By default this
    // will be a +link{NavigationButton} with +link{navigationButton.direction,direction} set
    // to
    // <smartclient>"back".</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.NavigationDirection#BACK}.</smartgwt>
    // <p>
    // The following +link{group:autoChildUsage,passthroughs} apply:
    // <ul>
    // <li>+link{NavigationBar.leftButtonTitle,leftButtonTitle} for +link{Button.title}</li>
    // <li>+link{NavigationBar.leftButtonIcon,leftButtonIcon} for +link{Button.icon}</li>
    // </ul>
    //
    // @visibility external
    //<
    leftButtonDefaults: {
        _constructor: "NavigationButton",
        direction: "back",
        clipTitle: true,

        click : function () {
            var creator = this.creator;
            if (creator.navigationClick != null) creator.navigationClick(this.direction);
        }
    },

    //> @attr navigationBar.title (HTMLString : null : IRW)
    // The title to display in the center of this navigation bar.
    // <p>
    // If there is not enough room for the title with the current titles of the
    // +link{NavigationBar.leftButton,left} and +link{NavigationBar.rightButton,right} buttons,
    // space will be used as follows:
    // <ul>
    // <li> if the title can be fully visible without clipping if it is placed slightly off-center, it
    // will be placed off-center, up to a maximum of +link{NavigationBar.maxCenterOffset,maxCenterOffset} pixels
    // <li> if that's not enough space, if a +link{NavigationBar.shortLeftButtonTitle,shortLeftButtonTitle}
    // is provided, it will be used in lieu of the normal left button title
    // <li> if that's still not enough space, the title of the left button will be hidden, leaving
    // only the icon.  This will be skipped if either +link{NavigationBar.alwaysShowLeftButtonTitle,alwaysShowLeftButtonTitle}
    // has been set or the button has no icon, which would leave the space blank.
    // <li> if that's still not enough space, the title text will be clipped, showing an ellipsis
    // (where browser support allows this)
    // </ul>
    //
    // @visibility external
    //<
    
    //title: null,

    //> @attr navigationBar.maxCenterOffset (int : 40 : IR)
    // Maximum amount in pixels that the title will be placed off center in an effort to avoid
    // clipping it - see +link{NavigationBar.title,title}.
    //
    // @visibility external
    //<
    maxCenterOffset: 40,

    //> @attr navigationBar.titleLabel (AutoChild Label : null : IR)
    // The AutoChild label used to display the +link{navigationBar.title, title} in this
    // NavigationBar.
    //
    // @visibility external
    //<
    titleLabelDefaults: {
        _constructor: "Label",
        height: "100%",
        styleName: "navBarHeader",
        align: "center",
        valign: "center",
        clipTitle: true,
        wrap: false,
        overflow: "hidden"
    },

    titleLabelSpacerDefaults: {
        _constructor: "LayoutSpacer",
        width: "*",
        height: 1,

        resized : function (deltaX, deltaY) {
            this.creator._autoFitTitle();
        }
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
    //rightButtonIcon: null,

    //> @attr navigationBar.rightButton (AutoChild NavigationButton : null : IR)
    // The button displayed to the right of the title in this NavigationBar. By default this
    // will be a +link{NavigationButton} with +link{navigationButton.direction,direction} set
    // to <code>"forward"</code>.
    // <p>
    // The following +link{group:autoChildUsage,passthroughs} apply:
    // <ul>
    // <li>+link{NavigationBar.rightButtonTitle,rightButtonTitle} for +link{Button.title}</li>
    // <li>+link{NavigationBar.rightButtonIcon,rightButtonIcon} for +link{Button.icon}</li>
    // </ul>
    //
    // @visibility external
    //<
    rightButtonDefaults: {
        _constructor: "NavigationButton",
        direction: "forward",
        clipTitle: true,

        click : function () {
            var creator = this.creator;
            if (creator.navigationClick != null) creator.navigationClick(this.direction);
        }
    },
    showRightButton:false,

    //> @attr navigationBar.controls (Array of string or canvas : null : IRW)
    // Controls to show in the navigation bar. The auto children names
    // "leftButton", "titleLabel", "rightButton" may be used to show the standard
    // navigation bar controls, as well as any Canvases (which will be embedded directly
    // in the navigation bar).
    // @visibility internal
    //<
    
    controls:["leftButton", "titleLabel", "rightButton"],

    //> @attr navigationBar.miniNavControl (AutoChild MiniNavControl : null : IR)
    // AutoChild of type +link{MiniNavControl}.  Not shown by default (see
    // +link{NavigationBar.showMiniNavControl,showMiniNavControl}).  Also, if a
    // +link{NavigationBar.customNavControl,customNavControl} is provided, then the
    // <code>customNavControl</code> is used instead of an automatically created
    // <code>miniNavControl</code>.
    //
    // @visibility external
    //<
    miniNavControlDefaults: {
        _constructor: "MiniNavControl",
        layoutAlign: "center",

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

    //> @attr navigationBar.showMiniNavControl (boolean : false : IR)
    // If set to <code>false</code>, then the +link{NavigationBar.miniNavControl,miniNavControl}
    // is not shown.
    //
    // @visibility external
    //<
    showMiniNavControl:false,

    //> @attr navigationBar.miniNavAlign (Alignment : "right" : IR)
    // Placement of +link{miniNavControl}, if present:
    // <ul>
    //   <li> "right" alignment places the miniNav on the far right
    //   <li> "center" alignment places the miniNav in the center, or to the right of the title
    //        if the title is present
    //   <li> "left" alignment will place the miniNav on the left, or to the right of the
    //        +link{leftButton} if its present.
    // </ul>
    // @visibility external
    //<
    miniNavAlign: "right",

    //> @attr navigationBar.customNavControl (Canvas : null : IRW)
    // An arbitrary component that will be placed where the +link{NavigationBar.miniNavControl,miniNavControl}
    // AutoChild would normally be placed (see +link{NavigationBar.miniNavAlign,miniNavAlign}).
    // @visibility external
    //<

    //> @method navigationBar.setControls()
    // Setter to update the set of displayed +link{navigationBar.controls} at runtime.
    // @param controls (Array of string or canvas)
    // @visibility internal
    //<
    setControls : function (controls) {
        this.controls = controls;
        var titleLabel = this.titleLabel,
            members = [],
            membersContainsTitleLabelSpacer = false;
        for (var i = 0; i < controls.length; i++) {
            var control = controls[i];
            // translate from autoChild name to live autoChild widget
            if (isc.isA.String(control)) control = this[control];
            if (control === titleLabel) {
                control = this.titleLabelSpacer;
            }
            if (control === this.titleLabelSpacer) membersContainsTitleLabelSpacer = true;
            if (members.contains(control)) {
                this.logWarn("The controls array contains " + isc.echo(control) + " two or more times.");
                continue;
            }
            members.add(control);
        }
        
        this.setMembers(members);
        if (membersContainsTitleLabelSpacer) {
            titleLabel.moveBelow(this.titleLabelSpacer);
        } else if (members.length > 0) {
            titleLabel.moveBelow(members[0]);
        }
    },

    initWidget : function () {
        this.Super("initWidget", arguments);

        var isRTL = this.isRTL();

        this.leftButton = this.createAutoChild("leftButton", {
            title: this.leftButtonTitle,
            icon: this.leftButtonIcon
        });
        var leftButtonMeasurer = this.leftButtonMeasurer = this.createAutoChild("leftButton", {
            top: -9999,
            left: isRTL ? 9999 : -9999,
            ariaRole: "presentation",
            ariaState: {
                hidden: true
            }
        });
        this.addChild(leftButtonMeasurer);
        var titleLabel = this.titleLabel = this.createAutoChild("titleLabel");
        this.titleLabelSpacer = this.createAutoChild("titleLabelSpacer");
        var titleLabelMeasurer = this.titleLabelMeasurer = this.createAutoChild("titleLabel", {
            top: -9999,
            left: isRTL ? 9999 : -9999,
            width: 1,
            contents: this.title,
            overflow: "visible",
            ariaRole: "presentation",
            ariaState: {
                hidden: true
            }
        });
        this.addChild(titleLabel);
        this.rightButton = this.createAutoChild("rightButton", {
            title: this.rightButtonTitle,
            icon: this.rightButtonIcon
        });

        this.setShowLeftButton(this.showLeftButton != false);
        this.setShowRightButton(this.showRightButton != false);

        // If the miniNavControl is to be shown, but it's not already in the controls array,
        // add it now.
        if (this.showMiniNavControl &&
            (this.controls == null ||
             (!this.controls.contains("miniNavControl") &&
              (this.customNavControl == null || !this.controls.contains(this.customNavControl)))))
        {
            // Copy the controls array so that we don't modify an instance default.
            var controls = this.controls;
            if (controls == null) {
                controls = this.controls = [];
            } else {
                controls = this.controls = controls.duplicate();
            }
            if (this.miniNavAlign == "left") {
                controls.addAt("miniNavControl", 1);
            } else if (this.miniNavAlign == "center") {
                controls.addAt("miniNavControl", 2);
            } else {
                controls.add("miniNavControl");
            }
        }

        if (this.customNavControl) {
            this.miniNavControl = this.customNavControl;
        } else {
            this.miniNavControl = this.createAutoChild("miniNavControl");
        }
        this.miniNavControl.setVisibility(this.showMiniNavControl ? isc.Canvas.INHERIT : isc.Canvas.HIDDEN);

        

        this.setControls(this.controls);
    },

    //> @method navigationBar.setTitle()
    // Updates the +link{NavigationBar.title,title} for this <code>NavigationBar</code>.
    // @param newTitle (HTMLString) new title HTML.
    // @visibility external
    //<
    setTitle : function (newTitle) {
        this.title = newTitle;
        this._autoFitTitle();
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

    //> @method navigationBar.setShortLeftButtonTitle()
    // Setter for +link{NavigationBar.shortLeftButtonTitle,shortLeftButtonTitle}.
    //
    // @param newShortLeftButtonTitle (HTMLString) new short title HTML.
    // @visibility external
    //<
    setShortLeftButtonTitle : function (newShortLeftButtonTitle) {
        this.shortLeftButtonTitle = newShortLeftButtonTitle;
        this._autoFitTitle();
    },

    //> @method navigationBar.setAlwaysShowLeftButtonTitle()
    // Setter for +link{NavigationBar.alwaysShowLeftButtonTitle,alwaysShowLeftButtonTitle}.
    //
    // @param newAlwaysShowLeftButtonTitle (boolean) new value for <code>alwaysShowLeftButtonTitle</code>.
    // @visibility external
    //<
    setAlwaysShowLeftButtonTitle : function (newAlwaysShowLeftButtonTitle) {
        this.alwaysShowLeftButtonTitle = newAlwaysShowLeftButtonTitle;
        this._autoFitTitle();
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
        this.showLeftButton = show;
        this.leftButton.setVisibility(show ? isc.Canvas.INHERIT : isc.Canvas.HIDDEN);
        this.reflow();
    },

    //> @method navigationBar.setRightButtonTitle()
    // Setter for +link{NavigationBar.rightButtonTitle,rightButtonTitle}.
    // @param newTitle (HTMLString) new title HTML for the right button.
    // @visibility external
    //<
    setRightButtonTitle : function (newTitle) {
        this.rightButtonTitle = newTitle;
        if (this.rightButton) this.rightButton.setTitle(newTitle);
        this.reflow();
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
        this.showRightButton = show;
        this.rightButton.setVisibility(show ? isc.Canvas.INHERIT : isc.Canvas.HIDDEN);
        this.reflow();
    },

    //> @method navigationBar.setCustomNavControl()
    // Setter to update the +link{navigationBar.customNavControl} at runtime.
    // @param controls (Array of string or canvas)
    // @visibility external
    //<
    setCustomNavControl : function (canvas) {
        this.customNavControl = canvas;
    },

    _autoFitTitle : function () {
        
        if (this.getDrawnState() == isc.Canvas.UNDRAWN) return;

        var titleLabel = this.titleLabel,
            titleLabelMeasurer = this.titleLabelMeasurer;

        titleLabelMeasurer.setContents(this.title);
        if (!titleLabelMeasurer.isDrawn()) titleLabelMeasurer.draw();
        else titleLabelMeasurer.redrawIfDirty();

        // innerWidth: width of the space that we have to work with
        var innerWidth = this.getInnerWidth();

        // These "extra" widths are widths that, for the purpose of these calculations, are fixed and
        // cannot be changed. The widths that we have control over are the widths of the leftButton
        // and title label.
        //
        // When the leftButton is actually to the left of the title label, then we have:
        // - outerLeftExtra: combined width of controls that are before the leftButton
        // - leftButtonWidth: what will be the width of the leftButton
        // - innerLeftExtra: combined width of controls that are between the leftButton and title label
        // - innerRightExtra: unused
        // - rightButtonWidth: unused
        // - outerRightExtra: combined width of the controls that are after the title label
        // Otherwise:
        // - outerLeftExtra: combined width of controls that are before the title label
        // - leftButtonWidth: unused
        // - innerLeftExtra: unused
        // - innerRightExtra: combined width of controls that are between the title label and the leftButton
        // - rightButtonWidth: what will be the width of the leftButton
        // - outerRightExtra: combined width of the controls that are after the leftButton
        var outerLeftExtra = 0,
            leftButtonWidth = 0,
            innerLeftExtra = 0,
            titleWidth = 0,
            innerRightExtra = 0,
            rightButtonWidth = 0,
            outerRightExtra = 0;

        var members = this.members.duplicate();
        if (this.showLeftButton == false) {
            members.remove(this.leftButton);
        }
        if (this.showRightButton == false) {
            members.remove(this.rightButton);
        }
        if (this.showMiniNavControl == false) {
            members.remove(this.miniNavControl);
        }

        titleWidth = titleLabelMeasurer.getVisibleWidth();

        var numMembers = members.length,
            i;

        // If we're not showing a title, then hide the titleLabel and return.
        var titleLabelIndex = members.indexOf(this.titleLabelSpacer);
        var showingLabel = titleLabelIndex >= 0 && !!this.title;
        if (!showingLabel) {
            titleLabel.hide();
            return;
        }

        var leftButtonIndex = members.indexOf(this.leftButton),
            showingLeftButton = leftButtonIndex >= 0 && this.showLeftButton != false,
            lhsWidth,
            rhsWidth;
        // If not showing the left button, there is no need to worry about the left button's impact
        // on layout, but we may still need to clip the title.
        if (!showingLeftButton) {
            for (i = 0; i < titleLabelIndex; ++i) {
                outerLeftExtra += members[i].getVisibleWidth();
            }
            for (i = titleLabelIndex + 1; i < numMembers; ++i) {
                outerRightExtra += members[i].getVisibleWidth();
            }

            lhsWidth = outerLeftExtra;
            rhsWidth = outerRightExtra;

        } else {
            var leftButtonMeasurer = this.leftButtonMeasurer;
            leftButtonMeasurer.setProperties({
                icon: this.leftButtonIcon,
                title: this.leftButtonTitle
            });
            if (!leftButtonMeasurer.isDrawn()) leftButtonMeasurer.draw();
            else leftButtonMeasurer.redrawIfDirty();
            var normalLeftButtonWidth = leftButtonMeasurer.getVisibleWidth();

            // The left button is to the left of the title label.
            if (leftButtonIndex < titleLabelIndex) {
                for (i = 0; i < leftButtonIndex; ++i) {
                    outerLeftExtra += members[i].getVisibleWidth();
                }
                for (i = leftButtonIndex + 1; i < titleLabelIndex; ++i) {
                    innerLeftExtra += members[i].getVisibleWidth();
                }
                for (i = titleLabelIndex + 1; i < numMembers; ++i) {
                    outerRightExtra += members[i].getVisibleWidth();
                }

                leftButtonWidth = normalLeftButtonWidth;

            // The left button is to the right of the title label.
            } else {
                
                for (i = 0; i < titleLabelIndex; ++i) {
                    outerLeftExtra += members[i].getVisibleWidth();
                }
                for (i = titleLabelIndex + 1; i < leftButtonIndex; ++i) {
                    innerRightExtra += members[i].getVisibleWidth();
                }
                for (i = leftButtonIndex + 1; i < numMembers; ++i) {
                    outerRightExtra += members[i].getVisibleWidth();
                }

                rightButtonWidth = normalLeftButtonWidth;
            }

            lhsWidth = outerLeftExtra + leftButtonWidth + innerLeftExtra;
            rhsWidth = innerRightExtra + rightButtonWidth + outerRightExtra;

            // Go through the cases, checking whether the left button's width should be reduced,
            // by, for example, changing its title to the shortLeftButtonTitle.

            var leftExtra,
                rightExtra;
            if (lhsWidth > rhsWidth) {
                leftExtra = lhsWidth - rhsWidth;
                rightExtra = 0;
            } else {
                leftExtra = 0;
                rightExtra = rhsWidth - lhsWidth;
            }
            var e = leftExtra + rightExtra;
            var unclippedWidth = lhsWidth + titleWidth + rhsWidth,
                r = innerWidth - unclippedWidth;

            // If the title can be fully visible without clipping if it is placed slightly off-center,
            // it will be placed off-center, up to a maximum of maxCenterOffset pixels.
            if (r >= 0 && !(this.maxCenterOffset < (e - r) / 2)) {
                this.leftButton.setTitle(this.leftButtonTitle);
            } else {
                var newLeftButtonWidth = normalLeftButtonWidth;

                // If that's not enough space, if a shortLeftButtonTitle is provided, it will be used
                // in lieu of the normal left button title.
                var shortLeftButtonWidth,
                    d;
                if (this.shortLeftButtonTitle) {
                    leftButtonMeasurer.setTitle(this.shortLeftButtonTitle);
                    leftButtonMeasurer.redrawIfDirty();
                    shortLeftButtonWidth = leftButtonMeasurer.getVisibleWidth();

                    d = shortLeftButtonWidth - normalLeftButtonWidth;
                    if (leftButtonIndex < titleLabelIndex) {
                        lhsWidth += d;
                    } else {
                        rhsWidth += d;
                    }
                    if (lhsWidth > rhsWidth) {
                        leftExtra = lhsWidth - rhsWidth;
                        rightExtra = 0;
                    } else {
                        leftExtra = 0;
                        rightExtra = rhsWidth - lhsWidth;
                    }
                    e = leftExtra + rightExtra;
                    unclippedWidth = lhsWidth + titleWidth + rhsWidth;
                    r = innerWidth - unclippedWidth;
                } else {
                    d = 0;
                }

                if (this.alwaysShowLeftButtonTitle ||
                    !this.leftButtonIcon ||
                    (shortLeftButtonWidth != null && (r >= 0 && !(this.maxCenterOffset < (e - r) / 2))))
                {
                    if (shortLeftButtonWidth == null) {
                        this.leftButton.setTitle(this.leftButtonTitle);
                    } else {
                        var isShorter = d < 0;
                        if (!isShorter) {
                            this.logWarn("The shortLeftButtonTitle:'" + this.shortLeftButtonTitle + "' is not shorter than the normal leftButton title:'" + this.leftButtonTitle + "'. The normal title will be used as it is shorter...");
                            this.leftButton.setTitle(this.leftButtonTitle);
                            if (leftButtonIndex < titleLabelIndex) {
                                lhsWidth -= d;
                            } else {
                                rhsWidth -= d;
                            }
                        } else {
                            this.leftButton.setTitle(this.shortLeftButtonTitle);
                            newLeftButtonWidth = shortLeftButtonWidth;
                        }
                    }
                } else {
                    // If that's still not enough space, the title of the left button will be hidden,
                    // leaving only the icon. This will be skipped if either alwaysShowLeftButtonTitle
                    // has been set or the button has no icon, which would leave the space blank.
                    leftButtonMeasurer.setTitle(null);
                    leftButtonMeasurer.redrawIfDirty();
                    var iconOnlyLeftButtonWidth = leftButtonMeasurer.getVisibleWidth();

                    d = iconOnlyLeftButtonWidth - (shortLeftButtonWidth != null ? shortLeftButtonWidth : normalLeftButtonWidth);
                    if (leftButtonIndex < titleLabelIndex) {
                        lhsWidth += d;
                    } else {
                        rhsWidth += d;
                    }

                    this.leftButton.setTitle(null);
                    newLeftButtonWidth = iconOnlyLeftButtonWidth;
                }

                if (leftButtonIndex < titleLabelIndex) {
                    leftButtonWidth = newLeftButtonWidth;
                } else {
                    rightButtonWidth = newLeftButtonWidth;
                }
            }
        }

        // If RTL, swap lhsWidth and rhsWidth.
        if (this.isRTL()) {
            var lhsWidthCopy = lhsWidth;
            lhsWidth = rhsWidth;
            rhsWidth = lhsWidthCopy;
        }

        var leftExtra,
            rightExtra;
        if (lhsWidth > rhsWidth) {
            leftExtra = lhsWidth - rhsWidth;
            rightExtra = 0;
        } else {
            leftExtra = 0;
            rightExtra = rhsWidth - lhsWidth;
        }
        var e = leftExtra + rightExtra;
        

        var unclippedWidth = lhsWidth + titleWidth + rhsWidth,
            r = innerWidth - unclippedWidth,
            newTitleWidth = innerWidth - rhsWidth - lhsWidth;

        if (newTitleWidth < 0) {
            titleLabel.hide();
        } else {
            titleLabel.setContents(this.title);

            // clear the prompt, if any to make sure we don't show stale data if we previously
            // overflowed and hence showed a prompt, but no longer overflow for the current
            // title and hence do not need to show a prompt
            titleLabel.setPrompt(null);

            // If r >= e, then it's possible to perfectly center the title without clipping it.
            if (r >= e) {
                titleLabel.setRightPadding(leftExtra);
                titleLabel.setLeftPadding(rightExtra);

            // Else if r >= 0, then it's still possible to place the title without clipping it,
            // but it must be offset by (e - r)/2 pixels. We may still clip the title here if
            // (e - r)/2 > this.maxCenterOffset.
            } else if (r >= 0) {
                var twiceCenterOffset = e - r,
                    twiceMaxCenterOffset = 2 * this.maxCenterOffset;
                if (twiceMaxCenterOffset < twiceCenterOffset) {
                    // factoring in the maxCenterOffset, the title will be clipped - set the
                    // prompt to the title string so the user can tap to see the full string
                    titleLabel.setPrompt(this.title);
                }
                if (rightExtra == 0) {
                    titleLabel.setLeftPadding(0);
                    titleLabel.setRightPadding(r + Math.max(0, twiceCenterOffset - twiceMaxCenterOffset));
                } else {
                    
                    twiceCenterOffset = Math.min(twiceCenterOffset, twiceMaxCenterOffset);
                    titleLabel.setLeftPadding(Math.floor((innerWidth - titleWidth - twiceCenterOffset) / 2) - lhsWidth);
                    titleLabel.setRightPadding(0);
                }

            // Else, just clear the padding. newTitleWidth is already set to the remaining
            // width, so let the browser take over figuring out how to draw the title text.
            } else {
                titleLabel.setRightPadding(0);
                titleLabel.setLeftPadding(0);

                // we overflowed the available width - set the prompt to the same string as the
                // contents of the label so the user can tap to see the full string
                titleLabel.setPrompt(this.title);
            }

            titleLabel.setRect(lhsWidth, null, newTitleWidth, null);
            titleLabel.show();
        }
    },

    _layoutChildrenDone : function () {
        this.Super("_layoutChildrenDone", arguments);
        this._autoFitTitle();
    },

    push : function (newPanel) {
        if (this._activePanelsStack == null) {
            this._activePanelsStack = [];
        }
        this._activePanelsStack.add(newPanel);
    },

    pop : function () {
        if (this._activePanelsStack == null) {
            this._activePanelsStack = [];
        }
        if (this._activePanelsStack.isEmpty()) {
            return null;
        }
        var res = this._activePanelsStack.last();
        this._activePanelsStack.setLength(this._activePanelsStack.getLength() - 1);
        return res;
    },

    setSinglePanel : function (singlePanel) {
        this._activePanelsStack = [singlePanel];
    }
});

isc.NavigationBar.registerStringMethods({
    //> @method navigationBar.navigationClick()
    // Notification method fired when the user clicks the +link{leftButton} or +link{rightButton}
    // @param direction (NavigationDirection) direction in which the user is attempting to 
    //   navigate
    // @visibility external
    //<
    navigationClick : "direction",

    //> @method navigationBar.upClick()
    // Notification method fired when the up button on the +link{NavigationBar.miniNavControl,miniNavControl}
    // is clicked.
    //
    // @include MiniNavControl.upClick()
    //<
    upClick : "",

    //> @method navigationBar.downClick()
    // Notification method fired when the down button on the +link{NavigationBar.miniNavControl,miniNavControl}
    // is clicked.
    //
    // @include MiniNavControl.downClick()
    //<
    downClick : ""
});
