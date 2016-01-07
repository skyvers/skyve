/*
 * Isomorphic SmartClient
 * Version SNAPSHOT_v10.1p_2015-12-10 (2015-12-10)
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


//> @object NavigationBarViewState
// Encapsulates state of a +link{NavigationBar}'s view. A <code>NavigationBarViewState</code>
// object is created to pass to +link{NavigationBar.setViewState()} so that multiple properties
// of the <code>NavigationBar</code> can be changed at once.
// @visibility external
// @treeLocation Client Reference/Layout/NavigationBar
//<
// An actual NavigationBar instance is a NavigationBarViewState object.

//> @attr NavigationBarViewState.members (Array of Canvas : null : R)
// @visibility internal
//<
//> @attr NavigationBarViewState.leftButtonIcon (SCImgURL : null : R)
// @visibility internal
//<
//> @attr NavigationBarViewState.leftButton (NavigationButton : null : R)
// @visibility internal
//<
//> @attr NavigationBarViewState.maxCenterOffset (Integer : null : R)
// @visibility internal
//<
//> @attr NavigationBarViewState.titleLabelSpacer (Canvas : null : R)
// @visibility internal
//<
//> @attr NavigationBarViewState.showMiniNavControl (Boolean : null : R)
// @visibility internal
//<
//> @attr NavigationBarViewState.miniNavControl (MiniNavControl : null : R)
// @visibility internal
//<
//> @attr NavigationBarViewState.showRightButton (Boolean : null : R)
// @visibility internal
//<
//> @attr NavigationBarViewState.rightButton (NavigationButton : null : R)
// @visibility internal
//<

// The following are the properties of a NavigationBar for which we support animating between
// values simultaneously.

//> @attr NavigationBarViewState.showLeftButton (Boolean : null : IRW)
// The new +link{NavigationBar.showLeftButton} setting. If unset, the
// <code>showLeftButton</code> setting is not changed.
// @visibility external
//<
//> @attr NavigationBarViewState.leftButtonTitle (HTMLString : null : IRW)
// The new +link{NavigationBar.leftButtonTitle} setting. If unset, the
// <code>leftButtonTitle</code> is not changed.
// @visibility external
//<
//> @attr NavigationBarViewState.shortLeftButtonTitle (HTMLString : null : IRW)
// The new +link{NavigationBar.shortLeftButtonTitle} setting. If unset, the
// <code>shortLeftButtonTitle</code> is not changed.
// @visibility external
//<
//> @attr NavigationBarViewState.alwaysShowLeftButtonTitle (Boolean : null : IRW)
// The new +link{NavigationBar.alwaysShowLeftButtonTitle} setting. If unset, the
// <code>alwaysShowLeftButtonTitle</code> setting is not changed.
// @visibility external
//<
//> @attr NavigationBarViewState.title (HTMLString : null : IRW)
// The new +link{NavigationBar.title} setting. If unset, the <code>title</code> is not changed.
// @visibility external
//<
//> @attr NavigationBarViewState.controls (Array of string or canvas : null : IRW)
// The new +link{NavigationBar.controls} setting. If unset, the <code>controls</code> array
// is not changed.
// @visibility external
//<


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

    //> @attr navigationBar.animateStateChanges (boolean : false : IRA)
    // Whether to animate a change of the view state via +link{NavigationBar.setViewState()}.
    // <p>
    // Enabling animation of state changes does have a performance impact because more components
    // need to be created by the <code>NavigationBar</code> to implement the animated transitions.
    // It is therefore recommended to leave <code>animateStateChanges</code> at its default value
    // of <code>false</code> unless +link{NavigationBar.setViewState()} might be called on this
    // <code>NavigationBar</code> instance and animation is desired.
    // <p>
    // Note also that when animation is enabled, certain AutoChild defaults and properties may
    // be used to create other AutoChildren that are internal to the animation implementation. This
    // generally does not cause an issue unless certain non-UI event handlers are added to the
    // defaults and/or properties (e.g. +link{Canvas.visibilityChanged()}, +link{Canvas.resized()}).
    // For those types of handlers, a check should be added to make sure that the handler is
    // running for the expected component.
    // @visibility external
    //<
    // <p>
    // <h3>Animation Performance</h3>
    // In modern browsers that support CSS3 transitions, the Enterprise, EnterpriseBlue, and
    // Graphite skins will use transitions for a smooth animation between states. In older
    // browsers and skins other than Enterprise, EnterpriseBlue, and Graphite, a fallback animation
    // using SmartClient's animation framework is used. This fallback can appear choppy for
    // wide <code>NavigationBar</code>s and on slow machines. To decrease the choppiness, try
    // lowering the default +link{Animation.interval} from 40 milliseconds to 17 milliseconds
    // in order to achieve around 60 frames per second.
    
    animateStateChanges: false,
    skinUsesCSSTransitions: false,
    animateStateChangeTime: 350,

    // These are the CSS style names that are applied to various elements when animating between
    // view states using CSS transitions. Limited customization is possible by changing the
    // CSS 'transition-*' styles for these style names. Note, however, that the framework requires
    // at least the element's 'opacity' to transition.
    addedFadeInStyleName: "navBarAddedFadeIn",
    removedFadeOutStyleName: "navBarRemovedFadeOut",
    fadeInStyleName: "navBarFadeIn",
    fadeOutStyleName: "navBarFadeOut",
    oldLeftButtonBackStyleName: "navBarOldLeftButtonBack",
    newLeftButtonBackStyleName: "navBarNewLeftButtonBack",
    oldLeftButtonForwardStyleName: "navBarOldLeftButtonForward",
    newLeftButtonForwardStyleName: "navBarNewLeftButtonForward",
    oldTitleBackStyleName: "navBarOldTitleBack",
    newTitleBackStyleName: "navBarNewTitleBack",
    oldTitleForwardStyleName: "navBarOldTitleForward",
    newTitleForwardStyleName: "navBarNewTitleForward",

    // An "event mask" component, the purpose of which is to intercept all UI events during a state
    // change animation.
    eventMaskPeerDefaults: {
        _showWithMaster: false,
        ariaState: {
            // Hide the presence of the event mask peer to screen readers because the event mask
            // is invisible to sighted users.
            hidden: true
        }
    },

    //> @attr navigationBar.leftButtonTitle (HTMLString : null : IRW)
    // +link{Button.title,Title} for the +link{NavigationBar.leftButton,leftButton}.
    //
    // @visibility external
    //<
    //leftButtonTitle:null,

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

    //> @attr navigationBar.showLeftButton (Boolean : null : IRW)
    // If set to <code>false</code>, then the +link{attr:leftButton,leftButton} is not shown.
    // @visibility external
    //<

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
    // @see attr:showLeftButton
    // @visibility external
    //<
    leftButtonDefaults: {
        _constructor: "NavigationButton",
        direction: "back",
        clipTitle: true,

        click : function () {
            var creator = this.creator;
            if (!creator._animating && creator.navigationClick != null) {
                creator.navigationClick(this.direction);
            }
        }
    },

    leftButtonSpacerDefaults: {
        _constructor: "LayoutSpacer",
        height: 1
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
        height: 1
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

    //> @attr navigationBar.showRightButton (Boolean : null : IRW)
    // If set to <code>false</code>, then the +link{attr:rightButton,rightButton} is not shown.
    // @visibility external
    //<

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
    // @see attr:showRightButton
    // @visibility external
    //<
    rightButtonDefaults: {
        _constructor: "NavigationButton",
        direction: "forward",
        clipTitle: true,

        click : function () {
            var creator = this.creator;
            if (!creator._animating && creator.navigationClick != null) {
                creator.navigationClick(this.direction);
            }
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
    setControls : function (controls, members) {
        this.controls = controls;
        if (members == null) members = this._controlsToMembers(this);
        
        if (!members.contains(this.leftButton)) {
            this.showLeftButton = false;
            this.leftButton.setVisibility(isc.Canvas.HIDDEN);
        }
        this.setMembers(members);
        if (members.contains(this.titleLabelSpacer)) {
            this.titleLabel.moveBelow(this.titleLabelSpacer);
        } else if (members.length > 0) {
            this.titleLabel.moveBelow(members[0]);
        }
    },
    _controlsToMembers : function (viewState) {
        var controls = viewState.controls;
        if (controls == null) return [];
        var titleLabel = this.titleLabel,
            titleLabelSpacer = this.titleLabelSpacer,
            members = [];
        
        for (var i = 0, numControls = controls.length; i < numControls; ++i) {
            var control = controls[i];
            
            if (control == null) continue;

            if ((control === "miniNavControl" || control === viewState.miniNavControl) &&
                viewState.showMiniNavControl == false)
            {
                continue;
            }

            // translate from autoChild name to live autoChild widget
            if (isc.isA.String(control)) {
                control = this[control];
                // If there is no such AutoChild, skip the control.
                if (control == null) continue;
            }
            if (control === titleLabel) {
                control = titleLabelSpacer;
            }
            if (members.contains(control)) {
                this.logWarn("The controls array contains " + isc.echo(control) + " two or more times.");
                continue;
            }
            members.add(control);
        }
        return members;
    },

    _$rightButton: "rightButton",
    initWidget : function () {
        this.Super("initWidget", arguments);

        var isRTL = this.isRTL();

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
        this.addChild(titleLabel);
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
        this.addChild(titleLabelMeasurer);
        this.rightButton = this.createAutoChild(this._$rightButton, {
            title: this.rightButtonTitle,
            icon: this.rightButtonIcon
        });

        var origShowLeftButton = this.showLeftButton;
        this.showLeftButton = true;
        // For efficiency, we avoid destroying the the leftButton when `this.showLeftButton != false'
        // changes from true to false; the leftButton is hidden instead. Otherwise, we would be destroying
        // the button each time the user navigated to the navigationPane of a SplitPane in certain
        // modes, and each time the user navigated to the first page of a NavStack.
        var leftButton = this.addAutoChild("leftButton", {
            autoParent: "none",
            title: this.leftButtonTitle,
            icon: this.leftButtonIcon,
            visibility: origShowLeftButton == false ? isc.Canvas.HIDDEN : isc.Canvas.INHERIT
        });
        if (this.controls == null ||
            (!this.controls.contains("leftButton") && !this.controls.contains(leftButton)))
        {
            origShowLeftButton = false;
        }
        this.showLeftButton = origShowLeftButton;

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

        // If animateStateChanges is enabled, create the AutoChildren that we will need to
        // animate state changes.
        if (this.animateStateChanges) {
            // If not using native CSS transitions (hence using the fallback), we need to use
            // the leftButtonSpacer to hold the place of the leftButton component in the new
            // members array.
            this.leftButtonSpacer = this.createAutoChild("leftButtonSpacer");

            var leftIconButton = this._leftIconButton = this.createAutoChild("leftButton", {
                icon: this.leftButtonIcon,
                title: isc.emptyString,
                visibility: isc.Canvas.HIDDEN
            });
            this.addChild(leftIconButton);

            var oldLeftTitleButton = this._oldLeftTitleButton = this.createAutoChild("leftButton", {
                icon: isc.Canvas._blankImgURL,
                visibility: isc.Canvas.HIDDEN
            });
            this.addChild(oldLeftTitleButton);

            var oldTitleLabel = this._oldTitleLabel = this.createAutoChild("titleLabel", {
                visibility: isc.Canvas.HIDDEN
            });
            this.addChild(oldTitleLabel);
        }

        
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
        if (this.leftButton != null) this.leftButton.setTitle(newTitle);
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
        if (this.leftButton != null && !this._animating) {
            this.leftButton.setIcon(newIcon);
        }
        
    },

    //> @method navigationBar.setShowLeftButton()
    // Show or hide the +link{NavigationBar.leftButton,leftButton}. The <code>leftButton</code>
    // must be a +link{attr:controls,control} of this <code>NavigationBar</code> or else it will
    // still be hidden.
    // @param show (Boolean) if <code>false</code>, then the <code>leftButton</code> will be
    // hidden. If unset or <code>true</code> then the <code>leftButton</code> will be shown as
    // long as it is a member of the <code>controls</code> array.
    // @visibility external
    //<
    setShowLeftButton : function (show) {
        
        if (show == null) show = true;
        if (!this.members.contains(this.leftButton)) show = false;
        this.showLeftButton = show;
        this.leftButton.setVisibility(show == false ? isc.Canvas.HIDDEN : isc.Canvas.INHERIT);
        // A call to reflow() is not necessary here because if the leftButton's visibility
        // changes, the layout is notified via childVisibilityChanged().
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
    // Show or hide the +link{NavigationBar.rightButton,rightButton}. The <code>rightButton</code>
    // must be a +link{attr:controls,control} of this <code>NavigationBar</code> or else it will
    // still be hidden.
    // @param show (Boolean) if <code>false</code>, then the <code>rightButton</code> will be
    // hidden. If unset or <code>true</code> then the <code>rightButton</code> will be shown as
    // long as it is a member of the <code>controls</code> array.
    // @visibility external
    //<
    setShowRightButton : function (show) {
        
        if (show == null) show = true;
        // `this.rightButton' might not be a member of the NavigationBar, so we need to check the
        // controls array.
        if (this.controls == null ||
            (!this.controls.contains(this._$rightButton) &&
             !this.controls.contains(this.rightButton)))
        {
            show = false;
        }
        this.showRightButton = show;
        this.rightButton.setVisibility(show ? isc.Canvas.INHERIT : isc.Canvas.HIDDEN);
    },

    //> @method navigationBar.setCustomNavControl()
    // Setter to update the +link{navigationBar.customNavControl} at runtime.
    // @param controls (Array of string or canvas)
    // @visibility external
    //<
    setCustomNavControl : function (canvas) {
        this.customNavControl = canvas;
        
    },

    //> @method navigationBar.setViewState() (A)
    // Sets multiple state attributes of this <code>NavigationBar</code> at once. If this
    // <code>NavigationBar</code> was created with +link{attr:animateStateChanges,animateStateChanges}
    // set to <code>true</code>, then the change-over to the new state attributes will be
    // animated if the direction is either
    // <smartclient>"forward"</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.NavigationDirection#FORWARD}</smartgwt>
    // or
    // <smartclient>"back".</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.NavigationDirection#BACK}.</smartgwt>
    // @param viewState (NavigationBarViewState) the new view state.
    // @param [direction] (NavigationDirection) an optional direction for animation. If
    // not specified or set to
    // <smartclient>"none"</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.NavigationDirection#NONE}</smartgwt>
    // then the state change will not be animated. The direction should be
    // <smartclient>"forward"</smartclient>
    // <smartgwt><code>NavigationDirection.FORWARD</code></smartgwt>
    // for operations that reveal new content and
    // <smartclient>"back"</smartclient>
    // <smartgwt>NavigationDirection.BACK</smartgwt>
    // for operations that reveal previously-displayed content.
    // @visibility external
    //<
    
    _$none: "none",
    _$back: "back",
    setViewState : function (viewState, direction, dontStartTransitions) {
        if (viewState == null) return;

        if (this._animating) {
            if (!isc.Browser._supportsCSSTransitions || !this.skinUsesCSSTransitions) {
                isc.Animation.finishAnimation(this._animationID);
            } else {
                this._transitionEnded(true);
            }
        }
        

        var controlsAsMembers,
            controlsDifferent = false,
            showLeftButtonDifferent = false,
            leftButtonTitleDifferent = false,
            shortLeftButtonTitleDifferent = false,
            alwaysShowLeftButtonTitleDifferent = false,
            titleDifferent = false,
            undef;

        if (viewState.controls !== undef) {
            controlsAsMembers = this._controlsToMembers(viewState);
            controlsDifferent = !controlsAsMembers.equals(this.members);

            if (!controlsAsMembers.contains(this.leftButton)) {
                viewState.showLeftButton = false;
            }
        }
        if (viewState.showLeftButton !== undef) {
            showLeftButtonDifferent = (this.showLeftButton != false) != (viewState.showLeftButton != false);
        }
        if (viewState.leftButtonTitle !== undef) {
            leftButtonTitleDifferent = this.leftButtonTitle != viewState.leftButtonTitle;
        }
        if (viewState.shortLeftButtonTitle !== undef) {
            shortLeftButtonTitleDifferent = this.shortLeftButtonTitle != viewState.shortLeftButtonTitle;
        }
        if (viewState.alwaysShowLeftButtonTitle !== undef) {
            alwaysShowLeftButtonTitleDifferent = !!this.alwaysShowLeftButtonTitle != !!viewState.alwaysShowLeftButtonTitle;
        }
        if (viewState.title !== undef) {
            titleDifferent = this.title != viewState.title;
        }

        // Return early if nothing is different.
        if (!(controlsDifferent ||
              showLeftButtonDifferent ||
              leftButtonTitleDifferent ||
              shortLeftButtonTitleDifferent ||
              alwaysShowLeftButtonTitleDifferent ||
              titleDifferent))
        {
            return;
        }

        if (direction == null || direction === this._$none ||
            !this.animateStateChanges || !this.isDrawn() || !this.isVisible())
        {
            if (controlsDifferent) {
                this.setControls(viewState.controls, controlsAsMembers);
            }
            if (showLeftButtonDifferent) {
                this.setShowLeftButton(viewState.showLeftButton);
            }
            if (leftButtonTitleDifferent) {
                this.leftButtonTitle = viewState.leftButtonTitle;
            }
            if (shortLeftButtonTitleDifferent) {
                this.shortLeftButtonTitle = viewState.shortLeftButtonTitle;
            }
            if (alwaysShowLeftButtonTitleDifferent) {
                this.alwaysShowLeftButtonTitle = !!viewState.alwaysShowLeftButtonTitle;
            }
            if (titleDifferent) {
                this.title = viewState.title;
            }

            if (this._layoutIsDirty) this.reflowNow();
            else this._autoFitTitle();

        } else {
            var useCSSTransitions = isc.Browser._supportsCSSTransitions && this.skinUsesCSSTransitions;

            

            // Create an event mask peer covering the NavigationBar to intercept all UI events.
            var eventMaskPeer = this.eventMaskPeer;
            if (eventMaskPeer == null) {
                eventMaskPeer = this.eventMaskPeer = this.createAutoChild("eventMaskPeer", {
                    left: this.getLeft(),
                    top: this.getTop(),
                    width: this.getWidth(),
                    height: this.getHeight()
                });
                this.addPeer(eventMaskPeer);
            } else {
                eventMaskPeer.setRect(this.getLeft(), this.getTop(), this.getWidth(), this.getHeight());
            }
            eventMaskPeer.moveAbove(this);
            eventMaskPeer.show();

            this._animating = true;
            if (isc.Canvas.ariaEnabled()) this.setAriaState("busy", true);

            var animationInfo = this._animationInfo = {
                direction: direction,

                showLeftButtonDifferent: showLeftButtonDifferent,
                leftButtonTitleDifferent: leftButtonTitleDifferent,
                shortLeftButtonTitleDifferent: shortLeftButtonTitleDifferent,
                alwaysShowLeftButtonTitleDifferent: alwaysShowLeftButtonTitleDifferent,
                titleDifferent: titleDifferent,
                controlsDifferent: controlsDifferent,

                oldViewState: null,
                oldMembers: null,
                oldAutoFitInfo: null,
                removedMembers: null,
                addedMembers: null,
                retainedMembers: null,
                newViewState: null,
                newMembers: null,
                newAutoFitInfo: null,
                leftButtonLogicalIconOrientation: this.leftButton._getLogicalIconOrientation(),
                leftIconButtonWidth: null
            };

            var oldMembers,
                oldViewState = animationInfo.oldViewState = {
                    members: null,
                    leftButtonIcon: this.leftButtonIcon,
                    leftButton: this.leftButton,
                    maxCenterOffset: this.maxCenterOffset,
                    titleLabelSpacer: this.titleLabelSpacer,
                    showMiniNavControl: this.showMiniNavControl,
                    miniNavControl: this.miniNavControl,
                    showRightButton: this.showRightButton,
                    rightButton: this.rightButton,

                    // these are the animatable properties
                    showLeftButton: this.showLeftButton != false,
                    leftButtonTitle: this.leftButtonTitle,
                    shortLeftButtonTitle: this.shortLeftButtonTitle,
                    alwaysShowLeftButtonTitle: this.alwaysShowLeftButtonTitle,
                    title: this.title,
                    controls: this.controls
                },
                oldShowLeftButton = oldViewState.showLeftButton,
                newMembers,
                newViewState = animationInfo.newViewState = isc.addProperties({}, oldViewState, viewState, {
                    // reset non-animatable properties
                    members: null,
                    leftButtonIcon: this.leftButtonIcon,
                    leftButton: this.leftButton,
                    maxCenterOffset: this.maxCenterOffset,
                    titleLabelSpacer: this.titleLabelSpacer,
                    showMiniNavControl: this.showMiniNavControl,
                    miniNavControl: this.miniNavControl,
                    showRightButton: this.showRightButton,
                    rightButton: this.rightButton
                }),
                newShowLeftButton = newViewState.showLeftButton;
            if (controlsDifferent) {
                oldMembers = this.members.duplicate();
                newMembers = controlsAsMembers;

                var addedMembers = animationInfo.addedMembers = [],
                    retainedMembers = animationInfo.retainedMembers = [];
                for (var i = 0, len = newMembers.length; i < len; ++i) {
                    var newMember = newMembers[i];
                    if (!oldMembers.contains(newMember)) {
                        addedMembers.add(newMember);
                        newMember.setOpacity(0);
                    } else {
                        retainedMembers.add(newMember);
                    }

                    
                    newMember.setVisibility(isc.Canvas.INHERIT);
                }

                var removedMembersLength = (oldMembers.length - retainedMembers.length),
                    removedMembers = animationInfo.removedMembers = [];
                for (var i = 0, len = oldMembers.length; i < len; ++i) {
                    var oldMember = oldMembers[i];
                    if (!newMembers.contains(oldMember)) {
                        removedMembers.add(oldMember);

                        oldMember.setVisibility(isc.Canvas.INHERIT);

                        // If we found all of the removed members, then stop iterating.
                        if (removedMembers.length == removedMembersLength) {
                            break;
                        }
                    }
                }

                

                // If the layout is dirty, then reflowNow() so that we have current sizing information
                // for the old members.
                var layoutWasDirty = this._layoutIsDirty;
                if (layoutWasDirty) {
                    this.reflowNow();
                }

                oldViewState.members = oldMembers;
                animationInfo.oldAutoFitInfo = this._calculateAutoFitInfo(oldViewState);
                if (layoutWasDirty) this._applyAutoFitInfo(animationInfo.oldAutoFitInfo);

                newViewState.members = newMembers;

                
                if (addedMembers.length > 0) {
                    if (newShowLeftButton) {
                        this.leftButton.setTitle(newViewState.leftButtonTitle);
                        this.leftButton.redrawIfDirty();
                        this.leftButton.setVisibility(isc.Canvas.INHERIT);
                    } else {
                        this.leftButton.setVisibility(isc.Canvas.HIDDEN);
                    }
                    this.setMembers(newMembers);
                    if (this._layoutIsDirty) {
                        this.reflowNow();
                    }
                    animationInfo.newAutoFitInfo = this._calculateAutoFitInfo(newViewState);
                    this.setMembers(oldMembers);
                    if (this._layoutIsDirty) {
                        this.reflowNow();
                    }
                    this._applyAutoFitInfo(animationInfo.oldAutoFitInfo);

                } else {
                    animationInfo.newAutoFitInfo = this._calculateAutoFitInfo(newViewState);
                }

            } else {
                controlsAsMembers = this.members.duplicate();
                
                if (!oldViewState.showMiniNavControl && this.miniNavControl != null) {
                    controlsAsMembers.remove(this.miniNavControl);
                }
                newMembers = oldMembers = controlsAsMembers;

                var layoutWasDirty = this._layoutIsDirty;
                if (layoutWasDirty) {
                    this.reflowNow();
                }

                oldViewState.members = oldMembers;
                animationInfo.oldAutoFitInfo = this._calculateAutoFitInfo(oldViewState);
                if (layoutWasDirty) this._applyAutoFitInfo(animationInfo.oldAutoFitInfo);

                newViewState.members = newMembers;

                // If we weren't showing the leftButton but will soon be showing the leftButton,
                // we need to make the leftButton visible so that it is factored into the new
                // auto-fit info calculation.
                layoutWasDirty = false;
                if ((!oldShowLeftButton || leftButtonTitleDifferent) &&
                    newShowLeftButton)
                {
                    this.leftButton.setTitle(newViewState.leftButtonTitle);
                    this.leftButton.redrawIfDirty();
                    this.leftButton.setVisibility(isc.Canvas.INHERIT);
                    layoutWasDirty = this._layoutIsDirty;
                    if (layoutWasDirty) this.reflowNow();
                } else if (!newShowLeftButton) {
                    this.leftButton.setVisibility(isc.Canvas.HIDDEN);
                    layoutWasDirty = this._layoutIsDirty;
                    if (layoutWasDirty) this.reflowNow();
                }

                animationInfo.newAutoFitInfo = this._calculateAutoFitInfo(newViewState);
                if (layoutWasDirty) this._applyAutoFitInfo(animationInfo.newAutoFitInfo);
            }

            var oldAutoFitInfo = animationInfo.oldAutoFitInfo,
                newAutoFitInfo = animationInfo.newAutoFitInfo;

            animationInfo.oldMembers = oldMembers;
            animationInfo.newMembers = newMembers;

            

            if (oldShowLeftButton || newShowLeftButton) {
                if (oldShowLeftButton) {
                    var oldLeftTitleButton = this._oldLeftTitleButton;
                    oldLeftTitleButton.setLeft(oldAutoFitInfo._leftButtonLeft);
                    oldLeftTitleButton.setOpacity(null);
                    oldLeftTitleButton.setTitle(oldAutoFitInfo.leftButtonTitle);
                    oldLeftTitleButton.setVisibility(isc.Canvas.INHERIT);
                    oldLeftTitleButton.redrawIfDirty();
                }
                

                this.leftButton.setVisibility(isc.Canvas.INHERIT);
                if ((oldShowLeftButton && !newShowLeftButton) ||
                    (oldShowLeftButton && newShowLeftButton))
                {
                    this.leftButton.setOpacity(null);
                } else {
                    
                    this.leftButton.setOpacity(0);
                }
                if (this.leftButtonIcon) {
                    var leftButtonMeasurer = this.leftButtonMeasurer;
                    leftButtonMeasurer.setProperties({
                        icon: this.leftButtonIcon,
                        title: isc.emptyString
                    });
                    if (!leftButtonMeasurer.isDrawn()) leftButtonMeasurer.draw();
                    else leftButtonMeasurer.redrawIfDirty();
                    animationInfo.leftIconButtonWidth = leftButtonMeasurer.getVisibleWidth();
                    leftButtonMeasurer.setTitle(isc.nbsp);
                    var slidingRight = this._isSlidingRight(newAutoFitInfo);
                    if (slidingRight) {
                        animationInfo.leftIconButtonWidth -= this.leftButton.getLeftPadding();
                        animationInfo.leftIconButtonWidth -= isc.Element._getLeftPadding(this.leftButton.getStateName());
                    } else {
                        animationInfo.leftIconButtonWidth -= this.leftButton.getRightPadding();
                        animationInfo.leftIconButtonWidth -= isc.Element._getRightPadding(this.leftButton.getStateName());
                    }
                    animationInfo.leftIconButtonWidth += this.leftButton.iconSpacing;
                    var leftIconButtonWidth = animationInfo.leftIconButtonWidth;

                    this.leftButton.setIcon(isc.Canvas._blankImgURL);
                    this._leftIconButton.setIcon(this.leftButtonIcon);

                    // Position the _leftIconButton over the leftButton so that the position of the
                    // icon would be the same if the leftButton's icon had not been changed to the
                    // blank image.

                    var leftIconOrientation = animationInfo.leftButtonLogicalIconOrientation === isc.Canvas.LEFT;
                    if ((oldShowLeftButton && !newShowLeftButton) ||
                        (oldShowLeftButton && newShowLeftButton))
                    {
                        var oldAutoFitInfo = animationInfo.oldAutoFitInfo;
                        this._leftIconButton.setLeft(oldAutoFitInfo._leftButtonLeft +
                                                     (leftIconOrientation
                                                      ? 0
                                                      : (oldAutoFitInfo._leftButtonWidth - leftIconButtonWidth)));
                        // We will either be fading the _leftIconButton out or not animating it.
                        this._leftIconButton.setOpacity(null);
                    } else {
                        
                        var newAutoFitInfo = animationInfo.newAutoFitInfo;
                        this._leftIconButton.setLeft(newAutoFitInfo._leftButtonLeft +
                                                     (leftIconOrientation
                                                      ? 0
                                                      : (newAutoFitInfo._leftButtonWidth - leftIconButtonWidth)));
                        // We will be fading the _leftIconButton in.
                        this._leftIconButton.setOpacity(0);
                    }

                    this._leftIconButton.setVisibility(isc.Canvas.INHERIT);
                }

                if (newShowLeftButton) {
                    this.leftButton.setTitle(newAutoFitInfo.leftButtonTitle);
                    this.leftButton.redrawIfDirty();
                }
            } else {
                this.leftButton.setVisibility(isc.Canvas.HIDDEN);
            }

            if (oldAutoFitInfo.titleLabelVisible) {
                var oldTitleLabel = this._oldTitleLabel;
                oldTitleLabel.setOpacity(null);
                oldTitleLabel.setContents(oldAutoFitInfo.title);
                oldTitleLabel.setRightPadding(oldAutoFitInfo.titleLabelRightPadding);
                oldTitleLabel.setLeftPadding(oldAutoFitInfo.titleLabelLeftPadding);
                
                oldTitleLabel.setRect(oldAutoFitInfo.titleLabelRect);
                oldTitleLabel.setVisibility(isc.Canvas.INHERIT);
                oldTitleLabel.redrawIfDirty();
            } else {
                this._oldTitleLabel.setVisibility(isc.Canvas.HIDDEN);
            }

            if (newAutoFitInfo.titleLabelVisible) {
                var titleLabel = this.titleLabel;
                
                titleLabel.setOpacity(0);
                titleLabel.setLeft(this._calculateNewTitleLabelLeft(animationInfo, 0));
            } else {
                this.titleLabel.setVisibility(isc.Canvas.HIDDEN);
            }

            // Set up the new members and initially hide them. At the end of the animation, the
            // new members are made visible and should be exactly in the correct place with the
            // exception of the leftButtonSpacer, which will need to be substituted at the end
            // of the animation.
            
            if (!useCSSTransitions) {
                var indexOfLeftButton = newMembers.indexOf(this.leftButton);
                if (indexOfLeftButton >= 0) {
                    newMembers[indexOfLeftButton] = this.leftButtonSpacer;
                    if (!newViewState.showLeftButton) this.leftButtonSpacer.setVisibility(isc.Canvas.HIDDEN);
                    else {
                        this.leftButtonSpacer.setVisibility(isc.Canvas.INHERIT);
                        this.leftButtonSpacer.setWidth(animationInfo.newAutoFitInfo._leftButtonWidth);
                    }
                }
            }
            this.setMembers(newMembers);
            if (this._layoutIsDirty) {
                this.reflowNow();
            }
            this._applyAutoFitInfo(animationInfo.newAutoFitInfo);

            if (newViewState.showLeftButton) {
                var leftButton = this.leftButton;
                
                leftButton.setVisibility(isc.Canvas.INHERIT);
                leftButton.setOpacity(0);
                this.addChild(leftButton);
            } else {
                this.leftButton.setVisibility(isc.Canvas.HIDDEN);
            }

            // Transfer removed members to children.
            var removedMembers = animationInfo.removedMembers;
            if (removedMembers != null) {
                for (var i = 0, len = removedMembers.length; i < len; ++i) {
                    var removedMember = removedMembers[i];
                    
                    removedMember.setOpacity(null);
                    this.addChild(removedMember);
                    if (!removedMember.isDrawn()) removedMember.draw();
                }
            }

            if (!useCSSTransitions) {
                this._animationID = isc.Animation.registerAnimation(this._fireAnimationStateChange, this.animateStateChangeTime, null, this);
            } else {
                var transitioningElements = animationInfo.transitioningElements = [],
                    backDirection = animationInfo.direction === this._$back,
                    slidingRight = this._isSlidingRight(newAutoFitInfo);

                this._leftIconButton._origStyleName = this._leftIconButton.styleName;
                this._oldLeftTitleButton._origStyleName = this._oldLeftTitleButton.styleName;
                this.leftButton._origStyleName = this.leftButton.styleName;
                this._oldTitleLabel._origStyleName = this._oldTitleLabel.styleName;
                this.titleLabel._origStyleName = this.titleLabel.styleName;

                if (!oldViewState.showLeftButton && newViewState.showLeftButton) {
                    transitioningElements.add(this._leftIconButton);
                } else if (oldViewState.showLeftButton && !newViewState.showLeftButton) {
                    transitioningElements.add(this._leftIconButton);
                }

                if (oldViewState.showLeftButton) {
                    if ((backDirection
                         ? animationInfo.newAutoFitInfo.titleLabelVisible
                         : animationInfo.oldAutoFitInfo.titleLabelVisible) &&
                        (newViewState.showLeftButton || oldViewState.showLeftButton))
                    {
                        isc.Element._updateTransformStyle(this._oldLeftTitleButton, "translateX(0px)", null, true);
                    }
                    transitioningElements.add(this._oldLeftTitleButton);
                }

                if (newViewState.showLeftButton) {
                    if ((backDirection
                         ? animationInfo.newAutoFitInfo.titleLabelVisible
                         : animationInfo.oldAutoFitInfo.titleLabelVisible) &&
                        slidingRight == (animationInfo.leftButtonLogicalIconOrientation === isc.Canvas.RIGHT))
                    {
                        
                        var dX = (this._calculateNewLeftButtonLeft(animationInfo, 0) -
                                  this._calculateNewLeftButtonLeft(animationInfo, 1));
                        isc.Element._updateTransformStyle(this.leftButton, "translateX(" + dX + "px)", null, true);
                    }
                    transitioningElements.add(this.leftButton);
                }

                if (animationInfo.oldAutoFitInfo.titleLabelVisible) {
                    
                    if (animationInfo.newAutoFitInfo._leftButtonIndex >= 0 || animationInfo.oldAutoFitInfo._leftButtonIndex >= 0) {
                        isc.Element._updateTransformStyle(this._oldTitleLabel, "translateX(0px)", null, true);
                        transitioningElements.add(this._oldTitleLabel);
                    } else if (animationInfo.titleDifferent) {
                        transitioningElements.add(this._oldTitleLabel);
                    }
                }

                if (animationInfo.newAutoFitInfo.titleLabelVisible) {
                    
                    if (animationInfo.newAutoFitInfo._leftButtonIndex >= 0 || animationInfo.oldAutoFitInfo._leftButtonIndex >= 0) {
                        
                        var dX = (this._calculateNewTitleLabelLeft(animationInfo, 0) -
                                  this._calculateNewTitleLabelLeft(animationInfo, 1));
                        isc.Element._updateTransformStyle(this.titleLabel, "translateX(" + dX + "px)", null, true);
                        transitioningElements.add(this.titleLabel);
                    } else if (animationInfo.titleDifferent) {
                        transitioningElements.add(this.titleLabel);
                    }
                }

                var removedMembers = animationInfo.removedMembers;
                if (removedMembers != null) {
                    removedMembers.map(function (removedMember) {
                        if (removedMember !== this.leftButton) {
                            removedMember._origStyleName = removedMember.styleName;
                            transitioningElements.add(removedMember);
                        }
                    }, this);
                }
                var addedMembers = animationInfo.addedMembers;
                if (addedMembers != null) {
                    addedMembers.map(function (addedMember) {
                        if (addedMember !== this.leftButton) {
                            addedMember._origStyleName = addedMember.styleName;
                            transitioningElements.add(addedMember);
                        }
                    }, this);
                }

                
                if (this._animateStateChangeTimer != null) {
                    isc.Timer.clear(this._animateStateChangeTimer);
                    this._animateStateChangeTimer = null;
                }
                if (!dontStartTransitions) this._animateStateChangeTimer = this.delayCall("_animateStateChange");
                else this._pendingAnimateStateChangeCall = true;
            }
        }
    },

    _animateStateChange : function (externalCaller) {
        
        this._animateStateChangeTimer = null;

        if (externalCaller) {
            if (!this._pendingAnimateStateChangeCall) {
                return;
            }
            this._pendingAnimateStateChangeCall = false;
        }

        this._disableOffsetCoordsCaching();

        var animationInfo = this._animationInfo,
            oldViewState = animationInfo.oldViewState,
            oldAutoFitInfo = animationInfo.oldAutoFitInfo,
            newViewState = animationInfo.newViewState,
            newAutoFitInfo = animationInfo.newAutoFitInfo,
            backDirection = animationInfo.direction === this._$back,
            slidingRight = this._isSlidingRight(newAutoFitInfo),
            Canvas_setStyleName = isc.Canvas._instancePrototype.setStyleName;

        if (!oldViewState.showLeftButton && newViewState.showLeftButton) {
            Canvas_setStyleName.call(this._leftIconButton, this.fadeInStyleName);
            this._leftIconButton.setOpacity(null);
        } else if (oldViewState.showLeftButton && !newViewState.showLeftButton) {
            Canvas_setStyleName.call(this._leftIconButton, this.fadeOutStyleName);
            this._leftIconButton.setOpacity(0);
        }

        if (oldViewState.showLeftButton) {
            if ((backDirection
                 ? newAutoFitInfo.titleLabelVisible
                 : oldAutoFitInfo.titleLabelVisible) &&
                (newViewState.showLeftButton || oldViewState.showLeftButton))
            {
                Canvas_setStyleName.call(this._oldLeftTitleButton, backDirection
                                                                   ? this.oldLeftButtonBackStyleName
                                                                   : this.oldLeftButtonForwardStyleName);
                
                var dX = (this._calculateOldLeftButtonLeft(animationInfo, 1) -
                          oldAutoFitInfo._leftButtonLeft);
                isc.Element._updateTransformStyle(this._oldLeftTitleButton, "translateX(" + dX + "px)", null, true);
            } else {
                Canvas_setStyleName.call(this._oldLeftTitleButton, this.fadeOutStyleName);
            }
            this._oldLeftTitleButton.setOpacity(0);
        }

        if (newViewState.showLeftButton) {
            if ((backDirection
                 ? newAutoFitInfo.titleLabelVisible
                 : oldAutoFitInfo.titleLabelVisible) &&
                slidingRight == (animationInfo.leftButtonLogicalIconOrientation === isc.Canvas.RIGHT))
            {
                Canvas_setStyleName.call(this.leftButton, backDirection
                                                          ? this.newLeftButtonBackStyleName
                                                          : this.newLeftButtonForwardStyleName);
                isc.Element._updateTransformStyle(this.leftButton, "translateX(0px)", null, true);
            } else {
                Canvas_setStyleName.call(this.leftButton, this.fadeInStyleName);
            }
            this.leftButton.setOpacity(null);
        }

        if (oldAutoFitInfo.titleLabelVisible) {
            if (newAutoFitInfo._leftButtonIndex >= 0 || oldAutoFitInfo._leftButtonIndex >= 0) {
                Canvas_setStyleName.call(this._oldTitleLabel, backDirection
                                                              ? this.oldTitleBackStyleName
                                                              : this.oldTitleForwardStyleName);
                
                var dX = (this._calculateOldTitleLabelLeft(animationInfo, 1) -
                          oldAutoFitInfo.titleLabelRect[0]);
                isc.Element._updateTransformStyle(this._oldTitleLabel, "translateX(" + dX + "px)", null, true);
                this._oldTitleLabel.setOpacity(0);
            } else if (animationInfo.titleDifferent) {
                Canvas_setStyleName.call(this._oldTitleLabel, this.removedFadeOutStyleName);
                this._oldTitleLabel.setOpacity(0);
            }
        }

        if (newAutoFitInfo.titleLabelVisible) {
            if (newAutoFitInfo._leftButtonIndex >= 0 || oldAutoFitInfo._leftButtonIndex >= 0) {
                Canvas_setStyleName.call(this.titleLabel, backDirection
                                                          ? this.newTitleBackStyleName
                                                          : this.newTitleForwardStyleName);
                isc.Element._updateTransformStyle(this.titleLabel, "translateX(0px)", null, true);
                this.titleLabel.setOpacity(null);
            } else if (animationInfo.titleDifferent) {
                Canvas_setStyleName.call(this.titleLabel, this.addedFadeInStyleName);
                this.titleLabel.setOpacity(null);
            }
        }

        var removedMembers = animationInfo.removedMembers;
        if (removedMembers != null) {
            removedMembers.map(function (removedMember) {
                if (removedMember !== this.leftButton) {
                    Canvas_setStyleName.call(removedMember, this.removedFadeOutStyleName);
                    removedMember.setOpacity(0);
                }
            }, this);
        }
        var addedMembers = animationInfo.addedMembers;
        if (addedMembers != null) {
            addedMembers.map(function (addedMember) {
                if (addedMember !== this.leftButton) {
                    Canvas_setStyleName.call(addedMember, this.addedFadeInStyleName);
                    addedMember.setOpacity(null);
                }
            }, this);
        }
    },

    _transitionEnded : function (transitionRemoved) {
        

        if (this._animateStateChangeTimer != null) {
            isc.Timer.clear(this._animateStateChangeTimer);
            this._animateStateChangeTimer = null;
        }

        var animationInfo = this._animationInfo,
            Canvas_setStyleName = isc.Canvas._instancePrototype.setStyleName,
            elementsToReset = [this._leftIconButton,
                               this._oldLeftTitleButton, this.leftButton,
                               this._oldTitleLabel, this.titleLabel];
        for (var i = 0, len = elementsToReset.length; i < len; ++i) {
            var elementToReset = elementsToReset[i];
            
            Canvas_setStyleName.call(elementToReset, elementToReset._origStyleName);
            elementToReset._origStyleName = null;
            if (elementToReset.isDrawn()) isc.Element._updateTransformStyle(elementToReset, null, null, true);
        }

        var removedMembers = animationInfo.removedMembers;
        if (removedMembers != null) {
            removedMembers.map(function (removedMember) {
                Canvas_setStyleName.call(removedMember, removedMember._origStyleName);
                removedMember._origStyleName = null;
            }, this);
        }
        var addedMembers = animationInfo.addedMembers;
        if (addedMembers != null) {
            addedMembers.map(function (addedMember) {
                Canvas_setStyleName.call(addedMember, addedMember._origStyleName);
            }, this);
        }

        this._enableOffsetCoordsCaching();

        this._cleanUpAfterAnimation(animationInfo);
    },

    // Wait for the 'opacity' transitions to end on each transitioning element.
    
    handleTransitionEnd : function (event, eventInfo) {
        if (isc.Browser._supportsCSSTransitions && this.skinUsesCSSTransitions && this._animating) {
            var animationInfo = this._animationInfo,
                transitioningElements = animationInfo.transitioningElements;
            
            if (eventInfo.propertyName === "opacity") {
                if (transitioningElements.remove(eventInfo.target)) {
                    if (transitioningElements.length == 0) {
                        this._transitionEnded(false);
                    }
                }
                
            }
        }
    },

    transitionsRemoved : function () {
        if (isc.Browser._supportsCSSTransitions && this.skinUsesCSSTransitions && this._animating) {
            this._transitionEnded(true);
        }
    },

    // If and only if:
    // - the title is showing and:
    //   - this.reverseOrder is true and the leftButton is not a control; or
    //   - this.reverseOrder is true, the leftButton is a control, and the leftButton is to the
    //     left of the titleLabel in the controls array; or
    //   - this.reverseOrder is not true, the leftButton is a control, and the leftButton is to
    //     the right of the titleLabel in the controls array
    // - or, the title is not showing and this.reverseOrder is true
    // .. will the returned `left' coordinate increase as `ratio' increases (conceptually, the
    // new title will be sliding rightward as it fades in). Otherwise, the returned `left'
    // coordinate will decrease as `ratio' increases (conceptually, the new title will be sliding
    // leftward as it fades in).
    //
    // The same sliding direction is used for the old title and new leftButtonTitle.
    _isSlidingRight : function (newAutoFitInfo) {
        return (newAutoFitInfo._titleLabelIndex >= 0 && ((this.reverseOrder && (newAutoFitInfo._leftButtonIndex < 0 ||
                                                                                newAutoFitInfo._leftButtonLeftOfTitleLabel)) ||
                                                         (!this.reverseOrder && newAutoFitInfo._leftButtonIndex >= 0 &&
                                                          !newAutoFitInfo._leftButtonLeftOfTitleLabel))) ||
               (newAutoFitInfo._titleLabelIndex < 0 && this.reverseOrder);
    },

    // Calculates the left coordinate of _oldLeftTitleButton, given the animation info and the
    // ratio of the animation (between 0 and 1).
    //
    // This method is not used when the leftButton is hidden in the new view state.
    // This method is also not used when the title was not visible in the old view state.
    _calculateOldLeftButtonLeft : function (animationInfo, ratio) {
        var oldAutoFitInfo = animationInfo.oldAutoFitInfo,
            newAutoFitInfo = animationInfo.newAutoFitInfo,
            leftButtonLogicalIconOrientation = animationInfo.leftButtonLogicalIconOrientation;
        

        var slidingRight = this._isSlidingRight(newAutoFitInfo);
        var uw = this._calculateUW(animationInfo, oldAutoFitInfo, newAutoFitInfo, leftButtonLogicalIconOrientation, slidingRight);
        if (animationInfo.direction === this._$back) {
            var slidingLeft = slidingRight;
            if (slidingLeft) {
                var rightmost = oldAutoFitInfo._leftButtonLeft;
                return Math.round(rightmost - ratio * uw);
            } else {
                var leftmost = oldAutoFitInfo._leftButtonLeft;
                return Math.round(leftmost + ratio * uw);
            }
        } else {
            if (slidingRight) {
                var leftmost = (newAutoFitInfo._leftButtonIndex >= 0 ? newAutoFitInfo : oldAutoFitInfo)._leftButtonLeft;
                return Math.round(leftmost + ratio * uw);
            } else {
                var rightmost = (newAutoFitInfo._leftButtonIndex >= 0 ? newAutoFitInfo : oldAutoFitInfo)._leftButtonLeft;
                return Math.round(rightmost - ratio * uw);
            }
        }
    },

    // Calculates the left coordinate of the leftButton, given the animation info and the ratio
    // of the animation (between 0 and 1).
    //
    // This method is not used when the sliding direction does not match the logical icon orientation;
    // otherwise, the new left button title would be sliding over (or under, depending on zOrder)
    // the icon. This method is also not used when the title was not visible in the old view state.
    _calculateNewLeftButtonLeft : function (animationInfo, ratio) {
        var oldAutoFitInfo = animationInfo.oldAutoFitInfo,
            newAutoFitInfo = animationInfo.newAutoFitInfo,
            leftButtonLogicalIconOrientation = animationInfo.leftButtonLogicalIconOrientation;
        

        var slidingRight = this._isSlidingRight(newAutoFitInfo);
        

        var uw = this._calculateUW(animationInfo, oldAutoFitInfo, newAutoFitInfo, leftButtonLogicalIconOrientation, slidingRight);
        if (animationInfo.direction === this._$back) {
            var slidingLeft = slidingRight;
            if (slidingLeft) {
                var leftmost = (newAutoFitInfo._leftButtonIndex >= 0 ? newAutoFitInfo : oldAutoFitInfo)._leftButtonLeft;
                return Math.round(leftmost + (1 - ratio) * uw);
            } else {
                var rightmost = (newAutoFitInfo._leftButtonIndex >= 0 ? newAutoFitInfo : oldAutoFitInfo)._leftButtonLeft;
                return Math.round(rightmost - (1 - ratio) * uw);
            }
        } else {
            if (slidingRight) {
                var rightmost = newAutoFitInfo._leftButtonLeft;
                return Math.round(rightmost - (1 - ratio) * uw);
            } else {
                var leftmost = newAutoFitInfo._leftButtonLeft;
                return Math.round(leftmost + (1 - ratio) * uw);
            }
        }
    },

    _calculateUW : function (animationInfo, oldAutoFitInfo, newAutoFitInfo, leftButtonLogicalIconOrientation, slidingRight) {
        
        var uw;
        if (animationInfo.direction === this._$back) {
            
            var slidingLeft = slidingRight;
            if (slidingLeft) {
                if (newAutoFitInfo._leftButtonIndex >= 0) {
                    uw = newAutoFitInfo._leftButtonLeft + newAutoFitInfo._leftButtonWidth;
                } else {
                    uw = oldAutoFitInfo._leftButtonLeft + oldAutoFitInfo._leftButtonWidth;
                }
                uw -= newAutoFitInfo.titleLabelRect[0] + newAutoFitInfo.titleLabelRect[2] - newAutoFitInfo._apparentTitleLabelRightPadding;
            } else {
                uw = (newAutoFitInfo.titleLabelRect[0] + newAutoFitInfo._apparentTitleLabelLeftPadding -
                      (newAutoFitInfo._leftButtonIndex >= 0 ? newAutoFitInfo : oldAutoFitInfo)._leftButtonLeft);
            }
        } else {
            
            if (slidingRight) {
                if (newAutoFitInfo._leftButtonIndex >= 0) {
                    uw = newAutoFitInfo._leftButtonLeft + newAutoFitInfo._leftButtonWidth;
                } else {
                    uw = oldAutoFitInfo._leftButtonLeft + oldAutoFitInfo._leftButtonWidth;
                }
                uw -= oldAutoFitInfo.titleLabelRect[0] + oldAutoFitInfo.titleLabelRect[2] - oldAutoFitInfo._apparentTitleLabelRightPadding;
            } else {
                uw = (oldAutoFitInfo.titleLabelRect[0] + oldAutoFitInfo._apparentTitleLabelLeftPadding -
                      (newAutoFitInfo._leftButtonIndex >= 0 ? newAutoFitInfo : oldAutoFitInfo)._leftButtonLeft);
            }
        }
        if (leftButtonLogicalIconOrientation === (slidingRight ? isc.Canvas.RIGHT : isc.Canvas.LEFT)) {
            uw -= animationInfo.leftIconButtonWidth;
        } else {
            uw -= (newAutoFitInfo._leftButtonIndex >= 0 ? newAutoFitInfo : oldAutoFitInfo)._leftButtonWidth;
        }
        return uw;
    },

    // Calculates the left coordinate of _oldTitleLabel, given the animation info and the ratio
    // of the animation (between 0 and 1).
    //
    // For direction "forward":
    // If sliding leftward, the left button is showing in the new view state, and the logical
    // icon orientation of the leftButton is "left":
    // old view state:
    // +--------------------------------------------------------------------------------------+
    // |                               <..................<                                   |
    // |                               <*Old Title 123456*<                                   |
    // |                               <..................<                                   |
    // +--------------------------------------------------------------------------------------+
    // new view state:
    // +--------------------------------------------------------------------------------------+
    // +--------+                   +-------------------------+                               |
    // | < Back |                   |*New Longer Title 123456*|                               |
    // +--------+                   +-------------------------+                               |
    // +--------------------------------------------------------------------------------------+
    // ^  ^                            ^
    // 0  B                            A
    //
    //
    // If the logical icon orientation of the leftButton is "right", the picture is slightly
    // different:
    // +--------------------------------------------------------------------------------------+
    // +--------+                   +-------------------------+                               |
    // | Back < |                   |*New Longer Title 123456*|                               |
    // +--------+                   +-------------------------+                               |
    // +--------------------------------------------------------------------------------------+
    // ^        ^                      ^
    // 0        B                      A
    // (A is unchanged, but B points to the right edge of the leftButton)
    //
    //
    // If the left button is not showing in the new view state and the logical icon orientation
    // of the leftButton is "left":
    // old view state:
    // +--------------------------------------------------------------------------------------+
    // ..............                  <..................<                                   |
    // . < Old Back .                  <*Old Title 123456*<                                   |
    // ..............                  <..................<                                   |
    // +--------------------------------------------------------------------------------------+
    // new view state:
    // +--------------------------------------------------------------------------------------+
    // |                            +-------------------------+                               |
    // |                            |*New Longer Title 123456*|                               |
    // |                            +-------------------------+                               |
    // +--------------------------------------------------------------------------------------+
    // ^  ^                            ^
    // 0  B                            A
    //
    // The width from 0 to B is the leftIconButtonWidth. Let UW = the distance from A to B
    //
    //
    //
    // For direction "back", the picture is similar to what we do for the new title label when
    // direction is "forward":
    // +---------------------------- VW ----------------------------+
    // >...... OTTW ......>                                         +------ OTTW ------+
    // >*Old Title 123456*>                                         |*Old Title 123456*|
    // >..................>                                         +------------------+
    // +------------------------------------------------------------+
    // ^                                                            ^
    // A                                                            B
    // where OTTW is the old title text width.
    _calculateOldTitleLabelLeft : function (animationInfo, ratio) {
        var oldAutoFitInfo = animationInfo.oldAutoFitInfo,
            newAutoFitInfo = animationInfo.newAutoFitInfo,
            leftButtonLogicalIconOrientation = animationInfo.leftButtonLogicalIconOrientation;
        

        var slidingRight = this._isSlidingRight(newAutoFitInfo);
        if (animationInfo.direction === this._$back) {
            var slidingLeft = slidingRight;
            if (slidingLeft) {
                var rightmost = oldAutoFitInfo.titleLabelRect[0],
                    vw = oldAutoFitInfo.titleLabelRect[2] - oldAutoFitInfo._apparentTitleLabelRightPadding;
                return Math.round(rightmost - ratio * vw);
            } else {
                var leftmost = oldAutoFitInfo.titleLabelRect[0],
                    vw = oldAutoFitInfo.titleLabelRect[2] - oldAutoFitInfo._apparentTitleLabelLeftPadding;
                return Math.round(leftmost + ratio * vw);
            }
        } else {
            var uw = this._calculateUW(animationInfo, oldAutoFitInfo, newAutoFitInfo, leftButtonLogicalIconOrientation, slidingRight);
            if (slidingRight) {
                var leftmost = oldAutoFitInfo.titleLabelRect[0];
                return Math.round(leftmost + ratio * uw);
            } else {
                var rightmost = oldAutoFitInfo.titleLabelRect[0];
                return Math.round(rightmost - ratio * uw);
            }
        }
    },

    // Calculates the left coordinate of the titleLabel containing the new title, given the
    // animation info and the ratio of the animation (between 0 and 1).
    //
    // Let new title text width (NTTW) be the width of the text of the new title. This is
    // equal to titleLabelRect[2] - _apparentTitleLabelRightPadding - _apparentTitleLabelLeftPadding
    //
    // However, the width of the visible area that we have to work with (VW) is limited to:
    //    titleLabelRect[2] - _apparentTitleLabelLeftPadding
    // .. when sliding leftward; otherwise:
    //    titleLabelRect[2] - _apparentTitleLabelRightPadding
    //
    // The ideal scenario is where the NTTW is less than or equal to VW.
    //
    // When sliding leftward, the progress of the returned `left' coordinates can be visualized
    // as:
    // +---------------------------- VW ----------------------------+
    // +------ NTTW ------+                                         <...... NTTW ......<
    // |*New Title 123456*|                                         <*New Title 123456*<
    // +------------------+                                         <..................<
    // +------------------------------------------------------------+
    // ^                                                            ^
    // B                                                            A
    //
    // Where the left coordinate of the A marker less the left padding is the calculated left
    // coordinate when ratio = 0, and the left coordinate of the B marker less the left padding
    // is the calculated left coordinate when ratio = 1.
    //
    // One invariant is that the calculated left coordinate for ratio = 1 should be titleLabelRect[0]
    // (the left coordinate of the titleLabel rect), regardless of whether we are sliding leftward
    // or rightward.
    //
    //
    //
    // For direction "back", the picture is similar to what we do for the old title label when
    // the direction is "forward":
    // old view state:
    // +--------------------------------------------------------------------------------------+
    // ..............                  >..................>                                   |
    // . < Old Back .                  >*Old Title 123456*>                                   |
    // ..............                  >..................>                                   |
    // +--------------------------------------------------------------------------------------+
    // new view state:
    // +--------------------------------------------------------------------------------------+
    // |                            +-------------------------+                               |
    // |                            |*New Longer Title 123456*|                               |
    // |                            +-------------------------+                               |
    // +--------------------------------------------------------------------------------------+
    // ^  ^                         ^
    // 0  B                         A
    //
    // When the logical icon orientation of the left button is "right":
    // +--------------------------------------------------------------------------------------+
    // ..............                  >..................>                                   |
    // . < Old Back .                  >*Old Title 123456*>                                   |
    // ..............                  >..................>                                   |
    // +--------------------------------------------------------------------------------------+
    // new view state:
    // +--------------------------------------------------------------------------------------+
    // |                            +-------------------------+                               |
    // |                            |*New Longer Title 123456*|                               |
    // |                            +-------------------------+                               |
    // +--------------------------------------------------------------------------------------+
    // ^            ^               ^
    // 0            B               A
    //
    // If the left button was not showing in the old view state, the picture is:
    // +--------------------------------------------------------------------------------------+
    // |                               >..................>                                   |
    // |                               >*Old Title 123456*>                                   |
    // |                               >..................>                                   |
    // +--------------------------------------------------------------------------------------+
    // new view state:
    // +--------------------------------------------------------------------------------------+
    // +--------+                   +-------------------------+                               |
    // | < Back |                   |*New Longer Title 123456*|                               |
    // +--------+                   +-------------------------+                               |
    // +--------------------------------------------------------------------------------------+
    // ^  ^                         ^
    // 0  B                         A
    
    _calculateNewTitleLabelLeft : function (animationInfo, ratio) {
        var oldAutoFitInfo = animationInfo.oldAutoFitInfo,
            newAutoFitInfo = animationInfo.newAutoFitInfo,
            leftButtonLogicalIconOrientation = animationInfo.leftButtonLogicalIconOrientation;
        

        var slidingRight = this._isSlidingRight(newAutoFitInfo);
        if (animationInfo.direction === this._$back) {
            var uw = this._calculateUW(animationInfo, oldAutoFitInfo, newAutoFitInfo, leftButtonLogicalIconOrientation, slidingRight);
            if (slidingRight) {
                var leftmost = newAutoFitInfo.titleLabelRect[0];
                return Math.round(leftmost + (1 - ratio) * uw);
            } else {
                var rightmost = newAutoFitInfo.titleLabelRect[0];
                return Math.round(rightmost - (1 - ratio) * uw);
            }
        } else {
            if (slidingRight) {
                var rightmost = newAutoFitInfo.titleLabelRect[0],
                    vw = newAutoFitInfo.titleLabelRect[2] - newAutoFitInfo._apparentTitleLabelRightPadding;
                return Math.round(rightmost - (1 - ratio) * vw);
            } else {
                var leftmost = newAutoFitInfo.titleLabelRect[0],
                    vw = newAutoFitInfo.titleLabelRect[2] - newAutoFitInfo._apparentTitleLabelLeftPadding;
                return Math.round(leftmost + (1 - ratio) * vw);
            }
        }
    },

    
    leftButtonIconFadeInDelayRatio: 0.3,
    leftButtonIconFadeOutDurationRatio: 0.7, // 1 - leftButtonIconFadeInDelayRatio
    titleFadeOutDurationRatio: 0.4,
    titleFadeInDelayRatio: 0.3,
    _fireAnimationStateChange : function (ratio, ID, earlyFinish) {
        
        var animationInfo = this._animationInfo;

        var oldViewState = animationInfo.oldViewState,
            newViewState = animationInfo.newViewState,
            backDirection = animationInfo.direction === this._$back;

        if (ratio < 1) {

            // Fading in the left icon with an initial delay
            if (!oldViewState.showLeftButton && newViewState.showLeftButton) {
                var leftButtonIconFadeInDelayRatio = this.leftButtonIconFadeInDelayRatio;
                if (ratio >= leftButtonIconFadeInDelayRatio) {
                    var convertedRatio = (ratio - leftButtonIconFadeInDelayRatio) / (1 - leftButtonIconFadeInDelayRatio);
                    var newOpacity = convertedRatio * 100;
                    
                    this._leftIconButton.setOpacity(newOpacity);
                }

            // Fading out the left icon with an earlier finish
            } else if (oldViewState.showLeftButton && !newViewState.showLeftButton) {
                var leftButtonIconFadeOutDurationRatio = this.leftButtonIconFadeOutDurationRatio;
                if (ratio <= leftButtonIconFadeOutDurationRatio) {
                    var convertedRatio = ratio / leftButtonIconFadeOutDurationRatio;
                    var newOpacity = (1 - convertedRatio) * 100;
                    
                    this._leftIconButton.setOpacity(newOpacity);
                } else {
                    this._leftIconButton.setVisibility(isc.Canvas.HIDDEN);
                }
            }

            if (oldViewState.showLeftButton) {
                

                // Fading out the old leftButton title as we slide it out (or over for direction "back")
                if ((backDirection
                     ? animationInfo.newAutoFitInfo.titleLabelVisible
                     : animationInfo.oldAutoFitInfo.titleLabelVisible) &&
                    (newViewState.showLeftButton || oldViewState.showLeftButton))
                {
                    var titleFadeOutDurationRatio = (backDirection
                                                     ? this.titleFadeOutDurationRatio
                                                     : this.leftButtonIconFadeOutDurationRatio);
                    if (ratio <= titleFadeOutDurationRatio) {
                        var convertedRatio = ratio / titleFadeOutDurationRatio;
                        var newOpacity = (1 - convertedRatio) * 100;
                        
                        this._oldLeftTitleButton.setOpacity(newOpacity);

                        var left = this._calculateOldLeftButtonLeft(animationInfo, ratio);
                        this._oldLeftTitleButton.setLeft(left);
                    } else {
                        this._oldLeftTitleButton.setVisibility(isc.Canvas.HIDDEN);
                    }

                // Fading out the old leftButton title
                } else {
                    var leftButtonIconFadeOutDurationRatio = this.leftButtonIconFadeOutDurationRatio;
                    if (ratio <= leftButtonIconFadeOutDurationRatio) {
                        var convertedRatio = ratio / leftButtonIconFadeOutDurationRatio;
                        var newOpacity = (1 - convertedRatio) * 100;
                        
                        this._oldLeftTitleButton.setOpacity(newOpacity);
                    } else {
                        this._oldLeftTitleButton.setVisibility(isc.Canvas.HIDDEN);
                    }
                }
            }

            if (newViewState.showLeftButton) {
                
                var slidingRight = this._isSlidingRight(animationInfo.newAutoFitInfo);

                if ((backDirection
                     ? animationInfo.newAutoFitInfo.titleLabelVisible
                     : animationInfo.oldAutoFitInfo.titleLabelVisible) &&
                    slidingRight == (animationInfo.leftButtonLogicalIconOrientation === isc.Canvas.RIGHT))
                {
                    // Once the title is finished animating, start fading in the new leftButton title.
                    var titleFadeOutDurationRatio = this.titleFadeOutDurationRatio;
                    if (ratio > titleFadeOutDurationRatio) {
                        var convertedRatio = (ratio - titleFadeOutDurationRatio) / (1 - titleFadeOutDurationRatio);
                        var newOpacity = convertedRatio * 100;
                        
                        this.leftButton.setOpacity(newOpacity);

                        var left = this._calculateNewLeftButtonLeft(animationInfo, ratio);
                        this.leftButton.setLeft(left);
                    }

                // Fading in the new leftButton title
                } else {
                    var leftButtonIconFadeInDelayRatio = this.leftButtonIconFadeInDelayRatio;
                    if (ratio >= leftButtonIconFadeInDelayRatio) {
                        var convertedRatio = (ratio - leftButtonIconFadeInDelayRatio) / (1 - leftButtonIconFadeInDelayRatio);
                        var newOpacity = convertedRatio * 100;
                        
                        this.leftButton.setOpacity(newOpacity);
                    }
                }
            }

            if (animationInfo.oldAutoFitInfo.titleLabelVisible) {
                if (animationInfo.newAutoFitInfo._leftButtonIndex >= 0 || animationInfo.oldAutoFitInfo._leftButtonIndex >= 0) {
                    var titleFadeOutDurationRatio = (animationInfo.newViewState.showLeftButton
                                                     ? this.titleFadeOutDurationRatio
                                                     : this.leftButtonIconFadeOutDurationRatio);
                    // Quickly fading out the old title as we are sliding it out at normal speed
                    if (ratio <= titleFadeOutDurationRatio) {
                        var convertedRatio = ratio / titleFadeOutDurationRatio;
                        var newOpacity = (1 - convertedRatio) * 100;
                        
                        this._oldTitleLabel.setOpacity(newOpacity);

                        var left = this._calculateOldTitleLabelLeft(animationInfo, ratio);
                        this._oldTitleLabel.setLeft(left);
                    } else {
                        this._oldTitleLabel.setVisibility(isc.Canvas.HIDDEN);
                    }

                // In this case, we fade out the old title using the same fade-out animation
                // as removed members, and fade in the new title using the same fade-in animation
                // as added members.
                } else if (animationInfo.titleDifferent) {
                    if (ratio < 0.5) {
                        var newOpacity = ((0.5 - ratio) / 0.5) * 100;
                        
                        this._oldTitleLabel.setOpacity(newOpacity);
                    } else {
                        this._oldTitleLabel.setVisibility(isc.Canvas.HIDDEN);
                    }
                }
            }

            // Fading in the new title with an initial delay before changing the opacity, as we are
            // sliding the new title in (either leftward or rightward).
            if (animationInfo.newAutoFitInfo.titleLabelVisible) {
                if (animationInfo.newAutoFitInfo._leftButtonIndex >= 0 || animationInfo.oldAutoFitInfo._leftButtonIndex >= 0) {
                    var titleFadeInDelayRatio = this.titleFadeInDelayRatio;
                    if (ratio >= titleFadeInDelayRatio) {
                        var newOpacity = (ratio - titleFadeInDelayRatio) / (1 - titleFadeInDelayRatio) * 100;
                        
                        this.titleLabel.setOpacity(newOpacity);

                        // As a small optimization, since the opacity of the title is 0 for the initial
                        // opacity delay period, we don't update the left coordinate of the titleLabel
                        // until we actually start fading in.
                        var left = this._calculateNewTitleLabelLeft(animationInfo, ratio);
                        this.titleLabel.setLeft(left);
                    }
                } else if (animationInfo.titleDifferent) {
                    if (ratio < 0.5) {
                        this.titleLabel.setOpacity(0);
                    } else {
                        var newOpacity = ((ratio - 0.5) / 0.5) * 100;
                        
                        this.titleLabel.setOpacity(newOpacity);
                    }
                }
            }

            if (ratio < 0.5) {
                // Fading out the removed members
                var removedMembers = animationInfo.removedMembers;
                if (removedMembers != null) {
                    var newOpacity = ((0.5 - ratio) / 0.5) * 100;
                    
                    for (var i = 0, len = removedMembers.length; i < len; ++i) {
                        removedMembers[i].setOpacity(newOpacity);
                    }
                }
            } else {
                // Fading in the added and retained members
                var addedMembers = animationInfo.addedMembers,
                    retainedMembers = animationInfo.retainedMembers;
                if (addedMembers != null || retainedMembers != null) {
                    var newOpacity = ((ratio - 0.5) / 0.5) * 100;
                    
                    for (var i = 0, len = (addedMembers == null ? 0 : addedMembers.length); i < len; ++i) {
                        addedMembers[i].setOpacity(newOpacity);
                    }
                    for (var i = 0, len = (retainedMembers == null ? 0 : retainedMembers.length); i < len; ++i) {
                        retainedMembers[i].setOpacity(newOpacity);
                    }
                }
            }

        } else {
            

            var newMembers = animationInfo.newMembers,
                indexOfLeftButtonSpacer = newMembers.indexOf(this.leftButtonSpacer);
            if (indexOfLeftButtonSpacer >= 0) {
                newMembers[indexOfLeftButtonSpacer] = this.leftButton;
                this.setMembers(newMembers);
            }

            this._cleanUpAfterAnimation(animationInfo);
        }
    },

    _cleanUpAfterAnimation : function (animationInfo) {
        var newViewState = animationInfo.newViewState;

        this.showLeftButton = newViewState.showLeftButton;
        this.leftButtonTitle = newViewState.leftButtonTitle;
        this.shortLeftButtonTitle = newViewState.shortLeftButtonTitle;
        this.alwaysShowLeftButtonTitle = newViewState.alwaysShowLeftButtonTitle;
        this.title = newViewState.title;
        this.controls = newViewState.controls;

        var autoFitInfo = animationInfo.newAutoFitInfo;

        // We don't need the old/new view state or auto-fit info any more.
        animationInfo.oldViewState = null;
        newViewState = animationInfo.newViewState = null;
        animationInfo.oldAutoFitInfo = null;
        animationInfo.newAutoFitInfo = null;

        if (this.leftButtonIcon) this.leftButton.setIcon(this.leftButtonIcon);

        this.leftButton.setOpacity(null);
        if (this.showLeftButton) {
            this.leftButton.setOpacity(null);
            this.leftButton.setVisibility(isc.Canvas.INHERIT);
        } else {
            this.leftButton.setVisibility(isc.Canvas.HIDDEN);
            this.leftButton.setContents(isc.nbsp);
        }

        if (autoFitInfo.titleLabelVisible) {
            this.titleLabel.setOpacity(null);
            this.titleLabel.setLeft(autoFitInfo.titleLabelRect[0]);
            this.titleLabel.setVisibility(isc.Canvas.INHERIT);
        } else {
            this.titleLabel.setVisibility(isc.Canvas.HIDDEN);
            this.titleLabel.setContents(isc.nbsp);
        }

        var removedMembers = animationInfo.removedMembers;
        if (removedMembers != null) {
            for (var i = 0, len = removedMembers.length; i < len; ++i) {
                var removedMember = removedMembers[i];
                removedMember.deparent();
                removedMember.setOpacity(null);
                removedMember.setVisibility(isc.Canvas.INHERIT);
            }
        }

        var addedMembers = animationInfo.addedMembers;
        if (addedMembers != null) {
            for (var i = 0, len = addedMembers.length; i < len; ++i) {
                var addedMember = addedMembers[i];
                addedMember.setOpacity(null);
                addedMember.setVisibility(isc.Canvas.INHERIT);
            }
        }

        // Hide the helper components again.
        this._leftIconButton.setVisibility(isc.Canvas.HIDDEN);
        this._leftIconButton.setOpacity(null);
        this._oldLeftTitleButton.setVisibility(isc.Canvas.HIDDEN);
        this._oldLeftTitleButton.setTitle(isc.nbsp);
        this._oldLeftTitleButton.setOpacity(null);
        this._oldTitleLabel.setVisibility(isc.Canvas.HIDDEN);
        this._oldTitleLabel.setContents(isc.nbsp);
        this._oldTitleLabel.setOpacity(null);

        this._animationID = null;
        this._animationInfo = null;
        this._animating = false;
        if (isc.Canvas.ariaEnabled()) this.setAriaState("busy", false);
        this.eventMaskPeer.hide();

        
    },

    _calculateAutoFitInfo : function (viewState) {
        

        var undef;

        var info = {
            // use undefined for position and sizing values so that we will get NaN if arithmetic
            // using the values is attempted
            _leftButtonLeft: undef,
            _leftButtonWidth: undef,
            _leftButtonIndex: undef,
            _titleLabelIndex: undef,
            _leftButtonLeftOfTitleLabel: null,
            leftButtonTitle: viewState.leftButtonTitle,
            titleLabelVisible: true,
            title: viewState.title,
            titleLabelPrompt: null,
            titleLabelRightPadding: 0,
            titleLabelLeftPadding: 0,
            titleLabelRect: null,
            _apparentTitleLabelRightPadding: undef,
            _apparentTitleLabelLeftPadding: undef
        };

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

        var members = viewState.members;
        

        var titleLabelMeasurer = this.titleLabelMeasurer;
        titleLabelMeasurer.setContents(viewState.title);
        if (!titleLabelMeasurer.isDrawn()) titleLabelMeasurer.draw();
        else titleLabelMeasurer.redrawIfDirty();
        titleWidth = titleLabelMeasurer.getVisibleWidth();
        titleLabelMeasurer.setContents(isc.nbsp);

        var numMembers = members.length,
            i;

        
        var leftButtonIndex = info._leftButtonIndex = (this._shouldIgnoreMember(viewState.leftButton) &&
                                                       !(viewState.showLeftButton && !this.isIgnoringMember(viewState.leftButton))
                                                       ? -1 : members.indexOf(viewState.leftButton)),
            showingLeftButton = leftButtonIndex >= 0;

        // If we're not showing a title, then set titleLabelVisible to false and return.
        var titleLabelIndex = info._titleLabelIndex = members.indexOf(viewState.titleLabelSpacer);
        var showingLabel = titleLabelIndex >= 0 && !!viewState.title;
        if (!showingLabel) {
            info.titleLabelVisible = false;

            if (showingLeftButton) {
                if (titleLabelIndex >= 0) {
                    info._leftButtonLeftOfTitleLabel = (leftButtonIndex < titleLabelIndex);
                }
                info._leftButtonLeft = viewState.leftButton.getLeft();
                var leftButtonMeasurer = this.leftButtonMeasurer;
                leftButtonMeasurer.setProperties({
                    icon: viewState.leftButtonIcon,
                    title: viewState.leftButtonTitle
                });
                if (!leftButtonMeasurer.isDrawn()) leftButtonMeasurer.draw();
                else leftButtonMeasurer.redrawIfDirty();
                info._leftButtonWidth = leftButtonMeasurer.getVisibleWidth();
                leftButtonMeasurer.setTitle(isc.nbsp);
            }
            return info;
        }

        var lhsWidth,
            rhsWidth;

        // If not showing the left button, there is no need to worry about the left button's impact
        // on layout, but we may still need to clip the title.
        if (!showingLeftButton) {
            for (i = 0; i < titleLabelIndex; ++i) {
                var member = members[i];
                if (!this._shouldIgnoreMember(member)) {
                    outerLeftExtra += member.getVisibleWidth();
                }
            }
            for (i = titleLabelIndex + 1; i < numMembers; ++i) {
                var member = members[i];
                if (!this._shouldIgnoreMember(member)) {
                    outerRightExtra += member.getVisibleWidth();
                }
            }

            lhsWidth = outerLeftExtra;
            rhsWidth = outerRightExtra;

        } else {
            info._leftButtonLeft = viewState.leftButton.getLeft();

            var leftButtonMeasurer = this.leftButtonMeasurer;
            leftButtonMeasurer.setProperties({
                icon: viewState.leftButtonIcon,
                title: viewState.leftButtonTitle
            });
            if (!leftButtonMeasurer.isDrawn()) leftButtonMeasurer.draw();
            else leftButtonMeasurer.redrawIfDirty();
            var normalLeftButtonWidth = leftButtonMeasurer.getVisibleWidth();
            leftButtonMeasurer.setTitle(isc.nbsp);

            // The left button is to the left of the title label.
            if ((info._leftButtonLeftOfTitleLabel = (leftButtonIndex < titleLabelIndex))) {
                for (i = 0; i < leftButtonIndex; ++i) {
                    var member = members[i];
                    if (!this._shouldIgnoreMember(member)) {
                        outerLeftExtra += member.getVisibleWidth();
                    }
                }
                for (i = leftButtonIndex + 1; i < titleLabelIndex; ++i) {
                    var member = members[i];
                    if (!this._shouldIgnoreMember(member)) {
                        innerLeftExtra += member.getVisibleWidth();
                    }
                }
                for (i = titleLabelIndex + 1; i < numMembers; ++i) {
                    var member = members[i];
                    if (!this._shouldIgnoreMember(member)) {
                        outerRightExtra += member.getVisibleWidth();
                    }
                }

                leftButtonWidth = normalLeftButtonWidth;

            // The left button is to the right of the title label.
            } else {
                
                for (i = 0; i < titleLabelIndex; ++i) {
                    var member = members[i];
                    if (!this._shouldIgnoreMember(member)) {
                        outerLeftExtra += member.getVisibleWidth();
                    }
                }
                for (i = titleLabelIndex + 1; i < leftButtonIndex; ++i) {
                    var member = members[i];
                    if (!this._shouldIgnoreMember(member)) {
                        innerRightExtra += member.getVisibleWidth();
                    }
                }
                for (i = leftButtonIndex + 1; i < numMembers; ++i) {
                    var member = members[i];
                    if (!this._shouldIgnoreMember(member)) {
                        outerRightExtra += member.getVisibleWidth();
                    }
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
            if (r >= 0 && !(viewState.maxCenterOffset < (e - r) / 2)) {
                info.leftButtonTitle = viewState.leftButtonTitle;
            } else {
                var newLeftButtonWidth = normalLeftButtonWidth;

                // If that's not enough space, if a shortLeftButtonTitle is provided, it will be used
                // in lieu of the normal left button title.
                var shortLeftButtonWidth,
                    d;
                if (viewState.shortLeftButtonTitle) {
                    leftButtonMeasurer.setTitle(viewState.shortLeftButtonTitle);
                    leftButtonMeasurer.redrawIfDirty();
                    shortLeftButtonWidth = leftButtonMeasurer.getVisibleWidth();
                    leftButtonMeasurer.setTitle(isc.nbsp);

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

                if (viewState.alwaysShowLeftButtonTitle ||
                    !viewState.leftButtonIcon ||
                    (shortLeftButtonWidth != null && (r >= 0 && !(viewState.maxCenterOffset < (e - r) / 2))))
                {
                    if (shortLeftButtonWidth == null) {
                        info.leftButtonTitle = viewState.leftButtonTitle;
                    } else {
                        var isShorter = d < 0;
                        if (!isShorter) {
                            this.logWarn("The shortLeftButtonTitle:'" + viewState.shortLeftButtonTitle + "' is not shorter than the normal leftButton title:'" + viewState.leftButtonTitle + "'. The normal title will be used as it is shorter...");
                            info.leftButtonTitle = viewState.leftButtonTitle
                            if (leftButtonIndex < titleLabelIndex) {
                                lhsWidth -= d;
                            } else {
                                rhsWidth -= d;
                            }
                        } else {
                            info.leftButtonTitle = viewState.shortLeftButtonTitle;
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
                    leftButtonMeasurer.setTitle(isc.nbsp);

                    d = iconOnlyLeftButtonWidth - (shortLeftButtonWidth != null ? shortLeftButtonWidth : normalLeftButtonWidth);
                    if (leftButtonIndex < titleLabelIndex) {
                        lhsWidth += d;
                    } else {
                        rhsWidth += d;
                    }

                    info.leftButtonTitle = null;
                    newLeftButtonWidth = iconOnlyLeftButtonWidth;
                }

                if (leftButtonIndex < titleLabelIndex) {
                    leftButtonWidth = newLeftButtonWidth;
                } else {
                    rightButtonWidth = newLeftButtonWidth;
                }
            }
        }

        info._leftButtonWidth = leftButtonWidth;

        // If reverseOrder is true (typically this means RTL mode), swap lhsWidth and rhsWidth.
        if (this.reverseOrder) {
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
            titleLabelWidth = innerWidth - rhsWidth - lhsWidth;

        // If the sum of all components' widths (other than the title's) is already more than the
        // available width, hide the titleLabel.
        if (titleLabelWidth < 0) {
            info.titleLabelVisible = false;

        } else {
            info.title = viewState.title;

            // clear the prompt, if any to make sure we don't show stale data if we previously
            // overflowed and hence showed a prompt, but no longer overflow for the current
            // title and hence do not need to show a prompt
            info.titleLabelPrompt = null;

            // If r >= e, then it's possible to perfectly center the title without clipping it.
            if (r >= e) {
                var half = (r - e) / 2;
                info._apparentTitleLabelRightPadding = leftExtra + half;
                info._apparentTitleLabelLeftPadding = rightExtra + half;
                info.titleLabelRightPadding = leftExtra;
                info.titleLabelLeftPadding = rightExtra;

            // Else if r >= 0, then it's possible to place the title without clipping it, but
            // it must be offset by (e - r)/2 pixels. We may still clip the title here if
            // (e - r)/2 > this.maxCenterOffset.
            } else if (r >= 0) {
                var twiceCenterOffset = e - r,
                    twiceMaxCenterOffset = 2 * viewState.maxCenterOffset;
                if (twiceMaxCenterOffset < twiceCenterOffset) {
                    // factoring in the maxCenterOffset, the title will be clipped - set the
                    // prompt to the title string so the user can tap to see the full string
                    info.titleLabelPrompt = viewState.title;
                }
                if (rightExtra == 0) {
                    info._apparentTitleLabelRightPadding = r + Math.max(0, twiceCenterOffset - twiceMaxCenterOffset);
                    info._apparentTitleLabelLeftPadding = 0;
                } else {
                    
                    twiceCenterOffset = Math.min(twiceCenterOffset, twiceMaxCenterOffset);
                    info._apparentTitleLabelRightPadding = 0;
                    info._apparentTitleLabelLeftPadding = Math.floor((innerWidth - titleWidth - twiceCenterOffset) / 2) - lhsWidth;
                }

                info.titleLabelRightPadding = info._apparentTitleLabelRightPadding;
                info.titleLabelLeftPadding = info._apparentTitleLabelLeftPadding;

            // Else, just clear the padding. titleLabelWidth is already set to the remaining
            // width, so let the browser take over figuring out how to draw the title text.
            } else {
                // we overflowed the available width - set the prompt to the same string as the
                // contents of the label so the user can tap to see the full string
                info.titleLabelPrompt = viewState.title;

                info.titleLabelRightPadding = info._apparentTitleLabelRightPadding = 0;
                info.titleLabelLeftPadding = info._apparentTitleLabelLeftPadding = 0;
            }

            info.titleLabelRect = [lhsWidth, null, titleLabelWidth, null];
            info.titleLabelVisible = true;
        }

        return info;
    },

    _autoFitTitle : function () {
        
        if (this._animating || this.getDrawnState() === isc.Canvas.UNDRAWN || !this.isVisible()) return;

        var autoFitInfo = this._calculateAutoFitInfo(this);
        this._applyAutoFitInfo(autoFitInfo);
    },

    // We might have skipped auto-fitting if the NavigationBar was not visible at the time.
    // Whenever the NavigationBar is made visible and the layout is not dirty (if it is,
    // _autoFitTitle() will be called by the deferred reflowNow()), call _autoFitTitle().
    _visibilityChanged : function () {
        if (this.isVisible() && !this._layoutIsDirty) {
            this._autoFitTitle();
        }
        return this.Super("_visibilityChanged", arguments);
    },

    _applyAutoFitInfo : function (autoFitInfo) {
        var leftButton = this.leftButton;
        if (this.showLeftButton != false && leftButton != null) {
            leftButton.setTitle(autoFitInfo.leftButtonTitle);
            leftButton.redrawIfDirty();
        }

        var titleLabel = this.titleLabel;
        if (!autoFitInfo.titleLabelVisible) {
            titleLabel.setVisibility(isc.Canvas.HIDDEN);
        } else {
            titleLabel.setContents(autoFitInfo.title);
            titleLabel.setPrompt(autoFitInfo.titleLabelPrompt);
            titleLabel.setRightPadding(autoFitInfo.titleLabelRightPadding);
            titleLabel.setLeftPadding(autoFitInfo.titleLabelLeftPadding);
            titleLabel.setRect(autoFitInfo.titleLabelRect);
            titleLabel.setVisibility(isc.Canvas.INHERIT);
            // Eliminate the sometimes noticeable delay between when the titleLabel is marked
            // for redraw (by setContents()) and the redraw with the new contents.
            titleLabel.redrawIfDirty();
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
