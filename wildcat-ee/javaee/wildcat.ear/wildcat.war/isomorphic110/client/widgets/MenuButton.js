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
//>	@class	MenuButton
//
//  Simple subclass of button associated with a menu widget (gets shown below the button).
// @visibility external
// @treeLocation Client Reference/Control
//<


//>	@class	IMenuButton
//
//  StretchImgButton based version of the +link{MenuButton} class.
//
// @visibility external
// @treeLocation Client Reference/Control
//<




isc._commonMenuButtonProperties = {
    _isMenuButton: true,

    //>	@attr	menuButton.title		(string : "Show Menu" : IRW)
	//			Default title for the button.
    //  @visibility external
    // @group i18nMessages
	//<
	
	//> @attr iMenuButton.title (string : "Show Menu" : IRW)
	// @include menuButton.title
	//<
    title:"Show Menu",

    //>	@attr	menuButton.height		(Number or String : 22 : IRW)
	//			Default height of the button.
    // @visibility external
	//<
	
	//> @attr iMenuButton.height (Number or String : 22 : IRW)
	// @include menuButton.height
	//<
	height:22,
	
    //> @attr menuButton.showMenuButtonImage (Boolean : true : IR)
    // Show menu button image (up / down arrowhead) for this menu button.
    // 
    // @visibility external
    //<
    
    //> @attr iMenuButton.showMenuButtonImage (Boolean : true : IR)
    // @include menuButton.showMenuButtonImage
    //<
    showMenuButtonImage:true,
    
    // We use the standard button icon handling to write out the menu button image
    // We need both standard (down) and "up" variations because of the menu showing above 
    // or below the button.
    
    //>	@attr	menuButton.menuButtonImage		(SCImgURL : "[SKIN]menu_button.gif" : IRA)
    // Image for menu button indicating that the button expands a menu.  This image is shown
    // for menus expanding down from the button.   Menu direction is controlled by
    // +link{MenuButton.showMenuBelow}.
    //
    // @see menuButton.menuButtonImageUp
    // @visibility external
    //<
    
    //> @attr iMenuButton.menuButtonImage (SCImgURL : "[SKIN]menu_button.gif" : IRA)
    // @include menuButton.menuButtonImage
    //<
	menuButtonImage:"[SKIN]menu_button.gif",

    //>	@attr	menuButton.menuButtonImageUp		(SCImgURL : "[SKIN]menu_button_up.gif" : IRA)
    // Image for menu button indicating that the button expands a menu.  This image is shown
    // for menus expanding up from the button.   Menu direction is controlled by
    // +link{MenuButton.showMenuBelow}.
    //
    // @see menuButton.menuButtonImage
    // @visibility external
    //<											

    //> @attr iMenuButton.menuButtonImageUp (SCImgURL : "[SKIN]menu_button_up.gif" : IRA)
    // @include menuButton.menuButtonImageUp
    //<
	menuButtonImageUp:"[SKIN]menu_button_up.gif",
    
    //> @attr   menuButton.hiliteAccessKey  (Boolean : true : IR)
    // If this MenuButton has a specified +link{canvas.accessKey, accessKey}, underline it
    // in the title of the button by default
    // @visibility external
    //<
    
    //> @attr iMenuButton.hiliteAccessKey (Boolean : true : IR)
    // @include menuButton.hiliteAccessKey
    //<
    hiliteAccessKey:true,
    
    // Default menuButtonImage size is 7px
    iconWidth:7,
    iconHeight:7,
    
    // Show the arrow on the right of the title, aligned with the right edge of the button.
    iconOrientation:"right",
    iconAlign:"right",

    // Align the title to the left of the button (away from the icon) by default.
    align:"left",
    
    //>	@attr	menuButton.showMenuBelow		(Boolean : true : IRW)
    //			The menu drops down below the menu button.
    //			Set to false if the menu should appear above the menu button.
    // @visibility external
    //<
    
    //> @attr iMenuButton.showMenuBelow (Boolean : true : IRW)
    // @include menuButton.showMenuBelow
    //<
	showMenuBelow:true,
    
	//> @attr menuButton.menuAlign (Alignment : null : IR)
    // The horizontal alignment of this button's menu, in relation to the button.  When unset,
    // default behavior is to align the right edges of button and menu if the page is in RTL 
    // mode, and the left edges otherwise.
    // @visibility external
    //<	
    //menuAlign: null,
    
    //> @attr iMenuButton.menuAlign (Alignment : null : IR)
    // @include menuButton.menuAlign
    // @visibility external
    //<
    
    //>	@attr menuButton.menu (Menu : null : IRW)
    // The menu to show.
    // <P>
    // For a menu button with no menu (menu: null) the up/down arrow image can
    // be suppressed by setting
    // +link{menuButton.showMenuButtonImage, showMenuButtonImage}: <code>false</code>.
    //
    // @visibility external
    //<
    //> @attr iMenuButton.menu (Menu : null : IRW)
    // @include menuButton.menu
    //<
    menu:null,
    
    //>Animation
    //> @attr menuButton.menuAnimationEffect (string : null : IRWA)
    // Allows you to specify an animation effect to apply to the menu when it is being shown.
    // Valid options are "none" (no animation), "fade", "slide" and "wipe".
    // If unspecified falls through to <code>menu.showAnimationEffect</code>
    // @visibility animation
    //<
    
    //> @attr iMenuButton.menuAnimationEffect (string : null : IRWA)
    // @include menuButton.menuAnimationEffect
    //<
    
    //<Animation

    // Choose the appropriate icon depending on whether the menu will be shown above or
    // below this menubutton.
    initWidget : function (a,b,c,d) {
        if (this.menuAlign == null) {
            this.menuAlign = !this.isRTL() ? isc.Canvas.LEFT : isc.Canvas.RIGHT;
        }
        if (this.showMenuButtonImage)  this._setUpIcon();
        return this.invokeSuper(isc.MenuButton, "initWidget", a,b,c,d);
    },
    
    //> @attr menuButton.autoDestroyMenu (Boolean : true : IRW)
    // If this menuButton is +link{canvas.destroy(),destroyed}, should it also destroy
    // its +link{menuButton.menu}?
    // @visibility external
    //<
    //> @attr iMenuButton.autoDestroyMenu (Boolean : true : IRW)
    // @include menuButton.autoDestroyMenu
    //<
    autoDestroyMenu:true,
    destroy : function (a,b,c,d,e) {
        if (this.menu != null && this.autoDestroyMenu && this.menu.destroy != null &&
            !this.menu.destroyed && !this.menu.destroying)
        {
            this.menu.destroy();
            this.menu = null;
        }
        return this.invokeSuper(isc.MenuButton, "destroy", a,b,c,d,e);
    },
    
    // setter for showMenuButtonImage and showMenuBelow - required since we need to update the 
    // icon property and trip a redraw
    //> @method MenuButton.setShowMenuButtonImage
    // Setter for the 'showMenuButtonImage' property - shows/hides the menu button image
    // at runtime.
    //
    // @param show (boolean) Should the image be shown
    // @visibility external
    //<
    
    //>@method iMenuButton.setShowMenuButtonImage()
    // @include menuButton.setShowMenuButtonImage()
    //<
    setShowMenuButtonImage : function (show) {
        if (show == this.showMenuButtonImage) return;
        this.showMenuButtonImage = show;
        if (show) this._setUpIcon();
        else this.icon = null;
        if (this.isDrawn()) this.markForRedraw();
    },
    
    //>@method MenuButton.setShowMenuBelow
    // Setter for the 'showMenuButtonBelow' property - determines whether the menu will be
    // shown above or below the menubutton.
    // @param below (boolean) True if the menu should be shown below the menubutton.
    // @visibility external
    //<
    
    //>@method iMenuButton.setShowMenuBelow()
    // @include menuButton.setShowMenuBelow()
    //<
    setShowMenuBelow : function (below) {
        if (below != this.showMenuBelow) {
            this.showMenuBelow = below;
            // If we're not showing the icon we don't need to update anything until we show the icon.
            if (this.showMenuButtonImage) {
                this._setUpIcon();
                if (this.isDrawn()) this.markForRedraw();
            }
        }
    },
    
    // helper to convert the menubutton image to an icon
    _setUpIcon : function () {
        var icon = this.showMenuBelow ? this.menuButtonImage : this.menuButtonImageUp;
        
        if (isc.isAn.Object(icon)) {
            if (icon.width) this.iconWidth = icon.width;
            if (icon.height) this.iconHeight = icon.height;
            this.icon = icon.src;
        } else {
            this.icon = icon;
        }
    },
    

    // If this click didn't come from dismissing a menu shown by this button, 
    // show our menu on click
    
    handleClick : function () {
        if (this.Super("handleClick", arguments) == false) return;
        
        if (this._hidingMenuClick) return;
        this.showMenu();
    },
    
    // This method is fired in response to the Page level click event following the
    // dismissing of the menu we were showing.
    // Use it to clear out this._hidingMenuClick
    
    _hidingMenuClickComplete : function () {
        if (this._hidingMenuClick) {
            var theMenu = this;
            isc.Page.setEvent(isc.EH.IDLE, 
                              function () { theMenu._hidingMenuClick = null },
                              isc.Page.FIRE_ONCE);
        }
    },
    
    // override keypress to show the menu on arrow_down keypress
    keyPress : function () {
        if (isc.EventHandler.lastEvent.keyName == "Arrow_Down") {
                this.showMenu();
                return false;
        }
        return this.Super("keyPress", arguments);
    },
    
    // Override _updateCanFocus to redraw the button when canFocus is changed - this will
    // regenerate the titleHTML, with the appropriate focus behavior.
    _updateCanFocus : function () {
        this.Super("_updateCanFocus", arguments);
        this.markForRedraw();
    },

    _addRollOverOverrides : function (menu) {
        

        menu.rootMenuButton = this;
        menu.autoDismissOnBlur = false;
        this.observe(menu, "mouseOver", function () {
            
            this.checkRollOverMouseOver();
        });
        this.observe(menu, "mouseOut", function () {
            
            this.checkRollOverMouseOut();
        });

        // replace getSubmenu() so we can observe mouse events
        var origGetSubmenu = menu.getSubmenu;
        menu.getSubmenu = function (item) {
            
            var submenu = origGetSubmenu.apply(this, arguments);
            if (isc.isA.Menu(submenu)) {
                if (submenu.rootMenuButton == null) {
                    this.rootMenuButton._addRollOverOverrides(submenu);
                } 
            }
            return submenu;
        };

        this.observe(menu, "_setVerticalScrollbar", function () {
            
            this._addExtraRollOverOverrides(menu);
        });
        if (menu.vscrollbar != null) this._addExtraRollOverOverrides(menu);
    },

    // Add mouseOver handlers to XXX_vscroll and to XXX_vscroll_thumb
    // This is to assure that the Menu is not hidden when the Menu has rollOver enabled
    // and the mouse enters the vscroll area
    
    _addExtraRollOverOverrides : function (menu) {
        var vscrollbar = menu.vscrollbar;
        

        var mouseOverAction = function () {
            
            this.checkRollOverMouseOver();
        };
        var mouseOutAction = function () {
            
            this.checkRollOverMouseOut();
        };

        this.observe(vscrollbar, "handleMouseOver", mouseOverAction);
        this.observe(vscrollbar, "handleMouseOut", mouseOutAction);

        var scrollbarPeers = vscrollbar.peers,
            numScrollbarPeers = scrollbarPeers == null ? 0 : scrollbarPeers.length;
        for (var i = 0; i < numScrollbarPeers; ++i) {
            var scrollbarPeer = scrollbarPeers[i];
            this.observe(scrollbarPeer, "handleMouseOver", mouseOverAction);
            this.observe(scrollbarPeer, "handleMouseOut", mouseOutAction);
        }
    },

    checkRollOverMouseOver : function () {
        if (this._hoverMenuTimer != null) {
            // if a timer's already set, clear it now
            isc.Timer.clear(this._hoverMenuTimer);
            this._hoverMenuTimer = null;
        }
        // If the menu isn't visible, hide any others and show it now
        if (isc.isA.Menu(this.menu) && !(this.menu.isVisible() && this.menu.isDrawn())) {
            isc.Menu.hideAllMenus();
            this.showMenu();
        }
    },

    checkRollOverMouseOut : function () {
        if (this._hoverMenuTimer != null) {
            // clear old timer if set
            isc.Timer.clear(this._hoverMenuTimer);
            this._hoverMenuTimer = null;
        }
        // If the menu's visible, start a timer to hide it
        if (isc.isA.Menu(this.menu) && this.menu.isVisible() && this.menu.isDrawn()) {
            // hide the menu on a short timer so we can move among menus and suchlike
            var menuButton = this;
            this._hoverMenuTimer = isc.Timer.setTimeout(function () {
                menuButton._hoverMenuTimer = null;

                // Make sure the menu's still showing before hiding it
                if (isc.isA.Menu(menuButton.menu) && menuButton.menu.isVisible() && menuButton.menu.isDrawn()) {
                    isc.Menu.hideAllMenus();
                }
            }, this.rollOverMenuHideDelay);
        }
    },

    //> @method MenuButton.showMenu()
    // Programmaticly forces this MenuButton to show it's menu.
    // @visibility external
    //<
    showMenu : function () {
        // lazily create the menu if necessary, so we can init with, or set menu to, an object 
        // properties block
        if (isc.isA.String(this.menu)) this.menu = window[this.menu];
        if (!isc.isA.Menu(this.menu)) this._createMenu(this.menu);
        if (!isc.isA.Menu(this.menu)) return;

        
        isc.Menu._currentMenuButton = this;

        var menu = this.menu;
        

        if (!menu.ruleScope) {
            var ruleScope = this.getRuleScope();
            if (ruleScope) menu.ruleScope = ruleScope;
        }

        // This may be the first call to showMenu(). If showMenuOnRollOver is enabled and the
        // MenuButton was provided with the menu to use (and it did not create the Menu itself),
        // then make sure that the rollOver overrides are applied.
        if (this.showMenuOnRollOver && menu.rootMenuButton == null) {
            if (menu.rootMenuButton == null) {
                this._addRollOverOverrides(menu);
            } 
        }

        // draw offscreen so that we can figure out what size the menu is
        // Note that we use _showOffscreen which handles figuring out the size, and
        // applying scrollbars if necessary.
        menu._showOffscreen();
        this.positionMenu(menu);
        menu.show(this.menuAnimationEffect);
    },

    positionMenu : function (menu) {
        if (!menu) return;
        // figure out the left coordinate of the drop-down menu
        var left = this.getPageLeft();

        if (this.menuAlign == isc.Canvas.CENTER) {
            // center-align the menu to the menuButton
            left = left - ((menu.getVisibleWidth() - this.getVisibleWidth()) / 2); 
        } else if (this.menuAlign == isc.Canvas.RIGHT) {
            // align the right-edge of the menu to the right edge of the menuButton
            left -= (menu.getVisibleWidth() - this.getVisibleWidth());
        }

        var top = this.showMenuBelow ? this.getPageTop()+this.getVisibleHeight()+1 :  
                                       this.getPageTop()-menu.getVisibleHeight()+2;
        // don't allow the menu to show up off-screen
        menu.placeNear(left, top);
    },
    
    repositionMenu : function () {
        var menu = this.menu;
        if (menu && isc.isA.Menu(menu) && menu.isDrawn() && menu.isVisible()) {
            this.positionMenu(menu);
        }
    },

    _createMenu : function (menu) {
        if (!menu) return;
        menu.autoDraw = false;

        var cons = this.menuConstructor || isc.Menu;
        this.menu = isc.ClassFactory.newInstance(cons, menu);
    },

    handleMoved : function (deltaX, deltaY) {
        // move the menu with the MenuButton
        this.Super("handleMoved", arguments);
        this.repositionMenu();
    },
    
    handleParentMoved : function (parent, deltaX, deltaY) {
        // move the menu with the MenuButton
        this.Super("handleParentMoved", arguments);
        this.repositionMenu();
    },
    
    // RollOver menus

    //> @attr menuButton.showMenuOnRollOver (Boolean : false : IRA)
    // Should the menu be shown automatically when the mouse moves over the button?
    // <p>
    // When enabled, menus used with this <code>MenuButton</code> should not be used with any
    // other component.
    // @group appearance
    // @visibility external
    //<

    //> @attr menuButton.rollOverMenuHideDelay (Number : 250 : IRA)
    // When +link{menuButton.showMenuOnRollOver, showMenuOnRollOver} is true, this is the delay 
    // in milliseconds before the menu is automatically hidden following mouseOut.
    // @group appearance
    // @visibility external
    //<
    rollOverMenuHideDelay: 250,

    mouseMove : function () {
        if (this.showMenuOnRollOver) this.mouseOver();
    },

    mouseOver : function() {
        if (this.showMenuOnRollOver) this.checkRollOverMouseOver();
        return this.Super("mouseOver", arguments);
    },

    mouseOut : function() {
        if (this.showMenuOnRollOver) this.checkRollOverMouseOut();
        return this.Super("mouseOut", arguments);
    }

};



// define us as a subclass of the Button
isc.ClassFactory.defineClass("MenuButton", "Button");

isc.MenuButton.addProperties(isc._commonMenuButtonProperties);

isc.MenuButton.addProperties({
    //>	@attr menuButton.skinImgDir (URL : "images/Menu/" : IRA)
    // Where do 'skin' images (those provided with the class) live?
    // This is local to the Page.skinDir
    // @group appearance, images
    //<
    skinImgDir:"images/Menu/",
             
    //>	@attr	menuButton.baseStyle		(CSSStyleName : "menuButton" : IRW)
	//			CSS baseStyle for the button
	//<
    baseStyle:"menuButton"
    
});


// IMenuButton is a subclass of IButton.
// 
isc.ClassFactory.defineClass("IMenuButton", "StretchImgButton");

isc.IMenuButton.addProperties(isc._commonMenuButtonProperties);

isc.IMenuButton.addProperties({
    //>	@attr	iMenuButton.labelSkinImgDir		(URL : "images/Menu/" : IRA)
    // skinImgDir to apply to the title label for this button only.
    // This is typically where the icon media for the menu will live.
    // This is local to the Page.skinDir
    // @group	appearance, images
    //<
    labelSkinImgDir:"images/Menu/"
    
});


//> @class ToolStripMenuButton
// Simple subclass of IMenuButton with appearance appropriate for a ToolStrip menu button.
// Can be used to create an icon-only menu button, and icon with text, or a text only button by setting the 
// icon and title attibutes as required.
// @treeLocation Client Reference/Layout/ToolStrip
// @visibility external
//<
isc.defineClass("ToolStripMenuButton", "IMenuButton").addProperties({
   showTitle:true,
   showRollOver:true,
   showDown:true,
   labelVPad:0,
   labelHPad:7,
   autoFit:true,
   src:"[SKIN]/ToolStrip/button/button.png",
   capSize:3,
   height:22
});

