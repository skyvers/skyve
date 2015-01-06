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

 





//>	@class	MenuBar
//      A MenuBar is a bar of buttons used to show a set of menus.
//  @treeLocation Client Reference/Control
//  @visibility external
//<

// declare the class itself
isc.ClassFactory.defineClass("MenuBar", "Toolbar");

// synonym: used to be named with lowercase "b"
isc.addGlobal("Menubar", isc.MenuBar);

// add default properties to the class
isc.MenuBar.addProperties( {
    //>	@attr	menuBar.menus		(Array of Menu : null : [IRW])
    // An array of menu object initializers or instantiated menu objects. Buttons for each
    // menu item will automatically be created. See the Menu Widget Class for fundamental
    // menu properties and other properties.  Titles for the buttons are derived from the 
    // <code>title</code> property of each menu.
    //  @visibility external
    //  @see class:Menu
    //<
	//menus:null,	

    //> @attr menu.menuButtonWidth (int : null : IR)
    // For a menu that has a +link{MenuButton} generated for it automatically (for example when
    // included in a +link{MenuBar}, the width that the MenuButton should have.  If unset, the
    // MenuButton will be as wide as <code>menu.width</code>.
    //
    // @visibility external
    //<

	overflow:isc.Canvas.VISIBLE,

    //>	@attr	menuBar.defaultHeight		(number : 22 : IRW)
	// Default to height for menu bars
	//		@group	sizing
	//<
	defaultHeight:22,					
	
	menuConstructor:"Menu",
	
	//	menuDefaults:{},
	
    //>	@attr	menuBar.buttonConstructor		(Class : MenuBarButton : IRWA)
	// Default constructor for menuBar buttons.  Change to a more exotic class if you dare!
    // (untested)
	//		@group	appearance
	//<
	buttonConstructor:"MenuBarButton",	

    //> @attr menuBar.tabIndex  (number : -1 : IRWA)
    // By default exclude menubars from the page's tab order. To include a menubar in the page's
    // tab order, set tabIndex to an explicit tab index, or <code>null</code> for automatically 
    // assigned tabIndex
    // @visibility external
    //<
    tabIndex:-1,
    
    // We want arrow keys to move us around the toolbar, not tabs.
    tabWithinToolbar:false,


	//>	@attr	menuBar.buttonDefaults		(object : (see below) : IRWA)
	//	The following are defaults for all menuBar buttons.  
	//	To add properties to all buttons of ALL menuBars, change the below.
	//	To add properties to all buttons of a particular menuBar you're creating,
	//		add a "button" property to the menuBar constructor with the defaults
	//		you want applied to the buttons.  This will automatically be added to each button.
	//		@group	appearance
	//<
	buttonDefaults: {
		showDown:false,
		showRollOver:true,
        
        showFocused:true,
        showFocusedAsOver:true
	}

});

isc.MenuBar.addMethods({

// Instantiates each Menu with automatically-generated IDs, using menuBar.menuConstructor and
// menuBar.menuDefaults
initWidget : function () {
	// call the superclass function
	this.Super("initWidget",arguments);
	
	//Note: we don't instantiate menus until they are displayed -- reduces init time.
},


//>	@method	menuBar.setButtons()
// Invalid to call on Menubar, use +link{setMenus()} instead.
// @param [newButtons] (Array of Button Properties) invalid; do not call
// @visibility external
//<
// Automatically creates MenuButtons from this.menus.
// Called internally to initialize the menuBar on draw, and from setMenus only.
setButtons : function () {
    var newButtons = [];

    if (this.menus) {
        // create a button for each menu
	    for (var i = 0; i < this.menus.length; i++) {
            var menu = this.menus[i];
            newButtons[i] = this._getButtonProperties(menu,i);
        }
    }
    return this.Super("setButtons", [newButtons], arguments);
},

// Helper method to get the properties for a menu button.
_getButtonProperties : function (menu, index, dontUseMenuWidth) {
    return {   
                title:menu.title, 
                width:(menu.menuButtonWidth ? menu.menuButtonWidth : (!dontUseMenuWidth ? menu.width : null)), 
                menuNum:index,
                
                focusChanged:function (hasFocus) {
                    if (isc.Browser.isMoz && hasFocus) this.bringToFront();
                }
            };

},

//>	@method	menuBar.setMenus()
// Dynamically reset the set of menus displayed by this menu bar. 
// @param menus (array) array of new menus for this menubar
// @visibility external
//<
setMenus : function (menus) {
    if (!isc.isAn.Array(menus)) menus = [menus];
    
    // get rid of old observations
    for (var i = 0; i < this.members.length; i++) {
        var member = this.members[i],
            menu = this.menus[member.menuNum];
        if (member.isObserving(menu, "hide")) {
            member.ignore(menu, "hide");
        }
    }

    // assign menus as new menus
    this.menus = menus;
    // create menuButtons for the menus we just assigned as this.menus
    this.setButtons();
},

// Helper method fired to update buttons in response to the menus shifting order
// - clears out old observations (for buttons that may be removed from the menubar) 
//   and remaps menu numbers

_remapButton : function (button, index) {
    if (!button) return;

    // in most cases we're still going to point at the same menu, but the position of that
    // menu in the menus array will have changed.
    // If index == -1 though, the menu is being removed, so clean up the observation
    if (index == -1) {
        var menuNum = button.menuNum,
            oldMenu = this.menus[button.menuNum];
        if (button.isObserving(oldMenu, "hide")) {
            button.ignore(oldMenu, "hide");
        }
    }
    // Update the menuNum so the members match the menus
    button.menuNum = index;
},

// Method required to deserialize Menus of the MenuBar
addMenu : function (newMenu, position) {
    // Suppress sizing button to match width of menu  
    return this.addMenus([newMenu], position, true);
},

//>	@method	menuBar.addMenus()
// Dynamically update the menuBar to include additional menus. Will update the visible set
// of buttons as appropriate
// @param newMenus (array) Array of new menus to add
// @param position (number) desired starting position of the new menus in the existing menus 
//  array
// @visibility external
//<
addMenus : function (newMenus, position, dontUseMenuWidth) {
    if (!newMenus) return;
    if (!isc.isAn.Array(newMenus)) newMenus = [newMenus];

    if (!this.menus) this.menus = [];
    if (position == null) position = this.menus.length;

    // If we have not yet initialized the buttons, we simply need to add the menus
    // the new buttons will be init'd along with the ones for pre-existent menus
    if (!this._buttonsInitialized) {
        this.menus.addListAt(newMenus, position);
    } else {
        
        // Every button AFTER the new buttons' position will need to be remapped
        for (var i = position; i < this.members.length; i++) {
            this._remapButton(this.members[i], (i + newMenus.length));
        }
        
        this.menus.addListAt(newMenus, position);
        
        var newButtons = [];
        for (var i = 0; i < newMenus.length; i++) {
            var index = this.menus.indexOf(newMenus[i]);
            newButtons[i] = this._getButtonProperties(newMenus[i], index, dontUseMenuWidth);
        }
        this.addButtons(newButtons, position);
    }
},

//>	@method	menuBar.removeMenus()
// Dynamically remove menus from the menuBar. Will update the visible set of buttons as 
// appropriate.
// @param menus (array) Array of menus to remove (will accept actual Menu components, 
//                      or numbers representing the index of the menus in the current menus array)
// @visibility external
//<
removeMenus : function (menus) {

    if (menus == null) return;
    if (!isc.isAn.Array(menus)) menus = [menus];    

    var membersToRemove = [],
        // make a new Array so existing Menu indices are stable while we form the new array
        newMenusArray = this.menus.duplicate();
        
    for (var i = 0; i < menus.length; i++) {
        var menu = menus[i];
        if (isc.isA.Number(menu)) menu = this.menus[menu];
        else if (!this.menus.contains(menu)) continue;

        newMenusArray.remove(menu);

        // form a list of generated MenuBarButtons to remove
        var index = this.menus.indexOf(menu);
        if (this._buttonsInitialized) membersToRemove.add(this.members[index]);
    }
    
    if (!this._buttonsInitialized) {
        this.menus = newMenusArray;
        return;    
    }
    
    for (var i = 0; i < this.menus.length; i++) {
        if (this.menus[i] == newMenusArray[i]) continue;
        this._remapButton(this.members[i], newMenusArray.indexOf(this.menus[i]));
    }
    
    this.menus = newMenusArray;
    this.removeButtons(membersToRemove);
},


//>	@method	menuBar.showMenu()
// Shows (opens) a menu.
// @param   menu    (Menu | integer) menu to show (may be specified as a menu object, or index of
//                                   the menu from +link{menuBar.menus, this.menus}).  
// @visibility external
//<
showMenu : function (menuNum) {
    var menu;
    if (isc.isA.Number(menuNum)) menu = this.menus[menuNum];
    else {
        menu = menuNum;
        menuNum = this.menus.indexOf(menu);
    }
    // could be a canvas or an object here, I guess
	if (!menu) {
		//>DEBUG
		this.logWarn("showMenu() called with invalid menu number: " + menuNum + ".  No effect.");
		//<DEBUG
		return;
	}
    
    var button;
    for (var i = 0; i < this.members.length; i++) {
        if (this.members[i].menuNum == menuNum) {
            button = this.members[i];
        }
    }
  
    if (!isc.isA.Canvas(menu)) {
		if (menu.ID == null) menu.ID = this.getID()+"_menu"+menuNum;
		menu.autoDraw = false;
		menu = this.menus[menuNum] = 
            isc.ClassFactory.newInstance(this.menuConstructor, menu, this.menuDefaults);
	}
	
	// hide whichever menu is currently showing (leaving the clickMask up)
    if (this.activeMenu != null) {
        this.menus[this.activeMenu].hideMenuTree();
    }
    
    
    menu.keyEventParent = this;
    
	// move the menu into place and show it	(automatically will be moved above clickmask)
    if (!this.vertical) {
        menu.moveTo(button.getPageLeft(), button.getPageBottom());
    } else {
        menu.moveTo(button.getPageRight(), button.getPageTop());
    }
	menu.show();
    
    // Don't set the "down" state on the button til after the menu has shown 
    // Otherwise when the clickMask shows, the button will be returned to state "up"
    // Also, while the menu is showing, avoid respoding to mouseOvers
    button._previousShowOver = button.showRollOver;
    button.showRollOver = false;
	button.setState(isc.StatefulCanvas.STATE_DOWN);

    // update this.activeMenu.
    // Note - when a menu is shown, the menuBar-button observes the hide method on the menu, to
    // reset it's visible state, and to clear 'activeMenu' from the menuBar.
	this.activeMenu = menuNum;
	if (!button.isObserving(menu,"hide")) {
		button.observe(menu,"hide","observer.menuHidden(observed)");
	}
	
    // bring the menuBar to the front so that they can switch to other menus
    var EH = isc.EH;
    if (EH.targetIsMasked(this)) this.bringToFront();
    
    // When the clickMask was shown by the first button clicked, one of our menu buttons had focus
    // and was therefore remembered as the maskedFocusCanvas.
    // Our buttons are no longer masked (bringToFront() above) so focussing on them won't update
    // the maskedFocusCanvas meaning it is essentially out of date at this point.
    // Clear this property so focus doesn't go back to the first button pushed when the clickMask
    // is hidden by menu.hideAllMasks()
    var topMask = EH.clickMaskRegistry.last(),
        maskedFocusCanvas = EH.getMaskedFocusCanvas(topMask);
    if (this.members.contains(maskedFocusCanvas)) EH.setMaskedFocusCanvas(null, topMask);
    
    // Update menu.focusOnHide to ensure that when the menu hides focus goes to the appropriate
    // button    
    menu.body.focusOnHide = button;
},

_focusInNextButton : function (forward) {
    if (!this.activeMenu == null) return this.Super("_focusInNextButton", arguments);
    if (forward == null) forward = true;
    
    // In this case, we're showing a menu, and the user pressed the left or right arrow
    // key.
    // Instead of shifting focus to the next button, show the next menu
    var activeMenu = this.activeMenu,
        step = forward ? 1 : -1,
        currentMenu = activeMenu + step,
        members = this.getMembers();
        
    while (activeMenu != currentMenu) {
        if (currentMenu < 0) currentMenu = members.length -1;
        else if (currentMenu >= this.members.length) currentMenu = 0;

        var button = members[currentMenu];
        if (!button.isDisabled()) {
            button.showMenu();
            break;
        }
        currentMenu += step;
    }
},

// override getFocusButtonIndex to return the button with an activeMenu, if one is defined, even
// if it doesn't have focus
getFocusButtonIndex : function () {
    if (this.activeMenu != null) return this.activeMenu;
    return this.Super("getFocusButtonIndex",arguments);

}

});





//>	@class	MenuBarButton
// Subclass of Button only used to show buttons in a MenuBar widget.
// <p>
// Allows mousing between MenuButtons in the same MenuBar to pop open Menus without a click.
// Also ensures that only one Menu from the MenuBar is showing at any given time.
//
//  @treeLocation Client Reference/Foundation
//<

isc.ClassFactory.defineClass("MenuBarButton","MenuButton");
isc.MenuBarButton.addProperties({
    showMenuButtonImage:false,
	showDown:false,
	autoDraw:false,
    align:"center"
});
isc.MenuBarButton.addMethods({
    
    // show menu on mouseOver if another menu in the menuBar is showing its menu
    // XXX unfortunately we don't get mouseMove while the mouse is down if the mouse went down
    // on another widget.  See EventHandler for why.
    mouseOver : function () {
        this.Super("mouseOver", arguments);

        // if another menu is currently being shown, show our menu instead.
        var activeMenuNum = this.parentElement.activeMenu;
    	if (activeMenuNum != null && activeMenuNum != this.menuNum) {
            this.showMenu();
        }
    },
    
    // override mouseDown to show menu (rather than click)
    mouseDown : function () {
    	// if this menu button is the one showing it's menu, hide it
    	if (this.parentElement.activeMenu == this.menuNum) {
    		isc.Menu.hideAllMenus();
    	// otherwise show it
    	} else {
            this.showMenu();
        }
    },
    // override mouseUp and click to do nothing
    mouseUp : function () {},
    click : function () {},
    mouseOut : function () {
    	if (this.parentElement.activeMenu != this.menuNum) {
    		this.Super("mouseOut", arguments);
    	}
    },
    
    // Override handleKeyPress - we're going to be firing showMenu() rather than click()
    handleKeyPress : function (event, eventInfo) {
    
        if (event.keyName == "Space" || event.keyName == "Enter") return this.showMenu();
        
        if (this.keyPress) {
            this.convertToMethod("keyPress");
            return this.keyPress(event, eventInfo);
        }
    },
    
    // override showMenu to delegate up to the menuBar
    showMenu : function () {
        this.parentElement.showMenu(this.menuNum);
    },
    
    menuHidden : function (menu) {
        if (isc._traceMarkers) arguments.__this = this;
        // When the menu got shown we put the button into a 'down' state
        // Clear this now.
        
        if (this.state == isc.StatefulCanvas.STATE_DOWN) {
            if (this.hasFocus && this.showFocused) this.setState(isc.StatefulCanvas.STATE_OVER);
            else this.setState(isc.StatefulCanvas.STATE_UP);
        } 
        
        this.showRollOver = this._previousShowOver;
        delete this._previousShowOver;
    	this.menuIsDown = false;      
    	if (this.parentElement.activeMenu == this.menuNum) {
    		this.parentElement.activeMenu = null;
    	}
        // clear the eventParent setting we set up on show - if the menu is shown elsewhere
        // we don't want to recieve bubbled events!
        delete menu.eventParent;
        // clear the observation - we'll re-observe when we re-show!
        this.ignore(menu, "hide");
        
    }
});

