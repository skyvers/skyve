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

 





//>	@class	Toolbar
//
// A Toolbar creates a vertical or horizontal strip of similar components (typically Buttons)
// and provides managed resizing and reordering behavior over those components.
// <p>
// If you are creating a bar with a mixture of different elements (eg some MenuButtons, some
// Labels, some Buttons, some custom components), you want to use a +link{ToolStrip}.  A
// Toolbar is better suited for managing a set of highly similar, interchangeable components,
// such as ListGrid headers.
//
// @treeLocation Client Reference/Layout
// @visibility external
//<

// declare the class itself
isc.ClassFactory.defineClass("Toolbar", "Layout");

// add default properties to the class
isc.Toolbar.addProperties( {
	//>	@attr	toolbar.buttons		(array : null : [IRW])
    // An array of button object initializers. See the Button Widget Class for standard
    // button properties. The following additional properties can also be specified for
    // button sizing and positioning on the toolbar itself:<br><br>
    // <ul><li>width--Specifies the width of this button as an absolute number of pixels, a
    // named property of the toolbar that specifies an absolute number of pixels, a
    // percentage of the remaining space (e.g. '60%'), or "*" (default) to allocate an
    // equal portion of the remaining space.
    // <li>height--Specifies the height of this button.
    // <li>extraSpace--Specifies an optional amount of extra space, in pixels, to separate
    // this button from the next button in the toolbar.</ul>
    //
    // @setter setButtons()
    // @see toolbar.addButtons()
    // @see toolbar.removeButtons()
    // @see class:Button
    // @visibility external
    //<

	//>	@attr	toolbar.vertical		(Boolean : false : [IRW])
	// Indicates whether the buttons are drawn horizontally from left to right (false), or
    // vertically from top to bottom (true).
	//		@group	appearance
    //      @visibility external
	//<
	vertical:false,

	//>	@attr	toolbar.overflow		(Overflow : Canvas.HIDDEN : IRWA)
	// Clip stuff that doesn't fit
	//		@group	clipping
	//<
	overflow:isc.Canvas.HIDDEN,
	
	//>	@attr	toolbar.height		(number : 20 : IRW)
	// Default to a reasonable height
	//		@group	sizing
	//<
	height:20,					

    //>	@attr	toolbar.buttonConstructor		(Class : Button : IRWA)
	// Default constructor for toolbar items.
	//		@group	appearance
    //	@visibility external
	//<
	buttonConstructor:"Button",

    //>	@attr	toolbar.canReorderItems		(Boolean : false : IRWA)
	//		If true, items can be reordered by dragging on them.
	//		@group	dragndrop
    //	@visibility external
	//<
	canReorderItems:false,

    //>	@attr	toolbar.canResizeItems		(Boolean : false : IRWA)
	//		If true, items (buttons) can be resized by dragging on them.
	//		@group	dragndrop
    //	@visibility external
	//<
	canResizeItems:false,

    //>	@attr	toolbar.canRemoveItems      (boolean : false : IRWA)
	// If true, items (buttons) can be dragged out of this toolbar to be dropped somewhere else
	//		@group	dragndrop
	//<
	canRemoveItems:false,

    //>	@attr	toolbar.canAcceptDrop (Boolean : false : IRWA)
	// If true, items (buttons) can be dropped into this toolbar, and the toolbar will
    // show a drop line at the drop location.  Override drop() to decide what happens when the
    // item is dropped.
    //
	//		@group	dragndrop
    //	@visibility external
	//<

    

    //>	@attr	toolbar.reorderOnDrop       (boolean : true : IRWA)
    //     On drop, should the Toolbar rearrange the buttons array?  Set to false by advanced
    //     classes that want to manage reordering themselves.
	//		@group	dragndrop
	//<
    reorderOnDrop:true,

    //>	@attr	toolbar.tabWithinToolbar   (boolean : true : IRWA)
    //      Should each button in the toolbar be included in the tab-order for the page, or
    //      should only one button in the toolbar show up in the tab-order, and arrow-keys be
    //      used to switch focus within the toolbar?
	//<
    tabWithinToolbar:true,
    

    

    //> @attr toolbar.allowButtonReselect (boolean : false : IRWA)
    // When a button is clicked but is already selected, should an additional
    // +link{buttonSelected} event be fired?
    //<
    allowButtonReselect:false,

	//>	@attr	toolbar.buttonDefaults		(object : varies : [IRWA])
    // Settings to apply to all buttons of a toolbar. Properties that can be applied to
    // button objects can be applied to all buttons of a toolbar by specifying them in
    // buttonDefaults using the following syntax:<br>
    // <code>buttonDefaults:{property1:value1, property2:value2, ...}</code><br>
    // See the Button Widget Class for standard button properties.
	//		@group	appearance
	//      @see class:Button
    //      @visibility external
	//<
	//	The following are defaults for all toolbar buttons.
	//	To add properties to all buttons of ALL toolbars, change the below.
	//	To add properties to all buttons of a particular toolbar you're creating,
	//		add a "button" property to the toolbar constructor with the defaults
	//		you want applied to the buttons.  This will automatically be added to each button.
	buttonDefaults: {
		click : function() {
            this.Super("click", arguments);
			this.parentElement.itemClick(this, this.parentElement.getButtonNumber(this))
		},
		doubleClick : function () {
		    this.Super("doubleClick", arguments);
			this.parentElement.itemDoubleClick(this, this.parentElement.getButtonNumber(this))
		},
        setSelected : function() {
            var oldState = this.isSelected();
            this.Super("setSelected", arguments);
            if (this.parentElement &&
                (this.parentElement.allowButtonReselect || oldState != this.isSelected()))
            {
                if (this.isSelected()) this.parentElement.buttonSelected(this);
                else this.parentElement.buttonDeselected(this);
            }
        },
        dragAppearance:isc.EventHandler.NONE,

        // Toolbars typically manipulate the tabIndex of their buttons.
        // If the user specifies a tabIndex on a toolbar button directly, assume they are
        // managing the tabIndex for the button - clear the flag that marks the button as having 
        // it's tabIndex managed by the toolbar
        setTabIndex : function (index) {
            this.Super("setTabIndex", arguments);
            this._toolbarManagedTabIndex = null;
        },
        
        // Override setAccessKey to take a second parameter, indicating that the accessKey is
        // being set by the toolbar
        // If this parameter is not passed in, assume the user / developer is setting the
        // accessKey and clear the flag that marks the button's accessKey as being managed by
        // the toolbar
        setAccessKey : function (accessKey, managedByToolbar) {
            if (!managedByToolbar) this._toolbarManagedAccessKey = null;
            this.Super("setAccessKey", [accessKey]);
        },
        
        // When focus goes to a button, set the tabIndex of the button to the toolbars tabIndex.
        // This means when tabbing out of the button, the focus will go to the appropriate next
        // element - use the _updateFocusButton() method on the toolbar to achieve this.        
        focusChanged : function (hasFocus) {
            if (this.hasFocus && this.parentElement._updateFocusButton) {
                this.parentElement._updateFocusButton(this)
            }
        },
            
        _focusInNextTabElement : function (forward, mask) {
            if (this.parentElement._focusInNextTabElement) {
                this.parentElement._focusInNextTabElement(forward, mask, this); 
            }
        }
	}
});


isc.Toolbar.addMethods({

//>	@method	toolbar.draw()	(A)
//	Override the draw method to set up the buttons first
//		@group	drawing
//<
draw : function (a,b,c,d) {
    if (isc._traceMarkers) arguments.__this = this;

	if (!this.readyToDraw()) return this;

    // If we've never init'd our buttons, do so now by calling setButtons with no parameters
	if (!this._buttonsInitialized) this.setButtons();
    
	this.invokeSuper(isc.Toolbar, "draw", a,b,c,d);
},


//>	@method	toolbar.keyPress()
// Override keypress to allow navigation between the buttons on the toolbar
//		@group	events
//<
// Note - this is typically going to be bubbled up from the menu bar buttons
keyPress : function () {
    var keyName = this.ns.EH.lastEvent.keyName;
    // note - if we're allowing the user to tab between the buttons on the toolbar, we don't need
    // to give them the navigation via arrow keys.
    if (!this.tabWithinToolbar) {
        if ((this.vertical && keyName == "Arrow_Up") || 
            (!this.vertical && keyName == "Arrow_Left")) {
            
            this._focusInNextButton(false);
            return false;

        } else if ((this.vertical && keyName == "Arrow_Down") || 
                   (!this.vertical && keyName == "Arrow_Right")){
            this._focusInNextButton();
            return false;
        }
    }
    
    return this.Super("keyPress", arguments);
},

_focusInNextButton : function (forward, startingIndex) {
    
    // Note - this.buttons is the list of button init objects.  The live widgets are available
    // via this.getMembers()
    forward = (forward != false);
    var focusIndex = (startingIndex != null ? startingIndex : this.getFocusButtonIndex());
    if (focusIndex == null) focusIndex = (forward ?  -1 : this.buttons.length);

    // find the next focusable member in this direction, if any
    focusIndex += forward ? 1 : -1;
    while (focusIndex >=0 && focusIndex < this.buttons.length) {
        var button = this.getMembers()[focusIndex]; 
        if (button._canFocus()) {
            button.focus();
            // Returning true will indicate successful shift of focus
            return true;
        }
        focusIndex += forward ? 1 : -1;
    }
    return false;
},

//> @method toolbar.getFocusButtonIndex()  (A)
//  @return (number)    Index of whichever button currently has focus for keyboard input
//                      [On a mouse click, this will typically match the value returned by
//                      toolbar.getMouseOverButtonIndex(), but is likely to differ if the button
//                      was activated by keyboard interaction]
//<
getFocusButtonIndex : function () {
    
    var buttons = this.getButtons(),
        focusItemNum;
    for (var i = 0; i < buttons.length; i++) {
        if (buttons[i].hasFocus) {
            focusItemNum = i;
            break;
        }
    }
    return focusItemNum;
},

// _focusInNextTabElement() - used when we're managing synthetic focus due to showing a 
// clickMask.
// Since we do custom management of our buttons' tabIndices, we need to also explicitly 
// manage synthetic tabbing to them

_focusInNextTabElement : function (forward, mask, button) {
    if (!isc.EH.targetIsMasked(this, mask)) {
        var focusButton = button ? this.members.indexOf(button) : null;
        
        if (!this.tabWithinToolbar) {
            if (forward && focusButton == null) { 
                var fb = this._currentFocusButton;
                if (fb != null) return this.fb.focus();
            }

        } else if (this._focusInNextButton(forward, focusButton)) return;
    }
    return this.Super("_focusInNextTabElement", arguments);   
},

// Widget level _canFocus
// Override this to return true. This will ensure that if a hard mask is showing, and we're 
// doing synthetic tab index management, the toolbar doesn't get skipped.
_canFocus : function (a,b,c,d) {
    var members = this.members;
    if (members && members.length > 0) {
        for (var i = 0; i < members.length; i++) {
            if (members[i]._canFocus()) return true;
        }
    }
    return this.invokeSuper(isc.Toolbar, "_canFocus", a,b ,c,d);
},

// Override focus() to put focus into the button(s) in the toolbar

// Override 'setFocus()' to update button focus only.

setFocus : function (hasFocus) {
    if (!this._readyToSetFocus()) return;
    var buttonIndex = this.getFocusButtonIndex();
    if (!hasFocus) {
        if (buttonIndex != null && this.members) this.members[buttonIndex].setFocus(false);
    } else {
        // If one of our buttons already has focus, just no op.
        if (buttonIndex != null) return;
        
        if (this._currentFocusButton) this._currentFocusButton.setFocus(true);
        else this._focusInNextButton();
    }
},

// Override focusAtEnd() so we can put focus into the first / last button if appropriate
focusAtEnd : function (start) {
    if (!this.tabWithinToolbar) {
        return this.Super("focusAtEnd", arguments);
    }

    // typecast start to a boolean before passing it to 'focusInNextButton' as the 'forward' 
    // param.
    start = !!start;
    var focusIndex = (start ?  -1 : this.buttons.length);
    this._focusInNextButton(start, focusIndex);
},

// An internal method to set the tab index of a button, and flag the button as having it's tab index
// managed by the toolbar.
_setButtonTabIndex : function (button, newTabIndex) {

    
    if (!button._toolbarManagedTabIndex && 
        (button._getNextTabWidget() != null || button._getPreviousTabWidget() != null)) 
    {
         button._removeFromAutoTabOrder();
    }         
    
    // Note that the toolbar is managing the tab index of the button
    button._toolbarManagedTabIndex = true;
    
    // update the tab index of the button.
    
    if (button.tabIndex != newTabIndex) button._setTabIndex(newTabIndex, false);
},

// Override updateMemberTabIndex (inherited from Layout)
// to be a No-Op, since we manage our members' (buttons') tabindices

updateMemberTabIndex : function () {
},


_slotChildrenIntoTabOrder : function () {
},

// _setButtonAccessKey()
// Internal method to set the accessKey for a button within this toolbar.
// Also sets the flag '_toolbarManagedAccessKey' on the button
_setButtonAccessKey : function (button, key) {
    button._toolbarManagedAccessKey = true;
    // see comment in the override for setAccessKey for why we're passing in this 2nd parameter
    button.setAccessKey(key, true);
},

    

// setupButtonFocusProperties()
// An internal method to set the tab indexes of any buttons in the toolbar without existing 
// user-specified tab indexes
setupButtonFocusProperties : function () {
    // first update the 'currentFocusButton' if its out of date.
    // This will set the tabIndex and accessKey for the button (unless that would override an
    // explicitly specified property for the button).
    
    // Note - this.buttons is the list of button init objects.
    // The actual button objects are available via this.getButtons()

    var focusButton = this._currentFocusButton; 
   
    if ( (!focusButton || !isc.isA.Canvas(focusButton) || 
          focusButton.visibility == isc.Canvas.HIDDEN ) && this.buttons.length > 0) 
    {
        var newFocusButton;
        for (var i = 0; i < this.members.length; i++) {
            
            if (isc.isA.Canvas(this.members[i]) &&
                this.members[i].visibility != isc.Canvas.HIDDEN) 
            {
                newFocusButton = this.members[i];
                break;
            }
        }
        this._updateFocusButton(newFocusButton)
        focusButton = this._currentFocusButton;
    }

    
    var defaultTabIndex;
    if (this.tabWithinToolbar) {
        defaultTabIndex = this.getTabIndex();
    } else {
        defaultTabIndex = -1;
    }

    // update the tabIndex of any buttons who have no user-specified tab index, and 
    // for which we haven't yet managed the tabIndex    
    var buttons = this.getButtons();
    for (var i = 0; i < buttons.length; i++) {
        var button = buttons[i];
        if (button != focusButton &&
            (button.tabIndex == null || button._autoTabIndex)) 
        {
            
            //this.logWarn("updating tab index of: " + button + " to " + defaultTabIndex);
            this._setButtonTabIndex(button, defaultTabIndex)
        }
    }        
},


_updateFocusButton : function (newFocusButton) {
    if (!newFocusButton) return;
    
    // Bail if the current focus button was passed in
    if (this._currentFocusButton == newFocusButton) {
        return;
    }
    
    // Update the accessKey for the current focus button unless it has / had an explicitly
    // specified accessKey
    if (newFocusButton.accessKey != this.accessKey &&
        (newFocusButton.accessKey == null || newFocusButton._toolbarManagedAccessKey)) 
    {
        this._setButtonAccessKey(newFocusButton, this.accessKey)
    }
    
    // Update focus button tab index (if allocated by us)
    if (newFocusButton.tabIndex == null || newFocusButton._autoTabIndex || 
        newFocusButton._toolbarManagedTabIndex) 
    {
        
        // set the newly focused button to the tabIndex of the Toolbar
        this._setButtonTabIndex(newFocusButton, this.getTabIndex());
    }
    
    var oldFocusButton = this._currentFocusButton;
    // If appropriate, remove the previous focus button from the tab order
    if (oldFocusButton != null && 
        (oldFocusButton.tabIndex == null || oldFocusButton._autoTabIndex || 
         oldFocusButton._toolbarManagedTabIndex)) 
    {
        // Remove from tab order if we are not tabbing between buttons
        if (!this.tabWithinToolbar) this._setButtonTabIndex(oldFocusButton, -1);
        
        // Clear the accessKey property if it was added by the toolbar
        if (oldFocusButton.accessKey != null && 
            oldFocusButton._toolbarManagedAccessKey) 
        {
            this._setButtonAccessKey(oldFocusButton, null)
        }
    }
    this._currentFocusButton = newFocusButton;
},

// Override _setTabIndex() to set also update the tab index of the buttons
_setTabIndex : function (a,b,c,d) {
    this.invokeSuper(isc.Toolbar, "_setTabIndex", a,b,c,d);

    // if this.tabWithinToolbar is true, update each of the buttons' tab index to match the
    // toolbars new tab index
    if (this.tabWithinToolbar) {
        var buttons = this.getButtons();
        for (var i = 0; i < buttons.length; i++) {
            if (buttons[i].tabIndex == null || buttons[i]._autoTabIndex ||
                buttons[i]._toolbarManagedTabIndex) 
                this._setButtonTabIndex(buttons[i], this.getTabIndex())
        }
    // otherwise use _updateFocusButton to update the tab index of the focus button only (other
    // buttons' tab index will already be -1 -- no need to change)        
    } else {
        var button = this._currentFocusButton;
        if (button != null) {
            this._currentFocusButton = null;
            this._updateFocusButton(button);
        }
    }
},

// Override setAccessKey() to alo set the accessKey for the toolbar
setAccessKey : function (accessKey) {
    this.Super("setAccessKey", arguments);
    
    // use updateFocusButton to update the accessKey for the focus button
    var button = this._currentFocusButton;
    if (button != null) {
        this._currentFocusButton = null;
        this._updateFocusButton(button);
    }
    
},

getLength : function (a,b,c,d) {
	// the Toolbar allows overriding the area allocated to layout members, so that it may be
    // larger or smaller than the Layout's area.
    if (this.innerWidth != null) return this.innerWidth;
    return this.invokeSuper(isc.Toolbar, "getLength", a,b,c,d);
},

//>	@method	toolbar.setButtons()
// Apply a new set of buttons to render in this toolbar as +link{toolbar.buttons}.
// 
// @param [newButtons] (Array of Button Properties) properties to create each button from
// @visibility external
//<
setButtons : function (newButtons) {

    // one time flag - allows us to set up our buttons on initial draw only.
    // If 'setButtons' is called before draw we won't unnecessarily remove and re-add them all.
    this._buttonsInitialized = true;
    
	//this.logWarn("setButtons at\n" + this.getStackTrace());

	// if buttons are passed in, use those 
    // Otherwise we'll just make actual button instances from the current items in this.buttons
	if (newButtons) this.buttons = newButtons;

    if (this.members == null) this.members = [];
    
    // destroy any existing members, and create new buttons from scratch
    var _buttons = this.members.duplicate();        
    for (var i = 0; i < _buttons.length ; i++) {
        var oldButton = _buttons[i];
        // destroy any members we automatically created from the buttons array
        if (!this.buttons.contains(oldButton)) {
            //this.logWarn("destroying old button " + i);
            // destroying it will automagically remove it from this as a member, so no
            // need to call this.removeMembers()
            _buttons[i].destroy();
        }
    }

	// now create actual button widgets
    if (this.buttons == null) this.buttons = [];

    var newMembers = [];
    for (var i = 0; i < this.buttons.length; i++) {
        var button = this.buttons[i];

        // allow widgets to be placed directly in the buttons array, which we simply add as
        // members and ignore.  These members will not have pick up buttonDefaults, hence won't
        // fire itemClick, have associated panes, allow managed resize, etc.
        if (!isc.isA.Canvas(button)) button = this.makeButton(button);
                
        newMembers[newMembers.length] = button;

        if (isc.isA.StatefulCanvas(button)) {
            var actionType = button.getActionType();

            if (actionType == isc.StatefulCanvas.RADIO) {
                            
                // For actionType:radio buttons, remember initial selected button.  
                // We update this on selection change.
                // This property will be returned on 'Toolbar.getSelectedButton()'
                // Note - no error checking for multiple selection within a toolbar
                // If each 'actionType' RADIO button has no specified radiogroup, the default
                // toolbar behavior is to put them into the same radioGroup. In this case default 
                // radiogroup selection behavior inherited from StatefulCanvas will prevent
                // multiple selection within a toolbar.
                // If the user has specified a radiogroup for any actionType:RADIO buttons, we
                // can't guarantee there won't be multiple selection within a toolbar.
                // In this case 'getSelectedButton()' will return the most recently selected RADIO
                // button within this toolbar, rather than the only selected radio button.
                if (button.selected) this.lastSelectedButton = button;
            }
        }
    }
    this.addMembers(newMembers, 0);

    if (this.canResizeItems) this.setResizeRules();
    // Set up the tab indexes for the buttons, and the accessKey for the focus button
    this.setupButtonFocusProperties();

},

// shouldHiliteAccessKey implementation for buttons in this toolbar
buttonShouldHiliteAccessKey : function () {
    // If the accessKey comes from the toolbar itself, don't hilite
    // otherwise we will end up with underlining of the title on multiple buttons which
    // is likely to look odd.
    if (this._toolbarManagedAccessKey) return false;
    return this.hiliteAccessKey;
},

makeButton : function (button) {
    // the default sizing behavior we want:
    // - horizontal toolbars autoSize button heights to the Toolbar's height and autoSize
    //   button widths to the button text.  
    // - vertical toolbars autoSize button width to the Toolbar's width and autoSize button
    //   heights to the (wrapped) button text.

    
    button.width = button.width || null;
    button.height = button.height || null;

    // set button properties to enable/disable dragging and dropping, so that dragging will
    // be allowed on members and will bubble to the Toolbar
    
    button.canDrag = this.canReorderItems || this.canDragSelectItems || this.canRemoveItems;

    // don't override canDragResize to true on the button if it's been explicitly turned off
    button.canDragResize = (button.canDragResize != null ? 
        button.canDragResize && this.canResizeItems : this.canResizeItems);

    // toolbar allows things to be dropped on it (currently no default behavior for what happens
    // on drop)
    button.canAcceptDrop = this.canAcceptDrop;
    // if you can drag items out of the toolbar, make the buttons droppable
    button.canDrop = this.canRemoveItems;
    
    button.shouldHiliteAccessKey = this.buttonShouldHiliteAccessKey;

    // create a new button widget
    //this.logWarn("creating new button " + i);
    return this._makeItem(button, null);
},

//>	@method	toolbar._makeItem()
// Creates and returns a widget for the toolbar
//		@group	drawing
//
//		@param	[buttonProperties]	(object)	the button properties
//		@param	[rect]		(object)	the rectangle for this widget, e.g. {top:50, left:100, ...}
//
//		@return	(object)	the created widget
//<
_makeItem : function (buttonProperties, rect) {
	var cons = (buttonProperties.buttonConstructor
						? buttonProperties.buttonConstructor
						: this.buttonConstructor
					  )
	;
	cons = this.ns.ClassFactory.getClass(cons);
	
	var item = cons.newInstance(
				{autoDraw:false},
				this.buttonDefaults,	// isc.Toolbar class defaults
				this.buttonProperties,	// isc.Toolbar instance defaults
				buttonProperties,       // properties for this button
				rect					// rectangle for the button
			);

    if (!isc.isA.StatefulCanvas(item)) return item;

    // if the button is of actionType 'radio' and the developer has not specified a
    // radioGroup, set radioGroup to the ID of this toolbar
    // Developer can override by setting 'radioGroup' property explicitly on the
    // item's properties.
    var unset;        
    if ((item.getActionType() == isc.StatefulCanvas.RADIO && item.radioGroup === unset)
        || item.defaultRadioGroup != null) {
        var rg = item.defaultRadioGroup != null ? item.defaultRadioGroup : this.getID();
        item.addToRadioGroup(rg);
    }
    
    return item;
},

//>	@method	toolbar.addButtons()
// Add a list of buttons to the toolbar
// @param [buttons]	(Array of objects) list of button object initializers.
// @param [position] (number) position to add the new buttons at
// @visibility external
//<
addButtons : function (buttons, position) {

    if (buttons == null) return;
    if (!isc.isAn.Array(buttons)) buttons = [buttons];

    if (!this._buttonsInitialized) this.setButtons();
    buttons.removeEvery(null);

    // (currently undocumented) feature - support passing in position for each
    // button being added - in this case the 2nd argument will be an array of the
    // same length as the buttons array.
    // Break into discrete blocks of adjacent buttons so we can use standard list manipulation
    // APIs rather than having to iterate through every button and add as a member individually,
    // rerunning layout.
    var discreteBlocks;
    if (isc.isAn.Array(position)) {
        if (position.length != buttons.length) {
            this.logWarn("addButtons passed " + buttons.length + " buttons with " + position.length
                + " discrete positions specified. Ignoring.");
            return;
        }
        var mapping = {};
        for (var i = 0; i < position.length; i++) {
            mapping[position[i]] = buttons[i];
        }
        
        // sort the positions - we'll need to add buttons starting with the
        // leftmost position (Otherwise adding an item, then a second item to the left of
        // it would shift the first item a slot to the right from the desired position).
        position.sort();
        
        discreteBlocks = [];
        var currentBlock = {buttons:[], position:position[0]},
            currentIndex = 0;
        for (var i = 0; i < position.length; i++) {
            var pos = position[i],
                button = mapping[pos];
                
            currentBlock.buttons.add(button);
            
            var nextPos = position[i+1]
            if (nextPos == null || nextPos != pos+1) {
            
                discreteBlocks[currentIndex] = currentBlock;
                currentIndex++
                
                // New discrete block for the next time through this loop
                if (nextPos != null) currentBlock = {buttons:[], position:nextPos};
            }
        }
        for (var i = 0; i < discreteBlocks.length; i++) {
            this.buttons.addListAt(discreteBlocks[i].buttons, discreteBlocks[i].position);
        }
    } else {    
        // Update this.buttons to include the new buttons:
        this.buttons.addListAt(buttons, position);
    }
    
    // if instantRelayout is true, delay the relayout until we've added the full
    // set of members
    var forceReflow = this.instantRelayout;
    this.instantRelayout = false;

    // Add as members to the right position, and let layout handle spacing and stuff
    var buttonWidgets;
    if (discreteBlocks == null) {
        buttonWidgets = this._createButtonInstances(buttons);
        this.addMembers(buttonWidgets, position)
    } else {
        for (var i = 0; i < discreteBlocks.length; i++) {
            var currentButtons = this._createButtonInstances(discreteBlocks[i].buttons);
            this.addMembers(currentButtons, discreteBlocks[i].position);
            
            if (buttonWidgets == null) buttonWidgets = currentButtons;
            else buttonWidgets.addList(currentButtons);
        }
    }
    if (forceReflow) {
        this.instantRelayout = true;
        if (this._layoutIsDirty) this._layoutIsDirty = false;
        this.reflow("addButtons");
    }
    
    // setResizeRules to update dragResizing, etc.
    if (this.canResizeItems) this.setResizeRules();

    buttonWidgets.map("show");  // auto-show the new members
},

_createButtonInstances : function (buttons) {
    var buttonWidgets = [];
    for (var i = 0; i < buttons.length; i++) {
       	var button = buttons[i],
        // call makeButton() to convert the init block to a widget with the appropriate
        // propoerties (canDrag, buttonDefaults, etc)
        // Note that canvases are just integrated into the buttons block without
        // attempting to modify properties, as with setButtons()
        buttonWidget = isc.isA.Canvas(button) ? button : this.makeButton(button);
        buttonWidgets[i] = buttonWidget;
    }
    return buttonWidgets;
},


//>	@method	toolbar.removeButtons()
//	Remove a list of buttons from the toolbar
//
// @param [buttons]	(Array) Array of buttons to remove. Buttons may be specified as pointers to 
// the button instances contained in this toolbar, or numbers indicating the index of the buttons
// in <code>this.buttons</code>.
// @visibility external
//<
removeButtons : function (buttons) {
    if (buttons == null) return;
    if (!isc.isAn.Array(buttons)) buttons = [buttons];
    
    // We're going to manipulate the this.buttons array (button description objects), and
    // the actual buttons in this.members - so will need pointers to both the 
    // button descriptor objects and the button instances.
    var buttonWidgets = [];
    
    // The buttons to remove can be specified as:
    // a) Index in this.buttons
    // b) Button widget
    // c) Button instantiation block
    // d) ID of button
    for (var i =0; i < buttons.length; i ++) {

        // resolve whatever object was passed in to a button instantiation block
        buttons[i] = this.buttons[this.getButtonNumber(buttons[i])];        
        
        if (buttons[i] == null)  {
            this.logWarn("removeButtons(): unable to find button for item number " + i + 
                        " in the array passed in.  Skipping this item.");
            buttons.removeItem(i);
            i -= 1;
            continue;
        }
        // get a pointer to the Canvas as well
        buttonWidgets[i] = this.getButton(this.buttons.indexOf(buttons[i]))
    }
    
    var completeButtons = this.buttons;
    // if (any of) the buttons aren't in this.buttons, this has no effect
    completeButtons.removeList(buttons);
  
    this.removeMembers(buttonWidgets);
// should we destroy them?
    
},

//>	@method	toolbar.getButton() ([])
//          Retrieves a button widget instance (within this toolbar) from the name / ID / index / 
//          descriptor object for the button (as with the getButtonNumber() method)
//          This provides a way to access a toolbar button's properties and methods directly.
//      @see    getButtonNumber()
//      @visibility external
//		@group	buttons
//		@param	index		(number | string | object)    identifier for the button to retrieve
//
//      @return (Button)    the button, or null if the button wasn't found
//<
getButton : function (index) {
    index = this.getButtonNumber(index);
	return this.getMember(index);
},

//>	@method	toolbar.getButtonNumber()	(A)
//			get the index of a button in the buttons array<p>
//          The button can be specified as - 
//          <ul>
//          <li>an index within this.buttons (just returned)
//          <li>the ID property of a button
//          <li>a pointer to the button descriptor object in this.buttons
//          <li>the actual button widget in this.members
//          </ul><p>
//			returns -1 if not found
//
//		@param	button		(number | string  | button object | button widget)
//
//		@return	(number)	index of the button in question
// @visibility external
//<
getButtonNumber : function (button) {
	// if we're passed an Object that isn't a Canvas, it might be a button configuration object
    if (isc.isAn.Object(button) && !isc.isA.Canvas(button)) return this.buttons.indexOf(button);
    // otherwise use normal member lookup
    return this.getMemberNumber(button);
},

//>	@method	toolbar.getButtons()
//		@group	buttons
//		@return (array) array of all buttons in the Toolbar
//<
getButtons : function () {
	return this.members;
},

//> @method toolbar.setCanResizeItems() 
// Setter for updating +link{toolbar.canResizeItems} at runtime.
// @param canResizeItems (boolean) New value for this.canResizeItems
// @visibility external
//<
setCanResizeItems : function (canResizeItems) {
    if (this.canResizeItems == canResizeItems) return;
    this.canResizeItems = canResizeItems;
    var buttons = this.getButtons();
    if (!buttons) return;
    for (var i = 0; i < buttons.length; i++) {
        var item = buttons[i];
        item.canDragResize = (item.canDragResize != null ? 
            item.canDragResize && canResizeItems : canResizeItems);
    }
    this.setResizeRules();
},

// update which edges a button can be resized from.
//
// When you dragResize buttons, it always effects the button on the left (or top), regardless
// of which side of the boundary between the buttons you click on (this is pulled off by
// switching the dragTarget on the fly).  This means the sides that each button can be resized
// from is affected by whether the adjacent buttons can be resized.  setResizeRules updates
// this; it needs to be called on any reorder, addition or removal of buttons.
setResizeRules : function () {
    if (!this.members) return;

    var rtl = this.isRTL();

    // buttons can resize along the long axis of the toolbar. 
    var edgeCursorMap, resizeFrom, resizeFromOneSide;
    if (this.vertical) {
        edgeCursorMap = {"T":isc.Canvas.ROW_RESIZE, "B":isc.Canvas.ROW_RESIZE };
        resizeFrom = ["T","B"];
        resizeFromOneSide = ["B"];
    } else {
        var colResizeCursor = rtl ? isc.Canvas.RTL_COL_RESIZE : isc.Canvas.COL_RESIZE;
        edgeCursorMap = {"L":colResizeCursor, "R":colResizeCursor };
        resizeFrom = ["L","R"];
        if (!rtl) {
            resizeFromOneSide = ["R"];
        } else {
            resizeFromOneSide = ["L"];
        }
    }

    var previousCantResize = false;
    for (var i = 0; i < this.members.length; i++) {
        var button = this.members[i];
        if (!button.canDragResize) {
            button.resizeFrom = button.edgeCursorMap = null;
            previousCantResize = true;
        } else {
            if (previousCantResize || i == 0)
            { 
                // the first button, or any button next to a button that can't resize, is not
                // allowed to resize from it's left/top.
                button.resizeFrom = resizeFromOneSide;
            } else {
                button.resizeFrom = resizeFrom;
            }
            button.edgeCursorMap = edgeCursorMap;
            previousCantResize = false;
        }
    }
},

//>	@method	toolbar.getSelectedButton()	(A)
// Get the button currently selected.
//		@return (object) button
//<
getSelectedButton : function () {
	return this.lastSelectedButton;
},	
	
//>	@method	toolbar.selectButton()  ([])
// Given an identifier for a button, select it.
// The button identifier can be a number (index), string (id), or object (widget or init block),
// as with the getButtonNumber() method.
// 
//      @see    getButtonNumber()
//		@group	selection
//		@param	buttonID		(number | string | object | canvas)    Button / Button identifier
//      @visibility external
//<
selectButton : function (buttonID) {

	if (!this.members) return;
	var btn = this.getButton(buttonID);
	if (btn && isc.isA.StatefulCanvas(btn)) btn.select();
},


//>	@method	toolbar.deselectButton()    ([])
//	Deselects the specified button from the toolbar, where buttonID is the index of
//  the button's object initializer. The button will be redrawn if necessary.
//  The button identifier can be a number (index), string (id), or object (widget or init block),
// as with the getButtonNumber() method.
//      @see    getButtonNumber()
//      @visibility external
//		@group	selection
//		@param	buttonID		(number | string | object | canvas)    Button / Button identifier
//<
deselectButton : function (buttonID) {
	var btn = this.getButton(buttonID);
	if (btn) btn.deselect();
},


//>	@method	toolbar.buttonSelected()	(A)
// One of the toolbar button was just selected -- update other buttons as necessary
//		@group	selection
//
//		@param	button		(button object)		a member of this.buttons
//<
buttonSelected : function (button) {
	if (button.getActionType() == isc.Button.RADIO) {
        this.lastSelectedButton = button;
	}
},


//>	@method	toolbar.buttonDeselected()	(A)
// Notification that one of the toolbar buttons was just DEselected
//		@group	selection
//
//		@param	button		(button object)		a member of this.buttons
//<
buttonDeselected : function (button) {
},


//>	@method	toolbar.itemClick() ([A])
//	Called when one of the buttons receives a click event
//		@group	event handling
//		@param	item		(button)		pointer to the button in question
//		@param	itemNum		(number)		number of the button in question
// @visibility external
//<
itemClick : function (item, itemNum) {
},

//>	@method	toolbar.itemDoubleClick() ([A])
//	Called when one of the buttons receives a double-click event
//		@group	event handling
//		@param	item		(button)		pointer to the button in question
//		@param	itemNum		(number)		number of the button in question
// @visibility external
//<
itemDoubleClick : function (item, itemNum) {
},

//>	@method	toolbar.getMouesOverButtonIndex()	(A)
//  @return (number) the number of the button the mouse is currently over, 
//                   or -1 for before all buttons, -2 for after all buttons
//                  See also getFocusButtonIndex()
//<
getMouseOverButtonIndex : function () {
	var offset = this.vertical ? this.getOffsetY() : this.getOffsetX();
	
	if (this.isRTL() || this.align == isc.Canvas.RIGHT) {
	    var leftGap = this.getInnerWidth() - this.memberSizes.sum();
	    if (leftGap > 0) offset-= leftGap;
	    
	}
    return this.inWhichPosition(this.memberSizes, offset, this.getTextDirection());
},


// Override prepareForDragging to handle dragResize / dragReorder of items in the toolbar.
prepareForDragging : function () {
    // NOTE: we currently set a canDrag, canDragResize, etc flags on our children.  However, we
    // could manage everything from this function instead, eg, pick dragResize if there is a
    // resize edge hit on the child, otherwise dragReorder.

    var EH = this.ns.EH;
    // This custom handling is for events bubbled from a member being drag repositioned
    // (drag reorder) or drag resized.
    // 
    
    var lastTarget = EH.lastEvent.target;
    while (lastTarget.dragTarget) {
        lastTarget = lastTarget.dragTarget;
    }
    var operation = EH.dragOperation;
    
    
    if (( (this.canResizeItems && operation == "dragResize") 
          || (this.canReorderItems && operation == "drag")
         ) && this.members.contains(lastTarget)) 
    {
        
        // If we hit a valid resize edge on a member, the member will have set the dragOperation to
        // dragResize
        if (operation == "dragResize") {
            // for drag resizes on the length axis, do specially managed resizing.  Don't interfere
            // with breadth-axis resize, if enabled
            if ((this.vertical && ["T","B"].contains(EH.resizeEdge)) ||
                (!this.vertical && ["L","R"].contains(EH.resizeEdge)))
            {
                EH.dragOperation = "dragResizeMember";
                // We can just return - prepareForDragging() is bubbled so was already fired
                // on the member and set up EH.dragTarget in this case
                return;
            }
        // otherwise, starting a drag on a button means dragReordering the members.
        } else if (operation == "drag") {
            EH.dragOperation = "dragReorder";
            return;
        }
    }
    
    return this.Super("prepareForDragging", arguments);
},

// Drag Reordering
// --------------------------------------------------------------------------------------------

// get the position where the button being reordered would be dropped, if dragging stopped at the
// current mouse coordinates
getDropPosition : function () {
    var position = this.getMouseOverButtonIndex();

    
    var EH = this.ns.EH,
        switchInMiddle = (this.reorderStyle == "explorer" || 
                        (EH.dropTarget && EH.dropTarget.parentElement == this));
    if (switchInMiddle && position >= 0) { 
        // if we are over a member, check whether we should switch to the next member or final
        // coordinate
        var buttonSize = this.memberSizes[position],
            offset = (this.vertical ? this.getOffsetY() : this.getOffsetX());
            
        
        offset -= this.memberSizes.slice(0, position).sum();
        var oldPosition = position;
        // switch to next coordinate in the middle of the button 
        if (offset > buttonSize/2) position++;

        //this.logWarn("oldPosition: " + oldPosition +
        //             ", size: " + buttonSize +  
        //             ", offset: " + offset +
        //             ", position: " + position);
    }

    var numMembers = this.members.length,
        maxIndex = (switchInMiddle ? numMembers : numMembers - 1);

    // for reorder/self-drop interactions, when we drag out of the Layout, we revert to the
    // original position.  For external drops, the only remaining case is a coordinate within
    // the Layout, but before all members.
    var revertPosition = this.dragStartPosition || 0,
        selfDrag = EH.dragTarget && EH.dragTarget.parentElement == this;

    // if beyond the last member, but still within the layout rect, convert to last member
	if (position == -2 && this.containsEvent()) {
	    position = maxIndex;
    }

	if (position < 0 || position > maxIndex) position = revertPosition;

    // for reorder/self-drop, check canReorder flag
    else if (selfDrag && (this.members[position] && this.members[position].canReorder == false)) 
    {
	    position = revertPosition;
    }
    return position;
},

// sent when button dragging for reordering begins
dragReorderStart : function () {
	var EH = this.ns.EH,
		startButton = EH.dragTarget
	;

	// if the button's canReorder property is false, it can't be reordered so forget it!
	if (startButton.canReorder == false) return false;
	
    

	if (startButton.showDown) startButton.setState(isc.StatefulCanvas.STATE_DOWN);	
	
	// get the item number that reordering started in (NOTE: depended on by observers like LV)
	this.dragStartPosition = this.getButtonNumber(startButton);
	return EH.STOP_BUBBLING;
},

// sent when button moves during drag-reorder
dragReorderMove : function () {
	var EH = this.ns.EH,
		startButton = EH.dragTarget,
		startPosition = this.dragStartPosition,
		currentPosition = this.getDropPosition();
	
    //this.logWarn("dragReorderMove: position: " + this.getMouseOverButtonIndex() + 
    //             ", drop position: " + this.getDropPosition());

	// remember the current position (NOTE: depended on by observers like LV)
	this.dragCurrentPosition = currentPosition;

    

    // create a temporary order for the members and lay them out in that order
    var members = this.members.duplicate();
    members.slide(startPosition, currentPosition);
    //this.logWarn("startPos: " + startPosition + ", currentPos: " + currentPosition + 
    //             "members: " + this.members + ", reordered: " + members);
    // NOTE: tell stackMembers() not to update sizes, since this is a temporary order
    this.stackMembers(members, null, false);

	return EH.STOP_BUBBLING;
},

// sent when button dragging for reordering ends
dragReorderStop : function () {
	var EH = this.ns.EH,
		startButton = EH.dragTarget,
		startPosition = this.dragStartPosition,
		currentPosition = this.dragCurrentPosition;

	startButton.setState(isc.StatefulCanvas.STATE_UP);

    

    if (currentPosition == startPosition) return false;

	// if we're supposed to actually reorder on drop, reorder now
    if (this.reorderOnDrop) this.reorderItem(currentPosition, startPosition);
    
    // notify observers
    if (this.itemDragReordered) this.itemDragReordered(startPosition, currentPosition);

	return EH.STOP_BUBBLING;
},

//>	@method	toolbar.dragStop()	(A)
//		@group	events, dragging
//			handle a dragStop event
//<
dragStop : function () {
    // NOTE: called at the end of an inter-toolbar move iteraction, not a dragReorder
	var EH = this.ns.EH,
        startButton = EH.dragTarget,
		startPosition = this.dragStartPosition;

	startButton.setState(isc.StatefulCanvas.STATE_UP);
    this.hideDropLine();

	return EH.STOP_BUBBLING;
},

// reorder an item programmatically
reorderItem : function (itemNum, newPosition) {
    this.reorderItems(itemNum, itemNum+1, newPosition);
},

// reorder multiple items programmatically
reorderItems : function (start, end, newPosition) {
    // reorder the button config
	this.buttons.slideRange(start, end, newPosition);
    // and array of button widgets
    this.reorderMembers(start, end, newPosition);
    // update which buttons can resize
    this.setResizeRules();
},



// Drag Resizing (of buttons)
// --------------------------------------------------------------------------------------------

// sent whem button dragging for resizing begins
dragResizeMemberStart : function () {
	var EH = this.ns.EH,
		item = EH.dragTarget,
		itemNum = this.getButtonNumber(item),
        rtl = this.isRTL();

	// if dragging from the left edge, switch to the previous item and drag resize from its right
    var offsetDrag = false;
	if ((!rtl && EH.resizeEdge == "L") || (rtl && EH.resizeEdge == "R")) {
        offsetDrag = true;
        itemNum--;
		EH.resizeEdge = (rtl ? "L" : "R");
	} else if (EH.resizeEdge == "T") {
        offsetDrag = true;
        itemNum--;
		EH.resizeEdge = "B";
    }
	// if not in a valid item, forget it
	if (itemNum < 0 || itemNum >= this.members.length || item == null) return false;
    EH.dragTarget = item = this.members[itemNum];

    
    item._oldCanDrop = item.canDrop;
    item.canDrop = false;
	
    // NOTE: depended upon by observers (ListGrid)
	this._resizePosition = itemNum;
	
	if (item.showDown) item.setState(isc.StatefulCanvas.STATE_DOWN);
    if (offsetDrag) {
        var mouseDownItem = this.members[itemNum+1];
        if (mouseDownItem) mouseDownItem.setState(isc.StatefulCanvas.STATE_UP);
    }	
	return EH.STOP_BUBBLING;
},

// sent whem item moves during drag-resizing
dragResizeMemberMove : function () {
    var EH = this.ns.EH,
        item = EH.dragTarget;

    // resize the item 
	item.resizeToEvent();
    // do an immediate redraw for responsiveness 
    item.redrawIfDirty("dragResize"); 
	return EH.STOP_BUBBLING;
},

// sent whem item dragging for resizing ends
dragResizeMemberStop : function () {
	var EH = this.ns.EH,
		item = EH.dragTarget;

    // restore old canDrop setting
    item.canDrop = item._oldCanDrop;

    // change appearance back
	item.setState(isc.StatefulCanvas.STATE_UP);

    // resize
	item.resizeToEvent();
 
    // record the new size
    var newSize = (this.vertical ? item.getHeight() : item.getWidth());
    this.resizeItem(this._resizePosition, newSize);

    if (this.itemDragResized) this.itemDragResized(this._resizePosition, newSize); // for observers
	return EH.STOP_BUBBLING;
},

// resize an item programmatically
resizeItem : function (itemNum, newSize) {
    // resize the item
    var item = this.members[itemNum];
    if (this.vertical) item.setHeight(newSize);
    else item.setWidth(newSize);
}

});

isc.Toolbar.registerStringMethods({
// itemClick handler for when an item is clicked
// (JSDoc comment next to default implementation)
itemClick : "item,itemNum",

//> @method toolbar.itemDragResized
// Observable, overrideable method - called when one of the Toolbar buttons is drag resized.
//
// @param itemNum (number) the index of the item that was resized
// @param newSize (number) the new size of the item
//
// @visibility external
//<
itemDragResized : "itemNum,newSize",

// Sent when an item is drag reordered.  This can be observed to have a related widget
// rearrange itself.
itemDragReordered : "itemNum,newPosition"
});


