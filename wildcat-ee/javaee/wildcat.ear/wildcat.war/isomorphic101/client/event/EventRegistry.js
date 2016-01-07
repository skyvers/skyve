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
//>	@object	EventRegistry
//
//	EventRegistry -- the event registry allows you to set global event handlers
//		that fire BEFORE the normal event processing fires.  This lets you
//		ensure that certain actions will happen when you want them to.
//
//	You define events by calling Page.setEvent, eg:
//
//		Page.setEvent("eventName","action", fireStyle)
//
//<

//
//	add properties to the Page object
//
isc.Page.addClassProperties(
{	
    //>	@classAttr	isc.Page._eventRegistry		(array : [] : IRWA)
	//			Registry for global events registered with Page event registry
	//		@group	EventRegistry
	//		@see	Page.setEvent()
	//<
    _eventRegistry : {},				

	//>	@classAttr	isc.Page._pageEventID		(number : 0 : IRWA)
	//			ID number for global events registered with Page event registry
	//		@group	EventRegistry
	//		@see	Page.setEvent()
	//<
	_pageEventID : 0,

	//>	@type	FireStyle
	// Flags to set automatic removal of events from the page event registry.
	//	@value	null                 Call the registered handler any time the event occurs
	//	@value	isc.Page.FIRE_ONCE   Call the registered handler the first time the event
    //                               occurs, then unregister the handler as though
    //                               +link{Page.clearEvent()} had been called
	// @group EventRegistry
	// @see Page.setEvent()
    // @visibility external
	//<
	FIRE_ONCE : "once",

    //>	@classAttr	isc.Page._keyRegistry		(array : [] : IRWA)
	//			Registry for keyboard events registered with Page key registry
	//		@group	KeyRegistry
	//		@see	Page.registerKey()
	//<
	_keyRegistry : {},

    
    _modifierKeysRegistry: new Array(32),
    _modifierKeysRegistryCount: 0,

    
    _OR: 0,
    _AND: 1
});


//
//	add methods for the 
//
isc.Page.addClassMethods({

//>	@classMethod	Page.setEvent()
// Register to be called whenever a given type of event occurs, at the page level.
// <p>
// This includes events that also occur on widgets (like "click") and events that only occur at
// the page level ("resize" and "load").
// <p>
// For events that also occur on widgets, page level event registrations will fire BEFORE the
// event handlers on Canvases.   If your action returns <code>false</code>, this will prevent
// the event from getting to the intended Canvas.
// <p>
// Capturing events on widgets can be done by setting one of the event methods available on Canvas
// (and hence available to all widget classes).  See +link{group:widgetEvents}.
//
//		@group	EventRegistry
//
//		@param	eventType (pageEvent)    	event type to register for ("mouseDown", "load", etc)
//		@param	action	(string)			string to be eval'd when event fires
//						(function)			function to be executed when event fires
//                      (object)            an object to call on which a method named "page" +
//                                          eventType will be called
//		@param	[fireStyle](FireStyle)	Flag to set automatic removal of the event after
//												it fires one or more times
//      @param  [functionName] (string)     optional - if an object was passed in as the second
//                                          parameter, this is a name of a method to call on that
//                                          object.
//		
//		@return			(number)	ID number of this event, may be used to remove the event later
//										via a call to <code>Page.clearEvent()</code>
// @see class:EventHandler
// @see classMethod:EventHandler.getX()
// @see classMethod:EventHandler.getY()
// @visibility external
//<
setEvent : function (eventType, action, fireStyle, functionName) {
	// make sure the action is a function
	if (isc.isA.String(action)) {
        
        if (eventType == isc.EH.LOAD || eventType == isc.EH.IDLE ||
            eventType == isc.EH.RESIZE || eventType == isc.EH.ORIENTATION_CHANGE) 
        {
            action = isc._makeFunction("target,eventInfo", action);
        } else {
            action = isc.Func.expressionToFunction("target,eventInfo", action);
        }
    }
    

    //>DEBUG
    if (this.logIsDebugEnabled()) {
        this.logDebug("setEvent("+eventType+"): action => " + 
                     (isc.isA.Function(action) ? isc.Func.getShortBody(action) : action));
                     //(eventType == "load" ? "\r\n" + Page.getStackTrace() : "")); 
    }
    //<DEBUG 

	var ID = isc.Page._pageEventID++,		// id of this event
		handler = {					// create the handler object that we'll save
			action : action,
            functionName : functionName,
			fireStyle : fireStyle,
			ID : ID
		};
	
	// make sure there's a slot for this eventType
    var registry = this._eventRegistry;
	if (!isc.isAn.Array(registry[eventType])) registry[eventType] = [];

	// add the handler
	registry[eventType].add(handler);

	// if this is the "idle" event, start the idle timer if necessary
	if (eventType == isc.EH.IDLE) {
//		this.logWarn("scheduling idle event " + action);
		isc.EventHandler.startIdleTimer();
	}

	// return the ID of this event
	return ID;
},



//>	@classMethod	Page.clearEvent()
//	Clear event(s) under the given eventType.<p>
//	To clear all events, omit the ID parameter.  To clear a specific event,
//	pass the ID that was returned by Page.setEvent().
//		@group	EventRegistry
//
//		@param	eventType	(PageEvent, Event) event type to clear
//		@param	[ID]		(number)	ID of the event to clear. 
//										If not specified, all events in eventType will be cleared.
// @see class:EventHandler
// @visibility external
//<
_$ID:"ID",
clearEvent : function (eventType, ID) {
    

    var registry = this._eventRegistry;
	if (ID == null) {
        registry[eventType] = [];
	} else {
        // If we're currently processing this event type, don't modify the length of the array
        // Clear the entry and allow the processing function to clear out the empty slots when
        // it completes
        if (this._processingEvent == eventType) {
            var reg = registry[eventType],
                index = isc.isA.Array(reg) ? reg.findIndex(this._$ID, ID) : -1;
            if (index != -1) reg[index] = null;

        // Otherwise just clear out the appropriate entry.
        } else {
            if (isc.isA.Array(registry[eventType])) {
                registry[eventType].removeWhere(this._$ID, ID);
            }
        }
	}
},

// Helper method to avoid reassembling 'pageClick' et all each time the event is fired
_getPageEventName : function (eventType) {
    var eventMap = this._pageEventMap = this._pageEventMap || {};
    if (!eventMap[eventType]) {
        eventMap[eventType] = 
                "page" + eventType.charAt(0).toUpperCase() + eventType.substring(1);
    }
    return eventMap[eventType];
},

//>	@classMethod	Page.handleEvent()	(A)
//	Handle an event by firing all events in the EventRegistry under a given eventType.
//	Called automatically by the isc.EventHandler in the normal course of handling events.
//		@group	EventRegistry
//
//		@param	target		(object)	Canvas or DOM object that received the event
//		@param	eventType	(string) 	name of this event
//		@param	eventInfo	(any)		information passed with a custom event (see e.g. Slider)
//
//		@return			(boolean)	false == cancel further event processing
//									anything else == continue processing
//<
handleEvent : function (target, eventType, eventInfo) {
    if (eventType == isc.EH.UNLOAD) isc.Canvas._handleUnload();

    // Check whether the set of modifier keys being held down has changed on every event.
    this._handleModifierKeysChanged();

	// get the list of handlers
	var list = isc.Page._eventRegistry[eventType];

	// if the list is empty, bail
	if (!isc.isAn.Array(list) || list.length == 0) return true;

    var pageEventName = this._getPageEventName(eventType);

	// execute each handler for this eventType in turn, as long as they don't return false
	var keepGoing = true;
	//	if any return false, return false to cancel event processing

    
    this._processingEvent = eventType;
    
	for (var i = 0, length = list.length; keepGoing && (i < length); i++) {
		var item = list[i];
        // Note: this array may be sparse - just skip empty entries
		if (!item) continue;

		// if an item is set to only fire once, remove it from the list.
        // NOTE: we want to do this immediately, that way if there's an error during processing of
        // the event, at least it will only happen once!
		if (item.fireStyle == isc.Page.FIRE_ONCE) list[i] = null;

        //>DEBUG
        if (this.logIsDebugEnabled()) {
            this.logDebug("handleEvent(" + eventType + "): firing action => " +
                                    isc.Func.getShortBody(item.action));
        }
        //<DEBUG 
    
        // fire the action
        if (isc.isA.Function(item.action)) {
            // function / expression style
		    keepGoing = (item.action(target,eventInfo) != false);
        } else {
            // object style: item.action is an Object (eg a Canvas), which should have either
            // "page"[eventName] invoked on it, or a custom function specified at registration
            // time and stored as item.functionName
            var object = item.action;

            if (!object || object.destroyed) {
                // if the item has been destroyed, remove the registration and continue
                list[i] = null;
                continue;
            }

            var functionName = item.functionName || pageEventName;
            if (isc.isA.Function(object[functionName])) {
                keepGoing = (object[functionName](target,eventInfo) != false);
            }           
        }
	}
    this._processingEvent = null;

	// collapse the list of handlers to get rid of any that have been cleared
    // (including those set to fire once).
    
	this._eventRegistry[eventType].removeEmpty();
		
	// return whether or not other event handlers should be fired
	return keepGoing;
},

//>	@classMethod	Page.actionsArePendingForEvent()	(A)
//		Return whether any actions are currently pending for a specific event.
//		@group	EventRegistry
//
//		@param	eventType	(string) 	name of this event
//
//		@return			(boolean)	true == at least one event is pending
//									false == no events pending
//<
actionsArePendingForEvent : function (eventType) {
	return (isc.isAn.Array(this._eventRegistry[eventType]) && this._eventRegistry[eventType].length != 0);
},



//
//	KeyRegistry -- global eventType for keyboard events
//



//>	@classMethod	Page.registerKey()
// Fire some action when the Page receives a keyPress event from a certain key.<br>
// Note that if a widget has keyboard focus, this action will fire only after any widget-level
// keyPress handlers have fired and bubbled the event up to the top of their ancestor chain.<br>
// Multiple actions can be registered to fire on a single keyPress using this method, and can
// be associated with different <code>target</code> objects (which will then be available as
// a parameter when the action is fired).
// <smartclient>
// <br>
// This differs from calling +link{Page.setEvent()} with the <code>"keyPress"</code>
// events registered via <code>setEvent()</code> will fire <i>before</i> widget level handlers 
// respond to the event, and will fire for every <code>keyPress</code> event, not just those
// triggered by some specific key or key-combination.
// </smartclient>
// 
// 
// @group	KeyRegistry
//		@param	key		(KeyIdentifier) key name or identifier object.
//		@param	action	(string)		Action to fire when key is pressed.
//              This can be a string of script to evaluate or a javascript function.<br>
//              This action will be passed 2 parameters: The name of the key pressed will be 
//              available as the first parameter or <code>key</code> keyword. The target 
//              passed into this method will be available as the second parameter or 
//             <code>target</code> keyword.
//      @param  [target]    (any)   If specified this object will be made available to the
//                                  action fired as a parameter.
// @see Canvas.keyPress()
// @see Page.setEvent()
// @see Page.unregisterKey()
// @visibility external
//<

registerKey : function (key, action, target) {

    if (key == null || action == null) return;

    // If passed an object for key, get keyName from it!
    var keyName = key;
    if (isc.isAn.Object(key)) {
        keyName = key.keyName;
    }

    // allow passing either "a" or "A".  Note toUpperCase() will simply no-op on numbers and
    // punctuation.
    if (keyName.length == 1) keyName = keyName.toUpperCase();

    // if we don't recognize the keyName, log a warning and bail
    // A definitive list of keyNames is in the '_virtualKeyMap' on EventHandler
    var isKeyName = false;
    for (var i in isc.EH._virtualKeyMap) {
        if (isc.EH._virtualKeyMap[i] == keyName) {
            isKeyName = true;
            break;
        }
    }

    if (!isKeyName) {
        this.logWarn(
            "Page.registerKey() passed unrecognized key name '" + key +"'. Not registering",
            "events"
        );
        return;
    }
    

    var keyRegistry = this._keyRegistry;
	// create an array under that key if necessary
	if (!keyRegistry[keyName]) keyRegistry[keyName] = [];

	// add the item to the key registry
	keyRegistry[keyName].add({target:target, action:action, key:key});
},

//>	@classMethod	Page.unregisterKey()
// Clears an action registered to fire on a specific a keyPress event via the +link{Page.registerKey()}
// method. 
//		@group	KeyRegistry
//		@see	Page.registerKey()
//
//		@param	actionID (KeyName) Name of key to clear registry entries for.
//		@param	[target] (object) target specified when the action was registered for the key.
//
// @visibility external
//<
unregisterKey : function (key, target) {

    var keyName = key;

	// if the registry item under that key doesn't exist, bail
	if (!this._keyRegistry[keyName]) {
        isc.Log.logInfo("Page.unregisterKey(): No events registered for key " + isc.Log.echo(keyName) + ".", "events");
        return false;
    }
	// remove the item
	this._keyRegistry[keyName].removeWhere("target", target)
},


//>	@classMethod	Page.handleKeyPress()	(A)
//			Handle a key press by firing messages to all listeners of that key 
//			registered with the Key Registry.
//		@group	KeyRegistry
//
//		@param	event	(DOM event) DOM event object (as passed by isc.EventHandler)
//		@return			(boolean)	false == stop further event processing	
//
//<
handleKeyPress : function () {
    // Get the name for the key
    var EH = isc.EH,
        keyName = EH.getKey(),
        keyRegistry = this._keyRegistry;

    //this.logInfo("keyName is " + keyName + 
    //             ", handlers are registered for: " + getKeys(Page._keyRegistry));

	// no one has registered an action for this key
    if (keyRegistry[keyName]) {
    
    
        // get the list of actions from the registry
        
        var actionsInReg = keyRegistry[keyName],
            actions = actionsInReg.duplicate(),
            length = actions.length,
            returnVal = true;

        // Pick up each action to fire from the registry

        for (var i = 0; i < length; i++) {
            var item = actions[i];
            // The item may have been unregistered by another item's action.
            // If so skip it.
            if (!actionsInReg.contains(item)) continue;

            if (!EH._matchesKeyIdentifier(item.key)) continue;

            // CALLBACK API:  available variables:  "key,target"
            // Convert a string callback to a function
            if (item.action != null && !isc.isA.Function(item.action)) {
                isc.Func.replaceWithMethod(item, "action", "key,target");
            }
            returnVal = ((item.action(keyName, item.target) != false) && returnVal);
        }
    }

    // if suppressBackspaceNavigation is true, return false here
    // Note that this will only fire if the event bubbled to the top of the widget
    // chain.
        
    if (returnVal != false &&
        isc.Page.suppressBackspaceNavigation && 
        keyName == this._$Backspace) 
    {
        returnVal = false;
    }
    	
	return returnVal;
},
_$Backspace:"Backspace",


//> @classAttr Page.suppressBackspaceNavigation (boolean : true : IRWA)
// By default most modern browsers will navigate back one page when the user hits the
// backspace key.
// <P>
// Setting this attribute to true will suppress this native behavior. Alternatively, 
// developers can explicitly return <code>false</code> from the keyPress event
// (either via event handlers applied to specific widgets, or via +link{Page.registerKey}
// for example) to suppress the native navigation behavior.
// @visibility external
//<
suppressBackspaceNavigation:true,


_registerModifierKeys : function (keys, logic, downAction, upAction, target) {
    if (keys == null || (downAction == null && upAction == null)) return;
    
    var registry = this._modifierKeysRegistry,
        index = this._getModifierKeysRegistryIndex(keys, logic);
    if (!registry[index]) registry[index] = [];
    registry[index].add({ target: target, downAction: downAction, upAction: upAction });
    ++this._modifierKeysRegistryCount;
},
_getModifierKeysRegistryIndex : function (keys, logic) {
    
    var flags = isc.Page._getModifierKeysFlags(keys);
    return ((logic == isc.Page._AND ? 0x1 : 0) | (flags << 1));
},
_getModifierKeysFlags : function (keys) {
    var ctrlKey = false, shiftKey = false, altKey = false, metaKey = false;
    if (isc.isAn.Array(keys)) {
        ctrlKey = keys.contains("Ctrl");
        shiftKey = keys.contains("Shift");
        altKey = keys.contains("Alt");
        metaKey = keys.contains("Meta");
    } else if (isc.isAn.Object(keys)) {
        ctrlKey = keys.ctrlKey;
        shiftKey = keys.shiftKey;
        altKey = keys.altKey;
        metaKey = keys.metaKey;
    } else if (keys == "Ctrl") {
        ctrlKey = true;
    } else if (keys == "Shift") {
        shiftKey = true;
    } else if (keys == "Alt") {
        altKey = true;
    } else if (keys == "Meta") {
        metaKey = true;
    }
    return (
        (ctrlKey ? 0x1 : 0) |
        (shiftKey ? 0x2 : 0) |
        (altKey ? 0x4 : 0) |
        (metaKey ? 0x8 : 0));
},

_unregisterModifierKeys : function (keys, logic, target) {
    
    var registry = this._modifierKeysRegistry,
        index = this._getModifierKeysRegistryIndex(keys, logic),
        items = registry[index];
    if (items) {
        var prevLength = items.length;
        items.removeWhere("target", target);
        this._modifierKeysRegistryCount -= (prevLength - items.length);
    }
},

_currentModifierFlags: 0,

_modifierKeysDown : function (keys) {
    var currentFlags = this._currentModifierFlags,
        flags = this._getModifierKeysFlags(keys);
    return (flags == (currentFlags & flags));
},

_handleModifierKeysChanged : function () {
    if (this._modifierKeysRegistryCount == 0) {
        return;
    }
    
    var registry = this._modifierKeysRegistry,
        prevFlags = this._currentModifierFlags,
        // This must match _getModifierKeysFlags() above:
        flags = (
            (isc.EH.ctrlKeyDown() ? 0x1 : 0) |
            (isc.EH.shiftKeyDown() ? 0x2 : 0) |
            (isc.EH.altKeyDown() ? 0x4 : 0) |
            (isc.EH.metaKeyDown() ? 0x8 : 0));

    if (flags != prevFlags) {
        for (var i = 0; i < 32; ++i) {
            if (registry[i] != null) {
                
                var f = (i >> 1),
                    prevDown = false, down = false;
                if (i < 16) {
                    prevDown = ((prevFlags & f) != 0);
                    down = ((flags & f) != 0);
                } else {
                    prevDown = ((prevFlags & f) == f);
                    down = ((flags & f) == f);
                }

                var prevUp = !prevDown,
                    up = !down,
                    wentDown = (prevUp && down),
                    wentUp = (prevDown && up),
                    actionProp = (wentDown ? "downAction" : (wentUp ? "upAction" : null));

                if (actionProp != null) {
                    var items = registry[i];
                    for (var j = 0, length = items.length; j < length; ++j) {
                        var item = items[j],
                            action = item[actionProp];
                        if (action != null) {
                            action(item.target);
                        }
                    }
                }
            }
        }
    }

    this._currentModifierFlags = flags;
    
}

});	// END isc.Page.addMethods

