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

 

//>	@class	EventHandler
//
// <smartclient>
// The ISC system provides a predictable cross-browser event-handling mechanism for ISC
// widgets. Events can be handled both at the page level (i.e., globally), and at the level of
// individual widgets.
// <p>
// With the exception of a few page-specific events ('load', 'unload', 'idle' and 'resize'),
// events are processed in the following sequence:
// <p>
// 1. The event is sent to any global (page-level) event handlers. These handlers can cancel
// further propagation of the event by returning false.  You can register to listen for any of the
// events linked in the seeAlso section (below) by calling +link{classMethod:Page.setEvent()}
// method.
// <p>
// 2. If the event occurred on a form element or a link, it is passed on to the browser so that
// the element will perform its default action. No widget receives the event.
// <p>
// 3. If the event occurred on an enabled widget (but not on a form element or link inside
// the widget), it is sent to that widget's event handler, if any. This handler can cancel
// further propagation of the event by returning false.  An "enabled" widget is any widget that
// defines an event handler for one of the supported events.  Interceptable events are defined in
// the +link{class:Canvas#methods#events, "widgetEvents" section of Canvas}.
// <p>
// 4. The event is "bubbled" up to the widget's parent in the containment hierarchy, if any.
// Again, the parent's handler for the event can cancel further propagation by returning
// false. This step is repeated, with the event "bubbling" up through the containment
// hierarchy, until a top-level widget is reached or the event is explicitly canceled.
// In brief, the ISC event model offers the best features of browser event models:
// <ul>
// <li> Page-first event handling allows you to reliably process or cancel any event before it
//      affects the objects on the page.
// <li> Event "bubbling" ensures that parent widgets receive events sent to their children,
//      and allows you to create generalized parent-level handlers rather than duplicating
//      code in each child.
// </ul>
// Note: Canceling propagation of an event may cancel its side effects as well, including the
// generation of other (synthetic) events. For example, if a global mouseDown handler returns
// false, drag-and-drop events will not be generated. Specific effects are discussed in the
// descriptions of the various events in the following sections.
// <p>
// SmartClient libraries will not interfere with native event handling when events occur
// outside of a target widget. You can therefore have HTML that is not ISC-based on the same
// page as widget objects that will react to native events as you would expect.
// <p>
// You can use isc.Event as an alias for isc.EventHandler.
// </smartclient>
// <smartgwt>
// SmartGWT provides a predictable cross-browser event-handling mechanism for SmartGWT
// widgets, and the EventHandler class provides various static APIs for retrieving details
// about events which have occurred.
// <P>
// Events may be intercepted at the page level using standard GWT methods such as
// <i>addNativePreviewHandler()</i> on the 
// <code>com.google.gwt.user.client.Event</code> class.
// <P>
// If the event occurred on an enabled widget it is sent to that widget's event handler,
// if any. This handler can cancel
// further propagation of the event by returning false.  An "enabled" widget is any widget that
// defines an event handler for one of the supported events.
// <p>
// The event is "bubbled" up to the widget's parent in the containment hierarchy, if any.
// Again, the parent's handler for the event can cancel further propagation by calling
// <code>event.cancel()</code>. This step is repeated, with the event "bubbling" up
//  through the containment hierarchy, until a top-level widget is reached or the event is
//  explicitly canceled.
// <P> 
// Where applicable, canceling an event will also typically suppress the native behavior
// (for example canceling a keypress while focused in a form item may prevent the
// key character being inserted in the text box).
// <p>
// SmartGWT libraries will not interfere with native event handling when events occur
// outside of a target widget. You can therefore have HTML that is not ISC-based on the same
// page as widget objects that will react to native events as you would expect.
// <smartgwt>
// 
// @see type:PageEvent    
// @see classMethod:Page.setEvent()
// @see classMethod:Page.clearEvent()
// @see class:Canvas#methods#widgetEvents
//
// @treeLocation   Client Reference/System
// @visibility external
//<




// create the isc.EventHandler object
isc.ClassFactory.defineClass("EventHandler");
// nicknames - isc.Event is publicly documented as an alias
isc.EH = isc.Event = isc.EventHandler;
// add class properties and constants
isc.EventHandler.addClassProperties(
{ 	
    //>	@classAttr	isc.EventHandler.lastEvent		(object : {} : IRWA)
	//		Last event that was processed by our event system.  We store the properties
	//		 of the event in a separate object so we can access them uniformly on both
	//		 platforms and so we can remember characteristics of the last event we've
	//		 seen even when we're not in the script context of this event.<br><br>
	//		
	//		To access properties of the last event, use:<ul>
	//			<li>isc.EventHandler.getLastEvent()
	//			<li>isc.EventHandler.getX()
	//			<li>isc.EventHandler.getY()
	//			<li>isc.EventHandler.getScreenX()
	//			<li>isc.EventHandler.getScreenY()
	//			<li>isc.EventHandler.getButtonNum()
	//			<li>isc.EventHandler.leftButtonDown()
	//			<li>isc.EventHandler.rightButtonDown()
	//			<li>isc.EventHandler.getKey()
    //          <li>isc.EventHandler.getKeyEventCharacterValue()
    //          <li>isc.EventHandler.getKeyEventCharacter()
	//			<li>isc.EventHandler.shiftKeyDown()
	//			<li>isc.EventHandler.ctrlKeyDown()
	//			<li>isc.EventHandler.altKeyDown()
	//			<li>isc.EventHandler.metaKeyDown()
	//			<li>isc.EventHandler.modifierKeyDown()
    //      </ul>
	//
	//		@group events
	//		@see	isc.EventHandler.getMouseEventProperties()
    //      @see    isc.EventHandler.getKeyEventProperties()
    //  @visibility eventhandler
	//<
	lastEvent : {},						
	
    //>	@classAttr	isc.EventHandler._dropRegistry		(array : [] : IRWA)
	//		Registry of canvases that are interested in receiving drop events.
	//		@group dragdrop
	//		@see	isc.EventHandler.eventHandledNatively()
    //  @visibility internal
	//<
    _dropRegistry : [],			        

	//>	@classAttr	isc.EventHandler._maskRegistry		(array : [] : IA)
	//		Registry of canvases that have their own _eventMask peers to block mouse
	//		events. Implemented so that canvases with contentsURL (iframe contents)
	//		won't swallow events during drag & drop. When a drag operation begins,
	//		we show the masks for all canvases in this registry.
    // @visibility internal
	//<
	_maskRegistry : [],
	
	//>	@classAttr	isc.EventHandler.passThroughEvents		(boolean : true : IRWA)
	//			if true, we pass events to anchors and form elements automatically.
	//			false == trap these events
	//		@see	isc.EventHandler.eventHandledNatively()
    // @visibility internal
	//<
	passThroughEvents:true,

	//>	@classAttr	isc.EventHandler.maskNativeEvents (boolean : true : IRWA)
	//      Whether the clickmask should mask events on non ISC-elements as well.  
    // @visibility internal
	//<
    maskNativeTargets:true,
						
	//>	@classAttr	isc.EventHandler.STILL_DOWN_DELAY (integer : 100 : IRWA)
    // amount of time between mouseStillDown messages (msec)
    // @visibility external
    //<
	STILL_DOWN_DELAY 	: 100,
	
	//=	@classAttr	isc.EventHandler.DOUBLE_CLICK_DELAY	amount of time (in milliseconds) between doubleClicks (integer: 500: IRWA)
    
	DOUBLE_CLICK_DELAY  : 500,
    //> @classAttr isc.EventHandler.IDLE_DELAY (integer : 10 : IRWA)
    // amount of time between idle messages (msec)
    // @visibility external
    //<
	IDLE_DELAY 			: 10,

	//> @classAttr isc.EventHandler.STOP_BUBBLING (string : "***STOP***" : IRA)
	// Return this constant from a child event to stop the event propagating to its parent,
	// without suppressing any native browser handling associated with the event.
	// Developers should not need to modify this value - it should be treated as read-only
	// in most circumstances.
	// @visibility external
	//<
	STOP_BUBBLING : "***STOP***",		


	//>	@classAttr	isc.EventHandler.ALL_EDGES	(Array of string : ["T","L","B","R","TL","TR","BL","BR"] : IR)
	// Constant containing the full set of edges a component may be resized from.
	// When a component is marked as canDragResize, this will be the default set of edges
	// from which it may be resized.
	// @visibility external
	//<
	ALL_EDGES : ["T","L","B","R","TL","TR","BL","BR"],

    eventTypes :	{	
        // Events are documented on Canvas as methods since that's how we expect users to register
        // them on widgets.  Some are also documented here for reference by Page.setEvent()

        //> @type PageEvent
        //
        // Events registerable via +link{classMethod:Page.setEvent()}
        //
        // 
        // @value "idle" 
        //        Fires repeatedly (every 10 ms by default) when the system is idle (i.e.,
        //        not busy running other scripts) after the page is loaded.
        // @value "load" 
        //        Fires when the page has finished loading. It corresponds to the
        //        browser 'load' event normally handled by window.onload.
        // @value "unload" 
        //        Fires when the page is exited or unloaded. It corresponds to the
        //        browser 'unload' event normally handled by window.onunload.
        // @value "resize" 
        //        Fires when the browser window is resized by the user. It corresponds
        //        to the browser 'resize' event normally handled by window.onresize.    
        //
        //
        // @value "mouseDown"
        //        Fires when the left mouse button is pressed on the Page.
        // @value "rightMouseDown"
        //        Fires when the right mouse button is pressed on the Page.
        // @value "mouseMove"
        //        Fires when the mouse moves on the Page.
        // @value "mouseUp"
        //        Fires when the left mouse button released on the Page.
        // @value "click"
        //        Fires when the user clicks the mouse on the Page.
        // @value "doubleClick"
        //        Fires when the uesr double-clicks on the Page.
        //
        // @value "showContextMenu"
        //        Fires when the right mouse button is clicked on the page.  If your event handler
        //        for this event returns false, the native browser context menu will be suppressed.<br>
        //        Note: On the Macintosh platform, <code>Command+Click</code> may be used instead
        //        of right-button click to trigger a context menu event.<br>
        //        On the Opera browser, <code>Ctrl+Shift+Click</code> should be used instead of
        //        right-button click.
        //
        // @value "keyPress" Fires when a user presses a key on the keyboard. 
        //
        // @value "orientationChange" Fires when the +link{Page.getOrientation()} changes due
        //        to browser-window resize or rotation of a mobile device.
        //
        // see classMethod:Page.setEvent()
        // see classMethod:Page.clearEvent()
        // @visibility external
        //<
        
    	MOUSE_DOWN : "mouseDown",
    	RIGHT_MOUSE_DOWN : "rightMouseDown",
    	MOUSE_MOVE : "mouseMove",			
    	MOUSE_UP : "mouseUp",				    
    	SHOW_CONTEXT_MENU : "showContextMenu",	

        CLICK : "click",		
    	DOUBLE_CLICK : "doubleClick",		
    	// the following mouse events are not available on Page 
    	MOUSE_OUT : "mouseOut",				
    	MOUSE_STILL_DOWN : "mouseStillDown",
    	MOUSE_OVER : "mouseOver",

        POINTER_DOWN: "pointerDown",
        POINTER_MOVE: "pointerMove",
        POINTER_UP: "pointerUp",
        POINTER_CANCEL: "pointerCancel",

        //>Touch
    	TOUCH_START: "touchStart",	
    	TOUCH_MOVE: "touchMove",		
    	TOUCH_END: "touchEnd",
    	TOUCH_CANCEL: "touchCancel",
    	LONG_TOUCH:"longTouch",
        //<Touch

        // XXX classify
    	SET_DRAG_TRACKER : "setDragTracker",
        GET_DRAG_DATA : "getDragData",      
        RELEASE_DRAG_DATA : "releaseDragData", 
        

        DRAG_START:"dragStart",
        DRAG_STOP:"dragStop",  
    	DRAG_MOVE : "dragMove",
    	DRAG_OUT : "dragOut",	
        DRAG_REPOSITION_START : "dragRepositionStart",
        DRAG_REPOSITION_MOVE : "dragRepositionMove",
        DRAG_REPOSITION_STOP : "dragRepositionStop",
        DRAG_RESIZE_START : "dragResizeStart",
        DRAG_RESIZE_MOVE : "dragResizeMove",
        DRAG_RESIZE_STOP : "dragResizeStop",

    	DROP_OVER : "dropOver",
    	DROP_MOVE : "dropMove",
    	DROP_OUT : "dropOut",
    	DROP : "drop",			

        TRANSITION_END : "transitionEnd",

    	KEY_DOWN : "keyDown",	
    	KEY_UP : "keyUp",		
    	KEY_PRESS : "keyPress",	
        MOUSE_WHEEL : "mouseWheel",
    	
    	SELECT_START : "selectStart",
        SELECTION_CHANGE : "selectionChange",
        FOCUS_IN : "focusIn",
        FOCUS_OUT : "focusOut",

    	IDLE : "idle",				
    	LOAD : "load",						
    	UNLOAD : "unload",					
    	RESIZE : "resize",
    	ORIENTATION_CHANGE : "orientationChange"
    },

    // Map used by getMouseEventProperties to convert from native mouse event names to 
    // canonicalized versions (available as constants on the EH class).
    _nativeMouseEventMap: {
        // By default all browsers give lowercase event names for the following events*
        //  *Verified on IE6, Windows; Moz 1.73, Windows; Safari 2, Mac; Moz 1.6, Unix;
        mousemove:"mouseMove",
        mousedown:"mouseDown",
        mouseup:"mouseUp",
        click:"click",

        contextmenu:"contextMenu",

        // Proprietary to IE6
        mousewheel:"mouseWheel",
        selectionchange:"selectionChange",
        // Proprietary to Moz
        DOMMouseScroll:"mouseWheel",


        // Also handle being passed an already canonicalized version - may happen if the 
        // event passed to getMouseEventProperrties was an ISC event rather than a native event
        mouseMove:"mouseMove",
        mouseDown:"mouseDown",
        mouseUp:"mouseUp",
        mouseWheel:"mouseWheel",

        pointerdown: "pointerDown",
        pointermove: "pointerMove",
        pointerup: "pointerUp",
        pointercancel: "pointerCancel",

        //>Touch
        touchstart:"touchStart",
        touchmove:"touchMove",
        touchend:"touchEnd",
        touchcancel:"touchCancel",
        touchStart:"touchStart",
        touchMove:"touchMove",
        touchEnd:"touchEnd",
        touchCancel:"touchCancel",
        //<Touch

        dragstart:"dragStart",
        drag:"drag",
        dragenter:"dragEnter",
        dragexit:"dragExit",
        dragleave:"dragLeave",
        dragover:"dragOver",
        drop:"drop",
        dragend:"dragEnd",

        selectionstart:"selectionStart",
        selectionStart:"selectionStart",
        selectionchange:"selectionChange",
        selectionChange:"selectionChange"
    },

    // mini state machine state used for firing synthetic mouseDown/mouseUp on Android
    _touchEventStatus : {
        READY_FOR_TOUCH: "ready",
        TOUCH_STARTED: "started",
        TOUCH_ENDING: "ending",
        TOUCH_COMPLETE: "complete"
    },

    
    _eventHandlerArgString:"event,eventInfo",   

    
	//>	@type	DragOperation
	// Builtin types of drag and drop interactions
	//		@group	dragdrop
    //	@value	isc.EventHandler.DRAG_RESIZE			Resizing by dragging
	DRAG_RESIZE : "dragResize",			
    //	@value	isc.EventHandler.DRAG_REPOSITION		Repositioning by dragging
	DRAG_REPOSITION : "dragReposition",	
    //	@value	isc.EventHandler.DRAG_SCROLL            Scroll/pan by drag
    DRAG_SCROLL : "dragScroll",
	// @value isc.EventHandler.DRAG_SELECT  Select content via drag
	DRAG_SELECT : "dragSelect",
	
    //	@value	isc.EventHandler.DRAG			General drag (custom implementation)
	DRAG : "drag",
	//<										

    //>	@type	DragAppearance
    // 		Different types of effects for showing dragging behavior.
    //	@group	dragdrop
	//	@visibility external
    //
    //	@value	"none"
    //      No default drag appearance is indicated. Your custom dragging routines should
    //      implement some behavior that indicates that the user is in a dragging situation,
    //      and where the mouse is.
	NONE : "none",
    //	@value	"tracker"
    //      A "drag tracker" object is automatically shown and moved around with the
    //      mouse. This is generally set to an icon that represents what is being dragged. The
    //      default tracker is a 10 pixel black square, but you can customize this icon. This
    //      dragAppearance is not recommended for use with drag resizing or drag moving.
	TRACKER : "tracker",
    //	@value	"target"
    //      The target object is actually moved, resized, etc. in real time. This is
    //      recommended for drag repositioning, but not for drag resizing of complex objects.
	TARGET : "target",
    //	@value	"outline"
    //      An outline the size of the target object is moved, resized, etc. with the
    //      mouse. This is recommended for drag resizing, especially for objects that take a
    //      significant amount of time to draw.
	OUTLINE : "outline",
    //<	

    //>	@type DragIntersectStyle
    // Different styles of determining intersection: with mouse or entire rect of target
    //
    // @value "mouse" Look for drop targets that are under the current mouse cursor position.
	INTERSECT_WITH_MOUSE : "mouse",
    // @value "rect" Look for drop targets by intersection of the entire rect of the drag
    //               target with the droppable target.
	INTERSECT_WITH_RECT : "rect",
    // @group dragdrop
	// @visibility external
    //<


    artificialDragTargetDefaults: {
        _constructor: "Canvas",
        canDrag: true,
        useNativeDrag: true,
        canDrop: true,
        dragAppearance: "none",

        cloneDragData : function () {
            return isc.EH.getNativeDragData();
        }
    },


    //>	@classAttr  dragTargetShadowDepth   (number : 10 : IRWA);
    //  If we are showing a shadow for some widget on drag, how deep should the shadow be.
    //  @group  dragdrop
    //<
    dragTargetShadowDepth : 10,

    
    dragOffsetX: 0,
    dragOffsetY: 0,

	_anchorTags : {
        A : true,
        AREA : true
    },
	_formTags : {
        INPUT : true,
        TEXTAREA : true,
        SELECT : true,
        OPTION : true
    },
    _labelString : "LABEL",

    // native event name to camelCase name
    _nativeKeyEventMap : {
        keydown:"keyDown",
        keyup:"keyUp",
        keypress:"keyPress",
        
        contextmenu:"contextMenu"
    },

    // In IE we don't get native keyPress events on non-character keys.
    // for these keys we generate synthetic keyPress events in handleKeyDown.
    // We use this map of non-character keys to determine which keys are not going to produce
    // a keyPress event natively in IE.
    
    _nonCharacterKeyMap : {
        Backspace:8,
        Tab:9,
        Shift:16,
        Ctrl:17,
        Alt:18,
        Pause_Break:19,
        Caps_Lock:20,
        // Note - escape seems to be a special case - it gives us no character value, but will fire 
        // a keyPress event natively
//        Escape:27,
        Page_Up:33,
        Page_Down:34,
        End:35,
        Home:36,
        Arrow_Left:37,
        Arrow_Up:38,
        Arrow_Right:39,
        Arrow_Down:40,
        Insert:45,
        Delete:46,
        Meta:91,
        //Meta:92,  
        f1:112,
        f2:113,
        f3:114,
        f4:115,
        f5:116,
        f6:117,
        f7:118,
        f8:119,
        f9:120,
        f10:121,
        f11:122,
        f12:123,        
        Num_Lock:144,
        Scroll_Lock:145
    },

    // In IE, certain alpha keys don't generate a normal keyPress event if the ctrl key
    // is down at the same time.  The map _modifierAppliedKeyMap has bindings for known
    // keys with this behavior.
    
    _modifierAppliedKeyMap : {
        A: true,
        C: true,
        D: true,
        R: true,
        V: true
    },

    // In Safari / Chrome we get no native keyPress events for character keys as well.
    // However the list is slightly different from in IE - it includes Escape.
    
    _safariNonCharacterKeyMap : {
        Backspace:true,
        Tab:true,
        Shift:true,
        Ctrl:true,
        Alt:true,
        Pause_Break:true,
        Caps_Lock:true,
        Escape:true,
        Page_Up:true,
        Page_Down:true,
        End:true,
        Home:true,
        Arrow_Left:true,
        Arrow_Up:true,
        Arrow_Right:true,
        Arrow_Down:true,
        Insert:true,
        Delete:true,
        Meta:true,
        Menu:true,
          
        f1:true,
        f2:true,
        f3:true,
        f4:true,
        f5:true,
        f6:true,
        f7:true,
        f8:true,
        f9:true,
        f10:true,
        f11:true,
        f12:true,
        // Untested 
        Num_Lock:true,
        Scroll_Lock:true
    },

    // Virtual key map
    // virtual key code mappings for every key on the keyboard.
    // Note: While each code maps to a separate key, we're normalizing to a key name - IE
    // we're not going to differentiate between Enter on the keyboard and Enter on the numeric 
    // key pad (we can't in some cases on some browsers anyway, and this functionality would be
    // confusing if you weren't expecting it)
    

    // JSDoc the developer friendly keynames - these are required for accessKeys, registering
    // page level key events, etc.
    //> @type KeyName
    //
    // Strings to identify the various keys on the keyboard.
    // <ul>
    // <li>  For alpha keys, the single (uppercase) character value is used, such as "Q"
    // <li>  For Numeric keys, the number is used as in a single character string, like "1"
    // <li>  Function keys are identified as <code>"f1"</code> - <code>"f12"</code>
    // <li>  Non alpha-numeric character keys (such as the key for "[" / "{") are identified by
    //       their unmodified character value (the value obtained by hitting the key without 
    //       holding shift down), by default - exceptions are listed below.
    // <li>  Additional key names:<br>
    //      - <code>Space</code><br>    
    //      - <code>Tab</code><br>    
    //      - <code>Enter</code><br>
    //      - <code>Escape</code><br>
    //      - <code>Backspace</code><br>
    //      - <code>Insert</code><br>
    //      - <code>Delete</code><br>
    //      - <code>Arrow_Up</code><br>
    //      - <code>Arrow_Down</code><br>
    //      - <code>Arrow_Left</code><br>
    //      - <code>Arrow_Right</code><br>            
    //      - <code>Home</code><br>
    //      - <code>End</code><br>
    //      - <code>Page_Up</code><br>    
    //      - <code>Page_Down</code><br>   
    //      - <code>Shift</code><br>
    //      - <code>Ctrl</code><br>
    //      - <code>Alt</code>
    // </ul>
    // [Note: Some keys may not be available for capture on every platform]
    // @visibility external
    //<
    // Avoid doc'ing keys we may not be able to capture, or which are likely to vary based on 
    // OEM keyboard layout, etc.
    //      - <code>Print_Screen</code><br>
    //      - <code>Scroll_Lock</code><br>
    //      - <code>Caps_Lock</code><br>
    //      - <code>Pause_Break</code><br>
    //      - <code>Num_Lock</code><br>
    //      - <code>Menu</code><br>
    
    // Key Identifiers differ from keyNames in that we include modifier detection as a
    // boolean - used in a few places in the code
    //> @type KeyIdentifier
    // Identifiers for keys pressed by the user used by various methods.<br>
    // Valid <code>keyIdentifier</code>s can be either +link{KeyName} strings, or objects.<br>
    // If a <code>keyIdentifier</code> is specified as an object, it should have the following
    // properties:<br>
    // - <code>keyName</code>: name of the key<br>
    // - <code>ctrlKey</code>: optional boolean - true if ctrl is down.<br>
    // - <code>shiftKey</code>: optional boolean - true if shift is down.<br>
    // - <code>altKey</code>: optional boolean - true if alt is down.
    // @see type:KeyName
    // @visibility external
    //<
    _virtualKeyMap : {
        // Note - have to quote numeric property names for older browsers
        '0':'_undefined',
                        // 1  Left mouse button 
                        // 2  Right mouse button 
                        // 3  Control-break processing 
                        // 4  Middle mouse button (three-button mouse) 
        '8':'Backspace',
        '9':'Tab',
        
                        //  '12':'Clear',   
        '13':'Enter',

        '16':'Shift',
        '17':'Ctrl',
        '18':'Alt',
        '19':'Pause_Break',
        '20':'Caps_Lock',
                        // 21-25  Reserved for Kanji systems 

        '27':'Escape',
                        // 28-31  Reserved for Kanji systems 
        
        
        '32':'Space',
        '33':'Page_Up',
        '34':'Page_Down',
        '35':'End',
        '36':'Home',
        '37':'Arrow_Left',
        '38':'Arrow_Up',
        '39':'Arrow_Right',
        '40':'Arrow_Down',
                        // 41  SELECT key 
                        // 42  [Win32: "Original equipment manufacturer (OEM) specific"] 
                        // 43  EXECUTE key 
        '44':'Print_Screen',    // 44 PRINT SCREEN key for Win 3.0 and later 
        '45':'Insert',
        '46':'Delete',
                        // 47  HELP key 
        // Note: these are above the main keyboard (not on the numeric keypad)                        
        '48':'0', "49":"1", "50":"2", "51":"3", "52":"4", 
        "53":"5", "54":"6", "55":"7", "56":"8", "57":"9",
        
        // 58-64  Undefined 
        // Exception: These are used on mousedown / mouseup for certain keys normally in
        // the 187-222 range on mac osx / moz
        
                        
        '58':';', // observed on shift+";" key (Mac osx, Moz)
        '59':';', // observed on ";" (mac osx, moz)
        '60':',', // observed on shift+ "," key (mac osx, moz)
        '61':'=', // mac osx, moz
        '62':"/", // observed on shift+ "/" key mac osx, moz
        

        // Standard Char keys                        
        '65':'A', '66':'B', '67':'C', '68':'D', '69':'E', '70':'F', 
        '71':'G', '72':'H', '73':'I', '74':'J', '75':'K', '76':'L', 
        '77':'M', '78':'N', '79':'O', '80':'P', '81':'Q', '82':'R',
        '83':'S', '84':'T', '85':'U', '86':'V', '87':'W', '88':'X',
        '89':'Y', '90':'Z',

        '91':'Meta',    // Meta Left
        '92':'Meta',    // Meta Right

        // 93  Application key [Win32: "Undefined"] 
        //      - from observation, this is returned from the 'context menu' key (next to the right
        //        meta key on windows 2k, IE and Moz, US keyboard) 
        '93':'Menu',
        
        //  94-95 Undefined 

        // keys on the numeric keypad                        
        '96':'0', '97':'1', '98':'2', '99':'3', '100':'4',
        '101':'5', '102':'6', '103':'7', '104':'8', '105':'9',

        '106':'*',  //  The Multiply key 
        '107':'+',  //  Add key  (on the keypad - not "=+")
                        // 108  Separator key 
        '109':'-',  //  Minus key
        '110':'.',  //  Decimal key
        '111':'/',  //  Divide key 

        '112':'f1', '113':'f2', '114':'f3', '115':'f4', '116':'f5', '117':'f6', 
        '118':'f7', '119':'f8', '120':'f9', '121':'f10', '122':'f11', '123':'f12',        
                        // 124-143  Unassigned 

        '144':'Num_Lock',
        '145':'Scroll_Lock',    // OEM specific - true on Windows

                        // 146-159  Unassigned 

        '160':'Shift',  //  Left SHIFT key [Win32: "Unassigned"] 
        '161':'Shift',  //  Right SHIFT key [Win32: "Unassigned"] 
        
        '162':'Ctrl',   //  Left CTRL key [Win32: "Unassigned"] 
        '163':'Ctrl',   //  Right CTRL key [Win32: "Unassigned"] 
        
        '164':'Alt',    //  Left ALT key [Win32: "Unassigned"] 
        '165':'Alt',    //  Right ALT key [Win32: "Unassigned"] 
        
                        //  166-185  Unassigned 
                        //  186-192  OEM specific   * See below

                        //  193-218  Unassigned 
                        //  219-228  OEM specific 
                        //  229  Precedes extended key [Win32: "Unassigned"] 
                        //  230  OEM specific 
                        //  231-232  Unassigned 
                        //  233-245  OEM specific 

        // xxx
        // There is no guarantee for the punctuation keys.  They will vary by locale and 
        // platform.
        // We can't ask for the keyboard mapping, and but let's handle the MS Windows US keyboard 
        // layout by default.
        // NOTE: this is one reason to make use of Ascii keycodes when we have them.
        
        '186':';',      //  VK_OEM_1 0xBA ";:" for US 
        '187':'=',      // '+',  VK_OEM_PLUS 0xBB "+" any country 
        '188':',',      //  VK_OEM_COMMA 0xBC "," any country 
        '189':'-',      //  VK_OEM_MINUS 0xBD "-" any country 
        '190':'.',      //  VK_OEM_PERIOD 0xBE "." any country 
        '191':'/',      //  VK_OEM_2 0xBF "/?" for US 
        '192':'`',      //  VK_OEM_3 0xC0 "`~" for US 
        
        
        '219':'[',      //  VK_OEM_4 0xDB "[{" for US 
        '220':'\\',     //  VK_OEM_5 0xDC "\|" for US 
        '221':']',      //  VK_OEM_6 0xDD "]}" for US 
        '222':"'"       //  VK_OEM_7 0xDE "'"" for US 

        
        ,'224':"Meta"

                        //  VK_OEM_AX 0xE1 AX key on Japanese AX keyboard 
                        //  VK_OEM_102 0xE2 "<>" or "\|" on RT 102-key keyboard 
                        
    },

 
    
    _charsetValueToKeyNameMap : {

        // Don't worry about any control characters that aren't directly mapped to a key
        // on the keyboard
        '8':'Backspace',
        '9':'Tab',
        '13':'Enter',
        '27':'Escape',

        // Normalize the character to the key name
        // Note: This is occasionaly ambiguous - such as when hitting "*", it could be the
        // * above the 8, or it could be the * on the keypad
        // Note: Choosing somewhat arbitrary names for the keys - just make sure this stays
        // constant.
        '32':'Space',   //' '
        '33':'1',   //'!'
        '34':"'",   //'"'
        '35':'3',   //'#',
        '36':'4',   //'$',
        '37':'5',   //'%',
        '38':'7',   //'&',
        
        '39':"'",
        '40':'9',   //'(',
        '41':'0',   //')',
        
        '42':'8',   //'*',  // May be wrong if on the keypad
        '43':'=',   //'+'   // May be wrong if on the keypad 
        
        '44':',',    '45':'-',     '46':'.',    '47':'/',
    
        '48':'0', '49':'1', '50':'2', '51':'3', '52':'4', 
        '53':'5', '54':'6', '55':'7', '56':'8', '57':'9',
        
        '58':';',    //':',
        '59':';',
    
        '60':',',   //'<',
        '61':'=',
        '62':'.',   //'>',
        '63':'/',   //'?',
    
        '64':'2',   //'@',  // an example of US-Only mapping
    
        '65':'A', '66':'B', '67':'C', '68':'D', '69':'E', '70':'F', '71':'G',
        '72':'H', '73':'I', '74':'J', '75':'K', '76':'L', '77':'M', '78':'N',
        '79':'O', '80':'P', '81':'Q', '82':'R', '83':'S', '84':'T', '85':'U',
        '86':'V', '87':'W', '88':'X', '89':'Y', '90':'Z',
        
        '91':'[',   '92':'\\',   '93':']',
        
        '94':'6',   //'^',
        '95':'-',   //'_',
        '96':'`',
    
        '97':'A',   //'a',
        '98':'B',   //'b',
        '99':'C',   //'c',
        '100':'D',  //'d',
        '101':'E',  //'e',
        '102':'F',  //'f',
        '103':'G',  //'g',
        '104':'H',  //'h',
        '105':'I',  //'i',
        '106':'J',  //'j',
        '107':'K',  //'k',
        '108':'L',  //'l',
        '109':'M',  //'m',
        '110':'N',  //'n',
        '111':'O',  //'o',
        '112':'P',  //'p',
        '113':'Q',  //'q',
        '114':'R',  //'r',
        '115':'S',  //'s',
        '116':'T',  //'t',
        '117':'U',  //'u',
        '118':'V',  //'v',
        '119':'W',  //'w',
        '120':'X',  //'x',
        '121':'Y',  //'y',
        '122':'Z',  //'z',
    
        '123':'[',  //'{',
        '124':'\\',  //'|',
        '125':']',  //'}',
        '126':'`'  //'~'           
        // Beyond this they are a bunch of special characters we should not need to worry about
        
    },

    
    _safariSpecialKeyPressMap : {
        '3':"Enter",
        
        '25':"Tab", // This happens with shift+tab

        '63232':"Arrow_Up",
        '63233':"Arrow_Down",
        '63234':"Arrow_Left",
        '63235':"Arrow_Right",

        // Note f8/f9/f10 don't fire an event - cos they take OS focus from the browser
        '64236':"f1", '64237':"f2", '64238':"f3", '64239':"f4", '64240':"f5", '64241':"f6", 
        '64242':"f7", '64243':"f8", '64244':"f9", '64245':"f10", '64246':"f11", '63247':"f12",

        '63273':"Home",
        '63275':"End",
        '63276':"Page_Up",
        '63277':"Page_Down"
    },
    
    
    // _eventHandlerMap - mapping between normal event names and names for internal handler 
    //  functions, eg "mouseDown" -> "handleMouseDown"
    // - Retrieve using "_getInternalHandlerName(event)"
    // - generated on the fly for any event name
    // (See comments in bubbleEvent())
    _eventHandlerMap : {
    },
    
	
    

    //>PluginBridges
    // if there are backMask-requring elements on the page, should we show/hide the backMask on the
    // dragMoveTarget on intersect or just show it all the time?
    //
    // Running intersection tests on every dragMove is expensive, so this is disabled.  Note
    // however, that these intersection tests are required to force correct repainting of
    // Applets obscured by drag and drop in realtime.  But that only happens on older JDKs and
    // can greatly slow down the browser.  See notes in Applet.repaintOnDragStop() for more info.
    dynamicBackMask: false,

    // don't bother to compute what may require backmasking, just always show the backMask.
    // Also useful if the page contains items that require backmasking but that SmartClient
    // doesn't know about - e.g custom iframes.
    alwaysBackMask: false,
    //<PluginBridges

    //>	@classAttr	isc.EventHandler.dragTrackerDefaults (object literal : _lookup_ : IA)
	//		Default properties for the drag tracker.
    // @visibility internal
	//<
    
	dragTrackerDefaults : { 							
			ID:"isc_dragTracker",
            _isDragTracker:true,
			width: 10,									
			height: 10,
            offsetX: -10,
            offsetY: -10,
            autoDraw: false,
            visibility:"hidden",
			overflow:"visible",
			cursor:"arrow"
	}
}
);// END isc.EventHandler.addClassProperties()

// Add each of the event types in isc.EventHandler.eventTypes directly to the event handler object
isc.EventHandler.addClassProperties(isc.EventHandler.eventTypes);

 
isc.EventHandler.addClassMethods({


handleSyntheticEvent : function (event) {

    var target = event.target;
    event._isSynthetic = true;
    //this.logWarn("synthetic event: " + isc.echoAll(event));

    if (target) {
        event.clientX += target.getPageLeft();
        event.clientY += target.getPageTop(); 
    
        
        if(isc.Browser.isIE) {
            /*
            this.logWarn("left margin: " +  target.getLeftMargin()
                         + " border: " + target.getLeftBorderSize()
                         + " padding: " + target.getLeftPadding());
            */
            event.clientX += target.getLeftMargin() + target.getLeftBorderSize()
                             + target.getLeftPadding() + 2;
            event.clientY += target.getTopMargin() + target.getRightBorderSize() 
                             + target.getTopPadding() + 2;
        }
        /*
        this.logWarn("synthetic event on: " + target.getID() 
                     + " ("+event.type+": "+event.clientX+", "+event.clientY+")");
        */

        switch (event.type) {
            case "mouseup": this.handleMouseUp(event); break;
            case "mousedown": this.handleMouseDown(event); break;
            case "mousemove": this.handleMouseMove(event); break;
        }
    }
},

//>	@classMethod	isc.EventHandler.handleEvent()  (A)
// Routine to handle generic events that are not handled specially 
//	 (currently everything but keyPress, mouseDown, mouseUp, mouseStillDown, mouseMove).
//
//		@group	eventHandling
//
//		@param	target		(object)	Canvas or DOM object that received the event
//		@param	eventType	(string) 	name of this event
//		@param	eventInfo	(any)		information passed with a custom event (see e.g. Slider)
//
//		@return			(boolean)	false == cancel further event processing
//									anything else == continue processing
//      @visibility eventhandler
//<

handleEvent : function (target, eventType, eventInfo) {
    
    this._handlingEvent = eventType;
	
    
        var EH = isc.EH;
    
	// process the event globally
	var returnVal;
  	if (isc.Page.handleEvent(target, eventType, eventInfo) == false) {
  	    returnVal = false;
  	
    // if the target is enabled
	// 	bubble the event up the target's chain
	} else if (EH.targetIsEnabled(target) && EH.bubbleEvent(target, eventType, eventInfo)==false) {
		returnVal =false;
	} else {
	    returnVal = true;
	}
	
	delete this._handlingEvent;
	
	return returnVal;
},


// Handler for the page-level load event.  Internal - developers use Page.setEvent() instead
handleLoad : function (DOMevent) {
    // ensure that SA_Page onload fires before Page's onload handlers.  The History module in
    // particular registers onload handlers that must fire before the Page onload, but because
    // IE's attachEvent() mechanism doesn't fire events in order of registration, we need to
    // kick SA_Page here.
    if (isc.SA_Page) isc.SA_Page._firePageLoadCallbacks();

    if (!isc.Browser.isMoz) {
        
        if (isc.EH._useEventListenerForUnload() && document && document.body) {
            document.body.addEventListener("unload", isc.EH.handleUnload, false);
        }
    }
    
    if (isc.Log.supportsOnError) {
    	return (isc.Page.handleEvent(null, isc.EH.LOAD) != false);
    } else {
        try {
    	    return (isc.Page.handleEvent(null, isc.EH.LOAD) != false);
        } catch (e) {

            isc.Log._reportJSError(e);
            throw e;; // extra semi for Safari
        }
    }
},


// Handler for the page-level unload event.  Internal - developers use Page.setEvent() instead
handleUnload : function (DOMevent) {
	
        var EH = isc.EH;

    var result = (isc.Page.handleEvent(null, EH.UNLOAD) != false);
    
    
    if (result == true) {
        this.releaseEvents();
    }
	return result;
},

//> @groupDef keyboardEvents
// SmartClient allows keyboard events to be captured at the page level via 
// +link{isc.Page.registerKey()}
// <smartclient> or +link{Page.setEvent()} </smartclient>
// or at the widget level
// via +link{canvas.keyDown()}, +link{canvas.keyPress()}, and +link{canvas.keyUp}.
// <P>
// Details about the key events can be retrieved via static methods on the EventHandler class
// including +link{isc.EventHandler.getKey()}, +link{isc.EventHandler.getKeyEventCharacter()} and
// +link{isc.EventHandler.getKeyEventCharacterValue()}.
// <P>
// As with other SmartClient event handling code, returning <code>false</code> will suppress the
// default native browser behavior.<br>
// <b>Note:</b> browsers do not allow cancellation of some keys' default behaviors.
// These cases vary by browser, and wherever native cancellation is supported, returning false
// from your event handler should be sufficient to suppress the behavior.
// <br>
// Some specific cases where default behavior cancellation is not always possible include:
// <ul><li>Some function keys (<code>f1, f3, f5,</code> etc) which trigger native browser behavior.
//         [These can be suppressed in Internet Explorer and Mozilla Firefox but not in some other
//          browsers such as Safari / Chrome, etc]</li>
//     <li>Some accelerator key combos such as <code>Alt+f3</code></li>
//     <li>The "Meta" key (the <code>Windows</code> / <code>Apple</code> key to show OS level menu)
//     </li>
// </ul>
// If you do want to include functionality for these keys in your application, we'd recommend 
// testing against your expected users' browser types. It is also worth considering whether by
// changing the functionality of these standard browser keys you may provide an unexpected 
// user experience (for example a user may press "f5" in an attempt to reload the application
// and be surprised by this triggering some alternative functionality in your application).
//
// @treeLocation Concepts
// @visibility external
// @title Keyboard Events
//<





// NOTE: naming: 
// - handleNativeKey*: directly called by DOM
// - handleKey*: called on EH object

// called directly by DOM
_$f10:"f10",
_$Escape:"Escape",
_keyDownKeyNames:[],
_syntheticKeypressFired:{},
_handleNativeKeyDown : function (DOMevent, fromOnHelp) {
	// Some browsers (like Mac IE) have problems dealing with events fired before the page finishes loading.
	//	Just skip key event processing if the page hasn't loaded yet.
    //!DONTCOMBINE
	if (!isc.Page.isLoaded()) return false;
 
    
        var EH = isc.EH;

    
    

    var lastEvent = EH.lastEvent;
    
    if (!DOMevent) DOMevent = EH.getWindow().event;

    // Get the details of the event (stored in EH.lastEvent)
    EH.getKeyEventProperties(DOMevent);

    if (isc.Browser.isIE && lastEvent.keyName == this._$f1 && !fromOnHelp) {
        
        return;
    }
    
    

    var returnVal = true;

    
    var keyName = lastEvent.keyName,
        charValue = lastEvent.characterValue;
        
    //isc.logWarn("In native keydown handler, lastEvent.keyName is " + keyName + 
    //            ", lastEvent.characterValue is " + charValue);
        
    var keyDownKeys = EH._keyDownKeyNames.duplicate();
    for (var i = 0; i < keyDownKeys.length; i++) {
        var prevKeyName = keyDownKeys[i];
        if (prevKeyName == null || prevKeyName == keyName) break;
        
        // skip this if it's a key where we always just fire a synthetic keypress on keyDown
        // since we will have already handled it.
        if (this._fireKeypressOnKeyDown(prevKeyName)) continue;
        
        
        
        
        lastEvent.characterValue = null;
        // copy the previous keyName onto the event so handleKeyPress gets the correct args
        lastEvent.keyName = prevKeyName;
        
        // Fire handleKeyPress with no arguments - this'll fire event handlers based on the
        // specified keyName, and remove it from keyDownKeyNames
        
        this.handleKeyPress();
        EH._syntheticKeypressFired[lastEvent.keyName] = true;
    }
    // We may have changed this to a previously-down keyName in the above loop. reset to the
    // current key.
    lastEvent.keyName = keyName;
    lastEvent.characterValue = charValue;
    
    // the above caught keyDown on another key.. this handles repeated keyDown events for the
    // same key, which should basically fire repeated keypress events.
    var fireSyntheticKeyPress = this._fireKeypressOnKeyDown(keyName);

    if (EH._keyDownKeyNames.indexOf(lastEvent.keyName) != -1) {
        // We can skip this logic if we know we'll fire keyPress (below).
        if (!fireSyntheticKeyPress) {
            returnVal = EH.handleKeyPress();
            EH._syntheticKeypressFired[lastEvent.keyName] = true;
            
            
        }
    } else {
        returnVal = EH.handleKeyDown(DOMevent);
    }

    
    if (returnVal != false && lastEvent.keyName) {
	    EH._keyDownKeyNames[EH._keyDownKeyNames.length] = lastEvent.keyName;
    }
    
    EH._ctrlKeyOnLastUpDown = lastEvent.ctrlKey;
    EH._altKeyOnLastUpDown = lastEvent.altKey;
            
    
    if (returnVal != false && fireSyntheticKeyPress) {
        returnVal = EH.handleKeyPress(DOMevent);
        EH._syntheticKeypressFired[lastEvent.keyName] = true;
    }
    if (returnVal == false) {
        this.cancelKeyEvent(DOMevent);
    }
    return returnVal;
},

// Browser Specific _fireKeypressOnKeyDown() versions:
// for cases where we know we'll have to fire a synthetic keyPress on keyDown:

// default
_fireKeypressOnKeyDown : function (keyName) { return false; },

// Mozilla
_mozFireKeypressOnKeyDown : function (keyName) { 
    return keyName == this._$f10 && this.shiftKeyDown();
},

// IE
_ieFireKeypressOnKeyDown : function (keyName) { 
    var EH = isc.EH;
    if (EH._nonCharacterKeyMap[keyName] != null) return true;
    // Testing in IE6+ shows that hardly any Ctrl- or Alt- combinations fire a keyPress event, 
    // and those that do, do not suppress native behavior if the custom event code returns 
    // false.  So in IE, we always want synthetic keyPress events for Ctrl- and Alt- 
    // combinations, and the old code base around the "_modifierAppliedKeyMap" map of keys 
    // that need special handling is obsolete and removed
    return EH._modifierKeyDownOnly();
},

// Safari / Chrome non character key
_safariFireKeypressOnKeyDown : function (keyName) { 
    var EH = isc.EH;
    if (EH._safariNonCharacterKeyMap[keyName] != null) return true;
    return EH._modifierAppliedKeyMap[keyName] != null && EH._modifierKeyDownOnly();
},

// internal routine used for deciding whether to generate synthetic keypress
_modifierKeyDownOnly : function (event) {
    if (!event) event = this.lastEvent;
    if (isc.Browser.isMac) return event.metaKey && !event.altKey && !event.shiftKey;
    // Return true if either the Ctrl key or Alt Key are down (but not both)
    else return (event.ctrlKey && !event.metaKey && !event.altKey && !event.shiftKey) ||
                (event.altKey && !event.metaKey && !event.ctrlKey && !event.shiftKey);
},

// handleKeyDown() fires the keyDown handler on the event target.
// called in response to document.keyDown _handleNativeKeyDown().

_$Tab:"Tab",
handleKeyDown : function (nativeEvent, scEventProperties) {

    var EH = isc.EH,
        lastEvent = EH.lastEvent,
        returnVal;
    // If the event is handled natively, we will avoid doing any of our own processing and
    // return true to allow native processing
    
    var handledNatively = EH.eventHandledNatively(lastEvent.eventType, lastEvent.nativeKeyTarget);
    if (handledNatively) returnVal = EH._handledNativelyReturnVal;

    if (scEventProperties != null) isc.addProperties(lastEvent, scEventProperties);
    if (!handledNatively) {    
    
        var eventInfo = [lastEvent, lastEvent.target, lastEvent.keyName];
            
        var target = lastEvent.keyTarget;
        //EH.logWarn("nativeEvent: " + EH.echoDOM(nativeEvent) +
        //           ", nativeTarget: " + EH.echoLeaf(lastEvent.nativeTarget));
        //EH.logWarn("lastEvent.target (before re-calling getEventTargetCanvas()):" + target); 
        if (target == null) target = this.getEventTargetCanvas(nativeEvent,
                                                               lastEvent.nativeKeyTarget);
        if (EH.targetIsEnabled(target)) {
            returnVal = (EH.bubbleEvent(target, EH.KEY_DOWN, eventInfo) != false);
        }
    }
    
    return returnVal;
},

// called by DOM
_handleNativeKeyUp : function (DOMevent) {
	// Some browsers (like Mac IE) have problems dealing with events fired before the page finishes loading.
	//	Just skip key event processing if the page hasn't loaded yet.
    //!DONTCOMBINE
	if (!isc.Page.isLoaded()) return false;
    
    var EH = isc.EH,
        lastEvent = EH.lastEvent;
    
    if (!DOMevent) DOMevent = EH.getWindow().event;        
    // get key event properties (stored in EH.lastEvent)
    EH.getKeyEventProperties(DOMevent);
    
    
    EH._ctrlKeyOnLastUpDown = lastEvent.ctrlKey
    EH._altKeyOnLastUpDown = lastEvent.altKey

    
    EH._syntheticKeypressFired[lastEvent.keyName] = null;
        
    
    if (EH._keyDownKeyNames.indexOf(EH.lastEvent.keyName) != -1) {
        if (EH.handleKeyPress(DOMevent) == false) {
            this.cancelKeyEvent(DOMevent);
            return false;
        }
    }
    
    var returnVal = EH.handleKeyUp(DOMevent)
    
    return returnVal
},

// called on EH
handleKeyUp : function (nativeEvent, scEventProperties) {
    var EH = isc.EH,
        lastEvent = EH.lastEvent,
        eventInfo = [lastEvent,  lastEvent.target, lastEvent.keyName];
        
    // If the event is handled natively, return true to allow native processing
    if (EH.eventHandledNatively(lastEvent.eventType, lastEvent.nativeKeyTarget)) {
        // Log.logWarn("keyup event handled natively - bailing");
        return EH._handledNativelyReturnVal;
    }
        

    var returnVal = true;

    if (scEventProperties != null) {
        isc.addProperties(lastEvent, scEventProperties);
    }
    
    var target = lastEvent.keyTarget;
    if (target == null) target = this.getEventTargetCanvas(nativeEvent,
                                                           lastEvent.nativeKeyTarget);

    if (EH.targetIsEnabled(target))
        returnVal = (EH.bubbleEvent(target, EH.KEY_UP, eventInfo) != false);

    
    // On Shift+f10 to show a SmartClient context menu, return false to cancel the keyUp event.
    if (!isc.Browser.isMac && lastEvent.keyName == EH._$f10 && EH.shiftKeyDown() && 	
        isc.Menu && isc.Menu._openMenus && isc.Menu._openMenus.length > 0) 
    {
        returnVal = false;
    }

    // Check whether the set of modifier keys being held down has changed on every event.
    isc.Page._handleModifierKeysChanged();

    // Clear EH.lastEvent's key properties
    EH.clearKeyEventProperties(lastEvent.keyName);

    // Return true to allow processing to continue unless something explicitly returned false.
    return returnVal;
},

// called by DOM
_handleNativeKeyPress : function (DOMevent) {
	// Some browsers (like Mac IE) have problems dealing with events fired before the page
    // finishes loading.  Just skip key event processing if the page hasn't loaded yet.
    //!DONTCOMBINE
	if (!isc.Page.isLoaded()) return false;

    
        var EH = isc.EH;

    // We already got the keyEventProperties stored in EH.lastEvent from the keyDown handler
    // However call to getKeyEventProperties required as keyCodes returned in IE are different
    // on keyDown and keyPress.
    
	var lastEvent = EH.lastEvent,
        eventType = EH.KEY_PRESS;

    if (!DOMevent) DOMevent = EH.getWindow().event;
            
    EH.getKeyEventProperties(DOMevent);
        
    
    lastEvent.eventType = eventType;
    // Respect ctrl / alt key state recorded on keyDown/keyUp
    
    lastEvent.ctrlKey = EH._ctrlKeyOnLastUpDown;
    lastEvent.altKey = EH._altKeyOnLastUpDown;
    
    //isc.logWarn("In native keyPress handler for key " + lastEvent.keyName + ". eventType is " + eventType + ", Ctrl is " + (lastEvent.ctrlKey ? "down" : "up"));
    
    
    if (EH._syntheticKeypressFired[lastEvent.keyName] == true) {
        
        // Remove the flag - if we get a second native keyPress event we do want to fire the
        // handler (implies the key is being held down!)
        EH._syntheticKeypressFired[lastEvent.keyName] = null;
        return;
    }
    
    var returnVal = EH.handleKeyPress(DOMevent);
    if (returnVal == false) {
        this.cancelKeyEvent(DOMevent);
    }
    return returnVal;

},

// cancelKeyEvent
// Fired when a key event handler returns false
// We use this to suppress native key event handling behavior where returning false from 
// the native event isn't sufficient.

_IECanSetKeyCode:{keydown:true,  keyup:true, keypress:true},
cancelKeyEvent : function (DOMevent) {
    
    
    if (isc.Browser.isIE || isc.Browser.isSafari) {
        
        if (this._IECanSetKeyCode[DOMevent.type] == true) {
            
            try {
                DOMevent.keyCode = 0;
            } catch (e) {
            }
        }
    }

},

// called on EH 
handleKeyPress : function (nativeEvent, scEventProperties) {
    // We already got the keyEventProperties stored in EH.lastEvent from the keyDown handler
    // However call to getKeyEventProperties required as keyCodes returned in IE are different
    // on keyDown and keyPress.
	var EH = isc.EH,
        lastEvent = EH.lastEvent,
        eventType = EH.KEY_PRESS;

    // if passed already-derived event properties, apply them to the EH.lastEvent.
    if (scEventProperties != null) {
        isc.addProperties(lastEvent, scEventProperties);
    } 
    
    
    var eventInfo = {keyName:lastEvent.keyName, characterValue:lastEvent.characterValue};
       
    // update the eventType since this may be a synthetically generated keyPress event (from
    // keyUp or repeated keyDown events).
    lastEvent.eventType = eventType;
    
    
    EH._keyDownKeyNames.removeAt(0);

	// call the global keyPress event (Set up via Page.setEvent("keyPress",...) )
	if (isc.Page.handleEvent(lastEvent.keyTarget, eventType) == false) return false;
    // If eventHandledNatively returns true don't fire widget level handlers, or allow 
    // registered keys to fire their actions.
	// NOTE: in IE, this will return the key number so we pass that value on
	var it = (EH.eventHandledNatively(eventType, lastEvent.nativeKeyTarget));
    
	if (it !== false) {
        //>DEBUG
        EH.logDebug("keyPress handled natively");
        //<DEBUG
        return EH._handledNativelyReturnVal;
        
    } else {
        //>DEBUG
        EH.logDebug("keyPress not handled natively");
        //<DEBUG
    }
    
    var target = lastEvent.keyTarget;
    if (target == null) target = this.getEventTargetCanvas(nativeEvent,
                                                           lastEvent.nativeKeyTarget);
    // Pass to the appropriate widget, and stop if this returns false.
    if (EH.targetIsEnabled(target)) {
        var handlerReturn = EH.bubbleEvent(target, lastEvent.eventType, eventInfo) 
        if (handlerReturn == false) return false;
    }

    
	if (handlerReturn != EH.STOP_BUBBLING 
        && isc.Page.handleKeyPress() == false) return false;
        

    // If we got a tab or shift-tab keypress, and we're showing a hard mask, explicitly stick
    // focus into the next widget in the page's tab order that isn't masked.
    
    if (this.clickMaskUp() && lastEvent.keyName == this._$Tab) {
        var topHardMask,
            registry = this.clickMaskRegistry;
        for (var i = registry.length-1; i >=0; i--) {
            if (this.isHardMask(registry[i])) {
                topHardMask = registry[i];
                break;
            }
        }
        if (topHardMask != null) {
            var focusCanvas = EH._focusCanvas;

            if (focusCanvas != null) {
            
                //>DEBUG
                this.logInfo("Telling focus canvas:" + focusCanvas + " to shift focus", 
                            "syntheticTabIndex")
                //<DEBUG
                focusCanvas._focusInNextTabElement(!this.shiftKeyDown(), topHardMask);
            } else {
                if (this.shiftKeyDown()) {
                    //>DEBUG
                    this.logInfo("Putting focus into last widget in response to Tab keydown",
                                "syntheticTabIndex")
                    //<DEBUG
                
                    this._focusInLastWidget(topHardMask);
                } else {
                    //>DEBUG
                    this.logInfo("Putting focus into first widget in response to Tab keydown",
                                "syntheticTabIndex")
                    //<DEBUG
                    this._focusInFirstWidget(topHardMask);
                }
            }
            // Always return false natively in this case - we don't want the focus to shift again
            return false;
        }
    }
    

    
    
    if (lastEvent.keyName == isc.EH._$f10 && isc.EH.shiftKeyDown()) {
        var returnVal = this.handleContextMenu(nativeEvent, true);
        
        if (isc.Browser.isIE && returnVal) {
            this._contextMenuShown = true;
        }
        return returnVal;
    }
    
    // In Moz hitting "Escape" during server turnaround (either XMLHttpRequest, or iframe)
    // kills the communication.
    // Avoid this by suppressing native Escape behavior during active comm.
    
    // Note: Opera has the same bug if using hidden frames (though not XMLHttpRequest)
    // but returning false from the Escape keypress doesn't resolve the issue in that browser.
    if (isc.Browser.isMoz &&
        isc.RPCManager && isc.RPCManager._activeTransactions.length > 0
        && lastEvent.keyName == isc.EH._$Escape) 
    {
        return false;
    }
    
	// return true to allow normal event processing unless anything explicitly returned false
	return true;
},
// Helper methods to put focus at the beginning or end of our managed tab-index.
_focusInFirstWidget : function (mask) {
    var widget = this._firstTabWidget;
    if (widget) {
        if ((!mask || !this.targetIsMasked(widget, mask)) && 
              widget.isDrawn() && widget.isVisible() && !widget.isDisabled() && 
              widget._canFocus()) 
        {
            // Call 'focusAtEnd()' rather than focus()
            // if the widget manages the tab index of sub elements (EG DynamicForm), we want
            // to notify it to put focus in the first sub element.
            widget.focusAtEnd(true);
        } else {
            widget._focusInNextTabElement(true, mask);
        }
    }
},
_focusInLastWidget : function (mask) {
    var widget = this._lastTabWidget;
 
    if (widget) {
        if ((!mask || !this.targetIsMasked(widget, mask)) && 
             widget.isDrawn() && widget.isVisible() && !widget.isDisabled() && 
             widget._canFocus()) 
        {
            widget.focusAtEnd();
        } else {
            widget._focusInNextTabElement(false, mask);
        }            
    }
},


 
//>	@classMethod	isc.EventHandler.handleMouseDown()
//			Special handler for mouseDown events.
//          Starts a timer to fire mouseStillDown if the target implements it.
//			sets the following variables for use in subsequent events
//				...
//
//		@group	mouseEvents
//		@param	DOMevent	(DOM event) DOM event object (as passed by isc.EventHandler)
//
//		@return				(boolean)	false == cancel native event processing
//										anything else == continue native event processing
//  @visibility internal
//<
handleMouseDown : function (DOMevent, syntheticEvent) {

    // In touch environments (iPhone etc), we respond to onTouchStart / onTouchStop events
    // and use these to fire our mouseDown/up/click events.
    // This is required to support drag/drop as the mouseDown/up series of events fires
    // after the touchStop event.
    // Simply no-op from the native mouseDown / Up etc event handlers so we don't get
    // doubled events and unpredictable behavior
    
    if ((isc.Browser.isTouch || navigator.pointerEnabled) && !syntheticEvent) return;

	
        var EH = isc.EH;

    EH._handlingMouseDown = true;
    var returnValue = EH.doHandleMouseDown(DOMevent, syntheticEvent);
    EH._handlingMouseDown = false;
    
    return returnValue;
},



_$IMG:"IMG",
_$alphaImgFilter:"progid:DXImageTransform.Microsoft.AlphaImageLoader",
doHandleMouseDown : function (DOMevent, syntheticEvent) {
	// Some browsers (like Mac IE) have problems dealing with events fired before the page finishes loading.
	//	Just skip mouse event processing if the page hasn't loaded yet.
	if (!isc.Page.isLoaded()) return false;

	var EH = this;

	// note that the mouse is down.  We do this BEFORE getting event properties to work around a bug
	// in Nav where the event.which (property to get the mouse button) is reported as 1 on a
	// mouseMove, even when the mouse is actually not pressed.
	EH._mouseIsDown = true;

    
    EH._firedSyntheticMouseUp = null;
    
	// get the properties of the event
    
    var event = syntheticEvent || EH.getMouseEventProperties(DOMevent);

    // if we switched event target, get rid of the focus
    
    var focusCanvas = EH._focusCanvas,
        forceBlur = focusCanvas != null &&
                    (focusCanvas != event.target) && !focusCanvas._useNativeTabIndex && 
                    !focusCanvas._useFocusProxy && 
                    !(isc.isA.DynamicForm!=null && isc.isA.DynamicForm(focusCanvas) && focusCanvas.getFocusSubItem() 
                      && focusCanvas.getFocusSubItem().hasFocus);
    if (forceBlur) {
        // In IE if we blur() here, the thing we clicked on never gets native focus (not clear 
        // why), so do this on a timeout, if appropriate
        if (isc.Browser.isIE) {
            var ID = EH._focusCanvas.getID();
            
            if (EH._delayedBlurArray == null) 
                EH._delayedBlurArray = ["if (", ID, " && ", ID, ".hasFocus)", ID, ".blur();"]
            else 
                EH._delayedBlurArray[1] = EH._delayedBlurArray[3] = EH._delayedBlurArray[5] = ID;
            isc.Timer.setTimeout(
                EH._delayedBlurArray.join(isc.emptyString),
                0
            );
            
        } else {
            EH._focusCanvas.blur();
        }
    }        

    
	// remember a copy of the event particulars as mouseDownEvent
	EH.mouseDownEvent = isc.addProperties({}, event);

	// get the target Canvas
	var target = event.target;

    if (isc.Browser.isAndroid && isc.Browser.isChrome && isc.isA.Canvas(target) && target.isDrawn()) {
        var topLevelCanvas = target.getTopLevelCanvas();
        topLevelCanvas.getClipHandle().dataset.iscMouseDownEventTimeStamp = String(event.DOMevent.timeStamp);
    }

    // handle mouseDown on the virtual click mask, if it's showing.  
    // If this is an hard clickMask, the click action will be cancelled by the
    // mask - since we actually do this on mouseDown, we need to set a flag to also
    // cancel mouseUp when it happens.
    
    
    var targetIsMasked = (EH.clickMaskClick(target) == false);
	if (targetIsMasked) {
        EH.__cancelNextMouseUp = true;
        
        // In Chrome, if the user clicks the scrollbar track/thumb of the page, if we
        // we return false from mouseDown, native scrolling is disallowed.
        // Catch this case and avoid killing the event
        if (isc.Browser.isChrome) {
            var sbWidth =  isc.Element.getNativeScrollbarSize();
            var pageHeight = isc.Page.getHeight(),
                pageWidth = isc.Page.getWidth(),
                hasHSB = isc.Page.getScrollWidth() > isc.Page.getWidth(),
                hasVSB = isc.Page.getScrollHeight() > isc.Page.getHeight();
            
            // Note that getWidth() reports size *inside* scrollbars
            // so we don't need to look at isc.Element.getNativeScrollbarSize() here
            // getHeight() reports size including space under scrollbars so we do need
            // to adjust there.
            if (hasHSB && isc.EH.getY() > (pageHeight - sbWidth) + isc.Page.getScrollTop()) {
                return true;
            }
            if (hasVSB && isc.EH.getX() > pageWidth + isc.Page.getScrollLeft()) {
                return true;
            }
        }
        return false;
    } else {
        // explicitly set the flag to NOT cancel the next mouseUp - this is required to note that 
        // we've fired the clickMaskClick()
        // See comments in handleMouseUp() for more details
        EH.__cancelNextMouseUp = false;
    }

    // NOTE: although we do send a rightMouseDown, we don't send a rightMouseMove or rightMouseUp at
    // the moment.  rightMouseDown is needed to implement a record or cell selection model that
    // matches Windows Explorer and Excel respectively - rightMouseMove and rightMouseUp are more
    // exotic.
    var eventType = EH.rightButtonDown() ? EH.RIGHT_MOUSE_DOWN : EH.MOUSE_DOWN;
	// call the global mouseDown handler
	if (isc.Page.handleEvent(target, eventType) == false) {
        return false;
    }

	// see if we shouldn't pass this event on to DOM object, and return true if we should
	if (EH.eventHandledNatively(eventType, event.nativeTarget)) 
        return EH._handledNativelyReturnVal;

	// if the target is not enabled, we shouldn't continue
    if (!EH.targetIsEnabled(target)) return false;
    
    var forceIEFocusTarget;
    
    if (target && !target.hasFocus) {
        // call 'focus' to focus on the widget.
    
        
        
        if ( ((isc.Browser.isMoz && target.canSelectText) || isc.Browser.isSafari )
             && target._useFocusProxy )
        {
            EH.focusInCanvas(target);
            
        
        } else if (!target._useNativeTabIndex) {
            target.focus("focus on mousedown");
        
        } else if (isc.Browser.isMoz || isc.Browser.isSafari) {
            target.focus("focus on mousedown");
        
        
        } else if (isc.Browser.isIE) {
            
            var nativeElement = event.nativeTarget;
            if (isc.Browser.isStrict) {
                forceIEFocusTarget = target;
            } else {
                if (nativeElement && nativeElement.tagName == this._$IMG) {
                    var style = nativeElement.style,
                        filter = style ? style.filter : null;
                    if (filter.contains(this._$alphaImgFilter)) {
                        forceIEFocusTarget = target;
                    }
                }
            }
        }
    } 
    
    // NOTE that we allow right drag, and treat it just like left drag, although you can do
    // something special on right drag by checking EH.rightButtonDown()
    if (target) EH.prepareForDragging(target);
    
    // bubble the mouseDown event to anyone who wants it
    var handlerReturn = EH.bubbleEvent(target, eventType, null, targetIsMasked);
    if (forceIEFocusTarget != null) forceIEFocusTarget.focus();
    
	if (handlerReturn == false) {
        // a an explicit "false" returned from mouseDown will cancel dragging
        delete EH.dragTarget;
        delete EH.dragTargetLink;
    }

	// if the right button is down, the return value can affect the context menu.
    // In DOM browsers, we receive a native showContextMenu event (see
    // this.handleContextMenu()), and our response to that affects whether the context
    // menu will be shown.
	if (EH.rightButtonDown()) {
        // Bail unless we're in a browser where we never get a separate right-mouse event
        if (!this.useSyntheticRightButtonEvents()) return true;

        
        if (target && 
            (EH.getBubbledProperty(target, "contextMenu") || 
             EH.getBubbledProperty(target, "showContextMenu", true) != isc.Canvas.getInstanceProperty("showContextMenu")))
        {
        
			// return false to suppress native context menu, since we'll show our own on mouseUp
            //this.logWarn("rightMouseDown: false");
            event.returnValue = false;
			return false;
		}
        // return true to allow the context menu in Nav4.  Note that when we return true here, we
        // never get mouseUp in Nav4.
        //Log.logWarn("rightMouseDown: true");
        return true;
	}
    // if the mouseDown handler didn't return false, set up a timer to send mouseStillDown events
	if (handlerReturn != false) {	
	    
		// if the target or a parent has a mouseStillDown message fire the mouseStillDown event,
		// this will keep firing on a timer until the mouse goes up
		if (EH.hasEventHandler(target, EH.MOUSE_STILL_DOWN)) {
			// call _handleMouseStillDown, which will start the timer automatically
			EH._handleMouseStillDown();
		}
	}

    

    var aboutToDrag = (EH.dragTarget != null &&
                       EH.dragOperation != EH.DRAG_SELECT);

    // Return false to cancel native drag mode if we're about to do an ISC drag.
    // Return false in Moz if text selection is diallowed    
    // (type-casting target._allowNativeTextSelection() to a boolean - if this 
    // returns undef we want to  return an explicit false so drag selection is disallowed)
    // Don't return false if we're handling a touch event because this cancels native touch
    // scrolling.
    var returnVal = (isc.Browser.hasNativeDrag && target._getUseNativeDrag()) ||
                    (!aboutToDrag && 
                     ((EH._handledTouch == EH._touchEventStatus.TOUCH_STARTED) ||
                      !(isc.Browser.isMoz || isc.Browser.isSafari) || 
                      !!target._allowNativeTextSelection(event)));
    return returnVal;
},

//>	@classMethod	isc.EventHandler.stillWithinMouseDownTarget()
//	Is the current event still within the last Canvas where the mouse went down?
//  Note: You need to call this method to get correct cross-platform determination of whether
//        the current event is still within the mouseDown target.
//		@group	mouseEvents
// @visibility internal
//<

stillWithinMouseDownTarget : function () {
    var mouseDownTarget = this.mouseDownTarget();
    if (!mouseDownTarget) return false; // mouse didn't go down in a Canvas

    var lastEvent = this.lastEvent;

    // see if we're within the same Canvas that the mouse went down in
    var stillWithin = (mouseDownTarget == lastEvent.target);
    if (!stillWithin) return false;

    if (lastEvent._stillWithin != null) return lastEvent._stillWithin;

    return stillWithin;
},

//>	@classMethod	isc.EventHandler.handleMouseMove()
// Starts dragging if the dragTarget is set and the mouse has moved more than [5] pixels
//	
// Sets the dropTarget if we're dragging and are over a droppable target
//
// Generates mouseOver/mouseOut or dragOver/dragOut events on Canvas boundaries.
//
//		@group	mouseEvents
//
//		@param	DOMevent	(DOM event) DOM event object (as passed by isc.EventHandler)
//
//		@return				(boolean)	false == cancel native event processing
//										anything else == continue native event processing
// @visibility internal
//<
// called directly by DOM
handleMouseMove : function (DOMevent) {

    // Some browsers (like Mac IE) have problems dealing with events fired before the page
    // finishes loading.  Just skip mouse event processing if the page hasn't loaded yet.
	if (!isc.Page.isLoaded()) return false;
    
        var EH = isc.EH;

    // No-Ops in Touch environments
    // (See comments in handleMouseDown for why we do this)
    if (!EH.nativeDragging && (isc.Browser.isTouch || navigator.pointerEnabled)) return;

    
    if (EH._handlingMouseDown || EH._handlingMouseUp) return;

	var event = EH.getMouseEventProperties(DOMevent);

    
    if ((isc.Browser.isMoz || isc.Browser.isIE)
        
        && !EH.immediateMouseMove
        
        && !EH.nativeDragging
    ) {
        //Log.logWarn("postponing mouseMove (last time: " + EH.lastMouseMoveTime + 
        //            " exceeds threshold of " + EH.delayThreshold);
        // set a timer to fire mouseMove later
        if (EH.delayedMouseMoveTimer == null) {
            EH.delayedMouseMoveTimer = 
                 isc.Timer.setTimeout({target:EH, methodName:"_delayedMouseMove",
                                       args:[isc.timeStamp()]}, 0, true);
        }
        
        
        return true;
    }

    
    var result = EH._handleMouseMove(DOMevent, event);
    
    return result;
},

_delayedMouseMove : function (ts) {
    //var now = isc.timestamp();
    //if ((now - ts) > 30) {
    //    this.logWarn("long timer firing delay: " + (now-ts) + "ms");
    //}

    this.delayedMouseMoveTimer = null;

    //var start = isc.timeStamp();
    this._handleMouseMove(null, this.lastEvent);
    //var end = isc.timeStamp();

    //if ((end - start) > 5) {
    //    this.logWarn("long mouse move: " + (now-ts) + "ms");
    //}
},


_handleMouseMove : function (DOMevent, event) {
    this._handlingMouseMove = true;
    var returnVal = this.__handleMouseMove(DOMevent, event);
    this._handlingMouseMove = null;
    return returnVal;
},

// When set to true, while the mouse is down, mouseMove() is only fired when the mouse is moved
// within the mouseDown target. This was the behavior of Nav4 and Mozilla Suite.
enableMouseMoveBackCompat: false,

__handleMouseMove : function (DOMevent, event) {
    var EH = this;

    var mouseDown = EH.mouseIsDown();

    // We might start dragging if:
    // - the mouse is down
    // - the mouse went down on a Canvas that allows dragging (EH.dragTarget, set up in 
    //   prepareForDragging)
    // - the Canvas does not have useNativeDrag:true
    // - we're not already dragging
    // - there are no open menus (since showing a context menu and handling a drag operation are
    //   mutually exclusive)
    var mightStartDrag = (mouseDown &&
                          EH.dragTarget &&
                          (!isc.Browser.hasNativeDrag || !EH.dragTarget._getUseNativeDrag()) &&
                          !EH.dragging &&
                          (!(isc.Menu && isc.Menu._openMenus && isc.Menu._openMenus.length > 0) ||
                          		EH.dragTarget._isScrollThumb ||
                          		isc.isA.Menu(EH.dragTarget) ));

    
    var missedMouseUp;
    if (isc.Browser.isIE && event.originalType !== EH.POINTER_MOVE) {
        var buttonNum = event.buttonNum;
        if (mouseDown) {
            
            if (!mightStartDrag && buttonNum == 0) {
                if (EH.dragging) {
                    
                } else {
                    missedMouseUp = true;
                }
            }
        
        } else if (buttonNum == 1 && event.eventType == EH.MOUSE_MOVE) {
            if (EH._firedSyntheticMouseUp) {
                
            } else {
                event.eventType = EH.MOUSE_DOWN;
                EH.handleMouseDown(null, event);
                event.eventType = EH.MOUSE_MOVE;
            }
        }
    }

    
 	var target = event.target,
 		isNative = EH.eventHandledNatively(EH.MOUSE_MOVE, event.nativeTarget)
 	;
    
	if (EH._mouseIsDownInScrollbar) {
        
        EH.handleMouseUp(DOMevent, true);
    
    } else if (missedMouseUp) {
        EH.logInfo("sythesizing mouseUp " +
                (EH._outOfWindow ? 
                    "due to mouseUp outside window," : 
                    
                    "[buttonNum cleared on mouseMove with no mouseUp event],") +
                " buttonNum: " + 
                   event.buttonNum);
        EH.handleMouseUp(DOMevent, true);
    }
    delete EH._outOfWindow;

    // If we synthesized a mouseUp() event, then update the `mightStartDrag' and `mouseDown'
    // local variables.
    if (!EH.mouseIsDown()) {
        mightStartDrag = mouseDown = false;
    }

    

    // NOTE: we allow right mouse button dragging, and at the EventHandler level we treat it
    // just like normal dragging, however, a specific drag handler can implement special
    // semantics for right dragging.

    
    if (isc.Browser.isMoz && (isc.Browser.geckoVersion < 20100914) && mouseDown &&
        event.target && event.target._useMozScrollbarsNone 
        && event.target != EH.mouseDownTarget()) 
    {
        event.nativeDraggingTarget = event.nativeTarget;
        event.nativeTarget = null;
        target = event.target = EH.mouseDownTarget();
    }

    // start dragging if:
    // - `mightStartDrag' is true
    // - the mouse has moved a certain distance, in either direction, from the mouseDown point
    if (mightStartDrag &&
        (Math.abs(event.x - EH.mouseDownEvent.x) > EH.dragTarget.dragStartDistance ||
         Math.abs(event.y - EH.mouseDownEvent.y) > EH.dragTarget.dragStartDistance))
    {
        EH.handleDragStart();
    }

	// if we're dragging, jump over to handleDragMove which does special processing
	if (EH.dragging) {
	    return EH.handleDragMove();
	}
	// if the right button is down
	if (EH.rightButtonDown()) {
		// don't send mouseMove.  We could send a 'rightMouseMove' event, but we've decided not
        // to do that now.
        
        
        if (!isc.Browser.isMac || !EH.ctrlKeyDown()) return true;
	}

    
    if (EH.enableMouseMoveBackCompat && mouseDown) {
        target = EH.stillWithinMouseDownTarget() ? EH.mouseDownTarget() : null;
	} else {
		target = event.target;
	}

	// if the target is not the last object that got the move event,
	// send the mouseOut and mouseOver routines to the appropriate objects
	if (target != EH.lastMoveTarget) {
        //>DEBUG
        if (this.logIsDebugEnabled()) {
            this.logDebug((EH.lastMoveTarget ? 
                           "mousing out of " + EH.lastMoveTarget + "  " : "") +
                           (target ? "mousing over " + target : ""));
        }
        //<DEBUG
		// send the mouseOut event to the last mover object
        var lastMoveTarget = EH.lastMoveTarget,
            hoverTarget,
            lastHoverTarget = EH.lastHoverTarget;

        if (lastMoveTarget) {
            delete EH.lastMoveTarget;
            EH.handleEvent(lastMoveTarget, EH.MOUSE_OUT);
        }

        // send the mouseOver event to the target
        if (target) {
            
            var hoverResult = EH.handleEvent(target, EH.MOUSE_OVER);
            // use 'getHoverTarget()' to determine which widget should receive a hover event.
            if (hoverResult != false) hoverTarget = target.getHoverTarget(event);
        }

        // Send hover events to the hover target/last hover target.
        // The Canvas level implementation handles actually setting up timers to fire 
        // user-visible hover handlers.
        if (hoverTarget != lastHoverTarget) {
            if (lastHoverTarget) lastHoverTarget.stopHover();
            if (hoverTarget) hoverTarget.startHover();
            
            EH.lastHoverTarget = hoverTarget;
        }

		// remember that we're the last move object
		EH.lastMoveTarget = target;
	}

	// call the global event handler
	if (isc.Page.handleEvent(target, EH.MOUSE_MOVE) == false) return false;

	// see if we shouldn't pass this event on to DOM object, and return true if we should
	if (isNative) return EH._handledNativelyReturnVal;

	// if the target isn't defined or isn't enabled, return false
	if (!EH.targetIsEnabled(target)) return false;

	// bubble the event
	EH.bubbleEvent(target, EH.MOUSE_MOVE);

    // update the cursor
	if (target) target._updateCursor();
    	
    
    return true;
    
},

//> @classMethod isc.EventHandler.getNativeMouseTarget() [A]
// Returns the natively reported target (or source) DOM element for the current mouse event.
// <b>NOTE:</b> SmartClient cannot guarantee that the same element will
// be reported in all browser/platform configurations for all event types.
// If you wish to make use of this value, we recommend testing your use case 
// in all target browser configurations.
//
// @return (HTML Element) native DOM element over which the mouse event occurred
// @visibility external
//<
// Common known issue - during drag/drop interactions some browsers may report
// the mouse down target (the target being dragged) rather than the target the user is currently
// hovering over.

getNativeMouseTarget : function (event) {
    if (!this.nativeTargetWarningLogged) {
        this.nativeTargetWarningLogged = true;
        this.logWarn("getNativeMouseTarget(). This method will return the DOM element " +
            "the browser reports as the target or source of the current mouse event. " +
            "Please note that SmartClient cannot guarantee that the same element will " +
            "be reported in all browser/platform configurations for all event types. " +
            "If you wish to make use of this value, we recommend testing your use case " +
            "in all target browser configurations.");
            
    }
    if (event == null) event = this.lastEvent;
    return event.nativeTarget || event.nativeDraggingTarget;
    
},


// handle a native mouseOut event
handleNativeMouseOut : function (DOMevent) {
    // we generally synthesize the mouseOut event during mouseMove.  However, for the special
    // case of mousing out of the browser window via exiting a Canvas which is which is flush
    // with the window border, we'll never get a mouseMove outside of our Canvas, so we have to
    // detect this case specially.

    
    if (isc.Browser == null) return;

	
        var EH = isc.EH;

    
    if (EH._handlingMouseDown || EH._handlingMouseUp) return;

    var event = (DOMevent ? DOMevent : EH.getWindow().event),
		target = (isc.Browser.isDOM ? event.target : event.srcElement),
        leavingWindow = false;

    if (isc.Browser.isIE) {
        
        leavingWindow = (event.toElement == null);
        
    } else {
        
        leavingWindow = (event.relatedTarget == null);
        
    }

    //EH.logWarn("leaving window:" + leavingWindow + ", lastMoveTarget: " + EH.lastMoveTarget);

    // used for detecting mouseUps that happen outside the window, where possible
    if (leavingWindow) EH._outOfWindow = true;   

    var lastMoveTarget = EH.lastMoveTarget;
    if (leavingWindow && lastMoveTarget) {
        // Update properties on the lastEvent object before bubbling handlers.
        
        EH._updateMouseOutEventProperties(event);
        delete EH.lastMoveTarget;
        EH.handleEvent(lastMoveTarget, EH.MOUSE_OUT);
        if (EH.lastHoverTarget) {
            EH.lastHoverTarget.stopHover();
            delete EH.lastHoverTarget;
        }
    }
},


// update lastEvent with properties from a native 'mouseOut' event.
_updateMouseOutEventProperties : function (nativeEvent) {
	
        var EH = isc.EH;

    var lastEvent = EH.lastEvent;

    // Store the target we're moving into as the event target (rather than the target we're
    // moving out of!)
    if (isc.Browser.isIE) {
        lastEvent.nativeTarget = nativeEvent.toElement;    
    } else {
        lastEvent.nativeTarget = nativeEvent.relatedTarget;
    }

    if (lastEvent.nativeTarget == null) lastEvent.target = null
    else lastEvent.target = this.getEventTargetCanvas(nativeEvent, lastEvent.nativeTarget);    
},



// Send the mouseDown target a periodic, synthetic "still down" event while the mouse stays
// down
//
// Allows for things like repeated scrolling while the mouse button is held down in
// scrollbar buttons.
//
// NOTE: mouseStillDown is also fired once immediately on mouseDown
_handleMouseStillDown : function (timeStamp) {
	// Some browsers (like Mac IE) have problems dealing with events fired before the page
    // finishes loading.  Just skip mouse event processing if the page hasn't loaded yet.
	if (!isc.Page.isLoaded()) return false;
	var EH = this;
	
	// clear the old timeout if necessary
    EH._stillDownTimer = isc.Timer.clear(EH._stillDownTimer);
	
	// if the mouse is already up, or the mouseDownTarget can no longer be found, bail
    
	if (!EH.mouseIsDown() || !EH.mouseDownTarget()) return false;
	
	// send the event up the chain of the target
	if (EH.bubbleEvent(EH.mouseDownTarget(), EH.MOUSE_STILL_DOWN) == false) return false;
    
	// start the timer to call this again after a delay.  
    
    var target = EH.mouseDownTarget(),
        delay = this._handlingMouseDown ? target.mouseStillDownInitialDelay :
                                          target.mouseStillDownDelay;
  	EH._stillDownTimer = this.delayCall("_handleMouseStillDown", [], delay);

    // alternate code that allows checking the actual delay before the timer fired
    //if (!this._handlingMouseDown) {
    //    this.logWarn("mouseStillDown fired after: " + (isc.timeStamp() - timeStamp) + "ms");
    //}
  	//EH._stillDownTimer = 
    //    isc.Timer.setTimeout("isc.EH._handleMouseStillDown(" + isc.timeStamp() + ")", delay);

	return true;
},



//>	@classMethod	isc.EventHandler.handleMouseUp()
//			Special handler for mouse up events.
//
//			fires  showContextMenu, click and doubleClick events as necessary
//
//		@group	mouseEvents
//		@param	DOMevent	(DOM event) DOM event object (as passed by isc.EventHandler)
//
//		@return				(boolean)	false == cancel native event processing
//										anything else == continue native event processing
//  @visibility internal
//<
// called directly from DOM, and by other methods
handleMouseUp : function (DOMevent, fakeEvent) {

    
	
        var EH = isc.EH;

    // No-Ops in Touch environments
    // (See comments in handleMouseDown for why we do this)
    if (!EH.nativeDragging && (isc.Browser.isTouch || navigator.pointerEnabled) && !fakeEvent) return;

    
    if (isc.Browser.isIE && !EH._mouseIsDown) {
    
        
        if (EH._firedSyntheticMouseUp) return;
    
        var lastEvent = EH.lastEvent;
        lastEvent.eventType = EH.MOUSE_DOWN;
        EH.handleMouseDown(null, EH.lastEvent);
    }
    EH._firedSyntheticMouseUp = fakeEvent;

    
    if (!fakeEvent) EH._handlingMouseUp = true;
    var returnValue = EH._handleMouseUp(DOMevent, fakeEvent);
    EH._handlingMouseUp = false;

    
    if (isc.Browser.isSafari) returnValue = true;
    
    
    
    
    return returnValue;
},
_handleMouseUp : function (DOMevent, fakeEvent) {

    // Some browsers (like Mac IE) have problems dealing with events fired before the page
    // finishes loading.  Just skip mouse event processing if the page hasn't loaded yet.
	if (!isc.Page.isLoaded()) return false;
	var EH = this,
		// get the properties of the event
		event = (!fakeEvent ? EH.getMouseEventProperties(DOMevent) : EH.lastEvent),
		isNative = false;
    
	EH._mouseIsDown = false;
	delete EH.__handleClickReturnValue;
	
	// clear the stillDownTimer
	EH._stillDownTimer = isc.Timer.clear(EH._stillDownTimer);

    // switch focus if the one-time flag has been set
    var focusTarget = EH._delayedFocusTarget;

    if (focusTarget) {
        //EH.logWarn("focusing in delayed target:" + focusTarget);
        focusTarget.focus();
        EH._delayedFocusTarget = null;
    }

    // EH.__cancelNextMouseUp is a one-time flag to cancel mouseUp.  
    // This is used when, on mouseDown, you want to cancel the entire click.
    // We set this flag to true on mouseDown if the target is masked and the clickEvent for the
    // click mask returned false.

    // Remember this flag's value, and clear it
    var cancelMouseUp = EH.__cancelNextMouseUp;
    EH.__cancelNextMouseUp = null;

    
    var targetIsMasked;
    if (cancelMouseUp == null) {
        cancelMouseUp = (EH.clickMaskClick(event.target) == false);
        targetIsMasked = cancelMouseUp;
    }

    // Return if cancelling the mouseUp
    if (cancelMouseUp == true) {
        if (EH.logIsDebugEnabled()) EH.logDebug("mouseUp cancelled by clickMask");
        return false;
    }
    
    var successfulDrag = false;
    // if we're dragging, stop dragging.  NOTE: this applies to right button down as well,
    // which is treated as normal dragging.
    if (EH.dragging) {
        successfulDrag = EH.handleDragStop();
    }

	// right mouse button just came up, it's a right click (handleContextMenu())
	if (EH.rightButtonDown(event)) {
        
		if (this.useSyntheticRightButtonEvents()) {
			EH.handleContextMenu();
		}

		// suppress the click event
		EH.__handleClickReturnValue = false;
	
	} else {
        // only send mouseUp and click if drag was unsuccessful (or never started)
        // NOTE: it's very important to send mouseUp and click if drag wasn't successful, 
        // because among other problems, frequently a D&D interaction will be triggered by a 
        // sloppy click where there is a small distance between mouseDown and mouseUp, and 
        // then the click will appear to have mysteriously failed.

         
        
        if (!successfulDrag) {            
            if (isc.Page.handleEvent(event.target, EH.MOUSE_UP) != false) {                

                var mouseUpReturn = true, 
                    x = event.x, y = event.y,
                    mouseDownTarget = EH.mouseDownTarget();

                isNative = EH.eventHandledNatively(EH.MOUSE_UP, event.nativeTarget);
                if (!isNative && EH.targetIsEnabled(mouseDownTarget)) {
                    if (mouseDownTarget.visibleAtPoint(x, y)) {
                        mouseUpReturn = EH.bubbleEvent(mouseDownTarget, EH.MOUSE_UP, null, 
                                targetIsMasked);

                    // On touch-enabled devices, we might not get another event for a while.
                    // If we're in the middle of handling the end of a touch event sequence,
                    // then fire mouseOut() on the mouseDownTarget so that it has a chance to
                    // reset its appearance.
                    //
                    // For example, if the user touchstart's a tab (making the tab the mouseDownTarget,
                    // then drags their finger off of the tab, firing the mouseOut() event here
                    // resets the tab's appearance to the STATE_UP state.
                    } else if (EH._handledTouch === EH._touchEventStatus.TOUCH_ENDING || mouseDownTarget.containsPoint(x, y)) {
                        mouseUpReturn = EH.bubbleEvent(mouseDownTarget, EH.MOUSE_OUT, null, 
                                targetIsMasked);
                    }
                }
                // NOTE: call handleClick even if the Canvas target is null or changed between 
                // mouseDown and mouseUp, because we should still fire page-level click in that
                // circumstance.  handleClick will check (redundantly) for same target before
                // firing Canvas-level click.
                // If ending a touch event, then make sure that the distance between the mouseDown
                // ('touchstart') position is not too far away from the mouseUp position. Otherwise,
                // if the user native touch scrolls a component, then we would often fire handleClick.
                if (mouseUpReturn != false &&
                    (EH._handledTouch !== EH._touchEventStatus.TOUCH_ENDING ||
                     
                     (EH.mouseDownEvent != null &&
                      Math.abs(EH.mouseDownEvent.x - x) <= 10 &&
                      Math.abs(EH.mouseDownEvent.y - y) <= 10)))
                {
                    // Fire a programmatic "fast click" on the DOMevent target if we're handling
                    // a touch event sequence.
                    // The reason for this is because we preventDefault() the 'touchend' event
                    // when the target is an element for which _shouldIgnoreTargetElem() returns
                    // false so that an input element will not receive keyboard focus if it wasn't
                    // the target of the original tap. However, this leads to the issue that the
                    // native action (such as navigation when a link is tapped, toggling a checkbox
                    // when a checkbox is tapped, etc.) is canceled. To counteract this, we fire
                    // a programmatic 'click' DOM event on the element. This performs the same native
                    // action that was canceled in _handleTouchEnd().
                    var targetElem;
                    if (EH._handledTouch === EH._touchEventStatus.TOUCH_ENDING &&
                        (targetElem = DOMevent.target && (DOMevent.target.nodeType == 1 ? DOMevent.target : DOMevent.target.parentElement)) != null &&
                        (!isc.Browser.isAndroid || event.originalType !== EH.CLICK))
                    {
                        if (!EH._shouldIgnoreTargetElem(targetElem)) {
                            var doc = DOMevent.view.document;

                            var clickDOMevent = doc.createEvent("MouseEvents");
                            clickDOMevent.initMouseEvent("click", true, true, DOMevent.view,
                                                         1, // one click
                                                         DOMevent.screenX, DOMevent.screenY,
                                                         DOMevent.clientX, DOMevent.clientY,
                                                         false, false, false, false,
                                                         0, null);
                            // Mark our 'click' events so that we know to ignore them in handleNativeClick().
                            clickDOMevent._fastClick = true;
                            window.setTimeout(function () {
                                targetElem.dispatchEvent(clickDOMevent);
                            }, 1);
                        }
                    }

                    EH.__handleClickReturnValue = EH.handleClick(event.target);
                }
            }
        }
    }

	// and always clear the redrawnWhileDown property
	delete EH.redrawnWhileDown;
	
	// clear all drag properties set by unscrupulous methods above
	EH.clearDragProperties();
	
    

	// EH._mouseIsDownInScrollbar must be cleared before we return, but *after* mouseUp and
    // click events been handled (and their bubbling averted by EH.eventHandledNatively()
    // immediately returning true) above
	if (EH._mouseIsDownInScrollbar) EH._mouseIsDownInScrollbar = false;

    //isc.Log.logWarn("handleMouseUp returning: " + 
    //                (isNative && EH.__handleClickReturnValue == true) +
    //                ", isNative: " + isNative + 
    //                ", handleClick: " + EH.__handleClickReturnValue);

    // If mouseUp or click returned false, __handleClickReturnValue will be false 
	// return true if the event was handled natively, or if neither of these handlers 
    // returned false.
    
    var target = event.target,
        overForm = isc.isA.DynamicForm!=null && isc.isA.DynamicForm(target);

    if (isNative && (overForm || EH.__handleClickReturnValue == true)) 
        return EH._handledNativelyReturnVal;
    // Return false unless this was over a form with an explicit true return val
    return (overForm && EH.__handleClickReturnValue == true);
},


// Clear all the miscellaneous isc.EventHandler properties set as a result of dragging.
clearDragProperties : function () {
	var EH = this;
    EH.wasNativeDragging = false;
    EH.crossFrameDragging = EH.nativeDragging = EH.dragging = false;
    delete EH._lastDragEnterTarget;
    if (EH._crossFrameDragCleanupTmrID != null) {
        isc.Timer.clear(EH._crossFrameDragCleanupTmrID);
        delete EH._crossFrameDragCleanupTmrID;
    }

	delete EH.dragTarget;
	delete EH.dragTargetStartRect;
	delete EH.dragTargetLink;

	delete EH.dragMoveTarget;
	delete EH.dragMoveAction;

	delete EH.dragOperation;
	delete EH.dragAppearance;

	delete EH.dropTarget;
	delete EH.lastDropTarget;
},


// handle context menu events.  Can be called directly by the browser or synthetically.

handleContextMenu : function (DOMEvent, synthetic) {
	// Some browsers (like Mac IE) have problems dealing with events fired before the page
    // finishes loading.  Just skip mouse event processing if the page hasn't loaded yet.
	if (!isc.Page.isLoaded()) return false;

    
    
        var EH = isc.EH;
    
    EH._handlingMouseUp = true; 
    var returnValue = EH._handleContextMenu(DOMEvent, synthetic);
    EH._handlingMouseUp = false;
    
    return returnValue;
},

_handleContextMenu : function (DOMEvent, synthetic) {
    
    var fromMouseEvent = this.isMouseEvent(this.lastEvent.eventType);
    
    if (this._contextMenuShown) {
        delete this._contextMenuShown;
        if (!synthetic) {
            return true;
        }
    }
    
    // If this is not a synthetic event, pick up the event properties
	if (DOMEvent) this.getMouseEventProperties(DOMEvent);
    var EH = this,
		event = EH.lastEvent,
        target = !fromMouseEvent ? event.keyTarget || event.target : event.target;
    // record that this was a mouse or keyboard triggered event. It's much harder to determine
    // this once we've wiped the previous event type
    
    event.keyboardContextMenu = !fromMouseEvent;
    
    event.eventType = "contextMenu";
        
    // If this came from a keyboard event, in IE the coordinates reported will be the
    // mouse coordinates. In order to show the context menu in the right place, specify the
    // event coordinates as matching basically the top/left of the focus canvas
    if (!fromMouseEvent) {
        event.x = target ? target.getPageLeft() : 0;
        event.y = target ? target.getPageTop() : 0;
    }

    
	if (isc.Browser.isSafari && EH.clickMaskClick(target) == false) {
        return false;
    }
        
	// handle the page-level showContextMenu event
	if (isc.Page.handleEvent(target, EH.SHOW_CONTEXT_MENU) == false) {
        return false;
    }

	// if the target is enabled, send it a showContextMenu event.
    var returnValue = true;
	if (EH.targetIsEnabled(target)) {	  
        returnValue = EH.bubbleEvent(target, EH.SHOW_CONTEXT_MENU);
    }

    if (returnValue != false) {
        
        var lastMoveTarget = EH.lastMoveTarget;
        if (lastMoveTarget) {
            delete EH.lastMoveTarget;
            EH.handleEvent(lastMoveTarget, EH.MOUSE_OUT);
        }
    }
    return returnValue;
},


handleNativeClick : function (DOMevent) {
    var EH = isc.EH,
		value = (EH.__handleClickReturnValue != false);

	delete EH.__handleClickReturnValue;

    if (DOMevent._fastClick) return value;

	

    if (isc.Browser.isAndroid) {

        
        if (isc.Browser.isChrome) {
            var mouseDownEvent = EH.mouseDownEvent;
            if (mouseDownEvent != null) {
                var lastEventTarget = EH.lastEvent.target; // save because it's going to be overwritten.
                EH.DOMevent = DOMevent;
                var event = EH.getMouseEventProperties(DOMevent);

                
                if (isc.isA.Canvas(event.target)) {
                    // Find the top-level canvas.
                    var topLevelCanvas = event.target.getTopLevelCanvas();

                    var topLevelClipHandle = topLevelCanvas.getClipHandle();
                    
                    var ts = topLevelClipHandle.dataset.iscMouseDownEventTimeStamp;
                    // If `ts' is null, then the top-level canvas and its descendants did not
                    // receive the last 'touchstart' event.
                    if (ts == null || parseFloat(ts) < mouseDownEvent.DOMevent.timeStamp) {
                        
                        DOMevent.preventDefault();
                        return false;
                    }
                }

                
                if (lastEventTarget !== event.target ||
                    (lastEventTarget._differentEventCharacteristics != null &&
                     lastEventTarget._differentEventCharacteristics(mouseDownEvent, event)))
                {
                    event.originalType = EH.CLICK;
                    event.eventType = EH.MOUSE_MOVE;
                    EH._handleMouseMove(DOMevent, event);
                    event.eventType = EH.MOUSE_DOWN;
                    EH.doHandleMouseDown(DOMevent, event);
                    event.eventType = EH.MOUSE_UP;
                    EH._handleMouseUp(DOMevent, true);
                }
            }

        
        } else {
            EH.DOMevent = DOMevent;
            var event = EH.getMouseEventProperties(DOMevent);

            switch (this._handledTouch) {
                case EH._touchEventStatus.READY_FOR_TOUCH:
                    event.originalType = EH.CLICK;
                    // fire synthetic mouseMove
                    event.eventType = EH.MOUSE_MOVE;
                    EH._handleMouseMove(DOMevent, event);
                    // fire synthetic mouseDown
                    event.eventType = EH.MOUSE_DOWN;
                    EH.doHandleMouseDown(DOMevent, event);
                    // fall through to handle the mouseUp

                case EH._touchEventStatus.TOUCH_STARTED:
                    // fire synthetic mouseUp
                    event.originalType = EH.CLICK;
                    event.eventType = EH.MOUSE_UP;
                    EH._handleMouseUp(DOMevent, true);
                    break;

                case EH._touchEventStatus.TOUCH_COMPLETE:
                    
                    if (isc.Browser.isAndroidWebView && isc.Browser.androidMinorVersion < 4.4) {
                        var targetElem = DOMevent.target && (DOMevent.target.nodeType == 1 ? DOMevent.target
                                                                                           : DOMevent.target.parentElement);
                        if (!EH._shouldIgnoreTargetElem(targetElem)) {
                            DOMevent.preventDefault();
                            return false;
                        }
                    }

                    break; // non-error case
            }
            // finished with synthetic events; return to resting state 
            this._handledTouch = EH._touchEventStatus.READY_FOR_TOUCH;
        }
    }

    if (isc.Browser.isIE && !isc.Browser.isIE10) {
        EH.DOMevent = DOMevent;
        var event = EH.getMouseEventProperties(DOMevent);

        if (event.target && event.target._getUseNativeDrag()) return false;
    }

//	return value;
},

// NOTE: handleClick() is always called by handleMouseUp() during processing of a native
// mouseUp event, because we always synthesize the click event ourselves.  handleNativeClick()
// is what we do when we get a native click event.
handleClick : function (target, eventType) {
	// Some browsers (like Mac IE) have problems dealing with events fired before the page
    // finishes loading.  Just skip mouse event processing if the page hasn't loaded yet.
	if (!isc.Page.isLoaded()) return false;

	var EH = this,
		// get the properties of the event as 'lastEvent'
		event = EH.lastEvent,
        returnVal;

	// get the eventType if it wasn't passed in
    
	if (!eventType) eventType = (EH.isDoubleClick(target) ? EH.DOUBLE_CLICK : EH.CLICK);
    //EH.logWarn("target: " + target + ", mouseDownTarget: " + EH.mouseDownTarget() +
    //             ", stillWithinTarget: " + EH.stillWithinMouseDownTarget() + 
    //             ", native target: " + this.echoLeaf(event.nativeTarget));

	// call the Page-level click handler.  Note we will fire page-level click even if there is
    // no Canvas target
	if (isc.Page.handleEvent(target, eventType) == false) {
        returnVal = false;

	// if the click event was handled natively, bail
	} else if (EH.eventHandledNatively(eventType, event.nativeTarget)) {
		returnVal = EH._handledNativelyReturnVal;

	// if we have an enabled target, bubble the event
	} else if (!EH.targetIsEnabled(target)) {
        returnVal = false;

    // don't fire click if the Canvas target changed between mouseDown and mouseUp
    // or when the target is the body of a menu (otherwise the menu will not register the click
    // when the menu is within a window). One exception is in handling a touch event. It is
    // very common for the mouseDown target (really the touchStart target) to be different than
    // the mouseUp (touchEnd/touchCancel) target.
    // In processing a touch event sequence, we have a tolerance in place where we consider
    // the distance from the touchStart position to the touchEnd position. If outside of the
    // tolerance, then handleClick() is not called.
    } else if (EH._handledTouch !== EH._touchEventStatus.TOUCH_ENDING &&
               !EH.stillWithinMouseDownTarget())
    {
        returnVal = false;

    // otherwise bubble the event through the target canvas chain
    } else {
        var target = EH.mouseDownTarget();
    	returnVal = EH.bubbleEvent(target, eventType);
    }

    // Record when the event completed. We use this for double-click detection in some cases.
    EH._lastClickCompleteTime = isc.timeStamp();

	return returnVal;
},


//>	@classMethod	isc.EventHandler.isDoubleClick()	(A)
// Return true if this click in the target should be considered a double click, 
// false for a single click
//
//		@group	mouseEvents
//		@param	target		(object)	Canvas or DOM object that received the event
//
//		@return				(boolean)	true == real double click
//										false == really a single click
// @visibility internal
//<
isDoubleClick : function (target) {
	var EH = this,
        target = target || EH.lastEvent.nativeTarget;
    
    // If this click event occurred within the double-click delay of the last event, fire the
    // double-click handler rather than the click handler.
    
    var useNativeEventTime = EH.useNativeEventTime != null ? EH.useNativeEventTime :
                            (isc.Browser.isMoz && isc.Browser.isWin),
        time, withinDoubleClickInterval;

    // Explicit flag to indicate second click within double-click delay. This is used
    // by auto-test systems to ensure we handle two clicks as double click or separate single clicks
    // regardless of the timing during playback!
    if (EH._isSecondClick != null) {
        withinDoubleClickInterval = EH._isSecondClick;
    } else {
            
        if (useNativeEventTime) {
            var domevent = EH.lastEvent.DOMevent
                time = domevent ? domevent.timeStamp : null;
            // Sanity check
            if (time == 0 || !isc.isA.Number(time)) {
                this.logDebug("Unable to derive native 'timeStamp' attribute from DOM event");
                time = isc.timeStamp();
            }
            withinDoubleClickInterval = ((time - EH.lastClickTime) < EH.DOUBLE_CLICK_DELAY);        
            
        } else {
            time = isc.timeStamp();
            withinDoubleClickInterval = 
                // if it took us less than the double click delay to process the first click
                ((EH._lastClickCompleteTime - EH.lastClickTime) < EH.DOUBLE_CLICK_DELAY) ?
                    // then check for a second click strictly within the normal interval
                    ((time - EH.lastClickTime) < EH.DOUBLE_CLICK_DELAY) :
                    // otherwise treat any click that closely follows completion of the first click
                    // as a double click
                    ((time - EH._lastClickCompleteTime) < 100);        
        }
    }
        

        
	// remember the click time for later
	EH.lastClickTime = time;

	// clear the lastClickTarget if it's already too late
	if (!withinDoubleClickInterval) {
		delete EH.lastClickTarget;
	}
	
    // If the two clicks occurred on the same target this is a doubleClick,
    // unless the special "noDoubleClicks" flag is set on the target, (in which case
    // we fire multiple single clicks).
     
    var isDouble = false; 
    if (target == EH.lastClickTarget) {
        isDouble = !target.noDoubleClicks;
        if (isDouble) {
            var current = target;
            while (current.parentElement) {
                current = current.parentElement;
                if (current.noDoubleClicks) {
                    isDouble = false; 
                    break;
                }
            }
        }
    }

	// if this is a doubleClick, remove the lastClickTarget so we'll go back to clicking next
    // time
	// otherwise note that we were the last thing clicked in case a double-click comes in later
	EH.lastClickTarget = (isDouble ? null : target);
	
	return isDouble;
},


// If the target passed in has a concept of enabled/disabled, determine if it is enabled.
targetIsEnabled : function (target) {

	// if no target specified, return false
	if (!target) return false;
    
    // If the target has been marked as destroyed, don't attempt to fire the handler on it
    if (target.destroyed) return false;

	// if the target has a isDisabled function, return the results of that
	//	for canvases, this is defined to go up the parent chain for us
	if (isc.isA.Function(target.isDisabled)) return !target.isDisabled();

	// otherwise, the target doesn't have the concept of being enabled, so assume it is enabled
	return true;
},


// Pointer events
// ---------------------------------------------------------------------------------------
_handlePointerDown : function (DOMevent) {
    var EH = isc.EH;

    if (DOMevent.pointerType === "touch") {
        return EH._handleTouchStart(DOMevent);
    } else {
        EH.DOMevent = DOMevent;
        var event = EH.getMouseEventProperties(DOMevent);
        if (EH.eventHandledNatively(DOMevent.type, DOMevent.target)) return EH._handledNativelyReturnVal;
        event.originalType = event.eventType;
        event.eventType = EH.MOUSE_DOWN;
        return EH.doHandleMouseDown(DOMevent, event);
    }
},

_handlePointerMove : function (DOMevent) {
    var EH = isc.EH;

    if (DOMevent.pointerType === "touch") {
        return EH._handleTouchMove(DOMevent);
    } else {
        EH.DOMevent = DOMevent;
        var event = EH.getMouseEventProperties(DOMevent);
        if (EH.eventHandledNatively(DOMevent.type, DOMevent.target)) return EH._handledNativelyReturnVal;
        event.originalType = event.eventType;
        event.eventType = EH.MOUSE_MOVE;
        return EH._handleMouseMove(DOMevent, event);
    }
},

_handlePointerUp : function (DOMevent) {
    var EH = isc.EH;

    if (DOMevent.pointerType === "touch") {
        return EH._handleTouchEnd(DOMevent);
    } else {
        EH.DOMevent = DOMevent;
        var event = EH.getMouseEventProperties(DOMevent);
        if (EH.eventHandledNatively(DOMevent.type, DOMevent.target)) return EH._handledNativelyReturnVal;
        event.originalType = event.eventType;
        event.eventType = EH.MOUSE_UP;
        return EH._handleMouseUp(DOMevent, true);
    }
},

_handlePointerCancel : function (DOMevent) {
    var EH = isc.EH;

    if (DOMevent.pointerType === "touch") {
        return EH._handleTouchCancel(DOMevent);
    } else {
        EH.DOMevent = DOMevent;
        var event = EH.getMouseEventProperties(DOMevent);
        if (EH.eventHandledNatively(DOMevent.type, DOMevent.target)) return EH._handledNativelyReturnVal;
        event.originalType = event.eventType;
        event.eventType = EH.MOUSE_UP;
        return EH._handleMouseUp(DOMevent, true);
    }
},


//>Touch

// Touch events
// ---------------------------------------------------------------------------------------

_handlingTouchEventSequence : function () {
    
        var EH = isc.EH;

    return (EH._handledTouch != null &&
            EH._handledTouch != EH._touchEventStatus.READY_FOR_TOUCH &&
            EH._handledTouch != EH._touchEventStatus.TOUCH_COMPLETE);
},


_handleTouchStart : function (DOMevent) {
	
        var EH = isc.EH;

    
    

    EH.DOMevent = DOMevent;
	var	event = EH.getMouseEventProperties(DOMevent);

    if (EH.eventHandledNatively(DOMevent.type, DOMevent.target)) return EH._handledNativelyReturnVal;

    
    delete event.touchStartReturnValue;

    // maintain touch state for synthetic mouseDown/mouseUp
    this._handledTouch = EH._touchEventStatus.TOUCH_STARTED;

    var returnValue = EH.handleEvent(event.target, EH.TOUCH_START);

    if (returnValue !== false) {
        event.originalType = EH.TOUCH_START;

        
        event.eventType = EH.MOUSE_MOVE;
        EH._handleMouseMove(DOMevent, event);

        event.eventType = EH.MOUSE_DOWN;
        returnValue = EH.doHandleMouseDown(DOMevent, event);

        // Treat the user holding the finger over an item as a "long touch" - this
        // will trip context menu behavior for mobile browsers (where right-click isn't
        // otherwise possible)
        // Could also trip hover prompt behavior at a pinch
        if (EH._longTouchTimer != null) isc.Timer.clear(EH._longTouchTimer);
        EH._longTouchTimer = this.delayCall("_handleLongTouch", [], EH.longTouchDelay);
    }
    
    return event.touchStartReturnValue;
},

// Number of ms before we fire the synthetic "longTouch" event (user holding their finger
// over a widget in a touch browser like iPad/safari)
longTouchDelay:500,
_handleLongTouch : function () {
    
        var EH = isc.EH;
    EH._longTouchTimer = null;

    // Showing a context menu and handling a drag operation are mutually exclusive. If already
    // dragging, then don't fire the long touch event (which could result in showing a context
    // menu).
    //
    // There is similar logic in __handleMouseMove() to not start a drag if there are open menus
    // on screen.
    if (EH.dragging) return;

    if (!EH.mouseIsDown() || !EH.mouseDownTarget() || !EH.stillWithinMouseDownTarget()) return;

	// send the event up the chain of the target
	EH.bubbleEvent(EH.mouseDownTarget(), EH.LONG_TOUCH);
},

_isDescendantOfNativeTouchScrollableElement : function (widget) {
    
    while (widget != null) {
        if (widget._usingNativeTouchScrolling() && (widget.hscrollOn || widget.vscrollOn)) {
            return true;
        }
        widget = widget.parentElement;
    }

    

    return false;
},

_handleTouchMove : function (DOMevent) {
	
        var EH = isc.EH;

    

    EH.DOMevent = DOMevent;
	var	event = EH.getMouseEventProperties(DOMevent);

    if (EH.eventHandledNatively(DOMevent.type, DOMevent.target)) return EH._handledNativelyReturnVal;

    var returnValue = EH.handleEvent(event.target, EH.TOUCH_MOVE);

    if (returnValue !== false) {
        event.originalType = EH.TOUCH_MOVE;
        event.eventType = EH.MOUSE_MOVE;
        EH._handleMouseMove(DOMevent, event);

        // prevent default full-page drag if we're doing an SC drag
        if (EH.dragging && window.event != null) window.event.preventDefault();
    }

    // kill "long touch" event on drag move - it's counter intuitive to have (for example)
    // both scrolling and a context menu when you drag.
    if (EH._longTouchTimer != null) {
        isc.Timer.clear(EH._longTouchTimer);
        EH._longTouchTimer = null;
    }

    
    if (isc.Browser.isChrome) {
        var targetElem = DOMevent.target && (DOMevent.target.nodeType == 1 ? DOMevent.target
                                                                           : DOMevent.target.parentElement);
        // Do not allow native touch scrolling if we're in the middle of a drag.
        if (EH.dragTarget != null) {
            DOMevent.preventDefault();
            return false;

        // <textarea>s may be natively touch-scrolled.
        
        } else if (targetElem != null && targetElem.tagName === "TEXTAREA" &&
                   (targetElem.scrollWidth > targetElem.clientWidth ||
                    targetElem.scrollHeight > targetElem.clientHeight)) {
            /*empty*/

        // Check whether this widget either is or is a descendant of a widget that is using
        // native touch scrolling and which can be scrolled.
        } else if (event.target != null) {
            var eventTarget = event.target;
            if (isc.isA.DrawItem && isc.isA.DrawItem(eventTarget)) {
                eventTarget = eventTarget.drawPane;
            }
            if (isc.isA.Canvas(eventTarget) && !EH._isDescendantOfNativeTouchScrollableElement(eventTarget)) {
                DOMevent.preventDefault();
                return false;
            }
        }
    }
},


_shouldIgnoreTargetElem : function (targetElem) {
    var EH = this;

    if (targetElem == null) return true;

    var tagName = targetElem.tagName;
    if (tagName === "INPUT") {
        var inputType = targetElem.type;

        
        if (inputType === "submit") {
            if (EH.mouseDownEvent != null &&
                EH.mouseDownEvent.DOMevent.target != null &&
                !targetElem.contains(EH.mouseDownEvent.DOMevent.target))
            {
                return true;
            }
        } else if (inputType !== "button" &&
                   inputType !== "checkbox" &&
                   inputType !== "file" &&
                   inputType !== "radio") {
            return true;
        }
    } else if (tagName === "LABEL" ||
               tagName === "SELECT" ||
               tagName === "TEXTAREA")
    {
        return true;
    }

    return false;
},


_handleTouchEnd : function (DOMevent) {
	
        var EH = isc.EH;

    

    EH.DOMevent = DOMevent;
	var	event = EH.getMouseEventProperties(DOMevent);

    if (EH.eventHandledNatively(DOMevent.type, DOMevent.target)) return EH._handledNativelyReturnVal;

    // maintain touch state for synthetic mouseDown/mouseUp
    if (EH._handledTouch == EH._touchEventStatus.TOUCH_STARTED) {
        EH._handledTouch = EH._touchEventStatus.TOUCH_ENDING;
    }

    var returnValue = EH.handleEvent(event.target, EH.TOUCH_END);
    if (returnValue !== false) {
        event.originalType = EH.TOUCH_END;
        event.eventType = EH.MOUSE_UP;
        EH._handleMouseUp(DOMevent, true);
    }

    if (EH._handledTouch == EH._touchEventStatus.TOUCH_ENDING) {
        EH._handledTouch = EH._touchEventStatus.TOUCH_COMPLETE;
    }

    if (EH._longTouchTimer != null) {
        isc.Timer.clear(EH._longTouchTimer);
        EH._longTouchTimer = null;
    }

    var targetElem = (DOMevent.target && (DOMevent.target.nodeType == 1 ? DOMevent.target
                                                                        : DOMevent.target.parentElement));
    if (!EH._shouldIgnoreTargetElem(targetElem)) {
        DOMevent.preventDefault();
        return false;
    }
},


_handleTouchCancel : function (DOMevent) {

	
        var EH = isc.EH;

    

    EH.DOMevent = DOMevent;
    var	event = EH.getMouseEventProperties(DOMevent);

    if (EH.eventHandledNatively(DOMevent.type, DOMevent.target)) return EH._handledNativelyReturnVal;

    // maintain touch state for synthetic mouseDown/mouseUp
    if (EH._handledTouch == EH._touchEventStatus.TOUCH_STARTED) {
        EH._handledTouch = EH._touchEventStatus.TOUCH_ENDING;
    }

    this.delayCall("_handleDelayedTouchCancel", [event, DOMevent]);
},

_handleDelayedTouchCancel : function (event, DOMevent) {
    
        var EH = isc.EH;
    var returnValue = EH.handleEvent(event.target, EH.TOUCH_END);
    if (returnValue !== false) {
        event.originalType = EH.TOUCH_CANCEL
        event.eventType = EH.MOUSE_UP;
        EH._handleMouseUp(DOMevent, true);
    }

    if (EH._handledTouch == EH._touchEventStatus.TOUCH_ENDING) {
        EH._handledTouch = EH._touchEventStatus.TOUCH_COMPLETE;
    }

    if (EH._longTouchTimer != null) {
        isc.Timer.clear(EH._longTouchTimer);
        EH._longTouchTimer = null;
    }
},

//<Touch

// Focus Handling
// ---------------------------------------------------------------------------------------


getFocusCanvas : function () {
    return this._focusCanvas;
},

// focusInCanvas and blurFocusCanvas
// Called as a result of an action that will focus in a canvas, such as calling widget.focus(),
// or clicking on the widget.


// helper to log native focus changes
_logFocus : function (target, isFocus) {
    if (!this.logIsDebugEnabled("nativeFocus")) return;

    this.logDebug((isFocus ? "onfocus" : "onblur") + 
                  " fired on: " + target + this._getActiveElementText(),
                  "nativeFocus");
},

_getActiveElementText : function () {
    if (!isc.Browser.isIE) return isc._emptyString;
    var activeElement = this.getActiveElement();
    
    if (activeElement == null) return isc._emptyString;
    return ", activeElement: " + ( 
                                  activeElement.tagName);
},


blurFocusCanvas : function (target, isNative) {
    //>DEBUG isNative indicates this call came from a blur handler on a native element (either
    // the handle or a focusProxy).
    var oldThread = this._thread;
    if (isNative) {
        this._setThread("BLR");
        this._logFocus(target);
        isc.EH._unconfirmedBlur = null;
    } //<DEBUG
    
    this._blurFocusCanvas(target, isNative);
    
    //>DEBUG
    if (isNative) this._thread = oldThread; //<DEBUG
},
_blurFocusCanvas : function (target, isNative) {
    //this.logWarn("_blurFocusCanvas called with focusCanvas: " + this._focusCanvas);
    if (this._focusCanvas) {
        var focusCanvas = this._focusCanvas;

        
        if (target != null && focusCanvas != target) return;
        this._focusCanvas = null;
        focusCanvas._focusChanged(false);
        
    }
},

focusInCanvas : function (target, isNative) {

    //>DEBUG isNative indicates this call came from a focus handler on a native element (either
    // the handle or a focusProxy).
    var oldThread = this._thread;
    if (isNative) {
        this._setThread("FCS");
        this._logFocus(target, true);
        isc.EH._unconfirmedFocus = null;
    } //<DEBUG
    
    // In Moz by default focus outline shows up on both mouseDown and focus via tab-keypress
    // we only want it to show up via keypress (like in IE).
    // use setShowFocusOutline to update the css setting to hide the focus outline if this focus
    // came from a mouseDown.
    if (isc.Browser.isMoz) {        
        if (isNative && (this.lastEvent.eventType != this.KEY_DOWN && 
                         this.lastEvent.eventType != this.KEY_PRESS && 
                         this.lastEvent.eventType != this.KEY_UP)) 
        {
            if (target && target.showFocusOutline) target.setShowFocusOutline(false,true);
        } else {
            if (target && target.showFocusOutline) target.setShowFocusOutline(true,true);
        }
    }
    this._focusInCanvas(target, isNative);

    //>DEBUG
    if (isNative) this._thread = oldThread; //<DEBUG
},
_focusInCanvas : function (target, isNative) {
    //this.logWarn("_focusInCanvas. Will set this._focusCanvas to target:" 
    //             + target + ", current focus target:" + this._focusCanvas);
    // if no target, or target doesn't want focus, or target has focus already just bail

    if (!target || target.hasFocus || !target._canFocus() || target.isDisabled() ) return;
    // Bail if this._focusCanvas is already pointing to the target.
    // Normally we'd expect target.hasFocus to be set in this case but if the _focusChanged
    // method on either the previous focus target or the new one forces a focus change, that
    // can occur before target.hasFocus gets modified.
    if (this._focusCanvas == target) return;
    
    // Handle the case of focus going to a masked widget
    this.checkMaskedFocus(target);
    // If the target is masked, update the topmask's 'maskedFocusCanvas'
    
    if (this.targetIsMasked(target) && !target._ignoreClickMaskFocus) {
        var topMask = this.clickMaskRegistry.last();
        this.setMaskedFocusCanvas(target, topMask);
    }
   
    
    if (isNative && isc.Browser.isMoz) {
        if (target.parentElement) target.parentElement._handleCSSScroll(null, true);
    }
    
    // blur the previous focus item, and focus in this one
    var blurCanvas = this._focusCanvas;
    this._focusCanvas = target;
    if (blurCanvas) blurCanvas._focusChanged(false)
    
    // if the blur handler forced focus into *another* widget, bail - we don't want to
    // fire _focusChanged on a stale widget - that'd cause hasFocus to be set on that widget
    // and be essentially stale
    if (this._focusCanvas != target) return;
    target._focusChanged(true);
    

    
},

// Helper to set (or clear) the 'masked focus canvas' for some mask.
setMaskedFocusCanvas : function (target, mask) {
    if (!mask) return;
    mask._maskedFocusCanvas = target;
},

getMaskedFocusCanvas : function (mask) {
    if (mask == null) mask = this.clickMaskRegistry.last();
    else mask = this.getClickMask(mask);
    if (mask) return mask._maskedFocusCanvas;
},

// fired when focus goes onto a widget which may be covered by a clickmask.
// if the mask is soft, it automatically dismisses that clickMask and fires the click
// action.
checkMaskedFocus : function (target) {
    
    
    if (isc.Browser.isIE || isc.Browser.seleniumPresent) {
 
        var activeElement = this.getActiveElement();
        var handle = target ? target.getHandle() : null;   
        // no handle - undrawn so bail
        if (!handle) return;
        var focusStillInCanvas;
        
        while (activeElement && activeElement.tagName) {
        
            if (activeElement == handle) {
                focusStillInCanvas = true;
                break;
            }
            // if "eventProxy" is specified look at that and bail early - it either is or isn't
            // the widget that fired the onfocus event.
            if (activeElement.eventProxy) {
                focusStillInCanvas = (activeElement.eventProxy == target.getID());
                break;
            }
            activeElement = activeElement.parentElement;
        }
        if (!focusStillInCanvas) return;
    }
    
    
    if (isc.Browser.isMobileWebkit) {
        if (isc.EH.isMouseEvent(isc.EH.lastEvent.eventType) && 
            (isc.EH.mouseDownTarget() == target))
        {
            return;
        }
    }
    
    var masks = this.clickMaskRegistry;
    for (var i = masks.length-1; i >= 0; i--) {
        var mask = masks[i];
        // As soon as we hit a mask we know we're ABOVE we know we're above all remaining
        // masks so we can bail.
        if (!this.targetIsMasked(target, mask)) return;
        else {
            // we should never be able to focus on a widget under an hard 
            // clickmask - just return false if we do hit this case
            if (this.isHardMask(mask)) return false;
            this._clickMaskClick(mask);
        }
    }
},


// prepare for a drag interaction on "target" (a Canvas)
prepareForDragging : function (target) {
	var EH = this;

    
    if (EH.dragging) EH.handleDragStop();

	delete EH.dragMoveAction;
	delete EH.dragTarget;

    // send prepareForDragging event: target is expected to set various dragging-related flag,
    // especially dragTarget, if it wants to be dragged.  See default implementation in Canvas.
    EH.bubbleEvent(target, "prepareForDragging");
    // no one set a drag target
    if (!EH.dragTarget) {
        if (this.logIsDebugEnabled("dragDrop")) this.logDebug("No dragTarget, not dragging", "dragDrop");
        return;
    }
    
    //>DEBUG
    if (this.logIsInfoEnabled("dragDrop"))
        this.logInfo("target is draggable with dragOperation: " + EH.dragOperation + 
                     ", dragTarget is : " + EH.dragTarget + 
                     (EH.dragTarget != target ? " (delegated from: " + target + ")" : ""), 
                     "dragDrop");
    //<DEBUG

    // Remember the original rect of the dragTarget in case we need it later.  It is used,
    // for example, to implement proportional resizing in Canvas.resizeToEvent().
    EH.dragTargetStartRect = EH.dragTarget.getRect();
},

// Handle the artificially generated dragStart event, sent to a draggable object when the mouse
// goes down in the object and then moves the dragStartDistance while still down.
// 
// Fires Canvas.dragStart() on the target (which may cancel the event) and sets up the visual
// appearance of dragging.
handleDragStart : function (nativeDragging) {
	var EH = this,
		event = EH.lastEvent;

	if (!EH.mouseIsDown() || !EH.dragTarget) return false;

    
	delete EH.dropTarget;
	delete EH.dragMoveTarget;
	// reset the dragOffsetX and dragOffsetY in case somebody set it before
    // these properties indicate the offset of the last event coordinates (typically this is
    // the position of the mouse) from the drag target
	EH.dragOffsetX = -10;
	EH.dragOffsetY = -10;
	
	// during dragging no Canvii will get mouseOver/mouseOut, so we need to send a final mouse
    // out event to avoid a Canvas getting stuck in the "over" state.  
    EH.handleEvent(EH.lastMoveTarget, EH.MOUSE_OUT);
    
    // likewise we need to send mouseOut to the mouseDownTarget to avoid it getting stuck in
    // the down state.  
    
    if (EH.lastMoveTarget != EH.mouseDownTarget()) {
        EH.handleEvent(EH.mouseDownTarget(), EH.MOUSE_OUT);
    }
    
    // If we're currently showing a hover, hide it.
    // We're not checking for the lastHoverCanvas matching the drag target etc since dragging could
    // be delegated from one drag target to another and there are no obvious cases where we'd
    // want the hover to be visible during dragging
    if (isc.Hover) isc.Hover.clear();

    // remember the drag offset; this is the distance between the point where the mouse went
    // down to start D&D and the top/left corner of the element being dragged.  We want to
    // maintain that offset during dragging, otherwise, dragRepositioned objects would have
    // their top-left corner snap to the mouse position when dragging began, and dragResized
    // objects would jerk by the dragOffset when dragging starts.
    // NOTE: capture offset before dragStart since dragStart might deparent.
    EH.dragStartOffsetX = EH.mouseDownEvent.x - EH.dragTarget.getPageLeft();
    EH.dragStartOffsetY = EH.mouseDownEvent.y - EH.dragTarget.getPageTop();

	// bubble the appropriate [dragStart, dragRepositionStart, or dragResizeStart] message to
    // the target.  This is an opportunity to set EH.dragAppearance
	var eventType = EH.dragOperation + "Start";
	
	if (EH.handleEvent(EH.dragTarget, eventType) == false) {
        //>DEBUG
        this.logInfo("drag cancelled by false return from: " + 
                      eventType + " on " + EH.dragTarget,
                      "dragDrop");
        //<DEBUG

		// if it returns false, cancel dragging
		delete EH.dragTarget;
		delete EH.dragTargetLink;

		// send the drag object a mouseOver since we sent it mouse out when dragging began
		EH.handleEvent(EH.dragTarget, EH.MOUSE_OVER);

		return false;
	}

	// clear the lastMoveTarget since we've sent a mouseOut with no mouseOver
	delete EH.lastMoveTarget;
	

	// if we're dragRepositioning, and the dragAppearance is not the tracker,
	// set the isc.EventHandler.dragOffsetX and .dragOffsetY to the offset from the drag target
	var dragAppearance = EH.dragTarget.getDragAppearance(EH.dragOperation);
	if (dragAppearance != EH.TRACKER)
    {
		EH.dragOffsetX = EH.dragStartOffsetX;
		EH.dragOffsetY = EH.dragStartOffsetY;
	}
	
    // dragAppearance
    // --------------
	// We set the action to be performed on mouseMove events during dragging, based on the
    // dragTarget's dragAppearance.  For all types of dragging, we have:
    // - a "dragMoveTarget", which is a Canvas, and is chosen on the basis of dragAppearance.
    //   It can be the dragTarget itself ("target" dragAppearance) or some other object which
    //   stands in, like an outline or small "tracker".
    // - a "dragMoveAction", which is a function called every mouseMove.  The two built-in
    //   dragOperations, dragResizing and dragRepositioning, work by setting the dragMoveAction
    //   to a function that drag resizes or drag repositions the dragMoveTarget.
    //
    // Note that the dragAppearance is independant of whether you are doing dragResizing,
    // dragRepositioning, or a custom drag.  If you set a dragAppearance and do a generic drag
    // (that is, set canDrag but not canDragReposition or canDragResize), then we default the
    // dragMoveAction to moving the dragMoveTarget around; this is often used to show a
    // tracker.
    if (EH.dragOperation == EH.DRAG_SCROLL ||
        nativeDragging)
    {
        EH.dragAppearance = EH.NONE;
    } else {
        EH.dragAppearance = EH.dragTarget.getDragAppearance(EH.dragOperation);
    }

	// "tracker" drag appearance
    // - a small Canvas, settable via setDragTracker, "tracks" the mouse cursor at a small offset
	//
	if (EH.dragAppearance == EH.TRACKER) {
		// using the drag tracker
		EH.dragMoveTarget = EH._makeDragTracker();
        // tracker moves to event
		if (!EH.dragMoveAction) EH.dragMoveAction = EH._moveDragMoveTarget;

		// if the target wants it, tell it to set the dragTracker image
		EH.dragTracker.setOverflow(isc.Canvas.VISIBLE);
		EH.bubbleEvent(EH.dragTarget, EH.SET_DRAG_TRACKER);

		// change the offset for the tracker -- default -10,-10 in dragTrackerDefaults, but
        // customizable in setDragTracker()
		EH.dragOffsetX = EH.dragTracker.offsetX;
		EH.dragOffsetY = EH.dragTracker.offsetY;

	// "outline" drag appearance
    //
	} else if (EH.dragAppearance == EH.OUTLINE) {
        EH.dragMoveTarget = EH.getDragOutline(EH.dragTarget);

        // on drag move, we'll move to the mouse
		if (!EH.dragMoveAction) EH.dragMoveAction = EH._moveDragMoveTarget;

	// "target" drag appearance
	//
	} else if (EH.dragAppearance == EH.TARGET) {
		EH.dragMoveTarget = EH.dragTarget;
		if (!EH.dragMoveAction) EH.dragMoveAction = EH._moveDragMoveTarget;
        
        // If the canvas wants to show a shadow on drag, show it now.
        if (EH.dragTarget.showDragShadow) this._showTargetDragShadow();
		
        // If the canvas should change opacity on drag, handle this now.
        if (EH.dragTarget.dragOpacity != null) this._setTargetDragOpacity();
	// custom move style
	//
	} else {
		// nothing special to do here -- your target should set things up manually in its
		// .dragStart() handler.  It can set EH.dragMoveTarget and/or EH.dragMoveAction if
        // desired.
	}

	// if a dragMoveTarget is set, bring it to the front and show it
	if (EH.dragMoveTarget) {
		// make sure dragMoveTarget and dragTarget agree on some basic stuff
		if (EH.dragMoveTarget != EH.dragTarget) {
			EH.dragMoveTarget.dragIntersectStyle = EH.dragTarget.dragIntersectStyle;

		}
        // show and bring to front
        EH.dragMoveTarget.show();
        EH.dragMoveTarget.bringToFront();
	}


    //>PluginBridges
    
    var backmaskTarget = EH.dragMoveTarget ? EH.dragMoveTarget : EH.dragTarget;
    if ((isc.Browser.isIE || isc.Browser.isMoz) && EH.dragAppearance != EH.OUTLINE
        // already backmasked or should never be backMasked
        && !(backmaskTarget._isBackMask || backmaskTarget.neverBackMask)) 
    {
        if (EH.alwaysBackMask) {
            this._showBackMask(backmaskTarget);
        } else {            
            var burnThroughElements = [];

            
            if (isc.BrowserPlugin) {
                var pluginInstances = isc.BrowserPlugin.instances;
                for (var i = 0; i < pluginInstances.length; i++) {
                    var pluginInstance = pluginInstances[i];
                    if (pluginInstance.isVisible()
                        && (backmaskTarget.parentElement == null    
                            || backmaskTarget.parentElement.contains(pluginInstance, true)))
                    {
                        burnThroughElements.add({instance: pluginInstance, 
                                                 rect: pluginInstance.getPageRect()});
                    }
                }
            }
    
            
            if (isc.Browser.isIE && isc.Browser.minorVersion >= 5.5 && isc.NativeSelectItem) {
                var selectItems = isc.NativeSelectItem.instances;
                for (var i = 0; i < selectItems.length; i++) {
                    var selectItem = selectItems[i];
                    if (selectItem.isVisible() 
                        && (backmaskTarget.parentElement == null 
                            || backmaskTarget.parentElement.contains(selectItem.containerWidget, true)))
                    {
                        burnThroughElements.add({instance: selectItem, rect: selectItem.getPageRect()});
                    }
                }
            }

            // if dynamicBackMask is false and we have elements that we could intersect with that
            // require backMasking, just show the backMask
            if(burnThroughElements.length > 0 && EH.dynamicBackMask === false) 
            {
                this._showBackMask(backmaskTarget);
            } else {
                // set up the cache so that handleDragMove can do fast intersect tests
                EH._burnThroughElementsCache = burnThroughElements;
            }
        }
    }
    //<PluginBridges

    
	EH.showEventMasks((EH.dragOperation == EH.DRAG_RESIZE));

	// set the 'dragging' flag to true since we're dragging
	EH.dragging = true;
    EH.nativeDragging = !!nativeDragging;
    EH.crossFrameDragging = false;

    if (EH.nativeDragging && EH.delayedMouseMoveTimer != null) {
        isc.Timer.clear(EH.delayedMouseMoveTimer);
        EH.delayedMouseMoveTimer = null;
    }

    //>DEBUG
    this.logInfo("Started dragOperation: " + EH.dragOperation + 
                  " with dragTarget: " + EH.dragTarget +
                  " dragAppearance: " + EH.dragAppearance, "dragDrop");
    //<DEBUG

	return true;
},

// Methods to show/hide drag shadows for targets.

_showTargetDragShadow : function () {
    var EH = isc.EH;
    var target = EH.dragTarget;

    EH._hideShadowAfterDrag = (!target.showShadow);
    EH._afterDragShadowDepth = target.shadowDepth;
    
    target.shadowDepth = EH.dragTargetShadowDepth;
    target.updateShadow();
    if (!target.showShadow) target.setShowShadow(true);
},

_hideTargetDragShadow : function () {
    var EH = isc.EH;
    var target = EH.dragTarget;

    if (EH._hideShadowAfterDrag) target.setShowShadow(false);
    target.shadowDepth = EH._afterDragShadowDepth;
    target.updateShadow();
    
    delete EH._hideShadowAfterDrag;
    delete EH._afterDragShadowDepth;
},

// Methods to modify the opacity of the drag target
_setTargetDragOpacity : function () {
    var EH = isc.EH;
    var target = EH.dragTarget;
    
    // Remember the old opacity 
    
    EH._dragTargetOpacity = target.opacity;
    
    target.setOpacity(target.dragOpacity);
},

_resetTargetDragOpacity : function () {
    var EH = isc.EH,
        target = EH.dragTarget;
    target.setOpacity(EH._dragTragetOpacity);
},


_showBackMask : function (target) {
    if (target._backMask) {
        if (!target._backMask.isVisible()) target._backMask.show();
    } else {
        target.makeBackMask({_eventHandlerDragMoveMask: true});
    }
},

_hideBackMask : function (target) {
    if(target._backMask && target._backMask._eventHandlerDragMoveMask
       && target._backMask.isVisible()) 
    {
        target._backMask.hide();  
    }
},


_getDragMoveComponents : function () {
    var dmt = this.dragMoveTarget;
    if (!dmt) return;
    var components = [dmt];
    if (dmt._backMask) components.add(dmt._backMask);
    if (dmt._edgedCanvas) components.add(dmt._edgedCanvas);
    if (dmt._shadow) components.add(dmt._shadow);
    return components;
},

// Helper to speed up the 'dragRepositionMove' et al name assembly
_dragMoveEventNames:{},
_getDragMoveEventName : function (dragOperation) {
    var eventNames = this._dragMoveEventNames;
    if (!eventNames[dragOperation]) {
        eventNames[dragOperation] = dragOperation + "Move";
    }
    return eventNames[dragOperation];
},

// Deliver the synthetic dragMove event, also handling the dragAppearance
handleDragMove : function () {
	var EH = this,
		event = EH.lastEvent;

	// get the drop target, if there is one
    isc._useBoxShortcut = true;
	EH.dropTarget = EH.getDropTarget(event);
    isc._useBoxShortcut = false;
    
    //>PluginBridges
    

    if (EH._burnThroughElementsCache && EH.dynamicBackMask) {
        var showBackMask = false;    
        var backmaskTarget = EH.dragMoveTarget ? EH.dragMoveTarget : EH.dragTarget;
        var backmaskTargetRect = backmaskTarget.getRect();

        for (var i = 0; i < EH._burnThroughElementsCache.length; i++) {
            var burnThroughElement = EH._burnThroughElementsCache[i];
            if (isc.Canvas.rectsIntersect(burnThroughElement.rect, backmaskTargetRect)) 
            {
                EH._lastBurnThroughElement = burnThroughElement.instance;
                showBackMask = true;
                break;
            }
        }

        // Applet/IFrame layering workaround - see doc in Applet.repaintIfRequired()
        if (EH._lastBurnThroughElement) {
            if (EH._lastBurnThroughElement.repaintIfRequired) EH._lastBurnThroughElement.repaintIfRequired();
        }

        if (showBackMask) {
            this._showBackMask(backmaskTarget);
        } else {
            this._hideBackMask(backmaskTarget);
            delete EH._lastBurnThroughElement;
        }
    } else if (isc.BrowserPlugin) {
        isc.BrowserPlugin.handleDragMoveNotify();
    }
    //<PluginBridges

	// if a dragMoveAction has been set, call it now
	if (EH.dragMoveAction) EH.dragMoveAction();

	// send the appropriate [dragMove, dragResizeMove, dragRepositionMove] event to the
    // dragTarget and bail if it returns false.  This is another way for your handler to stop
	// drop processing if you don't like what you're about to be dropped over.
    // NOTE: this cancels drop events being sent to the candidate drop target, not the
    // dragMoveAction, which eg moves the dragTracker with the mouse.
	if (EH.handleEvent(EH.dragTarget, this._getDragMoveEventName(EH.dragOperation)) == false) {
		delete EH.dropTarget;
		return false;
	}
	
	// if the dropTarget is not the last drop target that got a mouse event
	//	send the dropOver and dropOut routines to the targets
	if (EH.dropTarget != EH.lastDropTarget) {

        //>DEBUG
        this.logDebug("New drop target: " + EH.dropTarget, "dragDrop");
        //<DEBUG

		// send the mouseOut event to the last mover object
		if (EH.lastDropTarget) {
			EH.handleEvent(EH.lastDropTarget, EH.DROP_OUT);
		}

		// send the mouseOver event to the target
		if (EH.dropTarget) {
			EH.handleEvent(EH.dropTarget, EH.DROP_OVER);
		}
		
		// remember that we're the last move object
		EH.lastDropTarget = EH.dropTarget;
	}

	// call dropMove on the dropTarget
	if (EH.dropTarget) {
		EH.handleEvent(EH.dropTarget, EH.DROP_MOVE);
	}

    isc._useBoxShortcut = true;
    this._handleDragScroll();
    isc._useBoxShortcut = false;

    
    if (this.dragOperation == EH.DRAG_SELECT) return true;
    return false;
},

// Automatic scroll on drag: if we're dragging near the edge of a scrollable widget, scroll it
// in the appropriate direction.

_handleDragScroll : function () {
    var EH = this,
        dragTarget = EH.dragTarget;
    
    // don't use automatic drag scrolling at edges if the drag operation itself is scrolling
    // (the two behaviors would fight)
    if (EH.dragOperation == EH.DRAG_SCROLL) return;
    
    // Determine which widget would be scrolled (assuming we're over the right place)
    
    var scrollCandidates = [];
    // If this is a drag-select, we only ever want to scroll the widget itself or its
    // parent-list.
    // Otherwise respect dragScrollType
    var canvasList = (EH.dragOperation == EH.DRAG_SELECT) || 
                        dragTarget.dragScrollType == "parentsOnly" ?
                        dragTarget.getParentElements() : isc.Canvas._canvasList;
    ;
    if (EH.dragOperation == EH.DRAG_SELECT) canvasList.addAt(dragTarget, 0);

    // shortcut - if there are no valid scroll candidates, just bail
    // True for top level widgets with dragScrollType set to parentsOnly
    if (canvasList == null || canvasList.length == 0) return;
    
    for (var i = 0; i < canvasList.length; i++) {
        if (canvasList[i].isDrawn() && canvasList[i].isVisible() && 
            canvasList[i].shouldDragScroll()
        ) {
            scrollCandidates.add(canvasList[i]);
        }
    }
        
    var event = EH.lastEvent,
        eX = event.x, eY = event.y,
        matches = [];

    //this.logWarn("have dragScroll candidates: " + scrollCandidates);

    for (var i = 0; i < scrollCandidates.length; i++) {
        
        if (scrollCandidates[i].visibleAtPoint(eX, eY, false, EH._getDragMoveComponents()))
            matches.add(scrollCandidates[i]);
    }
        
    // If we end up with more than one scroll candidate, one must be an ancestor of the 
    // other [as visibleAtPoint() will not return true for a widget covered by another widget]
    // Check each of these for whether we're over the scroll area of the widget,
    // and remove from the list of candidates if not
        
    if (matches.length > 0) {
        var scrollWidget;
        for (var i = 0; i < matches.length; i++) {
            // dragScrollDirection used to limit dragScrolling of ancestors to either
            // horizontal or vertical
            // This property is set on certain widgets such as the scrollbar thumb where
            // scrolling the parent in response to dragging only makes sense on one axis
            if (matches[i]._overDragThreshold(dragTarget.dragScrollDirection)) {
                if (scrollWidget == null || scrollWidget.contains(matches[i], true)) 
                    scrollWidget = matches[i];
            }
        }
        if (scrollWidget != null) {
            scrollWidget._setupDragScroll(
                dragTarget.dragScrollDirection, 
                // scroll on drag select has some additional logic in Canvas.js to
                // avoid scrolling a parent too far
                EH.dragOperation == EH.DRAG_SELECT);
        }
    }
},


// Deliver the artificially generated 'dragStop' event when the mouse goes up at the end of a
// drag
handleDragStop : function () {

	var EH = this,
		event = EH.lastEvent,
        successfulDrag = false;

    EH.wasNativeDragging = EH.nativeDragging;

	// note that we're no longer dragging
    EH.crossFrameDragging = EH.nativeDragging = EH.dragging = false;

    //>DEBUG
    this.logInfo("end of drag interaction", "dragDrop");
    //<DEBUG

    // reset the drag offsets
	EH.dragOffsetX = EH.dragOffsetY = 0;

    var dragTarget = EH.dragTarget,
        dragMoveTarget = EH.dragMoveTarget,
        dragOperation = EH.dragOperation;

	// if the dragMoveTarget is the tracker or outline, hide it
	if (dragMoveTarget && 
        (dragMoveTarget == EH.dragTracker || dragMoveTarget == EH.dragOutline)) 
    {
		dragMoveTarget.hide();
        
	} else {
        // If we're showing a drag-shadow, hide (or shrink) it
        if (dragTarget.showDragShadow) EH._hideTargetDragShadow();
        
        // If the target has a special drag-opacity, revert to orginal opacity
        // If we're showing a drag-shadow, hide (or shrink) it
        if (dragTarget.dragOpacity != null) EH._resetTargetDragOpacity();
        
    }
    
    // if the dragTracker was customized via setDragTracker(), destroy it now and re-create
    // next time we need it.
    if (this.dragTracker && this.dragTracker._isCustomized) {
        this.dragTracker.destroy();
        delete this.dragTracker;
    }

    //>PluginBridges
    var backmaskTarget = EH.dragMoveTarget ? EH.dragMoveTarget : EH.dragTarget;
    this._hideBackMask(backmaskTarget);

    if (EH._burnThroughElementsCache) delete EH._burnThroughElementsCache;
    //<PluginBridges

	// if there is a dropTarget, 
    var dropTarget = EH.dropTarget;
	if (dropTarget) {
		// send it the 'dropOut' event so it can reset its visible state
		EH.handleEvent(EH.dropTarget, EH.DROP_OUT);

        EH.handleEvent(dropTarget, EH.DROP);
        
        successfulDrag = true;
    }

    // determine if we were dragging something other than the dragTarget (eg an outline)
    var wasDraggingTarget = (dragTarget == dragMoveTarget);

	// send the dragTarget the [ dragStop | dragRepositionStop | dragResizeStop ] event so it can
    // reset its visual state
	if (EH.handleEvent(dragTarget, dragOperation+"Stop") != false) {

        successfulDrag = true;
        
        if (dragOperation == EH.DRAG_RESIZE) {
 
            if (!wasDraggingTarget) {
                // if we're dragging a tracker, don't resize to fit it, rely on the event coords instead

                if (dragMoveTarget != null && this.dragAppearance != this.TRACKER) {

                    // resize the dragTarget to the size of the dragMoveTarget
                    dragTarget.setPageRect(  dragMoveTarget.getPageLeft(),
                                             dragMoveTarget.getPageTop(),
                                             dragMoveTarget.getWidth(),
                                             dragMoveTarget.getHeight(),
                                             true   );
                // drag appearance "none"
                } else {
                    var resizeEdge = isc.EH.resizeEdge;
                    if (resizeEdge != null) {
                        var X = isc.EH.getX(),
                            Y = isc.EH.getY(),
                            lOffset = resizeEdge.contains("L") ? X- EH.dragTargetStartRect[0] : 0,
                            tOffset = resizeEdge.contains("T") ? Y- EH.dragTargetStartRect[1] : 0;
                            
                        // One of "L", "R", "T", "B", "LR", etc
                        dragTarget.setPageRect(
                            resizeEdge.contains("L") ? X : EH.dragTargetStartRect[0],
                            resizeEdge.contains("T") ? Y : EH.dragTargetStartRect[1],
                            resizeEdge.contains("R") ? X - dragTarget.getPageLeft() 
                                : EH.dragTargetStartRect[2] - lOffset,
                            resizeEdge.contains("B") ? isc.EH.getY()-dragTarget.getPageTop()
                                :  EH.dragTargetStartRect[3] - tOffset,
                            true
                        );
                    }
                }
            }
            
            var deltaX = dragTarget.getVisibleWidth() - EH.dragTargetStartRect[2],
                deltaY = dragTarget.getVisibleHeight() - EH.dragTargetStartRect[3];

            // fire 'dragResized()' one-time event to indicate we're done drag-resizing
            
            dragTarget.dragResized(deltaX, deltaY);

        // otherwise if a reposition operation
        } else if (dragOperation == EH.DRAG_REPOSITION) {
            if (!wasDraggingTarget) {
                if (dragMoveTarget != null) {
                    // move the target if we were moving a different drag-move target.
                    dragTarget.setPageRect(  dragMoveTarget.getPageLeft(),
                                             dragMoveTarget.getPageTop()  );
                } else {
                    dragTarget.setPageRect(isc.EH.getX(), isc.EH.getY());
                }
				// and bring it to the front
				dragTarget.bringToFront();
			}
            
            // Fire 'dragRepositioned' to indicate we have drag-moved the widget
            EH.dragTarget.dragRepositioned();
		}

	// drag[Reposition|Resize]Stop returned explicit "false", indicating cancel -
	// if we were moving the dragTarget, put it back the way we found it
	} else {
		if (dragOperation == EH.DRAG_RESIZE) {
			// if we were actually resizing the original target
			if (wasDraggingTarget) {
				// set its rect back to its original rect
				dragTarget.setRect(EH.dragTargetStartRect);
			}
		} else if (EH.dragOperation == EH.DRAG_REPOSITION) {
			// if we were actually moving the original target
			if (wasDraggingTarget) {
				// set its location back to its original location
				dragTarget.moveTo(EH.dragTargetStartRect[0],EH.dragTargetStartRect[1]);
			}			
		}
        // Don't fire the one-time resized() / repositioned() events in this case.
	}
	
	// clear all drag properties (they may have been set by event handlers)
	EH.clearDragProperties();

	// 030801 jmd: hide eventMasks (which avoided swallowing of drag events in iframes)
	EH.hideEventMasks();

	// send the object under the mouse a 'mouseOver' event and make it the lastMoveTarget
    // In DOM browsers and IE4, mouseUp is sent to the object under the mouse, so we can
    // send mouseOver to the event target.
    // But, don't do this if handling the end of a touch event sequence because the overTarget
    // might be left thinking that the mouse is over it for a while. For example, if the user
    // drag-rearranges the header buttons of a ListGrid on a touch-enabled device, the user just
    // lifted his or her finger off of the display, so the header button might be shown in the
    // "Over" state for a while while EH waits for a touchstart event that might not ever come.
    if (EH._handledTouch != EH._touchEventStatus.TOUCH_ENDING) {
        var overTarget = EH.lastEvent.target;
        if (overTarget) EH.handleEvent(overTarget, EH.MOUSE_OVER);
        EH.lastMoveTarget = overTarget;
    }

	// return whether the drag was successful
	return successfulDrag;
},

// Return the Canvas that is the intended recipient of this event.
//
// This maps events from the physical DOM into our logical Canvas event space,
// allowing us to process events in canvases easily.
//
// If the targeted canvas has an 'eventProxy', that will receive the event instead.
//			
//		@param	DOMevent		(DOM event) 		DOM event object (as passed by isc.EventHandler)
//		@param	target			(DOM element) 		Native DOM element that got the event (default is DOMevent.target|srcElement).
//
//		@return				(Canvas || DOM object)	Canvas or DOM object that is event recipient
_$BODY:"BODY", _$HTML:"HTML",
_$eventProxyAttributeName:"eventProxy",
_$eventPartAttributeName:"eventpart",
getEventTargetCanvas : function (DOMevent, target, scEvent) {
    
    // DOMevent may be null if this method is being used to just
    // locate a canvas from a DOM element explicitly.
    
    if (DOMevent == null) DOMevent = {};
    
    var EH = this,
        wd = this.getWindow();

    

    if (!target) target = (isc.Browser.isIE ? DOMevent.srcElement : DOMevent.target);
    //this.logWarn("native target:"+ (target ? (target.tagName + ", " + target.id) : " null"));

    
    if (!EH._canAccessNativeTargetProperties(target)) {
        //this.logWarn("TextNode exception: had to return last target " + EH.lastTarget);    
        return EH.lastTarget;
    }

    if (DOMevent && DOMevent._isSynthetic) return DOMevent.target;

    // optimization: if there is no target or the target is the body tag (so there is no
    // canvas target), bail immediately.  This means we don't burn CPU time while the mouse
    // is moving around outside all Canvas's
    if (!target || target.tagName == this._$BODY || target.tagName == this._$HTML) {
        //this.logWarn("event targetted at body");
        return (EH.lastTarget = null);
    }

    if (target && target.tagName && target.tagName == this._$applet) {
        var appletID = isc.Applet ? isc.Applet.idForName(target.name) : null;
        return appletID ? window[appletID] : EH.lastTarget;
    }
   
    
    if (isc.Browser.isIE && target.parentElement == null) {
//         this.logWarn("Defaulting to last target because target: " + Log.echoLeaf(target) + 
//                      " has null parentElement");
        target = EH.lastTarget;
    } else {
        var eventProxyAttributeName = this._$eventProxyAttributeName,
            eventPartAttr = this._$eventPartAttributeName,
            eventPartElement = null,
            eventPart = null,
            eventPartID = null;

        // follow the DOM parent chain to find the nearest containing Element which has an
        // eventProxy attribute - the eventProxy attribute is the global ID of a Canvas
        // which will handle the event.
        // NOTE: all Canvii write out an eventProxy on the DIV that contains all
        // their content.  This is normal way events are routed, however, you can put an
        // "eventProxy" attribute in any HTML element to have its events handled by a Canvas.
        //var lookupChain = [];
        
        if (isc.Browser.isIE && !isc.Browser.isIE9) {
            // in IE, any attribute written in HTML is available as a property on the
            // HTMLElement object
            while (target != null) {
                //lookupChain.add(target);
                
                if (scEvent != null && eventPart == null && target[eventPartAttr] != null) {
                    eventPartElement = target;
                    eventPart = target.eventpart;
                    // Store the whole ID - no need for the performance hit of chopping
                    // off the leading widgetID_partName_ stuff unless this is
                    // actually required (let Canvas handle that).
                    eventPartID = target.id;
                }

                // stop if we've found the eventProxy attribute
                if (target.eventProxy) break;

                // otherwise continue to parent
                target = target.parentElement;
            }

            
            if (eventPartID == isc.emptyString) eventPartID = null;
        } else {
            // in DOM browsers, you have to use DOM methods like hasAttribute/getAttribute.
            while (target != null) {
                //lookupChain.add(target);
                var hasAttr = (target.hasAttribute != null);
                if (scEvent != null && eventPart == null && 
                    (target.eventpart != null ||
                    (hasAttr && target.hasAttribute(eventPartAttr))) )
                {
                    eventPartElement = target;
                    eventPart = target.getAttribute(eventPartAttr);
                    eventPartID = target.getAttribute("id");
                }

                // stop if we've found the eventProxy attribute
                if (target.eventProxy != null ||
                    (target.hasAttribute != null && 
                     target.hasAttribute(eventProxyAttributeName))) break;

                // otherwise continue to parent
                target = target.parentNode;
            }
        }

        // If we found an eventPart, hang onto it now.
        // Otherwise clear the stored eventPart info.
        
        if (scEvent && target != null) {
            scEvent.eventPart = eventPart;
            scEvent.eventPartElement = eventPartElement;
            scEvent.eventPartID = eventPartID;
        }

        

        // we followed the parent chain until it ended without finding an eventProxy
        // attribute, so there's no Canvas to handle this event
        if (!target) return (EH.lastTarget = null);

        // "target" is the first DOM element in the parent chain with an eventProxy
        // attribute.  It's eventProxy attribute is a String that is the global ID of a
        // Canvas.  convert target to a Canvas
        target = wd[target.getAttribute(eventProxyAttributeName)];

        // Canvii can use the eventProxy attribute to delegate events to other Canvii
        while (target && target.eventProxy) {
            //if (DOMevent.type != "mousemove") {
            //    this.logWarn("Canvas: " + target + " delegates to: " + target.eventProxy);
            //}
               
            if (isc.isA.String(target.eventProxy)) {
                // if eventProxy is the string ID of a Canvas, convert it to a pointer to the
                // other Canvas, and store the looked-up Canvas.
                target.eventProxy = wd[target.eventProxy];
            }
            target = target.eventProxy;
        }

        // now we have the final target of the event
        
        if (this.logIsInfoEnabled() && !DOMevent || 
            (DOMevent.type != "mousemove" && DOMevent.type != "selectstart")) 
        {
            if (target != null) {
                this.logInfo("Target Canvas for event '" + DOMevent.type + "': " + target);
            } else {
                this.logDebug("No target Canvas for event '" + DOMevent.type + "'");
            }
        }

        // if the event is in the drag tracker, send it to the last known object instead
        if ( target == EH.dragTracker ) {
           target = EH.lastTarget;
        }

        // remember the object for later, in case we get a weird event where we can't
        // figure out the target
        EH.lastTarget = target;
    }

    // if the target is a canvas, return that
    if (isc.isA.Canvas(target)) {
        if (scEvent && target.getEventTarget) {
            target = target.getEventTarget(scEvent);
        }
        return target;
    }

    // return null since no canvas target was found
    return null;
},

_$textObjectString:"[object Text]",
_canAccessNativeTargetProperties : function(target) {
    

    // Optimization - if we're not in Moz, looking at a text node, assume we're ok
    try {
        if (!(isc.Browser.isMoz && target == this._$textObjectString)) return true; 
        target.parentNode;
    } catch (e) {
        return false;
    }
    return true;
   
},



//>	@classMethod	isc.EventHandler.getDropTarget()	(A)
//	    Return the first object that's registered that it's interested in drops that is under
//	    the mouse.<br>
//      This method does not check the result of the dynamic 'willAcceptDrop()' method, so it's
//      possible that the canvas returned by this method will not actually accept drop from the
//      dragged widget.
//
//		@group	dragDrop
//		@param	event		(SC event)  Event object, as returned from EH.getMouseEventProperties
//
//		@return				(Canvas)	Canvas that should receive the drop, or null if none found
//  @visibility internal
//<
// Note: Not checking willAcceptDrop() is desired behavior - this allows behavior such as showing
// the no-drop indicator (for example no drop indicator on certain rows in a grid)
getDropTarget : function (event) {
	var EH = this;

    

	// if there is no target or the drag target can't be dropped, 
	//	return null because there can't be a drop target!
	if (!EH.dragTarget || !EH.dragTarget.canDrop || 
        EH.dragOperation == EH.DRAG_RESIZE) return null;

	var target = (EH.dragMoveTarget || EH.dragTarget),

	    dropCandidates = EH._dropRegistry,
		matches = [],
		i = 0,
		length = dropCandidates.length,
        
        // we'll always disallow dropping over self if dragAppearance is target
        canDropOnSelf = (target.getDragAppearance(EH.dragOperation) != isc.EH.TARGET)
	;

	// perform different checks based on how we're supposed to intersect the drop targets
	if (target.dragIntersectStyle == EH.INTERSECT_WITH_MOUSE) {
 
        
        if ((event.target != this.mouseDownTarget() ||
            (isc.Browser.isIE || (isc.Browser.isSafari && !isc.Browser.isTouch) || 
            (isc.Browser.isMoz && isc.Browser.geckoVersion > 20040616 &&
             !this.mouseDownTarget()._useMozScrollbarsNone))) )
        {
            var dropTarget = event.target;

            // allow delegating dropTarget for eg dropLine indicators
            while (dropTarget && dropTarget.dropTarget) dropTarget = dropTarget.dropTarget;
            if ((canDropOnSelf || dropTarget != target) &&
                (dropCandidates.contains(dropTarget)))
            {
                //this.logWarn("used quick check, dropTarget: " + dropTarget + 
                //              ", event target: " + event.target);
                return dropTarget;
            }
        }

		// check whether mouse coordinates are within candidate drop target
        // Note - we're using 'visibleAtPoint()' here rather than 'containsPoint()', as we don't
        // want to pick up a drop target that's occluded by another widget.
        // We may still pick up more than one match, as one canAcceptDrop:true widget may be
        // a child of another.
        // We ignore the dragMoveTarget in this check since it is likely to be under the mouse
        // (or near enough that a quick movement may put it under the mouse).
		for (;i<length;i++) {
			var candidate = dropCandidates[i];
            
            if (candidate.canAcceptDrop && !candidate.isDisabled() &&
                (
                 candidate.visibleAtPoint(event.x, event.y, false, 
                                          EH._getDragMoveComponents())
			     // candidate.containsPoint(event.x, event.y))
                ) &&
                (canDropOnSelf || !target.contains(candidate, true))
               ) 
            {
                matches.add(candidate);
			}
		}
    } else {
        // check whether dragMoveTarget (tracker, outline, etc) intersects candidate drop target
        for (;i<length;i++) {
            var candidate = dropCandidates[i];
            if (!canDropOnSelf && candidate == target) continue;
            
            if (candidate.intersects(target) && 
                candidate.canAcceptDrop && !candidate.isDisabled()) 
            {
                matches.add(candidate);
            }
        }
    }

    //this.logWarn("dropTarget matches" + matches);

	// if there's only one possible drop target, return it
	if (matches.length < 2) return matches[0];

    // For mouse intersection, we may have multiple matches.
    // If some match is the parent of another, the child will always be given preference
    var nearestMatch = matches[0];
    for (var i = 1; i < matches.length; i++) {
        var currentMatch = matches[i];
        // if one is an ancestor of the other, the descendant always wins
        if (nearestMatch.contains(currentMatch, true)) {
            nearestMatch = currentMatch;
        
        // intersectRect check about doesn't handle the case of overlapping widgets occluding
        // each other catch the case of 1 match occluding another.
        } else if (target.dragIntersectStyle == EH.INTERSECT_WITH_RECT) {
         
            // Othewise we want to find a common ancestor of both widgets,
            // and compare the z-indices of their separate ancestors in that common scope
            var commonParent = null,
                nmCommonScopeAncestor = nearestMatch, 
                cmCommonScopeAncestor = currentMatch;
                
            while (commonParent == null) {
                // if we've reached a top-level ancestor widget, the common scope is the document 
                // body, so we'll compare the top-parent of each match widget.
                if (nmCommonScopeAncestor.parentElement == null) {
                    commonParent = true;    // to break out of the while loop
                    cmCommonScopeAncestor = currentMatch.topElement || currentMatch;
    
                // Otherwise check if the ancestor's parentElement is also the ancestor of the 
                // currentMatch
                } else if (nmCommonScopeAncestor.parentElement.contains(currentMatch, true)) {
                        commonParent = nmCommonScopeAncestor.parentElement;
                        // iterate up the currentMatch's parents til we find one in the right scope
                        while (cmCommonScopeAncestor.parentElement != commonParent) {
                            cmCommonScopeAncestor = cmCommonScopeAncestor.parentElement;
                        }
                        
                } else {
                    // look at the ancestor's parent (which we know is not a common ancestor of both
                    // matches)
                    nmCommonScopeAncestor = nmCommonScopeAncestor.parentElement;
                }
            }
            
            // compare the z-indices of the ancestors in the same scope, and adjust nearestMatch if
            // necessary
            if (cmCommonScopeAncestor.getZIndex() > nmCommonScopeAncestor.getZIndex()) {
                nearestMatch = currentMatch;	
            }
        }
    }
    
    return nearestMatch;
},

// Register a canvas passed to receive drop events.
registerDroppableItem : function (item) {
	if (!item._dropRegistered) {
		this._dropRegistry.add(item);
		item._dropRegistered = true;
	}
},

// Un-register a canvas so it will no longer receive drop events
unregisterDroppableItem : function (item) {
	this._dropRegistry.remove(item);
	delete item._dropRegistered;
},


// Register this Canvas as needing to be "masked" during drag interactions, because it contains
// something that will otherwise swallow events.

registerMaskableItem : function (item, makeMask) {
	if (!this._maskRegistry.contains(item) && 
	        (item._maskTarget == null || !this._maskRegistry.contains(item._maskTarget))) {
        // keep a list of items that need masking so we can show their masks when a drag begins
		this._maskRegistry.add(item);

		if (item.dragMaskType == "iframe") {
		    if (makeMask) this.makeEventFrame(item, {eventProxy:item});
		} else if (item.dragMaskType == "hide") {
        } else if (item.dragMaskType == "hidePlugin") {
        } else {
	        // make an event mask for the item that just passes events through to it.  It won't be
	        // show()n yet.
	        if (makeMask) this.makeEventMask(item, {eventProxy:item});
		}
	}
},

//>	@classMethod	isc.EventHandler.unregisterMaskableItem()	(A)
//		Remove this canvas from the _maskRegistry and destroy its
//		event mask peer.
//   @visibility internal
//<
unregisterMaskableItem : function (item) {
	this._maskRegistry.remove(item);
    if (item._eventMask) item._eventMask.destroy();
	delete item._eventMask;
},

// Make a transparent mask suitable for capturing events, as a peer of the target Canvas
makeEventMask : function (canvas, properties, rect) {
    if (isc.isA.Function(canvas.makeEventMask)) return canvas.makeEventMask(properties, rect);

    var defaults = this._eventMaskProperties;
    
    // In IE7, the spacerHTML isn't sufficient to mask IFRAMEs properly, but an image works
    // Note: if you update this code, also check and update ScreenSpan.getInnerHTML()
    if (!defaults.contents) defaults.contents = isc.Browser.isIE && isc.Browser.version > 6 ? 
        isc.Canvas.blankImgHTML(3200,2400) : isc.Canvas.spacerHTML(3200,2400);
    var mask = isc.Canvas.create({
			ID:canvas.getID()+"_eventMask",
			cursor:canvas.cursor,
            _maskTarget: canvas
		}, defaults, properties);
    mask.setRect(rect ? rect : canvas.getRect());
    canvas._eventMask = mask;
	canvas.addPeer(mask);
    return mask;
},

//>	@classAttr	EventHandler._eventMaskProperties  (object : {...} : IRWA)
//  @visibility internal
//<
_eventMaskProperties : {
	autoDraw:false,

    // match the size of the Canvas we're masking
    _resizeWithMaster:true,
	_redrawWithMaster:false,

    // NOTE: we can't initialize the contents here because EventHandler loads before Canvas.
    // By setting very large contents with overflow:hidden we never need to redraw on resize.
    //contents:isc.Canvas.spacerHTML(3200, 2400),
    overflow:"hidden",

    // start out hidden, only show if explicitly shown
    visibility:"hidden",
    _showWithMaster:false,

    getTarget : function () {
        return this._maskTarget;
    },
    
    show : function () {
		// when we're show()n (because dragging has begun), move above the master.  NOTE: this
        // doesn't guarantee the eventMask stays above the master in general - only when
        // explicitly show()n
        var master = this.masterElement;
		this.moveAbove(master);
		return this.Super("show", arguments);
	}
},

// Show event masks for all registered canvases, or a single event mask
// spanning the entire screen if simpleMask is true.
showEventMasks : function (simpleMask, unmaskedItems) {
	var EH = this,
		maskedItems = EH._maskRegistry;
	if (simpleMask) {
        // just do a single screen-sized mask
		if (!EH._eventMask) EH._eventMask = isc.ScreenSpan.create({
            ID:"isc_EH_eventMask",
			mouseDown:function () {this.hide()},	// 030801 jmd: insurance against lock-out if stuck
            // if the screenspan gets destroyed() clear up our pointer to it
            pointersToThis:[{object:EH, property:"_eventMask"}]
		});
		EH._eventMask.show();
		EH._eventMask.bringToFront();
        //>BrowserPlugin
        // browser plugin masks typically cannot be replaced by a single screen mask, always
        // show these
        if (isc.BrowserPlugin) {
            maskedItems.intersect(isc.BrowserPlugin.instances).map("_showDragMask");
        }
        //<BrowserPlugin
	} else {
        // show masks for all components that need masking
		for (var i = 0; i < maskedItems.length; i++) {
            var item = maskedItems[i];

            // If we've been passed an explicit set of items to mask, ensure only those
            // items end up masked.
            if (unmaskedItems && unmaskedItems[item.getID()]) {
                item._hideDragMask();
			} else {
                item._showDragMask();
            }
		}
	}
},

// Hide event masks for all registered canvases, or the single event mask
// spanning the entire screen.
hideEventMasks : function () {
	var EH = this,
		maskedItems = EH._maskRegistry;

	if (EH._eventMask && EH._eventMask.isVisible()) {
		EH._eventMask.hide();
        if (isc.BrowserPlugin) {
            maskedItems.intersect(isc.BrowserPlugin.instances).map("_hideDragMask");
        }
	} else {
		for (var i = 0; i < maskedItems.length; i++) {
			maskedItems[i]._hideDragMask();
		}
	}
},


// Return true if the specified event should be handled by the native event mechanism rather
// than by our EventHandler mechanism.  Used to avoid interferance with native form event
// processing and other similar cases.

_handledNativelyReturnVal:isc.Browser.isIE ? isc.undef : true,
eventHandledNatively : function (eventType, nativeTarget, checkTargetOnly) {
    // If passed a native event name (standard behavior), convert the eventType to our ISC 
    // eventType.
    // Note: if we fail to convert it, we will simply work with whatever we were passed as
    // an event type.
    var iscEventType = eventType;
    if (!this.reverseEventTypes[eventType]) {
        if (this._nativeMouseEventMap[eventType]) 
            iscEventType = this._nativeMouseEventMap[eventType];
        else if (this._nativeKeyEventMap[eventType]) 
            iscEventType = this._nativeKeyEventMap[eventType];
    }
    
    var returnValue = this._eventHandledNatively(iscEventType, nativeTarget, checkTargetOnly);
    if (returnValue && this.logIsDebugEnabled() && iscEventType != "mouseMove") {
        this.logDebug(eventType + " event on " + 
                        (checkTargetOnly ? " native target:" + nativeTarget : this.lastTarget)
                        + " handled natively");
    }
    return returnValue;
}, 

_$handleNativeEvents:"handleNativeEvents",
_$applet: "APPLET",
_eventHandledNatively : function (eventType, nativeTarget, checkTargetOnly) {
    //!DONTCOMBINE

	eventType = (eventType || "");

    
	var EH = this,
		event = EH.lastEvent;

    
    if (!EH._canAccessNativeTargetProperties(nativeTarget)) {
        //EH.logInfo("eventHandledNatively() can't get to event target properties." +
        //           "  Returning true");
        return true;
    }
    if (nativeTarget && nativeTarget.tagName == this._$applet) return true;    


    // if there's no target canvas, the event did not occur over any ISC widgets...
    // don't interfere with event handling if it's a mouse event.  Return true
    
    var isMouseEvent = (EH.isMouseEvent(eventType) ||
                        EH.isTouchEvent(eventType) ||
                        EH.isPointerEvent(eventType)),
        iscTarget = isMouseEvent ? event.target : event.keyTarget;
    
 
    if (!checkTargetOnly && isMouseEvent && iscTarget == null) return true;

    //>DEBUG
    if ((this.logIsInfoEnabled() && eventType == EH.KEY_DOWN) ||
        (this.logIsDebugEnabled() && (eventType == EH.KEY_UP || eventType == EH.KEY_PRESS))) 
    {
        this.logInfo(eventType + " event with Canvas target: " + this.lastEvent.keyTarget +
                     ", native target: " + this.echoLeaf(nativeTarget));
    }
    //<DEBUG
	// if it's a form element or an anchor, just return true so the event can be processed
    // automatically
	// NOTE: we may have an image (or something else) that is contained in an anchor, 
	//		 so we have to look through the list of parentElements for an anchor tag, 
	//		 we can't just look at the nativeTarget
    
	if (EH.passThroughEvents && nativeTarget) {
    
    
        //if (eventType.startsWith("key")) {
        //    this.logWarn("nativeTarget: " + this.echoDOM(nativeTarget));
        //}
		// testTarget will change to be successive parentElements of the nativeTarget
		//	until we get to the body tag
		var testTarget = nativeTarget,
            
            // isNative will be true if 
            // - the target has been marked as handling native events, or
            // - we've found either a native form or anchor element, which is not a focusProxy
            handleNativeEvents = (testTarget.handleNativeEvents || 
                                    (testTarget.getAttribute ? 
                                     testTarget.getAttribute(this._$handleNativeEvents) : null)),
            tagName = testTarget.tagName,
            isNative
        ;
        
        if (!EH._falseString) EH._falseString = "false";            
        
        // If the 'handleNativeEvents' flag wasn't explicitly set, check for form items and
        // elements that will want to handle their own events.
        
        if (handleNativeEvents == null) {

            isNative = (!testTarget.focusProxy &&
                            
                           ((testTarget.form != null && tagName != EH._labelString) || 
                            
                            EH._formTags[tagName] != null ||
                            // editable DIVs (Not supported in Mozilla)
                            
                            (testTarget.isContentEditable && 
                                !testTarget.getAttribute(this._$eventProxyAttributeName)))
                        );

    		// if we didn't find a form, check if we're inside an anchor tag, because we want to
            // allow native processing (following the link).
            
            if (!isNative && (eventType != EH.MOUSE_WHEEL) && (eventType != EH.MOUSE_MOVE)) {
    			while (testTarget && 
                        testTarget.tagName != EH.BODY_TAG && testTarget.tagName != this._$HTML) 
                {
                    // Don't iterate up past any widget's handle - we don't expect canvii to be
                    // written out inside <A> tags
                    
                    if (testTarget.eventProxy != null ||
                        (testTarget.hasAttribute != null && 
                         testTarget.hasAttribute(this._$eventProxyAttributeName))) break;
                
    				if (EH._anchorTags[testTarget.tagName] != null) {
                        
                        var HNE = (testTarget.handleNativeEvents || 
                                    (testTarget.getAttribute ? 
                                     testTarget.getAttribute(this._$handleNativeEvents) : null));
            
                        if (HNE != null && !isc.isA.emptyString(HNE)) {
                            if (isc.isA.String(HNE)) 
                                HNE = (HNE == isc.EH._falseString ? false : true);
                        }
                        
                        if (HNE != false) {
                            isNative = true;
                            break;
                        }
    				}
    				testTarget = testTarget.parentNode;
    			}
    		}
        } else {
        
            // isNative derived directly from the "handleNativeEvents" property hung on the
            // DOM element.  This will be a string - convert "false" to false, so the check for 
            //  if (isNative) {... 
            // will do the right thing.
            isNative = handleNativeEvents;
            if (isNative == EH._falseString) isNative = false;
        }
        
		// if we found a native form or anchor element, return so we don't process the event
        // ourselves
		if (isNative) {
            return true;
            
        // if we had an explicit 'handleNativeEvents=false' specified on the target, avoid 
        // further checking
        } else if (handleNativeEvents != null) {
            return false;
        }
	}
    
    // At this point we know that the event occurred on a canvas, and not over any of the
    // special elements that need native handling.
    //
	// if the event was a mouse-event in a CSS scrollbar let it be handled natively.
    if (!checkTargetOnly && isMouseEvent &&
        this._eventOverCSSScrollbar(iscTarget, eventType, event)) 
    {
        return true;
    }
	// return false so isc event processing continues
	return false;
},

// Is the event passed in a mouse event?
isMouseEvent : function (eventType) {
    // This method is used by eventHandledNatively to determine whether the DOMevent it is 
    // looking at is a mouse event.
    // As such the eventType passed in may be the native event name - which is all lowercase
    // rather than our camelCase event names.  Handle either case.
    
    eventType = eventType || this.lastEvent.eventType;
    
    // list of all mouse events, native and ISC names
    if (this._mouseEvents == null) {
        this._mouseEvents = {
            mouseOver:true, mouseover:true,
            mouseDown:true, mousedown:true, rightMouseDown:true,
            mouseMove:true, mousemove:true,
            mouseOut:true, mouseout:true,
            mouseUp:true, mouseup:true,
            
            DOMMouseScroll:true, mousewheel:true, mouseWheel:true,
            click:true,
            doubleClick:true, doubleclick:true,
            showContextMenu:true, showcontextmenu:true,
            selectStart:true, selectstart:true
        }
    }

    if (this._mouseEvents[eventType] == true) return true;
    
        
    // IE's selectionChanged event can be triggered by mouse or keyboard - this is the last event
    // we record when the user puts focus in a TextItem by mouse or keyboard.
    // Check for event.keyName having been recorded to determine if this was 
    // Tab keypress or similar vs a mouse click
    if (eventType == "selectionChange") {
        return (this.lastEvent.keyName == null || this.lastEvent.keyName == "");
    }
    
    // context menu events can be mouse or keyboard triggered
    // We record a flag on the 'lastEvent' object as part of the handleContextMenu flow which
    // allows us to track this.
    if (eventType == "contextMenu" || eventType == "contextmenu") {
        return !this.lastEvent.keyboardContextMenu
    }
    // otherwise it's not a mouse event.
    return false;
},

_touchEventTypes: {
    touchstart: true,
    touchmove: true,
    touchend: true,
    touchcancel: true,
    touchStart: true,
    touchMove: true,
    touchEnd: true,
    touchCancel: true
},
isTouchEvent : function (eventType) {
    return (eventType in this._touchEventTypes);
},

_pointerEventTypes: {
    pointerdown: true,
    pointermove: true,
    pointerup: true,
    pointercancel: true,
    pointerDown: true,
    pointerMove: true,
    pointerUp: true,
    pointerCancel: true
},
isPointerEvent : function (eventType) {
    return (eventType in this._pointerEventTypes);
},

// Is the event passed in a key event?
isKeyEvent : function (eventType) {
    // This method is used by eventHandledNatively to determine whether the DOMevent it is 
    // looking at is a key event.
    // As such the eventType passed in may be the native event name - which is all lowercase
    // rather than our camelCase event names.  Handle either case.

    eventType = eventType || this.lastEvent.eventType;
    
    if (this._keyEvents == null) {
        this._keyEvents = {};
        var ke = this._keyEvents;
        
        // ISC names:
        ke[this.KEY_DOWN] =true; ke[this.KEY_PRESS] = true; ke[this.KEY_UP] = true;
        
        // add native event names:
        var nativeMap = this._nativeKeyEventMap;
        for (var name in nativeMap) ke[name] = true;
    }

    if (this._keyEvents[eventType] == true) return true;

    if (eventType == "contextMenu" || eventType == "contextmenu") {

        return !!this.lastEvent.keyboardContextMenu
    }

    // otherwise it's not a key event.
    return false;
},

// Did the current mouse event occur over a native CSS scrollbar?
_eventOverCSSScrollbar : function (iscTarget, eventType, event) {

    
    if (isc.Browser.isTouch) return false;

    //this.logWarn("checking event over css scrollbar");
    var EH = this;
    
    		
    // If there's no target or we're not showing native scrollbars, return false
	if (!iscTarget || iscTarget.showCustomScrollbars || 
        !(iscTarget.vscrollOn || iscTarget.hscrollOn)) return false;
        
    
    
    // if right to left, scrollbar on LEFT
    var scrollbarSize = isc.Element.getNativeScrollbarSize();
    if (iscTarget.isRTL()) {
        if ( (iscTarget.vscrollOn && (event.x < iscTarget.getPageLeft() + scrollbarSize) ) ||
             (iscTarget.hscrollOn && (event.y > iscTarget.getPageTop() + 
                                  iscTarget.getHeight() - scrollbarSize) ) 
           )
        {
            if (eventType==EH.MOUSE_DOWN) EH._mouseIsDownInScrollbar = true;
            return true;
        }
    // else if left to right (normal), scrollbar on RIGHT
    } else {
        if ((iscTarget.vscrollOn && (event.x > iscTarget.getPageRight() - scrollbarSize)) ||
            (iscTarget.hscrollOn && (event.y > iscTarget.getPageBottom() - scrollbarSize)) ) 
        {                      
            if (eventType==EH.MOUSE_DOWN) EH._mouseIsDownInScrollbar = true;
            //this.logWarn(eventType + " in scrollbar");
            return true;
        }
    }

    return false;
},


//>	@classMethod	isc.EventHandler.bubbleEvent()	(A)
// Bubble the eventType in question up through the Canvas hierarchiy
//
//		@group	eventBubbling
//		@param	target		(object)	Canvas or DOM object that received the event
//		@param	eventType	(string) 	name of this event
//		@param	eventInfo	(any)		information passed with a custom event (see e.g. Slider)
//      @param  [targetIsMasked] (boolean) If passed we have already tested whether this target 
//                                         is masked, so don't re-check in this method
//
//		@return				(boolean)	false == cancel further event processing
//										anything else == continue processing
//      @visibility internal
//<

_dontLogBubble : {
    mouseMove : true,
    mouseOver : true,
    mouseOut : true
},

bubbleEvent : function (target, eventType, eventInfo, targetIsMasked) {
	var EH = this,
    	event = EH.lastEvent;

    //>DEBUG
	var logBubble = this.logIsDebugEnabled() && !this._dontLogBubble[eventType];
    //<DEBUG

    // check if this widget is masked, and, if a click on this widget would be cancelled, block
    // all mouse events from going to the target.  This prevents rollovers and other
    // indications of interactivity from appearing on components when in fact a click will do
    // nothing.
    //
    // Note that a mouseDown that dismisses a soft clickmask does so before reaching this
    // check.
    
    var isMouseEvent = this.isMouseEvent(eventType);
    if (isMouseEvent) {
        
        if (targetIsMasked == null) {
            
            targetIsMasked = this.targetIsMasked(target, null);
        }
        if (targetIsMasked) {
            //>DEBUG
            if (logBubble) {
                this.logDebug(eventType + " on " + target + " blocked by clickmask");
            }
            //<DEBUG
            return false;
        }
    }

    // For each event, we check for the existance of the 'internal' handler function - named
    // 'handle' + eventType ("handleMouseDown", etc.).
    // If this is defined on the target, call that otherwise, if the event name itself is
    // defined, call that.
    // - Note: this means 'handleMouseDown' (etc.) as defined on the target widget is
    //   responsible for calling 'mouseDown'.
    var eventHandlerName = this._getInternalHandlerName(eventType);

    var prevTarget = null;
    while (target) {
        // never fire an event for a destroyed widget
        if (target.destroyed) break;
        var nextTarget = null;
        var method = null;

		// go up the eventParent or parentElement chain, using an eventProxy if one is defined
        // NOTE: calculate the next target here, because parent hierarchy might change during
        // event handling (eg clear() self when tearing off from a Layout), but the former
        // parent should still receive the event that happened within it.
        
        // Allow only key events or only mouse events to be bubbled up the control heirarchy
        // by specifying keyEventParent or mouseEventParent respectively.
        if (target.mouseEventParent && eventType.startsWith("mouse")) {
            nextTarget = target.mouseEventParent;
        } else if (target.keyEventParent && eventType.startsWith("key")) {
            nextTarget = target.keyEventParent;    
        } else {
            nextTarget = (target.eventParent || target.parentElement);
        }
		if (nextTarget && nextTarget.eventProxy) nextTarget = nextTarget.eventProxy;

		// If target is in edit mode, use target.editProxy for the event if defined
		if (target.editingOn && target.editProxy) {
		    var proxy = target.editProxy;

		    if (proxy[eventHandlerName] != null) {
	            method = eventHandlerName;
	        } else if (proxy[eventType] != null && proxy[eventType] != isc.Class.NO_OP
	                                        && !isc.is.emptyString(proxy[eventType])) {
	            method = eventType;

	            // if the eventType was defined as a string, convert it to a function
	            if (isc.isA.String(proxy[eventType])) {
	                proxy.convertToMethod(eventType);                
	            }

	            //>DEBUG
	            if (logBubble) {
	                this.logDebug("Bubbling event '" + eventType + "', target '" + target + 
	                              "' has handler: " + this.echoLeaf(proxy[eventType]));
	            }
	            //<DEBUG
	        }

		    // Using editProxy
		    if (method) target = proxy;
		}

		if (!method) {
		    if (target[eventHandlerName] != null) {
		        method = eventHandlerName;
		    } else if (target[eventType] != null && target[eventType] != isc.Class.NO_OP
		            && !isc.is.emptyString(target[eventType])) {
		        method = eventType;                                        


		        // if the eventType was defined as a string, convert it to a function
		        if (isc.isA.String(target[eventType])) {
		            target.convertToMethod(eventType);                
		        }

		        //>DEBUG
		        if (logBubble) {
		            this.logDebug("Bubbling event '" + eventType + "', target '" + target + 
		                    "' has handler: " + this.echoLeaf(target[eventType]));
		        }
		        //<DEBUG
		    }
		}
        
        // if we have either a 'handleEvent' method or a straight 'event' method, fire it
        if (method != null && target[method] != null) {
            //this.logWarn(target + "[" + method + "]" + " is:" + target[method]);
    
            var result;
            //try {
                // now call the event handler, and if it returns false or cancels bubbling, bail
                if (isc.DrawItem != null && isc.isA.DrawItem(target) && method == "dragMove") {
                    result = target[method](event, eventInfo, prevTarget);
                } else if (method == "handleDrop") {
                    // For backwards-compatibility, we only send drop() to an
                    // object if it returns "true" from willAcceptDrop(). We
                    // used to check for willAcceptDrop() before starting the
                    // bubble, so some code may assume that drop() will only be
                    // called if willAcceptDrop returned true.
                    var accept = target.willAcceptDrop();
                    if (accept) {
                        // If truthy, we do call drop(), then further bubbling depends on its return value.
                        result = target[method](event, eventInfo);
                    } else {
                        
                        var originalTarget = target.isA("EditProxy") ? target.creator : target;
                        if (originalTarget) {
                            var altTarget = originalTarget.creator;
                            if (altTarget && altTarget.editingOn && altTarget.editProxy) {
                                var altAccept = altTarget.editProxy.willAcceptDrop();
                                if (altAccept || (altAccept == null)) {
                                    // If the alternative target would accept, or would allow bubbling,
                                    // then change the answer
                                    accept = altAccept;
                                }
                            }
                        }

                        // Retest, since the answer may have changed
                        if (accept) {
                            // If truthy, we do call drop(), then further bubbling depends on its return value.
                            result = target[method](event, eventInfo);
                        } else if (accept == null) {
                            // If null, we don't call drop(), but we do bubble
                            result = true;
                        } else {
                            // If falsy but not null, we don't call drop(), and don't bubble (for back-compat)
                            result = false;
                        }
                    }
                } else {
                    result = target[method](event, eventInfo);
                }
            //} catch (e) {
            //    this.logWarn("error returned invoking event handler: " + 
            //                 target.ID + "." + method + ": " + e.toString() + this.getStackTrace());
            //}

			if (result == false) {
                //>DEBUG
                if (logBubble) {
                    this.logDebug("Bubbling for event '" + eventType + 
                                  "' cancelled via false return value by target: " + target);
                }
                //<DEBUG
                return false;
            }
			if (result == EH.STOP_BUBBLING) {
                //>DEBUG
                if (logBubble) {
                    this.logDebug("Bubbling for event '" + eventType + 
                                  "' cancelled via STOP_BUBBLING return value by target: " + target);
                }
                //<DEBUG
                
                // Note: returning the stop-bubbling code, so callers of this method will see
                // the difference between 
                // - events that returned false (typically return false to the DOM to cancel
                //   native propogation)
                // - events that stopped bubbling but didn't return false (return true to the
                //   DOM - allow native event handling to continue unhindered)
                // - events that made it all the way to the top of the object hierachy (may want
                //   to prevent further event processing / bubbling, but not cancel the event 
                //   natively by returning false)
                
                return EH.STOP_BUBBLING;
            //} else {
            //    this.logDebug("Skipping " + target + ", no handler");
            }
		}

        
        if (target.bubbleEvents == false ||
            (target.bubbleMouseEvents == false && EH.isMouseEvent(eventType))) 
        {
            //>DEBUG
            if (logBubble) {
                this.logDebug("Bubbling for event '" + eventType + 
                              "' stopped by '" + target + 
                              "' which does not allow bubbling");
            }
            //<DEBUG
            return true;
        } else if (isc.isAn.Array(target.bubbleMouseEvents)) {
            // target.bubbleMouseEvents is an array of event-names to suppress bubbling for
            if (target.bubbleMouseEvents.contains(eventType)) {
                //>DEBUG
                if (logBubble) {
                    this.logDebug("Bubbling for event '" + eventType + 
                                  "' stopped by '" + target + 
                                  "' which does not allow bubbling");
                }
                //<DEBUG
                return true;
            }
        }

        prevTarget = target;
        target = nextTarget;
	}

	// we got to the end and noone failed -- return true to keep propagating the event!
    //>DEBUG
    if (logBubble) this.logDebug("Event '" + eventType + "' bubbled to top");
    //<DEBUG
	return true;
},

//>	@classMethod	isc.EventHandler._getInternalHandlerName()	(A)
// For each event, return the name of the preferred handler function to be called
// by bubble handler.
// This consists of the event name (passed in) prefixed with 'handler', and with
// the first character converted to uppercase.
//          
//		@group	eventBubbling
//		@param	event		(string)	Name of the event
//
//		@return			(string)    Name of preferred handler
//      @visibility internal
//<
_getInternalHandlerName : function (eventName) {
    
    if (!this._eventHandlerMap[eventName]) {
        this._eventHandlerMap[eventName] = 
                        "handle" + eventName.charAt(0).toUpperCase() + eventName.substring(1);
//        isc.Log.logWarn("handler for eventType:" + eventName + " is " + this.eventHandlerMap[eventName]);
    }
    return this._eventHandlerMap[eventName];
},
    

// Determine whether a target canvas (or any of its parents up the event bubbling
// hierachy) have a handler defined for an event.
// Note: This means either having the event property itself defined, or 'handleEvent'
// defined.
hasEventHandler : function (target, event) {
    if (!isc.isAn.Object(target) || !isc.isA.String(event)) {
        isc.Log.logWarn(
            "EventHandler.hasEventHandler() passed bad parameters [" 
                + [target, event] + "]. returning null;", 
            "event"
        );
        return null;
    }
    var handlerName = this._getInternalHandlerName(event);
    if (this.getBubbledProperty(target, event, true, handlerName, true) != null) {
        return true;
    }
    return false;
},

// Return the value of a certain property for the target or, if not defined,
// for the first of the target's parents to define the property.
// When `expectingMethod' is true, then Class.NO_OP or an empty string are also considered to
// be "not defined".
getBubbledProperty : function (target, property, expectingMethod, property2, expectingMethod2) {
	while (target) {
        var val = target[property];
        if (val && (!expectingMethod || (val !== this.NO_OP &&
                                         !isc.isAn.emptyString(val))))
        {
            return val;
        }
        if (property2 != null) {
            val = target[property2];
            if (val && (!expectingMethod2 || (val !== this.NO_OP &&
                                              !isc.isAn.emptyString(val))))
            {
                return val;
            }
        }
		target = (target.eventParent || target.parentElement);
		if (target && target.eventProxy) target = target.eventProxy;
	}
	return null;
},


// handle the native "selectStart" event (IE specific) 
// In general, we want to suppress selection of text on web pages as it conflicts
// with drag and drop (and looks bad on pages with absolute positioning stuff).

handleSelectStart : function () {
    
        var EH = isc.EH;

    //isc.Log.logDebug("handleSelectStart() triggered (handler for native onselectstart).");

    // In IE9, this event is fired when a selection occurs in response to a call to 
    // programmatically select a block of text (as we do in FormItem.setSelectionRange())
    // In that case we want selection change to be allowed regardless of
    // event details such as the last mouseDown target.
    // We handle this by setting a flag to let us know we're explicitly selecting right now.
    if (EH._settingTextSelection) return true;

    // If the user is drag-selecting inside form items, allow selection to occur.
    // Don't allow selection to occur if the user is already performing an ISC recognized drag
    // and passes over a form item.
    
    var wd = EH.getWindow(),
        nativeTarget = wd.event ? wd.event.srcElement : null,
        mouseDownNativeTarget = EH.mouseDownEvent ? EH.mouseDownEvent.nativeTarget : null;

    if (nativeTarget && mouseDownNativeTarget == nativeTarget && nativeTarget.form
         && !EH.dragging) 
    {
        return true;
    }
    
    
    
    // Allow text selection if
    // - theres no target / mouseDownTarget canvas (don't interfere with native selection)
    // - mouseDown target doesn't disallow text selection
    // - mouseMove target doesn't disallow text selection 
    // - we're not performing a smartclient drag operation
    //
    // OnSelectStart is triggered from any type of selection (drag selection, shift+arrow 
    // keys, programmatic selection).
    // If this is a drag-selection, we want to check the lastMouseDown target
    // If the mouse is not down this is not a drag selection - don't bother checking for 
    // the last mouseDownTarget disallowing selection.
    // In this case getEventTargetCanvas(...) will still get us a pointer to the element
    // on which selection occurred from event.srcElement (set even though this may not be a
    // mouse event).
    
    if (isc.EH._allowTextSelection) return true;
    var mouseDownTarget = isc.EH.mouseIsDown() ? EH.mouseDownTarget() : null,
        target = EH.getEventTargetCanvas(wd.event);

    
    var dragging = (EH.dragging || EH.dragTarget) && EH.dragOperation != EH.DRAG_SELECT;
    var allowSelection = !dragging && 
                         (mouseDownTarget != null ? 
                            mouseDownTarget._allowNativeTextSelection() : true) &&
                         (target != null ? 
                            target._allowNativeTextSelection() : true);
    if (allowSelection) return true;
    return EH.killEvent();
},



// handle the 'onselectionchange' event.  This is an IE specific event.

_$selectionChange:"selectionChange",
handleSelectionChange : function (event) {

    if (!event) event = window.event;

    
        var EH = isc.EH;

    var lastEvent = EH.lastEvent;
    
     
    var nativeTarget = isc.Element._getElementFromSelection(document);
    if (nativeTarget) {
        // derive the target canvas from the target element 
        
        var targetCanvas = EH.getEventTargetCanvas(event, nativeTarget);

        lastEvent.nativeKeyTarget = nativeTarget
        lastEvent.keyTarget = targetCanvas;
        lastEvent.eventType = this._$selectionChange;
        
        if (targetCanvas) {
            targetCanvas.keyTarget = targetCanvas;
            EH.bubbleEvent(lastEvent.keyTarget, "selectionChange");
        }
    }
    
    // We could return false here to cancel the selection change 
    return true;
},

// native "onhelp" event - IE only
// This occurs when the user hits the f1 key and appears to be the only way to 
// cancel the native help dialog in response to f1 keypress in IE
// Handle this event by firing a synthetic keyPress event, allowing the user to return false
// and suppress the native behavior

handleNativeHelp : function () {
    
    // call any native handler that we may have clobbered (manually for speed)
    
    if (this._documentOnHelp) {
        if (this._documentOnHelp() == false) return false;
    }
    if (this._windowOnHelp) {
        if (this._windowOnHelp() == false) return false;
    }
    
    // allow 'handleNativeKeyDown' to actually fire developer defined keyDown and keyPress
    // handlers, and if they return false, we'll return false here, killing the native onhelp 
    // behavior.
    return isc.EH._handleNativeKeyDown(window.event, true);
},

//> @classMethod EventHandler.setDragTrackerImage()
// This API may be called to set the native HTML5 drag tracker image. The <code>x</code> and
// <code>y</code> parameters may be specified to affect the placement of the drag tracker image
// relative to the mouse cursor. The size of the drag tracker image is the intrinsic size of the
// image. Browsers may apply certain visual effects (such as a slight transparency) to this image.
// <p>
// Can only be called during the +link{Canvas.dragStart()} event (or methods called during the
// handling of that event).
// <p>
// <b>NOTES:</b>
// <ul>
// <li>Not supported in Opera 12.x or Safari.</li>
// <li>For best results, this image should be preloaded. Otherwise, the image might not appear
// for the first drag using this image.</li>
// <li>This API does not work in Chrome or Firefox on Windows 7 if the "Use visual styles on windows and buttons"
// setting is turned off.</li>
// </ul>
// @param src (SCImgURL) image source
// @param [x] (int) offset-x from the mouse cursor
// @param [y] (int) offset-y from the mouse cursor
// @visibility external
//<

setDragTrackerImage : function (src, x, y) {
    var lastEvent = this.lastEvent,
        lastDOMevent,
        dt;
    if (!isc.Browser.hasNativeDrag ||
        lastEvent == null ||
        (lastDOMevent = lastEvent.DOMevent) == null ||
        (dt = lastDOMevent.dataTransfer) == null ||
        dt.setDragImage == null ||
        lastDOMevent.type != this._$dragstart ||
        !this.dragTarget)
    {
        return;
    }

    
    if (isc.Browser.isSafari && !isc.Browser.isChrome) return;

    var imgElem = document.createElement("img");
    imgElem.src = this.dragTarget.getImgURL(src == null ? isc.Canvas._blankImgURL : src);

    

    x = x << 0;
    y = y << 0;

    dt.setDragImage(imgElem, x, y);
},


_$encodedDragTypePrefix: "application/x-isc-dragtype",
_currentDragTypeEncodingSchemeVersion: 0,
encodeDragType : function (dragType) {
    if (!dragType) return this._$encodedDragTypePrefix + this._currentDragTypeEncodingSchemeVersion.toString(36);

    var sb = isc.SB.create(),
        lastPos = 0,
        len;
    sb.append(this._$encodedDragTypePrefix, this._currentDragTypeEncodingSchemeVersion.toString(36));
    for (var i = 0, len = dragType.length; i < len; ) {
        var hi = dragType.charCodeAt(i++),
            codePoint = hi,
            startI = i;
        // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charCodeAt
        if (0xD800 <= hi && hi <= 0xDBFF) {
            var low = dragType.charCodeAt(i++);
            codePoint = ((hi - 0xD800) * 0x400) + (low - 0xDC00) + 0x10000;
        }

        // http://www.unicode.org/charts/PDF/U0000.pdf
        if ((0x30 <= codePoint && codePoint <= 0x39) ||
            (0x61 <= codePoint && codePoint <= 0x7A))
        {
            continue;
        }

        sb.append(dragType.substring(lastPos, startI - 1), '_', codePoint.toString(16), '_');
        lastPos = i;
    }
    sb.append(dragType.substring(lastPos, len));
    return sb.release(false);
},

_$utiPrefix: "dyn.a",
_utiLUT: {
    "a":  0, "b":  1, "c":  2, "d":  3, "e":  4,
    "f":  5, "g":  6, "h":  7, "k":  8, "m":  9,
    "n": 10, "p": 11, "q": 12, "r": 13, "s": 14,
    "t": 15, "u": 16, "v": 17, "w": 18, "x": 19,
    "y": 20, "z": 21, "0": 22, "1": 23, "2": 24,
    "3": 25, "4": 26, "5": 27, "6": 28, "7": 29,
    "8": 30, "9": 31
},
decodeDragDataItemTypeString : function (typeStr) {
    if (!typeStr) return null;
    typeStr = typeStr.toLowerCase();

    
    if (typeStr.startsWith(this._$utiPrefix)) {
        var lookupTable = this._utiLUT,
            outputCharCodes = [];
        // Each char of the base32-encoded string provides 5 bits of data.
        var numBits = 0,
            bits = 0;
        for (var i = this._$utiPrefix.length; i < typeStr.length; ++i) {
            var val = lookupTable[typeStr[i]];
            val <<= 3;
            bits |= val >>> numBits;
            numBits += 5;
            if (numBits >= 8) {
                outputCharCodes.add(bits & 255);
                numBits -= 8;
                if (numBits > 0) bits = (val << (5 - numBits)) & 255;
                else bits = 0;
            }
        }

        typeStr = String.fromCharCode.apply(String, outputCharCodes);
        var startPos = typeStr.indexOf(this._$encodedDragTypePrefix);
        if (startPos < 0) return null;
        var endPos = typeStr.indexOf(':', startPos + this._$encodedDragTypePrefix.length);
        if (endPos < 0) endPos = typeStr.length;
        typeStr = typeStr.substring(startPos, endPos);

    } else if (!typeStr.startsWith(this._$encodedDragTypePrefix)) {
        return null;
    }

    var str = typeStr.substring(this._$encodedDragTypePrefix.length),
        encodingSchemeVersion;
    if (// Empty string (no version)
        !str ||
        // or version too new
        (encodingSchemeVersion = parseInt(str[0], 36)) > this._currentDragTypeEncodingSchemeVersion)
    {
        return null;
    }

    var sb = isc.SB.create();
    if (0 == encodingSchemeVersion) {
        var lastPos = 0;
        for (var pos = str.indexOf('_'); pos >= 0; pos = str.indexOf('_', lastPos + 1)) {
            sb.append(str.substring(lastPos + 1, pos));
            lastPos = str.indexOf('_', pos + 1);
            if (lastPos < 0) return null;
            var codePoint = parseInt(str.substring(pos + 1), 16);
            if (isNaN(codePoint)) continue;
            if (codePoint >= 0x10000) {
                // http://en.wikipedia.org/wiki/UTF-16#Code_points_U.2B10000_to_U.2B10FFFF
                // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/fromCharCode
                var d = codePoint - 0x10000,
                    hi = (d >> 10) + 0xD800,
                    low = (d & 0x3FF) + 0xDC00;
                sb.append(String.fromCharCode(hi, low));
            } else {
                if (0xD800 <= codePoint && codePoint <= 0xDFFF) return null;

                sb.append(String.fromCharCode(codePoint));
            }
        }
        sb.append(str.substring(lastPos + 1));
    }
    return sb.release(false);
},

_getEncodedDragType : function (dt) {
    var types;
    if (dt == null || (types = dt.types) == null) return null;

    var len = types.length;
    for (var i = 0; i < len; ++i) {
        var type = types[i];
        if (type && type.startsWith(this._$encodedDragTypePrefix)) {
            return type;
        }
    }

    // Look for UTIs
    for (var i = 0; i < len; ++i) {
        var type = types[i];
        if (type && type.startsWith(this._$utiPrefix)) {
            // Need to check that this UTI encodes a drag type.
            var dragType = this.decodeDragDataItemTypeString(type);
            if (dragType != null) return type;
        }
    }

    return null;
},

_getDragType : function (dt) {
    var type = this._getEncodedDragType(dt);
    if (type == null) return null;
    return this.decodeDragDataItemTypeString(type);
},

//> @classMethod EventHandler.getNativeDragData()
// For a cross-frame drag, retrieves the data made available when the drag was initiated in the
// foreign frame via +link{EventHandler.setNativeDragData()}.
// <p>
// Can only be called during the +link{Canvas.drop} event (or methods called during the handling
// of that event, such as +link{ListGrid.recordDrop}); will return null if called at any other
// time, or if called during a non-HTML5 drag and drop.
// @return (Object) data made available in the foreign frame
// @visibility external
//<
_$drop: "drop",
getNativeDragData : function () {
    var lastEvent = this.lastEvent,
        lastDOMevent;
    if (!isc.Browser.hasNativeDrag ||
        lastEvent == null ||
        (lastDOMevent = lastEvent.DOMevent) == null ||
        lastDOMevent.dataTransfer == null ||
        lastDOMevent.type != this._$drop)
    {
        return null;
    }

    var dt = lastDOMevent.dataTransfer,
        encodedDragData;
    if (isc.Browser.isIE) {
        encodedDragData = dt.getData("Text");
    } else {
        var type = this._getEncodedDragType(dt);
        if (type == null) return null;
        encodedDragData = dt.getData(type);
    }

    var dragData = null;
    
    try {
        if (encodedDragData != null) dragData = isc.JSON.decode(encodedDragData);
    } catch (e) {
        this.logWarn("Failed to decode as JSON: " + e);
    }

    return dragData == null ? null : dragData.data;
},

//> @classMethod EventHandler.setNativeDragData()
// Sets the data available in a cross-frame HTML5 drag (see +link{Canvas.useNativeDrag}).
// <p>
// Data provided to this method must be valid for serialization to JSON via the
// +link{JSONEncoder}, or can simply be a String.
// <p>
// Can only be called during the +link{Canvas.dragStart()} event (or methods called during the
// handling of that event).
// <p>
// Do not pass in sensitive data (e.g. passwords, auth/session tokens, credit card numbers, SSNs, etc.).
// @param data (Object | String) data to make available to foreign frames
// @param [strData] (String) text data to set. This is the text that users may see if the drag
// is dropped into an external application such as Notepad or a non-Smart&#67;lient/Smart&nbsp;GWT
// web application.
// @visibility external
//<
_dragDataEncodeSettings: {
    dateFormat: "xmlSchema",
    prettyPrint: false,
    skipInternalProperties: true,
    strictQuoting: true
},
_$dragstart: "dragstart",
setNativeDragData : function (data, strData, dragType) {
    var lastEvent = this.lastEvent,
        lastDOMevent;
    if (!isc.Browser.hasNativeDrag ||
        lastEvent == null ||
        (lastDOMevent = lastEvent.DOMevent) == null ||
        lastDOMevent.dataTransfer == null ||
        lastDOMevent.type != this._$dragstart ||
        !this.dragTarget)
    {
        return;
    }

    if (dragType == null) dragType = this.dragTarget.dragType;

    if (isc._windowUUID == null) isc._windowUUID = isc.Math.randomUUID();
    var dragData = {
        iscVersion: isc.version,
        iscVersionNumber: isc.versionNumber,
        windowUUID: isc._windowUUID,
        dragType: dragType,
        data: data
    };
    var encodedDragData = isc.JSON.encode(dragData, this._dragDataEncodeSettings);

    var dt = lastEvent.DOMevent.dataTransfer;

    // IE only supports "Text" and "URL" data types:
    // http://msdn.microsoft.com/en-us/library/ie/ms536744.aspx
    if (isc.Browser.isIE) {
        dt.setData("Text", encodedDragData);

    } else {
        dt.setData(this.encodeDragType(dragType), encodedDragData);
        if (strData == null || (strData = String(strData).trim()).length == 0) strData = "\u00A0";
        dt.setData("Text", strData);
    }
},

_effectAllowedMap: {
    "none": "none",
    "copy": "copy",
    "copyLink": "copyLink",
    "copylink": "copyLink",
    "copyMove": "copyMove",
    "copymove": "copyMove",
    "link": "link",
    "linkMove": "linkMove",
    "linkmove": "linkMove",
    "move": "move",
    "all": "all",
    "uninitialized": "uninitialized"
},

// native ondragstart
// the ondragstart event happens if you try to drag some selected text or an image - it allows
// you to drop the selected text into any text editor, or save the image to the desktop.  If we
// don't cancel the event, IE takes over the drag, modifies the cursor and stops sending
// mouseMove.

handleNativeDragStart : function (DOMevent) {
    // Check if the widget has useNativeDrag:true.
    if (isc.Browser.hasNativeDrag && DOMevent.dataTransfer) {
        var EH = isc.EH,
            dt = DOMevent.dataTransfer,
            event = EH.getMouseEventProperties(DOMevent),
            target = event.target;
        if (target != null && (target = target._getNativeDragTarget()) != null) {
            event.target = target;

            
            if (!target.useNativeDrag) return false;

            if (isc.Browser.isIE) {
                if (target.dragType == null) {
                    this.logWarn("No dragType string is set on " + target.getID() + ". Canceling native drag...");
                    return false;
                }

                var crossFrameDrag = isc.Canvas.getCrossFrameDragByDragType && isc.Canvas.getCrossFrameDragByDragType(target.dragType);
                if (crossFrameDrag == null) {
                    this.logWarn("No cross-frame drag settings have been registered for dragType:'" +
                                 target.dragType + "'. Canvas.registerCrossFrameDrags() must be " +
                                 "called to register cross-frame drag settings for this dragType. " +
                                 "Canceling native drag...");
                    return false;
                }

                dt.effectAllowed = EH._effectAllowedMap[crossFrameDrag.effectAllowed];
            } else {
                dt.effectAllowed = "copy";
            }

            // Set null drag data by default because Firefox 22.0 cancels the drag if no data
            // is set on 'dragstart'.
            if (!(isc.isA.DataBoundComponent && isc.isA.DataBoundComponent(target))) {
                EH.setNativeDragData(null, null, target.dragType);

            } else {
                EH.setNativeDragData(target.cloneDragData(), null, target.dragType);
            }

            EH.dragTarget = target;
            EH.dragOperation = "drag";
            var returnVal = EH.handleDragStart(true);
            EH.handleDragMove();
            return returnVal;
        }
    }

    // If an ISC drag is occurring, return false to suppress the native drag so we can
    // continue to get mouse-moves and respond to the drag.
    if (isc.EH.dragTarget) return false;

    
    var target = isc.EH.mouseDownTarget();

    // if within a Canvas, allow the drag if you can select text within the Canvas.
    // This will allow the user to drag out a selected chunk of text to a text editor, etc. 
    
    if (target) return !!(target._allowNativeTextSelection());

    // call any native handler that we may have clobbered (manually for speed)
    if (this._documentDragStart) return this._documentDragStart();
    if (this._windowDragStart) return this._windowDragStart();

    
},


_useCrossFrameDragCleanupTimer: isc.Browser.isMoz && isc.Browser.version < 10,

_crossFrameDragCleanupDelayMillis: 600,
_setCrossFrameDragCleanupTimer : function () {
    var EH = this;
    if (EH._crossFrameDragCleanupTmrID != null) isc.Timer.clear(EH._crossFrameDragCleanupTmrID);
    EH._crossFrameDragCleanupTmrID = isc.Timer.setTimeout(EH._crossFrameDragCleanupAction, EH._crossFrameDragCleanupDelayMillis);
},
_crossFrameDragCleanupAction : function () {
    var EH = isc.EH;
    delete EH._crossFrameDragCleanupTmrID;
    if (EH.crossFrameDragging) {
        EH.artificialDragTarget.dragType = null;

        EH._handleNativeDragEnd(EH.lastEvent.DOMevent);
    } else if (EH.nativeDragging) {
        if (EH.dragMoveTarget) EH.dragMoveTarget.hide();
    }
},

_dropEffectsByEffectAllowed: {
    "none": "none",
    "copy": "copy",
    "copyLink": "copy",
    "copylink": "copy",
    "copyMove": "copy",
    "copymove": "copy",
    "all": "copy",
    "link": "link",
    "linkMove": "link",
    "linkmove": "link",
    "move": "move"
},
_$none: "none",
_handleNativeDragOver : function (DOMevent) {
    var dt = DOMevent.dataTransfer,
        initialDropEffect = dt.dropEffect,
        EH = isc.EH;

    
    if (dt.files != null && dt.files.length > 0) return;
    var types = dt.types,
        hasFiles = false;
    if (types != null) {
        for (var ri = types.length; ri > 0; --ri) {
            if (types[ri - 1] == "Files") {
                hasFiles = true;
                break;
            }
        }
    }

    if (!hasFiles) dt.dropEffect = EH._$none;

    var effectAllowed;

    
    if (isc.Browser.isIE) {
        try {
            effectAllowed = EH._effectAllowedMap[dt.effectAllowed];
        } catch (e) {
            effectAllowed = initialDropEffect;
        }
    } else {
        effectAllowed = dt.effectAllowed;
    }

    // This might be a cross-frame drag.
    if (!EH.nativeDragging) {
        var dragType = null;

        if (isc.Browser.isIE) {
            // Look up the cross-frame drag settings corresponding to the current effectAllowed
            // value.
            var crossFrameDrag = isc.Canvas.getCrossFrameDragByEffectAllowed(effectAllowed);

            // If no cross-frame drag is registered (see Canvas.registerCrossFrameDrags()), then
            // we don't know about this, so leave dropEffect set to "none" and return. By not
            // returning false here, the browser won't allow a drop in the application.
            if (crossFrameDrag == null) {
                return;
            }

            dragType = crossFrameDrag.dragType;
            
        } else {
            dragType = this._getDragType(dt);

            if (dragType == null) return;
        }

        // At this point we either obtained the dragType from the cross-frame drag settings or
        // recognize one of the drag data types as one of our special encoded type strings.

        // Initialize the dropEffect to "none".
        dt.dropEffect = EH._$none;

        // Create an artificial dragTarget.
        if (EH.artificialDragTarget) EH.artificialDragTarget.destroy();
        // faux createAutoChild
        var artificialDragTargetConstructor = EH.artificialDragTargetConstructor || EH.artificialDragTargetDefaults._constructor;
        EH.dragTarget = EH.artificialDragTarget = isc[artificialDragTargetConstructor].create(
            {
                autoDraw: false,
                _generated: true
            }, EH.artificialDragTargetDefaults, EH.artificialDragTargetProperties, {
                left: DOMevent.pageX,
                top: DOMevent.pageY,
                dragType: dragType
            }
        );

        var event = EH.getMouseEventProperties(DOMevent);
        event.target = EH.dragTarget;

        EH.dragOperation = "drag";
        EH._mouseIsDown = true;
        EH.mouseDownEvent = isc.addProperties({}, event);
        EH.handleDragStart(true);
        EH.crossFrameDragging = true;

        
        if (EH._useCrossFrameDragCleanupTimer) EH._setCrossFrameDragCleanupTimer();
    } else {
        if (EH._useCrossFrameDragCleanupTimer) EH._setCrossFrameDragCleanupTimer();

        // If a native drag is in progress, initialize the dropEffect to "none".
        dt.dropEffect = EH._$none;

        
        var lastEvent = EH.lastEvent;
        if (lastEvent != null &&
            lastEvent.screenX == DOMevent.screenX &&
            lastEvent.screenY == DOMevent.screenY)
        {
            if (lastEvent._lastDragOverReturnVal === false) {
                dt.dropEffect = EH._dropEffectsByEffectAllowed[effectAllowed];
            }
            return lastEvent._lastDragOverReturnVal;
        }
    }

    EH.handleMouseMove(DOMevent);

    
    var returnVal,
        dropTarget = EH.dropTarget;
    if (dropTarget && dropTarget.canAcceptDrop) {
        
        var altTarget = dropTarget.creator;
        if ((dropTarget.editingOn && dropTarget.editProxy && dropTarget.editProxy.willAcceptDrop()) ||
            (altTarget && altTarget.editingOn && altTarget.editProxy && altTarget.editProxy.willAcceptDrop()) ||
            dropTarget.willAcceptDrop())
        {
            returnVal = false;
            dt.dropEffect = EH._dropEffectsByEffectAllowed[effectAllowed];
        }
    }
    EH.lastEvent._lastDragOverReturnVal = returnVal;
    return returnVal;
},

_handleNativeDragEnter : function (DOMevent) {
    var EH = isc.EH;

    
    EH._lastDragEnterTarget = (DOMevent.srcElement || DOMevent.target);

    
    if (isc.Browser.isIE) {
        EH.handleMouseMove(DOMevent);

        
        var dropTarget = EH.dropTarget,
            returnVal;
        if (dropTarget && dropTarget.canAcceptDrop) {
            
            var altTarget = dropTarget.creator;
            if ((dropTarget.editingOn && dropTarget.editProxy && dropTarget.editProxy.willAcceptDrop()) ||
                (altTarget && altTarget.editingOn && altTarget.editProxy && altTarget.editProxy.willAcceptDrop()) ||
                dropTarget.willAcceptDrop())
            {
                if (DOMevent.preventDefault) DOMevent.preventDefault();
                returnVal = false;
            }
        }
        EH.lastEvent._lastDragOverReturnVal = returnVal;
        return returnVal;
    }
},

_handleNativeDragLeave : function (DOMevent) {
    var EH = isc.EH;
    
    if (EH._lastDragEnterTarget == (DOMevent.srcElement || DOMevent.target)) {
        EH._lastDragEnterTarget = null;

        if (!EH._useCrossFrameDragCleanupTimer) {
            if (EH.crossFrameDragging) {
                
                EH.artificialDragTarget.dragType = null;

                EH._handleNativeDragEnd(DOMevent);
            } else if (EH.nativeDragging) {
                if (EH.dragMoveTarget) EH.dragMoveTarget.hide();
            }
        }
    }
},

_handleNativeDragEnd : function (DOMevent) {
    var EH = isc.EH;
    EH.handleMouseUp(DOMevent);
},

_handleNativeDrop : function (DOMevent) {
    var EH = isc.EH,
        wasNativeDragging = EH.nativeDragging;
    EH.handleMouseUp(DOMevent);
    // Only return false if EH.nativeDragging was true. Otherwise, returning false here might disable
    // a drag and drop of files (for example, onto a file input).
    if (wasNativeDragging) return false;
},


_handleTransitionEnd : function (DOMevent) {
    var EH = isc.EH;

    var eventInfo = {
        DOMevent: DOMevent,
        eventType: EH.TRANSITION_END,
        nativeTarget: DOMevent.target,
        propertyName: DOMevent.propertyName,
        prefixedPropertyName: DOMevent.propertyName,
        elapsedTime: DOMevent.elapsedTime
    };
    if (DOMevent.propertyName != null && DOMevent.propertyName.startsWith(isc.Element.vendorCSSPrefix)) {
        eventInfo.propertyName = DOMevent.propertyName.substring(isc.Element.vendorCSSPrefix.length);
    }
    var target = eventInfo.target = EH.getEventTargetCanvas(DOMevent, eventInfo.nativeTarget, eventInfo);
    return EH.handleEvent(target, EH.TRANSITION_END, eventInfo);
},


// Handle a page-level resize event.
handleResize : function (DOMevent) {
    //Log.logWarn("page-level resize event");
    // delay briefly to avoid getting flooded by page-level resize events during drag resize on
    // Windows IE
    if (isc.EH.resizeTimer == null) {
        isc.EH.resizeTimer = isc.Timer.setTimeout("isc.EH._pageResize()", 0);
    }
    // Always return true. This will allow any 'window.onresize' handlers set up before
    // ISC was loaded to fire.
    
    return true;
},

// Fired when the user rotates a mobile device:
// Safari / iPhone and iPad only

handleOrientationChange : function (DOMEvent) {
    this._fireResizeEvent();
},


_pageResizePollMethod : function () { isc.EH._pageResize(true); },

// Note the "polling" param indicates that this method is not called from a native browser resize
// event - it either comes from the polling code if isc.Page.pollPageSize is true, or from some other
// case where we want to verify that the size hasn't changed.
// If this parameter is passed and the page size is not changed, this method will no-op
_pageResize : function (polling) {
    isc.EH.resizeTimer = null;
    var orientation = isc.Page.getOrientation();
    // !polling implies this came from a real resize event and the size has changed
    if (!polling) {
        // This both records the reported width, and ensures that it's up to date.
        this._previousInnerWidth = isc.Page.getWidth(window, true);
        this._previousInnerHeight = isc.Page.getHeight(window, true);

        if (this.resizingPollTimer != null) isc.Timer.clearTimeout(this.resizingPollTimer);
        this.resizingPollTimer = isc.Timer.setTimeout(this._pageResizePollMethod, 100);

    } else {
        // We want to force a recalculation of width / height here.
        // If the value has changed, re-run the resized handler to resize children etc.
        var newWidth = isc.Page.getWidth(window, true),
            newHeight = isc.Page.getHeight(window, true),
            unchanged = (orientation ==  this.currentOrientation) &&
                        (newWidth == this._previousInnerWidth && 
                         newHeight == this._previousInnerHeight)

        // If we're polling for content changes that introduce / hide scrollbars, 
        // re-run this method on every idle
        if (isc.Page.pollPageSize) {
            isc.Page.setEvent(isc.EH.IDLE, this._pageResizePollMethod, isc.Page.FIRE_ONCE);
        }
        // Don't actually fire the handler if there was no resize
        if (unchanged) return;

        // record the size so we can no-op next time this method is run
        this._previousInnerWidth = newWidth;
        this._previousInnerHeight = newHeight;
    }
    this._fireResizeEvent(orientation);
},

currentOrientation:isc.Page.getOrientation(),
_currentWidth:isc.Page.getWidth(),


_fireResizeEvent : function (orientation) {
    isc.Page.handleEvent(null, isc.EH.RESIZE);

    var width = isc.Page.getWidth();
    if (width == this._currentWidth) return;

    // Fire orientationChange event from resize event rather only on the native
    // onOrientationChange - this means we can fire it
    // on a desktop browser if the user drags from a portrait type sizing to a landscape
    // type sizing
    
    if (orientation == null) orientation = isc.Page.getOrientation();
    if (orientation != this.currentOrientation) {
        this._currentWidth = width;
        this.currentOrientation = orientation;
        isc.Page.handleEvent(null, isc.EH.ORIENTATION_CHANGE);
    }
},

// handle a native "mousewheel" event, currently only available in IE6 and above, and Mozilla

handleMouseWheel : function (DOMevent) {
    
        var EH = isc.EH;
    if (!DOMevent) DOMevent = EH.getWindow().event;
    var nativeTarget = (DOMevent.srcElement || DOMevent.target);
    if (EH.eventHandledNatively(DOMevent.type, nativeTarget)) return EH._handledNativelyReturnVal;
    
    EH.getMouseEventProperties(DOMevent);
    
    // Pass to the appropriate widget, and stop if this returns false.
    var target = EH.lastEvent.target;
    var returnValue;
    if (EH.targetIsEnabled(target)) {  
        returnValue = EH.bubbleEvent(target, EH.eventTypes.MOUSE_WHEEL);
    }

    if (returnValue == false) {
        
        if (DOMevent.preventDefault) DOMevent.preventDefault();
        return false;
    }
    // Return true to avoid interfering with native events
    return true;
},

//> @classMethod EventHandler.getWheelDelta()
// Applies to +link{canvas.mouseWheel(),mouseWheel} events only.
// Returns a numeric value indicating how far the mouse wheel was rotated. This value will be
// positive if the user scrolled the mousewheel forward or up, or negative if scrolled in the
// other direction. For a standard wheel-mouse, an increment of 1 relates to the smallest
// possible rotation of the mouse wheel. For other scrolling devices, such as scroll 
// gestures on a track pad, wheel delta may be reported in finer grained increments 
// (causing this method to return a fractional value).
// <P>
// Note that behavior for trackpad scroll-gestures may differ by browser, but where 
// separate vertical and horizontal scroll information is available, this method
// refers to a vertical scroll gesture.
// <P>
// Developers should also be aware
// that some browsers and operating systems allow the user to configure the sensitivity
// of the mouse wheel or trackpad, which may change this value.
//
// @deprecated in favor of +link{EventHandler.getWheelDeltaY()}
// @return (float) numeric value indicating how far the mouse wheel was rotated.
// @visibility external
//<
// canvas.scrollWheelDelta is currently not exposed - we may want to interlink docs with
// that attribute if it becomes exposed.
getWheelDelta : function (event) {
    return (event || this.lastEvent).wheelDelta;
},

//> @classMethod EventHandler.getWheelDeltaX()
// Horizontal scroll delta reported by a +link{canvas.mouseWheel(),mouseWheel} event
// (such as a horizontal swipe on a track-pad).
// <P>
// Returns a numeric value indicating how far the mouse wheel was rotated / the magnitude
// of the scroll gesture. This value will be
// positive if the user scrolled the mousewheel to the right, negative if scrolled in the
// other direction. 
// 
// @see EventHandler.getWheelDeltaY()
// @return (float) numeric value indicating how far the mouse wheel was rotated.
// @visibility external
//<
// canvas.scrollWheelDelta is currently not exposed - we may want to interlink docs with
// that attribute if it becomes exposed.
getWheelDelta : function (event) {
    return (event || this.lastEvent).wheelDelta;
},

//> @classMethod EventHandler.getWheelDeltaY()
// Applies to +link{canvas.mouseWheel(),mouseWheel} events only.
// Returns a numeric value indicating how far the mouse wheel was rotated. This value will be
// positive if the user scrolled the mousewheel forward or up, or negative if scrolled in the
// other direction. For a standard wheel-mouse, an increment of 1 relates to the smallest
// possible rotation of the mouse wheel. For other scrolling devices, such as scroll 
// gestures on a track pad, wheel delta may be reported in finer grained increments 
// (causing this method to return a fractional value).
// <P>
// Note that behavior for trackpad scroll-gestures may differ by browser, but where 
// separate vertical and horizontal scroll information is available, this method
// refers to a vertical scroll gesture.
// <P>
// Developers should also be aware
// that some browsers and operating systems allow the user to configure the sensitivity
// of the mouse wheel or trackpad, which may change this value.
//
// @see EventHandler.getWheelDeltaX()
// @return (float) numeric value indicating how far the mouse wheel was rotated.
// @visibility external
//<
// canvas.scrollWheelDelta is currently not exposed - we may want to interlink docs with
// that attribute if it becomes exposed.
getWheelDelta : function (event) {
    return (event || this.lastEvent).wheelDelta;
},


// Handle a "DOMMouseScroll" event
// This is the event Mozilla fires when the user spins the mouse scroll wheel.
// Fall through to the standard handleMouseWheel functionality, which will pick up the details
// for the event and cancel native behavior if appropriate

handleDOMMouseScroll : function (e) {
    return isc.EH.handleMouseWheel(e);
},

// Handle a scroll event
handleScroll : function (DOMevent) {
    
	//window.status = 'handleScroll ' + timeStamp();
    //return (EH.handleEvent(EH.getEventTargetCanvas(DOMevent), "_handleCSSScroll") != false);
},



prepareForLinkDrag : function (dragTarget, linkID) {
	this.dragTarget = (isc.isA.String(dragTarget) ? 
                       this.getWindow()[dragTarget] : dragTarget);
	this.dragTargetLink = linkID;
	return false;
},


// Drag Tracker
// ----------------------------------------------------------------------------------------

//>	@classMethod	isc.EventHandler.setDragTracker()
// Set the HTML for the drag tracker that follows the mouse during a drag and drop interaction.
// <P>
// Your canvas can use this routine to set the drag tracker to whatever HTML you want like so:
// <pre>
//    dragStart : function () {
//        isc.EventHandler.setDragTracker('Your contents here');
//    }
// </pre>
//
//		@group	dragDrop, dragTracker
//		@param	html		(string) 	HTML for the tracker
//		@param	[newWidth]	(int)	new width for the tracker. Default value: 10
//		@param	[newHeight]	(int) 	new height for the tracker. Default value: 10
//		@param	[offsetX]	(int)	x-offset for the tracker
//		@param	[offsetY]	(int) 	y-offset for the tracker
//      @param  [properties] (object)   Opportunity to pass in a free-form set of properties 
//                                      for the dragTracker
//  @visibility external
//<

setDragTracker : function (html, newWidth, newHeight, offsetX, offsetY, properties) {
    var dragTracker = this._makeDragTracker(properties);

	// set size (or reset to small size if it was previously set larger)
    newWidth = newWidth || 10;
    newHeight = newHeight || 10;
    dragTracker.resizeTo(newWidth, newHeight);

    // update contents
	dragTracker.setContents(html);

    // redraw right away for responsiveness
    dragTracker.redrawIfDirty("setDragTracker");

    // apply drag offset if specified
    if (offsetX) dragTracker.offsetX = offsetX;
    if (offsetY) dragTracker.offsetY = offsetY;

    // we don't want these new settings to stick globally - require the user to call
    // setDragTracker() every time they want to deviate from the defaults only.  Otherwise any
    // single instance of deviation from defaults would require a setDragTracker() on all other
    // DnD interactions on the page simply to reset back to defaults.
    dragTracker._isCustomized = true;
},


// Create the 'drag tracker' -- a canvas that follows the mouse to indicate
// that something is being dragged.  See  isc.EventHandler.setDragTracker() for 
// details on how to customize the tracker.
_makeDragTracker : function (overrides) {
    if (!this.dragTracker) {
        var defaults = this.dragTrackerDefaults; 
        // set default contents just to make sure the drag tracker is visible if you enable it
        // but neglect to set contents.  NOTE that it's not expected that anyone would actually
        // use a black square and the contents are never restored to a black square.  NOTE also
        // that we don't want to use a background color which would bleed through transparent
        // images.
        defaults.contents = isc.Canvas.imgHTML("[SKIN]black.gif",10,10);
	    this.dragTracker = isc.Canvas.create(defaults, overrides);
    } else if (overrides != null) this.dragTracker.setProperties(overrides);
    return this.dragTracker;
},

// Get a Canvas that draws an outline, whose initial size and position matches the Canvas
// passed as an argument.
getDragOutline : function (target, outlineSize, outlineColor) {
    
    if (!this.dragOutline) {
        this.dragOutline = isc.Canvas.create({
            autoDraw:false,
            overflow:isc.Canvas.HIDDEN
        })
        
        if (isc.Browser.isIE) this.dragOutline.setContents(isc.Canvas.spacerHTML(3200, 2400));
    }

    var outline = this.dragOutline;
    if (isc.Element.getStyleDeclaration(target.dragOutlineStyle)) {
        outline.setStyleName(target.dragOutlineStyle);
    } else {
        outline.setBorder((outlineSize || 1) + "px solid " + (outlineColor || "black"));
    }

    // size the outline so it matches the object being resized
    outline.setPageRect(target.getPageLeft(), target.getPageTop(),
                        target.getVisibleWidth(), target.getVisibleHeight());

    // if this outline will be used for a resize, the min and max sizes of the outline need to
    // match the object being resized
    outline.minWidth = target.minWidth;
    outline.minHeight = target.minHeight;
    outline.maxWidth = target.maxWidth;
    outline.maxHeight = target.maxHeight;


    // if the target wants to stay within its parent, the outline should stay there too
    outline.keepInParentRect = target.keepInParentRect;
    return outline;
},

//> @classMethod EventHandler.getDragRect()
// During a drag with +link{canvas.dragAppearance,dragAppearance} of either "target" or
// "outline", returns the page-relative coordinates of whatever element is being dragged.
// <P>
// Calling this method allows you to write drag and drop logic that works identically even if
// <code>dragAppearance</code> is subsequently changed.
//
// @return (Rect) global (page-relative) coordinates and size of the dragged element, as a
//                       4-element array [left,top,width,height], or null if not dragging
// @group dragdrop
// @visibility external
//<
getDragRect : function () {
    
    var target = this.dragMoveTarget || this.dragTarget;
    if (!target) return null;
    return target.getPageRect();
},



// move whatever component is intended to be moving during the drag (tracker, outline,
// dragTarget itself) 
_moveDragMoveTarget : function () {
    //!DONTCOMBINE

    var EH = this;

    var target = EH.dragMoveTarget;
    if (!target) return true;

    
    //>Moz1.4
    var stayInParent = (isc.Browser.isMoz && isc.Browser.geckoVersion < 20031007 &&
                        !target.keepInParentRect);

    if (stayInParent &&
        (target.parentElement && 
         !target.parentElement.containsPoint(EH.lastEvent.x,EH.lastEvent.y)))
    {
        return true;
    }
    //<Moz1.4

	// move the dragMoveTarget to the event
    isc._useBoxShortcut = true;
    target.moveToEvent(EH.dragOffsetX, EH.dragOffsetY);
    isc._useBoxShortcut = false;

    target.show();
	return true;
},

// Routine to move the object that's being moved via the dragMove mechanism.
_resizeDragMoveTarget : function () {
    //!DONTCOMBINE
    var EH = this;
	// move the dragMoveTarget to the event
	if (EH.dragMoveTarget) EH.dragMoveTarget.resizeToEvent(EH.resizeEdge);
	return true;
},

// Kill the current native event
killEvent : function (DOMevent) {
    if (!DOMevent) DOMevent = isc.EH.getWindow().event;
    DOMevent.cancelBubble = true;
    return false;
},

// Return the value that will stop event bubbling

stopBubbling : function () {
	return isc.EH.STOP_BUBBLING;
},

// Start a timer to fire the synthetic 'idle' event after a short delay.  This should never
// need to be called directly - call Page.setEvent('idle', action) instead.
_$handleIdle:"_handleIdle",
startIdleTimer : function () {
    // idle shouldn't fire until after page load, and the idle timer is kicked off at page
    // load, no use setting timers in between
    if (!isc.Page.isLoaded()) return;

    // start the timer if we don't already have one running
    if (!this.idleTimer) {
        this.idleTimer = isc.Timer.setTimeout({target:isc.EH, methodName:this._$handleIdle}, 
                                              this.IDLE_DELAY);
    }
},

// Call registered actions for the idle event.
_handleIdle : function () {
    // allow a new timer to be set (note: code triggered from idle frequently sets further idle
    // events, so this needs to happen first)
	this.idleTimer = null;

	// handle the page-level idle stuff, as normal
	var result = isc.Page.handleEvent(null,this.IDLE);

	// if there are any remaining actions for the idle event, start the timer again
	if (isc.Page.actionsArePendingForEvent(this.IDLE)) this.startIdleTimer();
	return result;
},

_threadCounter : 0,
_setThread : function (threadCode) {
    // use a rotating counter to distinguish things like successive mouseMoves
    var newThread = threadCode + this._threadCounter++;
    
    if (this._thread != null) this._interruptedThread = this._thread;
    this._thread = newThread;
    if (isc.Log.logIsInfoEnabled("RpcTabTiming")) {
        this._setThreadTimeStamp = isc.timeStamp();
    }
    if (this._threadCounter > 9) this._threadCounter = 0;
},
_clearThread : function () { 
    if (this._threadExitActions != null) this.runTeas();
    if (this._interruptedThread) {
        this._thread == this._interruptedThread;
        this._interruptedThread = null;
    } else {
        this._thread = null; 
    }
},


_setThreadExitAction : function (action) {
    isc.Timer.setTimeout(action, 0);
    var actions = this._threadExitActions;
    if (actions == null) actions = this._threadExitActions = [];
    actions.add(action);
},


runTeas : function () {
    //!OBFUSCATEOK
    var origThread = this._thread;
    while (this._threadExitActions != null) {
        var actions = this._threadExitActions;
        this._threadExitActions = null;

        if (this.logIsDebugEnabled()) {
            this.logDebug("firing threadExitActions: " + this.echoAll(actions));
        }

        for (var i = 0; i < actions.length; i++) {
            this._thread = origThread + "[E" + i + "]";
            var action = actions[i];
            if (isc.isA.String(action)) isc.eval(action);
            else action();
        }
    }
},

// NOTE: other codes exist in eg FormItem.js for other places where we get a direct call from
// the DOM
_threadCodes : {
    load : "LOD",
    mousedown : "MDN",
    mouseup : "MUP",
    mousemove : "MMV",
    mouseout : "MOU",
    touchstart : "TDN",
    touchmove : "TMVP",
    touchend : "TUP",
    contextmenu : "CXT",
    keypress : "KPR",
    keydown : "KDN",
    keyup : "KUP",
    resize : "RSZ"
},


_$nativeEvents:"nativeEvents",

dispatch : function (handler, event) {
    
    if (!event) event = this.getWindow().event;

    
    if (isc._evalRunning != null) {
        delete isc._evalRunning;
    }

    
    this._setThread(this._threadCodes[event.type] || event.type); 

    
    
    var result;
    if (isc.Log.supportsOnError) {
        result = handler.call(this, event);
    } else {
        
        try {
            result = handler.call(this, event);
        } catch (e) {
            isc.Log._reportJSError(e);
            
            throw e;;
        }
        
    }

    this._clearThread();

    if (result != false && this._replacedEvents[event.type]) {
        var baseResult = this._replacedEvents[event.type](event);
        if (baseResult == false) result = false;
    }

    return result;
},

// assign the handler
_$event: "event",



_$funcBody :
        
            "if (!isc.Browser.isIE && event == null) return;" + 
            ((isc.Browser.isMoz && isc.Browser.version >= 6) || isc.Browser.isChrome ?
             "if(event.defaultPrevented)return;" :
             (isc.Browser.isMoz ? "if(event.getPreventDefault&&event.getPreventDefault())return;" :
               isc.Browser.isSafari ? "if(event.returnValue==false)return;" : ""))
    
    + "var returnVal=argu"+"ments.callee._window.isc.EH.dispatch(argu"+"ments.callee._handler,event);"
    + (!isc.Browser.isIE && isc.Browser.isDOM ? 
    "if(returnVal==false)event.preventDefault();else if(returnVal==isc.EH.STOP_BUBBLING)event.stopPropogation();"
       : "")
    + "return returnVal;"
,
_replacedEvents: {},
_nativeEventName_TypeMap:{
    onmousedown:"mousedown",onmouseup:"mouseup",onclick:"click",ondblclick:"dblclick",
        oncontextmenu:"contextmenu",onmousewheel:"mousewheel",
    onmouseover:"mouseover",onmouseout:"mouseout",onmousemove:"mousemove",

    ondragstart:"dragstart",ondrag:"drag",ondragend:"dragend",
    ondragenter:"dragenter",ondragover:"dragover",ondragleave:"dragleave",
    ondrop:"drop",

    onresize:"resize", onload:"load",onunload:"unload",
    onselecttext:"selecttext",onselectionchanged: "selectionchanged",
    onkeydown:"keydown",onkeyup:"keyup",onkeypress:"keypress",

    // mobileIE support:
    // the MS* version of these works both on mobileIE10 and mobileIE11, but is technically
    // deprecated in favor of not having the "MS" prefix and being lowercase - so e.g. 
    // MSPointerDown -> pointerdown.  We are not going to bother to support mobileIE < 11
    pointerdown: "pointerdown",
    pointermove: "pointermove",
    pointerup: "pointerup",
    pointercancel: "pointercancel"
},
_documentEventHandlers:{},

// the only way to register events on mobile IE11+ is with event listeners
_useEventListeners: isc.Browser.isMobileIE,

captureEvent : function (object, nativeEventName, eventName, handler) {

    
    
    var wd = this.getWindow(),
        useEventListeners = this._useEventListeners;
        
    
    //var indirect = new Function(this._$event, this._$funcBody); 
    var indirect = isc._makeFunction(this._$event, this._$funcBody); 
    indirect._window = wd;
    indirect._handler = handler;
    //this.logWarn("indirect created: " + indirect.toString());
    var nativeEventType;
    
    if (!useEventListeners) {
        // If there's a handler on the document object already, fire it synthetically from
        // our handler, so we don't clobber functionality from other JS loaded before us.
        
        if (object[nativeEventName] != null) {
            var nativeEventType = this._nativeEventName_TypeMap[nativeEventName] || nativeEventName.substring(2);
            this._replacedEvents[nativeEventType] = object[nativeEventName];
        }
        
        object[nativeEventName] = indirect;

    //    Using addEventListener / attachEvent rather than assigning directly to
    //    document.onXXX:
    // By default we assign handlers directly to document.onXXX [or window.onXXX].
    // This means that we overwrite any previously defined handlers, and if code executed
    // after the framework loads could also clobber us.
    // We overrwrite pre-existant handlers 'politely' - that is we fire our handler, then, if
    // we didn't return false, fire the pre existant handler with the correct parameters and
    // return the appropriate return value.
    // However this doesn't give us any protection against later code overwriting our onXXX
    // handlers.
    // By contrast we could use "eventListeners", created by document.addEventListener() [or
    // proprietary "attachEvent" in IE]. Events can support multiple listeners, and they
    // can't be clobbered by overwriting document.onXXX.
    // We activate this via the isc_useEventListeners flag.
    // ----
    // Problem with attachEvent/addEventListener as an interop strategy: when a handler
    // cancels an event, the other registered handlers fire anyway.
    //
    // IE: If we are using 'attachEvent' the raw 'onXXX'handler will fire before the 
    // eventListener, and we have no way of knowing whether the handler canceled the event, as
    // event.returnVal is always reported as undefined (even if it has been explicitly set to 
    // false).
    // Hence frameworks have no way of signalling to each other that they have completely 
    // handled an event, and problems like doubled context menus might result.
    // Note: attachEvent and addEventListener are both supported in IE9
    // [running in "IE9" mode]
    //
    // Moz: If we are using 'addEventListener', we have the option of firing before or after the 
    // native onXXX handler fires - this is governed by the third parameter passed to 'attachEvent'. 
    // If we fire first we can call preventDefault() to cancel native behavior but this will not 
    // stop a handler assigned to document.onXXX from firing.
    // A method 'getPreventDefault()' exists on the event object which allows the later handler
    // to detect a previous cancellation so we would be relying on some other library checking 
    // the presence of this flag.
    // Alternatively, if we fire after some other framework's handler, we can check for that 
    // handler having cancelled the native event. The only drawback in this case is that since 
    // our logic fires later we're essentially giving precidence to the other library's handler 
    // - if they show a context menu and kill the event we will not show ours, etc.
    //
    // Safari: Behaves like Moz, with the exception that while event.getPreventDefault() is not
    // available on the event object, you CAN check event.returnValue == false for a previous
    // cancellation.
    //
    // - By default we use "Polite clobbering of the direct handler function" - if a direct 
    // handler was defined before our library loads, we retain the handler, then when the
    // event occurs, we fire our logic first, and if we did not cancel the event, fire the 
    // original handler and propogate its return value back to the browser.
    // This generally achieves interop assuming the SmartClient framework is loaded after
    // any other frameworks on the page.
    // - if ISC can't be loaded last, or the direct handler gets clobbered after loading,
    // the developer can flip on 'isc_useEventListeners' for the application and hope for the
    // best in terms of doubled events (most likely only a problem in IE).
    // --
    // Order of firing:
    // - order (tested with oncontextmenu)
    //   - IE (6 and 7): window.onXXX first, then listeners in what appears to be random order
    //                   [tested with 4 "attachEvent()" calls in addition to setting 
    //                    document.onXXX]
    //   - Moz (1.5.0.3): - with 3rd 'useCapture' param passed to attachEvent():
    //                      listeners first registered -> last registered, then window.onXXX 
    //                    - with 'useCapture' false:
    //                      window.onXXX then listeners first registered -> lastRegistered
    //   - Safari (2.0.3): As with Moz 
    //
    // W3c spec for event listeners: http://www.w3.org/TR/DOM-Level-2-Events/events.html#Events-EventListener 
    } else {        

        if (isc.Browser.isIE && isc.Browser.version < 11) {
            object.attachEvent(nativeEventName, indirect);
        } else if (isc.Browser.isDOM) {

            nativeEventType = this._nativeEventName_TypeMap[nativeEventName] || 
                                  nativeEventName.substring(2);
                                  
            object.addEventListener(nativeEventType, indirect, false);
    
        
        } else {
            this.logWarn("Unable to use event listeners in this browser");
            this._useEventListeners = false;
            return this.captureEvent(object, nativeEventName, eventName, handler);
        }
    }
    
    
    
    if (object === wd.document) {
        var propToRelease = (!useEventListeners || isc.Browser.isIE) ? 
                                                   nativeEventName : nativeEventType;
        this._documentEventHandlers[propToRelease] = indirect;
    }
    
    
},

// Set the page up to capture events that we care about.  
// Called automatically right below its definition.
captureEvents : function (wd) {
    var EH = this;
	
    // Convert the public flag to use event listeners rather than directly specifying
    // handlers at the docment level to an internal flag
    if (window.isc_useEventListeners != null) EH._useEventListeners = window.isc_useEventListeners;
    
    
    var reverseEventTypes = isc.makeReverseMap(EH.eventTypes);
    isc.addProperties(EH, {reverseEventTypes:reverseEventTypes});

	if (wd == null) wd = this.getWindow();
	var document = wd.document;
    
	// add a 'load' handler to set up the Page.isLoaded parameter
	isc.Page.setEvent(EH.LOAD, isc.Page.finishedLoading);

    // DOM standard addEventListener and IE's proprietary attachEvent are mechanisms for doing
    // "event listening" - getting notified of events without assigning a function directly to a DOM
    // element.
    // We support both approaches in captureEvent() - switchable via the isc_useEventListeners
    // flag. 
    // See comments in EH.captureEvent() for more on this.
    // Since the "load" event can't be cancelled, and since ISC is hosed if it never gets "load"
    // (eg, nothing responds to events, various actions will be indefinitely postponed), we're using
    // event listening for "load" even if the flag is set to false. 
    // Note:
    //   - IE & Moz: only window.onload fires, not document or document.body onload
    //   - Safari: document, body, and window.onload all fire
    if (isc.Browser.isIE && isc.Browser.version < 11) {
        wd.attachEvent("onload", EH.handleLoad);
    // HACK: Opera: addEventListener for load fires load way too early, during ISC module load,
    // not clear why yet.
    } else if (isc.Browser.isDOM && !isc.Browser.isOpera) {
        wd.addEventListener("load", EH.handleLoad, true);
    } else {
        this.captureEvent(wd, "onload", EH.LOAD, EH.handleLoad);
    }

    
    if (!this._useEventListenerForUnload()) {
        this.captureEvent(wd, "onunload", EH.UNLOAD, EH.handleUnload);
    }

    this.captureEvent(wd, "onresize", EH.RESIZE, EH.handleResize);
    
    // { iscEventName : [DOMObject, nativeName, EHFunction], .. }
    
    this.captureEvent(document, "onmousedown", EH.MOUSE_DOWN, EH.handleMouseDown);
    this.captureEvent(document, "onmousemove", EH.MOUSE_MOVE, EH.handleMouseMove);
    this.captureEvent(document, "onmouseup", EH.MOUSE_UP, EH.handleMouseUp);

    this.captureEvent(document, "onclick", EH.CLICK, EH.handleNativeClick);
    this.captureEvent(document, "ondblclick", EH.DOUBLE_CLICK, EH.handleNativeClick);

    
    this.captureEvent(document, "onscroll", "scroll", EH.handleScroll);
     
    // In IE6 we can capture the mousewheel event
    this.captureEvent(document, "onmousewheel", EH.MOUSE_WHEEL, EH.handleMouseWheel);
    
    // In Moz we also get events on mouse-wheel, but we have to capture them differently.
    if (isc.Browser.isMoz) {
        wd.addEventListener("DOMMouseScroll", EH.handleDOMMouseScroll, true);
    }
    

	// for all Canvii, we synthesize mouseOver/mouseOut by detecting that the target Canvas
    // changed.  However, we care about native mouseOut events in the case that the mouse
    // leaves the browser entirely.
    //this.captureEvent(document, "onmouseover", EH.MOUSE_OVER, EH.killEvent);
    this.captureEvent(document, "onmouseout", EH.MOUSE_OUT, EH.handleNativeMouseOut);

	// get the contextMenu trigger
    this.captureEvent(document, "oncontextmenu", EH.SHOW_CONTEXT_MENU, EH.handleContextMenu);

	// suppress the onselectstart event in a special way
	//	so we can still operate in form fields
    this.captureEvent(document, "onselectstart", EH.SELECT_START, EH.handleSelectStart);
    this.captureEvent(wd, "onselectstart", EH.SELECT_START, EH.handleSelectStart);

    
    if (isc.Browser.isIE) {

        
        this.captureEvent(document, "onselectionchange", 
                          EH.SELECTION_CHANGE, EH.handleSelectionChange);

        
        
    }
    
    if (wd.isc_captureKeyEvents != false) {
        
        this.captureEvent(document, "onkeydown", EH.KEY_DOWN, EH._handleNativeKeyDown);
        this.captureEvent(document, "onkeypress", EH.KEY_PRESS, EH._handleNativeKeyPress);
        this.captureEvent(document, "onkeyup", EH.KEY_UP, EH._handleNativeKeyUp);
        
	}

    if (isc.Browser.hasNativeDrag || isc.Browser.isIE) {
        // ondragStart
        this._windowDragStart = wd.ondragstart;
        this._documentDragStart = document.ondragstart;
        wd.ondragstart = document.ondragstart = null;
        this.captureEvent(document, "ondragstart", EH.DRAG_START, EH.handleNativeDragStart);
    }

    if (isc.Browser.hasNativeDrag) {
        this.captureEvent(document, "ondragend", EH.DRAG_STOP, EH._handleNativeDragEnd);

        this.captureEvent(document, "ondragenter", EH.DROP_OVER, EH._handleNativeDragEnter);
        
        this.captureEvent(document, "ondragover", EH.DROP_MOVE, EH._handleNativeDragOver);
        this.captureEvent(document, "ondragleave", EH.DROP_OUT, EH._handleNativeDragLeave);
        this.captureEvent(document, "ondrop", EH.DROP, EH._handleNativeDrop);
    }

    if (isc.Browser._supportsCSSTransitions) {
        document.addEventListener(isc.Browser._transitionEndEventType, EH._handleTransitionEnd, false);
    }

	// IE specific 'help' event
	if (isc.Browser.isIE) {
        // onhelp (invoked from f1 keypress only). See comments about keypress handling/cancelling
        // native behavior for why we capture this.
        this._windowOnHelp = wd.onhelp;
        this._documentOnHelp = document.onhelp;
        document.onhelp = wd.onhelp = EH.handleNativeHelp;

        
	}

    //>Touch
    if (isc.Browser.isTouch) {
        // initialize mini state machine used for firing synthetic mousedown/mouseup on Android
        this._handledTouch = EH._touchEventStatus.READY_FOR_TOUCH;

        if (isc.Browser.isMobileIE) {
            this.captureEvent(document, "pointerdown", EH.TOUCH_START, EH._handleTouchStart);
            this.captureEvent(document, "pointermove", EH.TOUCH_MOVE, EH._handleTouchMove);
            this.captureEvent(document, "pointerup", EH.TOUCH_END, EH._handleTouchEnd);
            this.captureEvent(document, "pointercancel", EH.TOUCH_CANCEL, EH._handleTouchCancel);
        } else {
            this.captureEvent(document, "ontouchstart", EH.TOUCH_START, EH._handleTouchStart);
            this.captureEvent(document, "ontouchmove", EH.TOUCH_MOVE, EH._handleTouchMove);
            this.captureEvent(document, "ontouchend", EH.TOUCH_END, EH._handleTouchEnd);
            this.captureEvent(document, "ontouchcancel", EH.TOUCH_CANCEL, EH._handleTouchCancel);
        }
    } else //<Touch
    if (navigator.pointerEnabled) {
        

        this.captureEvent(document, "onpointerdown", EH.POINTER_DOWN, EH._handlePointerDown);
        this.captureEvent(document, "onpointermove", EH.POINTER_MOVE, EH._handlePointerMove);
        this.captureEvent(document, "onpointerup", EH.POINTER_UP, EH._handlePointerUp);
        this.captureEvent(document, "onpointercancel", EH.POINTER_CANCEL, EH._handlePointerCancel);
    }

    
    if (isc.Browser.isMobile) {
        isc.Page.pollPageSize = true;
    }

    // install browser specific routine to check if we need a synhetic keyPress on keyDown
    var helper = null;
    if      (isc.Browser.isMoz)    helper = isc.EH._mozFireKeypressOnKeyDown;
    else if (isc.Browser.isIE)     helper = isc.EH._ieFireKeypressOnKeyDown;
    else if (isc.Browser.isSafari) helper = isc.EH._safariFireKeypressOnKeyDown;
    if (helper) isc.EH.addClassMethods({ _fireKeypressOnKeyDown : helper });
},


_useEventListenerForUnload : function () {
    return (isc.Browser.isSafari && isc.Browser.safariVersion <= 412);
},

// releaseEvents
// Method fired when the page unloads - explicitly clear out event handlers applied to the
// document object

releaseEvents : function (wd) {
    var EH = this;

	if (wd == null) wd = this.getWindow();
	var document = wd.document,
        handlers = this._documentEventHandlers;

    for (var eventName in handlers) {
        if (!this._useEventListeners) {
            document[eventName] = null;
        } else {
            if (isc.Browser.isIE && isc.Browser.version < 11) {
                document.detachEvent(eventName, handlers[eventName]);
            } else if (isc.Browser.isDOM) {
                document.removeEventListener(eventName, handlers[eventName], false);
            }
        }
    }
    // special case for ondragstart which never went through captureEvents and got added to
    // our _documentEventHandlers map
    if (isc.Browser.hasNativeDrag || isc.Browser.isIE) {
        document.ondragstart = wd.onhelp = null;
    }
    if (isc.Browser.isIE) {
        // ditto for onhelp
        document.onhelp = wd.onhelp = null;
    }
    delete this._documentEventHandlers;
},




////////////////
// Cross-browser event property API
////////////////

//>	@classMethod	isc.EventHandler.getLastEvent()
//			Return the last event that was seen.
//
//		@group	mouseEvents
//		@return	(SCEvent) last event
//  @visibility eventhandler
//<
getLastEvent : function () {
	return this.lastEvent;
},

//>	@classMethod	isc.EventHandler.getEventType()
// Get the type of the event
//
//		@group	mouseEvents
//		@param	[event]	(SC Event) Event from a call to getEventProperties().  
//                                 Default is to use isc.EventHandler.lastEvent.
//		@return			(int)	event type 
//  @visibility eventhandler
//<
getEventType : function (event) {
	return (event || this.lastEvent).eventType;
},


//>	@classMethod	isc.EventHandler.getTarget()
// Return the canvas that is the target of the mouse event.
// Returns null if no canvas found.
//
// @group mouseEvents
// @return (Canvas)	event target canvas
// @visibility external
//<
//		@param	[event]	(SC Event) Event from a call to getEventProperties().  
//                                 Default is to use isc.EventHandler.lastEvent.
getTarget : function (event) {
	return (event || this.lastEvent).target;
},


//>	@classMethod	isc.EventHandler.getDragTarget() (A)
//
// Returns the current dragTarget.  This is the component on which the drag and drop
// interaction was initiated.  This only returns something meaningful during a drag and drop
// interaction.
//
// @group	mouseEvents
//
// @return			(Canvas)   The dragTarget.
//
// @see canvas.dragTarget
// @visibility external
//<
getDragTarget : function () {
    return this.dragTarget;
},


//>	@classMethod	isc.EventHandler.getX()
//			Return the page-relative X (horizontal) coordinate of an event.
//
//		@group	mouseEvents
//		@return			(int)	x-coordinate in page coordinate space
//  @visibility external
//<
//		@param	[event]	(ISC Event) Event from a call to getEventProperties().  
//                                 Default is to use isc.EventHandler.lastEvent.
getX : function (event) {
	return (event || this.lastEvent).x;
},

//>	@classMethod	isc.EventHandler.getY()
//			Return the page-relative Y (vertical) coordinate of an event.
//
//		@group	mouseEvents
//		@return			(int)	y-coordinate in page coordinate space
//  @visibility external
//<
//		@param	[event]	(ISC Event) Event from a call to getEventProperties().  
//                                 Default is to use isc.EventHandler.lastEvent.
getY : function (event) {
	return (event || this.lastEvent).y;
},


//>	@classMethod	isc.EventHandler.getScreenX()
//			Return the screen-relative X (horizontal) coordinate of an event.
//
//		@group	mouseEvents
//		@param	[event]	(SC Event) Event from a call to getEventProperties().  
//                                 Default is to use isc.EventHandler.lastEvent.
//		@return			(int)	x-coordinate in screen coordinate space
//  @visibility eventhandler
//<
getScreenX : function (event) {
	return (event || this.lastEvent).screenX;
},

//>	@classMethod	isc.EventHandler.getScreenY()
//			Return the screen-relative Y (vertical) coordinate of an event.
//
//		@group	mouseEvents
//		@param	[event]	(SC Event) Event from a call to getEventProperties().  
//                                 Default is to use isc.EventHandler.lastEvent.
//		@return			(int)	y-coordinate in screen coordinate space
//  @visibility eventhandler
//<
getScreenY : function (event) {
	return (event || this.lastEvent).screenY;
},


//>	@classMethod	isc.EventHandler.mouseIsDown()
//		Is the mouse button currently down?
//		@return	(boolean)	true == mouse is currently down
//		@group	mouseEvents
//  @visibility eventhandler
//<
mouseIsDown : function () {
    return !!(this._mouseIsDown);
},

//>	@classMethod	isc.EventHandler.mouseDownTarget
//		Get the target of the last mouseDown event. May be null if the target has subsequently
//      been destroyed.
//		@return	(Canvas)	Canvas that got the mouseDown event, or null if not on a canvas
//		@group	mouseEvents
//<
mouseDownTarget : function () {
	return (this.mouseDownEvent ? this.mouseDownEvent.target : null);
},


//>	@classMethod	isc.EventHandler.getButtonNum()	(A)
//			Return the number of the button that was pressed:
//				1 == left mouse button (primary)
//				2 == right mouse button (secondary)
//			Tertiary mouse button is not supported as it not commonly found.
//
//		@group	mouseEvents
//		@param	event	(DOM event) DOM event object (as passed by isc.EventHandler)
//		@return			(int)	number of the mouse button	
//  @visibility internal
//<
// which mouse button was pressed?  primary=1, secondary=2	(that's all we support)

getButtonNum : function (event) {
	return (event || this.lastEvent).buttonNum;
},

//>	@classMethod	isc.EventHandler.leftButtonDown()
//			Returns true if the left mouse button is being pressed.
//
//		@platformNotes	Mac:
//		Macintosh platform generally has only one mouse button - ISC considers it to be the "left"
//		mouse button, so this method will return true if the mouse is down on a single-button mouse Mac.
//
//		@group	mouseEvents
//		@return			(Boolean)	true == left button is down, false == up	
//      @visibility external
//<
//		@param	[event]	(ISC Event) Event from a call to getEventProperties().  Default is to use isc.EventHandler.lastEvent.
leftButtonDown : function (event) {
	return ((event || this.lastEvent).buttonNum == 1);
},

//>	@classMethod	isc.EventHandler.rightButtonDown()
//			Returns true if the right mouse button is being pressed.
//
//		@platformNotes	Mac:
//		Macintosh platform generally has only one mouse button, and the
//		control key being held down serves the same purpose of the 
//		right mouse button on Windows.  This is taken into account automatically.<br>
//      Opera: 
//      The Opera browser does not pass right mouse button events to JavaScript code by default
//      (the user must explicitly enable this behavior via a menu item). Therefore we 
//      treat <b>Shift+Ctrl+Click</b> as a context click in Opera.
//
//		@group	mouseEvents
//		@return			(Boolean)	true == right button is down, false == up	
//      @visibility external
//<
//		@param	[event]	(ISC Event) Event from a call to getEventProperties().  
//                                  Default is to use isc.EventHandler.lastEvent.
rightButtonDown : function (event) {
	if (!event) event = this.lastEvent;
	return (event.buttonNum == 2) || (event.button == 2) || 
           (isc.Browser.isMac && event.ctrlKey) || 
           // Notes:
           // We use shift+ctrl+click because
           // - ctrl + click has native meaning - shows a save-as dialog if it occurs over
           //   an image
           // - alt + click has meaning - it puts focus onto the native browser menus
           // we're unable to suppress either of these
           (isc.Browser.isOpera && (event.ctrlKey && event.shiftKey)) || 
           // Note, for early Safari, we get no event on ctrl+click nor on right button click
           // so if the altKey is down, we'll assume treat this as a context click
           ((isc.Browser.isSafari && (isc.Browser.safariVersion < 125)) && event.altKey);

},

// In Nav, early Safari, and current Opera, we don't get real context menu events, so we 
// have to synthesize them in response to mouseDown / mouseUp events.
useSyntheticRightButtonEvents : function () {
    return isc.Browser.isOpera ||
                
                (isc.Browser.isSafari && (isc.Browser.safariVersion < 125));    
},

// Which key was pressed (for keyboard events)

//>	@classMethod	isc.EventHandler.getKeyEventCharacterValue()
//          Returns the numeric characterValue reported by the browser.
//          Only available on keyPress events, and only for character (or ascii control) keys
// @return (int) Numeric character value reported by the browser 
//                  (ASCII value of the key pressed)
// @group	keyboardEvents
//  @visibility external
//<
getKeyEventCharacterValue : function (event) {
	return (event || this.lastEvent).characterValue;
},

//>	@classMethod	isc.EventHandler.getKeyEventCharacter()
//			Return the character for the current key being pressed.
//			Note that this is only set reliably for keyPress events on character keys.
//                  
// @return (string) Character the user entered. May be null for non-character keys. 
//		@group	keyboardEvents
//      @visibility external
//<
getKeyEventCharacter : function (event) {
	return String.fromCharCode(this.getKeyEventCharacterValue(event));
},

//>	@classMethod	isc.EventHandler.getKey()
//			Return the name of the key for the event passed in.
//			Note that this is only set reliably for keyboard events.
//
//		@group	keyboardEvents
//		@return			(KeyName)		Key Name
//      @visibility external
//<
//		@param	[event]	(SC Event)  Event to return keyName for
//                                  Default is to use isc.EventHandler.lastEvent.
getKey : function (event) {
    return (event || this.lastEvent).keyName || null;
},

// Add getKeyName() as a synonym of getKey() since we refer to the property as event.keyName
getKeyName : function (event) {
    return this.getKey(event);
},

//>	@classMethod	isc.EventHandler.shiftKeyDown()
//			Return true if the shift key is being held down.
//			Note that this is only set reliably for keyboard events.
//
//		@group	keyboardEvents
//		@return			(Boolean)	true == shift key is down	
//      @visibility external
//<
//		@param	[event]	(ISC Event) Event from a call to getEventProperties().  
//                                  Default is to use isc.EventHandler.lastEvent.
shiftKeyDown : function (event) {
	return !!((event || this.lastEvent).shiftKey);
},

//>	@classMethod	isc.EventHandler.ctrlKeyDown()
//			Return true if the control key is being held down.
//			Note that this is only set reliably for keyboard events.
//
//		@group	keyboardEvents
//		@return			(Boolean)	true == control key is down	
//      @visibility external
//<
//		@param	[event]	(ISC Event) Event from a call to getEventProperties().  
//                                  Default is to use isc.EventHandler.lastEvent.
ctrlKeyDown : function (event) {
	return !!((event || this.lastEvent).ctrlKey);
},

//>	@classMethod	isc.EventHandler.altKeyDown()
//			Return true if the alt (option) key is being held down.
//			Note that this is only set reliably for keyboard events.
//
//		@group	keyboardEvents
//		@return			(Boolean)	true == alt key is down
//      @visibility external
//<
//		@param	[event]	(ISC Event) Event from a call to getEventProperties().  
//                                  Default is to use isc.EventHandler.lastEvent
altKeyDown : function (event) {
	return !!((event || this.lastEvent).altKey);
},


//>	@classMethod	isc.EventHandler.metaKeyDown()
//			Return true if the meta (windows or apple) key is being held down.
//			Note that this is not supported in all versions of IE.
//			Note that this is only set reliably for keyboard events.
//
//		@group	keyboardEvents
//		@return			(Boolean)	true == meta key is being held down	
//  @visibility internal
//<
//		@param	[event]	(SC Event) Event from a call to getEventProperties().  
//                                 Default is to use isc.EventHandler.lastEvent.

metaKeyDown : function (event) {
	return !!((event || this.lastEvent).metaKey);
},

//>	@classMethod	isc.EventHandler.modifierKeyDown()
//			Return true if the control key (windows) or command/apple key (apple) 
//          is being held down.
//			Note that this is not supported in all versions of IE.
//			Note that this is only set reliably for keyboard events.
//
//		@group	keyboardEvents
//		@return			(boolean)	true == control/command key is being held down
//  @visibility internal
//<
//		@param	[event]	(SC Event) Event from a call to getEventProperties().  
//                                 Default is to use isc.EventHandler.lastEvent.

modifierKeyDown : function (event) {
    if (isc.Browser.isMac) return !!((event || this.lastEvent).metaKey);
    else                   return !!((event || this.lastEvent).ctrlKey);
},

//>	@classMethod	isc.EventHandler.getMouseEventProperties()
//		Record the characteristics of a mouse event in the object passed in.
//
//		Sets the following properties:
//			eventType		type of the event
//			nativeTarget	DOM element that was the target of the event
//			target			Canvas that was the target of the event (may be null)
//			x 				page-level X coordinate
//			y				page-level Y coordinate
//			screenX			X coordinate relative to the top corner of the screen.
//			screenY			Y coordinate relative to the top corner of the screen.
//			buttonNum 		Mouse button pressed.  <code>null</code> == mouse button not pressed.
//
//		Note: we don't try to make element-relative coordinates (offsetX/Y) available, since
//            the HTML element that catches the event may not be the HTML element that
//            represents the Canvas that will handle the event.  For example, the DOM-level
//            target could be a table cell, ultimately contained within a DIV which represents
//            a Canvas.
//
//            Use
//					canvas.getOffsetX(), canvas.getOffsetY()
//            to get the coordinates of the event relative to the Canvas which is regarded as
//            the receiver of the event within ISC.
//
//		@group	events
//		@param	e		(DOM event) DOM event object (as passed by isc.EventHandler)
//      @visibility internal
//<
getMouseEventProperties : (isc.Browser.isIE ?
    function (e) {    
        var scEvent = this.lastEvent;
    if (!e) e = this.getWindow().event;
   
        scEvent.DOMevent = e;        
   
        scEvent.eventType = this._nativeMouseEventMap[e.type];

        scEvent.y = parseInt(e.clientY) + this.ns.Page.getScrollTop();
        scEvent.x = parseInt(e.clientX);

        if (!isc.Page.isRTL()) {
            scEvent.x += this.ns.Page.getScrollLeft();
        } else {
            


            
            if (isc.Browser.isIE && isc.Browser.version <= 7 && isc.Browser.isStrict &&
                isc.Page.getBodyOverflow() != isc.Canvas.HIDDEN)
            {
                if (!this._pageScrollbarThickness) {
                    
                    if (isc.Browser.version <= 6) {
                        this._pageScrollbarThickness = document.documentElement.offsetWidth - document.documentElement.clientWidth;

                    } else if (isc.Browser.version <= 7) {
                        this._pageScrollbarThickness = document.documentElement.offsetWidth - document.body.offsetWidth;

                    } else {
                        this._pageScrollbarThickness = document.body.offsetWidth - document.body.clientWidth;
                    }
                }
                scEvent.x -= this._pageScrollbarThickness;
            }
        }

        scEvent.nativeTarget = e.srcElement;

        //this.logWarn(this.echoDOM(scEvent.nativeTarget));
        
        // Hang onto the wheelDelta - only defined for mouseWheel events
        
        var delta = e.wheelDelta;
        if (delta != null) {
            scEvent.wheelDelta = - delta/120;
            scEvent.wheelDeltaY = scEvent.wheelDelta;
            scEvent.wheelDeltaX = 0;
        } else {
            scEvent.wheelDelta = null;
            scEvent.wheelDeltaX = null;
            scEvent.wheelDeltaY = null;
        }
        
        scEvent.screenX = e.screenX;
        scEvent.screenY = e.screenY;
        
        if (isc.Browser.isIE11) {
            // For a 'pointerup' or 'mouseup' event, `buttons' is 0 because no mouse button is being pressed.
            if (scEvent.eventType === isc.EH.POINTER_UP ||
                scEvent.eventType === isc.EH.MOUSE_UP)
            {
                if (e.button == 0) scEvent.buttonNum = 1;
                else if (e.button == 1) scEvent.buttonNum = 4;
                else if (e.button == 2) scEvent.buttonNum = 2;
                else scEvent.buttonNum = 0;

            } else {
                scEvent.buttonNum = e.buttons;
            }

        } else {
            scEvent.buttonNum = e.button;
        }

        // getKeyEventProperties (fired when a key goes down) should handle updating shiftKey
        // et al 
        // However if a native alert was fired in response to a key down event we don't get
        // the native keyUp, so will never clear these properties out. Avoid this by resetting
        // these properties on mouse events as well as key events.
        scEvent.shiftKey = (e.shiftKey == true);
        scEvent.ctrlKey = (e.ctrlKey == true);
        scEvent.altKey =  (e.altKey == true);
        scEvent.metaKey =  (e.metaKey == true);
        
        scEvent.target = this.getEventTargetCanvas(e, scEvent.nativeTarget, scEvent);

        /*
        this.logWarn("event: " + this.echo({
            type : e.type,
            button : e.button,
            target : e.srcElement,
            clientX : e.clientX,
            clientY : e.clientY,
            screenX : e.screenX,
            screenY : e.screenY
        }));
        */
        
        return scEvent;
    }
:   // isc.Browser.isDOM 
    function (e) {

        var scEvent = this.lastEvent;
        scEvent.DOMevent = e;
        scEvent.eventType = this._nativeMouseEventMap[e.type];

        var ignoreCoordinates = false;
        
        if (isc.Browser.isMobileWebkit) { 

            //>Touch
            if (isc.startsWith(scEvent.eventType, "touch")) {
                // e.touches is an array of event objects for each finger touching the screen.
                // Report the first finger's coordinates on the event object as a whole.
                if (scEvent.eventType == isc.EH.TOUCH_END) {
                    // "touchend" natively reports all coordinates as undefined or bogus (0) and
                    // has no e.touches Array.  This makes some sense since two or more fingers
                    // could have been touching the screen and so there is no single end coordinate
                    // to report.  In terms of this logic and mouse event handlers seeing analogous
                    // behavior to mouseUp, it means we need to avoid overwriting coordinates 
                    // recorded from the last touchstart / touchmove.  Native behavior noted on
                    // iPhone OS 3.2.
                    ignoreCoordinates = true;
                } else if (e.touches != null && e.touches[0] != null) {
                    var touch = e.touches[0];
                    // relative to element viewport 
                    scEvent.clientX = touch.clientX;
                    scEvent.clientY = touch.clientY;
                    // relative to screen
                    scEvent.screenX = touch.screenX;
                    scEvent.screenY = touch.screenY;
                    // relative to page (content start)
                    scEvent.x = touch.pageX;
                    scEvent.y = touch.pageY;

                    
                    if (isc.Browser.isIPad && isc.Browser.isMobileSafari && isc.Browser.iOSVersion == 7 &&
                        isc.Page.getOrientation() === "landscape")
                    {
                        var documentBody = this.getDocumentBody(),
                            bodyBCR = documentBody.getBoundingClientRect();
                        var realScrollTop = Math.max(0, (-bodyBCR.top) << 0);
                        
                        scEvent.screenY += ((-documentBody.scrollTop + realScrollTop) * isc.Page._getPageZoom()) << 0;
                        scEvent.y += -documentBody.scrollTop + realScrollTop;
                    }
                }

                /*
                 this.logWarn("native event: " + this.echo({
                 clientX : e.clientX,
                 clientY : e.clientY,
                 pageX : e.pageX,
                 pageY : e.pageY,
                 screenX : e.screenX,
                 screenY : e.screenY,
                 touchesLength : e.touches.length
                 }));
                 */
            } else if (isc.Browser.isAndroid && e.type == isc.EH.CLICK) {
                // Install valid coordinates for a native click so that same event
                // can be forwarded on and work properly for synthetic mouseDown/mouseUp.
                // See IDocument under EH.handleNativeClick() for explanation.
                scEvent.screenX = e.screenX;
                scEvent.screenY = e.screenY;

                scEvent.x = parseInt(e.clientX) + isc.Page.getScrollLeft(true);
                scEvent.y = parseInt(e.clientY) + isc.Page.getScrollTop();
            }
            //<Touch

        } else {
            scEvent.screenX = e.screenX;
            scEvent.screenY = e.screenY;

            

            
            if (scEvent.eventType != this.MOUSE_WHEEL) {
                var adjustForPageScroll = true; 
                scEvent.x = parseInt(e.clientX) + (adjustForPageScroll ? isc.Page.getScrollLeft(true)
                                                                        : 0);
                scEvent.y = parseInt(e.clientY) + (adjustForPageScroll ? isc.Page.getScrollTop()
                                                                        : 0);
            }

        } // end else on MobileWebkit

        scEvent.nativeTarget = e.target;

        scEvent._stillWithin = null; // clear cached result of stillWithinMouseDownTarget

        scEvent.target = this.getEventTargetCanvas(e, scEvent.nativeTarget, scEvent);
        
        
        if (scEvent.eventType == this.MOUSE_WHEEL) {            
            // deltaX / deltaY are available 
            var deltaY = e.wheelDeltaY,
                deltaX = e.wheelDeltaX,
                wheelDelta = e.wheelDelta,
                delta = wheelDelta,
                
                detail = e.detail,
                axis = e.axis;

            // If deltaX / deltaY are available, use them!             
            if (deltaY != null || deltaX != null) {
                
                // this.logWarn("Reported wheelDeltaX/Y:" + [scEvent.wheelDeltaX,scEvent.wheelDeltaY]);
                scEvent.wheelDeltaX = deltaX == null ? 0 : - deltaX/120;
                scEvent.wheelDeltaY = deltaY == null ? 0 : - deltaY/120;
                // for backCompat
                scEvent.wheelDelta = scEvent.wheelDeltaY;
                
            } else {
                // No detail, no wheelDeltaX / Y, just use "wheelDelta"                
                if (detail == 0 || (detail == null && wheelDelta != null)) {
                
                    var delta = e.wheelDelta;
                    if (delta != null) {
                        // may be fractional - this is ok.
                        scEvent.wheelDelta = - delta/120;
                        scEvent.wheelDeltaY = scEvent.wheelDelta;
                        scEvent.wheelDeltaX = 0;
                    } else {
                        scEvent.wheelDelta = null;
                        scEvent.wheelDeltaX = null;
                        scEvent.wheelDeltaY = null;
                    }
                    
                // No helpful "deltaX"/"deltaY", but we have event.detail, so use it!
                } else {
                    if (isc.Canvas.useNativeWheelDelta && e.detail == e.SCROLL_PAGE_UP) {
                        scEvent.wheelDelta = -Math.floor(
                            scEvent.target.height/isc.Canvas.scrollWheelDelta);
                    } else if (isc.Canvas.useNativeWheelDelta && e.detail == e.SCROLL_PAGE_DOWN) {
                        scEvent.wheelDelta =  Math.floor(
                            scEvent.target.height/isc.Canvas.scrollWheelDelta);
                    } else {
                        var vertical = axis == null || axis == 2;
                        // delta is a fraction, so that wheelDelta==1 when e.detail=3, but
                        // wheelDelta=0.333 when e.detail==1. This matches the earlier behavior
                        // for 3-line scrolling.
                        var delta = e.detail/3;
                        if (!isc.isA.Number(delta)) delta = 0;
                        if (vertical) {
                            scEvent.wheelDelta = delta;
                            scEvent.wheelDeltaY = delta;
                            scEvent.wheelDeltaX = 0;
                        } else {
                            scEvent.wheelDelta = 0;
                            scEvent.wheelDeltaY = 0;
                            scEvent.wheelDeltaX = delta;
                        }
                    }
                }
            }
        
        } else {
            scEvent.wheelDelta = null;
            
            scEvent.wheelDeltaX = null;
            scEvent.wheelDeltaY = null;
        }

        
		if (scEvent.eventType == isc.EH.MOUSE_MOVE || scEvent.eventType == isc.EH.TOUCH_MOVE) {
			// clear the button if the mouse is not down
			if (!this._mouseIsDown) scEvent.buttonNum = 0;

        // otherwise, a mouseDown/Up event - there's a button down, so which is it?

        //>Touch
		} else if (isc.Browser.isTouch) {
            if (e.targetTouches && e.targetTouches.length > 1) {
                // treat two fingers on a single target as context click by default
                scEvent.buttonNum = 2;
            } else {
                scEvent.buttonNum = 1;
            }
    
        //<Touch
		} else {
            
			scEvent.buttonNum = ((e.which == 1 || isc.Browser.isSafari && e.which == 65536) 
                                                    ? 1 : 2);
		}
        //this.logWarn("event: " + e.type + " which: " + e.which + 
        //             " e.button: " + e.button + ", scEvent.buttonNum: " + scEvent.buttonNum);
        //scEvent.nativeWhich = e.which;

        
		scEvent.shiftKey = (e.shiftKey == true);
		scEvent.ctrlKey = (e.ctrlKey == true);
		scEvent.altKey = (e.altKey == true);
		scEvent.metaKey = (e.metaKey == true);
        /*
        this.logWarn("event: " + this.echo({
            type : e.type,
            button : scEvent.buttonNum,
            target : e.target,
            clientX : e.clientX,
            clientY : e.clientY,
            screenX : e.screenX,
            screenY : e.screenY
        }));
        */

		//this.logWarn("event: " + e.type + "\r\n" + this.echoEvent(e));
        return scEvent;
    }
),

//>	@classMethod	isc.EventHandler.getKeyEventProperties()
//		Record the characteristics of a keyboard event
//
//		Sets the following properties on isc.EventHandler.lastEvent:
//			nativeKeyTarget	DOM element that was the target of the event
//          keyTarget       Target Canvas for the key event - matches the item that currently
//                          has focus
//			keyName			Name of the key pressed. (available on keyDown, keyUp and keyPress)
//          characterValue  Numeric character value reported by the key event.  Only recorded
//                          for keypress events - may be null or zero for non-character keys.
//
//			shiftKey		Shift key is currently down.  
//			ctrlKey			Control key is currently down.
//			altKey			Alt key is currently down.  
//			metaKey 		Meta key is currently down. 
//								Note: meta-key seems to not work on most platforms
//          eventType       Type of the event (keyDown, keyPress, etc.)
//
//		@group	events
//		@param	e		(DOM event) DOM event object (as passed by isc.EventHandler)
//  @visibility internal
//<

getKeyEventProperties : function (e) {

    if (e == null) e = this.getWindow().event;
        
    var scEvent = this.lastEvent;
    
    
    scEvent.nativeKeyTarget = (e.target || e.srcElement);
    scEvent.keyTarget = this._focusCanvas;     
    if (isc.isA && isc.DynamicForm && isc.isA.DynamicForm(this._focusCanvas)) {
        var itemInfo = isc.DynamicForm._getItemInfoFromElement(scEvent.nativeKeyTarget, 
                                                               scEvent.keyTarget);        
        if (itemInfo && itemInfo.item) scEvent.keyTarget = itemInfo.item;
    }

    scEvent.eventType = this.getKeyEventType(e.type);    
    //this.logWarn("getKeyEventProperties() - eventType " + scEvent.eventType +
    //              ", native key event target: " + scEvent.nativeKeyTarget +
    //              ", ISC calculated target: " + scEvent.keyTarget);

    // for keypresses only, record the character code (may be null or zero - assume the
    // developer knows what he's doing with this code)
    if (scEvent.eventType == this.KEY_PRESS) {
        scEvent.characterValue = this._determineKeyEventCharacterValue(e);
    }
    
    var keyName = this.determineEventKeyName(e);
    if (keyName != null) {
        scEvent.keyName = keyName;
    
    } else if (scEvent.eventType != isc.EH.KEY_PRESS) delete scEvent.keyName;

    

    
    scEvent.nativeKeyCode = e.keyCode;

    
	scEvent.shiftKey = (e.shiftKey == true || (isc.Browser.isMoz && scEvent.shiftKey));
    scEvent.ctrlKey = (e.ctrlKey == true);
	scEvent.altKey = (e.altKey == true);
	scEvent.metaKey = (e.metaKey == true);
    
    //this.logWarn("getKeyEventProperties() - keyName " + scEvent.keyName +
    //              ", ctrlKey: " + scEvent.ctrlKey +
    //              ", shiftKey: " + scEvent.shiftKey +
    //              ", altKey: " + scEvent.altKey);

},



getKeyEventType : function (nativeType) {
    if (!nativeType) return;
    return this._nativeKeyEventMap[nativeType];
},



// Return the charset value for the event.
// Note: returns null if we can't get a value, or if the value is zero (meaningless)
_determineKeyEventCharacterValue : function (DOMevent) {
    
     if (isc.Browser.isIE) return (DOMevent.keyCode || null);
     if (isc.Browser.isMoz) {
        return (DOMevent.which || null);
     }
     return (DOMevent.which || DOMevent.keyCode || null);
},
_$f1:"f1",
_$help:"help",
determineEventKeyName : function(DOMevent) {
    if (DOMevent == null) return;
    var keyCode = DOMevent.keyCode,
        which = DOMevent.which,
        EH = isc.EH,
        type = EH.getKeyEventType(DOMevent.type),
        windowEvent = this.getWindow().event
    ;

    // In IE we get have an onhelp handler tripped by f1 keypress only
    if (DOMevent.type == this._$help) return this._$f1;

    //this.logWarn("determineEventKeyName(): key properties to determine event: " +
    //             "keyCode:" + keyCode + 
    //             ", which:" + which + ", type:" + type);

    
    if (isc.Browser.isIE) {
        
        if (type == EH.KEY_DOWN || type == EH.KEY_UP) {
            return EH._virtualKeyMap[keyCode];
        }
        
        
        if (type == EH.KEY_PRESS) {
            // If we have the keyDown key name, just make use of it        
            var keyDownKeyName = EH._keyDownKeyNames[EH._keyDownKeyNames.length-1];
            if (keyDownKeyName != null) return keyDownKeyName;

            var name = EH._charsetValueToKeyNameMap[keyCode];
            
            if (!name && windowEvent && windowEvent.ctrlKey) {
                name = isc.EH._getKeyNameFromCtrlCharValue(keyCode);
            }
            return name;

        }
        
    
    } else if (isc.Browser.isMoz) {
        
        if (type == EH.KEY_DOWN || type == EH.KEY_UP) {
            return EH._virtualKeyMap[keyCode];

        } else if (type == EH.KEY_PRESS) {
        
            if (which == 0 && keyCode != 0) return EH._virtualKeyMap[keyCode];

            // If we have the keyDown key name, just make use of it
            var keyDownKeyName = EH._keyDownKeyNames[EH._keyDownKeyNames.length-1];
            if (keyDownKeyName != null) return keyDownKeyName;        
            
            
            if (keyCode == 0) {
                return EH._charsetValueToKeyNameMap[which];

            
            } else {
                return EH._virtualKeyMap[which];
            }

        }

    
    } else if (isc.Browser.isSafari) {
        if (type == EH.KEY_DOWN || type == EH.KEY_UP) {
            return EH._virtualKeyMap[keyCode];
        }
        
        var code = (which != null ? which : keyCode);
        if (code != null && code != 0) {        
            
            if (windowEvent && windowEvent.ctrlKey) {
                var keyName = isc.EH._charsetValueToKeyNameMap[code];
                if (keyName == null) {
                    
                    if ((isc.Browser.isChrome && isc.Browser.version < 33) ||
                        (!isc.Browser.isChrome && isc.Browser.version < 6))
                    {
                        if (code == 10) keyName = "Enter";
                        else keyName = isc.EH._getKeyNameFromCtrlCharValue(code);
                    } else {
                        keyName = isc.EH._getKeyNameFromCtrlCharValue(code);
                    }
                }
                return keyName;
            }

            
            var keyName = isc.EH._charsetValueToKeyNameMap[code];
            
            if (keyName == null) keyName = isc.EH._safariSpecialKeyPressMap[code]

            return keyName;            
            
        // Note: we are aware of a bug where keypress events on arrow keys give a keyCode of zero
        // in Safari. Return null here without logging a warning (below) 
        // - we have logic in the calling code to pick up the key name from the key-down 
        // event instead, so this is both known and worked around.
        } else if (type == this.KEY_PRESS) {
            return null;
        }
        
        

    
    } else {

        var code = which;
        
        // If which is null or zero, use keyCode (if it's not null or zero)
        if (code == null || (code == 0 && keyCode)) code = keyCode
        
        if (code != null) return isc.EH._charsetValueToKeyNameMap[code];
    }

    // Should never get here
    isc.Log.logWarn("EventHandler.determineEventKeyName(): Unable to determine key for '" + 
                    DOMevent.type + "' event. Returning null");
    return null;

},


_getKeyNameFromCtrlCharValue : function (ctrlCharValue) {

    if (ctrlCharValue == 30) return "6";
    if (ctrlCharValue == 31) return "-";

    return String.fromCharCode(ctrlCharValue + 64);

},

// Called from handleKeyUp
clearKeyEventProperties : function (keyName) {
	var scEvent = this.lastEvent;
    delete scEvent.eventType;
	delete scEvent.nativeKeyTarget;
	delete scEvent.characterValue;
    delete scEvent.keyName;
	delete scEvent.shiftKey;
	delete scEvent.ctrlKey;
	delete scEvent.altKey;
	delete scEvent.metaKey;
	// We should already have cleared the keyDownKeyNames entry as part of handleKeyPress 
	// (whether fired from native onkeypress or synthetically) for the key in question - this 
	// is really just a sanity check.
    this._keyDownKeyNames.remove(keyName);
},


// Destroyed targets
// We hang onto pointers to canvii in various places. Ensure these get cleared up when a canvas is
// destroyed, so we don't leak memory
// (Note many of these would be cleared on subsequent events in any case)
canvasDestroyed : function (canvas) {
    
    // if any clickMasks are showing, remove canvas from all clickMask "unmasked" lists
    if (this.clickMaskUp()) isc.EH.maskTarget(canvas);
    
    // if canvas was registered to receive the resize event, clear the registration 
    
    if (canvas._resizeID) isc.Page.clearEvent(canvas._$resize, canvas._resizeID);
    
    
    if (this.mouseDownEvent && this.mouseDownEvent.target == canvas)
        this.mouseDownEvent.target = null;
    if (this.lastClickTarget == canvas) this.lastClickTarget = null;
    if (this.lastEvent.target == canvas) this.lastEvent.target = null;
    if (this.lastEvent.keyTarget == canvas) this.lastEvent.keyTarget = null;
    if (this._focusCanvas == canvas) this._focusCanvas = null;
    if (this._delayedFocusTarget == canvas) this._delayedFocusTarget = null;
},

// ClickMask
// ------------------------------------------------------------------------------------------------
//  Basic behavior:
//  - Suppress mouse events such as 'mouseOver' et. al on targets that are not marked as unmasked.
//    This is typically a visual indication to the user that the target is masked.
//  - Fire clickAction on mouseDown on masked target.
//  - mode:    
//      o If "soft", hide the click mask on mouseDown on masked target, and allow the mouseDown
//        event to proceed.  [Use case: dismissing a pop up menu by clicking outside it]
//        * The mouseDown must be allowed to proceed as we don't want the click to be mysteriously
//          dropped in use cases like this one
//      o If "softCancel", dismiss the clickMask on mouseDown (as with a soft mask), but also 
//        cancel the mouseDown event if it occurred over a SmartClient widget.
//        [Use case: we use this in modal editing of ListGrids where stopOnerrors is true.
//         We can't use a true "hard" mask in this case because we wouldn't be able to have
//         the edit form items be unmasked without unmasking the whole grid and all it's 
//         ancestors].
//      o If "hard" prevent the mouseDown from reaching the intended target. (Also leave the click
//        mask in place, so future mouseDown's will fire the click action again).
//        [Use case: clicking outside a modal dialog box].
//       Hard masks: 
//       o Prevent interaction with native HTML written into masked widgets
//       o Disallow focus on masked targets via
//          - tabbing - (we do this by capturing tab keypresses)
//          - accessKey (removed from widget handle)
//          - programmatic - just remember the (attempted) focus target and re-focus when the
//            mask gets hidden
//  
//  Features:
//  - Layering / Stacking of multiple clickMasks.
//      Each call to showClickMask() returns a unique ID for the clickMask.
//      If showClickMask() is called multiple times, we create multiple stacked clickMask objects.
//      A click on a masked target will fire the click action of the topmost clickMask, and 
//      depending on the 'mode' property of the clickMask, fire the click action on the masks
//      underneath.
//      Clickmasks are hidden using hideClickMask(), which, if passed an ID parameter will hide only
//      the specified mask.
//      This nesting behavior is made necessary by use cases like the following:
//      - Date picker widgets are modal and dismissable - they show an soft clickMask which
//        whill hide the date-picker if the user clicks outside it.
//        They contain 'year' and 'month' menus, which are also modal and dismissable.  
//        If the user clicks outside the date-picker while it is showing it's year menu, both the
//        menu and the picker should be dismissed.
//        If the user clicks on the date-picker but outside the year menu (while it is visible),
//        the year menu should be dismissed and the date picker should receive a click event.
//        Therefore we need intelligent nesting of soft CM's where the masks track their
//        'unmaskedTargets' and click actions independantly.
//      - Modal window widgets can contain any other widget.  If a modal widow is shown containing
//        a data-bound listViewer, when the listViewer performs a server-side fetch, a click mask
//        is shown for the modal window, and another one for the server side fetch.
//        When the server side fetch returns, we should hide only the click-mask shown by the fetch.
//        Therefore we also need intelligent stacking of hard CM's (and the ability to 
//        hide clickMasks shown from specific 'showClickMask()' calls)
//  - unmasked targets:
//      Targets can be marked as unmasked either when the clickMask is instantiated (as an optional
//      third parameter to showClickMask()) or via a call to canvas.unmask() (falls through to
//      EventHandler.addUnmaskedTargets()).
//      Deprecated: The Canvas 'bringToFront()' method automatically adds widgets to the 
//      unmaskedTargets list of the formost clickMask - deprecated as of build 5.5, but 
//      still works.  Developers are encouraged to use unmask() instead.
//      Note that when a widget is 'unmasked' wrt a particular clickMask, it is effectively 
//      unmasked wrt any click masks underneath that one as well.
//      o If a "hard" clickMask is showing, we only support it having top-level
//        unmasked targets -- we can't support an unmasked child of a masked parent for
//        "hard" masks. [We DO support this for soft masks].
//       - the only known, cross-browser way to truly intercept all events, including events 
//         that might be received by handlers directly written into native elements, is to 
//         place a physical element ("screenspan") over the whole screen
//       - anything that is to be unmasked therefore has to be ready to have it's top-most 
//         element change zIndex to get above the screenspan, and this is generally not ok to 
//         for eg some widget nested deeply in a series of Layouts, since this might effectively 
//         cause a full-screen Layout to come to the front, occluding things that had been 
//         placed over it
//       - hard-unmasking a non-top-level widget comes about *only* for modal inline editing with
//         waitForSave *and* stopOnErrors set, which is a corner case where it would be acceptable 
//         to place limitations or require special coding to make masking work
//       - alternatives include:
//         o individually masking everything else on the screen by generating elements to place
//           on top (too slow)
//         o creating a top-level mask composed of 4 pieces with a rectangular opening for the
//           unmasked, non-top-level widget: complicated, especially if multiple widgets 
//           non-top-level widgets can become unmasked together, and they may move
//      o We never support an unmasked parent with a masked child widget.
//      o We always mask and unmask all peers (and descendants of peers) of a widget with 
//        the widget. Use cases include scrollbars, edges, shadows.
//
// Future enhancements:
//  - Component level clickMasks:
//      In Windows and other multiple-window desktop systems, a "clickMask" only extends to the
//      current Window.  For example if you open a menu within a window, other windows still respond
//      normally to mouseOver et al. 
//      At some point ISC may need to implement per-widget clickMasks as well.
// ------------------------------------------------------------------------------------------------



//> @type ClickMaskMode
// Passed as a parameter to +link{Canvas.showClickMask} to determine the masks behavior
// when clicked.
// @value "hard"   When the mask receives a click, it will fire its click action, 
//                  and cancel the event, leaving the clickMask up.
HARD:"hard",
// @value "soft"   When the mask receives a click, it will fire its click action, 
//                  then dismiss the clickMask and allow the event to proceed to its target.
SOFT:"soft",
// @group clickMask
// @visibility external
//<

SOFT_CANCEL:"softCancel",


//>	@classMethod	EventHandler.showClickMask()	(A)
//
// The clickmask intercepts all mouse events for everything on the screen except a list of 
// "unmasked" targets.  
// It is used in 2 ways:<br>
//  - To allow things such as modal dialogs which prevent interactions with widgets outside 
//    themselves<br>
//  - To allow things such as menus which respond to clicks outside themselves (without preventing
//    the event).<br>
// When a mouseDown occurs anywhere on the screen outside of an unmasked target, the clickMask's 
// "clickAction" fires.  <br>
// If the mask is soft it will disappear at this time. The event will be cancelled in this case
// unless mode is explicitly set to "softCancel" in which case we return false to kill the event.
// If the mask is hard, the event will be cancelled.<br>
// <br>
// This method returns an identifier for this clickMask.  If showClickMask() is called while a
// clickMask is already up, the clickmasks will 'layer', with the most recently shown clickMask 
// recieving click events first, and if  the mask is soft, passing the click event down to the
// previously shown mask.
// The clickMask can be hidden with a call to EventHandler.hideClickMask(), which takes an optional
// clickMaskID parameter. (If this parameter is not passed, all clickMasks will be hidden).
//
// @group   clickMask
// @param   clickAction     (string | method)   action to fire when the clickMask is clicked
// @param   mode    (ClickMaskMode)       
//      Should this mask be dismissed and allow events to proceed on outside click.
//      If passed <code>null</code> the mask will be drawn in <code>"hard"</code> mode.
// @param   unmaskedTargets (widget | Array of widgets)
//      Widget(s) to not be occluded by the clickMask. Note that if <code>mode</code> is 
//      <code>"hard"</code> only top level canvases (with no 
//      +link{Canvas.getParentCanvas()}) can be unmasked. If a canvas with a parentCanvas is passed
//      in, all its ancestors will also be unmasked.<br>
//      Also note that when a widget is unmasked, all its children are also unmasked (for both 
//      <code>"soft"</code> and <code>"hard"</code> masks)
//      
// @return  (string)    Unique identifier for this clickMask.
// @see method:Canvas.showClickMask
//
// @visibility eventhandler
//<
_maskCount : 0,
clickMaskRegistry : [],
showClickMask : function (clickAction, mode, unmaskedTargets, maskID) {
    // Set this flag to indicate showClickMask is running, even though
    // clickMaskUp won't yet return true
    
    this._showingClickMask = true;

    var autoHide;
    //>!BackCompat 2006.08.31 We used to take 'autoHide' rather than 'mode' as a parameter
    // for whether a clickMask should be hard or soft. 
    // If passed a boolean, just treat 'true' as soft, 'false' as hard.
    if (mode == true) {
        autoHide = true;
        mode = isc.EH.SOFT;
    } else if (mode == false || mode == null) {
        autoHide = false;
        mode = isc.EH.HARD;
    } else {
    //<!BackCompat
        // If mode is "soft" or "softCancel", autoHide is true
        autoHide = (mode != isc.EH.HARD);
    //>!BackCompat 2006.08.31
    }   //<!BackCompat

    if (unmaskedTargets == null) unmaskedTargets = [];
    else if (!isc.isAn.Array(unmaskedTargets)) unmaskedTargets = [unmaskedTargets]

    var EH = this,
        registry = EH.clickMaskRegistry,
        focusCanvas = EH.getFocusCanvas();
    
    if (this.logIsInfoEnabled("clickMask")) {
        this.logInfo("showing click mask, action: " + clickAction +
                 (autoHide ? ", autoHide true " : ", autoHide false ") + 
                 (maskID ? ", ID: " + maskID : "") +
                 ", focusCanvas: " + focusCanvas,
                 "clickMask");
    }

    // send mouse out the to the last mouse over target, so it doesn't get stuck in the "Over"
    // state while the clickMask is up, suppressing mouseMove/Over/Out
    // (Do this before the mask is up, otherwise this event won't be passed through to the 
    // last target!)
    var lastMoveTarget = EH.lastMoveTarget;
    if (lastMoveTarget) {
        delete EH.lastMoveTarget;
        EH.handleEvent(lastMoveTarget, EH.MOUSE_OUT);
    }

    // create a entry for this mask and add it to the registry
    
    var mask = {
        autoHide : autoHide,
        mode:mode,
        ID : (maskID != null ? maskID : "cm_" + EH._maskCount++),
        _unmaskedTargets : {}
        //,stackTrace:this.getStackTrace()
    };

    // Add the unmasked children to the mask object
    
    this._applyUnmaskedTargets(unmaskedTargets, mask);
    // remember what mask was on top before this one was created.
    var topMask = registry.last();
            
    registry.add(mask);
    // clear the flag indicating showClickMask is running 
    // [since "clickMaskUp" will now return true]
    delete this._showingClickMask;

    
    // the click action will fire on mouseDown outside of the unmasked targets
	mask.clickAction = clickAction;
	
    // blur the Canvas that currently has focus and remember which one it was, for possible
    // restoration of focus on clickMask hide
    if (focusCanvas != null && !unmaskedTargets.contains(focusCanvas) &&
        focusCanvas._ignoreClickMaskFocus) 
    {
        focusCanvas.blur("showing clickMask");
        this.setMaskedFocusCanvas(focusCanvas, mask);
    } else if (topMask != null) {
        this.setMaskedFocusCanvas(topMask._maskedFocusCanvas, mask);
    }
    var isHardMask = this.isHardMask(mask);
    
    // If this is a hard mask, we need to 
    // - pull all masked widgets out of the page level tab order
    // - If we're already showing a screenSpan, push masked top level elements behind it

    if (isHardMask) {
    
        var startTime = isc.timeStamp();
    
        // The canvii which will be hard-masked by this mask are basically all the canvii
        // down to the next visible hard clickMask.
        // If this is the only clickMask showing, or all the other masks are soft, this is
        // just all the canvii on the page other than our unmasked targets.
        
        var entireCanvasList;
        if (registry.length > 1) {
            var hasHardMaskBelow = false,
                otherMasks = [];
            // our position is registry.length-1, so start at registry.length -2
            for (var i = registry.length-2; i >=0; i--) {
                otherMasks.add(registry[i]);
                
                if (this.isHardMask(registry[i])) {
                    hasHardMaskBelow = true;
                    break;
                }
            }
            
            if (hasHardMaskBelow) {
                // the 'otherMasks' will list any masks below this one up to and including the
                // first hard mask - so their unmasked targets all need to get masked.
                for (var i = 0; i < otherMasks.length; i++) {
                    var newlyMasked = otherMasks[i]._unmaskedTargets;
                    if (newlyMasked) {
                        // pass in the flag to indicate that we're passing in an object
                        // rather than an array
                        // Also pass in the flag to indicate this is being called as part of showClickMask
                        this._hardMaskTargets(newlyMasked, unmaskedTargets, true, true);
                    }
                }
            // If no hard mask below, hard mask everything except our unmaskedTargets
            } else {
                entireCanvasList = true;
                this._hardMaskTargets(isc.Canvas._canvasList, unmaskedTargets, false, true);
            }
            
        // If no other mask showing, hard mask everything except our unmaskedTargets
        } else {
            entireCanvasList = true;
            this._hardMaskTargets(isc.Canvas._canvasList, unmaskedTargets, false, true);
        }
        
    }
    
    // If this is the first mask being shown, show the screenSpan if necessary
    
    if (this.maskNativeTargets) {

        if (topMask == null) {
            this.showScreenSpan(mask);

        // catch the case where the screenSpan is already showing and needs to be moved behind 
        // the top level unmasked target.
        } else if (isHardMask) {
            this._adjustSpanZIndex(mask._unmaskedTargets)
        }
    }

    // updateEventMasks()
    // Shows / Clears individual event masks over canvii if necessary
    this.updateEventMasks();
    
    
    return mask.ID;
},


// Canvas-level event masks and "soft" clickMasks:
// Some widgets contain HTML that will swallow mouse events
// (Flashlets / IFrames etc).
// If we are showing a hard mask ovr these widgets, they will now be covered by a screenSpan
// which will intercept the events before they get swallowed (ok).
// However if these widgets are "under" a soft mask, they would swallow clicks so we'd fail
// to dismiss the soft mask / fire the click mask action when the user clicked on the widget 
// in question.
// Handle this by showing widget level event masks for each registered maskable canvas
// that is covered by a soft mask (but not by a hard mask)
updateEventMasks : function () {
    var registry = this.clickMaskRegistry,
        topMask = registry ? registry[registry.length-1] : null;
    if (topMask && topMask.autoHide) {
        var softMaskedCanvii = {};
        isc.addProperties(softMaskedCanvii, topMask._unmaskedTargets);
        
        this.showEventMasks(false, softMaskedCanvii);
        
    // top mask is hard ==> no need for separate widget-level event masks
    // no masks are showing ==> clear any widget-level event masks
    } else {
        this.hideEventMasks();
    }
},



// Helper method to apply unmasked targets to a mask.
// Will not actually modify the widgets' handle.

_applyUnmaskedTargets : function (unmaskedTargets, mask) {
    
    // call the method to combine ancestors, descendants and peers of the targets into
    // the list.
    unmaskedTargets = this._getFullSetOfTargetsToUnmask(unmaskedTargets, mask);

    for (var i = 0; i < unmaskedTargets.length; i++) {
        var target = unmaskedTargets[i];
        if (target == null) continue;
        mask._unmaskedTargets[target.getID()] = target;
    }
},


// When we unmask canvases wrt a clickMask, we must also unmask:
// - for hard masks, all ancestors of the canvii
// - all peers of the canvii
// - all descendants of the canvii
// Call this method to add these additional canvii into the list to be unmasked
// Directly effects the list passed in (and returns the modified list)
_getFullSetOfTargetsToUnmask : function (unmaskedTargets, mask) {

    
    if (!unmaskedTargets || unmaskedTargets.length == 0 || !mask) return unmaskedTargets;
    
    // If we're passed any IDs, convert to pointers to the widget
    for (var i = 0; i < unmaskedTargets.length; i++) 
        unmaskedTargets[i] = this._getCanvas(unmaskedTargets[i]);

    // If we are creating a hard mask, ensure that we include all ancestors of any unmaskedTargets
    // passed in.
    var alreadyUnmasked = mask._unmaskedTargets;
    if (!mask.autoHide && unmaskedTargets.length > 0) {
        var length = unmaskedTargets.length;
        for (var i = 0; i < length; i++) {
            var target = unmaskedTargets[i];
            if (target.topElement && !alreadyUnmasked[target.topElement.getID()] && 
                                     !unmaskedTargets.contains(target.topElement)) 
            {      
                this.logWarn(
                    "Attempting to unmask target canvas:" +
                     target.getID() + " with respect to a hard click mask. " +
                     "This is not a top level Canvas - all ancestors of "+
                     "this Canvas will also be unmasked.", "clickMask"
                );
                unmaskedTargets.add(target.topElement);
            }
        }
    }

    // also recursively mask any peers of the targets.
    // Use cases: scrollbars, shadows, edge-canvii, etc.
    // - Note this is only necessary for the highest level targets being masked, since
    //   peers are at the same level in widget hierachy, so get masked (along with other 
    //   descendants) when the higher level parent is masked
    this._combineTopPeersIntoList(unmaskedTargets);
    
    // this method will modify the unmaskedTargets to include all descendants recursively.
    this._combineDescendantsIntoList(unmaskedTargets);
    
    return unmaskedTargets;
},


// Helper methods for determining targets to mask/unmask - takes a list of widgets and 
// recursively adds children of each widget to the list
_combineDescendantsIntoList : function (list) {
    var originalLength = list.length;
    for (var i = 0; i < originalLength; i++) {
        if (list[i] == null) continue;
        this._addDescendantsToList(list[i], list);
    }
},

// - Called directly from maskTargets(), addPeersToList, and _combineDescendantsIntoList()
_addDescendantsToList : function (widget, list, recursive) {
    
    // On the first (non-recursive) call we know that the widget is already in the list.
    if (recursive && !list.contains(widget)) list.add(widget);
    if (widget.children) {
        for (var i = 0; i < widget.children.length; i++) {
            this._addDescendantsToList(widget.children[i], list, true);
        }
    }
    
    // CanvasItems and containerWidgets
    // DynamicForm items can be rendered into a masked containerWidget, but the form itself be 
    // unmasked (we do this for ListGrid editing where the editorForm is unmaksed, but the
    // Listgrid body is masked so a click outside an edit item dismisses the editor).
    // Currently if the form contains any CanvasItems the canvasItems will NOT be unmasked in
    // this case since their parent is masked.
    // We need to handle this case if we want to support modalEditing and CanvasItems.
    // In order to handle this:
    // - when adding unmasked targets, if a DF is an unmasked target we ensure any CanvasItems'
    //   canvii are also unmasked even if they're contained in a different containerWidget
    // - in CanvasItem, when adding the canvas to the containerWidget as a child we explicitly check
    //   for the DF being unmasked and unmask explicitly if necessary.
    if (isc.DynamicForm && isc.CanvasItem && isc.isA.DynamicForm(widget)) {
        var items = widget.getItems() || [];
        for (var i = 0; i < items.length; i++) {
            if (items[i].containerWidget == widget) continue;
            if (isc.isA.CanvasItem(items[i]) && isc.isA.Canvas(items[i].canvas)) {
                this._addDescendantsToList(items[i].canvas, list, true);
            }
        } 
    }
    
},

// Helper methods for determining targets to mask/unmask - takes a list of widgets and 
// recursively adds peers (and descendents thereof) of the highest level widgets to the list
_combineTopPeersIntoList : function (list) {
    for (var i = 0, length = list.length; i < length; i++) {
        var t = list[i];
        if (t.parentElement && list.contains(t.parentElement)) continue;
        this._addPeersToList(list[i], list);
    }
},

// - Called directly from maskTargets() and _combineTopPeersIntoList()
_addPeersToList : function (widget, list, recursive) {
    if (recursive && !list.contains(widget)) list.add(widget);
    var peers = widget.peers;
    if (peers) {
        for (var i = 0; i < peers.length; i++) {
            this._addPeersToList(peers[i], list, true);
        }
    }
    
    // We also want to pick up descendants of peers.
    // (example might be a button with a floating label title)
    this._addDescendantsToList(widget, list);
},


// Given the ID of a click mask return a pointer to the mask object itself

_$ID:'ID',
getClickMask : function (ID) {
    var registry = this.clickMaskRegistry;
    if (isc.isAn.Object(ID)) {
        return registry.contains(ID) ? ID : null;
    }
    return registry.find(this._$ID, ID);
},

changeClickMaskID : function (oldID, newID) {
    var mask = this.getClickMask(oldID);
    if (mask) mask.ID = newID;
},

// is a specific click mask "hard" (IE should suppress events from passing through it) or "soft"
// (Should respond to clicks but not mask events).

isHardMask : function (mask) {
    if (!isc.isAn.Object(mask)) mask = this.getClickMask(mask);
    return mask == null ? false : (mask.mode == isc.EH.HARD);
},

getTopHardMask : function () {
    var registry = this.clickMaskRegistry;
    for (var i = registry.length -1; i >= 0; i--) {
        if (this.isHardMask(registry[i])) return registry[i];
    }
    return null;
},

// Given an array of widgets, ensure they are "hard masked".
// This means they are obscured by the screenSpan 
// [If we're in the process of showing a clickMask, this is handled by the calling method]
// Their accessKey is cleared, if necessary.
// no update to tabIndex is necessary
_hardMaskTargets : function (widgets, unmaskedTargets, targetsAsObject, fromShowClickMask) {
    if (!widgets) return;
    
    //this.logWarn("masking widgets: " + widgets + 
    //             ", unmaskedTargets is: " + this.echo(unmaskedTargets));

    // Allows us to pass in an 'unmaskedTargets' object on a mask which is a map like this:
    // {canvasName:true, canvasName:true, ...}
    if (targetsAsObject) {
        for (var canvasName in widgets) {
            var canvas = widgets[canvasName];
            this._hardMaskTarget(canvas, unmaskedTargets, fromShowClickMask);
        }
    } else {
        for (var i = 0; i < widgets.length; i++) {
            var canvas = this._getCanvas(widgets[i]);
            this._hardMaskTarget(canvas, unmaskedTargets, fromShowClickMask);
        }
    }

},

// Actually hard mask a target
_hardMaskTarget : function (canvas, unmaskedTargets, fromShowClickMask) {

    // handle the case where some member of the canvas list is not a valid canvas
    
    if (!isc.isA.Canvas(canvas) || canvas.destroyed) {
        isc.Log.logWarn(
            "showClickMask - attempting to remove invalid object :" +
             isc.Log.echo(canvas) +
            " from tab order",
            "clickMask"
        );
        return;
    }

    
    // Don't actually mask anything that's explicitly unmasked
    if (unmaskedTargets && unmaskedTargets[canvas.getID()]) return;


    
    if (canvas.isDrawn()) {
            
        // we just need to ensure that the top-parent of the masked canvii is behind the span
        // Note: we are guaranteed to have the top-parent in the unmasked canvii already, so skip
        // any non top level widgets
        if (canvas.getParentCanvas() == null) {
            // Widgets to show behind the screenspan - 2 possibilities:
            // - the mask is in the process of being shown:
            //  o If this is the only mask up, we haven't yet created the screenSpan, but when 
            //    we do we'll position it over all top level masked widgets 
            //  o If the span is already showing due to another mask below us, we'll adjust the
            //    zIndex at the end of the showClickMask method
            //  [Therefore no action to take in this case]
            // - this mask is already showing and this method was called from maskTarget(), so 
            //   we need to sink this widget explicitly behind the screenSpan.
            if (!fromShowClickMask && this._screenSpan && this._screenSpan.isDrawn() && 
                canvas.getZIndex() >= this._screenSpan.getZIndex()) 
            {
                //>DEBUG
                this.logDebug("lowering zIndex of: " + canvas, "clickMask");
                //<DEBUG

                
                canvas.setZIndex(isc.EH._screenSpan.getZIndex() -1);
            }

            
        }
    }
    
    // clear the accessKey from the canvas - note that we don't have to clear the
    // tabIndex as we explicitly manage tab-keypresses while the mask is up
    if (canvas.accessKey != null && canvas.isDrawn()) {
        canvas._setHandleAccessKey(null);
    }
},

    
//>	@classMethod	EventHandler.hideClickMask()	(A)
//  Hide the click mask.
// @param [ID]  (string)
//      Which clickMask to hide?  If not specified, hide all clickMasks.
// @group clickMask
// @see EventHandler.showClickMask()
// @see method:canvas.hideClickMask
// @visibility eventhandler
//<
hideClickMask : function (ID) {

    

    if (this.logIsInfoEnabled("clickMask")) 
        this.logInfo("hideClickMask called with ID: " + ID, "clickMask");
    
    var registry = this.clickMaskRegistry;
    // Ensure we have at least one CM showing
    if (registry.length == 0) return;

    // if there's no ID, just hide all clickmasks
    if (ID == null) {
    
        // Hide the first CM in the array
        this.hideClickMask(registry[0].ID)

        // call this method with no ID again (will hide the next item in the array, and so on)
        if (registry.length > 0) {
            this.hideClickMask();
        } else {
            this.logInfo("all clickmasks hidden", "clickMask");
        }
        return;
    }

    var mask = this.getClickMask(ID);

    // if we were passed a bad ID just return (the CM in question's probably already hidden!)
    if (mask == null) return;

    //this.logWarn("hiding mask that had unmasked targets: " + this.echo(mask._unmaskedTargets));

    // At this point we're working with a specific mask in the registry.

    // Get all the information we need from the clickMask being destroyed, and remove it from
    // the clickMaskRegistry        
    var index = registry.indexOf(mask),
        isTopMask = (index == (registry.length -1)),
        isHardMask = this.isHardMask(mask),
        nextMaskDown = (index > 0 ? registry[index -1] : null),
        isTopHardMask,
        nextHardMaskDown;
     
    if (this.logIsInfoEnabled("clickMask")) {
        var msg = "hiding clickMask ID: " + ID;
        if (isHardMask) msg += "[autoHide:false]";
        else msg += "[autoHide:true]";
        if (registry.length < 2) {
            msg += ", all masks hidden";
        } else {
            // report index
            msg += " with index: " + index + " of " + (registry.length - 1); 
        }
        this.logInfo(msg, "clickMask");
    } 
        
    // If this is a 'hard' clickMask with no hard mask on top of it, 
    // hard unmask any unmasked newly targets
    if (isHardMask) {
        nextHardMaskDown = this._getNextHardMask(index, false);
        var nextHardMaskUp = this._getNextHardMask(index, true);
        isTopHardMask = (nextHardMaskUp == null);
    }
    
    var focusCanvas = mask._maskedFocusCanvas,
        unmaskedTargets = mask._unmaskedTargets;
    // Actually remove the clickMask from the registry at this point
    
    registry.remove(mask);
              
    // At this point
    // - if we hid the topmost mask we need to restore focus to the previous focus widget
    // - if the mask we hid had another mask under it, we need to notify that mask of this
    //   mask's unmaskedTargets
    // - if we hid the only visible mask we need to hide the screenSpan
    // - if we hid a hard mask
    //    - if there's a hard mask below it, the screenspan should go behind that mask's
    //      unmaskedTargets
    //    - otherwise we need to shift the screenSpan to the back of the visible set of widgets.
    
    // If we have a 'nextMaskDown', update it with the focus canvas, etc.
    if (nextMaskDown != null) {
        
        if (unmaskedTargets != null) {
            if (nextMaskDown._unmaskedTargets == null) nextMaskDown._unmaskedTargets = {};
            isc.addProperties(nextMaskDown._unmaskedTargets, unmaskedTargets);
        }

        // We attempt to focus on the maskedFocusCanvas below... but if it's still masked
        // we can't.
        // Instead record the maskedFocusCanvas on the next mask down
        if (focusCanvas && !nextMaskDown._unmaskedTargets[focusCanvas.getID()]) {
            this.setMaskedFocusCanvas(focusCanvas, nextMaskDown);
        }
    }
    
    if (this._screenSpan) {
        // if this is the only mask showing, hide the screenspan
        // True whether soft or hard
        if (isTopMask && nextMaskDown == null) {   
            
            if (isc.Browser.isIE) {
                isc.Timer.setTimeout({target:this._screenSpan, methodName:"hide"}, 0);
            } else {
                this._screenSpan.hide();                
            }   
        
        // If we're hiding the top-most hard mask we need to reposition the screenspan
        } else if (isTopHardMask) {      
            if (nextHardMaskDown) {
                // slot the screenspan behind all unmasked targets down to the next hard-mask
                // (May be soft masks over that hard mask - have to put the screenspan behind those
                // unmasked targets)
                var unmaskedTargets = isc.addProperties({}, nextHardMaskDown._unmaskedTargets);
                var currentMaskIndex = registry.length-1,
                    currentMask = registry[currentMaskIndex];
                while (currentMask != nextHardMaskDown) {
                    isc.addProperties(unmaskedTargets, currentMask._unmaskedTargets);
                    currentMaskIndex--;
                    currentMask = registry[currentMaskIndex];
                }
                this._adjustSpanZIndex(unmaskedTargets);
            } else this._screenSpan.sendToBack();
        }
        
        // If this was a hard mask we need to ensure that all newly revealed targets
        // ("unmaskedTargets" of the mask below if there was one, otherwise all canvii)
        // have their accessKeys cleared
        
        if (isHardMask) {

            var unmasked;
            if (nextHardMaskDown != null) {
                unmasked = [];
                // iterate through each of the masks below us up to (and including) the 
                // hard mask below us, to determine which widgets will be no longer be 
                // hard-masked when this mask is hidden.
                for (var i = index-1; i >= 0; i--) {
                    var lowerMask = registry[i];
                    unmasked.addList(isc.getKeys(lowerMask._unmaskedTargets));
                    if (lowerMask == nextHardMaskDown) break;
                }
            } else {
                // everything is unmasked
                
                unmasked = isc.Canvas._canvasList;
            }

            // Call _hardUnmaskTargets to restore accessKeys.
            this._hardUnmaskTargets(unmasked, true);
        }
         
        // If we have a masked focus canvas, focus on it if it's unmasked
        if (focusCanvas != null && !focusCanvas.destroyed && !this.targetIsMasked(focusCanvas)) {
            if (this.logIsInfoEnabled("clickMask")) {
                this.logInfo("focusing in " + focusCanvas + " on clickMask hide " +
                             "with current focusCanvas: " + isc.EH._focusCanvas, "clickMask");
            }

                        
            var delayedFocus = (isc.Browser.isIE && this.lastEvent.eventType == this.MOUSE_DOWN)
            
            if (delayedFocus) {     
                this._delayedFocusTarget = focusCanvas;
            } else {
                // We've seen an "Unexpeced call to method or property access with the
                // following stack in IE:
                // Canvas.setFocus(_1=>true) Canvas.focus()
                // EventHandler?.hideClickMask(_1=>"isc_globalPrompt") 
                // Canvas.hideClickMask(_1=>undef) Window.clear(_1=>undef, _2=>undef, _3=>undef, _4=>undef) [a]Dialog.clearMessage() anonymous()           
                // RPCManager.doClearPrompt(_1=>Obj) RPCManager.$528(_1=>23) 
                //
                // so encase in try/catch block
                try {
                    focusCanvas.focus();
                } catch (e) {}
            }
        }
    }

    // updateEventMasks()
    // Shows / Clears individual event masks over canvii if necessary
    this.updateEventMasks();
    
    // If we hid the bottom mask, but other masks are showing on top of it, we currently
    // do nothing.
    // This is appropriate with 'unmaskedTargets', since nothing will be masked below
    // this widget.
    // We passed the 'maskedFocusCanvas' from up to the masks above it when we showed them (when
    // appropriate), so it should not matter that we're dropping the masked focus canvas for the
    // bottom mask.
},

// Given a clickMask in the registry determine the index of the next hard mask above or below
// it.
_getNextHardMask : function (maskIndex, above) {
    
    var registry = this.clickMaskRegistry;
        
    if (above) {
        for (var i = maskIndex+1; i < registry.length; i++) {
            if (this.isHardMask(registry[i])) return registry[i];
        }
    } else {
        for (var i = maskIndex-1; i >=0; i--) {
            if (this.isHardMask(registry[i])) return registry[i];
        }
    }
    // No hard mask was found above (or below) this mask
    return null;
},

// Resolve a canvas ID (or pointer to a canvas) to a canvas.
_getCanvas : function (canvas) {
    if (isc.isA.String(canvas)) return window[canvas];
    return canvas;
},


// Ensure widgets are not "hard masked"
_hardUnmaskTargets : function (widgets, fromHideClickMask) {
    if (!widgets || widgets.length == 0) return;
    
    for (var i = 0; i < widgets.length; i++) {
        // We pass either an array of widgets or an array of widget IDs - so we need to resolve 
        // these to canvii    
        var canvas = this._getCanvas(widgets[i]);
        if (!canvas) continue;
        
        // If we cleared the accessKey, reset it now
        if (canvas.accessKey != null && canvas.isDrawn()) {
            canvas._setHandleAccessKey(canvas.accessKey);
        }

        
        if (!canvas.isDrawn() && isc.isA.DynamicForm && isc.isA.DynamicForm(canvas) && 
            canvas.items && canvas.items.length > 0)
        {
            var item = canvas.items[0];
            
            if (item.containerWidget != canvas) canvas = item.containerWidget;
        }

        // Ensure that the canvas isn't obscured by the screenSpan
        // If the top level targets are behind the screenSpan, move it above it.
        
        if (!fromHideClickMask && canvas.getParentCanvas() == null &&
            canvas.getZIndex() <= this._screenSpan.getZIndex() &&
            canvas != this._screenSpan) 
        {
            canvas.setZIndex(this._screenSpan.getZIndex() +1);
            //>DEBUG
            this.logDebug("raised above screenspan: " + canvas, "clickMask");
            //<DEBUG
        }
    }

},

//>	@classMethod	EventHandler.clickMaskUp()	(A)
//  Determine whether a clickMask is currently showing.
//
// @param [ID]  (string)
//      Which clickMask to check?  If not specified, check whether any clickMask is showing.
// @group clickMask
// @return (boolean)    true if the click mask is showing
// @visibility eventhandler
//<
clickMaskUp : function (ID) {
    var registry = this.clickMaskRegistry;
    
    if (ID == null) return (registry.length > 0);
    else return (registry.find("ID", ID) != null);
},


//>	@classMethod	EventHandler.getAllClickMaskIDs()	(A)
//  Get the IDs for every click mask that's currently up.
// @group clickMask
// @return (Array)  Array of clickmask ID strings.
// @visibility internal
//<
getAllClickMaskIDs : function () {
    var registry = this.clickMaskRegistry;
    if (registry.length < 1) return [];
    
    return registry.getProperty("ID");
},

//>	@classMethod	EventHandler.showScreenSpan()	(A)
// @visibility internal
//<
showScreenSpan : function (mask) {
    
	if (!this._screenSpan) {
        this._screenSpan = isc.ScreenSpan.create(
            {ID:"isc_EH_screenSpan", 
             // If the screenspan gets destroyed, have it clear up our pointer to it.
             pointersToThis:[{object:this, property:"_screenSpan"}]
            },
            this.clickMaskProperties
         );
    }
	var span = this._screenSpan;
	span.show();
    
    if (!this.isHardMask(mask)) {
        span.sendToBack();
    } else {
        this._adjustSpanZIndex(mask._unmaskedTargets);
    }
},

// We show the screenSpan for clickMasks to suppress native interactions with DOM element such
// as form elements / links
_adjustSpanZIndex : function (unmaskedTargets) {
    // set a flag so we don't respond to 'bringToFront()' calls on widgets by 'unmasking' them
    
    this._adjustSpanZIndexRunning = true;

    var zIndex;
    for (var ID in unmaskedTargets) {
        var canvas = this._getCanvas(ID);
        // We only need to move the top level unmasked targets above the screenSpan.
        
        
        if (!canvas || canvas.destroyed || canvas.getParentCanvas() != null) {
            continue;
        }
        
        // If the canvas is a peer of an unmasked master, assume the masterElement manages
        // the zIndex of the peer, rather than calling bringToFront() here.
        // Avoids, for example, a shadow showing up in front of its target widget.
        
        if (canvas.getMasterCanvas() && unmaskedTargets[canvas.getMasterCanvas().getID()]) continue;
        
        canvas.bringToFront();
        if (zIndex == null) zIndex = canvas.getZIndex(true);
        
        // Remember zIndex is going to be the lowest zIndex of all unmasked canvii so we can 
        // slot the screenSpan underneath it.
        // We know that all peers unmask with their master (and allow the master to 
        // handle assigning the zIndex), so ensure the zIndex of the span is less than 
        // the zIndex of any peers.
        
        if (canvas.peers) {
            for (var i = 0; i < canvas.peers.length; i++) {
                if (!canvas.peers[i].isDrawn()) continue;
                zIndex = Math.min(zIndex, canvas.peers[i].getZIndex(true));
            }
        }
    }
    
    
    if (zIndex != null) this._screenSpan.setZIndex(zIndex -1);
    else this._screenSpan.bringToFront();

    this._adjustSpanZIndexRunning = false;
    
},

//> @classMethod    isc.EventHandler.maskTarget()
//
//      Ensure that a widget (or array of widgets) is below (obscured by) by the clickMask.
//
//      @group  clickMask
//      @param  target      (widget | Array of widgets)
//                  target[s] to unmask
//      @param  [maskID]    (string)
//                  ID of clickmask to move this target below.  If not passed in, will move below
//                  all visible clickMasks
//  @visibility eventhandler
//<

maskTarget : function (target, maskID) {
    // synonym for maskTargets
    return this.maskTargets(target, maskID);
},

//> @classMethod    isc.EventHandler.maskTargets()
//
//      Synonym for +link{classMethod:EventHandler.maskTarget()}
//
//      @group  clickMask
//      @param  target      (widget | Array of widgets)
//                  target[s] to unmask
//      @param  [maskID]    (string)
//                  ID of clickmask to move this target below.  If not passed in, will move below
//                  all visible clickMasks
// @visibility eventhandler
//<
maskTargets : function (targets, maskID, dontMaskChildren) {

    var registry = this.clickMaskRegistry;
    if (targets == null || registry.length == 0) return;

    if (!isc.isAn.Array(targets)) targets = [targets];
    else if (targets.length == 0) return;
    
    var mask =  (maskID == null ? registry[0] : 
                    (isc.isA.String(maskID) ? this.getClickMask(maskID) : maskID)
                );
    
    // If we couldn't get a mask, bail.
    if (mask == null) {
        // log at the info level -- this is likely to happen if the mask was hidden already, so 
        // a logWarn is a little strong
        this.logInfo("maskTargets called with invalid maskID - returning.", "event")
        return;
    }
    
    // At this point we have a valid mask and a set of targets to mask.
    
    // Iterate through targets adding any children or ancestors that need to also get masked
    var originalTargetsLength = targets.length    
    for (var i = 0; i < originalTargetsLength; i++) {
        
        var target = targets[i];
        
        // By default if this method is called we will mask all the widget's children as well as
        // the widget itself. This means if you call (for example) 'mask' on a currently unmasked
        // ListGrid, the body will also get masked.
        if (!dontMaskChildren && target.children != null) {
            this._addDescendantsToList(target, targets);
        }
    
        // We support having a parent be masked but a child be unmasked, but not a child be masked
        // but its parent be unmasked.
        // Therefore we will also iterate up through this widget's ancestor chain, masking all 
        // ancestors -- but not all of their children as this could mask just about everything on
        // the page if one of our targets is a child of a pageLayout, for example.
        // 
        // Also: Always mask / unmask peers with their masters
        // Use cases: Scrollbars, edged canvii, etc
        
        var parent = target.parentElement;
        while (parent != null) {
            if (!targets.contains(parent)) {
                targets.add(parent);
                if (dontMaskChildren) this._addPeersToList(parent, targets);
            }
            parent = parent.parentElement;
        }
        
        // At this point 'parent' is the top-level element, if defined
        if (dontMaskChildren || !parent) {
            this._addPeersToList(target, targets);
        }
        if (parent) this._addPeersToList(parent, targets);
    }
    
    // Now actually mask every target
    // When masking a widget we're essentially moving it down some number of layers in
    // the clickmask stack. (The simplest case of course is moving it from top to bottom).
    
    
    var maskIndex = registry.indexOf(mask);
    
    // Determine where the top hard mask is above / including this mask.
    // This will be used to determine whether the widget(s) being masked need to be hard masked
    var topHardMask;
    for (var i = maskIndex; i < registry.length; i++) {
        currentMask = registry[i];
        if (this.isHardMask(currentMask)) topHardMask = i;   
    }

    var targetsToHardMask;
    if (topHardMask != null) targetsToHardMask = [];
    for (var n = 0; n < targets.length; n++) {
        var target = targets[n];
        // always blur if it has focus.
        if (target.hasFocus && !target._ignoreClickMaskFocus) target.blur();
        
        var currentMask,
            topHardMask,
            targetLevel = null;
        
        for (var i = maskIndex; i < registry.length; i++) {
            currentMask = registry[i];
            if (currentMask._unmaskedTargets[target.getID()]) {
                targetLevel = i;
                // Always remove 'masked' targets from the masks' "unmaskedTargets" lists
                var map = currentMask._unmaskedTargets;
                if (map[target.getID()] === target) delete map[target.getID()];
                
            }
        }

        // If the target was unmasked wrt the top hard mask we need to hard mask it now.
        if (topHardMask != null && targetLevel != null && (topHardMask <= targetLevel)) {
            targetsToHardMask.add(target);
        }
    }
    // HardMaskTargets will handle 
    // - putting the targets behind the screenSpan if necessary
    // - getting rid of accessKey on the handle.
    if (topHardMask != null) this._hardMaskTargets(targetsToHardMask, null, false, false);
},

//> @classMethod    isc.EventHandler.addUnmaskedTarget()
//
//      Ensure that a widget (or array of widgets) is not masked by the clickMask.
//
//      @group  clickMask
//      @param  target      (widget | Array of widgets)
//                  target[s] to unmask
//      @param  [maskID]    (string)
//                  ID of clickmask to move this target above.  If not passed in, will move above
//                  all visible clickMasks
// @visibility eventhandler
//<
addUnmaskedTarget : function (target, maskID) {
    // synonym for addUnmaskedTargets
    return this.addUnmaskedTargets(target, maskID);
},
										
//> @classMethod    isc.EventHandler.addUnmaskedTargets()
//
//      Synonym for addUnmaskedTarget.
//
//      @group  clickMask
//      @param  target      (widget | Array of widgets)
//                  target[s] to unmask
//      @param  [maskID]    (string)
//                  ID of clickmask to move this target above.  If not passed in, will move above
//                  all visible clickMasks
// @visibility eventhandler
//<
// @param [recursive] (boolean) Internal debugging parameter indicating this is a call from 
//      within an addUnmaskedTargets() call, adding children of some target to the unmasked 
//      targets list

addUnmaskedTargets : function (targets, maskID) {

    // avoid unmasking targets in response to adjusting z-index as part of adjustSpanZIndex
    if (isc._unmaskOnBringToFront && this._adjustSpanZIndexRunning) return;

    var registry = this.clickMaskRegistry;
    if (targets == null || registry.length == 0) return;
    // support 'targets' being an array or a single widget
    if (!isc.isAn.Array(targets)) targets = [targets];

    if (targets.length == 0) return;
    var mask;
    
    // if passed no mask ID, completely unmask (so add to TOP mask's list of unmaskedTargets)
    if (maskID == null) {
        mask = registry.last();
    } else {
        if (isc.isA.String(maskID)) mask = this.getClickMask(maskID);
        // support being passed a mask object as well as an ID
        else mask = maskID;
    }
    // If we couldn't get a mask, bail.
    if (mask == null) {
        // log at the info level -- this is likely to happen if the mask was hidden already, so 
        // a logWarn is a little strong
        this.logInfo("addUnmaskedTargets called with invalid maskID - returning.", "clickMask")
        return;
    }

    // If we're "hard unmasking" anything, we need to ensure we hard unmask its ancestors as
    // well as children. We don't support a masked parent with unmasked children for 
    // hard masks.
    var hardMask = mask;
    while (hardMask && !this.isHardMask(hardMask)) {
        hardMask = registry[registry.indexOf(hardMask) -1];
    }
    if (hardMask != null) {
        
        if (isc._unmaskOnBringToFront && targets.length == 1 && targets[0].topElement != null) {
            //this.logWarn("not treating bringToFront as unmask because widget is not top-level: " +
            //             targets[0] + ", topElement: " + targets[0].topElement);
            return;
        }
    }
    
    // Combine ancestors (where appropriate), peers, descendants into the unmaskedTargets list
    targets = this._getFullSetOfTargetsToUnmask(targets, mask);

    //>DEBUG
    if (this.logIsDebugEnabled("clickMask")) {
        this.logDebug("Added unmasked targets:" + targets.getProperty("ID") + 
                      " [+ decendants] to clickMask with ID: " + mask.ID , "clickMask");
    }
    //<DEBUG
    
    var hardMaskAbove = false;
    for (var i = registry.indexOf(mask)+1; i < registry.length; i++) {
        if (this.isHardMask(registry[i])) hardMaskAbove = true;
    }
    
    for (var n = 0; n < targets.length; n++) {
        var target = targets[n];
        
        
        if (mask._unmaskedTargets == null) mask._unmaskedTargets = {};
        mask._unmaskedTargets[target.getID()] = target;
    }
   
    // Each target will be unmasked relative to the mask in question and any masks 
    // underneath it
    // If it was previously masked by a hard mask, and there is no hard mask ABOVE this one
    // we need to shift above the screenSpan / reset accessKey
    
    if (!hardMaskAbove) {
        this._hardUnmaskTargets(targets);
    }
},

//> @classMethod isc.EventHandler.targetIsMasked() (A)
// Return whether this Canvas is masked by a clickMask (see +link{Canvas.showClickMask()}).
//
// @param target (Canvas) widget to check
// @return (Boolean)   true if masked, false if not masked.
// @group clickMask
// @visibility external
//<
// Internal-only parameters:
// @param [maskID] (string) ID of click mask to check against - if not passed in, method
//                          will determine whether the widget is above the top clickMask.
// @param [hardMaskedOnly] (boolean) whether to consider only hard masks when checking
//                                   for masking
targetIsMasked : function (target, maskID, cancelOnly) {
    var registry = this.clickMaskRegistry;
    if (registry.length == 0) return false;
    
    // if we weren't given a target, a clickmask is up and the event occurred over a 
    // native page element rather than a widget - so the target is masked.
    if (target == null) return true;
    
    // If we weren't passed a maskID, just look at the topmost mask
    var mask;
    if (maskID == null) mask = registry.last();
    else if (isc.isA.String(maskID)) mask = registry.find("ID", maskID);
    else mask = maskID;
    if (!isc.isAn.Object(mask)) {
        this.logWarn("EventHandler.targetIsMasked() passed invalid maskID:" + maskID, 
                     "clickMask");
        return false;
    }
    
    var initialIndex = registry.indexOf(mask);
    var wouldCancelClick = false;
    for (var i = initialIndex; i < registry.length; i++) {
        // On the first iteration we already have a pointer to the mask
        if (i != initialIndex) mask = registry[i];
        
        // consider only masks that would cancel a click
        if (cancelOnly) {
            if (mask.mode == isc.EH.HARD || mask.mode == isc.EH.SOFT_CANCEL) {
                wouldCancelClick = true;
            } else {
                //this.logWarn("ignoring mask with mode: " + mask.mode);
                continue;
            }
        }

        if (mask._unmaskedTargets) {
            if (mask._unmaskedTargets[target.getID()]) return false;
            
            
            if (isc.DrawItem && isc.isA.DrawItem(target)) {
                var pane = target.drawPane;
                
                if (pane && pane.getID && mask._unmaskedTargets[pane.getID()]) return false;
            }
            
            // Special case: form items written into an unmasked container item should be treated as
            // unmasked (even if the form itself is masked)
            
            if (isc.isA.DynamicForm!=null && isc.isA.DynamicForm(target)) {
                var itemInfo = target._getEventTargetItemInfo(isc.EH.lastEvent);
                if (itemInfo && itemInfo.item && 
                    itemInfo.item.form == target && itemInfo.item.containerWidget != target &&
                    mask._unmaskedTargets[itemInfo.item.containerWidget.getID()]) return false;
            }
        }
    }
    
    return (cancelOnly && !wouldCancelClick ? false : true);
},

//> @classMethod    isc.EventHandler.clickMaskClick()
//
//      Called when a mouseDown occurred over a widget that may be masked by a clickMask.
//      If the target is masked, fire the appropriate clickMask action.
//      Returns true if the target is not masked, or the clickMask auto-hides (allows the
//      mouseDown event to proceed to it's target)
//      Returns false if the target is masked, and the clickMask is hard or has mode "softCancel" -- 
//      prevents the target from recieving the mouseDown event.
//
//      @group  clickMask
//      @param  target  (widget)    target of the mouseDown event.
//      @return         (boolean)   
//          True to allow the mouseDown event to proceed, false to cancel the event.
//      @visibility internal
//<
clickMaskClick : function (target) {

    // copy the clickMaskRegistry, so we don't get confused if the registry is modified by
    // the click action showing additional masks(for example)
    var maskReg = this.clickMaskRegistry.duplicate(),
        mask = maskReg.last();
     
    while (mask != null && (this.targetIsMasked(target) || target == this._screenSpan)) {
        if (this.logIsInfoEnabled("clickMask")) {
            this.logInfo("mouseDown on masked " + target + 
                          (mask.clickAction != null ? " firing clickAction, " : "") +
                          (mask.autoHide ? 
                            "will hide mask" + 
                                (mask.mode == isc.EH.SOFT_CANCEL ? " and block click" : "") : 
                           "will block click"));
        }
        var cancel = (mask.mode != isc.EH.SOFT);
        this._clickMaskClick(mask)

                
        // If the mask is hard return false to cancel the event -- we're done
        if (cancel) return false;

        // If the mask is soft (and not "softCancel"), fire clickMaskClick on the mask underneath it
        mask = maskReg[maskReg.indexOf(mask) -1];
    }
    
    // if we got here we've hit an unmasked target (possibly after hiding some autoHide true CM's)
    return true;
},

// actually fire the click action and (if appropriate) hide the mask.
_clickMaskClick : function (mask) {

    var autoHide = mask.autoHide,
        clickAction = mask.clickAction;

    if (autoHide == true) this.hideClickMask(mask.ID);

    // Fire the action if there is one.
    // Note: we don't care about the return value from the clickAction.
    if (clickAction != null) this.fireCallback(clickAction);
}

});	// END isc.EventHandler.addClassMethods()

// call captureEvents now to set things up for our event handling.
isc.EventHandler.captureEvents();



