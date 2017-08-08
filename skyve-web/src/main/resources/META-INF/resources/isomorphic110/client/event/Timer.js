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
//>	@class	Timer
//
// The Timer class provides a predictable cross-browser system for creating
// timed events.
//
// @treeLocation Client Reference/System
// @visibility external
//<






// create the Timer object
isc.ClassFactory.defineClass("Timer");

// add class properties and constants
isc.Timer.addClassProperties(
{ 	
	_eventList : null,								//>	@classAttr	isc.Timer._eventList		(object : null : IRWA)
															//		This is a single linked list of timerEvents that have been queued up for execution.
															//		The attribute itself points to the first element in the list.
															//
															//		@group timer
															//<
															
	listEvent : {action: null,		 				//>	@classAttr	isc.Timer.listEvent		(object : {...} : IRWA)
					 iterationInterval: null,		//			Properties for a timerEvent (a pseudoclass used in the isc.Timer class)
					 iterationsRemaining: 0,		//			
					 _nextEvent: null,			//			             action: reference to function for this timerEvent
					 _nextRunTime: null},			//			          condition: condition for continuing iteration, used by isc.Timer.setDoWhen() and isc.Timer.setDoUntil()
															//			  iterationInterval: time between iterations of a timerEvent
															//			iterationsRemaining: counter for remaining iterations of a timerEvent
															//			               time: time when timerEvent should fire, used by Timer.seAlarm()
															//			     _nextEvent: reference to next timerEvent in the eventList
															//			       _nextRunTime: absolute time when this timerEvent should be run
															//			           _execute: routine fired when timerEvent is executed.
															//<															
	//>	@type	Units
	//		Multiplier for an amount of time specified in milliseconds.
	MSEC :       1,			//	@value	isc.Timer.MSEC		milliseconds
	SEC :     1000,			//	@value	isc.Timer.SEC		seconds
	MIN :    60000,			//	@value	isc.Timer.MIN		minutes
	HOUR : 3600000,			//	@value	isc.Timer.HOUR		hours
	//<
	
	DEFAULT_TIMEOUT_LENGTH : 100,							//>	@classAttr isc.Timer.DEFAULT_TIMEOUT_LENGTH (number : 100 : R)
															//		Default time to delay if an explicit delay is not specified.
															//<	
	
	
	_clockHandle : null 									//>	@classAttr	isc.Timer.__clockHandle		(object : null : IRWA)
															//		Reference to the setTimeout() instance that may be running at any given time.
															//		Used to stop the timer, if necessary. If value is null, then the queue is not
															// 	processing.
															//
															//		@see Timer._stopClock()
															//<
}
);// END isc.Timer.addClassProperties()



// add a bunch of methods to the Timer object
isc.Timer.addClassMethods({



//>	@classMethod	Timer.setTimeout()
//    
// Execute an action in a given amount of time.  This method wraps the native setTimeout() method,
// correcting for browser-specific memory leaks.
//
// @see clear()
//
//	
// @param action (string expression or function)	
//			     	 Function to be called when delay has elapsed. 
//				     Can also be a string representation of an expression.
//	    			 Passing a string is preferred.
//
// @param delay (number) Time until action is executed (in milliseconds). If not specified, the
//                       default is 100 milliseconds.
// @return      (timerEvent) Reference to the timerEvent created. Note that this reference is provided
// 							 only so that it can be used as an argument for Timer.clear().
// @visibility external    
//<



// - incrementing count to identify delayed actions uniquely
_timeoutCount:0,
// - map of native timer event IDs to action ID for the delayed action stored on the isc.Timer object
_tmrIDMap:{},
// - map from action ID to native timer event ID
_reverseDelayedTmrIDMap:{},
setTimeout : function (action, delay, units, frequentTimer) {

    if (action == null) return;

	// if an object is passed in the place of the action parameter, 
	// then assign parameters from its values
	if (action.action != null) {
		delay = action.delay;
		units = action.units;
		action = action.action;
	}

	//defaults, loaded if not in parameters or in parameter object.
	if (units == null) units = isc.Timer.MSEC;
	if (delay == null) delay = isc.Timer.DEFAULT_TIMEOUT_LENGTH;

	// get the actual length to delay according to the units passed in
	delay = delay * units;

    
    if (isc.Browser.isMobileSafari) {
        if (isc.isA.String(action) || action._fireTime != null) action = {_action: action};
        action._fireTime = delay + isc.timeStamp();
    }

    var ID = "_timeout" + this._timeoutCount++;
    this[ID] = action;

    
    if ( this.logIsDebugEnabled("traceTimers")
        
        
       ) 
    {
        delete this.currentAction;
        action.timerTrace = this.getStackTrace(null, 1, null, true);
    }

    

    // actually set the native timeout to fire at the appropriate time.
    var tmrID = setTimeout(function () {
        isc.Timer._fireTimeout(ID, tmrID);
    }, delay);
    // Setting up a mapping between the native timer ID and the name of the temp slot we used
    // to store the action allows us to clear both if a developer calls 'clear()'
    
    this._tmrIDMap[tmrID] = ID;
    return tmrID;
},

_$TMR:"TMR",
_evalDurationThreshold:5000,
// method fired to actually execute a timeout
_fireTimeout : function (ID, tmrID, delayedTmrID) {
    // If an eval() is in mid execution, further delay the timeout until it completes
    // In FF 3 (seen on version 3.0.3), Mozilla introduced a behavior whereby if
    // a thread of code called the native "eval" function while there was a pending timeout,
    // the timeout would fire before the eval was evaluated, meaning essentially a timout could
    // interrupt an otherwise synchronous thread. We don't expect this behavior and it can cause
    // some bizarre errors - we workaround this by setting a flag before our wrapper around
    // the native eval method gets called, and if present not allowing any timeouts to fire
    if (isc._evalRunning != null) {
        if (this.logIsInfoEnabled()) {
            this.logInfo("timer ID:" + ID + " fired during eval. Delaying until this " +
                            "thread completes");
        }
        // Sanity check - if we've already waited for a long time, assume the eval
        // crashed and the _evalRunning flag is bogus.
        if (!this._evalDurationStart) this._evalDurationStart = isc.timeStamp();

        if ((isc.timeStamp() - this._evalDurationStart) > this._evalDurationThreshold) {
            this.logWarn("timer ID:" + ID + " fired during eval thread lasting more than " +
                        this._evalDurationThreshold + "ms. Thread may have caused an " +
                        "error and failed to complete. Allowing delayed action to fire.");
            delete isc._evalRunning;
        } else {

            delayedTmrID = setTimeout(function () {
                isc.Timer._fireTimeout(ID, tmrID, delayedTmrID);
            }, 0);
            // Store the native timer identifier so a call to clear() can suppress the new native
            // timeout from firing
            this._reverseDelayedTmrIDMap[ID] = delayedTmrID;
            return;
        }
    }

    delete this._evalDurationStart;

    var action = this[ID];

    
    if (action == null) {
        return;
    }

    
    if (action._action) action = action._action;

    

    // Clear out the temp action slot, and the mapping to native timer ID
    delete this[ID];
    
    delete this._tmrIDMap[tmrID];

    
    delete this._reverseDelayedTmrIDMap[delayedTmrID];

    isc.EH._setThread(this._$TMR);

    arguments.timerTrace = action.timerTrace;
    // this is not quite as good as having timerTraces stored on arguments, since it can only
    // show us the origin of the current timer thread, not multiple timers that set other timers
    this.currentAction = action;
    // fireCallback() will handle action specified as function, string to eval and
    // object with 'target' and 'methodName' attributes.
    // Since this is a new thread, pass in the param to catch errors - allows us to see JS
    // errors in the arbitrary code
    this.fireCallback(action, null, null, null, true);
    isc.EH._clearThread();
},




//>	@classMethod	Timer.clear()
//
// Cancels the processing of a timerEvent if it has not already fired.
//
// @param	timerEvent	(object)		timerEvent object previously returned from Timer.setTimeout()
//
// @visibility external
//<
clear : function (tmrID) {
    if (isc.isAn.Array(tmrID))
        for (var i = 0; i < tmrID.length; i++) this.clear(tmrID[i]);
    else {
        var ID = this._tmrIDMap[tmrID];
        // clear the temp action and the pointer to it
        delete this[ID]
        delete this._tmrIDMap[tmrID];

        
        if (this._reverseDelayedTmrIDMap.hasOwnProperty(ID)) {
            tmrID = this._reverseDelayedTmrIDMap[ID];
            delete this._reverseDelayedTmrIDMap[ID];
        }

        // natively clear the timeout
        clearTimeout(tmrID);
    }
	return null;
},

clearTimeout : function (tmrID) {
    return this.clear(tmrID);
},

_getTimeoutFireTime : function (ID) {
    var action = this[ID] || {};
    return action._fireTime;
},


firePendingTimeouts : function () {
    
    var tmrIDMapCopy = isc.addProperties({}, this._tmrIDMap),
        reverseDelayedTmrIDMap = this._reverseDelayedTmrIDMap,
        timeStamp = isc.timeStamp();
    for (var tmrID in tmrIDMapCopy) {
        if (!tmrIDMapCopy.hasOwnProperty(tmrID)) continue;

        var ID = tmrIDMapCopy[tmrID];
        if (ID == null) continue;
        // check whether each timeout is ready to fire
        var fireTime = this._getTimeoutFireTime(ID);
        if (fireTime != null && fireTime < timeStamp) {
            if (reverseDelayedTmrIDMap.hasOwnProperty(ID)) {
                var delayedTmrID = reverseDelayedTmrIDMap[ID];
                clearTimeout(delayedTmrID);
                this._fireTimeout(ID, tmrID, delayedTmrID);
            } else {
                clearTimeout(tmrID);
                this._fireTimeout(ID, tmrID);
            }
        }
    }
}



});	// END isc.Timer.addClassMethods()



