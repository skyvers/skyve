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
//> @class Animation
// Class with static APIs used by the animation subsystem.
// @treeLocation  Client Reference/System
// @visibility animation_advanced
//<
isc.ClassFactory.defineClass("Animation");

isc.Animation.addClassProperties({
    //> @classAttr Animation.interval   (number : 20 : IRWA)
    // Interval in ms between animation events.
    // @visibility animation_advanced
    //<
    interval:20,
    registry:[],

    // Some standard ratio functions
    // These functions take a value between zero and one, representing a linear ratio and 
    // return a value between zero and one that represents a non linear ratio.
    // Executed in global scope
    
    //> @classMethod Animation.smoothStart (A)
    // This is a static function which maps a linear ratio (value between zero and one 
    // representing how much of an animation has elapsed) to a ratio biased such that the
    // starts slowly and speeds up as it approaches 1.
    // @visibility animation_advanced
    //<    
    smoothStart : function (rawRatio) {
        return Math.pow(rawRatio, 2); 
    },
    
    //> @classMethod Animation.smoothEnd (A)
    // This is a static function which maps a linear ratio (value between zero and one 
    // representing how much of an animation has elapsed) to a ratio biased such that the
    // animation starts moving quickly, and appears to slow down as it approaches 1.
    // @visibility animation_advanced
    //<    
    smoothEnd : function (rawRatio) {
        return 1 - Math.abs(Math.pow(rawRatio-1, 2));    
    },
    
    //> @classMethod Animation.smoothStartEnd (A)
    // This is a static function which maps a linear ratio (value between zero and one 
    // representing how much of an animation has elapsed) to a ratio biased such that the
    // animation appears to accelerate from a slow start, then slow down again toward the end
    // of the animation.
    // @visibility animation_advanced
    //<    
    smoothStartEnd : function (rawRatio) {
        return (-Math.cos(rawRatio*Math.PI) + 1) / 2.0;
    },
    
    //> @classAttr Animation.animateTime (number : 1000 : IRWA)
    // Default total duration for animations with no specified duration.  Typically animations
    // involving canvases will pick up their duration from the Canvas level default, so this
    // property is only used in rare cases.
    // @visibility animation_advanced
    // @group animation
    //<
    animateTime:1000
    
});

isc.Animation.addClassMethods({
    // Unique IDs used to identify registered animation actions
    generateAnimationID : function () {
        if (!this._animationCount) this._animationCount = 0;
        return "_" + (this._animationCount++);    
    },
    
    // Can we use the native requestAnimationFrame() method rather than relying on
    // explicit timeouts?
    _useRequestAnimationFrame : function () {
    	if (this._browserHasRequestAnimationFrame == null) {
	    	this._browserHasRequestAnimationFrame = window.requestAnimationFrame != null;
    	}
    	return this._browserHasRequestAnimationFrame;
    },
    
    
    timeBased:false,
    
    // Raw handler fired in the global scope by the animation timer - fires the animation 
    // events
    timeoutAction : function () {
        if (isc.Animation) isc.Animation.fireTimer();
    },
    requestedAnimationAction : function (nativeTimestamp) {
    	
        if (isc.Animation) isc.Animation.fireTimer();
    },
    
    //> @type AnimationAcceleration
    // Acceleration effect for animations. Can either be a ratio function or a string.
    // Ratio functions take a value between 0 and 1 which represents how much of the 
    // animation's duration has elapsed, and return another value between 0 and 1 indicating
    // how close the animation is to completion. For a completely linear animation, the 
    // function would return the value it was passed. This allows you to bias animations to
    // [for example] speed up toward the end of the animation.<br>
    // The following strings are also supported for common ratio bias effects:
    //
    // @value "smoothStart" - animation will speed up as time elapses
    // @value "smoothEnd" - animation will slow down as time elapses
    // @value "smoothStartEnd" - animation will speed up in the middle
    // @value "none" - no bias
    // @visibility animation
    //<

    //> @classMethod Animation.registerAnimation()
    // Register an action to fire repeatedly for some duration of time.
    //
    // @param callback (callback) Action to fire repeatedly until the duration expires.
    //                            Passed 3 parameters for each step:<br>
    //                              - "ratio" (number between 0 and 1) indicating what fraction 
    //                                of the specified duration has elapsed<br>
    //                              - "ID" (string) the unique ID for this registered animation<br>
    //                              - "earlyFinish" (boolean) If true this animation was cut 
    //                                short via a call to +link{Animation.finishAnimation()} before
    //                                its duration had elapsed.
    // @param duration (number) Target duration for this animation in ms.  The callback will
    //                          actually be called a fixed number of times based on this target
    //                          duration and the default frame interval
    //                          (isc.Animation.interval), which may result in an animation that
    //                          is longer than the target duration if some frames exceed the
    //                          interval time.  The animation will be cut short if it exceeds
    //                          3 times the target duration
    // @param [acceleration] (AnimationAcceleration) Acceleration bias effect for the animation.
    // @param [target] (object) If specified the callback will be fired in the scope of the
    //                          target passed in.
    // @return (string) Unique ID for the registered animation action.
    // @visibility animation_advanced
    //<
    registerAnimation : function (callback, duration, acceleration, target) {
    
    	if (this._useRequestAnimationFrame()) {
    		if (this._requestedAnimationFrame == null) {
    			this._requestedAnimationFrame = window.requestAnimationFrame(this.requestedAnimationAction);
				this._startTime = isc.timeStamp();
    		}
    	} else {
			if (!this._animationTimer) {
				this._animationTimer = isc.Timer.setTimeout(this.timeoutAction, this.interval);
				this._startTime = isc.timeStamp();
			}
		}
				
        if (!target) target = this;
        if (!duration) duration = this.animateTime;
        
        
        if (isc.isA.String(acceleration)) {
            if (!isc.Animation.accelerationMap) {
                isc.Animation.accelerationMap =  {
                    smoothStart:isc.Animation.smoothStart, 
                    smoothEnd:isc.Animation.smoothEnd, 
                    smoothStartEnd:isc.Animation.smoothStartEnd
                    // Support the user specifying "none" - just don't use any biasing 
                    // function - same as if they said "foo"
                    // none:null
                }
            }
            acceleration = isc.Animation.accelerationMap[acceleration];
        }
        
        var ID = this.generateAnimationID();
        this.registry.add({
            ID:ID, target:target, callback:callback, duration:duration, elapsed:0, 
            totalFrames:Math.round(duration/this.interval), currentFrame:0, 
            // For frame based animation (the default), don't allow animation to exceed
            // three times the specified duration.
            maxDuration:duration*3,
            acceleration:acceleration
        });
        
        return ID;
    },
    
    //> @classMethod Animation.clearAnimation()
    // Clear a registered animation action. Only meaningful if the registered animation has
    // not completed (i.e. the specified duration for the action has not elapsed since the
    // action was registered). Will un-register the action and prevent it from firing again.
    // @param ID (string) ID for the action to be unregistered. This is the ID returned from
    //                      Animation.registerAnimation().
    // @visibility animation_advanced
    //<
    clearAnimation : function (ID) {
        for (var i=0; i<this.registry.length; i++) {       
            if (this.registry[i] && this.registry[i].ID == ID) {                
                this.registry.removeAt(i);
                break;
            }
        }
    },
    
    //> @classMethod Animation.finishAnimation()
    // "Finish" a registered animation, by clearing it, and firing it with a
    // ratio of 1 and an additional 'earlyFinish' which will be passed to the callback. 
    // @param ID (string) ID for the action to be finished. This is the ID returned from
    //                      Animation.registerAnimation().
    // @visibility animation_advanced
    //<
    finishAnimation : function (ID) {
        for (var i = 0; i < this.registry.length; i++) {
            if (this.registry[i] && this.registry[i].ID == ID) {
                var entry = this.registry[i];
                break;
            }
        }

        this.clearAnimation(ID);
        if (entry) this.fireAction(entry, 1, true);
    },
    
    // fireTimer() - this is fired every interval and handles:
    // - firing any animations whose total duration has not yet elapsed
    // - unregistering any animations whose total duration has elapsed
    // - setting up the timer to fire this method again after the next Animation.interval ms
    fireTimer : function () {
        var newTime = isc.timeStamp(),
            elapsed = (newTime - this._startTime),
            // Adjust for the difference between the actual elapsed time and the desired 
            // interval so we average out to firing as close to every [interval] ms as possible
            interval = Math.max(0, this.interval - (elapsed - this.interval));

        //this.logWarn("timer firing - elapsed is:"+ elapsed + ", so interval is:"+ interval);
    	if (this._useRequestAnimationFrame()) {
    		this._requestedAnimationFrame = window.requestAnimationFrame(this.requestedAnimationAction);
    	} else {
			this._animationTimer = isc.Timer.setTimeout(this.timeoutAction, interval);
		}
        this._startTime = newTime;

        for (var i = 0; i < this.registry.length; i++) {
            var entry = this.registry[i];
            // We don't expect this to happen because we do a removeEmpty below
            
            if (entry == null) continue;
            
            entry.elapsed += elapsed;
            
            var nextFrame = entry.currentFrame + 1;
            
            
            if (!isc.Animation.timeBased && 
                ((entry.elapsed / entry.maxDuration) > (nextFrame / entry.totalFrames) ))
            {
                nextFrame = Math.min(entry.totalFrames, 
                                     Math.ceil((entry.elapsed/entry.maxDuration) * entry.totalFrames)); 
            }
            
            entry.currentFrame = nextFrame;
            
            var unbiasedTimeRatio = entry.elapsed/entry.duration,
            	unbiasedFrameRatio = entry.currentFrame/entry.totalFrames;

			// We want to use the time-based ratio 
			// - if Animation.timeBased is explicitly true
			// - if the time-based ratio exceeds the frame-based ratio (implying we're 
			//   getting notified more frequently than once every 'interval' ms).
			var useFrameRatio = !isc.Animation.timeBased  &&
								(unbiasedTimeRatio > unbiasedFrameRatio),
				unbiasedRatio = useFrameRatio ? unbiasedFrameRatio : unbiasedTimeRatio;

			
				
            var ratio = unbiasedRatio,
                acceleration = entry.acceleration;
            if (acceleration && isc.isA.Function(acceleration)) {
            
				
            	if (!entry.accelerationTested) {
					try {
						ratio = entry.acceleration(ratio);
					} catch(e) {
						this.logWarn("Custom ratio function for animation:" + isc.Log.echoAll(entry) + 
									 "\nCaused an error:"+ (e.message ? e.message : e));
						// delete it, so even if its time hasn't elapsed we don't run into this error
						// repeatedly until the time expires
						entry.acceleration = null;
					}
					entry.accelerationTested = true;
				} else {
					ratio = entry.acceleration(ratio);
				}
            }
            //this.logWarn("ratio:"+ ratio);
            
            // If we've fired the animation for the duration of the entry, ensure we clear it
            // out so we don't fire it again
            // Note that we are checking the unbiased ratio - the acceleration is arbitrary, so
            // may fail to give us a value of 1, in which case we don't want to be left with
            // an incompleted animation.
            if (unbiasedRatio >= 1) {
                ratio = 1;
                this.registry[i] = null;
            }
            
            
            var error = null;
            try {
                //this.logWarn("firing frame of animation: " + entry.ID + " with ratio: " + ratio);
                error = this.fireAction(entry, ratio);
            } catch(e) {
                error = e;
            }
            if (error != null) {
                this.logWarn("Attempt to fire registered animation:" + isc.Log.echoAll(entry) + 
                 "\nCaused an error:"+ (error.message ? error.message : error));
                // delete it, so even if its time hasn't elapsed we don't run into this error
                // repeatedly until the time expires
                this.registry[i] = null;
            }

            if (unbiasedRatio >= 1) {
                this.logDebug("animation " + entry.ID + " completed", "animation");
            }
        }
        this.registry.removeEmpty();
        // Stop looping if we don't have any pending animations
        if (this.registry.length == 0) {
	    	if (this._useRequestAnimationFrame()) {
				window.cancelAnimationFrame(this._requestedAnimationFrame);
				this._requestedAnimationFrame = null;
			} else {
				isc.Timer.clearTimeout(this._animationTimer);
				this._animationTimer = null;
			}
        }
    },
    
    // fireAction will be called to actually fire each registered animation action    
    _$ratio_ID_earlyFinish:"ratio,ID,earlyFinish",
    fireAction : function (action, ratio, earlyFinish) {
    
        // pass the earlyFinish param on to the action callback. 
        
        var target = action.target;
        if (!target || target.destroyed) {
            return "No valid target. Target may have been destroyed since animation commenced";
        }
        target.fireCallback(action.callback, this._$ratio_ID_earlyFinish, 
                            [ratio,action.ID,earlyFinish]);
    },
    
    // Globally are any animations in progress?
    
    isActive : function () {
        return (this.registry && this.registry.length > 0);
    }
});

isc.Canvas.addProperties({
    //> @attr canvas.animateTime (number : 300 : IRWA)
    // Default total duration of animations. Can be overridden by setting animation times for
    // specific animations, or by passing a <code>duration</code> parameter into the appropriate
    // animate...() method.
    // @visibility animation
    // @group animation
    // @example animateMove
    //<
    
    animateTime:300,
    
    //> @attr canvas.animateAcceleration (AnimationAcceleration : "smoothEnd" : IRWA)
    // Default acceleration effect to apply to all animations on this Canvas.
    // Can be overridden by setting animationAcceleration for specific animations or by passing
    // an acceleration function directly into the appropriate method.
    // @visibility animation
    // @group animation
    //<    
    animateAcceleration:"smoothEnd",
    
    // List of supported animations.
    // For each of these we need to support the method 'animate[Type]' (like animateMove()).
    // These method names can also be passed as parameters to finishAnimation()
    
    _animations:["rect","fade","scroll","show","hide", "resize", "move"],
    
    //> @attr canvas.animateShowEffect (animateShowEffectId | animateShowEffect : "wipe" : IRWA)
    // Default animation effect to use if +link{Canvas.animateShow()} is called without an
    // explicit <code>effect</code> parameter
    // @visibility animation
    // @group animation
    //<
    animateShowEffect:"wipe",

    //> @attr canvas.animateHideEffect (animateShowEffectId | animateShowEffect : "wipe" : IRWA)
    // Default animation effect to use if +link{Canvas.animateHide()} is called without an
    // explicit <code>effect</code> parameter
    // @visibility animation
    // @group animation
    //<
    animateHideEffect:"wipe"

    //> @attr canvas.animateMoveTime  (number : null : IRWA)
    // Default time for performing an animated move.  If unset, <code>this.animateTime</code>
    // will be used by default instead
    // @visibility animation
    // @group animation
    //<
    
    //> @attr canvas.animateResizeTime  (number : null : IRWA)
    // Default time for performing an animated resize.  If unset, <code>this.animateTime</code>
    // will be used by default instead
    // @visibility animation
    // @group animation
    //<
    
    //> @attr canvas.animateRectTime  (number : null : IRWA)
    // Default time for performing an animated setRect.  If unset, <code>this.animateTime</code>
    // will be used by default instead
    // @visibility animation
    // @group animation
    //<
    
    //> @attr canvas.animateFadeTime  (number : null : IRWA)
    // Default time for performing an animated fade.  If unset, <code>this.animateTime</code>
    // will be used by default instead
    // @visibility animation
    // @group animation
    //<
    
    //> @attr canvas.animateScrollTime  (number : null : IRWA)
    // Default time for performing an animated scroll.  If unset, <code>this.animateTime</code>
    // will be used by default instead
    // @visibility animation
    // @group animation
    //<
    
    //> @attr canvas.animateShowTime  (number : null : IRWA)
    // Default time for performing an animated show.  If unset, <code>this.animateTime</code>
    // will be used by default instead
    // @visibility animation
    // @group animation
    //<
    
    //> @attr canvas.animateHideTime  (number : null : IRWA)
    // Default time for performing an animated hide.  If unset, <code>this.animateTime</code>
    // will be used by default instead
    // @visibility animation
    // @group animation
    //<

    //> @attr canvas.animateMoveAcceleration  (AnimationAcceleration : null : IRWA)
    // Default acceleration effect for performing an animated move.  If unset, 
    // <code>this.animateAcceleration</code> will be used by default instead
    // @visibility animation
    // @group animation
    //<
    
    //> @attr canvas.animateResizeAcceleration  (AnimationAcceleration : null : IRWA)
    // Default acceleration function for performing an animated resize.  If unset, 
    // <code>this.animateAcceleration</code> will be used by default instead
    // @visibility animation
    // @group animation
    //<
    
    //> @attr canvas.animateRectAcceleration  (AnimationAcceleration : null : IRWA)
    // Default acceleration function for performing an animated move and resize.  If unset, 
    // <code>this.animateAcceleration</code> will be used by default instead
    // @visibility animation
    // @group animation
    //<
    
    //> @attr canvas.animateScrollAcceleration  (AnimationAcceleration : null : IRWA)
    // Default acceleration function for performing an animated scroll.  If unset, 
    // <code>this.animateAcceleration</code> will be used by default instead
    // @visibility animation
    // @group animation
    //<
    
    //> @attr canvas.animateShowAcceleration  (AnimationAcceleration : null : IRWA)
    // Default acceleration function for performing an animated show.  If unset, 
    // <code>this.animateAcceleration</code> will be used by default instead
    // @visibility animation
    // @group animation
    //<
    
    //> @attr canvas.animateHideAcceleration  (AnimationAcceleration : null : IRWA)
    // Default acceleration function for performing an animated hide.  If unset, 
    // <code>this.animateAcceleration</code> will be used by default instead
    // @visibility animation
    // @group animation
    //<


    
})

isc.Canvas.addMethods({
    
    //> @method canvas.registerAnimation  (A)
    // Register some action to fire repeatedly for a specified duration
    // @param callback (callback) Action to fire repeatedly until the duration expires
    // @param [duration] (Integer) time in ms for which the action should be fired
    // @param [acceleration] (AnimationAcceleration) Acceleration effect to apply to the animation
    // @return (string) Unique identifier for the registered animation action
    // @visibility animation_advanced
    // @group animation
    //<
    registerAnimation : function (callback, duration, acceleration) {
        if (!acceleration) acceleration = this.animationAcceleration;
        if (!duration) duration = this.animateTime;
        return isc.Animation.registerAnimation(callback, duration, acceleration, this);
    },
    
    //> @method canvas.cancelAnimation  (A)
    // Clear some registered animation action
    // @param ID (string) ID of the animation as returned by canvas.registerAnimation()
    // @visibility animation_advanced
    // @group animation
    //<
    cancelAnimation : function (ID) {
        isc.Animation.clearAnimation(ID);
    },
    
    // ----------------------------------------------------------------------------------------
    // Specific animation effects (Higher level API)
    // ----------------------------------------------------------------------------------------
    
    // getAnimationType() will return the default duration for various animation types
    // getAnimationAcceleration() will return the default acceleration function used to bias 
    // animation ratios for the appropriate type
    _animateTimeMap:{},    
    _animateAccelerationMap:{},
    getAnimateTime : function (type) {
        if (!isc.isA.String(type) || isc.isAn.emptyString(type)) return this.animateTime;
        
        // type is something like "move" or "resize" 
        // - default duration specified via this.animateMoveTime
        if (!this._animateTimeMap[type]) {
            this._animateTimeMap[type] = "animate" + 
                                            type.substring(0,1).toUpperCase() + type.substring(1) + 
                                            "Time";
        }
        return this[this._animateTimeMap[type]] || this.animateTime;
    },
    
    getAnimateAcceleration : function (type) {
        if (!isc.isA.String(type) || isc.isAn.emptyString(type)) return this.animateAcceleration;
            
        // - default ratio biasing function specified via this.animate[Type]Acceleration
        if (!this._animateAccelerationMap[type]) {
            this._animateAccelerationMap[type] = "animate" + 
                                            type.substring(0,1).toUpperCase() + type.substring(1) + 
                                            "Acceleration";
        }
        return this[this._animateAccelerationMap[type]] || this.animateAcceleration;
    },

    // _getAnimationIDs() - each time an animation is set up, the ID of the animation action
    // is stored under this.[type]Animation (so this.rectAnimation, this.fadeAnimation, etc.)
    // Helper method to retrieve these IDs
    _animationIDs:{},
    _$Animation:"Animation", 
    _getAnimationID : function (type) {
        if (!this._animationIDs[type]) {
            this._animationIDs[type] = type + this._$Animation;
        }
        return this._animationIDs[type];
    },
    // _getAnimationMethodName() - the actions fired (repeatedly) for animations are canonically
    // named as this.fireAnimation[ActionType]().
    // Helper to cache / retrieve these names
    _animationMethodNames:{},
    _getAnimationMethodName : function (type) {
        if (!this._animationMethodNames[type]) {
            this._animationMethodNames[type] = "fireAnimation" + 
                                                type.substring(0,1).toUpperCase() + 
                                                type.substring(1);
        }
        return this._animationMethodNames[type];
    },
    
    // Helper method fired to start an animation. This method allows us to consolidate the
    // code path to:
    // - check if a current animation is in process (and finish it if so)
    // - store out info for use by repeatedly called animation action
    // - registers the animation to actually start firing
    // This method requires the following:
    // - the repeatedly fired action for an animation will be named "this.fireAnimation[AnimationType]"
    // - the info passed to this method will be stored as "this.[animationType]Info" (so the
    //   action should be prepared to access that object)
    // - the ID of the registered animation will be stored as "this.[animationType]Animation"
    // - When the animation completes, the method _clearAnimationInfo() should be called to
    //   clear out the stored animation ID and info. This is handled by the various 
    //   fireAnimation... methods when the animation is complete (ratio == 1), and typically 
    //   before the final callback fires to ensure isAnimating(..) is false at that point.
    
    _animationInfoAttrs:{},
    _runningAnimations:0,
    _startAnimation : function (type, info, duration, acceleration) {
        var ID = this._getAnimationID(type);
        // If an animation of the same type is already running, finish it before starting this
        // one.
        if (this[ID]) this.finishAnimation(type);
        
        // Hang onto the info passed in - will be used by the animation action fired
        if (!this._animationInfoAttrs[type]) { 
            // NB: using $ instead of _ prefix to avoid obfuscation problems
            this._animationInfoAttrs[type] = "$" + type + "AnimationInfo";
        }
        this[this._animationInfoAttrs[type]] = info; 
            
        if (duration == null) duration = this.getAnimateTime(type);
        if (acceleration == null) acceleration = this.getAnimateAcceleration(type);
 
        // Register the animation method to fire for the specified duration
        var animationId = this[ID] = 
            this.registerAnimation(this[this._getAnimationMethodName(type)], duration, acceleration);
        if (this.logIsInfoEnabled("animation")) {
            this.logInfo("starting animation " + animationId + " of type: " + type +
                         ", duration: " + duration + 
                         ", acceleration: " + this.echoLeaf(acceleration),
                         "animation");
        }
        this._runningAnimations ++;
       
        return animationId;
    },
    
    _clearAnimationInfo : function (type) {
        var ID = this._getAnimationID(type);
        if (!this[ID]) {
            return;
        }
        delete this[ID];
        delete this[this._animationInfoAttrs[type]];
        
        this._runningAnimations--;
        
    },
    
    // helper method to fire the final callback at the end of an animation.
    
    _pendingAnimationCallbacks: 0,
    animationComplete : function (earlyFinish) {},
    _fireAnimationCompletionCallback : function (callback, earlyFinish, synchronous) {    
        if (!callback) return;

        var widget = this,
            fireCallbackNow = earlyFinish || synchronous;

        var fireCallback = function () {
            widget.fireCallback(callback, "earlyFinish", [earlyFinish]);
            if (!fireCallbackNow) widget._pendingAnimationCallbacks--;
            widget.animationComplete(earlyFinish);
        }

                              

        if (fireCallbackNow) {
            fireCallback();
        } else {     
            isc.Timer.setTimeout(fireCallback, 0);
            this._pendingAnimationCallbacks++;
        }
    },
    
    //> @method canvas.finishAnimation()
    // Forces a running animation (animated move / resize, etc.) to instantly complete - jumping
    // to its finished state, and firing its callback, and passing the 'earlyFinish' parameter
    // to the callback.
    // @param [type] (string) animation type name ("move", "resize", etc). If not passed just
    //                        finish all animations
    // @visibility internal
    // @group animation
    //<
    
    finishAnimation : function (type) {   
    
        // if type is null finish animations of all types
        if (type == null) {
            for (var i = 0 ; i < this._animations.length; i++) {
                this.finishAnimation(this._animations[i]);
            }
            return;
        }
        
        // Every animation stores it's currently registered animation as this.[type]Animation
        // If we're not currently performing an animation of this type no need to proceed
        var ID = this._getAnimationID(type);
        if (!this[ID]) return;
        // Call 'finishAnimation' directly on the Animation class. This will cancel further
        // animations and fire the animation action with a ratio of 1, passing in the 
        // 'earlyFinish' parameter.
        if (this.logIsInfoEnabled("animation")) {
            this.logInfo("manual finish for animations: " + this.echoAll(this[ID]) +
                          (this.logIsDebugEnabled("animation") ? this.getStackTrace() : ""),
                          "animation");
        }

        isc.Animation.finishAnimation(this[ID]);
    },
    
    // --------------------------------
    // Developer visible APIS:
	    
	//> @method Callbacks.AnimationCallback
	// A +link{type:Callback} called when the move completes.
	//
	// @param earlyFinish (boolean)  parameter will be passed if the animation was
    //                               cut short by a call to finishAnimation
	//
	// @visibility external
	//<
    
    //> @method canvas.animateMove()
    // Animate a reposition of this canvas from its current position to the specified position
    // @param left (Integer) new left position (or null for unchanged)
    // @param top (Integer) new top position (or null for unchanged)
    // @param [callback] (AnimationCallback) When the move completes this callback will be fired. Single
    //                            'earlyFinish' parameter will be passed if the animation was
    //                            cut short by a call to finishAnimation
    // @param [duration] (Integer) Duration in ms of the animated move 
    // @param [acceleration] (AnimationAcceleration) Optional acceleration effect to bias the ratios
    // @visibility animation
    // @group animation
    // @example animateMove
    //<
    
    _$move:"move",
    animateMove : function (left, top, callback, duration, acceleration) {
        return this.animateRect(left, top, null, null, callback, duration, 
                                acceleration, this._$move);
    },
    fireAnimationMove : function (ratio, ID, earlyFinish) {
        // pass along the additional "type" parameter
        return this.fireAnimationRect(ratio, ID, earlyFinish, this._$move);
    },
    
    //> @method canvas.animateResize()
    // Animate a resize of this canvas from its current size to the specified size
    // @param width (Integer) new width (or null for unchanged)
    // @param height (Integer) new height (or null for unchanged)
    // @param [callback] (AnimationCallback) When the resize completes this callback will be fired. Single
    //                              'earlyFinish' parameter will be passed if the animation was
    //                              cut short by a call to finishAnimation
    // @param [duration] (Integer) Duration in ms of the animated resize 
    // @param [acceleration] (AnimationAcceleration) Optional acceleration effect to apply to the resize
    // @visibility animation
    // @group animation
    // @example animateResize
    //<
    _$resize:"resize",
    animateResize : function (width, height, callback, duration, acceleration) {
        return this.animateRect(null, null, width, height, callback, duration, acceleration, 
                                    this._$resize);
    },
    fireAnimationResize : function (ratio, ID, earlyFinish) {
        // pass along the additional 'type' parameter
        return this.fireAnimationRect(ratio, ID, earlyFinish, this._$resize);
    },
    
    //> @method canvas.animateRect()
    // Animate a reposition / resize of this canvas from its current size and position.
    // @param left (Integer) new left position (or null for unchanged)
    // @param top (Integer) new top position (or null for unchanged)
    // @param width (Integer) new width (or null for unchanged)
    // @param height (Integer) new height (or null for unchanged)
    // @param [callback] (AnimationCallback) When the setRect completes this callback will be fired. Single
    //                              'earlyFinish' parameter will be passed if the animation was
    //                              cut short by a call to finishAnimation
    // @param [duration] (Integer) Duration in ms of the animated setRect 
    // @param [acceleration] (AnimationAcceleration) Optional acceleration effect to apply to the animation
    // @visibility animation
    // @group animation
    // @example animateZoom
    //<
    // Additional type parameter allows us to pick up default durations for animated 
    // move / resizes, which fall through to this method
    
    _$rect:"rect",
    animateRect : function (left, top, width, height, callback, duration, acceleration, type) {
        if (type == null) {
            type = this._$rect;
            // when starting a new "rect" animation, we need to finish any currently running
            // "resize", "move", or "rect" animations.  "rect" animations will automatically be
            // killed by starting a new "rect" animation (in _startAnimation()), but we have to
            // kill "resize" or "move" animations here directly
            if (this.resizeAnimation != null) this.finishAnimation(this._$resize);
            if (this.moveAnimation != null) this.finishAnimation(this._$move);
        }
        
        // This info object will be available to the repeatedly fired animation action as
        // this.$rectAnimationInfo
        var info = {_fromRect:this.getRect(), _left:left, _top:top, _width:width, _height:height,
                    _callback:callback};                  
        // call this._startAnimation() to handle actually setting up the animation.
        return this._startAnimation(type, info, duration, acceleration);
    },
    
    // fireAnimationRect() - fired repeatedly on a timer as the setRect animation proceeds
    // when ratio == 1 the animation is complete
    // Note: we rely on this naming scheme "fireAnimation[AnimationType]" 
    // in '_startAnimation()' / 'finishAnimation()'
    fireAnimationRect : function (ratio, ID, earlyFinish, type) {

        
        var info = (type == this._$resize ? this.$resizeAnimationInfo : 
                    (type == this._$move ? this.$moveAnimationInfo : this.$rectAnimationInfo)),
            fromRect = info._fromRect,
            toLeft = info._left, toTop = info._top, 
            toWidth = info._width, toHeight = info._height,
            
            left =   
                toLeft != null ? this._getRatioTargetValue(fromRect[0], toLeft, ratio) : null,
            top = 
                toTop != null ? this._getRatioTargetValue(fromRect[1], toTop, ratio) : null;

        // hueristic for smooth enlarge/shrink and similar animations: during eg a shrink from
        // all 4 corners, we are increasing left while shrinking width.  In order for centered
        // content within the rect to stay at a stable position, width must shrink by exactly
        // double what left changes by.  This won't happen if we do separate 
        // (ratio * difference) calculations, no matter if we use Math.round,ceil,or floor.
        // Instead, if width and left or height and top are both changing and are an even
        // multiple of each other, use a multiple of left's delta for width instead of
        // calculating the width delta separately (likewise for top/height).
        var width, height;
        if (toWidth != null && left != null && (toLeft - fromRect[0] != 0)) {
            var sideRatio = (toWidth - fromRect[2]) / (toLeft - fromRect[0]);
            if (Math.floor(sideRatio) == sideRatio) {
                //this.logWarn("using ratio: " + sideRatio + 
                //             ", fromRect: " + fromRect + 
                //             ", toLeft,toWidth: " + [toLeft,toWidth] +
                //             ", on delta: " + (left - fromRect[0]));
                width = fromRect[2] + (sideRatio * (left - fromRect[0]))
            }
        }
        if (toHeight != null && top != null && (toTop - fromRect[1] != 0)) {
            var sideRatio = (toHeight - fromRect[3]) / (toTop - fromRect[1]);
            if (Math.floor(sideRatio) == sideRatio) {
                height = fromRect[3] + (sideRatio * (top - fromRect[1]))
            }
        }
        if (width == null && toWidth != null) {
            width = this._getRatioTargetValue(fromRect[2], toWidth, ratio);
        }
        if (height == null && toHeight != null) {
            height = this._getRatioTargetValue(fromRect[3], toHeight, ratio);
        }
      
        if (ratio == 1) {
            if (type == null) type = "rect";
            this._clearAnimationInfo(type);
        }
        // Pass in the additional 'animating' param.
        // - avoids setRect from relaying out children
        // - notifies it that this is not an external setRect call in the middle of an
        //   animation.
        
        //this.logWarn("ratio: " + ratio + ", animateRect: " + [left,top,width,height]);

        this.setRect(left, top, width, height, (ratio < 1));
        
        if (this.isDirty()) this.redraw("animated resize");

        if (ratio == 1) {
            this._fireAnimationCompletionCallback(info._callback, earlyFinish);
        }
    },
    
    // A very common pattern in our animations is to step incrementally from one value to 
    // another.  
    _getRatioTargetValue : function (from, to, ratio) {
        // Common thing - a null 'to' indicates no change
        if (to == null) return from;
        return (from + Math.floor(ratio * (to - from)));
    },

    
    //> @method canvas.animateFade()
    // Animate a change in opacity from the widget's current opacity to the specified opacity.
    // @param opacity (Integer) desired final opacity
    // @param [callback] (AnimationCallback) When the fade completes this callback will be fired. Single
    //                              'earlyFinish' parameter will be passed if the animation was
    //                              cut short by a call to finishAnimation
    // @param [duration] (Integer) Duration in ms of the animated fade 
    // @param [acceleration] (AnimationAcceleration) Optional animation acceleration to bias the ratios
    // @visibility animation
    // @group animation
    // @example animateFade
    //<
    animateFade : function (opacity, callback, duration, acceleration) {
        
        // if we're undrawn, just set the opacity instantly.
        if (!this.isDrawn()) {
            this.setOpacity(opacity);
            this._fireAnimationCompletionCallback(callback, true);
            return;
        }
        
        if (this.visibility == isc.Canvas.HIDDEN) {
            this.setOpacity(0);
            this.show();
        }
        // opacity of 'null' implies default - 100%
        if (opacity == null) opacity = 100;
        var info = {_fromOpacity:this.opacity != null ? this.opacity : 100,
                    _toOpacity:opacity, _callback:callback};
        return this._startAnimation("fade", info, duration, acceleration)
    },
    
    // fireAnimationFade() - fired repeatedly to perform an animation fade.
    fireAnimationFade : function (ratio, ID, earlyFinish) {
        var info = this.$fadeAnimationInfo,
            fromOpacity = info._fromOpacity,
            toOpacity = info._toOpacity;

        var opacity = this._getRatioTargetValue(fromOpacity, toOpacity, ratio);
        
        if (isc.Browser.isIE && opacity > 0 && !info._toggledVis && !isc.Browser.isIE9) {
            var styleHandle = this.getStyleHandle();
            if (styleHandle) {
                styleHandle.visibility = isc.Canvas.VISIBLE;
                styleHandle.visibility = isc.Canvas.INHERIT;
            }
            // we also need to toggle visibility of any peers which get faded with the master!
            var peers = this.peers;
            if (peers && peers.length > 0) {
                for (var i = 0; i < peers.length; i++) {
                    if (peers[i]._setOpacityWithMaster) {
                        var styleHandle = peers[i].getStyleHandle();
                        if (styleHandle) {
                            styleHandle.visibility = isc.Canvas.VISIBLE;
                            styleHandle.visibility = isc.Canvas.INHERIT;
                        }
                    }
                }
            }
            
            info._toggledVis = true;
        }

        
        if (ratio == 1) {
            this._clearAnimationInfo("fade");
        }
        this.setOpacity(opacity, (ratio < 1));
        if (ratio == 1) this._fireAnimationCompletionCallback(info._callback, earlyFinish);
    },
    

    //> @method canvas.animateScroll()
    // Animate a scroll from the current scroll position to the specified position.
    // @param scrollLeft (Integer) desired final left scroll position
    // @param scrollTop (Integer) desired final top scroll position
    // @param [callback] (AnimationCallback) When the scroll completes this callback will be fired. Single
    //                              'earlyFinish' parameter will be passed if the animation was
    //                              cut short by a call to finishAnimation
    // @param [duration] (Integer) Duration in ms of the animated scroll 
    // @param [acceleration] (AnimationAcceleration) Optional acceleration to bias the animation ratios
    // @visibility animation
    // @group animation
    //<
    animateScroll : function (scrollLeft, scrollTop, callback, duration, acceleration) {
        var overflow = this.overflow;
        if (this.overflow == isc.Canvas.VISIBLE) return;
        
        var info = {_fromLeft:this.getScrollLeft(), _fromTop:this.getScrollTop(),
                    _toLeft:scrollLeft, _toTop:scrollTop, _callback:callback};
        return this._startAnimation("scroll", info, duration, acceleration);
    },
    
    fireAnimationScroll : function (ratio, ID, earlyFinish) {
        var info = this.$scrollAnimationInfo,
            fromLeft = info._fromLeft, toLeft = info._toLeft,
            fromTop = info._fromTop, toTop = info._toTop,
            newLeft = this._getRatioTargetValue(fromLeft, toLeft, ratio),
            newTop = this._getRatioTargetValue(fromTop, toTop, ratio);
            
        if (ratio == 1) {
            this._clearAnimationInfo("scroll");
        }
        this.scrollTo(newLeft, newTop, null, (ratio < 1));
        if (ratio ==1 && info._callback) {
            this._fireAnimationCompletionCallback(info._callback, earlyFinish);
        }
    },
    
    
    // animate show effect / effectID split into separate objects for clarity / integration with
    // tools etc
    // (We could also separate animate hide effects from animate show effects). 
    //> @type animateShowEffectId
    // String specifying effect to apply during an animated show or hide.
    // @value "slide" content slides into or out of view as the widget grows or shrinks
    // @value "wipe" content is revealed or wiped as the widget grows or shrinks
    // @value "fade" widget's opacity smoothly fades into or out of view
    // @value "fly" widget moves into position from offscreen
    // @visibility animation
    //<
    
    //> @object AnimateShowEffect
    // Configuration object for effect to apply during an animated show or hide.
    // @treeLocation Client Reference/System
    // @visibility animation
    //<
    //> @attr AnimateShowEffect.effect (animateShowEffectId : null : IR) 
    // Effect to apply
    // @visibility animation
    //<
    
    //> @attr AnimateShowEffect.startFrom (string : null : IR)
    //   For show animations of type <code>"wipe"</code> and
    //   <code>"slide"</code> this attribute specifies where the wipe / slide should originate.
    //   Valid values are <code>"T"</code> (vertical animation from the top down, the 
    //   default behavior), and <code>"L"</code> (horizontal animation from the left side).
    // @visibility animation
    //<
    
    //> @attr AnimateShowEffect.endsAt (string : null : IR)
    //   For hide animations of type <code>"wipe</code> and 
    //   <code>"slide"</code> this attribute specifies where the wipe / slide should finish.
    //   Valid options are <code>"T"</code> (vertical animation upwards to the top of the canvas,
    //   the default behavior) and <code>"L"</code> (horizontal animation to the left of the
    //   canvas).
    // @visibility animation
    //<
    
    //> @method canvas.animateShow()
    // Show a canvas by growing it vertically to its fully drawn height over a period of time.
    // This method will not fire if the widget is already drawn and visible, or has overflow
    // other than <code>"visible"</code> or <code>"hidden"</code>.
    // @param [effect] (animateShowEffectId | AnimateShowEffect) Animation effect to use
    //      when revealing the widget. If ommitted, default behavior can be configured via
    //      +link{Canvas.animateShowEffect}
    // @param [callback] (AnimationCallback) When the show completes this callback will be fired. Single
    //                              'earlyFinish' parameter will be passed if the animation was
    //                              cut short by a call to finishAnimation.
    // @param [duration] (Integer) Duration in ms of the animated show. If unset, duration will be
    //   picked up from +link{canvas.animateShowTime}
    // @param [acceleration] (AnimationAcceleration) Optional acceleration effect function to 
    //   bias the animation ratios.  If unset, acceleration will be picked up from
    //   +link{canvas.animateShowAcceleration}
    // @visibility animation
    // @group animation
    // @example animateWipe
    //<
    _$show:"show",
    _$slide:"slide",
    _$wipe:"wipe",
    _$fade:"fade",
    _$fly:"fly",
    _$T:"T", _$L:"L",
    _showEffectAnimationMap:{slide:"show", wipe:"show", fly:"move", fade:"fade"},
    animateShow : function (effect, callback, duration, acceleration) {
        // have a way to default the animate show / hide effect for all calls to these methods
        if (effect == null) effect = this.animateShowEffect;
        
        var effectConfig;
        if (isc.isAn.Object(effect)) {
            effectConfig = effect;
            effect = effect.effect;
        }
        // If we're in the process of doing an animateHide(), finish that before we do the
        // animateShow() - this is required to avoid a no-op due to the fact that the widget
        // is currently drawn/visible.
        if (this._animatingHide != null) this.finishAnimation(this._animatingHide);
        
        // Could fire callback if it's already showing?
        if (this.isDrawn() && this.isVisible()) {
            return;
        }
        // Also - if we're in mid 'animateShow()' just bail
        
        if (this._animatingShow != null) {
            return;
        }
        
        // If we're undrawn, draw() if _drawOnShow() is true - true for top level widgets 
        // that are not peers.
        // Otherwise fall through to default 'show()' method to show [without drawing] 
        // immediately
        if (!this.isDrawn()) {
            if (this.parentElement && !this.parentElement.isDrawn()) {
                this.show(); 
                this.logInfo("not animating show, component not drawn", "animation");
                // again - this is an 'early finish'
                this.animateShowComplete(true);                
                return;
            } else {
                this.draw();
            }
        }
        
        // animateShow() / animateHide() fall through to various methods to perform the actual
        // animation based on the effect passed in.
        // This means we can't just check 'this.isAnimating("show")' or 'this.isAnimating("hide")'
        // - the animation may be performed via a move or fade.
        // Add a flag at the beginning of animateShow() / animateHide() so we can readily check
        // for the case where we're in this state.
        // Also - always fire an "animateShowComplete()" callback when the show/hide completes
        // this allows us to clear the flag before firing whatever callback was passed into
        // this method.
        this._animatingShow = this._showEffectAnimationMap[effect] || this._$show;
        this._animateShowCallback = callback;
        
        if (!this._animateShowCompleteCallback) 
            this._animateShowCompleteCallback = {target:this, methodName:"animateShowComplete"}

        if (effect == this._$fade) {
            var targetOpacity = this.opacity;
            this._fadeShowCallback = callback;
            this.setOpacity(0);
            this.show();
            // Explicitly default to animateShowTime / animateShowAcceleration rather than
            // falling through to animateFadeTime / Acceleration 
            if (duration == null) duration = this.animateShowTime;
            if (acceleration == null) acceleration = this.animateShowAcceleration;
            // Simply fall through to animate fade, then fire the callback on completion.
            return this.animateFade(targetOpacity, this._animateShowCompleteCallback,
                                    duration, acceleration);
        } else if (effect == this._$fly) {
            // fly effect not currently supported for non-top-level widgets
            
            if (this.parentElement != null) {           
                this.logInfo("animateShow() called with 'fly' effect - not supported for child widgets" + 
                             " defaulting to standard 'wipe' animation instead.", "animation");
                effect = this._$wipe; 
            } else {

                // Explicitly default to animateShowTime / animateShowAcceleration rather than
                // falling through to animateMoveTime / Acceleration 
                if (duration == null) duration = this.animateShowTime;
                if (acceleration == null) acceleration = this.animateShowAcceleration;
                // Simply fall through to animate move, then fire the callback on completion.

                var rtl = this.isRTL(),
                    specifiedLeft = this.getLeft(),
                    offscreenLeft = rtl ? isc.Page.getWidth() + isc.Page.getScrollLeft()
                                        : 0 - this.getVisibleWidth();

                this._flyShowPercentLeft = this._percent_left,                                        
                
                this.setLeft(offscreenLeft);
                this.show();
                return this.animateMove(specifiedLeft, null, this._animateShowCompleteCallback,
                                        duration, acceleration);
            }
        }
        // If we can't animate the show, just show and fire callback
        if (!this._canAnimateClip(effect)) {
            this.logInfo("not animating show, can't do clip animations", "animation");            
            this.show();
            // essentially this is an 'early finish'
            this.animateShowComplete(true);
            return;
        } 
        
        // Start from drawn / hidden - this way we can get the drawn scrollHeight.
        if (this.isVisible()) this.hide();
        
        var drawnHeight = this.getVisibleHeight(),
            drawnWidth = this.getVisibleWidth(),
            // default to showing from the top down
            // Note that we currently just support top down or left in so convert this to a 
            // boolean for simplicity
            
            vertical = effectConfig ? effectConfig.startFrom == this._$T : true,
            scrollStart = (vertical ? this.getScrollTop() : this.getScrollLeft()),
            slideIn = (effect == "slide"),
            
            info = {
                _userHeight:this._userHeight, _specifiedHeight:this.getHeight(),   
                _drawnHeight:drawnHeight,
                _userWidth:this._userWidth, _specifiedWidth:this.getWidth(),
                _drawnWidth:drawnWidth,
                
                _percentWidth:this._percent_width, _percentHeight:this._percent_height,

                _originalOverflow:this.overflow,
                
                _vertical:vertical,
                _scrollStart:scrollStart,
                _slideIn:slideIn,
                
                _callback:this._animateShowCompleteCallback
            };

        if (vertical) {
            if (this.vscrollOn && this.vscrollbar) {
                info._scrollThumbStart = this.vscrollbar.thumb.getTop();
                info._scrollThumbLength = this.vscrollbar.thumb.getHeight();
                
                // don't show the thumb with the s-b - we'll show it and grow it into view
                if (this.vscrollbar.thumb) {
                    this.vscrollbar.thumb._showWithMaster = false;
                    this.vscrollbar.thumb._suppressImageResize = true;
                }
                
                this.vscrollbar._suppressSetThumb = true;
                this.vscrollbar._suppressImageResize = true;
    
                // resize the scrollbar to be 1px so it doesn't flash when first shown
                this.vscrollbar.setHeight(1);
            }
            if (this.hscrollOn && this.hscrollbar) {
                this.hscrollbar._suppressImageResize = true;
                if (this.hscrollbar.thumb) this.hscrollbar.thumb._suppressImageResize = true;
                // If we're doing a wipe, we won't show the breadth scrollbar until
                // the rest of the widget has been 'wiped' into view
                if (!info._slideIn) {
                    this.hscrollbar._showWithMaster = false;
                } else {
                    this.hscrollbar.setTop(this.getTop());
                    this.hscrollbar.setHeight(1);
                }
            }
            
        } else {
            if (this.hscrollOn && this.hscrollbar) {
                info._scrollThumbStart = this.hscrollbar.thumb.getLeft();        
                info._scrollThumbLength = this.hscrollbar.thumb.getWidth();
                
                this.hscrollbar._suppressSetThumb = true;
                this.hscrollbar._suppressImageResize = true;
    
                
                // don't show the thumb with the s-b - we'll show it and grow it into view
                if (this.hscrollbar.thumb) {
                    this.hscrollbar.thumb._showWithMaster = false;            
                    this.hscrollbar.thumb._suppressImageResize = true;
                }
                this.hscrollbar.setWidth(1);
            } 
            if (this.vscrollOn && this.vscrollbar) {
                this.vscrollbar._suppressImageResize = true;
                if (this.vscrollbar.thumb) this.vscrollbar.thumb._suppressImageResize = true;
                // If we're doing a wipe, we won't show the breadth scrollbar until
                // the rest of the widget has been 'wiped' into view
                if (!info._slideIn) {
                    this.vscrollbar._showWithMaster = false;
                } else {
                    this.vscrollbar.setLeft(this.getLeft());
                    this.vscrollbar.setWidth(1);
                }
            }
        }
        
        // If we have a visible edged canvas suppress react to resize before we set overflow
        // to hidden or resize the handle so the edged canvas doesn't get resized as a peer
        
        if (this.showEdges && this._edgedCanvas) {
            this._edgedCanvas._suppressReactToResize = true;
        }
        
        // Set overflow to hidden, then grow to the drawn size (and then reset overflow)
        if (this.overflow == isc.Canvas.VISIBLE) {
            this.setOverflowForAnimation(isc.Canvas.HIDDEN, this.overflow);
        }

        // suppress adjustOverflow during the animation if we have scrollbars
        if (this.overflow == isc.Canvas.AUTO || this.overflow == isc.Canvas.SCROLL) { 
            this._suppressAdjustOverflow = true;
        }
        
        // additional param indicates that this is an animated resize
        this.resizeTo((vertical ? drawnWidth : 1), (vertical ? 1 : drawnHeight), true);
        if (slideIn) this.scrollTo((vertical ? null : scrollStart + (drawnWidth-1)), 
                                   (vertical ? scrollStart + (drawnHeight-1) : null));


        if (this.showEdges && this._edgedCanvas) {
            // Explicitly size the edgeCanvas' table (rather than sizing at 100%), so that
            // as the edged canvas resizes, the edge clips rather than growing/shrinking
            if (vertical) 
                this._assignSize(this._edgedCanvas.getHandle().firstChild.style, "height", drawnHeight);
            else 
                this._assignSize(this._edgedCanvas.getHandle().firstChild.style, "width", drawnWidth);
                
            this._edgedCanvas.setOverflow(isc.Canvas.HIDDEN);
                
            // If we're sliding in, align the handle with the top of the edged canvas to 
            // start with, so it will grow down from the top.
            if (slideIn) {
                if (vertical) {
                    var startEdgeSize = this._edgedCanvas._topMargin;
                    this._assignSize(this.getStyleHandle(), "marginTop", (this.getTopMargin() - startEdgeSize));
                } else {
                    var startEdgeSize = this._edgedCanvas._leftMargin;
                    this._assignSize(this.getStyleHandle(), "marginLeft", (this.getLeftMargin() - startEdgeSize));
                }
            } 
            
            // Don't show the main Canvas right away if we have edges, just show the edges.
            // Only show the edges, then show the main Canvas when the animation has passed the
            // edge.
            this._edgedCanvas.show();
        } else {
            var breadthScrollbar = vertical ? (this.hscrollOn ? this.hscrollbar : null) 
                                            : (this.vscrollOn ? this.vscrollbar : null),
                                            
                lengthScrollbar = vertical ? (this.vscrollOn ? this.vscrollbar : null)
                                           : (this.hscrollOn ? this.hscrollbar : null);
        
            // If we're sliding in, and we have an h-scrollbar, show it and allow it to grow 
            // before showing this canvas
            if (breadthScrollbar && info._slideIn) {
                breadthScrollbar.show();
                if (lengthScrollbar) lengthScrollbar.show();
            } else {
                this.show();
            }
        }

        return this._startAnimation(this._$show, info, duration, acceleration);

    },
    
    // Actually fire the show animation
    // Grows the widget (according to the current ratio), and if slideIn is true keeps scrolled
    // so the content appears to slide in with the bottom of the widget.
    fireAnimationShow : function (ratio, ID, earlyFinish) {
        var info = this.$showAnimationInfo,
            vertical = info._vertical;

                
        if (ratio < 1) {
            var drawnSize = (vertical ? info._drawnHeight : info._drawnWidth),
                size = this._getRatioTargetValue(1, drawnSize, ratio),
                delta = drawnSize - size,
                adjustForEdge = (this.showEdges && this._edgedCanvas),
                // Note if we're wiping into view we show the top edge, then the bottom edge
                // if we're sliding into view it's the other way around because of scrolling
                startEdgeSize, endEdgeSize;
            

            if (adjustForEdge) {
                // Note: we can't just check this.edgeSize, since we support asymmetric edges
                // and by default the value is just picked up from the EdgedCanvas class.
                startEdgeSize = (info._slideIn ? (vertical ? this._edgedCanvas._bottomMargin 
                                                           : this._edgedCanvas._rightMargin)
                                               : (vertical ? this._edgedCanvas._topMargin
                                                           : this._edgedCanvas._leftMargin)),
                endEdgeSize = (info._slideIn ? (vertical ? this._edgedCanvas._topMargin
                                                         : this._edgedCanvas._leftMargin)
                                             : (vertical ? this._edgedCanvas._bottomMargin
                                                         : this._edgedCanvas._rightMargin));
                
                this._edgedCanvas.resizeTo((vertical ? null : size), (vertical ? size: null), true);
                if (info._slideIn) {
                    if (vertical) this._edgedCanvas.scrollToBottom();
                    else this._edgedCanvas.scrollToRight();
                }
                
                // Just bail if we haven't started to expose the actual handle yet.
                if (size < startEdgeSize) return;

                // We don't need to resize the handle once it's completely exposed (at this
                // point we're just revealing the final edge)
                if (delta <= endEdgeSize) {
                    // If sliding in, align the top of the handle with the bottom of the
                    // top edge, and ensure we're now scrolled to our final scroll position
                    // (avoids a jump when we actually reach ratio 1).
                    if (info._slideIn) {
                        var marginProp = (vertical ? "marginTop" : "marginLeft"),
                            marginSize = (vertical ? this.getTopMargin() - delta 
                                                   : this.getLeftMargin() - delta);
                        this._assignSize(this.getStyleHandle(), marginProp, marginSize);
                                         
                        this.scrollTo((vertical ? null : info._scrollStart), 
                                      (vertical ? info._scrollTop : null), 
                                      null, true);
                    }
                    return;
                }
                
                // If we got here, we know the handle should be >= 1px tall, so needs to be
                // visible
                
                if (!this.isVisible()) {
                    this._showingAsAnimation = true;
                    this.show();
                    delete this._showingAsAnimation;
                }
            }


            var lengthScrollOn = vertical ? this.vscrollOn : this.hscrollOn,
                breadthScrollOn = vertical ? this.hscrollOn : this.vscrollOn;
            if (lengthScrollOn) {
                var lengthScrollbar;
                if (vertical) {
                    lengthScrollbar = this.vscrollbar;
                    if (lengthScrollbar) lengthScrollbar.resizeTo(null, size);
                } else {
                    lengthScrollbar = this.hscrollbar;
                    var sbsize = size;
                    if (this.vscrollOn) {
                        if (info._slideIn) {
                            sbsize -= this.scrollbarSize;
                        } else {
                            sbsize = Math.min(size, drawnSize-this.scrollbarSize);
                        }
                    }
                    if (sbsize > 0) {
                        if (lengthScrollbar) lengthScrollbar.resizeTo(sbsize, null);
                    }
                }

                if (info._slideIn && lengthScrollbar) {
                    if (vertical) lengthScrollbar.scrollToBottom();
                    else lengthScrollbar.scrollToRight();
                }
                
                // thumb
                if (lengthScrollbar && lengthScrollbar.thumb) {
                    var thumb = lengthScrollbar.thumb;
                    
                    // On a "slideIn" we need to grow the thumb then shift it in from the top
                    if (info._slideIn) {
                        var thumbStart = info._scrollThumbStart - delta,
                            thumbEnd = thumbStart + Math.min(size, info._scrollThumbLength),            
                            start = vertical ? this.getTop() : this.getLeft();
                            
                        if (thumbEnd <= start) {
                            // thumb should already be hidden - don't show it
                        } else {
                            // shorten the thumb if necessary
                            thumbStart = Math.max(start, thumbStart);
                            var thumbLength = Math.min(thumbEnd-thumbStart, size);
                            
                            thumb.resizeTo(vertical ? null : thumbLength,
                                           vertical ? thumbLength : null);                         

                            if (vertical) thumb.scrollToBottom()
                            else thumb.scrollToRight();

                            thumb.moveTo(vertical ? null : thumbStart,
                                         vertical ? thumbStart: null);                                         
                            if (!thumb.isVisible()) thumb.show();
                        } 
                        
                    // on a wipe animation, we simply show, then resize the thumb from the
                    // top down
                    } else {
                        var thumbStart = info._scrollThumbStart,
                            thumbEnd = Math.min((thumbStart + info._scrollThumbLength),
                                                (vertical ? 
                                                    this.getTop() + size : 
                                                    this.getLeft() + size));

                        var end = (vertical ? this.getTop() : this.getLeft()) + size
                        if (end <= thumbStart) {
                            // don't show the thumb yet unless its in view
                        } else {
                            if (vertical) thumb.setHeight(thumbEnd -thumbStart);
                            else thumb.setWidth(thumbEnd-thumbStart);
                            if (!thumb.isVisible()) thumb.show();
                        }
                    }
                }   
            }
            
            // If we're showing a breadth scrollbar, the widget handle renders at the the
            // specified size less the scrollbarsize
            // If the scrollbar is (partially or fully) hidden we therefore need to increase our
            // specified size by the difference between the rendered scrollbarSize and 
            // this.scrollbarSize to ensure the handle draws large enough
            var hiddenScrollbarDelta = 0;
            if (breadthScrollOn && breadthScrollbar) {
                var breadthScrollbar = vertical ? this.hscrollbar : this.vscrollbar;
                if (info._slideIn) {
                    
                    var sbStart = vertical ? (this.getTop() + Math.max(0, (size - this.scrollbarSize)))
                                           : (this.getLeft() + Math.max(0, (size - this.scrollbarSize)))
                    breadthScrollbar.moveTo(vertical ? null : sbStart, vertical ? sbStart : null);
                    
                    var sbSize = Math.min(size, this.scrollbarSize);
                    breadthScrollbar.resizeTo(vertical ? null : sbSize, 
                                              vertical ? sbSize : null);

                    if (vertical) {
                        breadthScrollbar.scrollToBottom();
                        if (breadthScrollbar.thumb) breadthScrollbar.thumb.scrollToBottom();
                    } else {
                        breadthScrollbar.scrollToRight();
                        if (breadthScrollbar.thumb) breadthScrollbar.thumb.scrollToRight();
                    }


                    if (size > this.scrollbarSize && !this.isVisible()) {
                        
                        this._showingAsAnimation = true;
                        this.show();
                        delete this._showingAsAnimation;
                    }
                } else {
                    if (delta <= this.scrollbarSize) {
                        if (!breadthScrollbar.isVisible()) breadthScrollbar.show();                     
                        breadthScrollbar.resizeTo(vertical ? null : this.scrollbarSize-delta,
                                                  vertical ? this.scrollbarSize-delta : null);
                    } 
                    // Otherwise we know the scrollbar isn't showing - nothing to do here
                }

                if (breadthScrollbar.isVisible()) {
                    hiddenScrollbarDelta = this.scrollbarSize - 
                                            (vertical ? breadthScrollbar.getHeight() 
                                                      : breadthScrollbar.getWidth());
                } else {
                    hiddenScrollbarDelta = this.scrollbarSize;
                }
            }
            
            // Actually resize the handle.
            // If we're showing edges, modify the height passed to resizeTo so we don't account
            // for the bottom edge which is currently clipped.
            var handleSize = size;
            if (adjustForEdge) handleSize += endEdgeSize;
            // ditto with the "breadth" axis scrollbar
            if (hiddenScrollbarDelta) handleSize += hiddenScrollbarDelta
            

            // Note additional param 
            // - to avoid firing layoutChildren() on every step.
            // - notify that this is not an external 'resize' call
            
            if (!this.resizeTo((vertical ? null : handleSize), (vertical ? handleSize : null),
                                true))
            {
                this._resized();
            }
            if (info._slideIn) { 
                this.scrollTo((vertical ? null : info._scrollStart + delta),
                              (vertical ? info._scrollStart + delta : null), 
                              null, true);
            }
            
        // Ratio == 1
        } else {
            // If we are showing edges we're not show()n until we get pushed / scrolled into
            // view.
            // If we're not visible now, call this.show()
            // (Only likely to happen from an early finish of the animation)
            if (!this.isVisible()) this.show();
            
            this._clearAnimationInfo("show");
            
            if (!this.resizeTo(info._specifiedWidth, info._specifiedHeight)) {
                // force _resized() notification to fire - this is required since if we're resizing
                // a breadth scrollbar our reported size may not have changed but our drawn size
                // may  have. We still want layouts etc to respond to the size change.
                this._resized();
            }
            this.setOverflowForAnimation(info._originalOverflow);
            
            if (this.overflow == isc.Canvas.AUTO || this.overflow == isc.Canvas.SCROLL) {
                delete this._suppressAdjustOverflow;

                // reset all the properties for auto-management of scrollbar.
                if (this.vscrollOn && this.vscrollbar) {
                    if (this.vscrollbar.visibility == isc.Canvas.HIDDEN) this.vscrollbar.show();
                    if (vertical) delete this.vscrollbar._suppressSetThumb;
                    delete this.vscrollbar._suppressImageResize;
                    this.vscrollbar._showWithMaster = true;
                    if (info._slideIn) this.vscrollbar.scrollTo(0,0);
                    if (this.vscrollbar.thumb) {
                        delete this.vscrollbar.thumb._suppressImageResize;
                        this.vscrollbar.thumb._showWithMaster = true;
                        if (info._sideIn) this.vscrollbar.thumb.scrollTo(0,0);
                    }
                    if (!vertical) {
                        this.vscrollbar.setWidth(this.getScrollbarSize());
                        this.vscrollbar.setThumb();
                    }
                }
                if (this.hscrollOn && this.hscrollbar) {
                    if (this.hscrollbar.visibility == isc.Canvas.HIDDEN) this.hscrollbar.show();
                    if (!vertical) {
                        delete this.hscrollbar._suppressSetThumb;
                    } else {
                        this.hscrollbar.setHeight(this.getScrollbarSize());
                        this.hscrollbar.setThumb();
                    }
                    delete this.hscrollbar._suppressImageResize;
                    this.hscrollbar._showWithMaster = true;
                    if (info._slideIn) this.hscrollbar.scrollTo(0,0);
                    if (this.hscrollbar.thumb) {
                        delete this.hscrollbar.thumb._suppressImageResize;
                        this.hscrollbar.thumb._showWithMaster = true;
                        if (info._slideIn) this.hscrollbar.thumb.scrollTo(0,0);
                    }
                }
            }
            
            if (this.showEdges && this._edgedCanvas) { 
                if (info._slideIn) {
                    var marginProp = (vertical ? "marginTop" : "marginLeft"),
                        marginSize = (vertical ? this.getTopMargin() : this.getLeftMargin());
                    this._assignSize(this.getStyleHandle(), marginProp, marginSize);
                    this._edgedCanvas.scrollTo((vertical ? null : 0), (vertical ? 0 : null));
                }
                if (vertical)
                    this._edgedCanvas.getHandle().firstChild.style.height = "100%";
                else 
                    this._edgedCanvas.getHandle().firstChild.style.width = "100%";
                                    
                this._edgedCanvas.setOverflow(isc.Canvas.VISIBLE);            
                delete this._edgedCanvas._suppressReactToResize;
            }
            
            
            
            this._userWidth = info._userWidth;
            this._userHeight = info._userHeight;
            
            this._percent_width = info._percentWidth;
            this._percent_height = info._percentHeight;
            
            if (info._slideIn) this.scrollTo((vertical ? null : info._scrollStart), 
                                             (vertical ? info._scrollStart : null));
            if (info._callback) {
                this._fireAnimationCompletionCallback(info._callback, earlyFinish);
            }
        }
    },
    
    // When doing an animateShow/animateHide we have to temporarily set overflow to "hidden"
    // so the user sees the handle clip its content as expected.
    // Use a separate method for this and set a flag so if necessary widgets can see
    // what the actual, suppressed overflow is
    
    setOverflowForAnimation : function (overflow, specifiedOverflow) {
        if (specifiedOverflow != null) {
            this._$suppressedOverflowDuringAnimation = specifiedOverflow;
        } else {
            delete this._$suppressedOverflowDuringAnimation;
        }
        this.setOverflow(overflow);
    },

    // Always fired when show animation completes    
    animateShowComplete : function (earlyFinish) {
        
        if (this._flyShowPercentLeft != null) {
            this._percent_left = this._flyShowPercentLeft;
            delete this._flyShowPercentLeft;
        }
        
        this._animatingShow = null;
        var callback = this._animateShowCallback;
        this._animateShowCallback = null;
        // Pass in the 'synchronous' param to fireAnimationComplete so the callback fires
        // synchronously. animateShowComplete() was itself fired asynchronously so no need 
        // to delay again.
        if (callback) this._fireAnimationCompletionCallback(callback, earlyFinish, true);
    },
    
    //> @attr canvas.canAnimateClip (boolean : null : IRWA)
    // Whether to "wipe" and "slide" show/hide animations.  Default is to allow such animations
    // for non-scrolling widgets.
    // @group animation
    // @visibility internal
    //<
    
    _canAnimateClip : function (effect) {
        if (this.canAnimateClip != null) return this.canAnimateClip;
        // - slide effect calls scrollTo() code - should not do so if scrollTo method has been
        //   overridden, as component may not understand animation, so check:
        //         this.scrollTo == isc.Canvas.getInstanceProperty("scrollTo")
        return (this.scrollTo == isc.Canvas.getInstanceProperty("scrollTo"));
        
    },

    //> @method canvas.animateHide()
    // Hide a canvas by shrinking it vertically to zero height over a period of time.
    // This method will not fire if the widget is already drawn and visible, or has overflow
    // other than <code>"visible"</code> or <code>"hidden"</code>.
    // @param [effect] (animateShowEffectId | animateShowEffect) How should the content of the
    //  window be hidden during the hide? If ommitted, default behavior can be configured via
    //  +link{Canvas.animateHideEffect}
    // @param [callback] (AnimationCallback) When the hide completes this callback will be fired. Single
    //                              'earlyFinish' parameter will be passed if the animation was
    //                              cut short by a call to finishAnimation.
    // @param [duration] (Integer) Duration in ms of the animated hide.  If unset, duration will be
    //   picked up from +link{canvas.animateHideTime} 
    // @param [acceleration] (AnimationAcceleration) Optional acceleration effect function to bias
    //   the animation ratios.  If unset, acceleration will be picked up from
    //   +link{canvas.animateShowTime}
    // @visibility animation
    // @group animation
    // @example animateWipe
    //<
    // @param [synchronousCallback] By default we fire the callback passed into this method
    //                      asynchronously, after the method completes, which allows the
    //                      screen to update before potentially complex callback logic fires
    //                      in some very advanced uses we may require the callback to fire
    //                      synchronously in response to the last step of animation - this
    //                      is achieved via this parameter (used in Layout.js)
    _$hide:"hide",
    _hideEffectAnimationMap:{slide:"hide", wipe:"hide", fly:"move", fade:"fade"},
    animateHide : function (effect, callback, duration, acceleration, synchronousCallback) {
        // have a way to default the animate show / hide effect for all calls to these methods
        if (effect == null) effect = this.animateHideEffect;
        
        var effectConfig;
        if (isc.isAn.Object(effect)) {
            effectConfig = effect;
            effect = effectConfig.effect;
        }
        // Complate any 'show' animation before starting to hide
        if (this._animatingShow != null) {
            this.finishAnimation(this._animatingShow);
        }
        // If we're already hidden, or undrawn, just bail
        
        if (!this.isVisible()) return;
        // don't allow 2 calls to animateHide to overlap
        if (this._animatingHide != null) return;
        // If we're undrawn, don't bother doing an animated hide, just hide and fire the
        // callback.
        
        if (!this.isDrawn() && !isc.isA.LayoutSpacer(this)) {        
            this.hide();
            if (callback) this._fireAnimationCompletionCallback(callback, true);
            return;
        }
        
        
        this._animatingHide = this._hideEffectAnimationMap[effect] || this._$hide;
        this._animateHideCallback = callback;
        if (!this._animateHideCompleteCallback) 
            this._animateHideCompleteCallback = {target:this, methodName:"_animateHideComplete"}

        if (effect == this._$fade) {
            this._fadeHideOpacity = this.opacity;
            
            this._performingFadeHide = true;
            
            // default to hide time rather than 'fade' time if no time was passed in
            if (duration == null) duration = this.animateHideTime;
            if (acceleration == null) acceleration = this.animateHideAcceleration;
            // Simply fall through to animate fade, then fire the callback on completion.
            return this.animateFade(0, this._animateHideCompleteCallback,
                                     duration, acceleration, synchronousCallback);
        } else if (effect == this._$fly) {
            this._flyHideLeft = this.getLeft();
            this._flyHidePercentLeft = this._percent_left;
            
            if (this.parentElement != null) {
                this.logInfo("animateHide() called with 'fly' effect - not supported for child widgets" + 
                             " defaulting to standard 'wipe' animation instead.", "animation");
                effect = this._$wipe; 
            } else {

                // Explicitly default to animateShowTime / animateShowAcceleration rather than
                // falling through to animateMoveTime / Acceleration 
                if (duration == null) duration = this.animateShowTime;
                if (acceleration == null) acceleration = this.animateShowAcceleration;
                // Simply fall through to animate fade, then fire the callback on completion.

                var rtl = this.isRTL(),
                    offscreenLeft = rtl ? isc.Page.getWidth() + isc.Page.getScrollLeft()
                                        : 0 - this.getVisibleWidth();
                
                return this.animateMove(offscreenLeft, null, 
                                        this._animateHideCompleteCallback, 
                                        duration, acceleration, synchronousCallback);
            }
        }
        // at this point we're doing a standard hide type animation with wipe / slide effect
        // HACK: LayoutSpacer never reports itself drawn, but can animate
        if ((!this._canAnimateClip(effect) || !this.isDrawn()) && 
            !this.isA(isc.LayoutSpacer)) 
        {    
            this.logInfo("not animating hide, can't do clip animations", "animation");
            this.hide();
            // pass the early-finish parameter
            this._animateHideComplete(true);
            return;
        }

        var drawnHeight = this.getVisibleHeight(),
            drawnWidth = this.getVisibleWidth(),
            vertical = (effectConfig ? effectConfig.endAt == this._$T : true),
            
            info = {_userHeight:this._userHeight, _specifiedHeight:this.getHeight(), 
                    _drawnHeight:drawnHeight,
                    _userWidth:this._userWidth, _specifiedWidth:this.getWidth(), 
                    _drawnWidth:drawnWidth,
                    _scrollStart:(vertical ? this.getScrollTop() : this.getScrollLeft()),
                    _vertical:vertical,
                    _slideOut:effect == "slide",
                    _originalOverflow:this.overflow,
                    _callback:this._animateHideCompleteCallback,
                    _synchronousCallback:synchronousCallback
            };

        if (info._slideOut) {
            // remember the length-scrollbar thumb position at the beginning of the animation
            // and size
            if (vertical && this.vscrollOn && this.vscrollbar) {
                info._scrollThumbStart = this.vscrollbar.thumb.getTop();
                info._scrollThumbLength = this.vscrollbar.thumb.getHeight();
            } else if (!vertical && this.hscrollOn && this.hscrollbar) {
                info._scrollThumbStart = this.hscrollbar.thumb.getLeft();        
                info._scrollThumbLength = this.hscrollbar.thumb.getWidth();
            }           
        }
        
        // If overflow is visible, set to hidden so the content will clip as we shrink 
        
        this.resizeTo(drawnWidth, drawnHeight, true);
        
        if (this.overflow == isc.Canvas.VISIBLE) {
            this.setOverflowForAnimation(isc.Canvas.HIDDEN, this.overflow);
        }
        // suppress adjustOverflow during the animation if we have scrollbars
        if (this.overflow == isc.Canvas.AUTO || this.overflow == isc.Canvas.SCROLL) {   
            this._suppressAdjustOverflow = true;
            
            if (this.vscrollOn && this.vscrollbar) {
                // allow the master to be hidden without hiding either scrollbar
                this.vscrollbar._showWithMaster = false;
                // avoid setThumb if this is the length scrollbar
                if (vertical) this.vscrollbar._suppressSetThumb = true;
                // avoid image resize so we can clip properly
                this.vscrollbar._suppressImageResize = true;
                
                if (this.vscrollbar.thumb) {
                    this.vscrollbar.thumb._suppressImageResize = true;
                }
            }
            if (this.hscrollOn && this.hscrollbar) {
                // allow the master to be hidden without hiding either scrollbar
                this.hscrollbar._showWithMaster = false;
                // avoid setThumb if this is the length scrollbar
                if (!vertical) this.hscrollbar._suppressSetThumb = true;
                // avoid image resize so we can clip properly
                this.hscrollbar._suppressImageResize = true;
                
                if (this.hscrollbar.thumb) {
                    this.hscrollbar.thumb._suppressImageResize = true;
                }
            }
        }
        
        if (this.showEdges) {
            this._edgedCanvas.setOverflow("hidden");
            this._edgedCanvas._suppressReactToResize = true;
            this._assignSize(this._edgedCanvas.getHandle().firstChild.style,
                             (vertical ? "height" : "width"),
                             (vertical ? this._edgedCanvas.getHeight() : this._edgedCanvas.getWidth()));
        }
        return this._startAnimation(this._$hide, info, duration, acceleration);
    },
    
    // fireAnimationHide() - called repeatedly during an animated show() / hide() if the
    // animate effect is "slide" or "wipe" rather than "fade".
    fireAnimationHide : function (ratio, ID, earlyFinish) {
        var info = this.$hideAnimationInfo,
            vertical = info._vertical;
        
        
        if (ratio < 1) {
            
            var drawnSize = (vertical ? info._drawnHeight : info._drawnWidth),
                size = this._getRatioTargetValue(drawnSize, 1, ratio),
                delta = drawnSize - size,
                adjustForEdge = (this.showEdges && this._edgedCanvas),
                // Note that if we're sliding in we hide the top edge first, whereas if
                // we're wiping, we hide the bottom edge first
                startEdgeSize, endEdgeSize,
                
                // if we're showing scrollbars, (for a vertical wipe/slide)
                // - HScrollbar will get clipped, then hidden as the hide proceeds
                // - VScrollbar will get clipped/resized as the hide proceeds
                // Both need to be reset on completion (can probably just use adjustOverflow?)
                hasHScrollbar = this.hscrollOn && this.hscrollbar,
                hasVScrollbar = this.vscrollOn && this.vscrollbar;
            
            // custom logic for showing edged canvii
            if (adjustForEdge) {
                
                startEdgeSize = (info._slideOut ? (vertical ? this._edgedCanvas._topMargin
                                                            : this._edgedCanvas._leftMargin)
                                               : (vertical ? this._edgedCanvas._bottomMargin
                                                           : this._edgedCanvas._rightMargin));
                endEdgeSize = (info._slideOut ? (vertical ? this._edgedCanvas._bottomMargin
                                                          : this._edigedCanvas._rightMargin)
                                               : (vertical ? this._edgedCanvas._topMargin 
                                                           : this._edgedCanvas._leftMargin));
                
                this._edgedCanvas.resizeTo((vertical ? null : size), (vertical ? size : null), true);
                if (info._slideOut) {
                    if (vertical) this._edgedCanvas.scrollToBottom();
                    else this._edgedCanvas.scrollToRight();
                }
                        
                // For the first few px of the animation were either clipping the bottom edge,
                // or sliding the top edge out of sight, and leaving the handle visible.
                if (delta < startEdgeSize) {
                    // slide case: shrink the top margin of the handle to shift it up with the
                    // top edge
                    if (info._slideOut) {
                        var marginProp = (vertical ? "marginTop" : "marginLeft"),
                            marginSize = (vertical ? this.getTopMargin() : this.getLeftMargin())
                        this._assignSize(this.getStyleHandle(), marginProp, (marginSize- delta)); 
                    }
                    // Bail - we don't want to resize the handle at all.
                    // fire resized notification anyway - allows layouts to respond to the new size
                    this._resized();
                    return;
                }
                
                // If we're sliding out of view, ensure the top of the handle is exactly level
                // with the top of the edged canvas.
                if (info._slideOut && !this._adjustedForEdge) {
                    var marginProp = (vertical ? "marginTop" : "marginLeft"),
                        marginSize = (vertical ? this.getTopMargin() : this.getLeftMargin())

                    this._assignSize(this.getStyleHandle(), marginProp, (marginSize - startEdgeSize));
                    this._adjustedForEdge = true;
                }

                // actually hide the canvas handle (without firing any handlers) if the total
                // size is less than the height of the edge at this point.
                if (adjustForEdge && size <= endEdgeSize) {
                    // set the flag so we don't trip the 'finishAnimation()' from hide()
                    this._hidingAsAnimation = true;
                    this.getStyleHandle().visibility = isc.Canvas.HIDDEN;                
                    delete this._hidingAsAnimation;
                }
            }

            // resize the vertical scrollbar with us as we shrink
            var lengthScrollbar = vertical ? (hasVScrollbar ? this.vscrollbar : null)
                                           : (hasHScrollbar ? this.hscrollbar : null);
            if (lengthScrollbar) {       
                if (vertical) lengthScrollbar.setHeight(size);
                else {
                    // If we're showing a v-scrollbar it appears to the right of the hscrollbar
                    // On a wipe, just wait till the vscrollbar is completely hidden before
                    // resizing
                    // On a slide, take the v-scrollbar width into account when resizing
                    var sbsize = size;
                    if (this.vscrollOn) {
                        if (info._slideOut) {
                            sbsize -= this.scrollbarSize;
                        } else {
                            sbsize = Math.min(size, drawnSize - this.scrollbarSize);
                        }
                    }
                    if (sbsize > 0) lengthScrollbar.setWidth(sbsize);
                    else lengthScrollbar.hide();
                }
                
                if (info._slideOut) {
                    if (vertical) lengthScrollbar.scrollToBottom();
                    else lengthScrollbar.scrollToRight();
                }
                
                if (lengthScrollbar.thumb && lengthScrollbar.thumb.isVisible()) {

                    // On a "slideOut" we need to move the thumb then shrink it off the top
                    if (info._slideOut) {
                        var thumbStart = info._scrollThumbStart - delta,
                            start = vertical ? this.getTop() : this.getLeft();
                        if (thumbStart >= start) {
                            lengthScrollbar.thumb.moveTo(vertical ? null : thumbStart,
                                                         vertical ? thumbStart : null);
                        } else {
                            lengthScrollbar.thumb.moveTo(vertical ? null : this.getLeft(),
                                                         vertical ? this.getTop() : null);
                            var thumbLength = info._scrollThumbLength + (thumbStart - start);
                            if (thumbLength > 0) {
                                lengthScrollbar.thumb.resizeTo(vertical ? null : thumbLength, 
                                                               vertical ? thumbLength : null);
                                lengthScrollbar.thumb.scrollTo(vertical ? null : start - thumbStart,
                                                               vertical ? start - thumbStart : null);                                                               
                            } else {
                                lengthScrollbar.thumb.hide();
                            }
                        }
                    // on a wipe animation, we simply resize the thumb so it appears to clip out
                    // of view
                    } else {
                                        
                        if (vertical) {
                            var bottom = (this.getTop() + size)
                            if (lengthScrollbar.thumb.getBottom() > bottom) {
                                var thumbHeight = bottom - lengthScrollbar.thumb.getTop();
                                if (thumbHeight > 0) lengthScrollbar.thumb.setHeight(thumbHeight);
                                else lengthScrollbar.thumb.hide();
                            }
                        } else {
                            var right = (this.getLeft() + size)
                            if (lengthScrollbar.thumb.getRight() > right) {
                                var thumbWidth = right - lengthScrollbar.thumb.getLeft();
                                if (thumbWidth> 0) lengthScrollbar.thumb.setWidth(thumbWidth);
                                else lengthScrollbar.thumb.hide();
                            }                    
                        }
                    }
                }
                
            }
            
            
            // "breadth" scrollbar -- scrollbar which will be clipped across its breadth as
            // it shrinks
            var breadthScrollbar = vertical ? (hasHScrollbar ? this.hscrollbar : null)
                                                 : (hasVScrollbar ? this.vscrollbar : null),
                 // If we're showing a breadth scrollbar, the widget handle renders at the the
                 // specified size less the scrollbarsize
                 // If the scrollbar is (partially or fully) hidden we therefore need to increase  
                 // our specified size by the difference between the rendered scrollbarSize and 
                 // this.scrollbarSize to ensure the handle draws large enough
                 hiddenScrollbarDelta = 0;

            if (breadthScrollbar) {
                
                var sbSize = this.scrollbarSize;
                // if we're sliding out, it will hide at the end (just move first)
                // Otherwise it'll hide first
                if (info._slideOut) {
                    if (size >= sbSize) {
                        var offset = (vertical ? this.getTop() : this.getLeft()) + size - sbSize;
                        breadthScrollbar.moveTo(vertical ? null : offset, vertical ? offset: null);
                    } else {
                        breadthScrollbar.moveTo(vertical ? null : this.getLeft(), 
                                                vertical ? this.getTop() : null);
                        breadthScrollbar.resizeTo(vertical ? null : size, vertical ? size : null);
                        if (vertical) breadthScrollbar.scrollToBottom();
                        else breadthScrollbar.scrollToRight();
                        var thumb = breadthScrollbar.thumb
                        
                        if (thumb) {
                            thumb.resizeTo(vertical ? null : size, vertical ? size : null);
                            if (vertical) thumb.scrollToBottom();
                            else thumb.scrollToRight();
                        }
                    }
                    
                    // hide the handle (leaving just the breadth sb, and bottom of the length sb 
                    // visible) if necessary
                    if (size <= sbSize) {
                        this._hidingAsAnimation = true;
                        if (this.isVisible()) this.hide();
                        delete this._hidingAsAnimation;
                        return;
                    }
                } else {
                    if (delta <= sbSize) {
                        breadthScrollbar.resizeTo(vertical ? null : sbSize-delta,
                                                  vertical ? sbSize-delta : null);
                        if (breadthScrollbar.thumb) {
                            breadthScrollbar.thumb.resizeTo(vertical ? null : sbSize-delta,
                                                            vertical ? sbSize-delta: null);
                        }
                    } else {
                        if (breadthScrollbar.isVisible()) breadthScrollbar.hide();
                    }
                }
                
                if (breadthScrollbar.isVisible()) {
                    hiddenScrollbarDelta = this.scrollbarSize - 
                                            (vertical ? breadthScrollbar.getHeight() 
                                                      : breadthScrollbar.getWidth());
                } else {
                    hiddenScrollbarDelta = this.scrollbarSize;
                }
            }
            
            // resize the handle (actually shrink it)
            // If we're showing an edge, resize add the size of the top (or bottom) edge onto
            // the height so we don't erroneously size leave space for the edge which has 
            // already been clipped out of view
            var handleSize = size;
            if (adjustForEdge) handleSize += startEdgeSize;

            if (hiddenScrollbarDelta) handleSize += hiddenScrollbarDelta;
            
            // resize the handle
            if (!this.resizeTo((vertical ? null : handleSize), 
                                (vertical ? handleSize : null), true)) 
            {
                // if resize() didn't change the specified size, explicitly fire _resized so
                // we react to scrollbar size changes et
                this._resized();
            }
            
            var scrollAdjustment;
            if (info._slideOut) {
                this.scrollTo((vertical ? null : info._scrollStart + delta), 
                              (vertical ? info._scrollStart + delta : null), null, true);
            }

        // Ratio == 1
        } else {
            this._clearAnimationInfo("hide");
            
            if (this.isVisible()) this.hide();

            if (info._originalOverflow) this.setOverflowForAnimation(info._originalOverflow);
            if (this.showEdges && this._edgedCanvas) {
                delete this._adjustedForEdge;                
                // allow the edged canvas to show up again
                this._edgedCanvas.setOverflow(isc.Canvas.VISIBLE);
                delete this._edgedCanvas._suppressReactToResize;
                if (vertical) this._edgedCanvas.getHandle().firstChild.style.height = "100%";
                else this._edgedCanvas.getHandle().firstChild.style.width  = "100%"
                // reset the margins to float the handle inside the edges.
                if (info._slideOut) {
                    var margins = this._calculateMargins(),
                        marginProp = (vertical ? "marginTop" : "marginLeft"),
                        marginSize = (vertical ? margins.top : margins.left)
                    this._assignSize(this.getStyleHandle(), marginProp, marginSize);
                }
            }
            if (this.overflow == isc.Canvas.AUTO || this.overflow == isc.Canvas.SCROLL) {

                delete this._suppressAdjustOverflow;
                if (vertical) {
                    if (this.vscrollOn && this.vscrollbar) {
                        if (this.vscrollbar.isVisible()) this.vscrollbar.hide();
                        
                        delete this.vscrollbar._suppressSetThumb;
                        delete this.vscrollbar._suppressImageResize;
                        this.vscrollbar._showWithMaster = true;                        
                        
                        if (this.vscrollbar.thumb) {
                            delete this.vscrollbar.thumb.suppressImageResize;   
                        }
                        if (info._slideOut) {
                            this.vscrollbar.scrollTo(0,0);
                            this.vscrollbar.setHeight(this.getHeight());
                            if (this.vscrollbar.thumb) this.vscrollbar.thumb.scrollTo(0,0);
                        }
                    }
                    // catch the case where we slid the body out of view, leaving just the
                    // breadth scrollbar visible
                    if (this.hscrollOn && this.hscrollbar) {
                        if (this.hscrollbar.isVisible()) this.hscrollbar.hide();
                        this.hscrollbar._showWithMaster = true;
                        delete this.hscrollbar._suppressImageResize;
                        if (info._slideOut) this.hscrollbar.scrollTo(0,0);
                        if (this.hscrollbar.thumb) {
                            delete this.hscrollbar.thumb._suppressImageResize;
                            if (info._slideOut) this.hscrollbar.thumb.scrollTo(0,0);
                        }
                    }
                } else {
                    if (this.hscrollOn && this.hscrollbar) {
                        if (this.hscrollbar.isVisible()) this.hscrollbar.hide();                        
                        delete this.hscrollbar._suppressSetThumb;
                        delete this.hscrollbar._suppressImageResize;
                        this.hscrollbar._showWithMaster = true;
                        if (this.hscrollbar.thumb)
                            delete this.hscrollbar._suppressImageResize;
                        if (info._slideOut) {
                            this.hscrollbar.scrollTo(0,0);
                            this.hscrollbar.setWidth(this.getWidth());
                            if (this.hscrollbar.thumb) this.hscrollbar.thumb.scrollTo(0,0);
                        }
                    }
                    if (this.vscrollOn && this.vscrollbar) {
                        if (this.vscrollbar.isVisible()) this.vscrollbar.hide();
                        this.vscrollbar._showWithMaster = true;
                        delete this.vscrollbar._suppressImageResize;
                        if (info._slideOut) this.vscrollbar.scrollTo(0,0);
                        if (this.vscrollbar.thumb) {
                            if (info._slideOut) this.vscrollbar.thumb.scrollTo(0,0);
                            delete this.vscrollbar._suppressImageResize;
                        }
                    }
                }
                
                // no need to reset the size /position of the scrollbars - 
                // this will happen automatically  when adjustOverflow runs
            }

            
            // reset the size (will also resize the edge if necessary)
            this.resizeTo(info._specifiedWidth, info._specifiedHeight);
            this._userHeight = info._userHeight;
            this._userWidth = info._userWidth;
            if (info._slideOut) this.scrollTo((vertical ? null : info._scrollStart), 
                                              (vertical ? info._scrollStart : null));
            if (info._callback) {
                this._fireAnimationCompletionCallback(info._callback, earlyFinish, true);
            }
        }
    },
    
    // animateHide always falls through to this callback regardless of the effect used
    _animateHideComplete : function (earlyFinish) {        
        delete this._animatingHide;
        var callback = this._animateHideCallback;
        delete this._animateHideCallback;
        
        // Fade / fly animations don't actually hide the widget, so call hide() now if we're
        // still visible
        // Note that this IS handled during wipe / slide hide animations by fireAnimationHide()
        // with ratio 1.
        if (this.isVisible()) this.hide();
        
        // reset anything we altered during the hide       
        if (this._performingFadeHide) {
            this.setOpacity(this._fadeHideOpacity);
            delete this._fadeHideOpacity;
            delete this._performingFadeHide;
        }
        if (this._flyHideLeft != null) {
            this.setLeft(this._flyHideLeft); 
            delete this._flyHideLeft;
        }
        if (this._flyHidePercentLeft != null) {
            this._percent_left = this._flyHidePercentLeft;
            delete this._flyHidePercentLeft;
        }
        
        // Always fire the callback passed in synchronously - this is itself a callback so
        // will have already been delayed if synchronousCallback was not set on the animation
        // config object
        if (callback) {
            this._fireAnimationCompletionCallback(callback, earlyFinish, true);
        }
    },
    
        
    //> @method canvas.isAnimating()
    // Is this widget currently performing an animation?
    // @param [types] (array) Animation types to check for - if unspecified all animation types
    //   will be checked.
    // @visibility internal
    //<
    
    isAnimating : function (types) {
        if (types == null) return this._runningAnimations > 0;
        
        if (types && !isc.isAn.Array(types)) {
            // reuse an array for efficiency
            if (!this._animatingTypesArray) this._animatingTypesArray = [];
            this._animatingTypesArray[0] = types;
            types = this._animatingTypesArray;
        }
        
        if (!types) types = this._animations;
        for (var i = 0; i < types.length; i++) {
            if (this[this._getAnimationID(types[i])] != null) {
//                this.logWarn("ID:" + this._getAnimationID(types[i]) +
//                    "set to:"  + this[this._getAnimationID(types[i])]);
                return true;
            }
        }
        return false;
    }
    
});
