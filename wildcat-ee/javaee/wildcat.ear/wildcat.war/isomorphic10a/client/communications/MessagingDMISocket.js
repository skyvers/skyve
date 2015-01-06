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

//------------------------------------------------------------------------------------
// full duplex RPC over Messaging
// 
// MessagingDMISocket binds a Messaging channel to receive responses and pairs requests sent 
// through it with responses to those requests - exposing a simple send(payload, callback)
// mechanism
isc.defineClass("MessagingDMISocket").addProperties({

// This is the channel we register with Messaging for receiveing realtime push updates from the
// server. 
receiveChannel: null,

// how long we wait for a reply before deleting the outstandingRequest
defaultRequestTimeout: 300000,  // 5 min
// how often the request reaper timer runs
requestReaperInterval: 5000,

// unique identifier for the request we send - we use these to pair responses with requests
sequence: 0,

// requests waiting to be sent pending Messaging channel subscription
pendingRequests: null,

// outstanding requests for which we are waiting on a response to our private channel
// XXX: how do we handle failure to respond?  implement timeouts?
outstandingRequests: null,


init : function () {
    this.Super("init");

    // Note: must init here so we definitely have an instance copy...sigh
    this.pendingRequests = [];
    this.outstandingRequests = [];

    // this is for direct binding, but create regardless because a receiving server socket can
    // support both remote and direct binding modes concurrently - but needs an ID to work with
    // direct binding
    this.ns.ClassFactory.addGlobalID(this);
    // likewise for the queue
    this.directBindingQueue = [];

    // force directBindingOnly if we are passed a directBinding target
    if (this.directBinding) this.directBindingOnly = true;

},

bind : function (callback) {
    var _this = this;
    this.logDebug("bind");

    if (!this.receiveChannel) {
        this.logError("Unable to bind null receiveChannel");
        return;
    }

    if (this.directBindingOnly) {
        this._bound = true;
        if (callback) this.fireCallback(callback);
        return;
    }

    if (!isc.Messaging) {
        this.logError("Messaging not available - unable to bind() socket");
        return;
    }


    
    isc.Messaging.isRemoteDebug = this.isRemoteDebug;
    isc.Messaging.subscribe(
        this.receiveChannel,
        function (data) {
           _this.receive(data);
        }, 
        function () {
            _this._bound = true;
            if (callback) _this.fireCallback(callback);
        }
    );
    delete isc.Messaging.isRemoteDebug;
},
close : function (callback) {
    if (!this.directBindingOnly) isc.Messaging.unsubscribe(this.receiveChannel);

    delete this._bound;
    this._clearReaperTimer();
    if (callback) this.fireCallback(callback);
},

_clearReaperTimer : function () {
    isc.Timer.clear(this._requestReaperTimer);
    delete this._requestReaperTimer;
},
_setReaperTimer : function (timeout) {
    this._clearReaperTimer();
    var _this = this;
    this._requestReaperTimer = isc.Timer.setTimeout(function () {
        _this._reapTimedOutRequests();
    }, timeout);
    this._nextReap = isc.timeStamp()+timeout;
},
_reapTimedOutRequests : function () {
    var now = isc.timeStamp();
    var timeToNextReap = -1;
    for (var i = 0; i < this.outstandingRequests.length;) {
        var request = this.outstandingRequests[i];
        if (now >= request.expiresAt) {
            this.outstandingRequests.removeAt(i);
            if (this.logIsDebugEnabled()) {
                this.logDebug("Reaped request: " + isc.echo(request));
            }
        } else {
            i++;
            // schedule nextReap for the next expiring request
            if (timeToNextReap == -1) timeToNextReap = request.timeout;
            else if (request.timeout < timeToNextReap) timeToNextReap = request.timeout;
        }
    }
    if (timeToNextReap != -1) this._setReaperTimer(timeToNextReap);
},

getNextSequence : function () {
    this.sequence += 1;
    return this.sequence;
},
send : function (sendChannel, payload, callback, packetProperties, requestProperties) {
    // a packet is what we actually send to the server - it contains the payload and additional
    // properties that allow the remote endpoint to properly respond
    var packet = isc.addProperties({
        payload: payload,
        sequence: this.getNextSequence(),
        originChannel: this.receiveChannel,
        expectsReply : callback != null
    }, packetProperties);

    // a request wraps the packet and any additional metadata we need in order to return the
    // response to the caller (e.g. callback)
    var request = isc.addProperties({
        sendChannel: sendChannel,
        packet: packet,
        callback: callback,
        // copy sequence from packet for ease of searching later
        sequence: packet.sequence
    }, requestProperties);

    // if channel doesn't exist, create it
    var _this = this;
    if (!this._bound) {
        // queue the request and establish receipt channel
        this.pendingRequests.add(request);
        this.bind(function () {
            // push all pending requests down the wire
            while(_this.pendingRequests.length) _this._send(_this.pendingRequests.shift());                        
        });
        return;
    }
    
    // we are bound - proceed
    this._send(request);
},
_send : function (request) {
    if (this.logIsDebugEnabled()) {
        this.logDebug("_send: "+isc.echo(request));
    }

    // if we expect a response, keep track of the request so we can pair up the response
    if (request.callback) {
        this.outstandingRequests.add(request);

        var now = isc.timeStamp();
        request.sendTime = now;

        if (request.timeout == null) request.timeout = this.defaultRequestTimeout;
        request.expiresAt = now+request.timeout;
        if (this._nextReap == null) {
            // schedule the reaper
            this._setReaperTimer(request.timeout);
        } else {
            // if the outbound request is scheduled to expire sooner than we would otherwise run
            // the reaper, pull the reaper forward to match request timeout
            if (this._nextReap > request.expiresAt) this._setReaperTimer(request.timeout);
        }
    }

    if (this.directBinding) {
        var directBindingSocket = this.directBinding.getSocket(request.sendChannel);
        if (!directBindingSocket) {
            this.logWarn("Unable to obtain direct binding socket for channel: "+request.sendChannel);
            return;
        }

        try {
            // targetWindow may have gone away - don't produce copious error messages if so
            this.directBinding.window.isc;
        } catch (e) {
            return;
        }


        this.sendDirectBindingPacket(request.packet, directBindingSocket, this.directBinding.window);        
    } else {
        
        isc.Messaging.isRemoteDebug = this.isRemoteDebug;
        isc.Messaging.send(request.sendChannel, request.packet, null, {doNotTrackRPC: this.doNotTrackRPC});        
        delete isc.Messaging.isRemoteDebug;
    }
},

sendDirectBindingPacket : function (packet, directBindingSocket, targetWindow) {
    
    directBindingSocket.directBindingQueue.add({
        serializedPacket: isc.Comm.serialize(packet),
        originSocket: this,
        originWindow: window
    });
    try {       
        var sockID = directBindingSocket.ID;
        targetWindow.setTimeout("if (window."+sockID+") "+sockID+".processDirectBindingQueue()", 0);
    } catch (e) {
        // window may have gone away - ignore error
    }
},
processDirectBindingQueue : function () {
    while (this.directBindingQueue.length) {
        var obj = this.directBindingQueue.shift();

        try {
            var packet = isc._makeFunction("return " + obj.serializedPacket)();
            this.delayCall("receive", [packet, obj.originSocket, obj.originWindow]);
        } catch (e) {
            // ignore any error
        }
    }
},

sendReply : function (requestPacket, replyPayload, originSocket, originWindow) {
          
    if (!requestPacket.expectsReply) return;    

    var replyPacket = {
        payload: replyPayload,
        originChannel: this.receiveChannel,
        sequence: this.getNextSequence(),
        inResponseTo: {
            originChannel: requestPacket.originChannel,
            sequence: requestPacket.sequence
        }
    }

    if (this.logIsDebugEnabled()) {
        this.logDebug("sendReply to :" +requestPacket.originChannel+": "+isc.echo(replyPacket));
    }

    if (originSocket) {
        // direct binding

        try {
            // targetWindow may have gone away - don't produce copious error messages if so
            originWindow.isc;
        } catch (e) {
            return;
        }

        this.sendDirectBindingPacket(replyPacket, originSocket, originWindow);
    } else {
        
        isc.Messaging.isRemoteDebug = this.isRemoteDebug;
        isc.Messaging.send(requestPacket.originChannel, replyPacket, null, {doNotTrackRPC: this.doNotTrackRPC});
        delete isc.Messaging.isRemoteDebug;
    }
},

// this fires for non-reply packets
packetReceived : function (packet, originSocket, originWindow) {

},

receive : function (data, originSocket, originWindow) {
    // this could be a reply to one of our messages or not - 
    var packet = data;

    if (packet.inResponseTo) {
        // this is a reply
        var request;
        var request = this.outstandingRequests.find("sequence", packet.inResponseTo.sequence);
        if (!request) {
            if (this.logIsWarnEnabled()) {
                this.logWarn("Unable to find originating request for response packet: "+isc.echo(packet));
                this.logWarn("Outstanding requests: " + isc.echo(this.outstandingRequests));
            }
            return;
        }   
        if (this.logIsDebugEnabled()) {
            this.logDebug("original request: " + isc.echo(request));
            this.logDebug("callback args "+isc.echoFull([packet.payload]));
        }
        if (request.callback) this.fireCallback(request.callback, "payload", [packet.payload]);
    } else {
        // this is not a reply                
        this.packetReceived(packet, originSocket, originWindow);
    }
}

});




