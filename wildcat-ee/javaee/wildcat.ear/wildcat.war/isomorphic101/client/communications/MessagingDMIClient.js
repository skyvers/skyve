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
//------------------------------------------------------------------------------------
// when you call MessagingDMI.connect() - you get one of these in the callback
isc.defineClass("MessagingDMIClient").addProperties({

socketConstructor: "MessagingDMISocket",

// this has an async interface so we can e.g. call the server for a truly unique GUID
getGUID : function (callback) {
    if (!this.GUID) this.GUID = isc.Math.randomUUID();
    this.fireCallback(callback, "GUID", [this.GUID]);
},


call : function (methodName, args, callback) {
    //!OBFUSCATEOK
    var _this = this;

    var props = {};
    if (isc.isAn.Object(methodName)) {
        // take a shallow copy because we will remove properties meant for the
        // MessagingDMISocket class later
        props = isc.addProperties({}, methodName); 
        methodName = props.methodName;
        args = props.args;
        callback = props.callback;
    }


    if (!args) args = [];
    if (!isc.isAn.Array(args)) {
        this.logError("Expected Array as second arg or props.args in "
         + "MessagingDMIClient.call(), but got:" + isc.echo(args));
        return;
    }

    // wrap the callback so we can supply the "retVal" arg
    var wrappedCallback = null;
    if (callback) {
        wrappedCallback = function (retVal) {
            _this.fireCallback(callback, "retVal", [retVal]);
        };
    }

    // canonicalize to props format
    isc.addProperties(props, {
        methodName: methodName,
        args: args
    });
    
    // remove property overrides meant for MessagingDMISocket
    var packetProperties = props.packetProperties;
    var requestProperties = props.requestProperties;
    delete props.packetProperties;
    delete props.requestProperties;

    // can override sendChannel - notify(), for example, does this
    var sendChannel = props.sendChannel || this.sendChannel;
    delete props.sendChannel;

    if (!sendChannel) {
        this.logError("Error in call() - unable to resolve sendChannel"+this.getStackTrace());
        return;
    }

    // if no socket has been provided, auto-connect one now
    if (!this.socket) this.connect(sendChannel);

    this.socket.send(sendChannel, props, wrappedCallback, packetProperties, requestProperties);
},

// target is either:
//   - a channel name (string) - typically the GUID or broadcast channel of a server - this
//     becomes our sendChannel
//   - a serverConfig properties block - we then derive our sendChannel from the receiveChannel
//     specified on this properties block
//   - a MessagingDMIServer instance - providing one of these triggers special short-circuit
//     logic that simply calls APIs on the provided instance without transiting the network.
//     This mode is useful for testing and is used by the Developer Console window-to-window
//     comm
connect : function (target, callback) {
    var _this = this;

    // Note: below we store the serverProperties on this object for possible future use

    // direct binding - store and return 
    // Note: don't use isA checks cross frame - that may fail on IE
    if (target.getServerProperties) {
        this.directBinding = target;
        target = target.getServerProperties();
    }

    if (isc.isA.String(target)) {
        // channel directly provided
        this.serverProperties = {
            GUID: target,
            receiveChannel: target
        };
        this.sendChannel = this.serverProperties.receiveChannel;
    } else if (target.receiveChannel) {
        // serverProperties block from MessagingDMIServer.getServerProperties() provided
        this.serverProperties = target;
        this.sendChannel = this.serverProperties.receiveChannel;
    }

    if (this.socket) this.socket.close();

    
    if (!this.receiveChannel) {
        // use GUID for receiveChannel - if not set, generate and call ourselves back
        this.getGUID(function (GUID) {
            _this.receiveChannel = GUID;
            _this.connect(target, callback);
        });
        return;
    }

    // socket will auto-connect when we send() on it
    this.socket = isc.ClassFactory.getClass(this.socketConstructor, true).create({
        receiveChannel: this.receiveChannel,
        directBinding: this.directBinding
    }, this.socketDefaults, this.socketProperties);

    // if a callback has been provided, round trip a message to the server before calling back
    if (callback) this.call("connect", null, callback);
},    

disconnect : function (callback) {
    var _this = this;
    
    if (!this.socket) {
        if (callback) this.fireCallback(callback);
        return;
    }
    
    var closeSocket = function (callback) {
       _this.socket.close(function () {
            delete _this.socket;       
            delete _this.directBinding;
            if (callback) _this.fireCallback(callback);
       });    
    }

    if (callback) {
        // disconnect() call first goes to server
        this.call("disconnect", [], function () {
            closeSocket(callback);
        });
    } else {
        // just close the socket
        closeSocket();
    }
},

// convenience method
ping : function (callback, timeout) {
    var requestProperties = timeout ? {timeout: timeout} : null;
    this.call({
        methodName: "ping",
        callback: callback,
        requestProperties: requestProperties
    });
}

});
