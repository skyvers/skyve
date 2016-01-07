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
// This file encapsulates SmartClient's HTML5 (and fallbacks) offline support.  For ease of 
// porting to pure GWT, isc.Offline is created as a plain JS object with no dependence on 
// the SmartClient class system



(function () {

//> @class Offline
// The Offline class provides features for storing and retrieving values in persistent browser
// storage.  Because this storage is both persistent (ie, it remains available after the 
// browser is closed and re-opened) and local to the browser, it allows applications to operate
// when the client is not connected to a network, albeit with some obvious limitations.
// <p>
// As well as providing straightforward APIs for storing, retrieving and removing values, 
// Offline support is integrated into the normal request/response cycle.  You can configure 
// the framework so that server responses are automatically cached in Offline storage.  Then,
// at some future point when the client is offline, responses for known requests are returned 
// from the Offline cache.
// <p>
// SmartClient makes use of various underlying storage techniques, depending on what the 
// browser supports, but the API to a SmartClient program is the same regardless of the 
// underlying storage - simple key/value pairs.  Currently, we provide Offline support for all 
// HTML5-compliant browsers, plus earlier versions of Internet Explorer (6 and 7).  The amount 
// of storage available is dictated by the browser, and varies from approximately 500KB to 
// approximately 5MB.
//
// @example offlineSupport
// @example offlinePrefs
// @treeLocation Client Reference/Data Binding
// @group offlineGroup
// @visibility external
//<

var Offline = {

    // Offline detection and manual override
    // ---------------------------------------------------------------------------------------
	explicitOffline: null,

	//>	@classMethod	Offline.isOffline()	
	// Returns true if the current browser session is offline (ie, not connected to a network).
	// If an online/offline state has been set explicitly (see +link{Offline.goOffline()} and 
	// +link{Offline.goOnline}), the explicitly-set state will be returned.  Otherwise, the 
	// offline state as reported by the browser will be returned.  See 
    // +link{useNativeOfflineDetection,useNativeOfflineDetection} for important notes on 
    // browser detection of offline state.
	//
    // @return (Boolean) true if the current browser session is offline
	// @see Offline.goOffline() 
	// @see Offline.goOnline() 
	// @see Offline.useNativeOfflineDetection()
	// @visibility external
	//<
    isOffline : function () {
		// If the explicit offline flag is set (ie, not null), use it in preference to whatever
		// the browser reports
		if (this.explicitOffline !== null) return this.explicitOffline;
        // Working via a local var so it can be flipped in the debugger for ease of testing
        var offline = window.navigator.onLine ? false : true;
        return offline;
    },
	
	//>	@classMethod	Offline.goOffline()	
	// Explicitly sets this session into offline mode.  This setting will override whatever 
	// state the browser reports.  This allows users to manually set an application into 
	// offline or online state.
	// 
	// @see Offline.goOnline() 
	// @see Offline.useNativeOfflineDetection()
	// @visibility external
	//<
	goOffline : function () {
		this.explicitOffline = true;
	},
	
	//>	@classMethod	Offline.goOnline()	
	// Explicitly sets this session into online mode.  This setting will override whatever 
	// state the browser reports.  This allows users to manually set an application into 
	// offline or online state.
	// 
	// @see Offline.goOffline() 
	// @see Offline.useNativeOfflineDetection()
	// @visibility external
	//<
	goOnline : function () {
		this.explicitOffline = false;
	},
	
	//>	@classMethod	Offline.useNativeOfflineDetection()	
	// Tells the Offline system to query the browser for the current online/offline state.
	// Calling this method switches off the explicit offline mode setting switched on by 
	// calling +link{Offline.goOnline()} or +link{Offline.goOffline()}.
    // <p>
    // It is important to note that browsers vary quite considerably in their ability to 
    // detect that they are offline.  Many older browsers simply can't do it; HTML5 browsers
    // expose the <code>navigator.onLine</code> property, but each browser's implementation 
    // is different.  Some browsers have a manual "Work Offline" mode which allows the user 
    // to make the decision, and SmartClient provides an equivalent mechanism with the 
    // <code>goOffline</code> and <code>goOnline</code> methods.  Generally speaking, these 
    // methods are more reliable than allowing the browser to decide whether your application
    // is offline.
	// 
	// @see Offline.goOnline() 
	// @see Offline.goOffline()
	// @visibility external
	//<
	useNativeOfflineDetection : function () {
		this.explicitOffline = null;
	},
    
    // Overall cache managment
    // ---------------------------------------------------------------------------------------
    KEY_PREFIX: "isc-",
    
    LOCAL_STORAGE: "localStorage",                // HTML5 browsers
    GLOBAL_STORAGE: "globalStorage",              // Older versions of Firefox
    DATABASE_STORAGE: "databaseStorage",          // Safari 3.1 & 3.2 (HTML5 browsers as well,
                                                  // but these older Safari versions have no 
                                                  // localStorage support)
    GEARS_DATABASE_API: "gears database api",     // Built into pre-4 versions of Chrome
    USERDATA_PERSISTENCE: "userData persistence", // IE5 and later (IE8+ also support localStorage)
    GOOGLE_GEARS: "google gears",                 // An explicitly installed Gears instance
    NO_MECHANISM: "no discovered mechanism",      // We won't get this very often!
    
    maxResponsesToPersist: 100,
    
    
    userDataPersistenceInIE8: false,
    
    localStorageType : function () {
        if (window.localStorage) {
            if (!this.userDataPersistenceInIE8 || !isc.Browser.isIE) {
                return this.LOCAL_STORAGE;
            }
        }
        if (window.globalStorage) return this.GLOBAL_STORAGE;
        if (window.openDatabase) return this.DATABASE_STORAGE;
        if (isc.Browser.isIE && isc.Browser.version >= 5) {
            if (!UserDataPersistence.isInitialized) UserDataPersistence.init();
            return this.USERDATA_PERSISTENCE;
        }
        // Not implementing anything else just yet
        return this.NO_MECHANISM;
    },

    // The "native" entry count includes metadata that we store for management and metrics
    getNativeStoredValuesCount : function () {
        switch (this.localStorageType()) {
            case this.LOCAL_STORAGE:
                return localStorage.length;
                break;
            case this.USERDATA_PERSISTENCE:
                return UserDataPersistence.getNativeStoredValuesCount();
                break;
            case this.GLOBAL_STORAGE:
            case this.DATABASE_STORAGE:
            case this.GEARS_DATABASE_API:
            case this.GOOGLE_GEARS:
                break;
        }
    },

    // The "SC" entry count is just data entries, gleaned from our own metrics
    getSCStoredValuesCount : function () {
        var entries = this.get(this.countKey);
        entries = entries ? entries * 1 : 0;
        return entries;
    },

    // The "native index" is the index into the underlying browser storage; it is not a stable 
    // or predictable value, and thus not a particularly useful one, unless you just want to 
    // do an unordered walk of the cache entries.  If you want to iterate over the stored 
    // values in a stable, guaranteed sequence, use getPriorityQueueKey/Value/Entry.  This 
    // method was only provided to give autoTests a way of verifying that priority queue 
    // contents are an exact match for actual stored keys
    getKeyForNativeIndex : function (index) {
        switch (this.localStorageType()) {
            case this.LOCAL_STORAGE:
                return localStorage.key(index);
            case this.USERDATA_PERSISTENCE:
                return UserDataPersistence.getKeyForNativeIndex(index);
                break;
            case this.GLOBAL_STORAGE:
            case this.DATABASE_STORAGE:
            case this.GEARS_DATABASE_API:
            case this.GOOGLE_GEARS:
                break;
        }
    },

    clearCache : function () {
        var count = this.getSCStoredValuesCount();
        while (this.getSCStoredValuesCount() > 0) {
            this.removeOldestEntry();
            // This just prevents an eternal loop if for whatever reason running 
            // removeOldestEntry() does not result in a different count 
            if (this.getSCStoredValuesCount() == count) break;
            count = this.getSCStoredValuesCount();
        }
    },

    
    clearCacheNatively : function () {
        // userData is nearly always a special case...
        if (this.localStorageType() == this.USERDATA_PERSISTENCE) {
            UserDataPersistence.clearCacheNatively();
            return;
        }
        var count = this.getNativeStoredValuesCount();
        this.logDebug("Removing all " + count + " entries from local storage");
        for (var i = 0; i < count; i++) {
            this._remove(this.getKeyForNativeIndex(0), false);
        }
    },
    
    logCacheContents : function (maxEntryLength) {
        var contents = this.getCacheContents();
        this.logDebug("Dumping the contents of the browser's local storage:");
        for (var key in contents) {
            var value = contents[key];
            if (value && value.length > maxEntryLength) {
                value = value.substring(0, maxEntryLength);
            }
            this.logDebug(key + " ===> " + value);
        }
    },
    
    getCacheContents : function () {
        var index = 0,
            contents = {},
            count = this.getSCStoredValuesCount();
        while (index < count) {
            var key = this.getPriorityQueueKey(index);
            // Since we now work via the managed priority queue, we will never get back 
            // anything we disn't insert, so this test will always be true
            if (key.indexOf(this.KEY_PREFIX) == 0) key = key.substring(this.KEY_PREFIX.length);
            contents[key] = this.get(key);
            index++;
        }
        return contents;
    },

    getCacheKeys : function () {
        var index = 0,
            keys = [],
            count = this.getSCStoredValuesCount();
        while (index < count) {
            var key = this.getPriorityQueueKey(index);
            keys[keys.length] = key;
            index++;
        }
        return keys;
    },

    removeOldestEntry : function () {
        var key = this.getAndRemoveOldestFromPriorityQueue();
        if (key == null) return;
        this.remove(key, true);
    },
    
    // Log the raw contents of userData storage - only intended for internal debug use
    _logUserDataContents : function (maxEntryLength) {
        var doc = this.userDataSpan.xmlDocument;
        var maxEntryLength = 100;
        this.logDebug("Dumping the contents of raw userData storage:");
        for (var i = 0; i < this.userDataSpan.xmlDocument.firstChild.attributes.length; i++) {
            var key = this.userDataSpan.xmlDocument.firstChild.attributes[i].name;
            var value = this.userDataSpan.getAttribute(key);
            if (value && value.length > maxEntryLength) {
                value = value.substring(0, maxEntryLength) + "...";
            }
            this.logDebug(key + " ===> " + value);
        }
    },

    // Managing entries
    // ---------------------------------------------------------------------------------------
	//>	@classMethod	Offline.put()	
	// Stores the passed-in value in browser-local storage, mapped to the passed-in key.  If
	// there is no room left to store the value, we discard values from the offline store, 
	// oldest first, until there is room to store the value.  If you don't want this behavior,
    // explicitly pass false in the <code>recycleEntries</code> parameter.
    // <p>
    // Note that limitations in the underlying storage engines mean that only primitive 
    // values - Strings, numbers and booleans - can be stored.  If you wish to store an Array
    // or Object, you will have to serialize it to JSON first, and then <code>eval</code> it 
    // after retrieval to turn it back into an object.
    // <p>
    // <b>Note:</b> This method throws an exception if it could not store the value (either 
    // because storage is full and recycleEntries was false, or because the value to store is
    // simply too large)
	// 
    // @param key              (String)  The key to use when storing the value
    // @param value            (any)     The value to store
    // @param [recycleEntries] (boolean) If false, suppresses the default behavior of repeatedly
    //                                   discarding the oldest entry if there is insufficient
    //                                   space to store the value
	// @see Offline.get() 
	// @visibility external
	//<
    put : function (key, value, recycleEntries) {
        var ts = new Date().getTime();
        var oldValue = this.get(key);
        while (true) {
            try {
                this._put(key, value);
                break;
            } catch (e) {
                if (recycleEntries !== false && this.isStorageException(e)) {
                    var entries = this.getStorageMetrics().storedEntries;
                    if (entries > 0) {
                        this.logDebug("Cache full; removing oldest entry and trying again");
                        this.removeOldestEntry();
                    } else { 
                        this.logDebug("Can't add this entry to browser-local storage, even " +
                                     "though the cache is empty - the item must be larger " +
                                     "than the browser's permitted cache space.");
                         break;
                    }
                } else {
                    throw e;
                }
            }
        }
        var end = new Date().getTime();
        var pqOK = false, metricsOK = false;
        while (!pqOK || !metricsOK) {
            try {
                if (!pqOK) this.addToPriorityQueue(key);
                pqOK = true;
                if (!metricsOK) this.updateMetrics("put", key, value, oldValue);
                metricsOK = true;
            } catch(e) {
                if (this.isStorageException(e)) {
                    if (recycleEntries !== false) {
                        var entries = this.getStorageMetrics().storedEntries;
                        if (entries > 0) {
                            this.logDebug("Cache full when updating priority queue or metrics; " + 
                                         "removing oldest entry and trying again");
                            this.removeOldestEntry();
                            continue;
                        }
                    }
                    this.logDebug("Cache full when updating priority queue or metrics; rolling " + 
                                 "back the entire update");
                    this._remove(key);
                    if (pqOK) this.removeFromPriorityQueue(key);
                    this.rebuildMetrics();
                    throw e;
                } else {
                    throw e;
                }
            }
        }
        this.logDebug("put() with key: " + key + "\nitem: " + this.echoLeaf(value) + ": " + 
                        (end - ts) + "ms. Maintaining the priority queue and metrics took " +
                        "a further " + new Date().getTime() - end + "ms");
    },
    
    _put : function (key, value, applyPrefix) {
        key = (applyPrefix === false ? "" : this.KEY_PREFIX) + key;
        switch (this.localStorageType()) {
            case this.LOCAL_STORAGE:
                localStorage.setItem(key, value);
                break;
            case this.USERDATA_PERSISTENCE:
                UserDataPersistence.putValue(key, value);
                break;
            case this.GLOBAL_STORAGE:
            //    globalStorage[''][key] = value;
            //    break;
            case this.DATABASE_STORAGE:
            //    this.database.executeSql(
            //        "INSERT INTO ResponseCache (key, value) " + 
            //        "('" + key + "', '" + value + "')");
            //    break;
            case this.GEARS_DATABASE_API:
            case this.GOOGLE_GEARS:
                this.logError("Persistence method '" + this.localStorageType() + "' not yet supported");
                break;
        }
    },
	
	isStorageException : function (e) {
        switch (this.localStorageType()) {
            case this.LOCAL_STORAGE:
                if (isc.Browser.isIE) {
                    return (e.number == -2147024882);
                } else if (isc.Browser.isMoz) {
                    return (e.name == "NS_ERROR_DOM_QUOTA_REACHED");
                } else {
                    return (e.name == "QUOTA_EXCEEDED_ERR");
                }
                break;
            case this.USERDATA_PERSISTENCE:
                return (e.number == -2147024857)
        }
	},
    
	//>	@classMethod	Offline.get()	
	// Returns the value mapped to the passed-in key from browser-local storage, or null if 
	// no such key exists.
	// 
    // @param key              (String)  The key to retrieve a value for
    // @return (any) The value mapped to the passed in key, or null if no such key exists
	// @see Offline.put() 
	// @visibility external
	//<
    get : function (key) {
        var ts = new Date().getTime(),
            item;
        switch (this.localStorageType()) {
            case this.LOCAL_STORAGE:
                item = localStorage.getItem(this.KEY_PREFIX + key);
                break;
            case this.USERDATA_PERSISTENCE:
                item = UserDataPersistence.getValue(this.KEY_PREFIX + key);
                break;
            case this.GLOBAL_STORAGE:
            //    return globalStorage[''][key];
            case this.DATABASE_STORAGE:
                // Not sure what's involved yet
            case this.GEARS_DATABASE_API:
            case this.GOOGLE_GEARS:
                break;
        }
        
        if (item) item = isc.clone(item);
        
        var end = new Date().getTime();
        this.logDebug("get() with key: " + key + 
                     "\nitem is: " + this.echoLeaf(item) +
                     ": " + (end - ts) + "ms");
        return item;
    },
    
    
	//>	@classMethod	Offline.remove()	
	// Removes the key/value pair mapped by the passed-in key from browser-local storage
	// 
    // @param key              (String)  The key to remove
	// @see Offline.put() 
	// @see Offline.get() 
	// @visibility external
	//<
    remove : function (key, skipPriorityQueueUpdate) {
        this.logDebug("Removing item for key: " + key);
        this.updateMetrics("remove", key);
        if (!skipPriorityQueueUpdate) this.removeFromPriorityQueue(key);
        this._remove(key);
    },
    
    _remove : function (key, applyPrefix) {
        key = (applyPrefix === false ? "" : this.KEY_PREFIX) + key;
        switch (this.localStorageType()) {
            case this.LOCAL_STORAGE:
                localStorage.removeItem(key);
                break;
            case this.USERDATA_PERSISTENCE:
                UserDataPersistence.removeValue(key);
                break;
            case this.GLOBAL_STORAGE:
            //    return globalStorage[''][key];
            case this.DATABASE_STORAGE:
                // Not sure what's involved yet
            case this.GEARS_DATABASE_API:
            case this.GOOGLE_GEARS:
                break;
        }
    },
    
    getUndecoratedKey : function (key) {
        if (key && key.startsWith(this.KEY_PREFIX)) {
            key = key.substring(this.KEY_PREFIX.length);
        }
        return key
    },

    
    // ---------------------------------------------------------------------------------------

    priorityQueueKey: "pq",  // this will actually have "isc-" prepended to it
    addToPriorityQueue : function (userKey) {
        //!DONTOBFUSCATE
        this.removeFromPriorityQueue(userKey);
        var key = this.toInternalKey(userKey);
        var pqText = this.get(this.priorityQueueKey);
        if (pqText) {
            eval("var pq = " + pqText);
        } else {
            var pq = [];
        }
        pq.push(key);
        this._put(this.priorityQueueKey, this.serialize(pq));
    },
    removeFromPriorityQueue : function (userKey) {
        //!DONTOBFUSCATE
        var key = this.toInternalKey(userKey);
        var pqText = this.get(this.priorityQueueKey);
        if (pqText) {
            eval("var pq = " + pqText);
        } else {
            var pq = [];
        }
        for (var i = 0; i < pq.length; i++) {
            if (pq[i] == key) {
                var leading = pq.slice(0, i);
                var trailing = pq.slice(i + 1);
                pq = leading.concat(trailing);
                break;
            }
        }
        this._put(this.priorityQueueKey, this.serialize(pq));
    },
    getAndRemoveOldestFromPriorityQueue : function () {
        //!DONTOBFUSCATE
        var pqText = this.get(this.priorityQueueKey);
        if (pqText) {
            eval("var pq = " + pqText);
        } else {
            var pq = [];
        }
        var oldest = pq.shift();
        this._put(this.priorityQueueKey, this.serialize(pq));
        return this.toUserKey(oldest);
    },
    getPriorityQueueEntry : function (index) {
        var key = this.getPriorityQueueKey(index);
        var value = this.get(key);
        var entry = {};
        entry[key] = value;
        return entry;
    },
    getPriorityQueueValue : function (index) {
        var key = this.getPriorityQueueKey(index);
        return this.get(key);
    },
    getPriorityQueueKey : function (index) {
        //!DONTOBFUSCATE
        var pqText = this.get(this.priorityQueueKey);
        if (pqText) {
            eval("var pq = " + pqText);
        } else {
            var pq = [];
        }
        return this.toUserKey(pq[index]);
    },
    getPriorityQueue : function () {
        //!DONTOBFUSCATE
        var pqText = this.get(this.priorityQueueKey);
        if (pqText) {
            eval("var pq = " + pqText);
        } else {
            var pq = [];
        }
        return pq;
    },
    
    toInternalKey : function (userKey) {
        if (this.localStorageType() == this.USERDATA_PERSISTENCE) {
            return UserDataPersistence.getDataStoreKey(this.KEY_PREFIX + userKey);
        }
        // Otherwise, we don't mess about with converting keys
        return userKey;
    },
    
    toUserKey : function (internalKey) {
        if (this.localStorageType() == this.USERDATA_PERSISTENCE) {
            return this.getUndecoratedKey(UserDataPersistence.getUserKey(internalKey));
        }
        // Otherwise, we don't mess about with converting keys
        return internalKey;
    },
    
    // Storage metrics
    // ---------------------------------------------------------------------------------------
    countKey: "storedEntryCount__",
    keyKey: "storedKeyBytes__",
    valueKey: "storedValueBytes__",
    
    updateMetrics : function (mode, key, value, oldValue) {
        var realKey = this.KEY_PREFIX + key,
            storedEntries = this.get(this.countKey) || 0,
            storedKeyBytes = this.get(this.keyKey) || 0,
            storedValueBytes = this.get(this.valueKey) || 0;
            storedKeyBytes = 1 * storedKeyBytes;
            storedValueBytes = 1 * storedValueBytes;
            if (mode == "remove") {
                var item = this.get(key);
                if (item != null) {
                    storedEntries--;
                    storedKeyBytes -= realKey.length;
                    storedValueBytes -= item.length;
                }
            } else {
                if (oldValue == null) {
                    storedEntries++;
                    storedKeyBytes += realKey.length;
                    storedValueBytes += value.length;
                } else {
                    storedValueBytes += value.length - oldValue.length;
                }
            }
            this._put(this.countKey, storedEntries);
            this._put(this.keyKey, storedKeyBytes);
            this._put(this.valueKey, storedValueBytes);
    },

    rebuildMetrics : function () {
        var pq = this.getPriorityQueue(),
            entries = 0, keyBytes = 0, valueBytes = 0;
        for (var i = 0; i < pq.length; i++) {
            var value = this.get(pq[i]);
            entries++;
            keyBytes += pq[i].length;
            valueBytes += value.length;
        }
        this._put(this.countKey, entries);
        this._put(this.keyKey, keyBytes);
        this._put(this.valueKey, valueBytes);
    },
    
    getStorageMetrics : function () {
        var storedEntries = this.get(this.countKey) || 0,
            storedKeyBytes = this.get(this.keyKey) || 0,
            storedValueBytes = this.get(this.valueKey) || 0,
            countLen = 0,
            keyLen = 0,
            valueLen = 0;
        if (storedEntries) countLen = storedEntries.length;
        if (storedKeyBytes) keyLen = storedKeyBytes.length;
        if (storedValueBytes) valueLen = storedValueBytes.length;
        storedEntries = 1 * storedEntries;
        storedKeyBytes = 1 * storedKeyBytes;
        storedValueBytes = 1 * storedValueBytes;
        var pqText = this.get(this.priorityQueueKey);
        var overhead = this.countKey.length + this.keyKey.length + this.valueKey.length +
                             countLen + keyLen + valueLen;
        var pqLength = pqText == null ? 0 : pqText.length + 
                                (this.KEY_PREFIX + this.priorityQueueKey).length;
        return {
            storedEntries: storedEntries,
            storedKeyBytes: storedKeyBytes,
            storedValueBytes: storedValueBytes,
            metricsOverhead: overhead,
            priorityQueue: pqLength,
            total: storedKeyBytes + storedValueBytes + overhead + pqLength
        }
    },
    
    getTotalStorageUsed : function () {
        var metrics = this.getStorageMetrics();
        return metrics.storedKeyBytes + 
               metrics.storedValueBytes + 
               metrics.metricsOverhead +
               metrics.priorityQueue;
    },

    // DataSource functionality
    // ---------------------------------------------------------------------------------------
    storeResponse : function (dsRequest, dsResponse) {
        var ts = new Date().getTime();
        dsResponse.offlineTimestamp = ts;

        var trimmedRequest = this.trimRequest(dsRequest),
            key = this.serialize(trimmedRequest),
            value = this.serialize(this.trimResponse(dsResponse));

        this.logDebug("storeResponse serializing: " + (new Date().getTime() - ts) + "ms");

        // Unless we are already storing a response for this request (in which case we'll just
        // freshen it), check if we're about to bust the maximum responses limit
        if (this.get(key) == null) {
            if (this.getSCStoredValuesCount() >= this.maxResponsesToPersist) {
                this.removeOldestEntry();
            }
        }
        this.put(key, value);
    },
    
    trimRequest : function (dsRequest) {
        var keyProps = ["dataSource", "operationType", "operationId",
                        "textMatchStyle", "values", "sortBy", 
                        "startRow", "endRow", "data"],
                 
            trimmed = {},
            undef;
        for (var i = 0; i < keyProps.length; i++) {
            if (dsRequest[keyProps[i]] !== undef) {
                trimmed[keyProps[i]] = dsRequest[keyProps[i]];
            }
        }

        return trimmed;
    },
    trimResponse : function (dsResponse) {
        var keyProps = ["dataSource", 
                        "startRow", "endRow", "totalRows", 
                        "data", "offlineTimestamp",
                        "status", "errors", // note we don't actually cache errors yet
                        "invalidateCache", "cacheTimestamp"],
            trimmed = {},
            undef;
        for (var i = 0; i < keyProps.length; i++) {
            if (dsResponse[keyProps[i]] !== undef) {
                trimmed[keyProps[i]] = dsResponse[keyProps[i]];
            }
        }
        return trimmed;
    },

    getResponse : function (dsRequest) {
        //!DONTOBFUSCATE
        var trimmedRequest = this.trimRequest(dsRequest),
            key = this.serialize(trimmedRequest),
            value = this.get(key),
            returnValue;
        // using raw eval() because SmartClient might not be available
        eval('returnValue = ' + value);
        if (returnValue) returnValue.fromOfflineCache = true;
        return returnValue;
    },
    serialize : function (obj) {
        return isc.Comm.serialize(obj, false);
    },
    
    // Offline storage browser
    // ---------------------------------------------------------------------------------------
    showStorageInfo : function () {
        if (!this.storageBrowser) {
            if (isc.Offline.localStorageType() == isc.Offline.USERDATA_PERSISTENCE) {
                isc.Timer.setTimeout(function () {
                isc.say("WARNING:  This browser uses an old storage mechanism that does not " +
                        "permit arbitrary key/value pair storage.  This means we have to " +
                        "store extra management data, with the upshot that the metrics reported " +
                        "for 'priority queue' and 'overhead' are indicative, but not accurate");
                }, 0);
            }
            this.metricsDF = isc.DynamicForm.create({
                width: "100%",
                numCols: 6,
                fields: [
                    {name: "storedEntries", title: "No. entries", disabled: true},
                    {name: "storedKeyBytes", title: "Used by keys", disabled: true},
                    {name: "storedValueBytes", title: "Used by values", disabled: true},
                    {name: "priorityQueue", title: "Used by Priority Queue", disabled: true},
                    {name: "metricsOverhead", title: "Metrics overhead", disabled: true},
                    {name: "total", title: "Total Bytes", disabled: true}
                ]
            });
            this.storageLG = isc.ListGrid.create({
                width: "100%",
                height: "*",
                canRemoveRecords: true,
                removeData: function (record) {
                    isc.ask("Remove this entry?", function (value) {
                        if (value) {
                            isc.Offline.remove(record.key);
                            isc.Offline.refreshStorageInfo();
                        }
                    });
                },
                rowDoubleClick: function(record) {
                    isc.Offline.createStorageEditorWindow();
                    isc.Offline.storageEditorWindow.show();
                    isc.Offline.storageEditor.editRecord(record);
                },
                fields: [
                    {name: "key", width: "25%", title: "Key"},
                    {name: "value", title: "Value"}
                ]
            });
            this.storageBrowser = isc.Window.create({
                autoCenter: true,
                canDragResize: true,
                width: Math.floor(isc.Page.getWidth() * 0.5),
                height: Math.floor(isc.Page.getHeight() * 0.5),
                title: "Offline Storage",
                items: [
                    this.metricsDF,
                    this.storageLG,
                    isc.HLayout.create({
                        width: "100%", height: 1,
                        members: [
                            isc.LayoutSpacer.create({width: "*"}),
                            isc.Button.create({
                                title: "Add Entry",
                                click: function () {
                                    isc.Offline.createStorageEditorWindow();
                                    isc.Offline.storageEditorWindow.show();
                                    isc.Offline.storageEditor.editNewRecord();
                                }
                            })
                        ]
                    })
                ]
            });
        }
        
        this.storageBrowser.show();
        this.refreshStorageInfo();
    },

    createStorageEditorWindow : function () {
        if (!isc.Offline.storageEditorWindow) {
            isc.Offline.storageEditor = isc.DynamicForm.create({
                fields: [
                    {name: "key", title: "Key", editorType: "TextAreaItem", width: 400},
                    {name: "value", title: "Value", editorType: "TextAreaItem", width: 400},
                    {name: "saveButton", type: "button", title: "Save", click: function () {
                        var form = isc.Offline.storageEditor;
                        if (form.saveOperationType == "update" &&
                                form.getValue("key") != form.getOldValue("key")) 
                        {
                            isc.ask("Key has changed - this will create a new entry. " +
                                    "Do you want to retain the old entry as well? (if " + 
                                    "you answer 'No', it will be removed", 
                                    function (value) {
                                        if (value === false) {
                                            isc.Offline.remove(form.getOldValue("key"));
                                        }
                                        if (value != null) {
                                            isc.Offline.put(form.getValue("key"), 
                                                            form.getValue("value"));
                                            isc.Offline.storageEditorWindow.hide();
                                            isc.Offline.refreshStorageInfo();
                                        }
                                    });
                        } else {
                            isc.Offline.put(form.getValue("key"), form.getValue("value"));
                            isc.Offline.storageEditorWindow.hide();
                            isc.Offline.refreshStorageInfo();
                        }
                    }}
                ]
            });
            isc.Offline.storageEditorWindow = isc.Window.create({
                bodyProperties: { margin: 5 },
                title: "Edit Offline Storage Entry",
                isModal: true,
                autoCenter: true,
                height: 280,
                width: 480,
                items: [
                    isc.Offline.storageEditor
                ]
            });
        }
    },
    
    refreshStorageInfo : function () {
        this.metricsDF.editRecord(isc.Offline.getStorageMetrics());
        var dataObj = isc.Offline.getCacheContents();
        var data = [];
        for (var key in dataObj) {
            data.add({key: key, value: dataObj[key]});
        }
        this.storageLG.setData(data);
    }

};

var UserDataPersistence = {

    isInitialized: false,
    poolSize: 10,
    
    keyIndexKey: "keyIndex",
    reverseKeyIndexKey: "reverseKeyIndex",
    
    
    init : function () {
        this.userDataSpan = [];
        for (var i = 0; i < this.poolSize; i++) {
            this.userDataSpan[i] = document.createElement('span');
            this.userDataSpan[i].ID = 'isc_userData_' + i;
            this.userDataSpan[i].style.behavior = 'url(#default#userdata)';
            document.body.appendChild(this.userDataSpan[i]);
            this.userDataSpan[i].load("isc_userData_" + i);
        }
        this.keyIndexStore = document.createElement('span');
        this.keyIndexStore.ID = 'isc_userData_keyIndex';
        this.keyIndexStore.style.behavior = 'url(#default#userdata)';
        document.body.appendChild(this.keyIndexStore);
        this.keyIndexStore.load("isc_userData_keyIndex");
        
        this.keyIndex = this.getKeyIndexFromStore();
        this.reverseKeyIndex = this.getReverseKeyIndexFromStore();
        if (!this.keyIndex) {
            this.keyIndex = {};
            this.reverseKeyIndex = {};
        } else if (!this.reverseKeyIndex) this.buildReverseKeyIndex();
        
        this.buildNextAttributeInfo();
        
        this.isInitialized = true;
    },
    
    clearCacheNatively : function () {
        for (var i = 0; i < this.poolSize; i++) {
            var attrs = this.userDataSpan[i].xmlDocument.firstChild.attributes;
            while (attrs.length > 0) {
                this.userDataSpan[i].removeAttribute(attrs[0].name);
            }
        }
        this.keyIndex = {};
        this.reverseKeyIndex = {};
        this.saveKeyIndex();
    },
    
    getNativeStoredValuesCount : function () {
        var count = 0;
        for (var i = 0; i < this.poolSize; i++) {
            count += this.userDataSpan[i].xmlDocument.firstChild.attributes.length;
        }
        return count;
    },
    
    
    getKeyForNativeIndex : function (index) {
        var iCounter = 0;
        for (var i = 0; i < this.poolSize; i++) {
            if (iCounter + this.userDataSpan[i].xmlDocument.firstChild.attributes.length > index) {
                var offsetIndex = index - iCounter;
                var attrName = this.userDataSpan[i].xmlDocument.firstChild.attributes[offsetIndex].name,
                    attrNum = attrName.substring(1),
                    dsKey = this.getKeyIndexValue(i, attrNum);
                return this.getUserKey(dsKey);
            }
            
        }
    },
    
    getKeyIndexValue : function (index, attrName) {
        var attrNum = attrName.substring(1);
        if (index == 0) {
            return "00000".substring(attrNum.length) + attrNum;
        }
        return index * 10000 + (1 * attrNum);
    },
    
    getUserKey : function (userKey) {
        return this.reverseKeyIndex[userKey];
    },
    
    getDataStoreKey : function (key) {
        return this.keyIndex[key];
    },
    
    _getValue : function (dataStore, attr) {
        return this.userDataSpan[dataStore].getAttribute(attr);
    },
    
    getValue : function (userKey) {
        var key = this.getDataStoreKey(userKey),
            undef;
        if (key === undef) return null;
        var dataStore = ("" + key).substring(0, 1),
            attr = "v" + (("" + key).substring(1) * 1);
        return this._getValue(dataStore, attr);
    },
    
    
    putValue : function (userKey, value) {
        var key = this.getDataStoreKey(userKey);
        if (key) {  // We're already storing a value for this key
            var dataStore = ("" + key).substring(0, 1),
                attr = "v" + (("" + key).substring(1) * 1),
                savedValue = this._getValue(dataStore, attr);
        } else {
            var dataStore = this.getDataStoreForNewItem(),
                attr = this.getNextAttributeName(dataStore);
        }
        this.userDataSpan[dataStore].setAttribute(attr, value);
                
        
        try {
            this.userDataSpan[dataStore].save("isc_userData_" + dataStore);
            this.addToKeyIndex(userKey, dataStore, attr);
        } catch(e) {
            if (isc.Offline.isStorageException(e)) {
                if (savedValue) {
                    this.userDataSpan[dataStore].setAttribute(attr, savedValue);
                } else {
                    this.userDataSpan[dataStore].removeAttribute(attr);
                    this.removeFromKeyIndex(userKey);
                }
            }
            throw e;
        }
    },
    
    removeValue : function (userKey) {
        var key = this.getDataStoreKey(userKey),
            undef;
        if (key === undef) {
            Offline.logDebug("userData: in removeValue, no value for key '" + userKey + "' was found");
            return;
        }
        var dataStore = ("" + key).substring(0, 1),
            attr = "v" + (("" + key).substring(1) * 1);
        this.userDataSpan[dataStore].removeAttribute(attr);
        this.userDataSpan[dataStore].save("isc_userData_" + dataStore);
        this.removeFromKeyIndex(userKey);
        this.unusedAttributeNumbers[dataStore].push(attr.substring(1) * 1);
    },
    
    getDataStoreForNewItem : function () {
        // TODO: For now, we'll allocate items to stores on a simple round-robin basis; always
        // allocating to the least-full store is a better approach, but requires us to track 
        // additional metrics
        var undef;
        if (this.nextDataStoreToUse === undef) {
            // Select the start point randomly, to avoid overusing the the first few pools in
            // applications that are restarted often, relative to the number of items they store
            this.nextDataStoreToUse = Math.floor(Math.random() * this.poolSize);
        }
        var rtnValue = this.nextDataStoreToUse++;
        if (this.nextDataStoreToUse >= this.poolSize) this.nextDataStoreToUse = 0;
        return rtnValue;
    },
    
    buildNextAttributeInfo : function () {
        this.nextAttributeNumber = [];
        this.unusedAttributeNumbers = [];
        for (var i = 0; i < this.poolSize; i++) {
            this.unusedAttributeNumbers[i] = [];
            var attrs = this.userDataSpan[i].xmlDocument.firstChild.attributes;
            var work = [];
            for (var j = 0; j < attrs.length; j++) {
                var num = attrs[j].name.substring(1) * 1;
                if (!isNaN(num)) work.add(attrs[j].name.substring(1) * 1);
            }
            if (work.sort) work.sort();
            else this.sort(work);
            var counter = 0;
            for (j = 0; j < work.length; j++) {
                if (work[j] == counter) {
                    counter++;
                    continue;
                }
                while(work[j] != counter && counter <= 9999) {
                    this.unusedAttributeNumbers[i].push(counter++);
                }
                counter++;
            }
            this.nextAttributeNumber[i] = counter;
        }
    },
    
    //>GWT_Standalone
    sort : function (array) {
        for (var i = 0; i < array.length; i++) {
            var swapped = false;
            for (var j = 1; j < array.length - i; j++) {
                if (array[j] < array[j-1]) {
                    var temp = array[j];
                    array[j] = array[j-1];
                    array[j-1] = temp;
                    swapped = true;
                }
            }
            if (!swapped) break;
        }
    },
    //<GWT_Standalone
    
    getNextAttributeName : function (dataStore) {

        if (this.unusedAttributeNumbers[dataStore] && 
                this.unusedAttributeNumbers[dataStore].length > 0) 
        {
            return "v" + this.unusedAttributeNumbers[dataStore].shift();
        }

        if (this.nextAttributeNumber[dataStore] == null) {
            this.nextAttributeNumber[dataStore] = 1;
        }
        
        return "v" + this.nextAttributeNumber[dataStore]++;
    },
    
    addToKeyIndex : function (userKey, dataStore, attr) {
        var keyIndexValue = this.getKeyIndexValue(dataStore, attr);
        this.keyIndex[userKey] = keyIndexValue;
        this.reverseKeyIndex[keyIndexValue] = userKey;
        this.saveKeyIndex();
    },
    
    
    removeFromKeyIndex : function (userKey) {
        var keyIndexValue = this.keyIndex[userKey];
        delete this.keyIndex[userKey];
        delete this.reverseKeyIndex[keyIndexValue];
        this.saveKeyIndex();
    },
    
    saveKeyIndex : function () {
        this.keyIndexStore.setAttribute(this.keyIndexKey, Offline.serialize(this.keyIndex));
        this.keyIndexStore.setAttribute(this.reverseKeyIndexKey, Offline.serialize(this.reverseKeyIndex));
        this.keyIndexStore.save("isc_userData_keyIndex");
    },
    
    buildReverseKeyIndex : function() {
        this.reverseKeyIndex = {};
        for (var key in this.keyIndex) {
            this.reverseKeyIndex[keyIndex[key]] = key;
        }
    },

    getKeyIndexFromStore : function () {
        //!DONTOBFUSCATE
        var kiText = this.keyIndexStore.getAttribute(this.keyIndexKey);
        if (kiText) {
            eval("var ki = " + kiText);
        } else {
            var ki = null;
        }
        return ki;
    },
    
    getReverseKeyIndexFromStore : function () {
        //!DONTOBFUSCATE
        var kiText = this.keyIndexStore.getAttribute(this.reverseKeyIndexKey);
        if (kiText) {
            eval("var ki = " + kiText);
        } else {
            var ki = null;
        }
        return ki;
    }
    
};

if (window.isc) {
    isc.defineClass("Offline").addClassProperties(Offline);
    isc.defineClass("UserDataPersistence").addClassProperties(UserDataPersistence);
} else {

//>GWT_Standalone
isc.addProperties = function (objOne, objTwo) {
    for (var propName in objTwo) objOne[propName] = objTwo[propName];
}

isc.addProperties(isc.Offline, {
    // utilities
    // ---------------------------------------------------------------------------------------
    serialize : function (object) {
        return isc.OfflineJSONEncoder.encode(object);
    },
    logDebug : function (message) {
        if (console) console.log(message);
    },
    logError : function (message) {
        if (console) {
            console.log(message);
        } else {
            alert(message);
        }
    },
    echoLeaf : function (obj) {
        var output = "",
            undef;
        if (obj === undef) return "undef";
        try {
            if (typeof obj == "Array") {
    			output += "Array[" + obj.length + "]";
            } else if (typeof obj == "Date") {
    			output += "Date(" + obj.toShortDate() + ")";
            } else if (typeof obj == "Function") {
                output += isc.Func.getName(obj, true) + "()";
    		} else {
    			switch (typeof obj) {
    			case "string" : 
                    // for shorter strings show the whole thing.  Also, in "longMode" don't
                    // shorten.
                    if (obj.length <= 40) { 
                        output += '"' + obj + '"'; 
                        break;
                    }
    
                    // for long strings, show an elipsis and the strings full length
                    output += '"' + obj.substring(0, 40) + '..."[' + obj.length + ']';

                    // convert CR/LF to avoid spanning several lines
                    output = output.replaceAll("\n", "\\n").replaceAll("\r", "\\r");
                    break;
    			case "object" :
    				// typeof null is "object"
    				if (obj == null) { output += "null"; break; }
    
                    // DOM object
                    if (obj.tagName != null) {
                        output += "[" + obj.tagName + "Element]";
                        break;
                    }
    
    			    var toString = "" + obj;
    			    if (toString != "" && toString != "[object Object]" && 
                        toString != "[object]") 
                    {
                        // someone went through the trouble of making a better toString(), so
                        // use it.  NOTE: check for "" because in IE an XmlNodeList among
                        // others will toString() to ""
                        output += toString;
                        break;
                    }
    
    			    // return generic "Obj"
                    output += "Obj";
                
    				break;
    			default: output += "" + obj; // invoke native toString()
    			}
    		}
    		return output;
        } catch (e) {
            var message = "[Error in echoLeaf: " + e + "]";
            output += message;
            return output;
        }            
	},

    echo : function (object) { return this.serialize(object); }

});


isc.OfflineJSONEncoder = {

_serialize_remember : function (objRefs, object, path) {
	objRefs.obj.add(object);
	objRefs.path.add(path);
},
_serialize_cleanNode : function (object) {
    var treeId = object["_isc_tree"];
    if (treeId != null) {
        var theTree = window[treeId];
        if (theTree && theTree.parentProperty && object[theTree.parentProperty]) {
            object = theTree.getCleanNodeData(object);
        }
    }
    return object;
},
_serialize_alreadyReferenced : function (objRefs, object) {
	var rowNum = objRefs.obj.indexOf(object);
	if (rowNum == -1) return null;
	return objRefs.path[rowNum];
},
_serialize_addToPath : function (objPath, newIdentifier) {
	if (isc.isA.Number(newIdentifier)) {
		return objPath + "[" + newIdentifier + "]";
	} else if (
               !isc.Comm._simpleIdentifierRE.test(newIdentifier))
    {
		return objPath + '["' + newIdentifier + '"]';
	} else {
		return objPath + "." + newIdentifier;
	}
}, 

encode : function (object) {
    this.objRefs = {obj:[],path:[]};
    var retVal = this._serialize(object, this.prettyPrint ? "" : null , null);
    this.objRefs = null;
    return retVal
},

dateFormat: "xmlSchema",



encodeDate : function (date) {
    if (this.dateFormat == "dateConstructor") { 
        return "new Date(" + date.getTime() + ")";
    } else { // quotes for xml schema
        return '"' + this.toSchemaDate(date) + '"';    
    }
},

toSchemaDate : function (date) {
    var dd = "" + date.getDate(),
        mm = "" + (date.getMonth() + 1),
        yyyy = "" + (date.getyear() + 1900),
        hh = "" + date.getHours(),
        mi = "" + date.getMinutes(),
        ss = "" + date.getSeconds();
        
    dd = dd.length == 1 ? "0" + dd : dd;    
    mm = mm.length == 1 ? "0" + mm : mm;    
    hh = hh.length == 1 ? "0" + hh : ff;    
    mi = mi.length == 1 ? "0" + mi : mi;    
    ss = ss.length == 1 ? "0" + ss : ss;    
        
   return yyyy + "-" + mm + "-" + dd + "T" + hh + ":" + mi + ":" + ss;
},

strictQuoting: true,
circularReferenceMode: "path",
circularReferenceMarker: "$$BACKREF$$",
prettyPrint: false,

_serialize : function (object, prefix, objPath) {	
    if (!objPath) {
        if (object && object.getID) objPath = object.getID();
        else objPath = "";
    }
	
	if (object == null) return null;

	// handle simple types
    // In Safari a cross-frame scripting bug means that the 'asSource' method may not always be
    // available as an instance method.
    // call the static version of the same method if this happens.
	if (this.isAString(object)) return this.asSource(object);
	if (this.isAFunction(object)) return null;
	if (this.isANumber(object) || this.isASpecialNumber(object)) return object;
	if (this.isABoolean(object)) return object;
	if (this.isADate(object)) return this.encodeDate(object);

    // skip instances (this is a simplification of the root code)
    if (this.isAnInstance(object)) return null;
	
	var prevPath = this._serialize_alreadyReferenced(this.objRefs, object);
    
	if (prevPath != null && objPath.contains(prevPath)) {
        
        var nextChar = objPath.substring(prevPath.length, prevPath.length+1);
        if (nextChar == "." || nextChar == "[" || nextChar == "]") {
            var mode = this.circularReferenceMode;
            if (mode == "marker") {
                return "'" + this.circularReferenceMarker + "'";    
            } else if (mode == "path") {
                return  "'" + this.circularReferenceMarker + ":" + prevPath + "'";   
            } else {
                return null;    
            }
        }
	}

    if (this.isAClassObject(object)) return null;

    if (object == window) return null;

	this._serialize_remember(this.objRefs, object, objPath);
	
	// if there is a serialize method associated with this object, call that
    // (NOTE: Leaving this in, in case we're running in a SmartClinet context)
	if (this.isAFunction(object._serialize)) {
        return object._serialize(prefix, this.objRefs, objPath);
    }

	// handle arrays as a special case
	if (this.isAnArray(object))	{
        return this._serializeArray(object, objPath, this.objRefs, prefix);
    }

    var data;
	// if the object has a getSerializeableFields, use whatever it returns, otherwise just use the object
    if (object.getSerializeableFields) {
		data = object.getSerializeableFields([], []);
    } else {
        data = object;
    }
	// and return anything else as a simple object
	return this._serializeObject(data, objPath, this.objRefs, prefix);
},

_serializeArray : function (object, objPath, objRefs, prefix) {
	// Replaced references to the SmartClient StringBuffer class with native strings throughout
    // this method, rather than port that class's functionality. Also removed pretty print 
    // facilities - we don't need them for this
	var output = "[";
	// for each element in the array
	for (var i = 0, len = object.length; i < len; i++) {
		var value = object[i];
        if (prefix != null) output += "\r" + prefix;

        var objPath = this._serialize_addToPath(objPath, i);
        var serializedValue =  
            this._serialize(value, (prefix != null ? prefix : null), objPath);
        output += serializedValue + ",";
        if (prefix != null) output += " ";
	}
	// get rid of the trailing comma, if any
	var commaChar = output.lastIndexOf(",");
	if (commaChar > -1) output = output.substring(0, commaChar);

	// add the end array marker
    if (prefix != null) output += "\r" + prefix;
    output += "]";

	// and return the output
	return output;	
},

_serializeObject : function (object, objPath, objRefs, prefix) {
	// Replaced references to the SmartClient StringBuffer class with native strings throughout
    // this method, rather than port that class's functionality. Also removed pretty print 
    // facilities - we don't need them for this
	var output = "{",
        undef;

        
    object = this._serialize_cleanNode(object);

    // Not trying to support every edge case at this point - skipping code that copes with
    // XMLNodes here, rather than porting isc.echoLeaf
    try {
        
    	for (var key in object) break;
    } catch (e) {
        return null;
    }

	for (var key in object) {
		if (key == null) continue;
        
        
        if (key == "xmlHttpRequest") continue;
        
        // skip internal properties, if the flag is set (simplified this code to remove 
        // framework calls)
		if (key.substring(0,1) == "_" || key.substring(0,1) == "$") continue;
		var value = object[key];

		if (this.isAFunction(value)) continue;

        // omit instances entirely if so configured (this code is simplified, we always 
        // skip instances)
        if (this.isAnInstance(value)) continue;

		// convert the key to a string
		var keyStr = key.toString();
		// and quote it
		keyStr = '"' + keyStr + '"';
    
        var objPath = this._serialize_addToPath(objPath, key);
        var serializedValue;
        // don't try to serialize references to GWT Java objects
        if (key != "__ref" && key != "__module") {
            serializedValue = 
                this._serialize(value, (prefix != null ? prefix : null), objPath);
        }

		// now output the key : value pair
        if (prefix != null) output += "\r" + prefix;
		output += keyStr + ":" + serializedValue + ",";
    
        if (prefix != null) output.append(" ");
	}
	// get rid of the trailing comma, if any
	var commaChar = output.lastIndexOf(",");
	if (commaChar > -1) output = output.substring(0, commaChar);

	// add the end object marker
    if (prefix != null) output += "\r" + prefix;
    output += "}";

	// and return the output
	return output;
},
    
// These methods ripped off from SmartClient's IsA class
isAString : function (object) {
    if (object == null) return false;
    if (typeof object == this._$function) return false;
    if (object.constructor && object.constructor.__nativeType != null) {
        return object.constructor.__nativeType == 4;
    }
    if (object.Class != null && object.Class == this._$String) return true;

    return typeof object == "string";
},
isAnArray : function (object) {
    if (object == null) return false;
    if (typeof object == this._$function) return false;
    if (object.constructor && object.constructor.__nativeType != null) {
        return object.constructor.__nativeType == 2;
    }
    if (isc.Browser.isSafari) return ""+object.splice == "(Internal function)";
    return ""+object.constructor == ""+Array;
},
isAFunction : function (object) {
    if (object == null) return false;
    if (isc.Browser.isIE && typeof object == this._$function) return true;
    var cons = object.constructor;
    if (cons && cons.__nativeType != null) {
        if (cons.__nativeType != 1) return false;
        if (cons === Function) return true;
    }

    return isc.Browser.isIE ? (isc.emptyString+object.constructor == Function.toString()) : 
                              (typeof object == this._$function);
},

isANumber : function (object) {
    if (object == null) return false;
    if (object.constructor && object.constructor.__nativeType != null) {
        if (object.constructor.__nativeType != 5) return false;
    } else {
        if (typeof object != "number") return false;
    }
    // it's a number, now check if it's a valid number
    return !isNaN(object) && 
        object != Number.POSITIVE_INFINITY && 
        object != Number.NEGATIVE_INFINITY;
},
isASpecialNumber : function (object) {
    if (object == null) return false;
    if (object.constructor && object.constructor.__nativeType != null) {
        if (object.constructor.__nativeType != 5) return false;
    } else {
        if (typeof object != "number") return false;
    }
    return (isNaN(object) || object == Number.POSITIVE_INFINITY ||
            object == Number.NEGATIVE_INFINITY);
},
isABoolean	: function (object) {
    if (object == null) return false;
    if (object.constructor && object.constructor.__nativeType != null) {
        return object.constructor.__nativeType == 6;
    }
    return typeof object == "boolean";
},

isADate : function (object) {
    if (object == null) return false;
    if (object.constructor && object.constructor.__nativeType != null) {
        return object.constructor.__nativeType == 3;
    }
    return (""+object.constructor) == (""+Date) &&
            object.getDate && isc.isA.Number(object.getDate()) 
},
isAnXMLNode : function (object) {
    if (object == null) return false;
    if (isc.Browser.isIE) {
        return object.specified != null && object.parsed != null && 
               object.nodeType != null && object.hasChildNodes != null;
    }
    var doc = object.ownerDocument;
    if (doc == null) return false;
    return doc.contentType == this._$textXML;
},
isAnInstance : function (object) {	
    return (object != null && object._scPrototype != null)
},
isAClassObject : function (object) {
	return (object != null && object._isClassObject == true)
}, 
    
    
// This method ripped out of the SmartClient String enhancements
asSource : function (string, singleQuote) {
    if (!this.isAString(string)) string = ""+string;

    var quoteRegex = singleQuote ? String._singleQuoteRegex : String._doubleQuoteRegex,
        outerQuote = singleQuote ? "'" : '"';
    return outerQuote +
               string.replace(/\\/g, "\\\\")
                     // quote whichever quote we use on the outside
                     .replace(quoteRegex, '\\' + outerQuote)
                     .replace(/\t/g, "\\t")
                     .replace(/\r/g, "\\r")
                     .replace(/\n/g, "\\n") + outerQuote;
}              
    
}; // close offline serializer definition

//<GWT_Standalone

} // close !window.isc

})(); // close wrapper function and execute
