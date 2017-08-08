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
// =======================================================================================
// OfflineDataSource
// =======================================================================================
//> @class OfflineDataSource
// DataSource implementation using isc.Offline for the persientence mechanis:  Loads cacheData 
// automatically at init via Offline.get(), and saves it via Offline.put() every time it's 
// modified
// 
// @visibility internal
//<


isc.defineClass("OfflineDataSource", "DataSource").addProperties({
    cacheAllData: true, 
    clientOnly: true,

    //> @attr offlineDataSource.storageKey              (string : null : [IR]) 
    // The key to be used for Offline storage.
    //<

    //> @method offlineDataSource.getOfflineStorageKey() 
    // Obtain a key to be used for Offline storage, whether one has been configured explcitly
    // or not.
    // 
    // @return (String) OfflineDataSource.storageKey with OfflineDataSource.ID used 
    //                  if storageKey is unset
    // @visibility internal
    //<
    getOfflineStorageKey : function() {
        return this.storageKey || this.getID();
    },

          
    updateOfflineCache : function() {
        var that = this;
        var func = function() {
            isc.Offline.put(that.getOfflineStorageKey(), isc.JSON.encode(that.cacheData));    
        }
        isc.Timer.setTimeout(func, 1);        
    },

    init : function () {
        this.Super("init", arguments);
        var data = this.cacheData;
        
        if (! data) {
            var json = isc.Offline.get(this.getOfflineStorageKey());
            if (json) {
                data = isc.JSON.decode(json, {dateFormat: "logicalDateConstructor"});
            } else {
                data = [];
            }
            this.setCacheData(data);
        }
        
        this.observe(this, "updateCaches", "observer.updateOfflineCache()");
    }
});