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



isc.defineClass("PortletEditProxy", "EditProxy").addMethods({
    canAdd : function (type) {
        // Don't let Portlets be added directly to Portlets, because it is almost never what
        // would be wanted.
        if (type == "Portlet") return false;
        return this.Super("canAdd", arguments);
    },
    drop : function () {
        return isc.EH.STOP_BUBBLING;
    }
});

isc.defineClass("PortalColumnEditProxy", "EditProxy").addMethods({
    // We don't actually want to add anything via drag & drop ... that will be
    // handled by PortalColumnBody
    canAdd : function (type) {
        return false;
    },
    drop : function () {
        return isc.EH.STOP_BUBBLING;
    }
});

