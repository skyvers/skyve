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
isc.defineClass("PortletEditProxy", "EditProxy").addMethods({
    canAdd : function (type) {
        // Don't let Portlets be added directly to Portlets, because it is almost never what
        // would be wanted. But let the caller ask parents ...
        if (type == "Portlet") return null;
        return this.Super("canAdd", arguments);
    },

    drop : function () {
        return null;
    },

    dropMove : function () {
        return null;
    },

    dropOver : function () {
        return null;
    }
});

isc.defineClass("PortalRowEditProxy", "LayoutEditProxy");
isc.PortalRowEditProxy.addProperties({
    // PortalRow has internal logic which handles drag/drop
    // in editMode, so defer to that.
    
    dropMove : function () {
        return this.creator.dropMove();
    },

    dropOver : function () {
        return this.creator.dropOver();
    },

    drop : function () {
        return this.creator.drop();
    }
});

isc.defineClass("PortalLayoutEditProxy", "LayoutEditProxy").addMethods({
    canAdd : function (type) {
        var result = this.Super("canAdd", type);

        // Don't allow drops to bubble out of the PortalLayout,
        // because the PortalLayout will handle everything
        // except for the "dead zone" in the column header,
        // which should conclusively be dead. So, we convert
        // any "null" response to "false", to conclusively deny
        // the drop.
        return result || false;
    }
});

isc.defineClass("PortalColumnEditProxy", "LayoutEditProxy").addMethods({
    // We don't actually want to add anything via drag & drop ... that will be
    // handled by PortalColumnBody
    canAdd : function (type) {
        return null;
    },
    
    drop : function () {
        return null;
    },

    dropMove : function () {
        return null;
    },

    dropOver : function () {
        return null;
    }
});

