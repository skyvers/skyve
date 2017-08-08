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
isc.defineInterface("Observer").addInterfaceProperties({

registerObserved : isc.ClassFactory.TARGET_IMPLEMENTS,
unregisterObserved : isc.ClassFactory.TARGET_IMPLEMENTS

});

isc.defineInterface("AutoObserver", "Observer").addInterfaceProperties({

observedName: "observed",
observations: {},

registerObserved : function (observed) {
    this[this.observedName] = observed;
    for (var method in this.observations) {
        this.observe(observed, method, this.observations[method]);
    }
    if (isc.isA.Canvas(observed)) {
        this.observe(observed, "destroy", "observer.unregisterObserved(observed)");
    }
},

unregisterObserved : function (observed) {
    this[this.observedName] = null;
    for (var method in this.observations) {
        this.ignore(observed, method);
    }
}

});

isc.defineInterface("GridAutoObserver", "AutoObserver").addInterfaceProperties({
observedName: "grid"
});


isc.defineClass("GridTotalRowsIndicator", "Label", "GridAutoObserver").addProperties({

height: 1,
wrap: false,
overflow: "visible",
valign: "center",

observations: {
    "dataArrived": "observer.gridDataChanged()",
    "setData": "observer.gridDataChanged()"
},

dynamicContents: true,
contents: "Total Rows: ${this.rowCount}",
rowCount: "N/A",

gridDataChanged : function () {
    var data = this.grid.data;
    if (!data) this.rowCount = "N/A";

    if (isc.isA.ResultSet(data)) {
        if (data.lengthIsKnown()) {
            if (data.getLength() != 0 && data.progressiveLoading) {
                var lastRowIndex = data.getLength()-1;
                if (data.rowIsLoaded(lastRowIndex)) this.rowCount = data.getLength();
                else this.rowCount = data.getLength()+"+ (progressive loading)";
            } else { 
                this.rowCount = data.getLength();
            }
        }
        else this.rowCount = "N/A";
    } else if (isc.isAn.Array(data)) {
        this.rowCount = data.getLength();
    }

    this.markForRedraw();
}

});


isc.defineClass("ObserverToolStrip", "ToolStrip").addProperties({

initWidget : function () {
    this.Super("initWidget", arguments);

    for (var i = 0; i < this.members.length; i++) {
        var m = this.members[i];
        if (isc.isAn.Observer(m)) {
            m.registerObserved(this.grid);
        }
    }
}

});

isc.defineClass("GridToolStrip", "ObserverToolStrip").addProperties({

membersMargin: 5,

addButtonDefaults: {
    _constructor: "Img",
    size: 16,
    layoutAlign: "center",
    src: "[SKIN]/actions/add.png",    
    click: "this.creator.grid.startEditingNew()"    
},

removeButtonDefaults: {
    _constructor: "Img",
    size: 16,
    layoutAlign: "center",
    src: "[SKIN]/actions/remove.png",    
    click: "this.creator.grid.removeSelectedData()"
},

refreshButtonDefaults: {
    _constructor: "Img",
    size: 16,
    layoutAlign: "center",
    src: "[SKIN]/actions/refresh.png",    
    click: "this.creator.grid.invalidateCache()"
},

exportButtonDefaults: {
    _constructor: "IButton",
    title: "Export to CSV",
    layoutAlign: "center",
    click: "this.creator.grid.exportData()"
},

totalRowsIndicatorDefaults: {
    _constructor: "GridTotalRowsIndicator",
    layoutAlign: "center"   
},

members: ["autoChild:removeButton", "autoChild:addButton", "autoChild:exportButton",
         "starSpacer",
         "autoChild:refreshButton", "autoChild:totalRowsIndicator"],

initWidget : function () {
    this.Super("initWidget", arguments);
}

});