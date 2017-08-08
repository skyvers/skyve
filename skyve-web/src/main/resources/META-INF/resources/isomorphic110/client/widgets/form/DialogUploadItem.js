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
// Class will not work without the ListGrid
if (isc.ListGrid) {




//> @class DialogUploadItem
// A FormItem that allows uploading a single file as a field in a record stored in a related
// DataSource.
//
// @visibility internal
//<

isc.defineClass("DialogUploadItem", "StaticTextItem").addProperties({
    iconHeight: 16,
    iconWidth: 16,
    icons: [
        {src: "[SKIN]MultiUploadItem/icon_add_files.png", name:"upload", prompt: "Upload File",  click: "item.showPicker(true)"},
        {src: "[SKIN]MultiUploadItem/icon_remove_files.png", name:"remove", prompt: "Remove File", click: "item.removeFile()"}
    ],

    pickerConstructor: "DialogUploadPicker",
    noFileString: "[NONE]",


init : function () {
    this.Super("init", arguments);
    
    if (!this.pickerDefaults) this.pickerDefaults = {};
    isc.addProperties(this.pickerDefaults, {
        dataSource: this.dataSource
    });
},

mapValueToDisplay : function (value) {
    return value == null ? this.noFileString : this.Super("mapValueToDisplay", arguments);
},

showPicker : function () {
    this.Super("showPicker", arguments);
    var primaryKey = this.getValue('primaryKey');
    // pass primaryKey to ensure that uploads overwrite any existing file for this field.
    this.picker.foreignKeyValues = {
        primaryKey: primaryKey
    };
},

removeFile : function () {
    var primaryKey = this.getValue();
    if (primaryKey != this.defaultValue) {
        var ds = isc.DataSource.get(this.dataSource);
        ds.removeData({primaryKey: primaryKey}, this.getID()+".removeFileCallback(dsResponse)");
    }
},

removeFileCallback : function (dsResponse) {
    if (dsResponse.status != isc.DSResponse.STATUS_SUCCESS) {
        isc.warn("Unable to remove file: " + dsResponse.data);
        return;
    }
    this.setValue(this.defaultValue);
},
fileUploaded : function (dsRequest, dsResponse) {
    var data = dsResponse.data;
    var valueMap = {};
    valueMap[data.primaryKey] = data.file_filename;
    this.setValueMap(valueMap);
    this.setValue(data.primaryKey);
},

destroy : function () {
    if (this.picker) this.picker.destroy(); 
    this.Super("destroy", arguments);
},

_shouldAllowExpressions : function () {
    return false;
}


});


isc.defineClass("DialogUploadPicker", "MultiFilePicker").addProperties({
    maxUploadFields: 1,
    uploadWithoutPKButtonName: "Upload",
    uploadWithPKButtonName: "Upload",
    showUploadRemoveButton: false,
    uploadWithoutPK: true
});

}
