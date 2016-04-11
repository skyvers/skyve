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
//> @class FilePickerForm
//
// Layout with various input forms so BatchUploader could be used to serve as general-purpose
// data importer.
//
// @visibility internal
//<


isc.defineClass("FilePickerForm", "VLayout").addProperties({
    autoDraw: false,
    width: "100%",
    padding: 10,
    
    showSelectForm: true,
    showUploadForm: true,
    showPasteForm: true,
    showFetchForm: true,
    
    formsDefaults: {
        titleWidth: 140,
        cellPadding: 6,
        width: "100%"
    },
    
    //> @attr filePickerForm.selectForm (AutoChild DynamicForm : null : IR)
    // Dynamic form to allow user to select filename under webroot
    // @visibility internal
    //<
    selectFormDefaults: {
        selectFileDialogDefaults: {
            actionStripControls: ["spacer:10", "pathLabel", 
                "previousFolderButton", "spacer:10", "upOneLevelButton",
                "spacer:10", "refreshButton", "spacer:2"],
            // disable file renames
            directoryListingProperties : {
                canEdit: false
            },
            title: "Select File",
            webrootOnly: true,
            width: "100%",
            showModalMask: true,
            isModal: true,
            checkFile : function (fileName) {
                return true;
            },
            loadFile : function (fileName) {
                if (this.checkFile(fileName)) {
                    var form = this.creator; 
                    if (this.currentDir.endsWith("/")) {
                        form.setValue("fileName", this.currentDir + fileName);
                    } else {
                        form.setValue("fileName", this.currentDir + "/" + fileName);
                    }
                    if (this.filePickerForm.uploadForm) {
                        this.filePickerForm.uploadForm.disable();                        
                    }
                    if (this.filePickerForm.pasteForm) {
                        this.filePickerForm.pasteForm.disable();                        
                    }
                    if (this.filePickerForm.fetchForm) {
                        this.filePickerForm.fetchForm.disable();                        
                    }
                    this.hide();   
                }
            }
        },
        
        selectFileDialogConstructor: "LoadFileDialog",
        
        fields: [
            {
                name: "fileName",
                title: "Select file from local disk",
                editorType: isc.TLinkItem || isc.LinkItem,
                target: "javascript",
                defaultValue: "select file",
                canEdit: false, 
                width: "*",
                colSpan: "*",
                click : function (form, item) {
                    var dialogProperties = isc.addProperties(
                        {
                            filePickerForm: form.creator
                        }, 
                        form.selectFileDialogDefaults, 
                        form.selectFileDialogProperties);
                    form.createAutoChild("selectFileDialog", dialogProperties).show();
                }
            }
        ]
    },
    
    selectFormConstructor: "DynamicForm",
    
    //> @attr filePickerForm.uploadForm (AutoChild DynamicForm : null : IR)
    // Dynamic form to allow user to upload file to server and use it's content
    // @visibility internal
    //<
    uploadFormDefaults: {
        fields: [
            {
                name: "file",
                title: "Upload file",
                editorType: isc.TUploadItem || isc.UploadItem,
                hoverWidth: 200,
                width: "*",
                colSpan: "*",
                startRow: true,
                itemHoverHTML : function () {
                    return "Upload file to server and proceed.";
                },
                titleHoverHTML : function () {
                    return this.itemHoverHTML()
                },
                change : function (form, item, value, oldValue) {
                    if (form.creator.selectForm) {
                        form.creator.selectForm.setDisabled(value != null);
                    }
                    if (form.creator.pasteForm) {
                        form.creator.pasteForm.setDisabled(value != null);
                    }
                    if (form.creator.fetchForm) {
                        form.creator.fetchForm.setDisabled(value != null);
                    }
                    return true;
                }
            }
        ]
    },
    
    uploadFormConstructor: "DynamicForm",

    //> @attr filePickerForm.pasteForm (AutoChild DynamicForm : null : IR)
    // Dynamic form to allow user to paste file content to text area and use this text.
    // @visibility internal
    //<
    pasteFormDefaults: {
        fields: [
            {
                name: "pasteData",
                type: "TextAreaItem",
                title: "Paste Data",
                width: "*",
                change : function (form, item, value, oldValue) {
                    var pickerForm = form.creator; 
                    if (pickerForm.selectForm) {
                        pickerForm.selectForm.setDisabled(value != null);
                    }
                    if (pickerForm.uploadForm) {
                        pickerForm.uploadForm.setDisabled(value != null);
                    }
                    if (pickerForm.fetchForm) {
                        pickerForm.fetchForm.setDisabled(value != null);
                    }
                    return true;
                }
            }
        ]
    },
    
    pasteFormConstructor: "DynamicForm",

    //> @attr filePickerForm.fetchForm (AutoChild DynamicForm : null : IR)
    // Dynamic form to allow user to provide url to file. This file will be downloaded and
    // it's content should be used.
    // @visibility internal
    //<
    fetchFormDefaults: {
        fields: [
            {
                name: "fileURL",
                type: "text",
                width: "*",
                title: "Fetch file from URL",
                startRow: true,
                change : function (form, item, value, oldValue) {
                    var pickerForm = form.creator; 
                    if (pickerForm.selectForm) {
                        pickerForm.selectForm.setDisabled(value != null);
                    }
                    if (pickerForm.uploadForm) {
                        pickerForm.uploadForm.setDisabled(value != null);
                    }
                    if (pickerForm.pasteForm) {
                        pickerForm.pasteForm.setDisabled(value != null);
                    }
                    return true;
                }
            }
        ]
    },
    
    fetchFormConstructor: "DynamicForm",
    
    //> @attr filePickerForm.orLabel (MultiAutoChild Label : null : IR)
    // Label 'OR' that inserted between other forms of filePickerForm.
    // @visibility internal
    //<
    orLabelDefaults: {
        contents: "OR",
        height: 20,
        align: "right"
    },
    
    orLabelConstructor: "Label",
    
    showOrLabel: true,
    
    //> @attr filePickerForm.pickButton (AutoChild Button : null : IR)
    // Button that initiates form submit.
    // @visibility internal
    //<    
    pickButtonDefaults: {
        name: "pickButton",
        title: "Pick",
        layoutAlign: "right",
        click :  function (form, item) {
            form.creator.saveData();
        }
    },
    
    pickButtonConstructor : "Button",
    
    //> @attr filePickerForm.useBuiltinRPC (boolean : false : IRW)
    // If enabled builtinRPC will be used instead of form submitting for selectFileForm and
    // fetchForm when pickButton is clicked.
    // @visibility internal
    //<
    useBuiltinRPC: false,
    
    initWidget : function () {
        this.Super("initWidget", arguments);
        var needOr = false;
        if (this.valuesManager == null) this.valuesManager = isc.ValuesManager.create();
        var formsData = isc.addProperties(this.formsDefaults, this.formsProperties);
        if (this.showSelectForm) {
            this.addAutoChild("selectForm", isc.addProperties({
                valuesManager: this.valuesManager }, 
                formsData)
            );
            needOr = true;
        }
        if (this.showUploadForm) {
            if (needOr && this.showOrLabel) {
                this.addMember(this.createAutoChild("orLabel", {
                    width: formsData.titleWidth
                }));    
            }
            this.addAutoChild("uploadForm", isc.addProperties({
                // the upload is done separately - don't add this form to the valuesManager
                //valuesManager: this.valuesManager
                }, formsData)
            );
            needOr = true;
        }
        if (this.showPasteForm) {
            if (needOr && this.showOrLabel) {
                this.addMember(this.createAutoChild("orLabel", {
                    width: formsData.titleWidth
                }));    
            }
            this.addAutoChild("pasteForm", isc.addProperties({
                valuesManager: this.valuesManager
                }, formsData)
            );
            needOr = true;
        }
        if (this.showFetchForm) {
            if (needOr && this.showOrLabel) {
                this.addMember(this.createAutoChild("orLabel", {
                    width: formsData.titleWidth
                }));
            }
            this.addAutoChild("fetchForm", isc.addProperties({
                valuesManager: this.valuesManager
                }, formsData)
            );
        }
        this.addAutoChild("pickButton");
    },
    
    setDataSource : function (dataSource) {
        if (this.selectForm) this.selectForm.dataSource = dataSource;
        if (this.uploadForm) this.uploadForm.dataSource = dataSource;
        if (this.pasteForm) this.pasteForm.dataSource = dataSource;
        if (this.fetchForm) this.fetchForm.dataSource = dataSource;
    },
    
    setValues : function (values) {
        this.valuesManager.setValues(values);
    },

    // because we submit forms selectively, out of band values supplied via setValues() will
    // be dropped by the client.  To enable tools like the BatchUploader to pass values other
    // than the uploaded data, we have to apply those values to each form individually, not via
    // the valuesManager as in setValues()
    setOutOfBandValues : function (values) {
        if (this.selectForm) this.selectForm.setValues(values);
        if (this.uploadForm) this.uploadForm.setValues(values);
        if (this.pasteForm) this.pasteForm.setValues(values);
        if (this.fetchForm) this.fetchForm.setValues(values);
    },
    
    getValues : function () {
        var values = isc.addProperties(this.valuesManager.getValues(), this.uploadForm.getValues());
        return values;
    },
    
    saveData : function (callback, requestProperties) {
        var pickerForm = this;
        var enableCallback = function (dsResp, data, dsReq) {
            if (pickerForm.selectForm) {
                pickerForm.selectForm.reset();
                pickerForm.selectForm.enable();
            }
            if (pickerForm.uploadForm) {
                pickerForm.uploadForm.clearValues();
                pickerForm.uploadForm.enable();
            }
            if (pickerForm.pasteForm) {
                pickerForm.pasteForm.enable();
            }
            if (pickerForm.fetchForm) {
                pickerForm.fetchForm.enable();
            }

            if (callback) pickerForm.fireCallback(callback, "dsResponse,data,dsRequest", [dsResp, data, dsReq]);
        } 
        if (this.selectForm && this.selectForm.getValue("fileName") &&
            this.selectForm.getField("fileName").defaultValue != this.selectForm.getValue("fileName"))
        {
            if (this.useBuiltinRPC) {
                isc.DMI.callBuiltin({
                    appID:"isc_builtin",
                    className:"com.isomorphic.tools.BuiltinRPC",
                    methodName: "importData",
                    arguments: [this.selectForm.getValues()],
                    callback : function (rpcResponse) {
                        enableCallback(rpcResponse, rpcResponse.data);
                    }
                });
            } else {
                this.selectForm.saveData(enableCallback, requestProperties);
            }
        }
        if (this.uploadForm && this.uploadForm.getValue("file")) {
            this.uploadForm.saveData(enableCallback, requestProperties);
        }
        if (this.pasteForm && this.pasteForm.getValue("pasteData")) {
            this.pasteForm.saveData(enableCallback, requestProperties);
        }
        if (this.fetchForm && this.fetchForm.getValue("fileURL")) {
            if (this.useBuiltinRPC) {
                isc.DMI.callBuiltin({
                    appID:"isc_builtin",
                    className:"com.isomorphic.tools.BuiltinRPC",
                    methodName: "importData",
                    arguments: [this.fetchForm.getValues()],
                    callback : function (rpcResponse) {
                        enableCallback(rpcResponse, rpcResponse.data);
                    }
                });
            } else {
                this.fetchForm.saveData(enableCallback, requestProperties);                
            }
        }
    }
});
