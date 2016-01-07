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
// Class will not work without the ListGrid
if (isc.ListGrid) {




//> @class MultiFileItem
// The MultiFileItem provides an interface for a user to save one or more files that are
// related to a DataSource record, where each file is represented by a record in a
// related DataSource.
// <P>
// Use MultiFileItem when a record may have one or more files associated with it (such as
// attachments to an email message) where no information about the files needs to be stored other
// than the files themselves.  If you have several fields associated with each file (such as an
// uploaded document with a version, comments and processes associated with it), consider
// instead an ordinary DataSource with on field of type "binary", and using the +link{FileItem}
// for upload.
// <P>
// See the +link{group:upload,Uploading Files} overview for more information on upload.
// <P>
// <b>DataSource Setup</B>
// <P>
// In a relationship sometimes called a "master-detail" relationship, the MultiFileItem stores
// files in a "detail" DataSource which are related to a "master" DataSource record being
// edited by the form which contains the MultiFileItem.
// <P>
// To use a MultiFileItem:
// <ul>
// <li> declare a "detail" DataSource to store the related files.  At a minimum, this
// DataSource must have:
// <ul>
// <li> a +link{dataSourceField.primaryKey,primaryKey} field
// <li> a field declaring a +link{dataSourceField.foreignKey,foreignKey} relationship to the
// primaryKey of the "master" DataSource
// <li> a field of type "binary"
// </ul>
// <li> +link{DataBoundComponent.dataSource,bind} a DynamicForm to the "master" DataSource 
// <li> in the DynamicForm bound to the "master" DataSource, declare a field with
// +link{formItem.editorType,editorType}:"MultiFileItem" and a <code>dataSource</code>
// property set to the ID of the "detail" DataSource
// </ul>
// An example "detail" DataSource for storing files is shown below.  This "detail" DataSource
// assumes a "master" DataSource with +link{DataSource.ID} "masterRecord" and with a primaryKey
// field "id".
// <pre>
// <code>
//   &lt;DataSource ID="uploadedFiles" serverType="sql"&gt;
//     &lt;fields&gt;
//        &lt;field name="fileId" type="sequence" primaryKey="true" hidden="true"/&gt;
//        &lt;field name="masterRecordId" type="number" foreignKey="masterRecord.id" hidden="true"/&gt;
//        &lt;field name="file" type="binary" title="File"/&gt;
//     &lt;/fields&gt;
//   &lt;/DataSource&gt;
// </code>
// </pre>
// <P>
// Aside from a single "binary" field, the "detail" DataSource should generally have only
// hidden fields, as shown above.  Additional internal fields (such as a "lastUpdated" field)
// may be added, but will not be editable via MultiFileItem. 
// <P>
// <b>Display</b>
// <P>
// The MultiFileItem appears as a list of files related to the current record.  An optional
// button, the +link{multiFileItem.removeButton,removeButton} allows removing files.  A
// second optional button, the +link{multiFileItem.editButton,editButton}, launches a
// picker for uploading further files.
// <P>
// <b>Saving</b>
// <P>
// In all cases, uploading a new file is an "add" DSRequest against the
// +link{multiFileItem.dataSource}.
// <P>
// The MultiFileItem has two modes, according to whether the "master" record is being newly created
// via an "add" operation or whether the master record is pre-existing ("update" operation).
// <P>
// If the master record is pre-existing, each file added by the user is uploaded as soon as the
// user exits the picker launched from the edit button, and the list of files shown in the main
// form reflects the actual list of stored files.  
// <P>
// If the master record is being newly created, files are not actually uploaded until
// <b>after</b> the master record is confirmed saved, and the list of fields shown in the main
// form reflects files which will be uploaded after the master record is saved.
// <P>
// In both cases, if there are multiple files to upload, they are uploaded one at a time, as a
// series of separate "add" DSRequests against the +link{multiFileItem.dataSource}.
// <P>
// Also in both cases, deletion of any file is immediate.  In the case of a pre-existing master
// record, all files shown actually exist as DataSource records, and deletion is performed as a
// "remove" DSRequest against the +link{multiFileItem.dataSource}.  
//
// @group upload
// @treeLocation Client Reference/Forms/Form Items
// @visibility external
//<
isc.ClassFactory.defineClass("MultiFileItem", "RelationItem");
isc.MultiFileItem.addProperties({

    //> @attr multiFileItem.dataSource (DataSource or ID : null : IR)
    // DataSource where files are stored.  
    // <P>
    // This DataSource is expected to have a field of type "binary" as well as a primaryKey and
    // foreignKey declaration to some other DataSource; see the +link{MultiFileItem} for an
    // overview.
    // <P>
    // This DataSource need only be capable of "fetch", "add" and "remove" - "update" is unused.
    //
    // @visibility external
    //<
 
    // NOTE: show flags for edit/removeButton set default true in superclass
   
    //> @attr multiFileItem.removeButton (AutoChild StatefulCanvas : null : IR)
    // Button for removing files.  Supports the properties of a +link{FormItemIcon}.
    //
    // @visibility external
    //<

    //> @attr multiFileItem.editButton (AutoChild StatefulCanvas : null : IR)
    // Button for launching a picker to add new files for upload.  Supports the properties of a
    // +link{FormItemIcon}.
    //
    // @visibility external
    //<

    //> @attr multiFileItem.pickerConstructor (SCClassName : "MultiFilePicker" : IR)
    // MultiFileItems use a +link{MultiFilePicker} instance as their picker.
    // The generated <code>picker</code> autoChild may be customized via the standard 
    // +link{AutoChild} pattern.
    // @visibility external
    //<
    pickerConstructor: "MultiFilePicker",

    canvasDefaults : {
       
        showHeader:false,
      
        canHover: true,
        cellHoverHTML : function (record, rowNum, colNum) {
            if (this.canvasItem.form.saveOperationIsAdd()) {
                var value = record[this.getFieldName(colNum)];
                if (value && this.displayShortName) {
                    value = value.replace(/^.*[\\\/]/, isc.emptyString)
                }
                return value;
            }
        },
        getCellValue : function (value, rowNum, colNum) {
            var value = this.Super("getCellValue", arguments);
            if (this.canvasItem.form.saveOperationIsAdd()) {
                if (!this.displayShortName || !value) return value;    
                value = value.replace(/.*(\\|\/)/g, isc.emptyString);
            }
            return value;
        }
    },
    
    // Override getDynamicDefaults to pick up dynamic defaults for the ListGrid based on
    // this item's direct settings
    getDynamicDefaults : function (childName) {
        if (childName == "canvas") {
            var defaults = {};
            if (this.emptyMessage != null) defaults.emptyMessage = this.emptyMessage;
            if (this.displayShortName != null) defaults.displayShortName = this.displayShortName;
            return defaults;
        }
        return this.Super("getDynamicDefaults", arguments);
        
    },
    
    //> @attr multiFileItem.emptyMessage (String : "Click icon to add..." : IR)
    // Empty message to display when there are no files listed.
    // @visibility external
    // @group i18nMessages
    //<
    emptyMessage:"Click icon to add...",

    //> @attr multiFileItem.editButtonPrompt (String : "Add files" : IR)
    // The +link{FormItemIcon.prompt,prompt} of the +link{editButton,edit button}.
    // @group i18nMessages
    // @visibility external
    //<
    editButtonPrompt: "Add files",

    //> @attr multiFileItem.removeButtonPrompt (String : "Remove selected files" : IR)
    // The +link{FormItemIcon.prompt,prompt} of the +link{removeButton,remove button}.
    // @group i18nMessages
    // @visibility external
    //<
    removeButtonPrompt: "Remove selected files",

    //> @attr multiFileItem.pickerUploadButtonInitialTitle (String : "OK" : IR)
    // The initial title of the upload button in the picker lauched by the
    // +link{editButton,edit button} that is used before the form is saved.
    // @see pickerUploadButtonTitle
    // @group i18nMessages
    // @visibility external
    //<
    pickerUploadButtonInitialTitle: "OK",

    //> @attr multiFileItem.pickerUploadButtonTitle (String : "Save" : IR)
    // The title of the upload button in the picker lauched by the
    // +link{editButton,edit button} that is used after the form is saved.
    // @see pickerUploadButtonInitialTitle
    // @group i18nMessages
    // @visibility external
    //<
    pickerUploadButtonTitle: "Save",

    //> @attr multiFileItem.pickerCancelButtonTitle (String : "Cancel" : IR)
    // The title of the cancel button in the picker lauched by the
    // +link{editButton,edit button}.
    // @group i18nMessages
    // @visibility external
    //<
    pickerCancelButtonTitle: "Cancel",

    //> @attr multiFileItem.pickerAddAnotherFileButtonTitle (HTMLString : "<u>Add&nbsp;another</u>" : IR)
    // The contents of the "Add another" file button in the picker launched by the
    // +link{editButton,edit button}.
    // @group i18nMessages
    // @visibility external
    //<
    pickerAddAnotherFileButtonTitle: "<u>Add&nbsp;another</u>",

    //> @attr multiFileItem.pickerUploadProgressLabel (HTMLString : "Saving ${fileName} ${formattedFileSize}" : IR)
    // Specifies the label of the progress meter in the picker lauched by the
    // +link{editButton,edit button}.  This property is a dynamic string, similar to the
    // +link{Canvas.dynamicContents} feature, with the variables <code>fileName</code> and
    // <code>formattedFileSize</code>.
    // @group i18nMessages
    // @visibility external
    //<
    pickerUploadProgressLabel: "Saving ${fileName} ${formattedFileSize}",

    //> @attr multiFileItem.displayShortName	(boolean : true : IR)
    //
    // If true, just the filename (without the path) is shown in the list of files.  If
    // false, the full path including the filename is shown.
    //
    // @visibility enternal
    //<		
    displayShortName: true,

    iconWidth:16,
    iconHeight:16,
    
    editButtonDefaults : isc.addProperties({},isc.RelationItem.getInstanceProperty('editButtonDefaults'),{
        src: "[SKIN]MultiUploadItem/icon_add_files.png",
        showOver:false
    }),
    
    // RelationItem calls this method from click on the edit item to determine whether
    // the picker should be modal.
    // Always show the picker modal - user can dismiss via save or cancel click, which
    // is a cleaner UI than auto dismissing (cancelling) on outside click
    
    showPickerModal : function () {
        return true;
    },
    
    removeButtonDefaults : isc.addProperties({},isc.RelationItem.getInstanceProperty('removeButtonDefaults'), {
        src: "[SKIN]MultiUploadItem/icon_remove_files.png",
        showOver:false
    }),

    canEditWithNoMasterRecord: true
});

//!>Deferred
isc.MultiFileItem.addMethods({

init : function () {
    // Override the edit and remove button contents with the user-specified prompt.
    this.editButtonDefaults = isc.addDefaults({
        prompt: this.editButtonPrompt
    }, this.editButtonDefaults);
    this.removeButtonDefaults = isc.addDefaults({
        prompt: this.removeButtonPrompt
    }, this.removeButtonDefaults);

    this.pickerProperties = isc.addProperties({}, this.pickerProperties, {
        uploadWithoutPKButtonName: this.pickerUploadButtonInitialTitle,
        uploadWithPKButtonName: this.pickerUploadButtonTitle,
        cancelButtonName: this.pickerCancelButtonTitle,

        addAnotherFileButtonProperties: {
            contents: this.pickerAddAnotherFileButtonTitle
        },

        progressMeterProperties: {
            progressLabelProperties: {
                // Override the dynamic contents of the progress label canvas to allow it to be
                // customized without exposing the internal class MultiFileProgressMeter.
                dynamicContents: false,
                dynamicContentVars: null,
                contents: null,
                getContents : function () {
                    var progressMeter = this.creator,
                        multiFileItem = progressMeter.creator.creator,
                        contents = multiFileItem.pickerUploadProgressLabel,
                        result = contents.evalDynamicString(multiFileItem, {
                            fileName: progressMeter.fileName,
                            formattedFileSize: progressMeter.getFormattedFileSize()
                        })
                    ;
                    return "<b><nobr>" + result + "</nobr></b>";
                },
                show : function () {
                    this.setContents(this.getContents());
                    this.Super("show", arguments);
                }
            }
        }
    });

    return this.Super("init", arguments);
},

removeSelectedData : function () {
    // if we're editing an existing record on the server just call removeSelectedData on the
    // canvas (listGrid) to clear the value from the dataSource
    var form = this.getValuesComponent();
    if (!form.saveOperationIsAdd()) return this.Super("removeSelectedData", arguments);

    // remove the field from the canvas and also the editor by calling the removeUploadField method
    // on the editor which will automatically update the canvas
    var selection = this.canvas.getSelection();
    // form is unset on the selection...
    var selectionLength = selection.length;
    for (var i = 0; i < selectionLength; ++i) {
        this.picker.removeUploadField(selection[i]._form, true);
    }
},


// notification fired when the form completes a save.  Allows RelationItems (including MUI) to
// save related records after the main record has been saved such that a PK is definitely
// available.
formSaved : function (request, response, data) {
    // if we're using a picker, show it in modal mode for the duration of the save so that any
    // validation errors are immediately visible
    if (this.picker) {
        this.showPicker(true, this.editButton);
        var form = this.getValuesComponent();
        this.picker.setForeignKeyValues(
            this.getDataSource().getForeignKeysByRelation(data, form.dataSource)
        );
        this.picker.saveData(this.getID()+".saveDataCallback()");
        // cancel further rpc callback processing until we're done saving
        return false;    
    } else {
        this.saveDataCallback();
    }
},

saveDataCallback : function () {
    if (this.picker) this.picker.hide();    
    
    this.getValuesComponent().formSavedComplete();
    if (this.picker && isc.ResultSet && !isc.isA.ResultSet(this.canvas.data)) {
        this.canvas.setData([]);
        this.canvas.fetchData(this.picker.foreignKeyValues);
    }
},

// called when the data changes in the picker
pickerDataChanged : function (picker) {
    // if this is going to be an update, then we're showing existing records in the canvasItem and
    // the user must save directly from the picker - so do nothing
    if (!this.getValuesComponent().saveOperationIsAdd()) return;

    // add operation - we're showing filenames selected so far in the canvasItem - update the
    // list. When the user saves the form with the CanvasItem on it we'll have to commit.
    this.canvas.setData(this.picker.getData());
},

destroy : function () {
    // FIXME we can't clear the value in an upload field, so we have to blow those away, but we
    // could recycle everything else
    if (this.picker) this.picker.destroy(); 
    this.Super("destroy", arguments);
}

});
//!<Deferred

//> @class MultiFilePicker
// The MultiFilePicker is a pop-up picker used by the +link{MultiFileItem} to allow the user to
// enter several files for upload.
// 
// @group upload
// @treeLocation Client Reference/Forms/Form Items/MultiFileItem
// @visibility external
//<

// Manages a stack of upload fields where the user can add and remove fields via buttons.

isc.defineClass("MultiFilePicker", "VStack").addProperties({
    height: 1,

    layoutMargin:10,
    
    styleName: "windowBackground",

    // NOTE: next two properties are basically cosmetic - they do not allow you to limit the
    // maximum number of uploaded
    
    //> @attr multiFilePicker.minUploadFields		(integer : 1 : [IRW])
    //
    // Minimum number of upload fields to show. This many fields will show up initially.
    //
    // @visibility external 
    //<		
    minUploadFields: 1,

    //> @attr multiFilePicker.maxUploadFields		(integer : null : [IRW])
    //
    // The maximum number of upload fields to show.  If not specified, user can add as many
    // upload fields as he wishes.
    //
    // @visibility external 
    //<		

    // min file size in bytes at which we show the progress bar (~200k)
    minFileSizeForProgressBar: 204800,

    // in milliseconds, how frequently we ask the server for the number of bytes uploaded so far.
    progressCheckFrequency: 1000,

    progressMeterConstructor: "MultiFileProgressMeter",

    uploadLayoutConstructor: "VStack",

    uploadWithPKButtonName: "Save",
    uploadWithoutPKButtonName: "OK",
    cancelButtonName: "Cancel",

    showUploadRemoveButton: true,
    uploadWithoutPK: false
});

//!>Deferred
isc.MultiFilePicker.addMethods({

addAnotherFileButtonDefaults: {
    canFocus:true,
    contents: "<u>Add&nbsp;another</u>"
},

creatorName:"picker", 
initWidget : function () {
    this.Super("initWidget", arguments);
    
    this.addAutoChild("uploadLayout", {height: 1});
    this.addAutoChild("addAnotherFileButton", {
        width: 75,
        height: 20,
        align: "left",
        cursor: isc.Canvas.HAND,
        icon: "[SKIN]MultiUploadItem/icon_add_files.png",
        click : "this.picker.addUploadField()"
    }, "Label");

    this.addAutoChild("toolbar", {
        width: 1,
        height: 1,
        membersMargin: 10,
        layoutMargin: 10,
        layoutAlign: "right"
    }, "HStack");

    this.addAutoChild("saveButton", {
        title: this.hasKeys() ? this.uploadWithPKButtonName: this.uploadWithoutPKButtonName,
        width: 80,
        updateTitle : function () {
            var picker = this.parentElement.picker;
            var newTitle = picker.hasKeys() ? picker.uploadWithPKButtonName : picker.uploadWithoutPKButtonName;
            if (newTitle != this.title) this.setTitle(newTitle);
        },                    
        click : function () {
            var picker = this.parentElement.picker;
            if (!picker.hasKeys() && !picker.uploadWithoutPK) {
                // 
                // this flow will just show the record in the main listGrid but not actually
                // save anything to server - assumption is that save would occur on 'saveData' on
                // the form
                picker.hide();
                picker.dataChanged();
            } else {
                // This will actually directly submit the form, storing the files on the existing
                // record
                picker.saveData();
            }
        },
        observes : [{source: this, message:"setForeignKeyValues", action: "observer.updateTitle()"}]
    }, "IButton", this.toolbar);

    this.addAutoChild("cancelButton", {
        title: this.cancelButtonName, 
        width: 80,
        // user can hit the cancel button during validation error from server.  do a saveData() to
        // complete callback flow        
        click : function () {
            this.picker.hide();
            
            if (this.picker.creator.form.saveOperationIsAdd()) this.picker.revertData();
        }
    }, "IButton", this.toolbar);

    this.clearData();
},

hasKeys : function () {
    return (this.foreignKeyValues && !isc.isAn.emptyObject(this.foreignKeyValues));
},

setForeignKeyValues : function (foreignKeyValues) {
    this.foreignKeyValues = foreignKeyValues;
},

revertData : function () {
    this.clearData(true);
},

clearData : function (revertOnly) {
    var forms = this.getForms(),
        formCount = 0
    ;
    for (var i = forms.length-1; i >=0; i--) {
        var remove = false;
        // if we're reverting, just remove items that are newly added
        if (revertOnly) remove = (forms[i].newlyAdded == true);
        else remove = true;

        // remove forms with no values (empty upload fields)
        if (isc.isAn.emptyObject(forms[i].getValues())) remove = true;

        if (remove) this.removeUploadField(forms[i]);
        else formCount++;
    }
    // preallocate the desired number of file upload fields
    for (var i = formCount; i < this.minUploadFields; i++) this.addUploadField();
},

// add another upload form, called via button
addUploadField : function () {
    
    var binaryField;

        if(this.dataSource==null) {
            isc.logWarn("A datasource must be defined on a MultiFileItem ("+this.id+") or it will both not function and elements will not render properly.");
        } else {
            var ds = isc.DataSource.get(this.dataSource),
                fields = ds.getFields();
            for (var key in fields) {
                if (fields[key].type == "binary" || fields[key].type == "imageFile") {
                    binaryField = key;
                    break;
                }
            }
            if (binaryField == null) {
                isc.logWarn("The datasource defined on a MultiFileItem ("+this.id+") must define a binary field");
            }
        }
        
        
        var form = this.createAutoChild("uploadForm", {
            dataSource: this.dataSource,
            cellPadding: 0,
            numCols: 2,
            colWidths: ['*','*'],
            width: 250,
            newlyAdded: true,
            elementChanged : function () {
                this.Super("elementChanged", arguments);
                this.picker.dataChanged();
            },
            fields : [
                {name: binaryField, multiple: false}
            ]
        }, isc.DynamicForm);

    var uploadFormLayout = this.createAutoChild("uploadFormLayout", {
            members: [form],
            height: 21
        }, isc.HLayout);

        this.uploadLayout.addMember(uploadFormLayout);
        if (this.showUploadRemoveButton) {
            var removeButton = this.createAutoChild("uploadRemoveButton", {
                form: form,
                picker: this,
                contents: isc.emptyString,
                cursor: isc.Canvas.HAND,
                icon: "[SKIN]MultiUploadItem/icon_remove_files.png",
        //        title: "Remove",
                click: "this.picker.removeUploadField(this.form, true)",
                iconSpacing : 6,
                width: 22, // 16px icon + 6px icon spacing
                height: 20
            }, isc.Label);
            uploadFormLayout.addMember(removeButton, 0);
        }

    if (this.maxUploadFields && this.maxUploadFields <= this.uploadLayout.getMembers().length)
        this.addAnotherFileButton.hide();
},

// remove one of the uploaded fields, called from button
removeUploadField : function (form, reAddToMin) {
        
    if (form._queueNum) {
        isc.rpc.cancelQueue(form._queueNum);
        this.transactionNum = null;
        if(this.progressMeter) this.progressMeter.hide();
    }

        if (form.parentElement) form.parentElement.destroy();
        if (reAddToMin && this.uploadLayout.getMembers().length < this.minUploadFields) {
            this.addUploadField();
        }

        if (this.maxUploadFields && this.maxUploadFields > this.uploadLayout.getMembers().length) {
            this.addAnotherFileButton.show();
        }

    this.dataChanged();
},

// get all the forms in the editor
getForms : function () {
    return this.uploadLayout.getMembers().map("getMember", this.showUploadRemoveButton ? 1 : 0);
},

// observable
dataChanged : function () {

},

show : function () {
    var forms = this.getForms();
    if (forms) forms.setProperty("newlyAdded", false);
    this.Super("show", arguments);
},

hide : function () {
    this.Super("hide", arguments);
    this.hideClickMask();
},

// get data for display in the list of files
getData : function () {
    var data = [];
    var forms = this.getForms();
    for (var i = 0; i < forms.length; i++) {
        var form = forms[i];
        if (!form) continue;
        var values = form.getValues();

        // skip forms with no values (empty upload fields)
        if (isc.isAn.emptyObject(values)) continue;
        
        // store a pointer back to the form on the values object
        values._form = form;
        data[data.length] = values;
    }
    return data;
},

// save the first non-empty upload form.  We upload each file serially, waiting for a server
// response before proceeding to the next file by calling this method again.
// Note that we could upload a maximum of two files at once (based on the HTTP 1.1 limit of 2
// connections per server), but that would prevent any other kind of traffic, eg, progress
// checks.
saveData : function (callback) {
    if (!callback) callback = this.saveCallback;
    if (!callback) callback = this.getID()+".hide()";
    this.saveCallback = callback;
    
    var forms = this.getForms();

    // client or server validation error may have occurred, but we have the master record, so let
    // the user correct the problem and save.
    this.saveButton.setTitle(this.uploadWithPKButtonName);
    
    // find the first form with values
    // remove all forms with no values
    var form;
    var fileName;
    while (forms.length > 0) {
        form = forms[0];
        fileName = form.getFields()[0].getValue();
        if (!fileName) {
            this.removeUploadField(form);
            forms.remove(form);
        }
        else break;
    }

    if (forms.length == 0) {
        // done saving forms
        if (this.progressMeter) {
            this.progressMeter.hide();
        }

        this.transactionNum = null;
        delete this.saveCallback;
        this.fireCallback(callback);
        this.clearData();
        return;
    }

    if (!form.validate()) return;
    
    if (!this.progressMeter) {
        this.progressMeter = this.createAutoChild("progressMeter", { 
            progressCheckFrequency : this.progressCheckFrequency
        });
        this.addMember(this.progressMeter, 0);
    }

    fileName = fileName.replace(/.*(\\|\/)/g, isc.emptyString);
    this.progressMeter.setFileName(fileName);
    this.progressMeter.hideProgressBar();
    this.progressMeter.show();

    if (this.hasKeys()) {
        for (var key in this.foreignKeyValues) form.setValue(key, this.foreignKeyValues[key]);
    }
    var dsRequest = form.saveData(this.getID()+".saveDataCallback(dsRequest, dsResponse, data)",
                         {
                             params: {formID : form.getID()},
                             // we want to be notified of an error so we can kill the progressCheck thread
                             willHandleError: true,
                             form: form,
                             showPrompt: false,
                             saveDataCallback: callback,
                             timeout: 0
                         }
    );
    this.transactionNum = dsRequest.transactionNum;
    form._queueNum = this.transactionNum;
    this.progressCheck(form.getID(), this.transactionNum);
},

// called when we get a server response to an upload attempt.  If it succeeded, proceeds to
// upload next file.
saveDataCallback : function (dsRequest, dsResponse, data) {
    var form = dsRequest.form;

    // if there was an error, kill the progressCheck thread
    if (dsResponse.status != isc.RPCResponse.STATUS_SUCCESS) {
        this.progressMeter.hide();
        this.transactionNum = null;
        if (dsResponse.status == isc.RPCResponse.STATUS_VALIDATION_ERROR) {
            form.setErrors(dsResponse.errors, true);
        } else {
            isc.warn(data);
        }
        // form didn't save - bail out and let the user correct any errors
        return;
    }
     
    // the form saved successfully - remove it from the list
    form._queueNum = null; // don't cancelQueue()
    this.removeUploadField(form);

    // save the next form
    this.saveData(dsRequest.saveDataCallback);

    if (this.callingFormItem.fileUploaded) {
        this.callingFormItem.fileUploaded(dsRequest, dsResponse);
    }
},

// check the progress of a file upload via a DMI performed in parallel to the upload itself.
// Calling progressCheck sets up a periodic polling of the server until all uploads complete
progressCheck : function (formID, origTransactionNum) {
    this.lastProgressCheckTime = new Date().getTime();
    isc.DMI.callBuiltin({
        methodName: "uploadProgressCheck", 
        callback: this.getID()+".progressCallback(rpcRequest, rpcResponse, data, "+origTransactionNum+")",
        arguments: formID,
        requestParams : {willHandleError: true, showPrompt: false, formID: formID}
    });
},

progressCallback : function (rpcRequest, rpcResponse, data, origTransactionNum) {
    // delayed progess callback - upload may have completed
    var formID = rpcRequest.formID;
    var form = window[formID];
    
    if (!form || this.transactionNum !== origTransactionNum) return;

    // assume transient failure - retry request immediately (single server failure in a server farm
    // is a common reason for this)
    if (rpcResponse.status != isc.RPCResponse.STATUS_SUCCESS) this.progressCheck(formID);

    // server reports validation errors - typically file too large.
    if (data.errors) {
        isc.rpc.cancelQueue(this.transactionNum);
        form.setErrors(data.errors, true);
        this.saveButton.show();
        this.transactionNum = null;
        this.progressMeter.hide();
        return;    
    }
        
    this.progressMeter.setFileSize(data.totalBytes);

    // if the total size of the file is less than the configured amount for which we would show
    // a progress bar, just return - no further requests will be made for this file.
    if (data.totalBytes < this.minFileSizeForProgressBar) {
        this.progressMeter.hideProgressBar();
        return;
    }


    // file is large enough for us to show a progress meter.  Show it and schedule the next progressCheck
    this.progressMeter.setBytesReceived(data.bytesSoFar);
    this.progressMeter.showProgressBar();
    this.progressMeter.setPercentDone(100 * data.bytesSoFar / data.totalBytes);

    var checkDelay = this.progressCheckFrequency - (new Date().getTime() - this.lastProgressCheckTime);
    if (checkDelay < 0) checkDelay = 0;
    this.delayCall("progressCheck", [formID, origTransactionNum], checkDelay);
}

});
//!<Deferred

// progress meter for reporting on progress of uploaded files
isc.defineClass("MultiFileProgressMeter", "VStack").addClassMethods({

formatBytes : function (fileSize) {
    var suffix;
    if (fileSize < 1024) {
        fileSize = Math.round(fileSize/1024);
        suffix = "B";
    } else if(fileSize < (1024*1024)) {
        fileSize = Math.round(fileSize/1024);
        suffix = "KB";
    } else {
        fileSize = Math.round(fileSize/(1024*1024)*100)/100;
        suffix = "MB";
    }

    return fileSize+"&nbsp;"+suffix;
}

});
isc.MultiFileProgressMeter.addProperties({
    height: 50
});

//!>Deferred
isc.MultiFileProgressMeter.addMethods({

initWidget : function () {
    this.Super("initWidget", arguments);

    this.progressLabelProperties = isc.addDefaults(this.progressLabelProperties, {
        dynamicContentsVars: { progressMeter: this },
        dynamicContents: true,
        contents: "<b><nobr>Saving ${progressMeter.fileName} ${progressMeter.getFormattedFileSize()}</nobr></b>"
    });

    this.addAutoChild("progressLabel", {
        height: 1
    }, "Canvas");
},

setFileName : function (fileName) {
    this.fileName = fileName;
    delete this.fileSize;
    this.bytesSoFar = 0;
    this.bytesReceived = 0;
    this.progressLabel.markForRedraw();
    if (this.progressBar) this.setPercentDone(0);
},

setFileSize : function (fileSize) {
    this.fileSize = fileSize;
    this.progressLabel.markForRedraw();
},

setBytesReceived : function (bytesReceived) {
    this.bytesSoFar = this.bytesReceived;;
    this.bytesReceived = bytesReceived;
    this.progressLabel.markForRedraw();
},

getFormattedFileSize : function () {
    if (!this.fileSize) return isc.emptyString;

    var result = "<br>";
    if (this.bytesReceived) {
        result += isc.MultiFileProgressMeter.formatBytes(this.bytesReceived) + " of ";
    }
    result += isc.MultiFileProgressMeter.formatBytes(this.fileSize);
    if (this.bytesSoFar && this.progressCheckFrequency) {
        var delta = this.bytesReceived - this.bytesSoFar;
        delta = isc.MultiFileProgressMeter.formatBytes(delta * 1000 / this.progressCheckFrequency);             
        result += " (" + delta + "/sec)";
    }
    return result;
},

showProgressBar : function () { 
    this.addAutoChild("progressBar", {
        overflow: "visible"
    }, "Progressbar");
    this.progressBar.show();
},

hideProgressBar : function () {
    if (this.progressBar) this.progressBar.hide();
},

setPercentDone : function (percentDone) {
    this.progressBar.setPercentDone(percentDone);
}

});

//!<Deferred

}

// Make old name available as a synonym
isc.addGlobal("MultiUploadItem", isc.MultiFileItem);
isc.addGlobal("MultiUploadPicker", isc.MultiFilePicker);
