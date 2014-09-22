/*
 * Isomorphic SmartClient
 * Version v10.0p_2014-09-10 (2014-09-10)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */

 



//> @groupDef upload
// SmartClient provides special client and server-side support for file upload that allows
// uploaded files to be treated like ordinary DataSource fields.  This includes:
// <ul>
// <li>the +link{FileItem} and +link{MultiFileItem} FormItems that enable users to upload one or
// more files as a background operation, without leaving the current page
// <li>server-side support that allows binary uploads to be treated as a normal DataSource field
// values, with all other aspects of server-side integration unchanged
// <li>built-in SQL &amp; Hibernate DataSource support that can store and retrieve uploaded
// files from SQL databases
// </ul>
// The following documentation assumes you are using the SmartClient Java Server.  If you are
// not, skip to the sections near the end of this document.
// <P>
// <b>Single file upload: "binary" field type</b>
// <P>
// To use SmartClient's client-server upload system, you use a DataSource field of
// +link{dataSourceField.type,type} "binary".  By default, a DynamicForm bound to a DataSource
// with a field of type "binary" will use the +link{FileItem}, which displays a standard HTML
// &lt;input type="upload"&gt; form control.
// <P>
// When you call +link{dynamicForm.saveData()} on a DynamicForm containing a FileItem,
// SmartClient processes the save identically to a saveData() call that did not include a file
// upload:
// <ul>
// <li> if you are using the built-in SQL connectors via serverType:"sql", the file will be
// saved to SQL as described under +link{type:FieldType,field type "binary"}.
// <li> if you have server-side business logic, the inbound request may be routed to your
// business logic via RPCManager dispatch or DMI declarations as normal, your business logic
// will receive a normal DSRequest, and you are expected to provide a normal DSResponse.
// </ul>
// <P>
// Client-side callbacks, such as the callback passed to saveData(), fire normally.
// <P>
// Note that FileItems cannot be programmatically populated - this is a browser security
// restriction over which we have no control.  This restriction means that we are unable to 
// populate a FileItem with the correct filename when a form is editing an existing record.
// Also, when you call saveData() on a form that is editing a new record, the FileItem will
// be cleared on successful completion of the saveData() call; this is a side-effect of the
// form being placed into "edit" mode.  In both of these cases, the fact that the FileItem 
// has been cleared will not cause the persisted binary data to be removed by SmartClient 
// Server on subsequent calls to setData().  If the user selects another file, it will 
// overwrite the existing one; if the FileItem is left blank, the server simply ignores it.
// If you actually wish to wipe out the value of a binary field, call 
// +link{DataSource.updateData(),updateData()} on the underlying dataSource, passing an 
// explicit null value for the binary field.
// <P>
//
// <b>Restricting upload sizes</b> 
// <p> 
// The server framework includes mechanisms for setting maximum allowable file sizes. The 
// first, applied using +link{server_properties, global configuration properties}, is meant to 
// prevent an end user from uploading a file large enough to cause memory issues on the server.
// <p> 
// To configure the maximum allowed size of a single uploaded file (disabled by default), set 
// the <b>fileUpload.maxFileSize</b> property's value (in bytes): 
// <p style="text-indent: 25px">
// fileUpload.maxFileSize: 104857600 
// <p> 
// To configure the maximum combined size of all files in a single request (disabled by 
// default), set the <b>fileUpload.maxSize</b> property's value (also in bytes):  
// <p style="text-indent: 25px">
// fileUpload.maxSize: 209715200 
// <p> 
// Another configuration property controls the default value of a "binary" DataSourceField's 
// +link{dataSourceField.maxFileSize,maxFileSize} attribute, suitable for managing storage 
// requirements for a given DataSource over time (e.g., limiting images to 10MB). 
// <p style="text-indent: 25px">
// DSRequest.maxUploadFileSize: 104857600
// <p>
//
// <b>Processing File Uploads with server-side business logic</b>
// <P>
// Server-side business logic that processes file uploads may retrieve upload files via the
// server side API dsRequest.getUploadedFile(<i>fieldName</i>).  The uploaded file is returned
// as an instance of ISCFileItem, which provides access to a Java InputStream as well as
// metadata about the file (size, name).  
// See the server-side JavaDoc (com.isomorphic.*) for details.
// <P>
// <span style="color:red;font-weight:bold;">NOTE:</span> request processing engines such as
// Struts may parse the inbound request before SmartClient receives it.  If you are creating an
// RPCManager object inside of a Struts Action and the file being uploaded is not available via
// <code>dsRequest.getUploadedFile()</code>, this is likely to be the problem, and you should
// remove Struts from the processing of the upload.
// <P>
// Server-side validation errors may be provided, including validation errors for the uploaded
// file (such as too large or invalid content), and will be displayed in the form that
// attempted an upload.
// <P>
// Be aware of the following special concerns when processing file uploads:
// <ul>
// <li> if you provide your own Java Servlet or JSP that creates an instance of RPCManager in
// order process SmartClient requests, many APIs of the HttpServletRequest are not safe to call
// before you have created the RPCManager, passing in the HttpServletRequest.  These include
// getReader(), getParameter() and other commonly called methods.  This is a limitation of
// Java Servlets, not specific to SmartClient
// <li> unlike other DataSource "add" and "update" operations, you are not expected to return
// the file as part of the data returned in the DSResponse
// </ul>
// <P>
// <b>Multi file upload: MultiFileItem</b>
// <P>
// The MultiFileItem provides an interface for a user to save one or more files that are
// related to a DataSource record, where each file is represented by a record in a
// related DataSource.
// <P>
// See the +link{MultiFileItem} docs for details.
// <P>
// <b>Upload without the SmartClient Server</b>
// <P>
// If it is acceptable that the application will do a full-page reload after the upload
// completes, you can simply:
// <ul>
// <li> set +link{DynamicForm.encoding} to "multipart"
// <li> include an +link{UploadItem} to get a basic HTML upload control
// <li> set +link{DynamicForm.action} to a URL where you have deployed server-side code to
// handle the upload
// <li> call +link{dynamicForm.submitForm()} to cause the form to be submitted
// </ul>
// This cause the DynamicForm component to submit to the form.action URL like an ordinary HTML
// &lt;form&gt; element.  Many 
// +externalLink{http://www.google.com/search?q=html+file+upload+example,online tutorials}
// are available which explain how to handle HTML form file upload in various server-side
// technologies.
// <P>
// Note that when you submitForm(), the only values that will be sent to your actionURL are 
// values for which actual FormItems exist.  This differs from saveData(), in which the
// entire set of +link{dynamicForm.values,form values} are always sent.  To handle submitting
// extra values, use +link{HiddenItem}s.
// <P>
// For further details, see the +link{UploadItem} docs.
// <P>
// <b>Background upload without the SmartClient Server</b>
// <P>
// Achieving background file upload without using the SmartClient server is also possible
// although considerably more advanced.  In addition to the steps above, create a hidden
// &lt;iframe&gt; element in the page, and use +link{dynamicForm.target} to target the form
// submission at this IFRAME.  In order receive a callback notification when the upload
// completes, after processing the file upload, your server should output HTML content for the
// IFRAME that includes a &lt;SCRIPT&gt; block which will navigate out of the IFRAME (generally
// via the JavaScript global "top") and call a global method you have declared as a callback.
//
// @title Uploading Files
// @visibility external
// @treeLocation Client Reference/Forms/Form Items/FileItem
// @example upload
// @example customBinaryField
// @example multiFileItem
//<

//>	@class FileItem
//
// Binary data interface for use in DynamicForms. Allows users to select a single file for
// upload. In read-only mode (canEdit:false), can display the contents of "imageFile" fields.
//
// <P>
// <b>Editable mode</b>
// <P>
// The +link{fileItem.editForm} will be automatically generated and displayed as 
// +link{canvasItem.canvas,this.canvas}, allowing the user to select file(s) to upload.
// <P>
// See the +link{group:upload,Upload Overview} for information on using this control.
//
// <P>
// <b>Read-only mode</b>
// <P>
// For fields of type <code>"blob"</code> the raw data value will be displayed in the 
// generated +link{fileItem.displayForm}.
// <P>
// For other fields, the +link{fileItem.displayCanvas} will be displayed.
// <P>
// For <code>"imageFile"</code> fields with +link{fileItem.showFileInline, showFileInline}
// set to true, the image-file will be streamed and displayed inline within the displayCanvas.
// <P>
// Otherwise, the displayCanvas will render out +link{fileItem.viewIconSrc,View} and 
// +link{fileItem.downloadIconSrc,Download} icons and the fileName.
//
// @group upload
// @treeLocation Client Reference/Forms/Form Items
// @visibility external
//<
isc.ClassFactory.defineClass("FileItem", "CanvasItem");

isc.FileItem.addProperties({
    // we want our value to show up in the forms values object!
    shouldSaveValue:true,

    // this flag causes FormItem to set multiple="true" for the element in the DOM
    _propagateMultiple: true,
    
    //> @attr fileItem.multiple (Boolean : true : [IR])
    // When true, allow the file-selection dialog shelled by the browser to select multiple 
    // files.
    // <P>
    // Support is not full-cycle at the server - that is, there are server APIs for retrieving
    // each file that was uploaded, but no built-in support for storing multiple files against
    // a single DataSource field.  However, you can write custom server DMI code to do
    // something with the files - for instance, you could create multiple new DataSource 
    // records for each file via a server DMI like this below:
    //
    // <pre>
    //    String fileNameStr = (String)dsRequest.getValues().get("image_filename").toString();
    //
    //    String[] fileNames = fileNameStr.split(", ");
    //    List files = dsRequest.getUploadedFiles();
    //
    //    for (int i = 0; i < files.size(); i++) {
    //        ISCFileItem file = (ISCFileItem)files.get(i);
    //        InputStream fileData = file.getInputStream();
    //        DSRequest inner = new DSRequest("mediaLibrary", "add");
    //        Map values = new HashMap();
    //        values.put("title", dsRequest.getValues().get("title"));
    //        values.put("image", fileData);
    //        values.put("image_filename", fileNames[i]);
    //        values.put("image_filesize", file.getSize());
    //        values.put("image_date_created", new Date());
    //        
    //        inner.setValues(values);
    //        inner.execute();
    //    }
    //    
    //    DSResponse dsResponse = new DSResponse();
    //    
    //    dsResponse.setStatus(0);
    //
    //    return dsResponse;
    // </pre>
    //
    // @visibility external
    //< 
    multiple: true,

    //> @attr fileItem.accept (String : null : [IR])
    // A comma-separated list of valid MIME types, used as a filter for the file picker window.
    // @visibility external
    //< 

    //> @attr fileItem.showFileInline  (boolean : null : IR)
    // Indicates whether to stream the image and display it
    // inline or to display the View and Download icons.
    // 
    // @visibility external
    //<

    //> @attr fileItem.editForm (AutoChild DynamicForm : null : RA)
    // The +link{class:DynamicForm} created automatically when +link{formItem.canEdit, canEdit}
    // is true. Displays a single +link{fileItem.uploadItem, item} for manipulating a file.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>fileItem.editFormDefaults</code> and
    // <code>fileItem.editFormProperties</code>.
    // <P>
    //
    // @group upload
    // @visibility external
    //<
    editFormConstructor: "DynamicForm",
    editFormDefaults: {
        // Default the form to fill the available space
        autoDraw:false,
        // suppress redraws as much as possible - redraw == killing the item value.
        _redrawWithParent:false,
        redrawOnResize:false,
        canSubmit:true,
        getSaveOperationType:function () {
            if (this.targetItem && this.targetItem.form) 
                return this.targetItem.form.getSaveOperationType();
            return this.Super("getSaveOperationType", arguments);
        }
    },
    
    //> @attr fileItem.uploadItem (AutoChild UploadItem : null : RA)
    // The +link{class:UploadItem} created automatically and displayed in the 
    // +link{editForm} when +link{formItem.canEdit, canEdit} is true.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>fileItem.uploadItemDefaults</code> and
    // <code>fileItem.uploadItemProperties</code>.
    // <P>
    //
    // @group upload
    // @visibility external
    //<
    uploadItemConstructor: isc.TUploadItem || isc.UploadItem,
    //> @attr fileItem.displayForm (AutoChild DynamicForm : null : RA)
    // The +link{class:DynamicForm} created automatically when +link{formItem.canEdit, canEdit}
    // is false and the field is of type "blob". Displays a single 
    // +link{fileItem.displayItem, item} for viewing the content of a blob file.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>fileItem.displayFormDefaults</code> and
    // <code>fileItem.displayFormProperties</code>.
    //
    // @group upload
    // @visibility external
    //<
    displayFormConstructor: "DynamicForm",
    displayFormDefaults: {
        autoDraw:false,
        // suppress redraws as much as possible - redraw == killing the item value.
        _redrawWithParent:false,
        redrawOnResize:false,
        canSubmit:true
    },

    //> @attr fileItem.displayItem (AutoChild StaticTextItem : null : RA)
    // The +link{class:StaticTextItem} created automatically and displayed in the 
    // +link{displayForm} when +link{formItem.canEdit, canEdit} is false and the field type is
    // "blob".
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>fileItem.displayItemDefaults</code> and
    // <code>fileItem.displayItemProperties</code>. 
    // <P>
    //
    // @group upload
    // @visibility external
    //<
    displayItemConstructor: "StaticTextItem",

    //> @attr fileItem.displayCanvas (AutoChild Canvas : null : RA)
    // The +link{class:Canvas} created automatically when +link{formItem.canEdit, canEdit}
    // is false and the field is of any type other than "blob".
    // <P>
    // If the field is of type "imageFile", and +link{fileItem.showFileInline, showFileInline} 
    // is true, the contents of the canvas are set to HTML that streams the image file for 
    // display. Otherwise, the item renders icons that allow the file to be 
    // +link{fileItem.viewIconSrc,viewed} or +link{fileItem.downloadIconSrc,downloaded}.
    // <P>
    // This component is an +link{type:AutoChild} and as such may be customized via 
    // <code>fileItem.displayCanvasDefaults</code> and 
    // <code>fileItem.displayCanvasProperties</code>.
    //
    // @group upload
    // @visibility external
    //<
    displayCanvasConstructor: "Canvas",

    _$blob:"blob",
    _createCanvas : function () {
        if (!isc.isA.Canvas(this.canvas)) {
            // Save the read-only state of our canvas

            this._isReadOnly = this.isReadOnly();
            this.canvas = (this._isReadOnly ? this._createReadOnlyCanvas()
                                            : this._createEditableCanvas());
        }
        this.containerWidget.addChild(this.canvas);
    },
    
    _createReadOnlyCanvas : function () {
        var props = {},
            canvas
        ;

        if (this.type == this._$blob) {
            // A read-only blob is rendered as a StaticTextItem.
            props = isc.addProperties({width: "100%", height: 10}, this.displayFormDefaults, this.displayFormProperties,
                {
                    action:this.action,
                    targetItem:this,
                    items:[
                        isc.addProperties({}, this.displayItemDefaults, this.displayItemProperties,
                            {
                                type:"text", editorType: this.displayItemConstructor,
                                width:this.width, height:"*",
                                name:this.getFieldName(), showTitle:false
                            }
                        )
                    ]
                }
            );
            var theClass = isc.isA.Class(this.displayFormConstructor) ? 
                    this.displayFormConstructor : isc[this.displayFormConstructor];
            canvas = this.displayForm = theClass.create(props);
        } else {
            props = isc.addProperties({width: "100%", height: 10}, 
                        this.displayCanvasDefaults, this.displayCanvasProperties
            );
            var theClass = isc.isA.Class(this.displayCanvasConstructor) ? 
                    this.displayCanvasConstructor : isc[this.displayCanvasConstructor];
            canvas = this.displayCanvas = theClass.create(props);
        }
        
        return canvas;
    },

    _createEditableCanvas : function () {
        var props = isc.addProperties({}, this.editFormDefaults, this.editFormProperties,
            {
                action:this.action,
                targetItem:this,
                addOperation:this.form.addOperation,
                updateOperation:this.form.updateOperation,
                removeOperation:this.form.removeOperation,
                fetchOperation:this.form.fetchOperation,
                items:[
                    isc.addProperties({}, this.uploadItemDefaults, this.uploadItemProperties,
                        {
                            name: this.getFieldName(), 
                            showTitle: false, targetItem: this, 
                            editorType: this.uploadItemConstructor,
                            width: this.width, height: this.height, multiple: this.multiple,
                            accept: this.accept, 
                            getElementName : function () {
                                return this.getFieldName();
                            },
                            changed : function (form, item, value) {
                                this.targetItem.storeValue(value);
                            },
                            _fireStandardHandler : function () {
                                var targetItem = this.targetItem;
                                return targetItem._fireStandardHandler.apply(targetItem, arguments);
                            }
                        }
                    ),
                    // FileItems are used with the SmartClient server - _transaction item to contain
                    // details of the transaction when submitted to the server.
                    
                    {name:"_transaction", type:"HiddenItem"}
                ]
            }
        );

        var theClass = isc.isA.Class(this.editFormConstructor) ? 
                this.editFormConstructor : isc[this.editFormConstructor];
        this.editForm = theClass.create(props);
        return this.editForm;
    },

    // Update enabled/disabled state of the element to match our read-only/disabled state.
    setElementReadOnly : function (readOnly) {
        // The two states require two different canvas's therefore a redraw.
        // This override is necessary because CanvasItem avoids redraws by default.
        this.redraw();
    },

    
    redraw : function () {
        // This occurs when changing the state of canEdit.
        if (this._isReadOnly != this.isReadOnly()) {
            var value = this.getValue();
            if (this.canvas) {
                delete this.canvas.canvasItem;
                this.canvas.destroy(true);
            }
            this._isReadOnly = this.isReadOnly();
            this.setCanvas(this._isReadOnly ? this._createReadOnlyCanvas()
                                            : this._createEditableCanvas());
            this.setValue(value);
        }
        this.Super("redraw", arguments);
    },

    // support setValue() if the newValue is empty (to clear a programmatically set value)
    // and ignore setting the value to the current value
    setValue : function (newValue) {
        if (this.isReadOnly()) {
            var form = this.form,
                record = form.getValues()
            ;
            if (this.type == "blob") {
                // Update the StaticTextItem value
                this.canvas.items[0].setValue(newValue);
            } else {
                this.setCanvasContent(newValue);
            }
            
            return this.Super("setValue", arguments)
        } else {
            if (newValue == null || isc.isA.emptyString(newValue)) {
                this.canvas.items[0].setValue(newValue);
                return this.Super("setValue", arguments);
            }

            return this.canvas.items[0].setValue(newValue);
        }
    },
    
    setCanvasContent : function (data) {
        var record = this.getFormRecord();

        
        if ((this.type == "imageFile" || this.type == "viewFile") && this.showFileInline != false) {
            this.canvas.setHeight("*");
            this.canvas.setWidth("*");
            this.canvas.setContents(this.getImageHTML() || "&nbsp;");
        } else {
            if (this.showFileInline == true) { // non-imageFile field
	            this.logWarn("setValue(): Unsupported field-type for showFileInline: " +this.type);
            }
            this.canvas.setHeight(20);
            this.canvas.setWidth("*");
            this.canvas.setContents(this.getViewDownloadHTML(data, record) || "&nbsp;");
        }
    },


    setWidth : function (width) {
        if (this.canvas && !this.isReadOnly()) {
            this.canvas.items[0].setWidth(width);
        }
        this.Super("setWidth", arguments);
    },
    setHeight : function (height) {
        if (this.canvas && !this.isReadOnly()) {
            this.canvas.items[0].setHeight(height);
        }
        this.Super("setHeight", arguments);
    },

    getViewDownloadHTML : function (value, record) {

        //if (isc.isA.String(value)) return value;
        if (record == null) return null;

        var form = this.form,
            ds = form.getDataSource(),
            field = ds ? ds.getField(this.name) : null,
            filenameField = (ds ? ds.getFilenameField(this.name) : null) || this.name + "_filename",
            name = record[filenameField],
            // see if the form has a value for the pk-field
            pkFields = ds ? ds.getPrimaryKeyFieldNames() : null,
            missingPkValues = (pkFields == null)
        ;

        
        if (field && !field.filenameSuppressed && (name == null || isc.isAn.emptyString(name))) {
            return this.emptyCellValue;
        }

        if (pkFields) {
            var values = form.getValues();
            for (var i = 0; i < pkFields.length; i++) {
                var pk = pkFields[i];
                if (isc.DynamicForm._getFieldValue(pk, this, values, form, true) == null) {
                    missingPkValues = true;
                    break;
                }
            }
        }

        
        if (missingPkValues) {
            // never show view/download if the record has no PK
            return this.emptyCellValue;
        }

        return "<nobr>" + this._getViewIconSrc() + "&nbsp;" + this._getDownloadIconSrc() + 
            (name ? "&nbsp;" + name : "") + "</nobr>";
    },
    
    //> @attr fileItem.viewIconSrc (SCImgURL : "[SKIN]actions/view.png" : [IR])
    // Returns the URL for an Icon that will allow the file to be viewed.
    // @visibility external
    // @group images
    //<
 
    //> @attr fileItem.downloadIconSrc (SCImgURL : "[SKIN]actions/download.png" : [IR])
    // Returns the URL for an Icon that will allow the file to be downloaded
    // @visibility external
    // @group images
    //<
    
	//> @method fileItem._getViewIconSrc()
	//  returns HTML for an Icon that will allow the file to be viewed
	// 
	// @return (String) the HTML of the view link
	// @visibility internal
	//<
    _getViewIconSrc : function() {
        return isc.Canvas.imgHTML({
            src: this.viewIconSrc,
            width: 16,
            height: 16,
            extraCSSText: "cursor:" + isc.Canvas.HAND,
            extraStuff: " onclick='" + this.getID() + ".viewFile()'"
        });
    },
	//> @method fileItem._getDownloadIconSrc()
	//  returns HTML for an Icon that will allow the file to be downloaded
	// 
	// @return (String) the HTML of the download link
	// @visibility internal
	//<
    _getDownloadIconSrc : function() {
        return isc.Canvas.imgHTML({
            src: this.downloadIconSrc,
            width: 16,
            height: 16,
            extraCSSText: "cursor:" + isc.Canvas.HAND,
            extraStuff: " onclick='" + this.getID() + ".downloadFile()'"
        });
    },

    getFormDataSource : function () {
        // get the DS from either the parent form or it's VM
        var ds = this.form.getDataSource() || 
                (this.form.valuesManager ? this.form.valuesManager.getDataSource() : null)
        ;
        return ds;
    },

    getFormRecord : function () {
        // get the data from either the VM or the parent form
        var record = this.form.valuesManager ? this.form.valuesManager.getValues() : null;
        if (!record || isc.isAn.emptyObject(record)) record = this.form.getValues();
        return record;
    },

    getImageHTML : function () {
        var record = this.getFormRecord();
        if (!record || isc.isAn.emptyObject(record)) return;

        var field = this.form.getField(this.name),
            urlProperty = this.name + "_imgURL",
            value = record ? record[urlProperty] : null,
            ds = this.getFormDataSource()
        ;

        if (value == null && ds) {
            var dimensions = isc.Canvas.getFieldImageDimensions(field, record);

            value = record[urlProperty] = 
                isc.Canvas.imgHTML(ds.streamFile(record, field.name),
                    dimensions.width, dimensions.height);
        }

        return value;
    },

    viewFile : function () {
        isc.DS.get(this.getFormDataSource()).viewFile(this.getFormRecord(), this.name);
    },

    downloadFile : function () {
        isc.DS.get(this.getFormDataSource()).downloadFile(this.getFormRecord(), this.name);
    },

    _shouldAllowExpressions : function () {
        return false;
    }
});
