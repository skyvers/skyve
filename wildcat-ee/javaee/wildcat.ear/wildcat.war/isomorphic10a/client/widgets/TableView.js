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




//> @class TableView
// Shows a listing of records with one or more fields from each record, with
// built-in support for navigation and editing of lists of records.
// <p/>
// The TableView provides built-in controls such as +link{tableView.showNavigation,navigation arrows} and
// shows fields from the provided records in one of several built-in +link{type:RecordLayout}s.
// <p/>
// NOTE: This widget is intended primarily for creating handset/phone-sized interfaces
// and does not have an appearance in any skin other than Mobile.
// 
// @treeLocation Client Reference/Grids
// @visibility external
//<

isc.ClassFactory.defineClass("TableView", "ListGrid");

isc.TableView.addClassProperties({

    //> @type TableMode
    // Controls the display mode of TableView record display
    // @value  TableView.PLAIN    The default mode which displays a list of rows
    PLAIN:"plain",
    // @value  TableView.GROUPED  Grouped table is a set of rows embedded in a rounded rectangle
    GROUPED:"grouped",
    // @visibility external
    //<
     
    //> @type RecordLayout
    // Controls the style of TableView record display
    // @value  TableView.TITLE_ONLY  Show +link{tableView.titleField, title field} only
    TITLE_ONLY:"titleOnly",
    // @value  TableView.TITLE_DESCRIPTION  Show +link{tableView.titleField, title} and 
    //                                      +link{tableView.descriptionField, description} fields only
    TITLE_DESCRIPTION:"titleAndDescription",
    // @value  TableView.SUMMARY_INFO  Show +link{tableView.titleField, title}, 
    //                                      +link{tableView.descriptionField, description} and
    //                                      +link{tableView.infoField, info} fields only
    SUMMARY_INFO:"summaryInfo",
    // @value  TableView.SUMMARY_DATA  Show +link{tableView.titleField, title}, 
    //                                      +link{tableView.descriptionField, description} and
    //                                      +link{tableView.dataField, data} fields only
    SUMMARY_DATA:"summaryData",
    // @value  TableView.SUMMARY_FULL  Show +link{tableView.titleField, title}, 
    //                                      +link{tableView.descriptionField, description},
    //                                      +link{tableView.infoField, info} and
    //                                      +link{tableView.dataField, data} fields similar to the
    //                                      iPhoneOS Mail application
    SUMMARY_FULL:"summaryFull",
    // @visibility external
    //<
     
    //> @type NavigationMode
    // Controls the navigation mode of records.
    // @value  TableView.WHOLE_RECORD Clicking anywhere on the record navigates
    WHOLE_RECORD: "wholeRecord",
    // @value  TableView.NAVICON_ONLY Only clicking directly on the navigation icon
    //                                triggers navigation
    NAVICON_ONLY: "navIconOnly"
    // @visibility external
    //<

});

isc.TableView.addProperties({

    //> @attr tableView.iconField  (String : "icon" : IRW)
    // This property allows the developer to specify the icon displayed next to a record.
    // Set <code>record[tableView.iconField]</code> to the URL of the desired icon to display.
    // Only applies if +link{showIconField} is <code>true</code>.
    //
    // @visibility external
    //<
    iconField: "icon",

    //>	@attr tableView.showIconField  (Boolean : true : IRW)
    // Should an icon field be shown for each record? A column in the table is set
    // aside for an icon as specified on each record in the +link{iconField}.
    //
    // @visibility external
    //<	
    showIconField: true,
    
    //> @attr tableView.titleField  (String : "title" : IRW)
    // Field to display for an individual record as the main title.
    //
    // @visibility external
    //<
    titleField: "title",

    //> @attr tableView.infoField  (String : "info" : IRW)
    // Field to display as part of individual record in "summary" +link{recordLayout}s.
    //
    // @visibility external
    // @see recordLayout
    //<
    infoField: "info",

    //> @attr tableView.dataField  (String : "data" : IRW)
    // Field to display as part of individual record in "summary" +link{recordLayout}s.
    //
    // @visibility external
    //<
    dataField: "data",

    //> @attr tableView.descriptionField  (String : "description" : IRW)
    // Field to display as part of individual record in all +link{recordLayout}s 
    // except "titleOnly".
    //
    // @visibility external
    //<
    descriptionField: "description",

    //>	@attr tableView.recordNavigationProperty  (String : "_navigate" : IRW)
    // Boolean property on each record that controls whether navigation controls are shown for
    // that record. If property is not defined on the record a navigation icon is shown
    // if +link{showNavigation} is <code>true</code>.
    //
    // @visibility external
    //<	
    recordNavigationProperty: "_navigate",

    //> @attr tableView.tableMode  (TableMode : "plain" : IRW)
    // The display mode of the table.
    //
    // @visibility external
    //<	
    tableMode: isc.TableView.PLAIN,

    //> @attr tableView.recordLayout (RecordLayout : "titleOnly" : IRW)
    // Sets the arrangement of data fields from the record.
    // <p/>
    // Note that controls supported by the TableView itself, such as navigation icons, are
    // implicitly added to the data fields described in the RecordLayout.  If an
    // +link{iconField} has been configured, it too is an implicitly shown field, to the left
    // of the area controlled by RecordLayout.
    //
    // @visibility external
    //<
    recordLayout: isc.TableView.TITLE_ONLY,

    //> @attr tableView.navIcon (SCImgURL : "[SKINIMG]/iOS/listArrow_button.png" : IRW)
    // The navigation icon shown next to records when
    // +link{showNavigation} is true and +link{navigationMode} is set to
    // "navIconOny".
    //
    // @visibility external
    //<
    navIcon: "[SKINIMG]/iOS/listArrow_button.png",
    
    //> @attr tableView.wholeRecordNavIcon (SCImgURL : "[SKINIMG]/iOS/listArrow.png" : IRW)
    // The navigation icon shown next to records when +link{showNavigation}
    // is true and +link{navigationMode} is set to "wholeRecord".
    //
    // @visibility external
    //<
    wholeRecordNavIcon: "[SKINIMG]/iOS/listArrow.png",

    //> @attr tableView.showNavigation (boolean : null : IRW)
    // Whether to show navigation controls by default on all records.  Can also be configured
    // per-record with +link{recordNavigationProperty}.
    //
    // @visibility external
    //<
    

    //> @attr tableView.navigationMode (NavigationMode : "wholeRecord" : IRW)
    // Set navigation mode for this TableView.
    //
    // @visibility external
    //<
    navigationMode: isc.TableView.WHOLE_RECORD,

    // SKINNING --------------------------------------------------

    //> @attr tableView.recordTitleStyle (CSSStyleName : "recordTitle" : IRW)
    // Default style for title.
    // @visibility external
    //<
    recordTitleStyle: "recordTitle",

    //> @attr tableView.recordDescriptionStyle (CSSStyleName : "recordDescription" : IRW)
    // Default style for description.
    // @visibility external
    //<
    recordDescriptionStyle: "recordDescription",

    //> @attr tableView.recordDataStyle (CSSStyleName : "recordData" : IRW)
    // Default style for data field.
    // @visibility external
    //<
    recordDataStyle: "recordData",

    //> @attr tableView.recordInfoStyle (CSSStyleName : "recordInfo" : IRW)
    // Default style for info field.
    // @visibility external
    //<
    recordInfoStyle: "recordInfo",

    // COLUMN DEFINITIONS --------------------------------------------------

    iconFieldDefaults: {
        width: 50,
        imageSize: 30,
        align: "center",
        type: "image"
    },

    titleFieldDefaults: {
        name: "TVtitleField",
        width: "*",
        type: "text",
        formatCellValue : function (value, record, rowNum, colNum, grid) {
            // Defer to user-provided record formatter if provided
            if (grid.formatRecord != null) {
                return grid.formatRecord(record);
            }

            var title = grid._getFormattedFieldValue(record, grid.titleField),
                description = grid._getFormattedFieldValue(record, grid.descriptionField),
                info = grid._getFormattedFieldValue(record, grid.infoField),
                data = grid._getFormattedFieldValue(record, grid.dataField),
                html = ""
            ;
            if (grid.recordLayout == isc.TableView.SUMMARY_INFO ||
                grid.recordLayout == isc.TableView.SUMMARY_FULL)
            {
                html += "<span class='" + grid.recordInfoStyle + "'>" + 
                        info + "</span>";
            }
            html += "<span class='" + grid.recordTitleStyle + "'>" + title + "</span>";
            if (grid.recordLayout != isc.TableView.TITLE_ONLY) {
                html += "<span class='" + grid.recordDescriptionStyle + "'>" + 
                        description + "</span>";
            }
            if (grid.recordLayout == isc.TableView.SUMMARY_DATA ||
                grid.recordLayout == isc.TableView.SUMMARY_FULL)
            {
                html += "<span class='" + grid.recordDataStyle + "'>" + 
                        data + "</span>";
            }
            return html;
        }
    },

    navigationFieldDefaults: {
        name: "TVnavigationField",
        width: 54,
        align: "right",
        formatCellValue : function (value, record, rowNum, colNum, grid) {
            if (grid.getShowNavigation(record)) {
                var icon = isc.Img.create({
                    autoDraw: false,
                    autoFit: true,
                    imageType: "normal",
                    src: grid.getNavigationIcon(record)
                });
                return icon.getInnerHTML();
            }
            return grid._$nbsp;
        } 
    },

    groupByFieldDefaults: {
        // Hidden
        showIf: "false"
    },

    // DEFAULT GRID STYLING --------------------------------------------------

    // disable canAddFormulaField / canAddSummaryField
    canAddFormulaFields:false,
    canAddSummaryFields:false,
        
    showHeader: false,

    // Selection management done by tableView.recordClick handler
    selectionType: "none",
    
    skinImgDir: "images/iOS/",
    baseStyle: "tableCell",
    border: "0px",

    // don't wrap, as that will mess up the look of the trees
    wrapCells: false,
    cellHeight: 44,
    alternateRecordStyles: false,
    canCollapseGroup: false,
    groupStartOpen: "all",

    
    ignoreEmptyCriteria: false
});

isc.TableView.addMethods({
    
    initWidget : function () {
        this.Super("initWidget", arguments);

        // Holds fieldName->colNum mapping to optimize value formatting calls
        this._colIndexes = {};

        // If formatRecord was passed to us as a string, convert it to a method
        if (this.formatRecord != null && !isc.isA.Function(this.formatRecord)) 
            isc.Func.replaceWithMethod(this, "formatRecord", "record");
    },

    // Override to hide any user-defined fields and to add table view display fields
    setFields : function(newFields) {
        this.invokeSuper(isc.TableView, "setFields", this._defineTableFields(newFields));
    },

    // Hide any pre-defined fields, add table view display fields, and create
    // fields for undefined groupBy fields.
    _defineTableFields : function (baseFields) {
        var columns = baseFields || [];

        // Hide all pre-defined fields
        for (var i = 0; i < columns.length; i++) {
            columns[i].showIf = "false";
        }

        // Setup display fields

        // Icon column
        if (this.showIconField) {
            var existingColumn = columns.find(this.fieldIdProperty, this.iconField);
            if (existingColumn) columns.remove(existingColumn);

            this._iconCell = columns.length;
            columns[columns.length]
                = isc.addProperties({name: this.iconField},
                                    this.iconFieldDefaults,
                                    this.iconFieldProperties);
        }

        // Title column
        var existingColumn = columns.find(this.fieldIdProperty, this.titleFieldDefaults.name);
        if (existingColumn) columns.remove(existingColumn);

        columns[columns.length]
            = isc.addProperties({}, //{name: this.titleField},
                                this.titleFieldDefaults,
                                this.titleFieldProperties);

        // Navigation icon column
        var existingColumn = columns.find(this.fieldIdProperty, this.navigationFieldDefaults.name);
        if (existingColumn) columns.remove(existingColumn);

        this._navigateCell = columns.length;
        columns[columns.length]
            = isc.addProperties({},
                                this.navigationFieldDefaults,
                                this.navigationFieldProperties);

        if (this.groupByField) {
            // Create a hidden column for each groupBy field if not
            // already user-defined. 
            var fields;
            if (isc.isA.Array(this.groupByField)) {
                fields = this.groupByField;
            } else {
                fields = [ this.groupByField ];
            }            
            for (var i = 0; i < fields.length; i++) {
                var field = columns.find(this.fieldIdProperty, fields[i]);
                if (field) {
                    // Apply specific properties
                    isc.addProperties(field, 
                                      this.groupByFieldDefaults,
                                      this.groupByFieldProperties);
                } else {
                    // Define groupBy field
                    columns[columns.length]
                        = isc.addProperties({name: fields[i]},
                                            this.groupByFieldDefaults,
                                            this.groupByFieldProperties);
                }
            }
        }

        return columns;
    },

    // Get formatted value for a field
    _getFormattedFieldValue : function (record, fieldName) {
        var value = record[fieldName] || this._$nbsp,
            colNum = this._colIndexes[fieldName],
            undef
        ;
        if (colNum == null || colNum == undef) {
            colNum = isc.Class.getArrayItemIndex(fieldName, this.getAllFields(),
                                                 this.fieldIdProperty);
            this._colIndexes[fieldName] = colNum;
        }
        if (colNum >= 0) {
            value = this.getFormattedValue(record, fieldName, value);
        }
        return value;
    },

    //> @method tableView.getNavigationIcon
    // Icon to display as a NavigationIcon per record. Default behavior returns
    // +link{navIcon} or +link{wholeRecordNavIcon} depending on
    // +link{navigationMode} but could be overridden to customize this
    // icon on a per-record basis.
    //
    // @param record (Record) the record
    // @return (Image) the image
    //<
    getNavigationIcon : function (record) {
        return (this.navigationMode == isc.TableView.NAVICON_ONLY 
            ? this.navIcon 
            : this.wholeRecordNavIcon);
    },

    //> @method tableView.getShowNavigation
    // Whether to show navigation controls for some specific record. If the
    // +link{recordNavigationProperty, record navigation property} is set
    // on the record in question, this will be respected, otherwise will return the
    // result of the value set via +link{showNavigation}.
    //
    // @param record (Record) record to be checked for navigation state
    // @return (boolean) true if navigation controls should be shown for this record
    //<
    getShowNavigation : function (record) {
        if (record && record[this.recordNavigationProperty] != null) {
            return record[this.recordNavigationProperty];
        }
        return this.showNavigation;
    },

    recordClick : function (viewer, record, recordNum, field, fieldNum, value, rawValue) {
        if (fieldNum != this._iconCell &&
            fieldNum != this._navigateCell &&
            this.canSelectRecord(record))
        {
            this.selectSingleRecord(record);
        }

        if (fieldNum == this._navigateCell || this.navigationMode == isc.TableView.WHOLE_RECORD) {
	    if (this.recordNavigationClick) {
		// CALLBACK API:  available variables:  "record"
		// Convert a string callback to a function
		isc.Func.replaceWithMethod(this, "recordNavigationClick", "record");
                this.recordNavigationClick(record);
            }
        } else if (fieldNum == this._iconCell) {
	    if (this.imageClick) {
		// CALLBACK API:  available variables:  "record"
		// Convert a string callback to a function
		isc.Func.replaceWithMethod(this, "imageClick", "record");
                this.imageClick(record);
            }
        }
    },

    // Override grid.getBaseStyle to handle grouped styling
    getBaseStyle : function (record, rowNum, colNum) {
        if (this.isGrouped) {
            var node = this.data.get(rowNum),
                first = this.data.isFirst(node),
                last = this.data.isLast(node)
            ;
            if (first && last) {
                return (colNum == 0 ? "cellOnlyLeft"
                                    : (colNum == this.fields.length-1 ? "cellOnlyRight"
                                                                      : "cellOnly"));
            } else if (first) {
                return (colNum == 0 ? "cellTopLeft"
                                    : (colNum == this.fields.length-1 ? "cellTopRight"
                                                                      : "cellTop"));
            } else if (last) {
                return (colNum == 0 ? "cellBottomLeft"
                                    : (colNum == this.fields.length-1 ? "cellBottomRight"
                                                                      : "cellBottom"));
            }
        }
        return this.Super("getBaseStyle", arguments);
    }
});

isc.TableView.registerStringMethods({
    //> @method tableView.recordNavigationClick
    // Executed when the user clicks on a record, or on the navigate icon for a
    // record depending on +link{navigationMode}.
    //
    // @param  record (ListGridRecord)  record clicked
    // @visibility external
    //<
    recordNavigationClick : "record",

    //> @method tableView.imageClick
    // Executed when the user clicks on the image displayed in a record if
    // +link{iconField} has been specified.
    //
    // @param  record (ListGridRecord)  record clicked
    // @visibility external
    //<
    imageClick : "record",

    //> @method tableView.formatRecord()
    // Formatter to apply to record display.
    //
    // @param  record (ListGridRecord)  record to format
    // @return (HTML) formatted record contents
    // @visibility external
    //<
    formatRecord : "record"

});
