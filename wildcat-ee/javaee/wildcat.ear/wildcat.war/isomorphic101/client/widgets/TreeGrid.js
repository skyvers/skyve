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
//>	@class	TreeGrid
//
// The SmartClient system supports hierarchical data (also referred to as tree data
// due to its "branching" organization) with:
// <ul>
//   <li> the +link{class:Tree} class, which manipulates hierarchical data sets
//   <li> the TreeGrid widget class, which extends the ListGrid class to visually
//        present tree data in an expandable/collapsible format.
// </ul>
// For information on DataBinding Trees, see +link{group:treeDataBinding}.
// <p>
// A TreeGrid works just like a +link{ListGrid}, except one column (specified by
// +link{TreeGridField.treeField}) shows a hierarchical +link{Tree}.  A TreeGrid is not limited
// to displaying just the +link{Tree} column - you can define additional columns (via
// +link{TreeGrid.fields}) which will render just like the columns of a +link{ListGrid}, and
// support all of the functionality of ListGrid columns, such as
// +link{listGridField.formatCellValue,formatters}.
// <p>
// Except where explicitly overridden, +link{ListGrid} methods, callbacks, and properties
// apply to TreeGrids as well.  The +link{ListGrid} defines some methods as taking/returning
// +link{ListGridField} and +link{ListGridRecord}.  When using those methods in a TreeGrid,
// those types will be +link{TreeGridField} and +link{TreeNode}, respectively.
// 
// @implements DataBoundComponent    
// @treeLocation Client Reference/Grids
// @visibility external
//<

// define us as a subclass of the ListGrid
isc.ClassFactory.defineClass("TreeGrid", "ListGrid");

// Synonym for backCompat.  NOTE: define an alias rather than make a subclass, otherwise,
// attempts to skin the class using the old name would only affect the subclass!
isc.addGlobal("TreeViewer", isc.TreeGrid);

//>	@class	TreeGridBody
//
//  GridRenderer displayed as the body of a TreeGrid. Includes various overrides for
//  specialized event handling and display.
//
//  @treeLocation Client Reference/Grids/TreeGrid/
//  @visibility internal
//<
isc.defineClass("TreeGridBody", isc.GridBody).addProperties({
    // Override the internal _updateCellStyle() method to style the tree-field's internal
    // table (without regening the HTML)
    _$TABLE:"TABLE",
    _$zeroBorderPadding:"padding:0px;border:0px;",


    _getShowClippedValuesOnHover : function () {
        if (this.showClippedValuesOnHover == null) return true;
        return this.showClippedValuesOnHover;
    },

    cellValueIsClipped : function (rowNum, colNum, c, d, e) {
        var grid = this.grid,
            treeFieldNum = grid._treeFieldNum,
            treeFieldBody = grid.getFieldBody(treeFieldNum);
        if (this === treeFieldBody && colNum == grid.getLocalFieldNum(treeFieldNum)) {
            var cell = this.getTableElement(rowNum, colNum);
            if (cell == null) return false;
            var table = cell.firstChild; 
            while (table && table.tagName != this._$TABLE) table = table.firstChild;
            if (table && table.rows && table.rows[0]) {
                var titleCell = table.rows[0].lastChild,
                    titleClipperHandle = titleCell;
                
                if (isc.Browser.isMoz && titleCell.firstChild && titleCell.firstChild.hasAttribute &&
                    titleCell.firstChild.hasAttribute("_titleClipper"))
                {
                    titleClipperHandle = titleCell.firstChild;
                }
                return this._cellValueIsClipped(titleClipperHandle);
            }
        }
        return this.invokeSuper(isc.TreeGridBody, "cellValueIsClipped", rowNum, colNum, c, d, e);
    },

    defaultCellValueHoverHTML : function (record, rowNum, colNum, d, e, f) {
        var grid = this.grid,
            treeFieldNum = grid._treeFieldNum,
            treeFieldBody = grid.getFieldBody(treeFieldNum);
        if (this === treeFieldBody && colNum == grid.getLocalFieldNum(treeFieldNum)) {
            // Call the standard ListGrid.getCellValue() method to give us the formatted title
            // of the cell being dragged, excluding the TreeGrid folder/file icons, etc.
            return this.grid.invokeSuper(isc.TreeGrid, "getCellValue", record, rowNum, treeFieldNum);
        }
        return this.invokeSuper(isc.TreeGridBody, "defaultCellValueHoverHTML", record, rowNum, colNum, d, e, f);
    },

    _updateCellStyle : function (record, rowNum, colNum, cell, className) {
        if (cell == null) cell = this.getTableElement(rowNum, colNum);
        if (cell == null) return; // cell not currently drawn

        if (!this.showHiliteInCells && 
            colNum == this.grid.getLocalFieldNum(this.grid.getTreeFieldNum())) 
        {
            if (record == null) record = this.getCellRecord(rowNum, colNum);
            // determine the CSS style className if not provided
            if (className == null) className = this.getCellStyle(record, rowNum, colNum);
            // There will be a clip-div around the table.
            // In IE the NOBR also counts as a node.
            var table = cell.firstChild; 
            while (table && table.tagName != this._$TABLE) table = table.firstChild;
            if (table) {

                var customCSSText;

                // Apply custom css text from getCellCSSText to the table element and any cells.
                // Note: this is not required in most cases - we write out no-style-doubling css
                // on these elements so we'll ignore bg color, images, border etc. 
                // This is really just required to pick up changes to the text color / weight etc
                if (this.getCellCSSText) {
                    customCSSText = this.getCellCSSText(record, rowNum, colNum);
                    if (customCSSText != null && !isc.isAn.emptyString(customCSSText)) {
                        // we always append no-style-doubling css to the custom css to avoid
                        // doubled borders etc
                        
                        customCSSText += isc.Canvas._$noStyleDoublingCSS;
                    } else customCSSText = null;
                }

                table.className = className;
                if (customCSSText != null) table.cssText = customCSSText;

                var innerRows = table.rows,
                    
                    innerCells = innerRows[0].childNodes;
                if (innerCells && innerCells.length > 0) {
                    for (var i = 0; i < innerCells.length; i++) {
                        innerCells[i].className = className;
                        if (customCSSText) {
                            // Title is the last cell in the tree-title table - this cell
                            // requires additional specification for the icon padding
                            if (i == innerCells.length-1) {
                                customCSSText += (this.isRTL() ? "paddingRight:" : "paddingLeft:")
                                                + this.iconPadding; 
                            }
                            innerCells[i].cssText = customCSSText;
                        }
                    }
                }
            }
        }

        // Actually style the cell itself
        return isc.GridRenderer.getPrototype()._updateCellStyle.apply(
                                        this, [record, rowNum, colNum, cell, className]);
    },

    //>	@method	treeGridBody.click()	(A)
    // Handle a click in the "open" area of a record specially
    //		@group	event handling	
    //
    //		@return	(boolean)	false == cancel further event processing
    //<
    // Note: We override click rather than putting this logic into rowClick / cellClick, as
    // we don't want folder toggling to occur if the user's mouse is hovering over the open
    // area while the user triggers standard record click handling by hitting the Space-bar.
    
    click : function (event, eventInfo) {
        if (!this._suppressEventHandling()) {

            var tg = this.grid,
                recordNum = tg.getEventRecordNum(),
                node = recordNum < 0 ? null : tg.getRecord(recordNum);
            // if what they clicked on is a folder, toggle it's state.  The 'open' observation
            // will automaticaly redraw for us
            if (node != null && tg.data.isFolder(node) && tg.clickInOpenArea(node)) {
                
                if (isc.screenReader) {
                    this._putNativeFocusInRow(recordNum);
                }
                tg.toggleFolder(node);

                // clear out the pointer to the last record clicked, and the last row selected
                // by keyboard navigation. (Prevents index-based keyboard navigation from
                // jumping around due to the number of rows changing as folders toggle)
                tg.clearLastHilite();
                tg._lastRecordClicked = null;
                
                // If we set up a maskedMouseDownCell, clear it up so it doesn't confuse
                // future click events.
                
                delete this._maskedMouseDownCell;

                // Note: if you click in the open area to toggle a folder, no nodeClick or
                // folderClick events will be fired, since we've already taken action in
                // response to your click by toggling a folder
                // Return EH.STOP_BUBBLING to stop propogation.
                return isc.EH.STOP_BUBBLING;
            }
        }
        return this.Super("click", arguments);
    },

    // Override mouseDown and mouseUp in the body to avoid selecting when clicking in open
    // area by default.
    
    //>	@method	treeGridBody.mouseDown()	(A)
    // Handle a click in the open area on mouseDown specially
    //		@group	event handling	
    //
    //		@return	(boolean)	false == cancel further event processing
    //<
    mouseDown : function () {
    	// get the item that was clicked on -- bail if not found
        var rowNum = this.getEventRow(),
            node = rowNum < 0 ? null : this.grid.data.get(rowNum);

        if (node != null) {
            if (this.grid.clickInOpenArea(node)) {
                // if they clicked in the open area of the record,
                // just bail because we're supposed to open the folder instead
                // TreeGrid.click() will actually open the folder
                
                return isc.EH.STOP_BUBBLING;
            } else if (this.grid.clickInCheckboxArea(node) && this.canSelectRecord(node)) {

                // Toggle node selection state
                var selectionType = this.grid.selectionType;
                if (selectionType == isc.Selection.SINGLE) {
                    this.deselectAllRecords();
                    this.selectRecord(node);
                } else if (selectionType == isc.Selection.SIMPLE
                    || selectionType == isc.Selection.MULTIPLE) {
                    if (this.selection.isSelected(node)) this.deselectRecord(node);
                    else this.selectRecord(node);
                }

                // Note: if you click in the checkbox area to select a node, no nodeClick or
                // folderClick events will be fired, since we've already taken action in
                // response to your click by selecting/deselected the node.
                // Return EH.STOP_BUBBLING to stop propogation.
                return isc.EH.STOP_BUBBLING;
            }
        }

        return this.Super("mouseDown", arguments);
    },

    //>	@method	treeGridBody.mouseUp()	(A)
    // Handle a click in the open area on mouseUp specially
    //		@group	event handling	
    //
    //		@return	(boolean)	false == cancel further event processing
    //<
    mouseUp : function () {
        // get the item that was clicked on -- bail if not found
        var rowNum = this.getEventRow(),
            node = rowNum < 0 ? null : this.grid.data.get(rowNum);
        if (node != null &&
            (this.grid.clickInOpenArea(node) || this.grid.clickInCheckboxArea(node)))
        {
            // don't select the row; on click() we'll open the folder
            return isc.EH.STOP_BUBBLING;
        } else {
            // proceed to super (select the row)
            return this.Super("mouseUp", arguments);
        }
    },

    // Override to place embedded components for the tree field indented as a title
    // would be if TG.indentRecordComponents == true.
    placeEmbeddedComponent : function (component) {
        if (this.grid.indentRecordComponents) {
            var colNum = component._currentColNum;
            if (colNum == this.grid.getTreeFieldNum() && !component.snapOffsetLeft) {
                var record = component.embeddedRecord;
                if (record != null) {
                    component.snapOffsetLeft
                        = this.grid.getOpenAreaWidth(record) + this.grid.iconPadding;
                }
            }
        }
        return this.Super("placeEmbeddedComponent", arguments);
    },

    getTableHTML : function (colNum, startRow, endRow, discreteCols, asyncCallback, isAsync) {
        var data = this.grid.data,
            preCacheRange = (
                isc.isA.ResultTree(data) &&
                
                !this.isEmpty());

        var actualStartRow = startRow,
            actualEndRow = endRow;

        if (preCacheRange) {
            
            var drawRect = this._getTableHTMLDrawArea(startRow, endRow, false);
            actualStartRow = drawRect[0];
            actualEndRow = drawRect[1];

            
            data._pushCachedRange(actualStartRow, actualEndRow);
        }

        var ret = this.invokeSuper(
                isc.TreeGridBody,
                "getTableHTML",
                colNum, startRow, endRow, discreteCols, asyncCallback, isAsync);

        if (preCacheRange) {
            data._popCachedRange(actualStartRow, actualEndRow);
        }
        return ret;
    }
});

isc.TreeGrid.addClassProperties({

	// default field to display a tree
	TREE_FIELD : {name:"nodeTitle", treeField:true,

	            // This flag will avoid standard type formatters running based on field
	            // type as part of _formatCellValue.
	            // type formatting already occurs in the default getNodeTitle() 
	            // implementation.
	            // This is the correct place for this formatting, since if that method is
	            // overridden we would expect it to return a string rather than an object
	            // of whatever field type the treeField is.
	            _suppressTypeFormatting:true,

    			getCellValue : function (list,record,recordNum,colNum) {
                    if (!list.getNodeTitle) {
                        var fieldName = colNum == null ? null : list.getFieldName(colNum);
                        return record == null || fieldName == null ? null : record[fieldName];
                    }
                    return list.getNodeTitle(record,recordNum, this)
                },
                canFilter: false,    
                // return the title for the Header Button over the tree column.
                
                getFieldTitle : function (viewer, fieldNum) {
                    var field = viewer.getField(fieldNum);              
                    if (field.name == "nodeTitle") return viewer.treeFieldTitle;
                    
                    // otherwise just return the title of the field, or failing that, the field's name
                    return field.title || field.name;
                }
    },
    
    // _getTreeCellTemplate - returns the HTML template array used for the start of
    // tree grid cells.
    // This is a dynamic method - it incorporates the standard 'noDoublingCSS' string into the
    // returned HTML template. That string can change at runtime due to setNeverUseFilters()
    // so we need to react to this and regenerate the template.
    _getTreeCellTemplate : function () {
        if (!this._observingDoublingStrings) {
            isc.Canvas._doublingStringObservers.add({
                target:this, 
                methodName:"_doublingStringsChanged"
            });
            this._observingDoublingStrings = true;
        }
        if (this._$treeCellTemplate == null) {
            this._$treeCellTemplate = [
                
                "<table" + (isc.Browser.isIE && isc.screenReader ? " unselectable='on'" : "") +
                " role='presentation' cellpadding=0 cellspacing=0 class='", // [0]
                ,                                                   // [1] - this.getCellStyle()
                "' style='",          // [2]
                ,                                                   // [3] - getCellCSSText()
                // use _$noStyleDoublingCSS to suppress any border / background image etc from the
                // cell style
                // Also use noStyleDoublingCSS and explicitly re-apply the gridrenderer cell style and 
                // cell cssText to each cell within the tree-cell table.
                
                isc.Canvas._$noStyleDoublingCSS + "'><colgroup><col width='",// [4]
                ,                                                   // [5] - indent cell width
                "px'/><col width='",                                // [6]
                ,                                                   // [7] - icon cell width
                
                "px'/><col/></colgroup><tbody><tr><td" +
                (isc.Browser.isIE && isc.screenReader ? " unselectable='on'" : "") + " style='line-height:0px;", // [8] (indent cell)
                ,                                                   // [9] - getCellCSSText()
                isc.Canvas._$noStyleDoublingCSS + "' class='",      // [10]
                ,                                                   // [11] - getCellStyle()
                "'>",                                               // [12]
                ,                                                   // [13] - indentHTML
                "</td>"                                             // [14] 
                // (we'll write the title cell out using _$treeCellTitleTemplate)
            ];
        }
        return this._$treeCellTemplate;
    },

    _getTreeCellTitleTemplate : function () {
        if (!this._observingDoublingStrings) {
             isc.Canvas._doublingStringObservers.add({
                target:this, 
                methodName:"_doublingStringsChanged"
            });
            this._observingDoublingStrings = true;
        }

        if (this._$treeCellTitleTemplate == null) {
            this._$treeCellTitleTemplate = [

                
                "<td" + (isc.Browser.isIE && isc.screenReader ? " unselectable='on'" : "") + " style='line-height:0px;", // [0] (icon(s) cell)
                ,                                                      // [1] - getCellCSSText()
                ";" + isc.Canvas._$noStyleDoublingCSS + "' class='",   // [2]
                ,                                                      // [3] - getCellStyle()
                
                "'>" + (isc.Browser.isSafari || isc.Browser.isIE ? "<nobr>" : ""), // [4]
                ,                                                   // [5] - opener icon HTML
                ,                                                   // [6] - 'extra' icon if there is one
                ,                                                   // [7] - icon for item (eg folder/file icon)
                (isc.Browser.isSafari || isc.Browser.isIE ? "</nobr>" : "") +
                    "</td><td",                                     // [8] (start of value cell)
                ,                                                   // [9] ID attribute, or null
                (isc.Browser.isIE && isc.screenReader ? " unselectable='on'" : "") + " style='", // [10]
                ,                                                   // [11] - getCellCSSText()
                ";" + isc.Canvas._$noStyleDoublingCSS 
                    + (isc.Page.isRTL()
                       ? "padding-left:1px;padding-right:"
                       : "padding-right:1px;padding-left:"),        // [12]
                ,                                                   // [13] - this.iconPadding
                "px;' class='",                                     // [14]
                ,                                                   // [15] - getCellStyle()
                "'>",                                               // [16]
                ,                                                   // [17] - <NOBR> or null
                ,                                                   // [18] - value
                ,                                                   // [19] - </NOBR> or null
                "</td>"                                             // [20]
            ];
        }
        return this._$treeCellTitleTemplate;
    },
    
    _doublingStringsChanged : function () {
        this._$treeCellTemplate = null;
        this._$treeCellTitleTemplate = null
    }


});


isc.TreeGrid.addProperties({

	//>	@attr	treeGrid.dataSource		(DataSource or ID : null : IRW)
    // @include dataBoundComponent.dataSource
	//<

	//>	@attr	treeGrid.data		(Tree : null : IRW)
	// A +link{class:Tree} object containing of nested +link{object:TreeNode}s to 
    // display as rows in this TreeGrid.  
    // The <code>data</code> property will typically not be explicitly specified for 
    // databound TreeGrids, where the data is returned from the server via databound component
    // methods such as <code>fetchData()</code>
	//      @visibility external
	//		@group	data
	//<

    //> @attr treeGrid.initialData (List of TreeNode : null : IRA)
    // You can specify the initial set of data for a databound TreeGrid using this property.
    // The value of this attribute should be a list of <code>parentId</code>-linked
    // +link{TreeNode}s in a format equivalent to that documented on +link{Tree.data} or, for
    // TreeGrids with +link{treeGrid.dataFetchMode,dataFetchMode} set to
    // +link{type:FetchMode,"paged"}, on +link{ResultTree.data}.
    // <P>
    // If you create a standalone +link{class:Tree} or +link{class:ResultTree} as the
    // TreeGrid's +link{treeGrid.data,data} then you may equivalently specify this initial set
    // of tree nodes in that tree's +link{Tree.data,data} property.
    // @see TreeNode
    // @see Tree.data
    // @see ResultTree.data
    // @visibility external
    // @example initialData
    //<
    
    //> @attr treeGrid.loadDataOnDemand   (boolean : null : IRW)
    // For databound treeGrid instances, should the entire tree of data be loaded on initial 
    // fetch, or should folders load their children as they are opened.
    // <P>
    // If unset, calling +link{fetchData()} will default it to true, otherwise, if a ResultTree
    // is passed to +link{setData()}, the +link{resultTree.loadDataOnDemand} setting is
    // respected.
    // <P>
    // Note that when using <code>loadDataOnDemand</code>, every node returned by the server is
    // assumed be a folder which may load further children.  See
    // +link{resultTree.defaultIsFolder} for how to control this behavior.
    // 
    // @see dataFetchMode
    // @group databinding
    // @visibility external
    // @example initialData
    //<
    

    //> @attr treeGrid.keepParentsOnFilter (boolean : null : IR)
    // @include resultTree.keepParentsOnFilter
    // @visibility external
    //<

    //> @attr treeGrid.dataFetchMode (FetchMode : "basic" : IR)
    // @include resultTree.fetchMode
    // @visibility external
    //<

    //> @attr treeGrid.serverFilterFields (Array of String : null : IR)
    // @include resultTree.serverFilterFields
    // @visibility external
    //<

    //> @attr treeGrid.autoFetchTextMatchStyle (TextMatchStyle : "exact" : IR)
    // With +link{loadDataOnDemand}:true, TreeGrids fetch data by selecting the child nodes of
    // each parent, which should be exact match, so we default to
    // <code>autoFetchTextMatchStyle:"exact"</code>.
    // See +link{listGrid.autoFetchTextMatchStyle} for details.
    //
    // @group dataBinding
    // @visibility external
    //<
    autoFetchTextMatchStyle:"exact",
    
    //> @attr treeGrid.cascadeSelection (Boolean : false : [IR])
    // Should children be selected when parent is selected? And should parent be
    // selected when any child is selected?
    // @visibility external
    //<
    cascadeSelection:false,

    //> @attr treeGrid.showPartialSelection (Boolean : false : [IRW])
    // Should partially selected parents be shown with special icon?
    // @visibility external
    //<
    showPartialSelection:false,

    //> @attr treeGrid.selectionProperty (string : null : [IRA])
    // @include listGrid.selectionProperty
    // @visibility external
    //<

    //> @attr treeGrid.canSelectAll (boolean : null : [IRW])
    // This property is not supported on TreeGrid, and only applies to the +link{ListGrid}
    // superclass.
    // 
    // @group selection
    // @visibility external
    //<

    
    booleanTrueImage:null,
    booleanFalseImage:null,
    booleanPartialImage:null,

    showClippedValuesOnHover:null,

    //> @attr treeGrid.treeRootValue (any : null : IRA)
    // For databound trees, use this attribute to supply a +link{DataSourceField.rootValue} for this
    // component's generated data object.
    // <P> 
    // This property allows you to have a particular component navigate a tree starting from any
    // given node as the root.
    // @group databinding
    // @visibility external
    //<
    
    
    //> @attr   treeGrid.fields       (Array of TreeGridField: null : IRW)
    // An array of field objects, specifying the order, layout, dynamic calculation, and
    // sorting behavior of each field in the treeGrid object. In TreeGrids, the fields
    // array specifies columns. Each field in the fields array is TreeGridField object.
    // <p>
    // If +link{TreeGrid.dataSource} is also set, this value acts as a set of overrides as
    // explained in +link{attr:DataBoundComponent.fields}.
    //
    // @group databinding
    // @see TreeGridField
    // @visibility external
    //<

    //> @object TreeGridField
    //
    // An object literal with a particular set of properties used to configure the display of
    // and interaction with the columns of a +link{TreeGrid}.
    // +link{TreeGrid} is a subclass of +link{ListGrid} and as a result, for all fields except
    // the field containing the +link{Tree} itself (specified by
    // +link{treeGridField.treeField}, all properties settable on
    // +link{ListGridField} apply to TreeGridField as well.
    // <p>
    // This class documents just those properties that are specific to TreeGridFields - see
    // +link{ListGridField} for the set of inherited properties.
    // 
    // @see ListGridField
    // @see TreeGrid.fields
    // @see ListGrid.setFields
    //
    // @treeLocation Client Reference/Grids/TreeGrid
    // @visibility external
    //< 

    // Tree Field
    // ---------------------------------------------------------------------------------------

    //> @attr treeGridField.treeField (Boolean : see below : IRW)
    //
    // The field containing <code>treeField: true</code> will display the +link{Tree}.  If no
    // field specifies this property, if a field named after the +link{Tree.titleProperty} of
    // the Tree is present in +link{TreeGrid.fields}, that field will show the tree.  Note that
    // when using a DataSource, you typically define the title field via
    // +link{DataSource.titleField} and the generated +link{ResultTree} automatically uses this
    // field.
    //
    // If none of the above rules apply, the first field in +link{TreeGrid.fields} is assigned to
    // display the +link{Tree}.
    //
    // @group treeField
    // @visibility external
    //<

    //> @attr treeGridField.canExport (Boolean : null : IR)
    //	Dictates whether the data in this field be exported.  Explicitly set this
    //  to false to prevent exporting.  Has no effect if the underlying 
    //  +link{dataSourceField.canExport, dataSourceField} is explicitly set to 
    //  canExport: false.
    //
    // @visibility external
    //<

    //>	@attr	treeGrid.treeFieldTitle		(string : "Name" : IR)
	//	Visible title for the tree column (field).
    // @group treeField
    // @visibility external
	//<
	treeFieldTitle:"Name",		
    
    //> @attr treeGrid.autoAssignTreeField (boolean : true : IR)
    // If no field was specified as the "tree-field" (showing indentations for tree hierarchy
    // and tree icons), should we assign one of the other fields to be the tree-field?
    // Useful when we want to display a tree or partial tree as a flattened list.
    // @group treeField
    //<
    autoAssignTreeField:true,

    //>	@attr	treeGrid.showRoot		(Boolean : false : IR)
    // Specifies whether the root node should be displayed in the treeGrid.
    // <P>
    // This property is only available for "children" modelType trees, hence is not allowed for
    // trees that load data from the server dynamically via +link{fetchData()}.  
    // <P>
    // To get the equivalent of a visible "root" node in a tree that loads data dynamically,
    // add a singular, top-level parent to the data.  However, note that this top-level parent
    // will technically be the only child of root, and the implicit root object will be
    // returned by +link{tree.getRoot,this.data.getRoot()}.
    //
    // @group treeField
    // @visibility external
	//<
	showRoot:false,
    
    //>	@attr	treeGrid.separateFolders		(boolean : null : IR)
    // If specified, this attribute will override +link{Tree.separateFolders} on the
    // data for this treeGrid.
    // <P>
    // Specifies whether folders and leaves should be segregated in the treeGrid display.
    // Use +link{tree.sortFoldersBeforeLeaves} to customize whether folders appear before 
    // or after their sibling leaves.
    // <P>
    // If unset, at the treeGrid level, the property can be set directly on
    // +link{treeGrid.data,the tree data object} or for dataBound TreeGrids on the
    // +link{treeGrid.dataProperties}.
    //
    // @group treeField
    // @visibility external
	//<
//	separateFolders:false,

	//> @attr treeGrid.dataProperties (Tree : null : IR)
    // For a <code>TreeGrid</code> that uses a DataSource, these properties will be passed to
    // the automatically-created ResultTree.  This can be used for various customizations such as
    // modifying the automatically-chosen +link{tree.parentIdField}.
    // @group databinding
    // @visibility external
    //<
	//
    //> @attr treeGrid.sortFoldersBeforeLeaves (boolean : null : IR)
    // If specified, this attribute will override +link{tree.sortFoldersBeforeLeaves} on
    // the data for this treeGrid.
    // <P>
    // Specifies whether when +link{tree.separateFolders} is true, folders should be displayed
    // before or after their sibling leaves in a sorted tree. If set to true, with
    // sortDirection set to Array.ASCENDING, folders are displayed before their sibling leaves
    // and with sort direction set to Array.DESCENDING they are displayed after. To invert
    // this behavior, set this property to false.
    // @group treeField
    // @see treeGrid.separateFolders
    // @visibility external
    //<
//    sortFoldersBeforeLeaves:null,

	//>	@attr	treeGrid.displayNodeType (DisplayNodeType : isc.Tree.FOLDERS_AND_LEAVES : [IRW])
    //          Specifies the type of nodes displayed in the treeGrid. 
    // @see type:DisplayNodeType for options
    // @group treeField
    // @visibility external
	//<
	displayNodeType:isc.Tree.FOLDERS_AND_LEAVES,
	
	//> @attr treeGrid.autoPreserveOpenState (PreserveOpenState : null : [IR])
	// For dataBound treeGrids this specifies the +link{resultTree.autoPreserveOpenState},
	// governing whether the open state of the tree should be preserved when new data
	// arrives due to cache invalidation.
	//
	// @visibility external
	//<
	
	// Drag and Drop
	// --------------------------------------------------------------------------------------------
    //>	@attr	treeGrid.canDragRecordsOut		(Boolean : false : IRW)
	//	@include ListGrid.canDragRecordsOut
    //  @group	dragdrop
    // @see TreeNode.canDrag
    // @see TreeNode.canAcceptDrop
    // @visibility external
    // @example treeDropEvents
    //<
	canDragRecordsOut:false,			

	// Drag and Drop
	// --------------------------------------------------------------------------------------------

    //>	@attr	treeGrid.canAcceptDroppedRecords		(Boolean : false : IRW)
	//	@include ListGrid.canAcceptDroppedRecords
    // @see TreeNode.canDrag
    // @see TreeNode.canAcceptDrop
    //  @group	dragdrop
    // @visibility external
    // @example dragReparent
    //<
	//canAcceptDroppedRecords:false,		
	
    //>	@attr	treeGrid.canReorderRecords		(Boolean : false : IRWA)
    // @include ListGrid.canReorderRecords
    // @see TreeNode.canDrag
    // @see TreeNode.canAcceptDrop
    // @group dragdrop
    // @visibility external
    // @example dragReparent
	//<
	//canReorderRecords:false,
 
    //> @attr treeGrid.canDropOnLeaves          (Boolean : false : IRWA)   
    // Whether drops are allowed on leaf nodes.
    // <P>
    // Dropping is ordinarily not allowed on leaf nodes unless +link{canReorderRecords} is
    // set.  
    // <P>
    // The default action for a drop on a leaf node is to place the node in that leaf's parent
    // folder.  This can be customized by overriding +link{folderDrop()}.
    // <P>
    // Note that enabling <code>canDropOnLeaves</code> is usually only appropriate where you
    // intend to add a custom +link{folderDrop()} implementation that <b>does not</b> add a
    // child node under the leaf.  If you want to add a child nodes to a leaf, instead of
    // enabling canDropOnLeaves, use empty folders instead - see +link{Tree.isFolder} for how
    // to control whether a node is considered a folder.
    //
    // @visibility external
    //<

    //> @attr treeGrid.canDropRootNodes (Boolean : true : IRW)   
    // Whether to allow dropping new root nodes for the grid
    // <P>
    // If this property is false, attempts to drop a new root node will result in dropping
    // on the nearest root node instead.
    //<
    canDropRootNodes: true, 
    
    //> @attr treeGrid.canReparentNodes     (boolean : null : IRW)
    // If set this property allows the user to reparent nodes by dragging them from their
    // current folder to a new folder.<br>
    // <b>Backcompat:</b> For backwards compatibility with versions prior to SmartClient 1.5,
    // if this property is unset, but <code>this.canAcceptDroppedRecords</code> is true, we
    // allow nodes to be dragged to different folders.
    // @see TreeNode.canDrag
    // @see TreeNode.canAcceptDrop
    // @group dragdrop
    // @visibility external
    //<
    //canReparentNodes:null,
	
	//>	@attr	treeGrid.dragDataAction		(DragDataAction : isc.ListGrid.MOVE : IRWA)
    //
    // Specifies what to do with data dragged from this TreeGrid (into another component, or
    // another node in this TreeGrid.  The default action is to move the data.  A setting of
    // "none" is not recommended for trees because Trees maintain the node open state on the nodes
    // themselves, and hence having multiple Tree objects share a reference to a node can have
    // unintended consequences (such as opening a folder in one tree also triggering an open in
    // another tree that shares the same node).
    // <br><br>
    // For DataBound trees (+link{class:ResultTree}), the expectation is that
    // +link{method:TreeGrid.folderDrop} will be overridden to perform whatever action took
    // place as the result of the drag and drop interaction.
	//
    // @see group:sharingNodes
    // @visibility external
	//<
	dragDataAction:isc.ListGrid.MOVE,
    
    
    
    //> @attr treeGrid.openDropFolderDelay (integer : 600 : IRWA)
    // When dragging something over a closed folder, delay in milliseconds before the folder
    // automatically opens.
    //<
    openDropFolderDelay:600,

    // D&D Error Messages
    // error messages for invalid drag and drop situations.  Can be customized on a per
    // instance basis so something more application-specific can be said, eg "a manager cannot
    // become his own employee"

    //> @attr treeGrid.parentAlreadyContainsChildMessage (String : "This item already contains a child item with that name." : IR)
    // Message displayed when user attempts to drag a node into a parent that already contains
    // a child of the same name.
    // @see attr:treeGrid.canDragRecordsOut
    // @see attr:treeGrid.canAcceptDroppedRecords
    // @see attr:treeGrid.canReorderRecords
    // @group i18nMessages
    // @visibility external
    //<
    parentAlreadyContainsChildMessage:"This item already contains a child item with that name.",
    
	//>	@attr treeGrid.cantDragIntoSelfMessage (String : "You can't drag an item into itself." : IR)
    // Message displayed when user attempts to drop a dragged node onto itself.
    // @see attr:treeGrid.canDragRecordsOut
    // @see attr:treeGrid.canAcceptDroppedRecords
    // @see attr:treeGrid.canReorderRecords
    // @group i18nMessages
    // @visibility external    
    //<
	cantDragIntoSelfMessage:"You can't drag an item into itself.",

	//>	@attr treeGrid.cantDragIntoChildMessage (String : "You can't drag an item into one of it's children." : IR)
    // Message displayed when user attempts to drop a node into a child of itself.
    // @see attr:treeGrid.canDragRecordsOut
    // @see attr:treeGrid.canAcceptDroppedRecords
    // @see attr:treeGrid.canReorderRecords
    // @group i18nMessages
    // @visibility external
    //<
	cantDragIntoChildMessage:"You can't drag an item into one of it's children.",

    // Body Rendering
	// --------------------------------------------------------------------------------------------

    //>	@attr	treeGrid.fixedFieldWidths		(boolean : true : IRWA)
	//			make trees fixedFieldWidths by default
	//		@group	appearance
	//<
	fixedFieldWidths:true,

    //>	@attr	treeGrid.wrapCells		(boolean : false : IRWA)
	//			don't wrap, as that will mess up the look of the trees
	//		@group	appearance
	//<
	wrapCells:false,

	//>	@attr	treeGrid.showHiliteInCells		(boolean : false : IRWA)
	// Should the hilite show across the entire record or just in the text of the item itself	
	//<
	showHiliteInCells:false,
	
    // Images: locations, sizes, and names
    // --------------------------------------------------------------------------------------------
    //> @attr treeGrid.indentSize (number : 20 : IRW)
    // The amount of indentation (in pixels) to add to a node's icon/title for each level
    // down in this tree's hierarchy.
    // <p>
    // This value is ignored when +link{treeGrid.showConnectors,showConnectors} is
    // <code>true</code> because fixed-size images are used to render the connectors.
    // @visibility external
    // @group appearance
    //<
    indentSize:20,
    
    //> @attr treeGrid.extraIconGap (int: 2 : IR)
    // The amount of gap (in pixels) between the extraIcon (see +link{treeGrid.getExtraIcon()})
    // or checkbox icon and the +link{treeGrid.nodeIcon,nodeIcon}/
    // +link{treeGrid.folderIcon,folderIcon} or node text.
    // @group appearance
    // @visibility external
    //<
    extraIconGap:2,
    
    //>	@attr	treeGrid.iconSize		(number : 16 : [IRW])
    //          The standard size (same height and width, in pixels) of node icons in this
    //          treeGrid.
    // @group treeIcons
    // @visibility external
    //<
	iconSize:16,

    //>	@attr	treeGrid.openerIconSize		(number : null : [IRW])
    // Default width and height in pixels of the opener icons, that is, the icons which show
    // the open or closed state of the node, typically a [+] or [-] symbol, if not overridden
    // by +link{TreeGrid.openerIconWidth}/+link{TreeGrid.openerIconHeight}.
    // <P>
    // If +link{showConnectors} is true, the opener icon includes the connector line, and
    // defaults to +link{listGrid.cellHeight,cellHeight}.
    // <P>
    // Otherwise, <code>openerIconSize</code> defaults to +link{iconSize}.
    //
    // @group treeIcons
    // @visibility external
    //<
    
    //> @attr treeGrid.openerIconWidth (number : null : [IRW])
    // Width in pixels of the opener icons, that is, the icons which show the open or closed
    // state of the node, typically a [+] or [-] symbol.
    // <P>
    // If not specified, +link{TreeGrid.openerIconSize} is used instead.
    //
    // @group treeIcons
    // @visibility external
    //<

    //> @attr treeGrid.openerIconHeight (number : null : [IRW])
    // Height in pixels of the opener icons, that is, the icons which show the open or closed
    // state of the node, typically a [+] or [-] symbol.
    // <P>
    // If not specified, +link{TreeGrid.openerIconSize} is used instead.
    //
    // @group treeIcons
    // @visibility external
    //<

    //>	@attr	treeGrid.skinImgDir		(URL : "images/TreeGrid/" : IRWA)
	//		Where do 'skin' images (those provided with the class) live?
	//		This is local to the Page.skinDir
	//		@group	appearance, images
	//<
	skinImgDir:"images/TreeGrid/",	

    //> @attr treeGrid.showLoadingIcons (boolean : true : IR)
    // If set, when a folder is loading it's children from the server (+link{Tree.getLoadState()}
    // returns "loading"), it uses a distinct icon image given by +link{loadingIcon}.  This is
    // typically used to show a small animating "spinner" icon to let the user know data is being
    // fetched.
    // @group treeIcons
    // @visibility external
    //<
    showLoadingIcons:true,
    
    //> @attr treeGrid.loadingIcon (SCImgURL : "[SKIN]folder_loading.gif" : [IRW])
    // If +link{showLoadingIcons} is set, this icon will be used when the folder is 
    // +link{Tree.getLoadState(),loading children from the server}.
    // @group treeIcons
    // @visibility external
    //<
    loadingIcon:"[SKIN]folder_loading.gif",
    
    //>	@attr	treeGrid.folderIcon        (SCImgURL : "[SKIN]folder.gif" : [IRW])
    // The URL of the base icon for all folder nodes in this treeGrid. Note that this URL will
    // have +link{treeGrid.openIconSuffix}, +link{treeGrid.closedIconSuffix} or 
    // +link{treeGrid.dropIconSuffix} appended to indicate state changes if appropriate - 
    // see documentation on  +link{treeGrid.showOpenIcons} and +link{treeGrid.showDropIcons}.
    // @group treeIcons
    //      @visibility external
    // @example nodeTitles
    //<
    folderIcon:"[SKIN]/folder.gif",

    //> @attr   treeGrid.dropIconSuffix   (String : "drop" : [IR])
    // If +link{treeGrid.showDropIcons} is true, this suffix will be appended to the
    // +link{treeGrid.folderIcon} when the user drop-hovers over some folder.
    // @group treeIcons
    // @visibility external
    //<
    dropIconSuffix:"drop",
    
    //> @attr   treeGrid.openIconSuffix   (String : "open" : [IR])
    // If +link{showOpenIcons} is true, this suffix will be appended to the
    // +link{folderIcon} for open folders in this grid.
    // @group treeIcons
    // @visibility external
    //<
    openIconSuffix:"open",

    //> @attr   treeGrid.closedIconSuffix   (String : "closed" : [IR])
    // This suffix will be appended to the +link{folderIcon} for closed folders.
    // If +link{showOpenIcons} is set to <code>false</code> this suffix will also be
    // appended to open folders' icons.
    // @group treeIcons
    // @visibility external
    //<
    closedIconSuffix:"closed",

    
    //> @attr   treeGrid.nodeIcon  (SCImgURL : "[SKIN]file.gif" : [IRW])
    // The filename of the default icon for all leaf nodes in this grid. To specify a 
    // custom image for an individual node, set the +link{customIconProperty} directly on
    // the node.
    // @group treeIcons
    // @visibility external
    // @example nodeTitles
    //<
    nodeIcon:"[SKIN]/file.gif",    
    
    //>@attr treeGrid.showOpenIcons (Boolean : true : IRW)
    // If true, show a different icon for <code>open</code> folders than closed folders.
    // This is achieved by appending the +link{openIconSuffix} onto the 
    // +link{folderIcon} URL [for example <code>"[SKIN]/folder.gif"</code> might be 
    // replaced by <code>"[SKIN]/folder_open.gif"</code>.<br>
    // <b>Note</b> If this property is set to <code>false</code> the same icon is shown for
    // open folders as for closed folders, unless a custom folder icon was specified. This will be
    // determined by +link{folderIcon} plus the +link{closedIconSuffix}.
    // @group treeIcons
    // @visibility external
    // @example nodeTitles
    //<
    
    showOpenIcons:true,

    //>@attr treeGrid.showDropIcons (Boolean : true : IRW)
    // If true, when the user drags a droppable target over a folder in this TreeGrid, show 
    // a different icon folder icon.
    // This is achieved by appending the +link{treeGrid.dropIconSuffix} onto the
    // +link{TreeGrid.folderIcon} URL (for example <code>"[SKIN]/folder.gif"</code> may be
    // replaced by <code>"[SKIN]/folder_drop.gif"</code>).
    // @group treeIcons
    // @visibility external
    // @example nodeTitles
    //<
    showDropIcons:true,
    
    //> @attr   treeGrid.customIconProperty   (String : "icon" : [IRW])
    // This property allows the developer to rename the 
    // +link{TreeNode.icon, default node.icon} property.
    // @group treeIcons
    // @visibility external
    //<
    customIconProperty:"icon",

    //> @attr   treeGrid.customIconOpenProperty (string : "showOpenIcon" : [IRWA])
    // This property allows the developer to rename the 
    // +link{TreeNode.showOpenIcon, default node.showOpenIcon} property.
    // @see treeGrid.customIconProperty
    // @see treeGrid.showCustomIconOpen
    // @visibility external
    // @group treeIcons
    //<
    customIconOpenProperty:"showOpenIcon",
    
    //> @attr   treeGrid.customIconDropProperty (string : "showDropIcon" : [IRWA])
    // This property allows the developer to rename the 
    // +link{TreeNode.showDropIcon, default node.showDropIcon} property.
    // @see treeGrid.customIconProperty
    // @see treeGrid.showCustomIconDrop
    // @visibility external
    // @group treeIcons
    //<
    customIconDropProperty:"showDropIcon",

    //> @attr   treeGrid.showCustomIconOpen   (Boolean : false : [IRWA])
    // Should folder nodes showing custom icons (set via the +link{customIconProperty}),
    // show open state images when the folder is opened.
    // If true, the +link{openIconSuffix} will be appended to the image URL
    // (so <code>"customFolder.gif"</code> might be replaced with 
    // <code>"customFolder_open.gif"</code>).<br>
    // <b>Note</b> that the +link{closedIconSuffix} is never appended to custom folder icons.<br>
    // Can be overridden at the node level via the default property +link{treeNode.showOpenIcon}
    // and that property can be renamed via +link{treeGrid.customIconOpenProperty}.
    // @group treeIcons
    // @visibility external
    //<
    showCustomIconOpen:false,
    
    //> @attr   treeGrid.showCustomIconDrop   (Boolean : false : [IRWA])
    // Should folder nodes showing custom icons (set via the +link{treeGrid.customIconProperty},
    // default +link{treeNode.icon}),
    // show drop state images when the user is drop-hovering over the folder.
    // If true, the +link{treeGrid.dropIconSuffix} will be appended to the image URL
    // (so <code>"customFolder.gif"</code> might be replaced with 
    // <code>"customFolder_drop.gif"</code>).<br>
    // Can be overridden at the node level via the default property +link{treeNode.showDropIcon}
    // and that property can be renamed via +link{treeGrid.customIconDropProperty}.
    // @group treeIcons
    // @visibility external
    //<
    showCustomIconDrop:false,

    //> @attr   treeGrid.showDisabledSelectionCheckbox  (Boolean : false : [IR])
    // Should tree nodes show a disabled checkbox 
    // +link{ListGrid.selectionAppearance, selectionAppearance}:"checkbox" 
    // is set on the treegrid, and a node can't be selected? 
    // <P>
    // If set to <code>false</code> the treeGrid will use 
    // +link{treeGrid.leaveSelectionCheckboxGap} to determine whether to leave
    // a blank space where the checkbox would normally appear.
    //
    // @see ListGrid.recordCanSelectProperty
    // @group treeIcons
    // @visibility external
    //<
    
    //> @attr treeGrid.leaveSelectionCheckboxGap (Boolean : true : [IR])
    // If +link{ListGrid.selectionAppearance, selectionAppearance}:"checkbox" 
    // is set on the treegrid, and a node can't be selected, should a gap be left where
    // the checkbox icon would normally appear, in order to make the node's icon and title
    // line up with the content for other nodes in the same parent?
    // <p>
    // Has no effect if +link{showDisabledSelectionCheckbox} is <code>true</code>
    // @see ListGrid.recordCanSelectProperty
    // @group treeIcons
    // @visibility external
    //<
    leaveSelectionCheckboxGap:true,
    
    // ---------------------------------
    // DEPRECATED ICON PROPERTIES:
    // 
            
    //>	@attr	treeGrid.folderOpenImage        (String : null : [IRW])
    //          The filename of the default icon for all open folder nodes in this treeGrid.
    //      @visibility external
    //  @deprecated as part of SmartClient release 5.5 in favor of +link{TreeGrid.folderIcon}
    //<

    //>	@attr	treeGrid.folderClosedImage		(string : null : [IRW])
    // The filename of the default icon for all closed folder nodes in this treeGrid. Use
    // the node.icon property (null by default) to specify a custom image for an individual
    // folder node. The same custom image will be used for both the open and closed folder
    // images.
    //      @visibility external
    //  @deprecated as part of SmartClient release 5.5 in favor of +link{TreeGrid.folderIcon}
    //<

    //>	@attr	treeGrid.folderDropImage       (String : null : [IRW])
    // The filename of the icon displayed for a folder node that will accept drag-and-drop
    // data when the mouse is released.
    //      @visibility external
    //  @deprecated as part of SmartClient release 5.5 in favor of +link{TreeGrid.folderIcon}    
    //<

    //>	@attr	treeGrid.fileImage             (SCImgURL : "[SKIN]file.gif" : [IRW])
    // The filename of the default icon for all leaf nodes in this treeGrid. Use the
    // node.icon property (null by default) to specify a custom image for an individual
    // node.
    //      @visibility external
    //  @deprecated as part of SmartClient release 5.5 in favor of +link{TreeGrid.nodeIcon}
    //<
    
    // --------------------

    //>	@attr	treeGrid.manyItemsImage        (SCImgURL : "[SKIN]folder_file.gif" : [IRW])
    // The filename of the icon displayed use as the default drag tracker when for multiple
    // files and/or folders are being dragged.
    // @group dragdrop
    //      @visibility external
    //<
	manyItemsImage:"[SKIN]folder_file.gif",
    
    //>	@attr	treeGrid.showConnectors (Boolean : false : [IRW])
    // Should this treeGrid show connector lines illustrating the tree's hierarchy?
    // <P>
    // For the set of images used to show connectors, see +link{connectorImage}.
    // <P>
    // <b>Note</b>: in order for connector images to be perfectly connected, all styles for
    // cells must have no top or bottom border or padding.  If you see small gaps in connector
    // lines, check your CSS files.  See the example below for an example of correct
    // configuration, including example CSS.
    // 
    // @group treeIcons
    // @example connectors
    // @visibility external
    //<
    
    showConnectors : false,
    
    //>	@attr	treeGrid.showFullConnectors (Boolean : true : [IRW])
    // If +link{treeGrid.showConnectors} is true, this property determines whether we should show
    // vertical continuation lines for each level of indenting within the tree. Setting to
    // false will show only the hierarchy lines are only shown for the most indented path ("sparse"
    // connectors).
    // @group treeIcons
    // @visibility external
    //<
    // Default to false since older skins won't have all the media required to render full
    // connector lines out.
    // The logic to show full connectors involves iterating through the parents for each node
    // being written out. This is a potential performance hit. We could improve this performance
    // by adding caching logic to the Tree when calculating where the continuation lines should
    // appear if this is a problem.
    showFullConnectors:true,
    
    //> @attr treeGrid.showOpener (Boolean : true : [IRW])
    // Should the opener icon be displayed next to folder nodes? This is an icon
    // which visually indicates whether the folder is opened or closed (typically via
    // a [+] or [-] image, or a turn-down arrow) and may be clicked to expand or collapse
    // the folder.
    // <P>
    // For folders with no children, this icon is not shown unless 
    // +link{treeGrid.alwaysShowOpener} is <code>true</code>. Note that for trees which
    // +link{treeGrid.loadDataOnDemand,load data on demand}, we may not know if a folder
    // has any descendants if it has never been opened. As such we will show the
    // opener icon next to the folder. Once the user opens the icon and a fetch occurs,
    // if the folder is empty, and +link{alwaysShowOpener} is false, the opener icon
    // will be hidden.
    // <P>
    // For more information on load on demand trees, and how we determine whether
    // a node is a a folder or a leaf, please refer to the +link{group:treeDataBinding}
    // documentation.
    //
    // @visibility external
    //<
    showOpener:true,
    
        
    //> @attr treeGrid.alwaysShowOpener (Boolean : false : IRW)
    // If +link{treeGrid.showOpener} is true, should we display the opener icon
    // for folders even if they have no children?
    // <P>
    // Note that for trees which
    // +link{treeGrid.loadDataOnDemand,load data on demand}, we may not know if a folder
    // has any descendants if it has never been opened. As such we will show the
    // opener icon next to the folder. Once the user opens the icon and a fetch occurs,
    // if the folder is empty, and this property is false, the opener icon
    // will be hidden.
    // <P>
    // For more information on load on demand trees, and how we determine whether
    // a node is a a folder or a leaf, please refer to the +link{group:treeDataBinding}
    // documentation.
    //
    // @visibility external
    //<
    alwaysShowOpener:false,

    //>	@attr	treeGrid.openerImage        (SCImgURL : "[SKIN]opener.gif" : [IR])
    // The base filename of the opener icon for the folder node when 'showConnectors' is false
    // for this TreeGrid.<br>
    // The opener icon is displayed beside the folder icon in the Tree column for folder nodes.
    // Clicking on this icon will toggle the open state of the folder.<br>
    // The filenames for these icons are assembled from this base filename and the state of the
    // node, as follows:<br>
    // If the openerImage is set to <code>{baseName}.{extension}</code>, 
    // <code>{baseName}_opened.{extension}</code> will be displayed next to opened folders, and
    // <code>{baseName}_closed.{extension}</code> will be displayed next to closed folders, or
    // if this page is in RTL mode, <code>{baseName}_opened_rtl.{extension}</code> and
    // <code>{baseName}_closed_rtl.{extension}</code> will be used.
    //
    // @group treeIcons
    //      @visibility external
    //<
	openerImage:"[SKIN]opener.gif",
    

    //>	@attr	treeGrid.connectorImage        (SCImgURL : "[SKIN]connector.gif" : [IR])
    // The base filename for connector icons shown when +link{TreeGrid.showConnectors} is true.
    // Connector icons are rendered into the title field of each row and show the dotted
    // hierarchy lines between siblings of the same parent node. For each node, a connector icon
    // may be shown:<ul>
    // <li>As an opener icon for folder nodes, next to the folder icon</li>
    // <li>In place of an opener icon for leaf nodes, next to the leaf icon</li>
    // <li>As a standalone vertical continuation line in the indent to the left of the node, to show
    //     a connection between some ancestor node's siblings (only relevant if
    //     +link{TreeGrid.showFullConnectors} is true).</li>
    // </ul>
    // Note that +link{TreeGrid.showFullConnectors} governs whether connector lines will be
    // displayed for all indent levels, or just for the innermost level of the tree.
    // <P>
    // The filenames for these icons are assembled from this base filename and the state of the
    // node.  Assuming the connectorImage is set to <code>{baseName}.{extension}</code>, the
    // full set of images to be displayed will be:
    // <P>
    // <code>{baseName}_ancestor[_rtl].{extension}</code> if +link{TreeGrid.showFullConnectors}
    //  is true, this is the URL for the vertical continuation image to be displayed at the
    //  appropriate indent levels for ancestor nodes with subsequent children.
    // <P>
    // For nodes with no children:
    // <ul>
    // <li><code>{baseName}_single[_rtl].{extension}</code>: Shown when there is no connector line
    //  attached to the parent or previous sibling, and no connector line to the next sibling. For
    //  +link{TreeGrid.showFullConnectors,showFullConnectors:true} trees, there will always be a
    //  connector leading to the parent or previous sibling if its present in the tree so this
    //  icon can only be displayed for the first row.</li>
    // <li><code>{baseName}_start[_rtl].{extension}</code>:  Shown when the there is no connector
    //  line attached to the parent or previous sibling, but there is a connector to the next
    //  sibling. As with <code>_single</code> this will only ever be used for the first row if
    //  +link{TreeGrid.showFullConnectors} is true</li>
    // <li><code>{baseName}_end[_rtl].{extension}</code>:  Shown if we are not showing a connector 
    //  line attached to the next sibling of this node (but are showing a connection to the previous
    //  sibling or parent).</li>
    // <li><code>{baseName}_middle[_rtl].{extension}</code>:  Shown where the we have a connector
    //  line leading to both the previous sibling (or parent) and the next sibling.
    // </ul>
    // For folders with children. Note that if +link{TreeGrid.showFullConnectors} is false, open
    // folders will never show a connector to subsequent siblings:
    // <ul>
    // <li><code>{baseName}_opened_single[_rtl].{extension}</code> opened folder node with 
    //  children when no connector line is shown attaching to either the folder's previous sibling
    //  or parent, or to any subsequent siblings.</li>
    // <li><code>{baseName}_opened_start[_rtl].{extension}</code>:  opened folder with children
    //  when the there is no connector line attached to the parent or previous sibling, but there 
    //  is a connector to the next sibling.</li>
    // <li><code>{baseName}_opened_end[_rtl].{extension}</code>:  opened folder with children 
    //  if we are not showing a connector line attached to the next sibling of this node (but are
    //  showing a connection to the previous sibling or parent).</li>
    // <li><code>{baseName}_opened_middle[_rtl].{extension}</code>: opened folder with children 
    //  where the we have a connector line leading to both the previous sibling (or parent) and the
    //  next sibling.
    // </ul>
    // <ul>
    // <li><code>{baseName}_closed_single[_rtl].{extension}</code> closed folder node with 
    //  children when no connector line is shown attaching to either the folder's previous sibling
    //  or parent, or to any subsequent siblings.</li>
    // <li><code>{baseName}_closed_start[_rtl].{extension}</code>:  closed folder with children
    //  when the there is no connector line attached to the parent or previous sibling, but there 
    //  is a connector to the next sibling.</li>
    // <li><code>{baseName}_closed_end[_rtl].{extension}</code>:  closed folder with children 
    //  if we are not showing a connector line attached to the next sibling of this node (but are
    //  showing a connection to the previous sibling or parent).</li>
    // <li><code>{baseName}_closed_middle[_rtl].{extension}</code>: closed folder with children 
    //  where the we have a connector line leading to both the previous sibling (or parent) and the
    //  next sibling.
    // </ul>
    // (Note '[_rtl]' means that "_rtl" will be attached if isRTL() is true for this widget).
    // @group treeIcons
    //      @visibility external
    //<
	connectorImage:"[SKIN]connector.gif",

    //>	@attr treeGrid.offlineNodeMessage (String : "This data not available while offline" : [IRW])
    // For TreeGrids with loadDataOnDemand: true, a message to show the user if an attempt is 
    // made to open a folder, and thus load that node's children, while we are offline and 
    // there is no offline cache of that data.  The message will be presented to the user in 
    // in a pop-up dialog box.
    // 
    // @visibility external
    // @group offlineGroup, i18nMessages
    // @see dataBoundComponent.offlineMessage
    //<
    offlineNodeMessage: "This data not available while offline",
    
    //> @attr treeGrid.indentRecordComponents (Boolean : true : IRW)
    // For record components placed "within" the +link{TreeGridField.treeField,treeField}
    // column, should the component be indented to the position where a title would normally
    // show?
    // <P>
    // For more general placement of embedded components, see
    // +link{ListGrid.addEmbeddedComponent, addEmbeddedComponent}.
    // 
    // @visibility external
    //<
    indentRecordComponents: true,

    //> @attr treeGrid.createDefaultTreeField (Boolean : true : IR)
    // If no fields are specified, create a single field with 
    // +link{treeGridField.treeField} set to <code>true</code> to show the tree.
    // <P>
    // This automatically generated field will display values derived by calling
    // +link{treeGrid.getNodeTitle()}, and have the column title set to the specified
    // +link{TreeGrid.treeFieldTitle}.
    // <P>
    // Has no effect if fields are explicitly specified.
    // <P>
    // This is a convenience setting to allow a TreeGrid to be created without specifying a
    // field list.  If fields are specified, refer to the documentation on property
    // +link{treeGrid.autoAssignTreeField} for a way to automatically have one of the fields be
    // use as the tree field if no fields have +link{treeGridField.treeField} set.
    // <P>
    // For databound treeGrids, if there is no explicit fields array specified, developers
    // who wish to pick up all fields from the DataSource definition rather than displaying
    // this single automatically generated tree field may 
    // either set this property to false, or set +link{treeGrid.useAllDataSourceFields}
    // to <code>true</code>.
    //
    // @visibility external
    //<
    createDefaultTreeField: true,
    
    //> @attr treeGrid.useAllDataSourceFields (boolean : null : IRW)
    // @include listGrid.useAllDataSourceFields
    // @visibility external
    //<

    // Disble groupBy for TreeGrids altogether - we're already showing data-derived hierarchy!
    canGroupBy: false,
    
    
    ignoreEmptyCriteria: false,
 
    // users tend to navigate trees by opening and closing nodes more often than by scrolling,
    // so optimize for that use case.
    drawAllMaxCells:50,
    drawAheadRatio:1.0,
    
    // heavily used strings for templating
    _openIconIDPrefix: "open_icon_",
    _extraIconIDPrefix:"extra_icon_",
    _iconIDPrefix: "icon_",
    _titleField: "nodeTitle"

});

isc.TreeGrid.addMethods({

initWidget : function () {
	this.invokeSuper(isc.TreeGrid, this._$initWidget);
    
    // if no dataSource is specified on this TG, pick up the dataSource off the data model
    if (!this.dataSource && this.data != null && this.data.dataSource) {
        this.dataSource = this.data.dataSource;
    }

    if (this.createDefaultTreeField && 
        (this.getDataSource() == null || !this.useAllDataSourceFields))
    {
	    // if the fields are not set or of zero length, initialize with a single TREE_FIELD
        // NB: it is not safe to try to determine the tree field before setFields has been run,
        // since fields in this.fields might not be shown if they have a showIf:false
	    if (!this.fields || this.fields.length == 0) {
		    this.fields = [isc.TreeGrid.TREE_FIELD];
        }
    }
},

setDataSource : function (ds, fields) {
    if (this.createDefaultTreeField) {
        // if no fields were passed in, default to showing the tree field.  This matches the
        // behavior if a datbound treeGrid is initialized with no fields.
        if (fields == null || fields.length == 0) {
            fields = [isc.TreeGrid.TREE_FIELD];
        }
    }
    return this.Super("setDataSource", [ds, fields]);
},

cellValueHoverHTML : function (record, rowNum, colNum, defaultHTML) {
    if (colNum == this._treeFieldNum) {
        var returnVal = this.Super("cellValueHoverHTML", arguments);
        if (returnVal != null && !isc.isAn.emptyString(returnVal)) returnVal = "<nobr>" + returnVal + "</nobr>";
        return returnVal;

    // the clipped value hovers are only enabled by default for the tree field.
    } else if (this.showClippedValuesOnHover !== true) return null;
    else return defaultHTML;
},

// make sure one of the fields has been set up as the special "tree field"
_initTreeField : function () {

	// if the fields are not set or of zero length, initialize with a single TREE_FIELD
	if ((!this.fields || this.fields.length == 0) &&
	     (this.getDataSource() == null || !this.useAllDataSourceFields)) 
	{
        
        if (this.createDefaultTreeField) this.fields = [isc.TreeGrid.TREE_FIELD];
	} else {
    
        // see which field is the tree field.  Note this handles both the case that the special
        // constant TreeGrid.TREE_FIELD was provided as a field, and the case that the caller
        // marked a field as a the treeField.
        
        // if none of the fields is specified as the treeField, we look for a "title" field,
        // then we default to the first field in the array; we use this.completeFields so that
        // the treeField property of hidden fields will be checked as well--otherwise we would
        // default another field to be the tree field, and end up with more than one treeField
        // if the hidden treeField became visible again.
        
        var completeFields = this.completeFields,
            fields = this.fields,
            treeFieldNum;
        
        for (var i = 0; i < completeFields.length; i++) {
            if (completeFields[i].treeField) {
                treeFieldNum = fields.indexOf(completeFields[i]);
                break;
            }
        }

        if (treeFieldNum == null) {
            // if autoAssignTreeField has been set false, don't assign a default tree field in 
            // the absence of an explicit marker
            if (!this.autoAssignTreeField) return;
        
    	    // if there is no explicit marker, look for the field that matches the
            // titleProperty declared on the Tree
    	    var titleProp = this.data.titleProperty,
                fieldNum = fields.findIndex(this.fieldIdProperty, titleProp);
            if (fieldNum != -1) treeFieldNum = fieldNum;
        }

        // use the first field if none were marked as the tree field
        if (treeFieldNum == null) {
            treeFieldNum = 0;
            // Skip any fields where treeField is explicitly marked false
            // this includes auto-generated checkbox, rowNumber, etc fields
            while (this.fields[treeFieldNum] && 
                    this.fields[treeFieldNum].treeField == false) 
            {
                treeFieldNum++;
            }
            if (this.fields[treeFieldNum] == null) return;
        }

        // store the chosen fieldNum
        this._treeFieldNum = treeFieldNum;

        // use the properties of TREE_FIELD as defaults for the field
        // Note: We're manipulating the field object in the fields array. 
        // this.completeFields also contains a pointer to this object.
        // We don't want to replace the slot in either array with a different object as
        // that would make them out of synch (causes errors sorting, etc.)
        // - instead just copy any unset properties across from the TREE_FIELD field.
        var treeField = fields[treeFieldNum],
            fieldDefaults = isc.TreeGrid.TREE_FIELD,
            // Don't clobber explicit formatCellValue() / displayField with our
            // custom node title logic.
            hasCustomCellValue = treeField.formatCellValue != null
                             || treeField.displayField != null;
        for (var property in fieldDefaults) {
            if (hasCustomCellValue && property == "getCellValue") {
                continue;
            }
            if (treeField[property] == null) {
                treeField[property] = fieldDefaults[property]
            }
        }
    }
},

// because we store _treeFieldNum as a number, we need to recalc when fields are changed or
// their numbering changes.  This include setFields(), reorderFields(), showField() and hideField().
// 
// Note that the chosen treeField won't shift on reorder, because we install the TREE_FIELD
// properties into the chosen field, and the TREE_FIELD properties includes the treeField:true
// marker.
deriveVisibleFields : function (a,b,c,d) {
    this.invokeSuper(isc.TreeGrid, "deriveVisibleFields", a,b,c,d);
    this._initTreeField();
},

getEmptyMessage : function () {

    if (this.isOffline()) {
        return this.offlineMessage;
    }
    
    // can't just check for data != null because ListGrid initWidget sets data to [] if unset
    // and we must make sure we have a tree.
    if (isc.isA.Tree(this.data) && this.data.getLoadState(this.data.getRoot()) == isc.Tree.LOADING) 
        return this.loadingDataMessage == null ? "&nbsp;" 
                                : this.loadingDataMessage.evalDynamicString(this, {
            loadingImage: this.imgHTML(isc.Canvas.loadingImageSrc, 
                                       isc.Canvas.loadingImageSize, 
                                       isc.Canvas.loadingImageSize)
        });
    return this.emptyMessage.evalDynamicString(this, {
        loadingImage: this.imgHTML(isc.Canvas.loadingImageSrc, 
                                   isc.Canvas.loadingImageSize, 
                                   isc.Canvas.loadingImageSize)
    });
},

isEmpty : function () {
    // can't just check for data != null because ListGrid initWidget sets data to [] if unset
    // and we must make sure we have a tree.
    if (!isc.isA.Tree(this.data)) return true;

    var root = this.data.getRoot();
    if (root == null) return true;

    var rootHasChildren = this.data.hasChildren(root);
    if (rootHasChildren || this.showRoot || this.data.showRoot) return false;
    return true;
},

// Folder Animation
// ---------------------------------------------------------------------------------------
// Because of grouping, the implementation of all of these properties is actually on ListGrid, but is
// re-doc'd here for clarity

//> @attr treeGrid.animateFolders (Boolean : true : IRW)
// If true, when folders are opened / closed children will be animated into view.
// @group animation
// @visibility animation
// @example animateTree
//<

//> @attr treeGrid.animateFolderMaxRows (integer : null : IRW)
// If +link{animateFolders} is true for this grid, this number can be set to designate
// the maximum number of rows to animate at a time when opening / closing a folder.
// @see getAnimateFolderMaxRows()
// @group animation 
// @visibility external
//<

//> @attr treeGrid.animateFolderTime (number : 100 : IRW)
// When animating folder opening / closing, if +link{treeGrid.animateFolderSpeed} is not
// set, this property designates the duration of the animation in ms.
// @group animation
// @visibility animation
// @see treeGrid.animateFolderSpeed
//<

//> @attr treeGrid.animateFolderSpeed (number : 3000 : IRW)
// When animating folder opening / closing, this property designates the speed of the
// animation in pixels shown (or hidden) per second. Takes precedence over the 
// +link{treeGrid.animateFolderTime} property, which allows the developer to specify a
// duration for the animation rather than a speed.
// @group animation
// @visibility animation
// @see treeGrid.animateFolderTime
// @example animateTree
//<    
    
//> @attr treeGrid.animateFolderEffect (AnimationAcceleration : null : IRW)
// When animating folder opening / closing, this property can be set to apply an
// animated acceleration effect. This allows the animation speed to be "weighted", for
// example expanding or collapsing at a faster rate toward the beginning of the animation
// than at the end.
// @group animation
// @visibility animation
//<    

//> @attr treeGrid.animateRowsMaxTime (number : 1000 : IRW)
// If animateFolderSpeed is specified as a pixels / second value, this property will cap
// the duration of the animation.
// @group animation
// @visibility animation_advanced
//<    

//> @method treeGrid.shouldAnimateFolder ()
// Should this folder be animated when opened / closed? Default implementation will
// return true if +link{treeGrid.animateFolders} is true, the folder being actioned
// has children and the child-count is less than the result of
// +link{treeGrid.getAnimateFolderMaxRows}.
// @group animation
// @param folder (TreeNode) folder being opened or closed.
// @return (boolean) returns true if folders should be animated when opened / closed.
// @visibility external
//<

//> @method treeGrid.getAnimateFolderMaxRows() [A]
// If +link{animateFolders} is true for this treeGrid, this method returns the 
// the maximum number of rows to animate at a time when opening / closing a folder.
// This method will return +link{treeGrid.animateFolderMaxRows} if set. Otherwise
// the value will be calculated as 3x the number of rows required to fill a viewport,
// capped at a maximum value of 75.
// @return (integer) maximum number of rows to be animated when opening or closing folders.
// @group animation
// @visibility external
//<

// View State 
// ---------------------------------------------------------------------------------------
 
//> @type TreeGridOpenState  
// An object containing the open state for a treeGrid.
// Note that this object is not intended to be interrogated directly, but may be stored 
// (for example) as a blob on the server for state persistence across sessions.
// 
// @group viewState
// @visibility external
//<
// TreeGridOpenState object is implemented as an array of strings, each of which is the path
// to a currently open folder (all other folders are closed)


//> @method treeGrid.getOpenState() 
// Returns a snapshot of the current open state of this grid's data as
// a +link{type:TreeGridOpenState} object.<br>
// This object can be passed to +link{treeGrid.setOpenState()} to open the same set of folders
// within the treeGrid's data (assuming the nodes are still present in the data).
// @return (TreeGridOpenState) current open state for the grid.
// @group viewState
// @see treeGrid.setOpenState()
// @visibility external
//<
getOpenState : function () {
    var tree = this.data;
    if (tree == null) {
        this.logWarn("getOpenState() called for a treeGrid with no data");
        return [];
    }
    // Defer to ResultTree if available
    if (tree.getOpenState) return tree.getOpenState();

    // Must be using a static Tree
    var root = tree.getRoot(),
        openState = [];
        
    this._addNodeToOpenState(tree, root, openState);
    return isc.Comm.serialize(openState);
},  
// _addNodeToOpenState implemented in ListGrid
// Used for groupTree open/closed state maintenance logic
    
//>	@method	treeGrid.setOpenState() 
// Reset this set of open folders within this grid's data to match the 
// +link{type:TreeGridOpenState} object passed in.<br>
// Used to restore previous state retrieved from the grid by a call to 
// +link{treeGrid.getOpenState()}.
//
// @param openState (TreeGridOpenState) Object describing the desired set of open folders.
// @group viewState
// @see treeGrid.getOpenState()
// @visibility external
//<
setOpenState : function (openState) {
    // Defer to ResultTree if available
    if (this.data && this.data.setOpenState) {
        this.data.setOpenState(openState);
        return;
    }
    // Must be using a static Tree
    openState = this.evalViewState(openState, "openState")
    if (!openState) return;
    
    if (!this.data) {
        this.logWarn("unable to set open state for this treeGrid as this.data is unset");
        return;
    }
    this.data.closeAll();
    this.data.openFolders(openState);
},

//>	@method	treeGrid.getSelectedPaths() 
// Returns a snapshot of the current selection within this treeGrid as 
// a +link{type:ListGridSelectedState} object.<br>
// This object can be passed to +link{treeGrid.setSelectedPaths()} to reset this grid's selection
// the current state (assuming the same data is present in the grid).<br>
// @group viewState
// @see treeGrid.setSelectedPaths();
// @visibility external
// @return (ListGridSelectedState) current state of this grid's selection
//<
getSelectedPaths : function () {
    if (!this.selection) return null;

    var selection = this.selection.getSelection(),
        selectionLength = selection.length,
        selectedPaths = [];

    // store paths only.
    for (var i = 0; i < selectionLength; ++i) {
        selectedPaths[i] = this.data.getPath(selection[i]);
    }
    return isc.Comm.serialize(selectedPaths);
},


// ----------------------------------------------------------------------------
// panelHeader related methods

showActionInPanel : function (action) {
    return this.Super("showActionInPanel", arguments);
},


//>	@method	treeGrid.setSelectedPaths() 
// Reset this grid's selection to match the +link{type:ListGridSelectedState} object passed in.<br>
// Used to restore previous state retrieved from the grid by a call to 
// +link{treeGrid.getSelectedPaths()}.
//
// @group viewState
// @param selectedPaths (ListGridSelectedState) Object describing the desired selection state of
//                                              the grid
// @see treeGrid.getSelectedPaths()
// @visibility external
//<
setSelectedPaths : function (selectedPaths) {
    selectedPaths = this.evalViewState(selectedPaths, "selectedPaths")
    if (!selectedPaths) return;
    
    var selection = this.selection, data = this.data;
    if (data && selection) {
        selection.deselectAll();
        var nodes = [];
        // use find to look up node by path
        for (var i = 0; i < selectedPaths.length; i++) {
            var node = data.find(selectedPaths[i]);
            if (node) nodes.add(node);
        }
        this.selection.selectList(nodes);
        this.fireSelectionUpdated();
    }
},

//> @type   TreeGridViewState  
// An object containing the "view state" information for a treeGrid. In addition to the 
// state data contained by a +link{type:ListGridViewState} object, this will also contain the 
// current open state of the treeGrid in question.<br>
// Note that this object is not intended to be interrogated directly, but may be stored 
// (for example) as a blob on the server for view state persistence across sessions.
// 
// @group viewState
// @visibility external
//<
// TreeGridViewState object is implemented as a simple JS object containing the following 
// fields:
// - selected [an (undocumented) treeGridSelectedState object - an array of selected nodes' paths]
// - field [a ListGridFieldState object]
// - sort [a ListGridSortState object]
// - open [a TreeGridOpenState object]

//>	@method	treeGrid.getViewState() 
// Overridden to return a +link{type:TreeGridViewState} object for the grid.
// @return (TreeGridViewState) current view state for the grid.
// @group viewState
// @see type:TreeGridViewState
// @see treeGrid.setViewState();
// @visibility external
//<    
getViewState : function () {
    var state = this.Super("getViewState", [true]);
    state.open = this.getOpenState();
    return "(" + isc.Comm.serialize(state) + ")";
},
    

//>	@method	treeGrid.setViewState() 
// Overridden to take a +link{type:TreeGridViewState} object.
//
// @param viewState (TreeGridViewState) Object describing the desired view state for the grid
// @group viewState
// @see treeGrid.getViewState()
// @visibility external
//<    
setViewState : function (state) {

    // Ensure we set open state after setting sort state
    this.Super("setViewState", arguments);
    // don't bother warning on error - Super() will have done that already
    state = this.evalViewState(state, "viewState", true)
    if (!state) return;
    
    if (state.open) this.setOpenState(state.open);
    // Re-apply selection so that nodes just opened can be found.
    if (state.selected) this.setSelectedState(state.selected);
},


// if data is not specified, use an empty Tree.
getDefaultData : function () {
    // NOTE: initializing to a ResultTree would effectively trigger fetch on draw.  Don't want
    // to do this unless fetchData() is called (possibly via autoFetchData property), in which
    // case the empty starter Tree will be discarded and replaced by a ResultTree
    //if (this.dataSource) return this.createResultTree();
    return isc.Tree.create({_autoCreated:true});
},

//>	@method	treeGrid.setData()
// Set the +link{class:Tree} object this TreeGrid will view and manipulate.
//
// @param newData (Tree) Tree to show
// @visibility external
//<
setData : function (newData) {
    if (this.data) {
        if (this.separateFolders != null) this.data.setSeparateFolders(this.separateFolders);
        if (this.sortFoldersBeforeLeaves != null) {
            this.data.setSortFoldersBeforeLeaves(this.sortFoldersBeforeLeaves);
        }
    }

    this.Super("setData", arguments);
    if (!isc.isA.Tree(this.data)) return;

    // Set the `separateFolders` and `showRoot` options of the tree as well.
    if (this.separateFolders != null) this.data.setSeparateFolders(this.separateFolders);
    if (this.sortFoldersBeforeLeaves != null) {
        this.data.setSortFoldersBeforeLeaves(this.sortFoldersBeforeLeaves);
    }

    if (this.showRoot && isc.ResultTree && isc.isA.ResultTree(this.data)) {
        this.logWarn("showRoot may not be set with a databound treeGrid, unexpected " +
                     "results may occur");
    }
    this.data.setShowRoot(this.showRoot);

    // should we show only branches or leaves
    this.data.setOpenDisplayNodeType(this.displayNodeType);

    if (this.autoPreserveOpenState != null) {
        this.data.autoPreserveOpenState = this.autoPreserveOpenState
    }
},

draw : function (a,b,c,d) {
    
    if (this.initialData && (!isc.ResultSet || !isc.isA.ResultSet(this.data)) &&
        (!this.data || !isc.isA.ResultTree(this.data))) {
        this.setData(this.createResultTree());
    }
    
    this.invokeSuper(isc.TreeGrid, "draw", a,b,c,d);
},

bodyConstructor:"TreeGridBody",

// Override bodyKeyPress to handle open and closing of trees
// Note: standard windows behavior with Left and Right arrow key presses in a treeGrid is:
// - multiple selection seems to *always* be disallowed, so doesn't come into play
// - arrow right on a closed folder will open the folder
// - arrow right on an open folder (with content) will move selection to the first child node
// - arrow left on an open folder will close the folder
// - arrow left on a node within a folder will move selection to the node's parent folder

bodyKeyPress : function (event) {

    // if exactly one record is selected, mimic windows LV behaviors for arrow left and right
    var selection = this.selection;
    if (this.selectionType != isc.Selection.NONE && 
        this.data.getLength() > 0 &&
        selection.anySelected() &&
        // no multipleSelected on CellSelection
        (!selection.multipleSelected || !selection.multipleSelected()))
    {
        var node = this.getRecord(this.getFocusRow());

        // Left/right arrow key interaction if we can expand records:
        // - allow first right arrow to open a folder
        // - allow second right arrow to expand a folder
        // - allow third right arrow to navigate into the the folder
        // 
        // - allow first left arrow to collapse a 'canExpand' folder
        // - allow second left arrow to close a folder
        // - allow third left arrow to navigate up to parent
        // Expand / collapse is implemented at the ListGrid level.
        if (event.keyName == "Arrow_Left") {
            if (!this.canExpandRecords || 
                       !(this._canExpandRecord(node,this.getFocusRow()) 
                         && this.isExpanded(node))
                        ) 
            {
                if (this.data.isFolder(node) && this.data.isOpen(node)) {
                    this.closeFolder(node);
                    return false;
                } else 
                {
                    // if node is open and has parent, iterate over nodes, until we
                    // reach to parent then navigate to that record directly.
                    var parent = this.data.getParent(node);
                    if (parent) {
                        var row = this.getFocusRow();
                        while(row>0 && this.getRecord(row) != parent) {
                            row--;
                        }

                        // if parent was found, navigate to it
                        if (this.getRecord(row) == parent) {
                            this._navigateToNextRecord(row-this.getFocusRow(),false);
                            return false;
                        }
                    }
                }
            }
            
        } else if (event.keyName == "Arrow_Right") {
            if (this.data.isFolder(node)) {
                if (!this.data.isOpen(node)) {
                    this.openFolder(node);
                    return false;
                // If we're collapsed, allow left arrow to collapse, before navigating!
                } else if (!this.canExpandRecords || 
                           !(this._canExpandRecord(node,this.getFocusRow()) 
                             && !this.isExpanded(node))
                            ) 
                {
                    if (this.data.hasChildren(node)) {
                        this._navigateToNextRecord(1,false);
                        return false;
                    }
                }
            } 
        }
    }
    
    return this.Super("bodyKeyPress", arguments);
    
},


// fire synthetic context menu events for nodes
_cellContextClick : function (record, recordNum, fieldNum) {

	if (recordNum < 0 || fieldNum < 0) return true; // not in body, allow native context menu

	var isFolder = this.data.isFolder(record);
    
    // fire synthetic context click events.  Note any of these can cancel further processing by
    // returning an explicit false, which presumably indicates they've shown a context menu
    if (this.nodeContextClick && this.nodeContextClick(this, record, recordNum) == false) {
        return false;
    }
    if (isFolder) {
        if (this.folderContextClick && this.folderContextClick(this, record, recordNum) == false) {
            return false;
        }
    } else {
        if (this.leafContextClick && this.leafContextClick(this, record, recordNum) == false) {
            return false;
        }
    }

    // fire the superclass implementation of this method to fire 'cellContextClick', if defined,
    // and show the default context menu if appropriate
    return this.Super("_cellContextClick", arguments);
	
},

//>	@method	treeGrid.handleEditCellEvent()
//		@group	event handling	
//			Override handleEditCellEvent to not allow editing if the click / doubleClick event 
//          occurred over the open area of the treeGrid
//
//		@return	(boolean)	false == cancel further event processing
//<
handleEditCellEvent : function (recordNum, fieldNum) {

    var record = this.getRecord(recordNum);
    // if they're clicking in the open or checkbox area of the list,
    // don't allow editing to proceed
    if (this.clickInOpenArea(record) || this.clickInCheckboxArea(record)) return false;
	
	// return the results of a call to the superclass method
	return this.Super("handleEditCellEvent",arguments);
},

//>	@method     treeGrid.canEditCell()	
// Overridden to disallow editing of the +link{treeNode.name, name} field of this grid's data
// tree. Also disallows editing of the auto-generated tree field, which displays the result
// of +link{method:Tree.getTitle} on the node.
// @return (Boolean) Whether to allow editing this cell
// @visibility external
//<
canEditCell : function (rowNum, colNum) {

    if (this.Super("canEditCell", arguments) == false) return false;

    
    if (this.getField(colNum)[this.fieldIdProperty] == this.data.nameProperty) return false;
    
    
    if (this.getField(colNum)[this.fieldIdProperty] == this._titleField) return false;

    return true;
},

//> @method treeGrid.startEditingNew()
// This inherited +link{listGrid.startEditingNew,ListGrid API} is not supported by the TreeGrid
// since adding a new tree node arbitrarily at the end of the tree is usually not useful.
// Instead, to add a new tree node and begin editing it, use either of these two strategies:
// <ol>
// <li> add a new node to the client-side Tree model via +link{Tree.add()}, then use
// +link{listGrid.startEditing(), TreeGrid.startEditing()} to begin editing this node.  Note that if using a DataSource, when the
// node is saved, an "update" operation will be used since adding a node directly to the
// client-side +link{ResultTree} effectively means a new node has been added server side.
// <li> use +link{DataSource.addData()} to immediately save a new node.  Automatic cache sync
// by the +link{ResultTree} will cause the node to be integrated into the tree.  When the
// callback to addData() fires, locate the new node by matching primary key and call
// +link{listGrid.startEditing(), TreeGrid.startEditing()} to begin editing it.
// </ol>
//
// @group  editing
//
// @param  [newValues] (object)  Optional initial set of properties for the new record
// @param  [suppressFocus] (boolean) Whether to suppress the default behavior of moving focus
//                                   to the newly shown editor.
// @visibility external
//<

// Override the method to determine the widths of the form items displayed while editing to
// account for the tree-field indents
getEditFormItemFieldWidths : function (record) {

    var level = this.data.getLevel(record);
    if (!this.showRoot) level--;
    var openerIconSize = this.getOpenerIconWidth(record),
        indentSize = level * (this.showConnectors ? openerIconSize : this.indentSize)
    ;
    indentSize += this.iconSize + openerIconSize;
    if (this._getCheckboxIcon(record)) {
        indentSize += (this._getCheckboxFieldImageWidth() + this.extraIconGap);
    } else if (this.getExtraIcon(record)) {
        indentSize += (this.iconSize + this.extraIconGap);
    }
    
    var widths = this.Super("getEditFormItemFieldWidths", arguments),
        treeFieldNum = this.getTreeFieldNum();

    widths[treeFieldNum] -= indentSize;
    return widths;
},

// return the DataSource for the current record, to allow embedded editing
getRecordDataSource : function (record) {
    return this.data.getNodeDataSource(record);
},

//>	@method	treeGrid.rowClick()
//
// This override to +link{ListGrid.rowClick()}.  This implementation calls through to the
// +link{TreeGrid.nodeClick}, +link{TreeGrid.folderClick}, +link{TreeGrid.leafClick} methods, as
// appropriate unless the click was on the expand/collapse control of a folder - in which case
// those callbacks are not fired.
// <p>
// Do not override this method unless you need a rowClick callback that fires even when the
// user clicks on the expand/collapse control.  If you do override this method, be sure to call
// <code>return this.Super("rowClick", arguments);</code> at the end of your override to
// preserver other handler that are called from the superclass (for example,
// +link{ListGrid.recordClick()}.
// <p>
//
//      @param  record      (TreeNode)    record that was clicked on
//		@param	recordNum   (number)	index of the row where the click occurred
//		@param	fieldNum	(number)	index of the col where the click occurred
//
// @see TreeGrid.nodeClick()
// @see TreeGrid.folderClick()
// @see TreeGrid.leafClick()
// @see ListGrid.recordClick()
//
// @visibility external
//<

rowClick : function (record, recordNum, fieldNum) {

    var node = record;

	// if the're clicking in the open or checkbox area of the list, 
	//  it's already been processed properly on mouseDown so just bail
	if (this.clickInOpenArea(node) || this.clickInCheckboxArea(node)) return false;
	
	this._lastRecordClicked = recordNum;
	if (recordNum < 0 || fieldNum < 0) return false; // not in body
	
	var node = this.getRecord(recordNum),
        isFolder = this.data.isFolder(node);

    if (this.nodeClick) this.nodeClick(this, node, recordNum);

    if (isFolder) {
        if (this.folderClick) this.folderClick(this, node, recordNum);
    } else {
        if (this.leafClick) this.leafClick(this, node, recordNum);
    }
	
	// execute the super class click method - to pick up field click and recordClick
    // Note: be sure not to call any handlers the ListGrid will call so as not to get a dup
	return this.Super("rowClick",arguments);
},



//>	@method	treeGrid.recordDoubleClick()
//
// Handle a doubleClick on a tree node - override of ListGrid stringMethod of same name.  If
// the node is a folder, this implementation calls +link{TreeGrid.toggleFolder()} on it.  If
// the node is a leaf, calls +link{TreeGrid.openLeaf()} on it.
// 
// @see listGrid.recordDoubleClick()
// @visibility external
//<
recordDoubleClick : function (viewer, record, recordNum, field, fieldNum, value, rawValue) {
	// if the're clicking in the open or checkbox area of the list, 
	//  it's already been processed properly on mouseDown so just bail
	if (this.clickInOpenArea(record) || this.clickInCheckboxArea(record)) return false;
	// If this is an editable grid, don't toggle the folder, but do return true to allow
	// editing to proceed.
	if (this.isEditable() && this.editEvent == isc.EH.DOUBLE_CLICK &&
	    this.canEditCell(recordNum,fieldNum)) 
	{
	    return true;
	}
	if (this.data.isFolder(record)) {
		return this.toggleFolder(record);
	} else
		return this.openLeaf(record);
},
    
dataChanged : function () {

    this.Super("dataChanged", arguments);

    
    var folder = this._pendingFolderAnim;
    if (folder && this.data.isOpen(folder) && 
        this.data.getLoadState(folder) == isc.Tree.LOADED) 
    {
        this._startFolderAnimation(folder);
        this._pendingFolderAnim = null;
    }
},
    

//>	@method	treeGrid.openLeaf()   ([A])
// Executed when a leaf node receives a 'doubleClick' event. This handler must be
// specified as a function, whose single parameter is a reference to the relevant leaf node in
// the tree's data.<br>
// See the ListGrid Widget Class for inherited recordClick and recordDoubleClick events.
//
//      @visibility external
//		@param	node		(TreeNode)		node to open
//      @see    class:ListGrid
//<
openLeaf : function (node) {},


// Drag and Drop
// ----------------------------------------------------------------------------------------

//>	@method	treeGrid.transferDragData()
// @include dataBoundComponent.transferDragData()
//<

// ----------------------------------------------------------------------------------
// Customizations of the drag-tracker for tree grids

//> @method treeGrid.getDragTrackerIcon()
// Return an icon to display as a drag tracker when the user drags some node(s).<br>
// Default implementation:<br>
// If multiple nodes are selected and +link{TreeGrid.manyItemsImage} is defined, this
// image will be returned.<br>
// Otherwise returns the result of +link{TreeGrid.getIcon()} for the first node being 
// dragged.
// <p>
// Note: Only called if +link{listGrid.dragTrackerMode} is set to <code>"icon"</code>. 
// @param records (Array of ListGridRecord) Records being dragged
// @return (string) Image URL of icon to display
// @group dragTracker
// @visibility external
//<
getDragTrackerIcon : function (records) {
    
    var icon;
    if (records && records.length > 1 && this.manyItemsImage !=null)
        icon = this.manyItemsImage;
    else if (records && records[0]) icon = this.getIcon(records[0], true);
    return icon;
},

// Override getDragTrackerTitle() to just return the icon and title of the row, not
// the indent, opener icon, etc.
// Override not currently documented as it's essentially the same as the superclass 
// implementation - we just reformat the title cell value to avoid it showing the
// indent and opener icon.
getDragTrackerTitle : function (record, rowNum, a,b,c,d) {
    var fieldNum = this.getFieldNum(this.getTitleField()); 
    if (fieldNum != this.getTreeFieldNum()) 
        return this.invokeSuper(isc.TreeGrid, "getDragTrackerTitle", record, rowNum, a,b,c,d);
    
    // We need to apply the base (non selected) standard cellStyle/cssText to the drag tracker 
    // table.
    
    var cellStyle = this.getCellStyle(record, rowNum, fieldNum),
        cellCSSText = this.getCellCSSText(record,rowNum,fieldNum);
        
    if (this.selection.isSelected(record)) {
        var styleIndex = this.body.getCellStyleIndex(record, rowNum, fieldNum),
            standardSelectedStyle = this.body.getCellStyleName(styleIndex, record, 
                                                                rowNum, fieldNum);
        if (standardSelectedStyle == cellStyle) {
            styleIndex -= 2;
            cellStyle = this.body.getCellStyleName(styleIndex, record, rowNum, fieldNum);
        }
    }
    
    
    // Call the standard ListGrid.getCellValue() method to give us the formatted title
    // of the cell being dragged, excluding the TreeGrid folder/file icons, etc.
    var value = this.invokeSuper(isc.TreeGrid, "getCellValue",  record, rowNum, fieldNum);
    
    // Now use _getTreeCellTitleArray() to tack on the icon for the node.
    var titleCell = this._getTreeCellTitleArray(
                        value, record, rowNum, fieldNum, false, cellStyle, cellCSSText ).join(isc.emptyString);
    
    
    return ["<table class='", cellStyle,
             "' style='", cellCSSText, 
             "'><tr>", titleCell, "</tr></table>"].join(isc.emptyString);
},




//>	@method	treeGrid.willAcceptDrop()	(A)
// 
// This method overrides +link{ListGrid.willAcceptDrop()} and works as follows:
// <br><br>
// First, +link{ListGrid.willAcceptDrop()} (the superclass definition) is consulted.  If it
// returns false, then this method returns false immediately.<br>
// This handles the following cases:<br>
// - reordering of records withing this TreeGrid when +link{ListGrid.canReorderRecords} is true<br>
// - accepting dropped records from another dragTarget when +link{ListGrid.canAcceptDroppedRecords} 
//   is true and the dragTarget gives us a valid set of records to drop into place.<br>
// - disallowing drop over disabled nodes, or nodes with <code>canAcceptDrop:false</code>
// <br>
// This method will also return false if the drop occurred over a leaf node whos immediate 
// parent has <code>canAcceptDrop</code> set to <code>false</code><br>
// If +link{TreeGrid.canReparentNodes} is true, and the user is dragging a node from one folder
// to another, this method will return true to allow the change of parent folder.<br>
// <br><br>
// Otherwise this method returns true.
//
// @group event handling	
// @return	(boolean)	true if this component will accept a drop of the dragData
//
// @visibility external
//<
willAcceptDrop : function () {
    // Bail if Superclass willAcceptDrop fails
    // (Checks that the record is enabled, etc.)
    var superAccept = this.Super("willAcceptDrop", arguments);
    if (!superAccept) return superAccept;

    isc._useBoxShortcut = true;
	// get the record being dropped on
	var recordNum = this.getEventRecordNum(null, true),
        newParent = recordNum < 0 ? null : this.data.get(recordNum);
    isc._useBoxShortcut = false;

    // dropping in the body in open space means add to root
    if (newParent == null) {
        if (this.canDropInEmptyArea == false || !this.canDropRootNodes) return false;
        newParent = this.data.getRoot();
    }

	// if we can't get the new parent, or it can't accept drops, return false
	if (!newParent || newParent.canAcceptDrop == false) return false;

    // don't allow drop over non-folder nodes, unless we're allowing record reordering or
    // canDropOnLeaves is set
    var isFolder = this.data.isFolder(newParent);
    if (!isFolder && !(this.canReorderRecords || this.canDropOnLeaves)) return false;

    // check for dropErrors (dropping record over self, etc.)
    var moveList = isc.EH.dragTarget.getDragData();
    if (!isc.isAn.Object(moveList) || this.getDropError(moveList, newParent) != null) {
        return false
    }
    // Even if we are allowing record reordering, don't allow the user to drop into a 
    // parent with canAcceptDrop explicitly set to false
    
    if (!isFolder) {
        newParent = this.data.getParent(newParent);
        if (newParent.canAcceptDrop == false) return false;
    }

    // If we're dragging data in from another listGrid we're done here
    // (this relies on canAcceptDropRecords getting checked by the superClass implementation
    // for this case).
    if (isc.EH.dragTarget != this) return true;
    
    // If we can reorder records, but not reparent, we need to catch the cases where
    // - records selected currently come from multiple folders
    // - the drop folder doesn't match the source folder for the node[s]
    var canReparent = this.canReparentNodes;
    //>!BackCompat 2006.06.27
    if (canReparent == null && this.canAcceptDroppedRecords) canReparent = true;
    //<!BackCompat
    
    if (!canReparent) {
        if (!isc.isAn.Array(moveList)) moveList = [moveList];
        var currentParent;
        currentParent = this.data.getParent(moveList[0]);

        if (currentParent != newParent) return false;
        
        for (var i = 1; i < moveList.length; i++) {
            if (currentParent != this.data.getParent(moveList[i])) return false;
        }
    }
    
	// if we get here, it should be OK!
	return true;
},

// Override setUpDragProperties to pick up this.canReparentNodes 
_setUpDragProperties : function () {

    // set up our specific drag-and-drop properties
	this.canDrag = (this.canDrag || this.canDragRecordsOut || 
                    this._canDragRecordsToSelf() || this.canDragSelect);
	this.canDrop = (this.canDrop || this.canDragRecordsOut || this._canDragRecordsToSelf());
	this.canAcceptDrop = (this.canAcceptDrop || this.canAcceptDroppedRecords || 
                            this._canDragRecordsToSelf());
},


// allow the user to drag records to self if they can be reordered or reparented
_canDragRecordsToSelf : function () {
    var canReparentNodes = this.canReparentNodes;
    //>!BackCompat 2006.06.27
    if (canReparentNodes == null && this.canAcceptDroppedRecords) {
        if (!this._canReparentBackcompatNotified) {
            this.logInfo("'canReparentNodes' is unset. Allowing node reparenting as " + 
                         "'canAcceptDroppedRecords' is set to true. For explicit control, " +
                         "use 'canReparentNodes' instead.", "dragDrop");
            this._canReparentBackcompatNotified = true;
        }
        canReparentNodes = this.canAcceptDroppedRecords;
    }
    //<!BackCompat
    
    return this.canReorderRecords || canReparentNodes;
},

// if there's a problem that makes this drop invalid, return an error string to display
getDropError : function (moveList, newParent) {

	// don't allow a parent to be dropped on it's own descendant
	for (var i = 0, length = moveList.length; i < length; i++) {
		if (this.data.isDescendantOf(newParent, moveList[i])) {
			return this.cantDragIntoChildMessage;
		}
	}

	// make sure they're not trying to drag a folder into itself
    var isFolder = this.data.isFolder(newParent);
    if (isFolder) {
    	for (i = 0; i < length; i++) {
	    	if (moveList[i] == newParent) {
		    	return this.cantDragIntoSelfMessage;
    		}
	    }
    }

    return null;
},

//>	@method	treeGrid.dropMove()	(A)
//			mouse is moving over the list while dragging is happening
//			show a hilite in the appropriate record if necessary
//		@group	event handling	
//
//		@return	(boolean)	false == cancel further event processing
//<
dropMove : function () {
    var eventRow = this.getEventRow();
    // before the beginning of the rendered area, aka over the header; do nothing
    if (eventRow == -1) return false;

    // bail on drops over foreign widgets if not configured to drag records out
    if (this.canDragRecordsOut == false && isc.EH.dropTarget != this && !this.contains(isc.EH.dropTarget)) return false;
    
    // bail on drops from foreign widgets if not configured to accept foreign drops
    if (!this.canAcceptDroppedRecords && isc.EH.dragTarget != this) {
        return false;
    }
    
    // if after the end of the list, choose root
    var eventNode = (eventRow == -2 ? this.data.getRoot() : this.data.get(eventRow)),
        dropFolder = this.getDropFolder(),
        position = (this.canReorderRecords ? this.getRecordDropPosition(eventRow) : null);
        
    // We used to check willAcceptDrop() here, but that prevented spring-loaded folders
    // from working in the case where the folder being hovered over is will not accept the
    // drop, but one of its children might accept the drop.  So now, we always set the
    // timer to open a folder being hovered on and updateDropFolder() logic checks for
    // willAcceptDrop().
    
    // suppress duplicate runs, but updateDropFolder() whenever the lastDropFolder, eventNode
    // or lastPosition have changed because event though we may still be within the same
    // dropFolder, we may want to change the dropFolder icon state based on whether the tree
    // willAcceptDrop() at the new location.
    if (dropFolder != this.lastDropFolder || 
        eventNode != this._lastEventNode || position != this._lastPosition) {

        // Set up a function to be executed in the global scope to open the drop folder.
        if (!this._openDropFolder) {
            this._openDropFolder = this.getID() + ".openDropFolder()";
        }

        // If there's a running openDropFolderTimer, clear it
        if (this.openDropFolderTimer) isc.Timer.clear(this.openDropFolderTimer);    
			
        // If the dropFolder is closed, set up a new openDropFolderTimer
        if (!this.data.isOpen(dropFolder)) {
            this.openDropFolderTimer = 
                            isc.Timer.setTimeout(this._openDropFolder, this.openDropFolderDelay);
        }

        // remember the new drop-folder as this.lastDropFolder, and update its icon.
        // [note this calls 'willAcceptDrop()']
        this.updateDropFolder(dropFolder);
    }

    // If the drop is disallowed, show the 'no drop' cursor
    
    if (!this.willAcceptDrop()) {
        this.body.setNoDropIndicator();
    } else {        
        this.body.clearNoDropIndicator();
    }
    
    // Show the drag line if appropriate
    if (this.shouldShowDragLineForRecord(dropFolder)) {
        if (this.data.isOpen(dropFolder)) this.showDragLineForRecord(eventRow, position);
        else this.hideDragLine();
    }
    this._lastEventNode = eventNode;
    this._lastPosition = position;

    
},

//>	@method	treeGrid.getEventRow()
// @include gridRenderer.getEventRow()
// @group events
// @visibility external
//<

    
getEventRecordNum : function (y, allowRootNodeRemapping) {
    var recordNum = this.Super("getEventRecordNum", arguments);
    if (recordNum < 0 && allowRootNodeRemapping && !this.canDropRootNodes) {
        var data = this.data,
            children = data.getChildren();
        if (children != null) {
            if (recordNum == -2) {
                for (var i = children.length - 1; i >= 0; i--) {
                    if (data.isFolder(children[i])) return this.getRecordIndex(children[i]);
                }
            } else {
                for (var i = 0; i < children.length; i++) {
                    if (data.isFolder(children[i])) return this.getRecordIndex(children[i]);
                }
            }
        }
    }
    return recordNum;
},

//> @attr treeGrid.recordDropAppearance (RecordDropAppearance : isc.ListGrid.BOTH : [IRW])
// If +link{canAcceptDroppedRecords} is true for this treeGrid, this property governs
// whether the user can drop between, or over records within the grid.
// This controls what +link{type:RecordDropPosition} is passed to the +link{recordDrop()}
// event handler.
//
// @visibility external
//<

recordDropAppearance: isc.ListGrid.BOTH,

//> @method treeGrid.getDropFolder()
// When the user is dragging a droppable element over this grid, this method returns the folder
// which would contain the item if dropped. This is the current drop node if the user is hovering
// over a folder, or the node's parent if the user is hovering over a leaf.
// @group events
// @return (node) target drop folder
// @visibility external
//<
getDropFolder : function () {

    var eventRow = this.getEventRecordNum(null, true),
        data = this.data,
        // before the beginning of the list (over header), or after the end, use root
        eventNode = (eventRow < 0 ? data.getRoot() : data.get(eventRow));
    
    // if we're over the root, we're going to drop into the root (no choice)
    if (data.isRoot(eventNode)) return data.getRoot();    
    
    var isFolder = data.isFolder(eventNode);

    // if we can't reorder records, it's easy
    if (!this.canReorderRecords) return (isFolder ? eventNode : data.getParent(eventNode));
    
    var position = this.getRecordDropPosition(eventNode);
    
    // If we're over a leaf (anywhere), or
    // we're over the "before" or "after" part (top / bottom 1/4) of any folder, or
    // we're over the "after" part (bottom 1/4) of a closed or empty folder, return the 
    // parent of the node,
    // except don't return the parent of a folder if the parent is the root node and
    // canDropRootNodes is false; return the node itself in that case.
    if (!isFolder || position == isc.ListGrid.BEFORE || position == isc.ListGrid.AFTER &&
        (!data.isOpen(eventNode) || !data.hasChildren(eventNode)))
    {
        var parent = data.getParent(eventNode);
        return isFolder && !this.canDropRootNodes && data.isRoot(parent) ? eventNode : parent;
    } else {
        // In this case we're either over the "over" position of a closed folder, or the 
        // "below" position for an open folder.  In either case we'll want to drop into this 
        // folder, before the first child
        return eventNode;
    }

},

//>	@method	treeGrid.openDropFolder()	(A)
// Method to open the folder we're currently hovering over (about to drop)
// Called on a timer set up by this.dropMove
//		@group	event handling	
//<
openDropFolder : function () {
    var dropFolder = this.lastDropFolder;
    
    // if we're not over a closed folder, bail!
    if (!dropFolder || 
        !this.data.isFolder(dropFolder) || 
        this.data.isOpen(dropFolder))          return false;

    // Open the folder
    this.openFolder(dropFolder);
    // show the drag line if we can reorder
    if (this.shouldShowDragLineForRecord(dropFolder)) {
        this.showDragLineForRecord(this.data.indexOf(dropFolder), isc.ListGrid.OVER)
    }
    
},

getRecordDropPosition : function (recordNum, y, dropAppearance) {
    if (this.recordDropAppearance == isc.ListGrid.OVER) return isc.ListGrid.OVER;
    if (this.recordDropAppearance == isc.ListGrid.BODY) return null;

    // If a y-coordinate was not passed, get it from the offset of the last event
    if (y == null) y = this.body.getOffsetY();

    // which row is the mouse over?
    if (recordNum == null) recordNum = this.getEventRow(y);

    var data = this.data;
    if (!isc.isA.Number(recordNum)) recordNum = data.indexOf(recordNum);

    var record = recordNum < 0 ? null : data.get(recordNum);

    if (record && data.isFolder(record)) {
        return this._getRecordDropPosition(recordNum, y, dropAppearance);
    }

    // if no recordDropAppearance override was supplied, force it to BETWEEN
    // unless +link{TreeGrid.canDropOnLeaves} has been set to true
    if (dropAppearance == null && !this.canDropOnLeaves) {
        dropAppearance = isc.ListGrid.BETWEEN;
    }
        
    // If we're over a leaf, allow the super method to take care of it.
    return this.invokeSuper(isc.TreeGrid, "getRecordDropPosition", recordNum, y, 
                            dropAppearance);
},

// Override showDragLineFor record - if the drop will occur inside a folder, we'll show the
// drag line after the folder (before the first child)
showDragLineForRecord : function (recordNum, position, a,b,c) {

    if (recordNum == null) recordNum = this.getEventRecordNum();    
    if (position == null) position = this.getRecordDropPosition(recordNum);

    // If dropping over an open folder, show the drag line before the first child (after the
    // folder)    
    if (position == isc.ListGrid.OVER) {
        var node = this.getRecord(recordNum),
            data = this.data;
        if (data.isFolder(node) && data.isOpen(node)) position = isc.ListGrid.AFTER;
    }
    
    // Have the default implementation actually show the drag line.
    return this.invokeSuper(isc.TreeGrid, "showDragLineForRecord", recordNum, position, a,b,c);
},


//>	@method	treeGrid.dropOut()	(A)
//			mouse just moved out of the range of the list while dragging is going on
//			remove the hilite
//		@group	event handling	
//
//		@return	(boolean)	false == cancel further event processing
//<
dropOut : function () {
    // Hide drag line
    this.hideDragLine();
    // clear no-drop indicator
    this.body.clearNoDropIndicator();
    
    // Clear any remembered drop folder
    this._lastEventNode = null;
    this.updateDropFolder();
    
    // If we have a timer waiting to open a drop folder, clear it
    // (Note - if it did fire it would bail anyway because lastDropMoveRow has gone, but
    // this is more efficient)
    if (this.openDropFolderTimer) isc.Timer.clear(this.openDropFolderTimer);    
    
},

//>	@method	treeGrid.updateDropFolder()	(A)
// Takes a record (or record index) as a parameter
// Applies the folderDropImage icon to the parameter (or parent folder, if passed a leaf)
// Clears out any folderDropImage applied to another folder.
// Remembers the folder passed in as this.lastDropFolder.
//		@group	drawing, event handling
//
//		@param newFolder (object or index)
//<
updateDropFolder : function (newFolder) {

    var LDF = this.lastDropFolder;
    this.lastDropFolder = newFolder;

    // Set the icons on both the previous and current drop folder
    //
    // Special _willAcceptDrop flag: set for getIcon() and only update to drop state if the
    // body willAcceptDrop() the new folder - see comments in dropMove()
    if (newFolder) {
        newFolder._willAcceptDrop = this.body.willAcceptDrop(newFolder)
		this.setRowIcon(newFolder, this.getIcon(newFolder));
    }

    if (LDF && LDF != newFolder) {
        delete LDF._willAcceptDrop;   
        this.setRowIcon(LDF, this.getIcon(LDF));
    }
},


//> @method treeGrid.transferSelectedData()
// Simulates a drag / drop type transfer of the selected records in some other grid to this
// treeGrid, without requiring any user interaction.<br>
// See the +link{group:dragging} documentation for an overview of grid drag/drop data
// transfer.
// @param sourceGrid (ListGrid) source grid from which the records will be transferred
// @param [folder] (TreeNode) parent node into which records should be dropped - if null
//    records will be transferred as children of the root node.
// @param [index] (integer) target index (drop position) within the parent folder 
// @param [callback] (Callback) optional callback to be fired when the transfer process has 
//                       completed.  The callback will be passed a single parameter "records",
//                       the list of nodes actually transferred to this component (it is called 
//                       "records" because this logic is shared with +link{class:ListGrid}).
// @group dragging
// @example dragTree
// @visibility external
//<
transferSelectedData : function (source, folder, index, callback) {
    
    if (!this.isValidTransferSource(source)) { 
        if (callback) this.fireCallback(callback);
        return; 
    }
    
    // don't check willAcceptDrop() this is essentially a parallel mechanism, so the developer 
    // shouldn't have to set that property directly.
    if (index == null) index = 0;
    if (folder == null) folder = this.data.getRoot();
    
    // Call cloneDragData() to pull the records out of the source's dataSet
    // Note we don't need to call 'transferDragData' here - that is all handled after 
    // transferNodes now, potentially by a server callback
    
    var nodes = source.cloneDragData();
    this.transferNodes(nodes, folder, index, source, callback);
},

// Insert after last child if we're not allowed to drop a new root node, drop occurs directly
// on a folder that's open and is a child of the root node, and the drop position is "after".
_dropAfterLastChild : function (position, dropItem, newParent) {
    if (this.canDropRootNodes) return false;
    if (dropItem != newParent) return false;

    var data = this.data,
        parent = data.getParent(dropItem);
    return data.isRoot(parent) && data.isOpen(dropItem) && position == isc.ListGrid.AFTER;    
},

//>	@method	treeGrid.drop()	(A)
//		@group	event handling	
//			handle a drop in the list
//			if possible, move or copy the items automatically
//			NOTE: at this point, we should be assured that we can accept whatever was dragged...
//		@return	(boolean)	false == cancel further event processing
//<
drop : function () {
    if (!this.willAcceptDrop()) return false;

    // NOTE: we perform some redundant checks with willAcceptDrop(), but this is not a time
    // critical method, and the errors being checked for would corrupt the Tree and so should
    // never be allowed, so it makes sense to check them here as well since willAcceptDrop()
    // might be incorrectly overidden.

	// get what was dropped and where it was dropped
	var moveList = isc.EH.dragTarget.cloneDragData(),
		recordNum = this.getEventRecordNum(null, true),
        position = this.getRecordDropPosition(recordNum),
        dropItem = recordNum < 0 ? null : this.data.get(recordNum),
        newParent = this.getDropFolder();

    // dropping in the body in open space means add to root
    if (!dropItem) dropItem = this.data.getRoot();

    //this.logWarn("valid drop with parent: " + this.echo(newParent));

    // figure out if this is a drag within the same Tree data model.  This can happen within the
    // same TreeGrid or across two TreeGrids.
    var dragTree = isc.EH.dragTarget.getData(),
        dragWithinTree = ( isc.isA.Tree(dragTree) && 
                           isc.isA.Tree(this.data) && 
                           dragTree.getRoot() == this.data.getRoot() );
    // make sure that they're not trying to drag into parent containing child with same name.
    // NOTE: this particular check is postponed until drop() because it's not self-evident why
    // the widget won't accept drop, so we want to warn() the user
    
    for (var i = 0; i < moveList.length; i++) {
        
        var child = moveList[i];

        // NOTE: If dragging in from another tree - set dragDataAction to "copy" to test the
        // code below, otherwise you end up with 2 trees pointing at the same object

        // name collision: see if there's already a child under the newParent that has the same
        // name as the child we're trying to put under that parent
        var collision = (this.data.findChildNum(newParent, this.data.getName(child)) != -1);

        // this collision is not a problem if we're reordering under the same parent
        var legalReorder = dragWithinTree && this.canReorderRecords && 
                            newParent == this.data.getParent(child);
        if (collision && !legalReorder) {
            this.logInfo("already a child named: " + this.data.getName(child) + 
                         " under parent: " + this.data.getPath(newParent));
            isc.warn(this.parentAlreadyContainsChildMessage);
            return false;
        }            
    }

	// At this point, everything looks OK and we are accepting the drop	

    // figure out where the dropped should be placed in the parent's children 
    var index = null;
    if (this.canReorderRecords) {
        if (recordNum < 0 || this._dropAfterLastChild(position, dropItem, newParent)) {
            // already set dropItem to root
            newParent = dropItem;
            // special case: dropped in empty area of body, make last child of root
            index = this.data.getChildren(newParent).getLength();
        } else if (dropItem == newParent) {
            // if dropped directly on a folder, place at beginning of children
            index = 0;
        } else {
            // otherwise place before or after leaf's index within parent
            index = (position == isc.ListGrid.BEFORE ? 0 : 1) + 
                        this.data.getChildren(newParent).indexOf(dropItem);
        }
    }
    
    var dropPosition = position;
    // if onFolderDrop exists - allow it to cancel the drop
    
    if (this.onFolderDrop != null &&  
        (this.onFolderDrop(moveList,newParent,index,dropPosition,isc.EH.dragTarget) == false)) return false;
    
    this.folderDrop(moveList, newParent, index, isc.EH.dragTarget);

	// open the folder the nodes were dropped into
	this.data.openFolder(newParent);
			
	// return false to cancel further event processing
	return false;
},

//> @method treeGrid.recordDrop()
// The superclass event +link{listGrid.recordDrop} does not fire on a TreeGrid, use
// +link{folderDrop} instead.
//
// @visibility external
//<

//> @method treeGrid.folderDrop() [A]
//
// Process a drop of one or more nodes on a TreeGrid folder.
// <P>
// This method can be overridden to provide custom drop behaviors and is a more appropriate
// override point than the lower level +link{Canvas.drop()} handler.
// <P>
// If this is a self-drop, nodes are simply reordered. An "update" operation will
// be submitted to update the +link{tree.parentIdField,parentId} field of the moved node(s). 
// <P>
// For a drop from another widget, +link{treeGrid.transferDragData()} is called which,
// depending on the +link{TreeGrid.dragDataAction,dragDataAction} specified on the source
// widget, may either remove the source nodes from the original list (<code>dragDataAction:"move"</code>)
// or just provide a copy to this tree (<code>dragDataAction:"copy"</code>).
// <P>
// In either case the new row(s) appear in the <code>folder</code> at the <code>index</code>
// specified by the arguments of the same name.
// <P>
// If this grid is databound, the new nodes will be added to the dataset by calling
// +link{dataSource.addData()}.  Further, if the new nodes were dragged from another
// databound component, and +link{DataBoundComponent.addDropValues,addDropValues}
// is true, +link{DataBoundComponent.getDropValues,getDropValues} will be called for every item
// being dropped.
// <P>
// As a special case, if the <code>sourceWidget</code> is also databound and a
// +link{dataSourceField.foreignKey,foreignKey} relationship is declared from the
// <code>sourceWidget</code>'s DataSource to this TreeGrid's DataSource, the interaction will
// be treated as a "drag recategorization" use case such as files being placed in folders,
// employees being assigned to teams, etc.  "update" DSRequests will be submitted that
// change the foreignKey field in the dropped records to point to the tree folder that was the
// target of the drop.  In this case no change will be made to the Tree data as such, only to
// the dropped records. 
// <P>
// For multi-record drops, Queuing is automatically used to combine all DSRequests into a
// single HTTP Request (see QuickStart Guide, Server Framework chapter).  This allows the
// server to persist all changes caused by the drop in a single transaction (and this is
// automatically done when using the built-in server DataSources with Power Edition and
// above).
// <P>
// If these default persistence behaviors are undesirable, return false to cancel them, then
// and implement your own behavior, typically by using grid.updateData() or addData() to add
// new records.
// <p><b>NOTE:</b> the records you receive in this event are the actual Records from the source
// component.  Use +link{DataSource.copyRecords()} to create a copy before modifying the records
// or using them with updateData() or addData().
//
// @param nodes (Array of TreeNode) List of nodes being dropped
// @param folder (TreeNode) The folder being dropped on
// @param index (int) Within the folder being dropped on, the index at which the drop is
//                        occurring.  Only passed if +link{canReorderRecords} is true.
// @param sourceWidget (Canvas) The component that is the source of the nodes (where the nodes
//                              were dragged from).
// @param [callback] (Callback) optional callback to be fired when the transfer process has 
//                       completed.  The callback will be passed a single parameter "records",
//                       the list of nodes actually transferred to this component (it is called
//                       "records" because this is logic shared with +link{class:ListGrid})
//
// @visibility external
// @example treeDropEvents
//<
folderDrop : function (nodes, folder, index, sourceWidget, callback) {
    this.transferNodes(nodes, folder, index, sourceWidget, callback);

},


//> @method treeGrid.transferNodes() [A]
//
// Transfer a list of +link{TreeNode}s from another component (does not have to be a databound
// component) into this component.  This method is only applicable to list-type components,
// such as +link{ListGrid,listGrid}, +link{TreeGrid,treeGrid} or +link{TileGrid,tileGrid}.
// <P>
// This method implements the automatic drag-copy and drag-move behavior and calling it is
// equivalent to completing a drag and drop of the <code>nodes</code>.
// <P>
// Note that this method is asynchronous - it may need to perform server turnarounds to prevent
// duplicates in the target component's data.  If you wish to be notified when the transfer
// process has completed, you can either pass the optional callback to this method or implement
// the +link{dataBoundComponent.dropComplete()} method on this component.
// <P>
// See also +link{transferSelectedData}.
//
// @param nodes (Array of TreeNode) Nodes to transfer to this component
// @param folder (TreeNode) The target folder (eg, of a drop interaction), for context
// @param index (integer) Insert point within the target folder data for the transferred nodes
// @param sourceWidget (Canvas) The databound or non-databound component from which the nodes
//                              are to be transferred.
// @param [callback] (Callback) optional callback to be fired when the transfer process has
//                       completed.  The callback will be passed a single parameter "records",
//                       the list of nodes actually transferred to this component (it is called
//                       "records" because this is logic shared with +link{class:ListGrid})
//
// @visibility external
// @example treeDropEvents
//<
transferNodes : function (nodes, folder, index, sourceWidget, callback) {

    // storeTransferState returns false if a prior transfer is still running, in which case
    // we just bail out (transferNodes() will be called again when the first transfer 
    // completes, so we aren't abandoning this transfer, just postponing it) 
    if (!this._storeTransferState("transferNodes", nodes, folder, index, 
                                  sourceWidget, callback)) {
        return;
    }

    // If parent folder is null, we're dropping into the TreeGrid body, which implies root
    folder = folder || this.data.root;

    // figure out if this is a drag within the same Tree (even if from another TreeGrid)
    var dragTree = sourceWidget.getData(),
        dragWithinTree = ( isc.isA.Tree(dragTree) && 
                           isc.isA.Tree(this.data) && 
                           dragTree.getRoot() == this.data.getRoot() );
    // if we're dropping an item from one tree to another that both share the same root, perform a
	// move instead.  Note that this ignores dragType (eg clone vs copy) completely.
    var dataSource = this.getDataSource(),
        sourceDS = sourceWidget.getDataSource();
	if (dragWithinTree && (this.dragDataAction != isc.TreeGrid.COPY && 
                           this.dragDataAction != isc.TreeGrid.CLONE)) 
    {
        if (dataSource != null && this.data != null && 
            isc.ResultTree && isc.isA.ResultTree(this.data)) 
        {
            this._dropRecords[0].noRemove = true;
            var wasAlreadyQueuing = isc.rpc.startQueue();

            // NOTE: We are possibly going to do some client-side reordering here.  Depending 
            // on whether we're moving nodes forwards or backwards within their siblings, or
            // neither (if we're reparenting) or both (if we have multiple selected), we'll be
            // changing which index within the parent is the correct one to insert at.  Thus
            // we'll establish upfront which is the correct sibling node to insert before, and
            // always the actual index by reference to that node's current location as the 
            // loop progresses
            var currentChildren = dragTree.getChildren(folder);
            var insertBeforeNode, undef;
            if (index != null) {
                if (index < currentChildren.getLength()) {
                    insertBeforeNode = currentChildren.get(index);
                }
            }
            if (insertBeforeNode == undef) {
                insertBeforeNode = currentChildren.last();
            }

            var loadingMarker = isc.ResultSet.getLoadingMarker();
            for (var i = 0; i < nodes.length; i++) {
                var node = nodes[i];
                if (node == null) continue;
                if (this.shouldSaveLocally() || 
                        node[this.data.parentIdField] == folder[this.data.idField]) 
                {
                    // The user has dragged a node to a different location within the the same
                    // parent.  This change cannot be automatically persisted, so we'll just
                    // reflect the change locally so it doesn't appear to the user that nothing
                    // has happened (though, in fact, nothing *has* happened - some kind of 
                    // index update on the underlying persistent store needs to be performed in
                    // order for a user interaction of this type to persist beyond the current
                    // UI session).
                    // If index is null, it's unclear what we should do.  We could either leave 
                    // the node where it is, or move it to the end of the list (as we would if
                    // we were adding to the parent).  This may change, but right now we just 
                    // leave it where it is
                    // Note: We use the 'moveBefore' API on tree rather than simple "move"
                    // - we want to ensure we end up next to the "nextSibling" rather than
                    //   necessarily at the current index of the next-sibling
                    if (index != null) {
                        dragTree.moveBefore(node, insertBeforeNode);
                    }
                } else {
                    
                    // NOTE: getCleanNodeData() scrubs off the isOpen flag if it was auto-
                    // generated, but we need to hang onto it, otherwise dragging an open 
                    // folder from one parent to another causes it to snap shut.
                    var saveIsOpenFlag = nodes[i]["_isOpen_" + this.data.ID];
                    var node = isc.addProperties({}, this.data.getCleanNodeData(nodes[i], true, false)),
                        oldValues = isc.addProperties({}, node);
                    if (saveIsOpenFlag != null) node["_isOpen_" + this.data.ID] = saveIsOpenFlag;
                    node[this.data.parentIdField] = folder[this.data.idField];
                    var dropNeighbor = null,
                        children = this.data.getChildren(folder);
                    if (index == null) {
                        dropNeighbor = children.last();
                        if (dropNeighbor == loadingMarker) {
                            dropNeighbor = null;
                        }
                    } else if (index > 0) {
                        dropNeighbor = children.get(index - 1);
                        if (dropNeighbor == loadingMarker) {
                            dropNeighbor = null;
                        }
                    }
                    
                    // We pass a number of parameters relating to this drop up to the server,
                    // so that they are available in the callback.  This allows us to give
                    // the impression that a drop has taken place at a particular position
                    // within the parent.  This isn't what has actually happened - see the 
                    // above comment about dragging nodes to different locations within the
                    // same parent in a databound TreeGrid.
                    this.updateDataViaDataSource(node, dataSource, { 
                        oldValues : oldValues,  
                        parentNode : this.data.getParent(nodes[i]),
                        newParentNode : folder,
                        dragTree : dragTree,
                        draggedNode : node,
                        draggedNodeList: nodes,
                        dropNeighbor: dropNeighbor,
                        dropIndex : index
                    }, sourceWidget);                          
                }
            }
            
        } else {
            
            // move the nodes within the tree
            var currentChildren = dragTree.getChildren(folder);
            var insertBeforeNode, undef;
            if (index != null) {
                if (index < currentChildren.getLength()) {
                    insertBeforeNode = currentChildren.get(index);
                }
            }
            if (insertBeforeNode == null) {
      	    	dragTree.moveList(nodes, folder, index);
            } else {            
                dragTree.moveListBefore(nodes, insertBeforeNode);
            }
        }
	} else if (dataSource != null) {
         var canRecat;
        if (this.dragRecategorize == "always" || this.dragRecategorize != "never" &&
            (sourceDS != null && sourceDS != dataSource && this.data != null && 
             isc.ResultTree && isc.isA.ResultTree(this.data) && 
             sourceWidget.dragDataAction == isc.TreeGrid.MOVE))
        {
            // check for a foreign key relationship between some field in the source DS to some
            // field in the treeGrid DS
            var relationship = sourceDS.getTreeRelationship(dataSource);
            
            if (relationship != null && relationship.parentIdField) {
                var cannotRecat = false,
                    pkFields = sourceDS.getPrimaryKeyFields();
                
                // If the detected foreignKeyField is a Primary Key, we can't modify it.
                // Catch this case and log a warning
                
                for (var pk in pkFields) {
                    if (pk == relationship.parentIdField) {
                        this.logWarn("dragRecategorize: data source has dataSource:" 
                                    + sourceDS.getID() + ". foreignKey relationship with " +
                                    "target dataSource " + dataSource.getID() + 
                                    " is based on primary key which cannot be modified.");
                        cannotRecat = true;
                    }
                }
                if (!cannotRecat) canRecat = true;
                //>DEBUG
                this.logInfo("Recategorizing dropped nodes in dataSource:" + sourceDS.getID());
                //<DEBUG
            }
            
            // Remember that we performed updates rather than adds, so we don't remove records
            // later on in transferDragData()
            this._dropRecords[0].noRemove = true;
    
            var wasAlreadyQueuing = isc.rpc.startQueue();
            for (var i = 0; i < nodes.length; i++) {
                var node = {};
                var pks = sourceDS.getPrimaryKeyFieldNames();
                for (var j = 0; j < pks.length; j++) {
                    node[pks[j]] = nodes[i][pks[j]];
                }
                if (canRecat) {
                    node[relationship.parentIdField] = folder[relationship.idField];
                }
                isc.addProperties(node, 
                    this.getDropValues(node, sourceDS, folder, index, sourceWidget));

                this.updateDataViaDataSource(node, sourceDS, null, sourceWidget);
            }
        } else {

            
            if (isc.isA.Tree(dragTree) && sourceWidget.dragDataAction == isc.TreeGrid.MOVE) {
                nodes = dragTree.getCleanNodeData(nodes, sourceWidget.dataSource == null);
            }

            

            var wasAlreadyQueuing = isc.rpc.startQueue();
            for (var i = 0; i < nodes.length; i++) {
                var data = nodes[i],
                    resultTree = this.data;
                if (resultTree) {
                    data[resultTree.parentIdField] = folder[resultTree.idField];
                }
                isc.addProperties(data, 
                    this.getDropValues(data, sourceDS, folder, index, sourceWidget));
                
                this._addIfNotDuplicate(data, sourceDS, sourceWidget, null, index, folder);
            }
        }
    } else {
        // add the dropped nodes to the tree at the specified point - they could be rows from a
        // ListGrid, or anything - it's up to the developer to have it make sense
        //this.logWarn("adding dragData at parent: " + newParent + ", position: " + position);
        for (var i = 0; i < nodes.length; i++) {
            this._addIfNotDuplicate(nodes[i], sourceDS, sourceWidget, null, index, folder);
        }
    }
    
    // If this._transferDuplicateQuery is undefined or 0,we didn't need to fire any server 
    // queries, so we can call transferDragData to complete the transfer and send the queue 
    // of updates to the server 
    if (!this._transferDuplicateQuery) {
        isc.Log.logDebug("Invoking transferDragData from inside transferNodes - no server " +
                         "queries needed?", "dragDrop");
        sourceWidget.transferDragData(this._transferExceptionList, this);
        if (dataSource) {
            // send the queue unless we didn't initiate queuing
            if (!this._wasAlreadyQueuing) isc.rpc.sendQueue();
        }
    }
    
    this._transferringRecords = false;
    
},

// NOTE: Overrides (but invokes) the DBC version
_updateComplete : function (dsResponse, data, dsRequest) {
    if (!dsRequest.dragTree) return;
    
    // If the node we're dropping into is not in the tree (ie, it is neither the root node
    // nor the child of another node), warn the user and bail
    if (dsRequest.newParentNode != this.data.root &&
        dsRequest.dragTree.getParent(dsRequest.newParentNode) == null)
    {
        isc.logWarn("Target folder is no longer in the Tree in TreeGrid cache sync");
        return;
    }
    
    var neighbor = dsRequest.dropNeighbor,
        dragTree = dsRequest.dragTree,
        siblings = dragTree.getChildren(dsRequest.newParentNode),
        nodeList = dsRequest.draggedNodeList,
        idField = dragTree.idField,
        nodePosition = nodeList.findIndex(idField, dsRequest.draggedNode[idField]),
        index, undef;
    if (neighbor == null) {
        index = 0;
    } else {
        for (var i = 0, siblingsLength = siblings.getLength(); i < siblingsLength; ++i) {
            var sibling = siblings.getCachedRow(i);
            if (sibling == neighbor) {
                index = i + 1;
                break;
            }
        }
    }
    
    if (index !== undef) { 
        // Step the insert point forward to ensure that nodes are inserted in the same order
        // they were passed to folderDrop.  This is necessary because some of the nodes may 
        // have already been moved into position synchronously (if we had a multi-node drag
        // where the dragged nodes came from several parents)
        //isc.logWarn("nodeList: " + nodeList.getProperty("Name"));
        var siblingsLength = siblings.getLength();
        while (index < siblingsLength) {
            var sibling = siblings.getCachedRow(index);
            if (sibling != null) {
                //isc.logWarn("existing node: " + sibling.Name);
                var existingIndex = nodeList.findIndex(idField, sibling[idField]);
                //isc.logWarn("existingIndex: " + existingIndex + ", nodePosition: " + nodePosition);
                if (existingIndex == -1 || existingIndex > nodePosition) break;
            }
            index++;
        }
    }
    
    if (index === undef) {
        isc.logWarn("Could not order dropped node by reference to neighbor; trying absolute index");
        index = dsRequest.dropIndex;
    }
    
    // If index is still undefined, something's gone wrong.  Log a warning and bail
    if (index === undef) {
        isc.logWarn("Unable to determine drop location in TreeGrid cache sync");
        return;
    }

//    dragTree.move(dsRequest.draggedNode, dsRequest.newParentNode, index);
    var nodeToMove = this.data.find(idField, dsRequest.draggedNode[idField]);
    dragTree.move(nodeToMove, this.data.getParent(nodeToMove), index);
    
    this.Super("_updateComplete", arguments);
},


// Tree-specific HTML generation
// --------------------------------------------------------------------------------------------

//>	@method	treeGrid.getTreeCellValue()
//			Returns the HTML to display a cell with
//          <ul>
//          <li>Indentation</li>
//          <li>Open / Close Icon (folders only)</li>
//          <li>Optional extra icon</li>
//          <li>Folder / Node Icon</li>
//          <li>Value for the cell</li>
//          </ul>
//			OVERRIDE in your subclass for a more complicated presentation
//
//      @param  value           (string)    value to display in the cell
//		@param	record			(TreeNode)	tree node in question	
//		@param	recordNum		(number)	number of that tree node
//      @param  fieldNum        (number)    number of the field being output as treeField
//
//		@return	(HTML)	HTML output for the cell
//      @visibility internal
//<
// iconPadding - padding between the folder open/close icon and text.
// Make this customizable, but not exposed - very unlikely to be modified
iconPadding:3,
_$closeTreeCellTable:"</tr></tbody></table>",
_$semi:";",
getTreeCellValue : function (value, record, recordNum, fieldNum, gridBody) {

    // This returns HTML to achieve
    //  - an indent equal to what level of the tree you're viewing
    //  - open / close icon
    //  - an optional additional icon
    //  - Folder / Record icon
    //  - title for the cell.

    // If passed a null or LOADING record just return the value passed in.
    if (record == null || Array.isLoading(record)) {
        return value;
    }
	// get the level of the node
	var level = this.data.getLevel(record),    
        template = isc.TreeGrid._getTreeCellTemplate(),
        cssText = this.getCellCSSText(record, recordNum, fieldNum),
        styleName = this.getCellStyle(record, recordNum, fieldNum);

    
    template[1] = styleName
    template[3] = (this._fixTitleWidth()
                   ? "table-layout:fixed;width:100%;" + (cssText != null ? cssText : "")
                   : cssText);

    // catch custom css text with no closing ";"
    if (template[3] != null && !template[3].endsWith(this._$semi)) template[3] += this._$semi;

    // styling for indent cell
    template[9] = cssText;
    template[11] = styleName;
    
    template[13] = this.getIndentHTML(level, record, template, 5);

    // Get the HTML for the icons and title from _getTreeCellTitleArray(), and fold them
    // into our template
    var titleCellTemplate = this._getTreeCellTitleArray(value, record, recordNum, 
                                fieldNum, this.shouldShowOpenerIcon(), 
                                styleName, cssText, template, 7);
    for (var i = 0, j = 15; i < titleCellTemplate.length; i++) {
        template[j] = titleCellTemplate[i];
        j++;
    }
    template[j] = this._$closeTreeCellTable;
    
    return template.join(isc.emptyString);
},

shouldShowOpenerIcon : function () {
    // Note if showOpener is false, but showConnectors is true, we still want
    // to show an "opener icon" by the node folder or leaf icon - it's just going to
    // be the end of a connector line
    return this.showOpener || this.showConnectors; 
},

_getTreeCellValueID : function (recordNum) {
    return this.ID + "_"+"valueCell" + recordNum;
},

// _getTreeCellTitleArray() - helper method for getTreeCellValue() to return the
// "title" portion of the treeCell value - that is: the icons and the title, without
// any indent

_getTreeCellTitleArray : function (value, record, recordNum, fieldNum, showOpener,
                                   cellStyle, cellCSSText, treeCellTemplate, iconCellWidthOffset) {

    var iconCellWidth = 0;

    if (cellCSSText == null) cellCSSText = this.getCellCSSText(record, recordNum, fieldNum);
    if (cellCSSText == null) cellCSSText = "";
    else cellCSSText += ";";
    if (!this.wrapCells) cellCSSText += "white-space:nowrap;";
    if (this._fixTitleWidth()) {
        cellCSSText += "overflow:hidden;" + isc.Browser._textOverflowPropertyName +
                       ":ellipsis";
    }
    if (cellStyle == null) cellStyle = this.getCellStyle(record, recordNum, fieldNum);

    var template = isc.TreeGrid._getTreeCellTitleTemplate();
    template[1] = cellCSSText;
    template[3] = cellStyle;
    if (showOpener) {
        // opener icon (or small indent)
        var openIcon = this.getOpenIcon(record),        
            openIconWidth = this.getOpenerIconWidth(record),
            openIconHeight = this.getOpenerIconHeight(record),
            openerID = (recordNum != null ? this._openIconIDPrefix+recordNum : null);
        if (openIcon) {
            template[5] = this.getIconHTML(openIcon, openerID, openIconWidth, null, openIconHeight);
            iconCellWidth += openIconWidth;
        } else {
            template[5] = this._indentHTML(openIconWidth || this.iconSize);
            iconCellWidth += openIconWidth;
        }
    } else template[5] = null;
    var checkboxIcon = this._getCheckboxIcon(record),
        extraIcon = checkboxIcon || this.getExtraIcon(record),
        extraIconID = (recordNum != null ? this._extraIconIDPrefix+recordNum : null),
        extraIconSize = (checkboxIcon != null ?  this._getCheckboxFieldImageWidth() : this.iconSize),
        extraIconGap = this.extraIconGap,
        icon = this.getIcon(record),
        iconID = (recordNum != null ? this._iconIDPrefix+recordNum : null)
    ;

    // extra icon if there is one
    if (extraIcon) {
        template[6] = this.getIconHTML(extraIcon, extraIconID, extraIconSize, extraIconGap);
        iconCellWidth += extraIconSize + extraIconGap;
    } else template[6] = null;
    // folder or file icon
    template[7] = this.getIconHTML(icon, iconID, record.iconSize);
    iconCellWidth += icon == null ? 0 : (record.iconSize || this.iconSize);

    // When ARIA is enabled, set an ID on the value cell so that we can reference it.
    if (isc.Canvas.ariaEnabled()) {
        template[9] = " id='" + this._getTreeCellValueID(recordNum) + "'";
    } else {
        template[9] = null;
    }

    template[11] = cellCSSText;
    template[13] = this.iconPadding;
    template[15] = cellStyle;

    if (isc.Browser.isIE && isc.Browser.version < 10 && !this.wrapCells) {
        template[17] = "<NOBR>";
        template[19] = "</NOBR>";

    
    } else if (this._fixTitleWidth() && isc.Browser.isMoz && isc.Browser.version < 21) {
        template[17] = "<div style='overflow:hidden;text-overflow:ellipsis' _titleClipper='true'>";
        template[19] = "</div>";
    } else {
        template[19] = template[17] = null;
    }

    template[18] = value;

    if (treeCellTemplate) treeCellTemplate[iconCellWidthOffset] = iconCellWidth;
    return template;
},

_fixTitleWidth : function () {
    var treeField = this.getTreeFieldNum(),
        frozen = this.fields[treeField] && this.fields[treeField].frozen,
        gettingAutoSize = 
            frozen ? (this.frozenBody && this.frozenBody._gettingAutoSizeHTML) 
                    : (this.body && this.body._gettingAutoSizeHTML);
    return this.fixedFieldWidths && !gettingAutoSize;
},

//> @method TreeGrid.getCellAlign()
// Return the horizontal alignment for cell contents. Default implementation will always
// left-align the special +link{treeGridField.treeField} [or right-align if the page is in
// RTL mode] - otherwise will return +link{listGridField.cellAlign} if specified, otherwise
// +link{listGridField.align}.
//
//
// @param   record (ListGridRecord) this cell's record
// @param	rowNum	(number)	row number for the cell
// @param	colNum	(number)	column number of the cell
// @return	(Alignment)     Horizontal alignment of cell contents: 'right', 'center', or 'left'	
// @visibility external
//< 
getCellAlign : function (record, rowNum, colNum) {
    var field = this.getField(colNum);
    if (field && field.treeField) {
        return this.isRTL() ? "right" : "left";
    }
    return this.Super("getCellAlign", arguments);
},


// Override getCellValue() to return custom HTML for the tree-field
// Note: Developers are always advised to override formatCellValue rather than this method
// directly (which could lead to certain conflicts). 
getCellValue : function (record, rowNum, colNum, gridBody, b, c, d) {
    var value = this.invokeSuper(isc.TreeGrid, "getCellValue", record, rowNum, colNum, gridBody, b,c,d);
    if (colNum == this.getTreeFieldNum()) {
// that takes an already formatted value and applies stuff to it.    
        value = this.getTreeCellValue(value, record, rowNum, colNum, gridBody);
    }
    return value;
},

// overridden to create/clear draw cache
// (Fired for both frozen and normal body)
bodyDrawing : function (body,a,b,c,d) {
    this._drawCache = {};
    return this.invokeSuper(isc.TreeGrid, "bodyDrawing", body,a,b,c,d);
},

//> @method TreeGrid.getNodeTitle()
//
// Returns the title to show for a node in the tree column.  If the field specifies the
// <code>name</code> attribute, then the current <code>node[field.name]</code> is returned.
// Otherwise, the result of calling +link{method:Tree.getTitle} on the node is called.
// <br><br>
// You can override this method to return a custom title for node titles in the tree column.
//
// @param node      (TreeNode)  The node for which the title is being requested.
// @param recordNum (Number)  The index of the node.
// @param field     (DSField) The field for which the title is being requested.
// 
// @return (HTML) the title to display.
//
// @see method:Tree.getTitle
//
// @visibility external
//<
getNodeTitle : function (record, recordNum, field) {
    
    var value;
    if (field.name && field.name != this._titleField) {
        
        if (recordNum == -1) return record[field.name];
        value = this.getEditedRecord(recordNum)[field.name];
    } else {
        value = this.data.getTitle(record);
    }
    // This will convert to a string etc.
    return this.applyCellTypeFormatters(value, record, field, recordNum, this.getFieldNum(field));

},

//> @method TreeGrid.getTitleField()
// Method to return the fieldName which represents the "title" for records in this
// TreeGrid.<br>
// If <code>this.titleField</code> is explicitly specified for this treeGrid, respect it - 
// otherwise always return the tree-field (+link{TreeGrid.treeField}) for this grid.
// @return (string) fieldName for title field for this grid.
//<
getTitleField : function () {
    if (this.titleField != null) return this.titleField;
    return this.getFieldName(this.getTreeFieldNum());
},

//>	@method	treeGrid.getTreeFieldNum()	(A)
//		Return the number of the tree field for this treeGrid.
//
//		@return	(number)	Number for the tree node.
//<
getTreeFieldNum : function () { return this._treeFieldNum; },

//>	@method	treeGrid.getOpenAreaWidth()	(A)
//		
//		@param	node		(TreeNode)		tree node clicked on
//
//		@return	(number)	Return the width of the open area (relative to wherever the tree field is)
//<
getOpenAreaWidth : function (node) {
    var openerIconSize = this.showOpener ? this.getOpenerIconWidth(node) : 0,
        indentSize = (this.showConnectors ? openerIconSize : this.indentSize)
    ;
    return ((this.data.getLevel(node)-(this.showRoot?0:1)) * indentSize) + openerIconSize;
},

getOpenerIconSize : function (node) {
    return (this.openerIconSize || (this.showConnectors ? this.cellHeight : this.iconSize));
},
getOpenerIconWidth : function (node) {
    return this.openerIconWidth || this.getOpenerIconSize(node);
},
getOpenerIconHeight : function (node) {
    return this.openerIconHeight || this.getOpenerIconSize(node);
},

//>	@method	treeGrid.clickInOpenArea()	(A)
//			for a given click, was it in the open/close area or on the main part of the item
//			OVERRIDE in your subclasses for different open/close schemes
//		@param	node		(TreeNode)		tree node clicked on
//
//		@return	(boolean)		true == click was in open area, false == normal click
//<
clickInOpenArea : function (node) {

	if (!this.data.isFolder(node) || ! this.showOpener) return false;
	
	// get some dimensions
	var treeFieldNum = this.getTreeFieldNum(),
        body = this.getFieldBody(treeFieldNum),
        localFieldNum = this.getLocalFieldNum(treeFieldNum),
		fieldLeft = body.getColumnLeft(localFieldNum),
		fieldWidth = body.getColumnWidth(localFieldNum),
		openAreaWidth = this.getOpenAreaWidth(node),
		x = body.getOffsetX()
	;

	// textDirection: switch based on drawing in left-to-right (default) or right-to-left order
	if (this.isRTL()) {
        fieldLeft += body.getScrollWidth() - body.getViewportWidth();
		var fieldRight = fieldLeft + fieldWidth;
		return x >= (fieldRight - openAreaWidth) && x <= fieldRight;
	} else {
		return x >= fieldLeft && x < fieldLeft + openAreaWidth;	
	}
},

//> @method treeGrid.isOverOpenArea()
// Returns true if the last event occurred over the indented area or over the
// open / close icon of a folder node in this TreeGrid. Returns false if the event
// did not occur over a folder node.
//
// @return (Boolean) true if the user clicked the open icon
// @visibility external
//<
isOverOpenArea : function () {
    var node = this.getRecord(this.getEventRow());
    if (node == null) return false;
    return this.clickInOpenArea(node);
},

//> @method treeGrid.clickInCheckboxArea() (A)
// For a given click, was it in the checkbox area?
// @param  node (TreeNode) tree node clicked on
//
// @return (boolean)       true == click was in checkbox area, false == normal click
//<
clickInCheckboxArea : function (node) {
	if (this.selectionAppearance != this._$checkbox) return false;
	return this.isOverExtraIcon(node);
},

//> @method treeGrid.isOverExtraIcon()
// Returns true if the last event occurred over +link{TreeGrid.getExtraIcon(),extra icon}
// for the current node.
// <P>
// Returns false if the event did not occur over an extraIcon, or if no extraIcon is
// showing for the node in question.
//
// @return (Boolean) true if the user clicked the extra icon
// @visibility external
//<

isOverExtraIcon : function (node) {

    if (node == null) node = this.getRecord(this.getEventRow());
    if (node == null) return false;
    // No extra-icon for the node
    var checkboxIcon = this._getCheckboxIcon(node),
        extraIcon = checkboxIcon || this.getExtraIcon(node);
        
    // No checkbox/extra icon - just bail.
    if (extraIcon == null) return false;

    var extraIconSize = (checkboxIcon != null ?  this._getCheckboxFieldImageWidth() : this.iconSize);

	// get some dimensions
	var treeFieldNum = this.getTreeFieldNum(),
        body = this.getFieldBody(treeFieldNum),
        localFieldNum = this.getLocalFieldNum(treeFieldNum),
		fieldLeft = body.getColumnLeft(localFieldNum),
		fieldWidth = body.getColumnWidth(localFieldNum),
		openAreaWidth = this.getOpenAreaWidth(node),
		x = body.getOffsetX()
	;

	// textDirection: switch based on drawing in left-to-right (default) or right-to-left order
	if (this.isRTL()) {
		var fieldRight = fieldLeft + fieldWidth;
		return (x >= (fieldRight - openAreaWidth - extraIconSize) &&
		    x <= (fieldRight - openAreaWidth));
	} else {
		return (x >= (fieldLeft + openAreaWidth) &&
		    x < (fieldLeft + openAreaWidth + extraIconSize));
	}
},

//> @method treeGrid.getIndentHTML() (A)
// Return the HTML to indent a record
// @param level  (number)   indent level (0 == root, 1 == first child, etc)
// @param record (treeNode) record for which we're returning indent HTML
//
// @return (HTML) HTML to indent the child
//<
getIndentHTML : function (level, record, treeCellTemplate, indentCellWidthOffset) {
    var drawLevel = level;
    if (!this.showRoot) drawLevel--;

    var indentWidth = (this.showConnectors ? this.getOpenerIconWidth(record) : this.indentSize),
        
        shift1px = this.isPrinting || isc.Browser.isIE || isc.Browser.isOpera || isc.Browser.isEdge,
        indentCellWidth = (shift1px ? 1 : 0);

    // If showFullConnectors is true we need to write out vertical connector lines between 
    // ancestors who are siblings.
    
    if (this.showConnectors && this.showFullConnectors) {
        // assume the level passed in is correct
        //var level = this.data.getLevel(record),
        var levels = this.data._getFollowingSiblingLevels(record);
        // we don't care about the innermost level (connector written out as part of opener icon)
        levels.remove(level);        
        if (!this.showRoot) levels.remove(0);
        if (levels.length != 0) {
            if (!this._ancestorConnectorHTML) {
                var state = "ancestor";
                if (this.isRTL()) state += "_rtl";
                
                var connectorURL = isc.Img.urlForState(this.connectorImage, null, null,
                                                        state),
                                    connectorHTML = this.getIconHTML(connectorURL, null,
                                                        this.cellHeight);
                this._ancestorConnectorHTML = connectorHTML;
            }
            var singleIndent = this._indentHTML(indentWidth),
                indent = isc.StringBuffer.create(isc.emptyString)
            ;

            // explicit NOBR tag required in IE6 to ensure the indents don't wrap
            // when they run out of horizontal space 
            indent.append("<NOBR>");
            if (shift1px) indent.append(this._indentHTML(1));
            for (var i = (this.showRoot ? 0 : 1); i < level; i ++) {                
                if (levels.contains(i)) {
                    indent.append(this._ancestorConnectorHTML);
                    indentCellWidth += this.cellHeight;
                } else {
                    indent.append(singleIndent);
                    indentCellWidth += indentWidth;
                }
            }
            indent.append("</NOBR>");
            indent = indent.release(false);
            if (treeCellTemplate) treeCellTemplate[indentCellWidthOffset] = indentCellWidth;
            return indent;
        }
    }
    indentCellWidth = drawLevel * indentWidth;
    if (shift1px) indentCellWidth = Math.max(1, indentCellWidth);
    var indentHTML = this._indentHTML(indentCellWidth);
    
    if (isc.Browser.isIE9 || (isc.Browser.isStrict && (isc.Browser.isIE7 || isc.Browser.isIE8))) {
        indentHTML = "<NOBR>" + indentHTML + "</NOBR>";
    }
    if (treeCellTemplate) treeCellTemplate[indentCellWidthOffset] = indentCellWidth;
    return indentHTML;
},


_indentHTML : function (numPixels) {
	if (numPixels == 0) return isc.emptyString;

    var cache = isc.TreeGrid._indentHTMLCache;
    if (cache == null) cache = isc.TreeGrid._indentHTMLCache = {};

    if (cache[numPixels] == null) cache[numPixels] = isc.Canvas.spacerHTML(numPixels, 1);

    return cache[numPixels];
},

//>	@method	treeGrid.getOpenIcon()	(A)
// Get the appropriate open/close opener icon for a node. Returns null if +link{showOpener} is
// set to false.
//
// @param	node (TreeNode)	tree node in question
// @return	(URL)		URL for the icon to show the node's open state
//
// @visibility external
//<
getOpenIcon : function (record) {
    if (this.showOpener == false && !this.showConnectors) return null;
    if (!this.data) return null;
	if (isc.isA.Number(record)) record = this.data.get(record);
    if (record == null) return null;
    
	// if the record has a specific openIcon, use that
	if (record.openIcon) {
		return record.openIcon;

	} else {

        // If showOpener is false, this method will show just lines for both leaves and folders
        var isFolder = this.showOpener && this.data.isFolder(record),
            // non-folders can never have children, or be open
            hasChildren = isFolder,
            isOpen = isFolder,
            // does this node have adjacent siblings above or below, or is there a gap
            // between it's next sibling at the same level in either direction.
            start,
            end;
        if (isFolder) {
    		// If the folder doesn't have it's data fully loaded show the
            // folder as being closed
    		var loadState = this.data.getLoadState(record);
            if (loadState == isc.Tree.UNLOADED || 
                (loadState == isc.Tree.FOLDERS_LOADED && 
                 this.displayNodeType != isc.Tree.FOLDERS_ONLY)) 
            {
                hasChildren = true;
                isOpen = false;
            // If the data is loaded for the folder, use the data APIs to determine
            // whether this has children or not.
    		} else {
                hasChildren = this.data.hasChildren(record, this.displayNodeType);
                isOpen = (hasChildren || this.alwaysShowOpener) && this.data.isOpen(record);
            }

        }
        
        // if we're an open folder, showing sparse connectors, we have a gap below us
        if (isOpen && !this.showFullConnectors) end = true
        else {
            end = !this._shouldShowNextLine(record);
        }

        start = !this._shouldShowPreviousLine(record);
        // punt it over to getOpenerImageURL which will assmble the URL from the state info.
        return this.getOpenerImageURL(isFolder, hasChildren, isOpen, start, end);
	}
},

// _shouldShowPreviousLine
// Internal method - should we show a continuation connector line going up to the previous row 
// for some record?
// True if the previous row is a sibling of this record, or if this is the first record in
// some folder (so the previous row contains parent of this record)
_shouldShowPreviousLine : function (record) {
    if (!this.data.isEmpty() && this.data.first() == record) {
        return false;
    }

    // always show a previous line if we're showing "full connectors"
    if (this.showFullConnectors) return true;

    var rowNum = this.data.indexOf(record),
        previousRecord = this.getRecord(rowNum - 1),
        parent = this.data.getParent(record);

    if (previousRecord == null) return false;
    return (parent == previousRecord || parent == this.data.getParent(previousRecord));
},

// _shouldShowNextLine
// Internal method - should we show a continuation connector line going down to the next row for
// some record?
// True only if the next row is a sibling of this record.
_shouldShowNextLine : function (record) {
    if (this.showFullConnectors) {
        var data = this.data,
            parent = data.getParent(record),
            children = data.getChildren(parent);
        return (children.indexOf(record) != (children.getLength() - 1));
    }
    var rowNum = this.data.indexOf(record),
        nextRecord = this.getRecord(rowNum +1);
    
    if (nextRecord == null) return false;
    return (this.data.getParent(record) == this.data.getParent(nextRecord));
},

//>	@method	treeGrid.getOpenerImageURL()	(A)
// Helper method called from getOpenIcon to retrieve the appropriate image URL string for
// the opener.
//
// @param isFolder (boolean) Is the node in question a folder? For showConnectors:true mode, this 
//                          method returns connector lines for leaves as well as open icons for folders
// @param	hasChildren (boolean)   Is the node in question a folder with children?
// @param   isOpen   (boolean)  Is the node an open folder?
// @param   startLine (boolean)   True if the previous row in the TreeGrid is not a sibling
//                                  or the parent of the node in question.  (Node effectively
//                                  starts a new hierarchy continuation line).
// @param   endLine   (boolean)   True if the next row in the TreeGrid is not a sibling
//                                  of the node in question.  (Node effectively ends a
//                                  hierarchy continuation line).
// @return	(string)		URL for the icon to show the node's state
//
// @visibility internal
//<
getOpenerImageURL : function (isFolder, hasChildren, isOpen, startLine, endLine) {   
    // Assemble the appropriate filename based on the base filename for connector / opener
    // images
    // Do this once per TreeGrid since each TreeGrid can have a different tg.openerImage /
    // tg.connectorImage.
    
    if (!this._openerImageMap) {
        // Assemble the various file names based on the possible states of these
        // images - the "opened" state (opened, closed, or has no children)
        //        - the position within the parent folder state 
        //          (start, middle, end, separate)
        //         - RTL if appropriate.

        var img = this.openerImage;
            
        this._openerImageMap = {
            // use Img.urlForState
            // This handles splitting the base name into base + extension, and plugging in
            // the state name parameter (third parameter).

            // opener opened.  NOTE: doesn't switch with RTL, which is an assumption that it
            // will generally be a downward pointing glyph.
            opened:isc.Img.urlForState(img, null, null, "opened"),
            // opener closed
            closed:isc.Img.urlForState(img, null, null, (this.isRTL() ? "closed_rtl" : "closed")),
            // opener opening
            
            opening:isc.Img.urlForState(img, null, null, "opening")
        }
    }

    // generate connector / opener w/connector images
    if (this.showConnectors && !this._connectorImageMap) {
        // for connectors, we have start/end/middle for position on a line of connectors,
        // "single" for a node with no peers, and opener/leaf variations.
        var img = this.connectorImage,
            states = ["single", "start", "end", "middle", 
                      "opened_single", "opened_start", "opened_middle", "opened_end",
                      "closed_single", "closed_start", "closed_middle", "closed_end"],
            map = {},
            isRTL = this.isRTL(),
            rtl = "_rtl";
    
        for (var i = 0; i < states.length; i++) {
            var state = states[i],
                suffix = state;
            if (isRTL) suffix += rtl;
            map[state] = isc.Img.urlForState(img, null, null, suffix);
        }

        this._connectorImageMap = map;
    }
    
    if (this.showConnectors) {
        var imageMap = this._connectorImageMap;
        if (hasChildren || (isFolder && this.alwaysShowOpener)) {
            if (isOpen) {
                
                if (!this.showFullConnectors) {
                    // if we're showing sparse connectors, no need to check for 'end'  - if it's 
                    // open this is always true.
                    if (startLine) return imageMap.opened_single;
                    return imageMap.opened_end;
                }
                if (startLine && endLine) return imageMap.opened_single;
                else if (startLine) return imageMap.opened_start;
                else if (endLine) return imageMap.opened_end;
                else return imageMap.opened_middle;
                
            } else {
                if (startLine && endLine) return imageMap.closed_single;
                if (startLine) return imageMap.closed_start;
                if (endLine) return imageMap.closed_end;
                return imageMap.closed_middle;
            }
        } else {
            // leaf
            if (startLine && endLine) return imageMap.single;
            if (startLine) return imageMap.start;
            if (endLine) return imageMap.end;
            return imageMap.middle;
        }
    } else {
        var imageMap = this._openerImageMap;
        // we don't return any image if we're not showing connectors, and this is not a folder
        // with children.
        if (!isFolder || (!hasChildren && !this.alwaysShowOpener)) return null;
        if (isOpen) return imageMap.opened;
        return imageMap.closed;
    }
},

_$checkbox:"checkbox",
_getCheckboxIcon : function (record) {
    var icon = null;      
    if (this.selectionAppearance == this._$checkbox) {
        var isSel = this.selection.isSelected(record) ? true : false;
        var isPartSel = (isSel && this.showPartialSelection &&
                    this.selection.isPartiallySelected(record)) ? true : false;
        // checked if selected, otherwise unchecked        
        icon = isPartSel ? (this.checkboxFieldPartialImage || this.booleanPartialImage)
                             : isSel ? (this.checkboxFieldTrueImage || this.booleanTrueImage)
                                     : (this.checkboxFieldFalseImage || this.booleanFalseImage);
        if (!this.body.canSelectRecord(record)) {
            if (this.showDisabledSelectionCheckbox) {
                // show the disabled checkbox, making sure to capture the
                // disabled state
                if (icon != this._$blank) icon = isc.Img.urlForState(icon, null, null, "Disabled");
            } else {
                if (this.leaveSelectionCheckboxGap) {
                    // record cannot be selected but we want
                    // the space allocated for the checkbox anyway.
                    icon = isc.Canvas._blankImgURL;
                } else {
                    // leaving no gap looks better in some cases (EG showConnectors
                    // set to true)
                    icon = null;
                }
            }
        }
        if (icon == this._$blank) icon = isc.Canvas._blankImgURL;
    }
    return icon;
},

//> @method treeGrid.getExtraIcon() (A)
// Get an additional icon to show between the open icon and folder/node icon for a particular 
// node.
// <P>
// NOTE: If +link{listGrid.selectionAppearance} is <code>"checkbox"</code>, this method will
// NOT be called. Extra icons cannot be shown for that appearance.
//
// @param	node (TreeNode)	tree node in question
// @return	(URL)		URL for the extra icon (null if none required)
//
// @visibility external
//<
getExtraIcon : function (record) {
    // Default trees don't make use of this.
    return null;
},

//>	@method	treeGrid.getIcon()
// Get the appropriate icon for a node.
// <P>
// By default icons are derived from +link{folderIcon} and +link{nodeIcon}.
// Custom icons for individual nodes can be overridden by setting the +link{customIconProperty}
// on a node.
// <p>
// If you want to suppress icons altogether, provide an override of this method that simply
// returns null.
// <p> 
// Note that the full icon URL will be derived by applying +link{Canvas.getImgURL()} to the
// value returned from this method.
//
// @param	node (TreeNode)	tree node in question
// @return	(URL)		URL for the icon to show for this node
// @visibility external
//<
getIcon : function (node, defaultState) {
    if (isc.isA.Number(node)) node = this.data.get(node);
    if (!node) return null;
    
    var icon = node[this.customIconProperty],
        customIcon = (icon != null),
        isFolder = this.data.isFolder(node);
  
    if (!customIcon) {
        if (isFolder) icon = this.folderIcon;
        else icon = this.nodeIcon;
    }
    var state;
    if (isFolder) {
        // Default folder icon is the 'closed' icon. This will be used for dragTrackers, etc
        // Note: check for the special _willAcceptDrop flag set by updateDropFolder() - when a
        // user hovers over a folder for a while, we spring it open, and that causes a redraw,
        // but the folder is not necessarily droppable.
        var isDrop = defaultState ? false : (this.lastDropFolder == node && node._willAcceptDrop),
            isOpen = defaultState ? false : !!this.data.isOpen(node),
            isLoading = this.data.getLoadState(node) == isc.Tree.LOADING;
            
        if (isLoading && this.showLoadingIcons) {
            return this.loadingIcon;
        } else if (isDrop) {
            // backCompat - respect old dropIcon / folderDropImage if specified
            if (node.dropIcon != null) icon = node.dropIcon;
            else if (!customIcon && this.folderDropImage != null) icon = this.folderDropImage;
            else {
                var showDrop;
                if (customIcon) {
                    showDrop = node[this.customIconDropProperty];
                    if (showDrop == null) showDrop = this.showCustomIconDrop;
                } else { 
                    showDrop = this.showDropIcons;
                }
                if (showDrop) state = this.dropIconSuffix;
            }
        } else if (isOpen) {
                
            // backCompat - respect old openIcon / folderOpenImage if specified
            if (node.openedIcon != null) icon = node.openedIcon;
            else if (!customIcon && this.folderOpenImage != null) icon = this.folderOpenImage;
            // Don't override already set drop state
            else {
                var showOpen;
                if (customIcon) {
                    showOpen = node[this.customIconOpenProperty];
                    if (showOpen == null) showOpen = this.showCustomIconOpen;
                } else {
                    showOpen = this.showOpenIcons;
                }
                if (showOpen) state = this.openIconSuffix;                
                
                else if (!customIcon) state = this.closedIconSuffix;
            }
        } else {
            
            // Respect old 'folderClosedImage' if specified
            // Otherwise - if the icon is not custom, append "_closed" state
            
            if (!customIcon) {
                if (this.folderClosedImage) icon = this.folderClosedImage;
                else state = this.closedIconSuffix;
            }
        }
    // not a folder:
    } else {
        // Pick up the old 'fileImage' for back compat, if specified.
        if (!customIcon && this.fileImage) icon = this.fileImage;
    }
    return icon == null ? null : isc.Img.urlForState(icon, false, false, state);
},

// helper method - caches generated image templates on a per-draw basis for faster html generation.
_getIconHTMLCacheKey : function (icon, iconWidth, extraRightMargin, iconHeight) {
    return icon + "#w=" + iconWidth + ",extraRightMargin=" + extraRightMargin + ",h=" + iconHeight;
},
_$absMiddle: "absmiddle",

getIconHTML : function (icon, iconID, iconWidth, extraRightMargin, iconHeight) {

    if (icon == null) return isc.emptyString;

    if (iconWidth == null) iconWidth = this.iconSize;
    if (iconHeight == null) iconHeight = iconWidth;

    // make sure the iconHTML cache exists
    // Note this method can fire before drawCache has been set up due to autoSize logic
    // requesting cell HTML before body draw. If this occurs, just default the
    // cache object.
    if (this._drawCache == null) {
        this._drawCache = {};
    }
    var cache = this._drawCache.iconHTML;
    if (cache == null) cache = this._drawCache.iconHTML = {};

    // if not in cache, generate and store - keyed by the image src
    var cacheKey = this._getIconHTMLCacheKey(icon, iconWidth, extraRightMargin, iconHeight),
        template = cache[cacheKey];
    if (template == null) {
        

        var extraCSSText;
        if (extraRightMargin) {
            
            extraCSSText = (this.isRTL() ? "margin-left:" : "margin-right:") + extraRightMargin + "px";
        }

        template = cache[cacheKey] = this._getImgHTMLTemplate({
            src: icon,
            width: iconWidth,
            height: iconHeight,
            name: iconID,
            align: this._$absMiddle,
            extraCSSText: extraCSSText,
            
            generateSpan: isc.Canvas._generateSpanForBlankImgHTML
        });
    }

    // Note: We need to update the image ID for each icon - this is in the 16'th slot in the
    // array of strings used as a template (see Canvas.imgHTML())
    template[16] = iconID;

    return template.join(isc._emptyString);
},


//>	@method	treeGrid.setRowIcon()	(A)
// Set the icon for a particular record to a specified URL (relative to Page.imgDir + this.imgDir
//
//		@param	record		(TreeNode)	tree node
//		@param	URL		(URL)		URL for the record icon
//<
setRowIcon : function (record, URL) {

	// normalize the record from a number if necessary
	if (!isc.isA.Number(record)) record = this.data.indexOf(record);
    // set the image
    
    if (record != -1 && this.getIcon(record) != null) {
        this.setImage(this._iconIDPrefix + record, URL, null, isc.Canvas._generateSpanForBlankImgHTML);
    }
},

//> @method treeGrid.setNodeIcon() 
// Set the icon for a particular treenode to a specified URL
//
//		@param node		(TreeNode) tree node
//		@param icon		(SCImgUrl) path to the resource
//		@group treeIcons
//		@visibility external
//<		
setNodeIcon : function (node, icon) {
	//make the change persist across redraws 
	node[this.customIconProperty] = icon;
	//efficiently refresh the image
    this.setImage(this._iconIDPrefix + this.getRecordIndex(node), icon, null, isc.Canvas._generateSpanForBlankImgHTML);
},

// -------------------
// Printing

getPrintHTML : function (printProperties, callback) {
    var expand = this.printExpandTree;
    if (expand == null) expand = printProperties ? printProperties.expandTrees : null;
    
    if (expand && this.data) {
        if (isc.ResultTree && isc.isA.ResultTree(this.data) && this.data.loadDataOnDemand) {
            this.logWarn("Printing TreeGrid with option to expand folders on print not supported " +
                            "for load on demand trees.");
        } else {
            this.data.openAll();
        }
    }
    return this.Super("getPrintHTML", arguments);
},

// Multiple copies of this string are prepended to the tree field, in order to indent it,
// when exporting tree data via +link{DataBoundComponent.getClientExportData}.
//exportIndentString:null,

getExportFieldValue : function (record, fieldName, fieldIndex) {
    var val = this.Super("getExportFieldValue", arguments);
    
    // Prepend tree depth indent string, ensuring that children of root are not indented
    if (fieldIndex == this.getTreeFieldNum() && this.exportIndentString) {
        var level = this.data.getLevel(record);
        while (--level > 0) val = this.exportIndentString + val;
    }

    return val;
}
});



// Register "stringMethods" for this class
isc.TreeGrid.registerStringMethods({
//    folderDropMove:"viewer,folder,childIndex,child,position",

    //> @method treeGrid.folderOpened()
    //
    // This method is called when a folder is opened either via the user manipulating the
    // expand/collapse control in the UI or via +link{TreeGrid.openFolder()}.  You can return
    // <code>false</code> to cancel the open.
    //
    // @param node (TreeNode) the folder (record) that is being opened
    // 
    // @return (boolean) false to cancel the open, true to all it to proceed
    //
    // @visibility external
    //<
    folderOpened : "node",

    //> @method treeGrid.folderClosed()
    //
    // This method is called when a folder is closed either via the user manipulating the
    // expand/collapse control in the UI or via +link{TreeGrid.closeFolder()}.  You can return
    // <code>false</code> to cancel the close.
    //
    // @param node (TreeNode) the folder (record) that is being closed
    // 
    // @return (boolean) false to cancel the close, true to all it to proceed
    //
    // @visibility external
    //<
    folderClosed : "node",

    //> @method treeGrid.folderClick()
    //
    // This method is called when a folder record is clicked on.
    //
    // @param viewer (TreeGrid) The TreeGrid on which folderClick() occurred.
    // @param folder (TreeNode) The folder (record) that was clicked
    // @param recordNum (number) Index of the row where the click occurred.
    // 
    // @see treeGrid.nodeClick()
    //
    // @visibility external
    //<
    folderClick : "viewer,folder,recordNum",

    //> @method treeGrid.leafClick()
    //
    // This method is called when a leaf record is clicked on.
    //
    // @param viewer (TreeGrid) The TreeGrid on which leafClick() occurred.
    // @param leaf (TreeNode) The leaf (record) that was clicked
    // @param recordNum (number) Index of the row where the click occurred.
    // 
    // @see treeGrid.nodeClick()
    //
    // @visibility external
    //<
    leafClick : "viewer,leaf,recordNum",

    //> @method treeGrid.nodeClick()
    //
    // This method is called when a leaf or folder record is clicked on.  Note that if you set
    // up a callback for <code>nodeClick()</code> and e.g. +link{treeGrid.leafClick()}, then
    // both will fire (in that order) if a leaf is clicked on.
    //
    // @param viewer (TreeGrid) The TreeGrid on which leafClick() occurred.
    // @param node (TreeNode) The node (record) that was clicked
    // @param recordNum (number) Index of the row where the click occurred.
    // 
    // @see treeGrid.folderClick()
    // @see treeGrid.leafClick()
    //
    // @visibility external
    // @example treeDropEvents
    //<
    nodeClick : "viewer,node,recordNum",

    //> @method treeGrid.folderContextClick()
    //
    // This method is called when a context click occurs on a folder record.
    //
    // @param viewer (TreeGrid) The TreeGrid on which the contextclick occurred.
    // @param folder (TreeNode) The folder (record) on which the contextclick occurred.
    // @param recordNum (number) Index of the row where the contextclick occurred.
    //
    // @return (boolean) whether to cancel the event
    //
    // @see treeGrid.nodeContextClick();
    //
    // @visibility external
    //<
    folderContextClick : "viewer,folder,recordNum",

    //> @method treeGrid.leafContextClick()
    //
    // This method is called when a context click occurs on a leaf record.
    //
    // @param viewer (TreeGrid) The TreeGrid on which the contextclick occurred.
    // @param leaf (TreeNode) The leaf (record) on which the contextclick occurred.
    // @param recordNum (number) Index of the row where the contextclick occurred.
    //
    // @return (boolean) whether to cancel the event
    //
    // @see treeGrid.nodeContextClick();
    //
    // @visibility external
    //<
    leafContextClick : "viewer,leaf,recordNum",

    //> @method treeGrid.nodeContextClick()
    //
    // This method is called when a context click occurs on a leaf or folder record.  Note that
    // if you set up a callback for <code>nodeContextClick()</code> and
    // e.g. +link{treeGrid.leafContextClick}, then both will fire (in that order) if a leaf
    // is contextclicked - unless <code>nodeContextClick()</code> returns false, in which case
    // no further contextClick callbacks will be called.
    //
    // @param viewer (TreeGrid) The TreeGrid on which the contextclick occurred.
    // @param node (TreeNode) The node (record) on which the contextclick occurred.
    // @param recordNum (number) Index of the row where the contextclick occurred.
    //
    // @return (boolean) whether to cancel the event
    //
    // @see treeGrid.folderContextClick();
    // @see treeGrid.leafContextClick();
    //
    // @visibility external
    //<
	nodeContextClick : "viewer,node,recordNum",

    //> @method treeGrid.dataArrived
    // Notification method fired whenever this TreeGrid receives new data nodes from the 
    // dataSource. Only applies to databound TreeGrids where +link{treeGrid.data} is a 
    // +link{ResultTree} - either explicitly created and applied via +link{treeGrid.setData()} or
    // automatically generated via a +link{treeGrid.fetchData(),fetchData()} call.
    // @param parentNode (TreeNode) The parentNode for which children were just loaded
    // @visibility external
    //<
    dataArrived:"parentNode",

    //> @method treeGrid.onFolderDrop
    // Notification method fired when treeNode(s) are dropped into a folder of this TreeGrid.
    // This method fires before the standard +link{method:treeGrid.folderDrop} processing occurs
    // and returning false will suppress that default behavior.
    // @param nodes (Array of TreeNode) List of nodes being dropped
    // @param folder (TreeNode) The folder being dropped on
    // @param index (integer) Within the folder being dropped on, the index at which the drop is
    //                        occurring.
    // @param dropPosition (RecordDropPosition) position with respect to the target record
    // @param sourceWidget (Canvas) The component that is the source of the nodes (where the nodes
    //                              were dragged from).
    // @return (boolean) return false to cancel standard folder drop processing
    // @visibility sgwt
    //<
    onFolderDrop:"nodes,folder,index,dropPosition,sourceWidget"
});
