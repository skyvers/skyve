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




//>	@class ColumnTree
//
// The SmartClient system supports hierarchical data (also referred to as tree data
// due to its "branching" organization) with:
// <ul>
//   <li> the +link{class:Tree} class, which manipulates hierarchical data sets
//   <li> the TreeGrid widget class, which extends the ListGrid class to visually
//        present tree data in an expandable/collapsible format.
//   <li> the ColumnTree widget class, which visually
//        presents tree data in a so-called 
//        "<a href=http://en.wikipedia.org/wiki/Miller_Columns>Miller Column</a>" format.
// </ul>
// For information on DataBinding Trees, see +link{group:treeDataBinding}.
// <p>
// A ColumnTree shows a single branch of the underlying +link{class:Tree} horizontally, from 
// left to right. Thus, the leftmost column shows all the top-level nodes. When the user clicks
// one of those nodes, a new column is shown immediately to the right of the top-level column,
// showing the selected node's children. Additional columns are shown as required to present
// lower-level children. The behavior of ColumnTree is similar to that of the Browser interface
// in the Apple&trade; iTunes&trade; application.
// 
// @implements DataBoundComponent    
// @treeLocation Client Reference/Grids
// @visibility external
//<

// define us as a subclass of Layout
isc.ClassFactory.defineClass("ColumnTree", "Layout", "DataBoundComponent");

isc.ColumnTree.addClassProperties({

	// default field to display a ColumnTree's nodes
	TREE_FIELD : {
	    // Arbitrary name - ListGrids expect their fields to be identified by a name, even
	    // though in this case we're not representing a field in the record objects
	    name:"treeField",
	    width:"*",
    	getCellValue : function (list,record,recordNum,colNum) {
            return list.creator.getCellValue(list, record, recordNum, colNum);
        }
    }

});

isc.ColumnTree.addProperties({
    orientation: "horizontal",

    animateMemberEffect:{effect:"slide", startFrom:"L", endAt:"R"},
    
	//>	@attr	columnTree.dataSource		(DataSource or ID : null : IRW)
    // @include dataBoundComponent.dataSource
	//<

	//>	@attr columnTree.data (Tree : null : IRW)
	// A +link{class:Tree} object consisting of nested +link{object:TreeNode}s to 
    // display in this ColumnTree.  
    // The <code>data</code> property will typically not be explicitly specified for 
    // databound ColumnTrees, where the data is returned from the server via databound component
    // methods such as <code>fetchData()</code>
	// @setter setData()
	// @group data
	// @visibility external
	//<

	//> @attr columnTree.dataProperties (Tree : null : IR)
    // For a <code>ColumnTree</code> that uses a DataSource, these properties will be passed to
    // the automatically-created ResultTree.  This can be used for various customizations such as
    // modifying the automatically-chosen +link{tree.parentIdField}.
    // @group databinding
    // @visibility external
    //<

    //> @method columnTree.fetchData()
    // Uses a "fetch" operation on the current +link{DataSource,grid.dataSource} to retrieve data
    // that matches the provided criteria, and displays the matching data in this component as a
    // tree.
    // <P>
    // This method will create a +link{ResultTree} to manage tree data, which will
    // subsequently be available as <code>columnTree.data</code>.  DataSource records
    // returned by the "fetch" operation are linked into a tree structure according to
    // +link{dataSourceField.primaryKey,primaryKey} and
    // +link{dataSourceField.foreignKey,foreignKey} declarations on DataSource fields.  See the
    // +link{group:treeDataBinding} topic for complete details.
    // <P>
    // By default, the created ResultTree will use folder-by-folder load on demand, asking the
    // server for the children of each folder as the user opens it.
    // <P>
    // The +link{ResultTree} created by <code>fetchData()</code> can be customized by setting
    // +link{columnTree.dataProperties} to an Object containing properties and methods to apply to
    // the created ResultTree.  For example, the property that determines whether a node is a
    // folder (+link{Tree.isFolderProperty,isFolderProperty}) can be customized, or
    // level-by-level loading can be disabled via
    // +link{resultTree.loadDataOnDemand,loadDataOnDemand:false}.
    // <P>
    // The callback passed to <code>fetchData</code> will fire once, the first time that data is
    // loaded from the server.  If using folder-by-folder load on demand, use the
    // +link{resultTree.dataArrived()} notification to be notified each time new nodes are loaded.
    // <P>
    // Note that, if criteria are passed to <code>fetchData()</code>, they will be passed every
    // time a new "fetch" operation is sent to the server.  This allows you to retrieve multiple
    // different tree structures from the same DataSource.  However note that the server is expected
    // to always respond with an intact tree - returned nodes which do not have parents are dropped
    // from the dataset and not displayed.
    //
    // @include dataBoundComponent.fetchData()
    // @group dataBoundComponentMethods
    // @visibility external
    //<
    
    //>	@attr   columnTree.autoFetchData       (boolean : false : IR)
    // @include dataBoundComponent.autoFetchData
    // @group databinding
    // @visibility external
    //<	

    //>	@attr columnTree.autoFetchTextMatchStyle       (TextMatchStyle : null : IR)
    // @include dataBoundComponent.autoFetchTextMatchStyle
    // @group databinding
    // @visibility external
    //<

    //> @method columnTree.filterData()
    // @include dataBoundComponent.filterData()
    // @group dataBoundComponentMethods
    // @visibility external
    //<

    //> @attr columnTree.initialCriteria   (Criteria : null :IR)
    // @include dataBoundComponent.initialCriteria
    // @visibility external
    //<
    
	//>	@attr columnTree.showDetailFields (Boolean : true : IR)
	// @include dataBoundComponent.showDetailFields
    //<

    //> @attr columnTree.dataFetchMode (FetchMode : "paged" : IRW)
    // @include dataBoundComponent.dataFetchMode
    //<
    
    
        
    //>	@attr	columnTree.folderIcon        (SCImgURL : "[SKIN]folder.gif" : [IRW])
    // The URL of the base icon for all folder nodes in this columnTree. Note that this URL will
    // have +link{openIconSuffix} or +link{closedIconSuffix} appended to 
    // indicate state changes if appropriate - see documentation on  +link{showOpenIcons}
    // @group treeIcons
    // @visibility external
    //<
    folderIcon:"[SKIN]/folder.gif",
    
    //> @attr   columnTree.customIconProperty   (String : "icon" : [IRW])
    // This property allows the developer to customize the icon displayed next to a node.
    // Set <code>node[grid.customIconProperty]</code> to the URL of the desired icon to display and
    // it will be shown instead of the standard +link{nodeIcon} for this node.<br>
    // Note that if +link{showCustomIconOpen} 
    // is true for this grid, customized icons for folder nodes will be appended with the 
    // +link{openIconSuffix} suffix on state change,
    // as with the standard +link{folderIcon}.  Also note that for
    // custom folder icons, the +link{closedIconSuffix} will never be appended.
    // @group treeIcons
    // @visibility external
    //<
    customIconProperty:"icon",
    
    //>	@attr	columnTree.skinImgDir		(URL : "images/TreeGrid/" : IRWA)
    //		Where do 'skin' images (those provided with the class) live?
    //		This is local to the Page.skinDir. By default the ColumnTree shares icons with
    //      the TreeGrid class.
    //		@group	appearance, images
    //<
    skinImgDir:"images/TreeGrid/",	

    //>	@attr   columnTree.nodeIcon       (SCImgURL : "[SKIN]file.gif" : [IRW])
    // @include treeGrid.nodeIcon
    // @group treeIcons
    // @example millerColumns
    // @visibility external
    //<	
    nodeIcon: "[SKIN]file.gif",
    
    //>	@attr   columnTree.openIconSuffix   (String : "open" : [IRW])
    // @include treeGrid.openIconSuffix
    // @group treeIcons
    // @visibility external
    //<	
    openIconSuffix: "open",
    
    //>	@attr   columnTree.closedIconSuffix   (String : "closed" : [IRW])
    // @include treeGrid.closedIconSuffix
    // @group treeIcons
    // @visibility external
    //<	
    closedIconSuffix: "closed",
    
    //>	@attr   columnTree.showOpenIcons   (Boolean : true : [IRW])
    // @include treeGrid.showOpenIcons
    // @group treeIcons
    // @example millerColumns
    // @visibility external
    //<	
    showOpenIcons: true,
    
    //> @attr   columnTree.showCustomIconOpen   (Boolean : false : [IRWA])
    // @include treeGrid.showCustomIconOpen
    // @group treeIcons
    // @visibility external
    //<	
    showCustomIconOpen:false,

    //> @attr   columnTree.customIconOpenProperty (string : "showOpenIcon" : [IRWA])
    // @include treeGrid.customIconOpenProperty
    // @group treeIcons
    // @visibility external
    //<	
    customIconOpenProperty:"showOpenIcon",

    //> @attr columnTree.showMultipleColumns   (boolean : null : [IRW])
    // When set to false, only displays a single column at a time, showing a slide animation 
    // when moving between columns.
    // @group treeIcons
    // @visibility external
    //<	
    //showMultipleColumns: true,

    //>	@attr   columnTree.loadDataOnDemand    (boolean : null : IR)
    // For databound columnTree instances, should the entire tree of data be loaded on initial 
    // fetch, or should each column be loaded as needed. If unset the default 
    // ResultTree.loadDataOnDemand setting will be used.
    // @group databinding
    // @visibility external
    //<

    //> @attr columnTree.column (MultiAutoChild ListGrid : null : IR)
    // Instance of ListGrid used to display each column of the tree.
    // @visibility external
    //<
    
    // column is an autoChild so columnConstructor / defaults / properties are all implied.
    // Explicitly doc columnProperties to make this an obvious override point, and to
    // have it be picked up by SGWT
    //> @attr columnTree.columnProperties (ListGrid properties : null : IRA)
    // Standard set of properties to apply to each generated +link{columnTree.column,column}
    // in this columnTree. Developers may also override +link{columnTree.getColumnProperties()}
    // to return dynamic properties based on the node being displayed.
    // @visibility external
    //<
    
    showColumn:true,
    columnConstructor:"ListGrid",
    
    columnDefaults: {
        
        animateTime: 100,
        animateEffect: "slide",
       
        // disable canAddFormulaField / canAddSummaryField
        canAddFormulaFields:false,
        canAddSummaryFields:false,
        
        canSort:false,
        canGroupBy:false,
        showHeaderMenuButton:false,
        selectionChanged: function (record, state) {
            if (state) {
               this.creator.nodeSelected(this, record);
            }
        },
        bodyProperties: {
            _updateCellStyle : function(record, rowNum, colNum, cell, className) {
                var zeroBorderPadding = "padding:0px;border:0px;";
                if (cell == null) cell = this.getTableElement(rowNum, colNum);
                if (cell == null) return; // cell not currently drawn
        
                if (!this.showHiliteInCells) 
                {
                    if (record == null) record = this.getCellRecord(rowNum, colNum);
                    // determine the CSS style className if not provided
                    if (className == null) className = this.getCellStyle(record, rowNum, colNum);
                    // There will be a clip-div around the table.
                    // In IE the NOBR also counts as a node.
                    var table = cell.firstChild; 
                    while (table && table.tagName != "TABLE") table = table.firstChild;
                    if (table) {
                        table.className = className;
                        if (this.getCellCSSText) {                    
                            // Use this._getCompleteCellCSSText
                            // This handles the overflow settings for Moz, converting the 
                            // getCellCSSText stringMethod to a method, etc.
                            cell.style.cssText = isc.StringBuffer.concat(
                                                    this._getCompleteCellCSSText(
                                                       record, rowNum, colNum, className),
                                                    this.zeroBorderPadding);
                        }
                    }
                }
                
                // Actually style the cell itself
                return isc.GridRenderer.getPrototype()._updateCellStyle.apply(
                                                this, [record, rowNum, colNum, cell, className]);
            } 
        }  
    },
    
    //> @attr columnTree.fields (Array of ListGridField : null : IRW)
    // An array of field objects, specifying the order, layout, dynamic calculation, and
    // sorting behavior of each field in each column in the columnTree object. In ColumnTrees, 
    // the fields array specifies sub-columns within each main column. 
    // Each field in the fields array is a ListGridField object.
    // <p>
    // If +link{ColumnTree.dataSource} is also set, this value acts as a set of overrides as
    // explained in +link{attr:DataBoundComponent.fields}.
    //
    // @group databinding
    // @see ListGridField
    // @visibility external
    //<
    
    //> @attr columnTree.showHeaders (Boolean : false : IR)
    // If set, each column in the ColumnTree will show a header with the title of the selected
    // node from the column to the left.
    // @visibility external
    //<
    showHeaders: false,
    
    //> @attr columnTree.firstColumnTitle (String : " " : IR)
    // A title for the leftmost column if +link{showHeaders} is set (the remaining columns
    // have their titles derived from the item selected in the column to the left). Ignored
    // if +link{showHeaders} is not set.<br><br>
    // Note: if you do not want a heading for the first column leave this attribute at its 
    // default value of " ". If you set it to null or the empty string, SmartClient will
    // fall back to displaying the field's name in the heading.
    // @visibility external
    //<
    firstColumnTitle: "&nbsp;",
    
    //> @attr columnTree.showNodeCount (Boolean : false : IR)
    // If set, and +link{showHeaders} is also set, each column's header will show 
    // a count of the number of nodes in that column
    // @visibility external
    //<
    showNodeCount: false,
    
	// don't wrap, as that will mess up the look of the trees
	wrapCells: false,
	
	// iconPadding - padding between the folder open/close icon and text.
    // Make this customizable, but not exposed - very unlikely to be modified
    iconPadding: 3,
	
    
    ignoreEmptyCriteria: false,
    
    //>	@attr columnTree.backButtonTitle (String : "Back" : [IRW])
    // When using +link{columnTree.showMultipleColumns, single-column mode}, this i18n property
    // dictates the title for the +link{columnTree.backButton, button} docked to the top left 
    // which allows navigation back through the column tree.
    // @group i18nMessages
    // @visibility external
    //<	
    backButtonTitle: "Back",

    //> @attr columnTree.backButton (AutoChild IButton : null : [IRW])
    // When using +link{columnTree.showMultipleColumns, single-column mode}, this is the
    // "Back" button that you see hovering above the column UI and that allows backward
    // navigation.
    // @visibility external
    //<	
    backButtonDefaults: {
        _constructor: "IButton",
        snapTo: "TR",
        left: 5,
        top: 5,
        autoFit: true,
        click : function () {
            this.creator.navigateBack();
        }
    },
    
    overflow: "hidden"
    
});

isc.ColumnTree.addMethods({    
    
// Can't do this with a columnDefaults attribute because we need to clone the fields
getDynamicDefaults : function(autoChildName) {
    
    if (autoChildName == "column") {   // That's our only autoChild at the moment    
        return {
            autoDraw: false,
            showHiliteInCells: true,
            leaveScrollbarGap: false,
            selectionType: "single",
            showHeader: false,
            fields: isc.clone(this.fields)
        };
    }
    
},

initWidget : function () {
	this.Super("initWidget", arguments);

    // default showMultipleColumns to false if it's unset and we're running on a handset
    if (this.showMultipleColumns == null) 
        this.showMultipleColumns = !isc.Browser.isHandset;
    
    this.columns = [];

    // if no dataSource is specified, pick up the dataSource off the data model
    if (!this.dataSource && this.data != null && this.data.dataSource) {
        this.dataSource = this.data.dataSource;
    }
    
    // if the fields are not set or of zero length, initialize with a single TREE_FIELD
	if (!this.fields || this.fields.length == 0) {
		this.fields = [isc.ColumnTree.TREE_FIELD];
	}

    if (this.showMultipleColumns == false && this.showHeaders && this.showBackButton != false) {
        this.backButton = this.createAutoChild("backButton", { title: this.backButtonTitle, disabled: true });
        this.addChild(this.backButton);
        this.backButton.bringToFront();
    }
    
    // Show the first column (the direct children of root in the underlying tree)
    this.columns[0] = this.createAutoChild("column", 
        this.getColumnProperties(this.data ? this.data.getRoot() : null, 0), null, false);
    this.addColumn(this.columns[0], 0);	        
    
    this.currentColumn = 0;
    
    if (this.data) this.populateFirstColumn();

},

populateFirstColumn : function () {
    if (this.data.showRoot) {
        this.columns[0].setData([this.data.getRoot()]);
    } else {
        var children = this.data.getChildren(this.data.getRoot());
        if (isc.isA.ResultSet(children)) {
            children = children.getAllLoadedRows();
        }
        this.columns[0].setData(children);
    }
    
    if (this.shouldShowHeader(null, 0)) {
        this.columns[0].setShowHeader(true);
        this.columns[0].setFieldProperties(0, {title: this.getColumnTitle(null, 0)});
    }
},

// Return true when this column should not affect columns to its right.
// Useful for "non-tree" columns that should not close other columns when selected.
treeIsTied : function (column, node) {
    return false;
},

//> @method columnTree.getSelectedRecord()
// Get the selected record, that is, the parent of the nodes in the rightmost visible column.
// <P>
// This is generally the most recently clicked node unless programmatic navigation has taken
// place.
// <P>
// If only the first column is showing, the root node is returned (which can be detected via
// +link{Tree.isRoot()}).
//
// @return (Record) the selected record
// @visibility external
//<
getSelectedRecord : function () {
    if (this.currentColumn <= 0) return this.data.getRoot();
    var column = this.getColumn(this.currentColumn-1);
    return column.getSelectedRecord();
},

//> @method columnTree.navigateBack()
// Navigate to the previous column.
//
// @visibility external
//<
navigateBack : function () {
    if (this.currentColumn <= 0) return;

    // nodeSelected takes the column that was clicked and the record that was clicked in that
    // column.  Navigating backward is the same as if a record was just clicked *2* levels
    // back.  In other words, if you are in column 1 (two levels showing), column 1 will go
    // away, column 0 will be the only column visible, so it's as though the root node was just
    // selected.
    var column, record;
    if (this.currentColumn >= 2 ) {
        column = this.getColumn(this.currentColumn-2);
        record = column.getSelectedRecord();
    } else {
        column = this.getColumn(0);
        record = this.data.getRoot();
    }

    this.logInfo("navigating to column: " + (this.currentColumn-1) + 
                 " to node: " + this.data.getTitle(record));

    this.nodeSelected(column, record, true);
},


slideTransition : function (oldPane, newPane, container, right) {
    if (!isc.Browser.isWebKit) {
        if (right) {
            newPane.deselectAllRecords();
            newPane.animateShow();
        } else {
            oldPane.animateHide();
            newPane.show();
        }
        newPane.bringToFront();
        
        return;
    }

    this.logInfo((right ? "right" : "left") + " slideTransition from: " + 
                 oldPane + " to " + newPane + " within " + container);

    // draw the new pane hidden
    newPane.hide();
    container.addChild(newPane);
    if (!newPane.isDrawn()) newPane.draw();

    var oldStyle = oldPane.getStyleHandle();
    var newStyle = newPane.getStyleHandle();
    
    // place the new element offscreen right (instantly)
    newStyle.setProperty("-webkit-transition", "none");
    var translation = "translate3d(" + (right ? "-" : "") + container.getViewportWidth() + "px, 0%, 0%)";
    newStyle.setProperty("-webkit-transform", translation);

    var oldOverflow = container.overflow;
    container.setOverflow("hidden");
    // will be initially invisible since clipped
    newPane.show();

    
    isc.Timer.setTimeout(function () {
    
        // set both to animate
        oldStyle.setProperty("-webkit-transition", "-webkit-transform 0.3s ease-in-out");
        newStyle.setProperty("-webkit-transition", "-webkit-transform 0.3s ease-in-out");

        // move old offscreen
        translation = "translate3d(" + (right ? "" : "-") + container.getViewportWidth() + "px, 0%, 0%)";
        oldStyle.setProperty("-webkit-transform", translation);

        // undo translation on new
        newStyle.setProperty("-webkit-transform", "translate3d(0px, 0%, 0%)");

        isc.Timer.setTimeout(function () { 
            oldPane.hide(); 
            container.setOverflow(oldOverflow);
        }, 350);
    
    }, 0);
},

//> @method columnTree.nodeSelected() 
// Called when a node is selected in any column.  Default behavior is to show the next level
// of the tree in a column to the right of the current column.
// <P>
// The new column will be created if it is not already showing.  Any columns further to the
// right, showing deeper levels of the tree, will be removed.
// @param column [ListGrid] the column where a node was selected
// @param node [TreeNode] the node that was selected
// @return (boolean) override and return false to cancel the default action
// @visibility external
//<
nodeSelected : function (column, node, backward) {
    // Give the 'onNodeSelected' handler an opportunity to suppress default handling if present
    
    if (this.onNodeSelected != null && (this.onNodeSelected(column,node) == false)) {
        return;
    }
    
    var idx = this.getColumnIndex(node),
        isFolder = this.data.isFolder(node);
    
    // Hide columns to the right of the column immediately to the right of the one we clicked. 
    // The column immediately to the right of the one we clicked is going to be repopulated 
    // with the clicked node's children, and possibly a new heading; anything further to 
    // the right is no longer relevant.
    var nextColumnIdx = idx + 1;
    
    if (!isFolder) nextColumnIdx -= 1;
    var nextColumn = this.columns[nextColumnIdx];
    if (!this.treeIsTied(column, node)) {
        if (this.showMultipleColumns != false) this.hideColumnsToRight(nextColumnIdx);

        if (!isFolder) return;

        this.data.openFolder(node);

        // Create or re-use a list grid, as appropriate    
        if (isc.isA.ListGrid(nextColumn)) {
            
            nextColumn.deselectAllRecords();
            var children = this.data.getChildren(node);
            if (isc.isA.ResultSet(children)) {
                children = children.getAllLoadedRows();
            }
            nextColumn.setData(children);
            this.addColumn(nextColumn, nextColumnIdx);
        } else {
            nextColumn = this.columns[nextColumnIdx] = this.createAutoChild("column", 
                this.getColumnProperties(node, idx+1), null, false);
            var children = this.data.getChildren(node);
            if (isc.isA.ResultSet(children)) {
                children = children.getAllLoadedRows();
            }
            nextColumn.setData(children);
            this.addColumn(nextColumn, nextColumnIdx);
        }

        // Fix up column headings
        if (this.shouldShowHeader(node, nextColumnIdx)) {
            nextColumn.setShowHeader(true);
            var newTitle = this.getColumnTitle(node, nextColumnIdx);
            nextColumn.setFieldProperties(0, {title: newTitle});
        }
        
        // If the data is already locally cached, add the node count (if required)
        // This will be done asynchronously if we need a data fetch here
        if (nextColumn.data.getLength() > 0) {  
            this.updateHeadingNodeCount(node);
        }
    }

    //var columnToHide = (backward ? nextColumn : column);
    //var columnToShow = (backward ? column : nextColumn);

    var columnToHide = (backward ? this.columns[this.currentColumn] : column);
    var columnToShow = nextColumn; //(backward ? column : nextColumn);
    
    if (this.showMultipleColumns == false) {
        this.slideTransition(columnToHide, columnToShow, this, backward ? true : false); 
    } else {
        columnToShow.show();
    }

    this.currentColumn = (nextColumnIdx < 0 ? 0 : nextColumnIdx);

    this.logInfo("currentColumn is now: " + this.currentColumn);
    if (this.backButton) {
        //this.columns[nextColumn].addChild(this.backButton);
        this.backButton.bringToFront();
        this.backButton.setDisabled(this.currentColumn <=0);
    }
    
},



addColumn : function (column, index) {
    if (this.showMultipleColumns == false) {
        column.resizeTo("100%", "100%");
        this.addChild(column, index);
    } else {
        this.addMember(column, index);
    }
},


getCurrentTitle : function () {
    return this.columns[this.currentColumn].getFieldTitle(0);
},

getPreviousTitle : function () {
    if (this.currentColumn <= 0) return "";
    return this.columns[this.currentColumn - 1].getFieldTitle(0);
},

updateHeadingNodeCount : function (parentNode) {

    var idx = this.getColumnIndex(parentNode);
    if (!this.shouldShowHeader(parentNode, idx) || !this.showNodeCount) return;
    
    if (idx < 0) return;  // Node count is not applicable to the first column
    if (this.columns[idx+1].data.getLength() == 0) return;

    var newTitle = this.data.getTitle(parentNode);
    if (this.showNodeCount) {
        newTitle = newTitle + " (" + this.columns[idx+1].data.getLength() + ")";
    }
    this.columns[idx+1].setFieldProperties(0, {title: newTitle});

},    

getColumnIndex : function (treeNode) {
    if (this.data.showRoot) {
        return this.data.getLevel(treeNode);
    } else {
        var level = this.data.getLevel(treeNode);
        //return level - (level==0 ? 0 : 1);
        return level-1;
    }
},

hideColumnsToRight : function (idx) {
    for (var i = idx+1; i < this.columns.length; i++) {
        this.columns[i].hide();
        this.columns[i].deselectAllRecords();
    }
},

    
//> @method columnTree.shouldShowHeader()
// Whether the indicated column should show a header.  Returns this.showHeaders by default,
// override for different behavior.
// 
// @param node (TreeNode) parent node for the nodes to be shown in the column
// @param colNum (int) index of the column
// @visibility external
//<

shouldShowHeader : function (node, colNum) {
    return this.showHeaders;
},

//> @method columnTree.getColumnTitle()
// Returns the title to show for the header of indicated column.  Only called if
// +link{shouldShowHeader()} returns true for this column.
// <P>
// By default, returns +link{firstColumnTitle} for the first column, and for subsequent
// columns, the result of +link{Tree.getTitle(),this.data.getTitle()} called on the
// <code>node</code> passed to this function.
// 
// @param node (TreeNode) parent node for the nodes to be shown in the column
// @param colNum (int) index of the column
// @visibility external
//<

getColumnTitle : function (node, colNum) {
    if (colNum == 0) {
        return this.firstColumnTitle;
    } else {
        return this.data.getTitle(node);
    }
},

//> @method columnTree.getRecord()
// Retrieve a record by index.  
// <P>
// If <code>colNum</code> is passed, returns the record found in that column at that index,
// or null if the column doesn't exist or the index is too high.
// <P>
// With no <code>colNum</code> parameter, a record's index is it's position counting from the
// first record of the first column and including all records in each column. Note that both
// index and colNum are zero-based - so the first column is column 0, not column 1.
// 
// @param index (int) index of record to return.
// @param [colNum] (Integer) optional index of the column
// @return (TreeNode) node at the specified index
// @visibility external
//<

getRecord : function (index, colNum) {
    if (index == null || index < 0) return null;
    
    if (colNum != null) {
        if (colNum < 0 || colNum > this.columns.length) {
            return null;
        }
        if (index > this.columns[colNum].data.length ||
            !this.columns[colNum].isVisible()) {
            return null;
        }
        return this.columns[colNum].data[index];
    }
    
    var count = 0;
    for (var idx = 0; idx < this.columns.length; idx++) {
        if (!this.columns[idx].isVisible()) continue;
        if (count + this.columns[idx].data.length > index) {
            return this.columns[idx].data[index-count];
        }
        count += this.columns[idx].data.length;
    } 
    
    return null;
},

// These HTML generation functions were copied from TreeGrid and are very similar.
// This code needs to be factored out of both ColumnTree and TreeGrid, and placed in ListGrid

_$treeCellTemplate:[
    "<table cellpadding=0 cellspacing=0 class='",       // [0]
    ,                                                   // [1] - this.getCellStyle()
    "' style='",                                        // [2]
    ,                                                   // [3] - get.getCellCSSText()
        
    "border:0px;padding:0px;'><tr><td>",                // [4]
    ,                                                   // [5] - indentHTML
    "</td>",
    
    
    "<td>" + (isc.Browser.isSafari || isc.Browser.isIE ? "<nobr>" : ""), // [6],                                             // [6]
    ,                                                   // [7] - opener icon HTML
    ,                                                   // [8] - 'extra' icon if there is one
    ,                                                   // [9] - icon for item (eg folder/file icon)
    (isc.Browser.isSafari ? "</nobr>" : "") + 
        "</td><td style='padding-left:",                // [10]
    ,                                                   // [11] - this.iconPadding
    "px;'>",                                            // [12]
    ,                                                   // [13] - NOBR or null
    ,                                                   // [14] - value
    "</td>",
    "</tr></table>"
],

getTreeCellValue : function (value, list, record, recordNum, fieldNum) {

    // This returns HTML to achieve
    //  - open / close icon
    //  - an optional additional icon
    //  - Folder / Record icon
    //  - title for the cell.
    // (It differs from the equiv. function in TreeGrid in that it doesn't add
    //  an indent dependent on level in the tree)
    // If passed a null record just return the value passed in.
    if (record == null) {
       return value;
    }

    var template = this._$treeCellTemplate;
    
    template[1] = list.getCellStyle(record, recordNum, fieldNum);
    template[3] = list.getCellCSSText(record, recordNum, fieldNum);
    
    // Get the HTML for the icons and title from _getTreeCellTitleArray(), and fold them
    // into our template
    var titleCellTemplate = this._getTreeCellTitleArray(value, record, recordNum, true);
    for (var i = 0; i < 10; i++) {
        template[6+i] = titleCellTemplate[i];
    }
    return template.join(isc.emptyString);    
},

// _getTreeCellTitleArray() - helper method for getTreeCellValue() to return the
// "title" portion of the treeCell value - that is: the icons and the title, without
// any indent

_$treeCellTitleTemplate:[
    
    "<td>" + (isc.Browser.isSafari || isc.Browser.isIE ? "<nobr>" : ""), // [0]
    ,                                                   // [1] - opener icon HTML
    ,                                                   // [2] - 'extra' icon if there is one
    ,                                                   // [3] - icon for item (eg folder/file icon)
    (isc.Browser.isSafari ? "</nobr>" : "") + 
        "</td><td style='padding-left:",                // [4]
    ,                                                   // [5] - this.iconPadding
    "px;'>",                                            // [6]
    ,                                                   // [7] - NOBR or null
    ,                                                   // [8] - value
    "</td>"
],
_getTreeCellTitleArray : function (value, record, recordNum, showOpener) {
    
    var template = this._$treeCellTitleTemplate;
//    if (showOpener) {
        // opener icon (or small indent)
//        var openIcon = this.getOpenIcon(record),        
//            openIconSize = this.openerIconSize || (this.showConnectors ? this.cellHeight : null),
//            openerID = (recordNum != null ? this._openIconIDPrefix+recordNum : null);
            
//        if (openIcon) {
//            template[1] = this.getIconHTML(openIcon, openerID, openIconSize);
//        } else {
//            template[1] = this._indentHTML(openIconSize || this.iconSize);
//        }
//    } else 
      template[1] = null;
    
//    var extraIcon = this.getExtraIcon(record),
//        extraIconID = (recordNum != null ? this._extraIconIDPrefix+recordNum : null), 
      var icon = this.getIcon(record),
          iconID = (recordNum != null ? this._iconIDPrefix+recordNum : null);
        
    // extra icon if there is one
//    template[2] = (extraIcon ? this.getIconHTML(extraIcon, extraIconID) : null);
    template[2] = null;
    // folder or file icon
    template[3] = this.getIconHTML(icon, iconID, record.iconSize);
    
    template[5] = this.iconPadding;
    template[7] = this.wrapCells ? null : "<NOBR>"
    template[8] = value;
    return template;
},


// Override getCellValue() to return custom HTML for the tree-field
// Note: Developers are always advised to override formatCellValue rather than this method
// directly (which could lead to certain conflicts). 
getCellValue : function (list, record, rowNum, colNum, a, b, c, d) {
    var value = this.getNodeTitle(record, rowNum);
    value = this.getTreeCellValue(value, list, record, rowNum, colNum);
    return value;
},

//>	@method	columnTree.getIcon()
// @include treeGrid.getIcon
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
            isOpen = defaultState ? false : !!this.data.isOpen(node);

        if (isDrop) {
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
        }  else {
            
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
    return isc.Img.urlForState(icon, false, false, state);
},

// cut/paste from TreeGrid - in TG, this was doing some caching, but the existence of the 
// cache depended on a method called bodyDrawing() running, which I don't have because of my
// different inheritance stack.
_$absMiddle: "absmiddle",
_imgParams: {},
getIconHTML : function (icon, iconID, iconSize) {
    if (icon == null) return isc.emptyString;

    if (iconSize == null) iconSize = this.iconSize;

    
    var imgParams = this._imgParams;
    imgParams.src = icon;
    imgParams.width = imgParams.height = iconSize;
    imgParams.name = iconID;
    imgParams.align = this._$absMiddle;
    var template = this._getImgHTMLTemplate(imgParams);

    // Note: We need to update the image ID for each icon - this is in the 16'th slot in the
    // array of strings used as a template (see Canvas.imgHTML())
    template[16] = iconID;

    return template.join(isc._emptyString);
},

//> @method columnTree.getNodeTitle()
//
// Returns the title to show for a node in the ColumnTree.  The default implementation
// returns the result of calling +link{method:Tree.getTitle} on the node.
// <br><br>
// You can override this method to return a custom title for nodes in the tree.
//
// @param node (TreeNode) The node for which the title is being requested.
// @param recordNum (Number) The index of the node.
// @param field (DSField) The field for which the title is being requested.
// 
// @return (HTML) the title to display.
//
// @see method:Tree.getTitle
//
// @visibility external
//<
getNodeTitle : function (record, recordNum, field) {

    return this.data.getTitle(record);
},


//>	@method	columnTree.getData()
// Returns the +link{class:Tree} object this ColumnTree is viewing
//
// @visibility external
//<
getData : function () {
	return this.data;
},

//>	@method	columnTree.setData()
// Set the +link{class:Tree} object this ColumnTree will view
//
// @param newData (Tree) Tree to show
// @visibility external
//<
setData : function (newData,a,b,c) {
    
    if (!isc.isA.Tree(newData)) return;
    
    this.data = newData;

    this.data.columnTree = this;
    this.data.dataArrived = "this.columnTree.updateHeadingNodeCount(parentNode);";

    // Set the `separateFolders` and `showRoot` options of the tree as well.
    this.data.setSeparateFolders(this.separateFolders);

    if (this.showRoot && isc.isA.ResultTree(this.data)) {
        this.logWarn("showRoot may not be set with a databound columnTree, unexpected " +
                     "results may occur");
    }
    this.data.setShowRoot(this.showRoot);

    // Should we show only branches or leaves.
    this.data.setOpenDisplayNodeType(this.displayNodeType);

	// Initiate the component
    this.data.openFolder(this.data.root);
    // If we have any other columns open, hide them now.
    
    this.hideColumnsToRight(0);
    this.populateFirstColumn();
},


// Override the normal data model implementations, because we want a ResultTree...
useExistingDataModel : function (criteria, operation, context) {
    return false;  // ColumnTree always builds a new tree
},

createDataModel : function (criteria, operation, context) {
    return this.createResultTree(criteria, context.afterFlowCallback, context, null);
},    

updateDataModel : function (criteria, operation, context) {
   // return this.createResultTree(criteria, context.callback, context, null);
},

//> @method columnTree.getColumn() [A]
// Advanced API - get the ListGrid representing the indicated column.
// @param column (int or TreeNode) column number, or parent node of the nodes shown in the
// column
// @return (ListGrid) ListGrid that renders the indicated column, or null if column is not
// shown
// @visibility external
//<
getColumn : function (col) {
    if (isc.isAn.Object(col)) { // assume a TreeNode
        var idx = this.getColumnIndex(col) + 1;
        if (this.columns[idx] && this.columns[idx].isVisible()) return this.columns[idx];
    } else {
       if (this.columns[col] && col <= this.currentColumn) return this.columns[col];
    }
    return null;
},

//> @method columnTree.getColumnProperties() [A]
// Additional properties to apply to the ListGrid that will show the indicated column.
// Returns nothing by default. This method can be overridden to allow, for example, different 
// styling, icons, or row heights per column.
// @param node (TreeNode) parent node for the nodes to be shown in the column
// @param colNum (int) index of the column
// @return (ListGrid Properties) properties to be applied to the column
// @visibility external
//<
getColumnProperties : function (node, colNum) {
    
},

// Selection
// --------------------------------------------------------------------------------------------

// Simple helper methods to avoid having to refer directly to this.selection
// Actually implemented in DataBoundComponent

//> @method columnTree.selectRecord()
//
// Select/deselect a +link{Record} passed in explicitly, or by index.
//
// @param record (Record | number) record (or row number) to select
// @param [newState] (boolean) new selection state (if null, defaults to true)
// @param [colNum] (number) Column number 
//
// @group selection
// @visibility external
//<

//> @method columnTree.deselectRecord()
//
// Deselect a +link{Record} passed in explicitly, or by index.
// <P>
// Synonym for <code>selectRecord(record, false)</code>
//
// @param record (Record | number) record (or row number) to deselect
// @param [colNum] (number) Column number 
//
// @group selection
// @visibility external
//<

//> @method columnTree.selectRecords()
//
// Select/deselect a list of +link{Record}s passed in explicitly, or by index.
//
// @param records (Array of Record | numbers) records (or row numbers) to select
// @param [newState]  (boolean) new selection state (if null, defaults to true)
// @param [colNum] (number) Column number 
//
// @group selection
// @visibility external
//<

//> @method columnTree.deselectRecords()
//
// Deselect a list of +link{Record}s passed in explicitly, or by index.
// <P>
// Synonym for <code>selectRecords(records, false)</code>
//
// @param records (Array of Record | numbers) records (or row numbers) to deselect
// @param [colNum] (number) Column number 
//
// @group selection
// @visibility external
//<

//> @method columnTree.selectAllRecords()
// Select all records in the supplied column (the first column if none is passed)
//
// @param [colNum] (number) Column number 
// @group selection
// @visibility external
//<
selectAllRecords : function (colNum) {
    if (colNum == null) colNum = 0;
    if (!this.columns[colNum]) return;
    this.columns[colNum].selectAllRecords();
},

//> @method columnTree.deselectAllRecords()
// Deselect all records in the supplied column (the first column if none is passed)
//
// @param [colNum] (number) Column number 
// @group selection
// @visibility external
//<
deselectAllRecords : function (colNum) {
    if (colNum == null) colNum = 0;
    if (!this.columns[colNum]) return;
    this.columns[colNum].deselectAllRecords();
},

//> @method columnTree.anySelected()
// Whether at least one item is selected in the supplied column (the first column if none is passed)
//
// @param [colNum] (number) Column number 
// @return (Boolean) true == at least one item is selected in the supplied column, 
// false == nothing at all is selected in the supplied column (note that there may be selections
// in other columns in this columnTree)
// @group selection
// @visibility external
//<
anySelected : function (colNum) {
    if (colNum == null) colNum = 0;
    if (!this.columns[colNum]) return false;
    return this.columns[colNum].anySelected();
},

//> @method columnTree.getSelection()
//
// Returns the array of objects selected in the specified column of the columnTree
//
// @param colNum (number) Column within the columnTree to return the selection for
// @return (Array of ListGridRecord) list of records, empty list if nothing selected
//
// @group selection
// @visibility internal
//<
getSelection : function (colNum) {
    if (colNum == null) colNum = 0;
    if (!this.columns[colNum]) return [];
    return this.columns[colNum].getSelection();
},

//> @method columnTree.getSelectionObject()
//
// Returns the +link{Selection} object associated with the specified column of the columnTree
//
// @param colNum (number) Column within the columnTree to return the selection for
// @return (Selection) The specified column's underlying +link{Selection} object
//
// @group selection
// @visibility internal
//<
getSelectionObject : function (colNum) {
    if (colNum == null) colNum = 0;
    if (!this.columns[colNum]) return null;
    return this.columns[colNum].selection;
}
    
});
    
    
isc.ColumnTree.registerStringMethods({
    // itemSelected - handler fired when the user changes the selection.
    nodeSelected : "column, node",
    
    //> @method ColumnTree.onNodeSelected()
    // Notification method fired when a node is selected. Return false to suppress default
    // behavior.
    // @param column (ListGrid) The column (ListGrid instance) in which the node was selected
    // @param node (TreeNode) The selected node
    // @return (boolean) Return false to cancel default behavior
    // @visibility sgwt
    //<
    
    onNodeSelected : "column,node"
})
