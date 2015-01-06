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




//>	@class TileGrid
// A TileGrid is a +link{DataBoundComponent} that displays a list of objects as a set
// of "tiles", where each tile represents one object, and the tiles are laid out in a grid with
// multiple tiles per row.  Each tile displays one or more properties of the object it
// represents.
//
// @implements DataBoundComponent
// @treeLocation Client Reference/Grids
// @visibility external
//<
isc.ClassFactory.defineClass("TileGrid", "TileLayout", "DataBoundComponent");

isc.TileGrid.addProperties({

//> @attr tileGrid.fields (Array of DetailViewerField : null : IR)
// Array of field definitions to control the default rendering of tiles.
// <P>
// If not specified, if the DataSource has an +link{dataSource.iconField,iconField}, only the
// <code>iconField</code> and +link{dataSource.titleField,titleField} will be shown.
// Otherwise, all non-+link{dataSourceField.hidden,hidden}
// non-+link{dataSourceField.detail,detail} fields will be shown, similar to the default set of
// fields shown by a +link{ListGrid}.
// <P>
// Only applicable if using the default +link{SimpleTile} class for tiles.
// <P>
// For SimpleTiles, it is possible to use +link{DetailViewerField.getCellStyle()} and 
// +link{StatefulCanvas.getStateSuffix()} to make a single field statefully styled:
// <pre>
// <smartclient>
// isc.TileGrid.create({
//      fields:[
//          {name:'animalName',
//           getCellStyle : function (value, field, record, viewer) {
//               if (value == "Tiger") return "tigerStyle" + viewer.currentTile.getStateSuffix();
//               else return viewer.tileGrid.tileValueStyle + viewer.currentTile.getStateSuffix();
//           }
//          }
//      ]
// });
// </smartclient>
// <smartgwt>
// final TileGrid tileGrid = new TileGrid();
// DetailViewerField animalNameField = new DetailViewerField("countryName");  
// animalNameField.setCellStyleHandler(new CellStyleHandler() {  
//     public String execute(Object value, DetailViewerField field, Record record) {
//         SimpleTile tile = tileGrid.getCurrentTile();
//         if (value == "Tiger") return "tigerStyle" + tile.getStateSuffix();
//         else return "nonTigerStyle" + tile.getStateSuffix();
//     }  
// });  
// tileGrid.setFields(animalNameField);  
// </smartgwt>
// </pre>
//
// @visibility external
//<

//>	@attr tileGrid.showDetailFields (Boolean : false : IR)
// By default, TileGrids will not show fields marked +link{dataSourceField.detail,detail:true}
// in the DataSource.  See also +link{tileGrid.fields}.
// @visibility external
//<

//> @attr tileGrid.tileValueStyle (CSSClassName : "tileValue" : IR)
// When using the default +link{SimpleTile}, CSS style for each value shown within a tile.
// @visibility external
//<
tileValueStyle:"tileValue",

// @attr TileGrid.valuesShowRollOver (boolean : false : IR)
// Should tile values change state when the mouse goes over them?
// @visibility external
valuesShowRollOver: false,

// @attr TileGrid.valuesShowSelected (boolean : true : IR)
// Should tile values change state when they are selected?
// @visibility external
valuesShowSelected: true,

// @attr TileGrid.valuesShowDown (boolean : false : IR)
// Should tile values change state when the mouse goes down on them?
// @visibilty external
valuesShowDown: false,

//> @attr tileGrid.tileValueAlign   (String : "center" : IR)
// Horizontal alignment for tile values: "left", "right" or "center".
// @visibility external
//<
tileValueAlign:"center",

// ability to show labels dubious and not doc'd yet - better approach probably to support
// optionally showing a label on a per-line basis, or suggest using formatters
showLabels:false,
tileLabelStyle:"tileLabel",

//> @attr tileGrid.wrapValues (Boolean : false : IR)
// Whether values should be allowed to wrap by default, or should be shown on one line
// regardless of length.
// @visibility external
//<
wrapValues: false,

// allows sorting via panelHeader by default
canSortFields: true,

//>	@attr tileGrid.data (Array[] of Record | Array[] of TileRecord | RecordList : null : IRW)
// A List of TileRecord objects, specifying the data to be used to create the
// tiles.  
// <p>
// This property will typically not be explicitly specified for databound TileGrids, where
// the data is returned from the server via databound component methods such as
// +link{fetchData()}. In this case the data objects will be set to a 
// +link{class:ResultSet,resultSet} rather than a simple array.
//
// @group	data
// @see TileRecord
// @setter setData()
// @visibility external
//<

//> @attr tileGrid.printTilesPerLine (number : null : IR)
// How many tiles should be present in a line when printing?
//
// @visibility external
//<

// ---------------------------inherited from dataBoundComponent--------------------------------
//>	@attr TileGrid.dataSource		(DataSource or ID : null : IRW)
// @include dataBoundComponent.dataSource
//<

//> @method TileGrid.fetchData()
// @include dataBoundComponent.fetchData()
// @group dataBoundComponentMethods
// @visibility external
// @example databoundFetch
//<

//> @method TileGrid.filterData()
// @include dataBoundComponent.filterData()
// @group dataBoundComponentMethods
// @visibility external
//<

//>	@attr TileGrid.autoFetchData       (boolean : false : IR)
// @include dataBoundComponent.autoFetchData
// @group databinding
// @visibility external
// @example fetchOperation
//<

//> @method TileGrid.invalidateCache()
// @include dataBoundComponent.invalidateCache()
// @group dataBoundComponentMethods
// @visibility external
//<

//> @method TileGrid.removeSelectedData()
// @include dataBoundComponent.removeSelectedData()
// @group dataBoundComponentMethods
// @visibility external
// @example removeOperation
//<

//> @method TileGrid.getSelection()
// @include dataBoundComponent.getSelection()
//
// @group  selection
// @visibility external
//<

//> @method TileGrid.getSelectedRecord()
// @include dataBoundComponent.getSelectedRecord()
//
// @group  selection
// @return (TileRecord) first selected record, or null if nothing selected
// @visibility external
//<

//> @method TileGrid.selectRecord()
// @include dataBoundComponent.selectRecord()
//<

//> @method TileGrid.deselectRecord()
// @include dataBoundComponent.deselectRecord()
//<

//> @method TileGrid.selectRecords()
// @include dataBoundComponent.selectRecords()
//<

//> @method TileGrid.selectRange()
// @include dataBoundComponent.selectRange()
//<

//> @method TileGrid.deselectRecords()
// @include dataBoundComponent.deselectRecords()
//<

//> @method TileGrid.deselectRange()
// @include dataBoundComponent.deselectRange()
//<

//> @method TileGrid.selectAllRecords()
// @include dataBoundComponent.selectAllRecords()
//<

//> @method TileGrid.deselectAllRecords()
// @include dataBoundComponent.deselectAllRecords()
//<

//> @method TileGrid.anySelected()
// @include dataBoundComponent.anySelected()
//<

//>	@attr TileGrid.autoFetchTextMatchStyle       (TextMatchStyle : "substring" : IR)
// @include dataBoundComponent.autoFetchTextMatchStyle
// @group databinding
// @visibility external
//<
autoFetchTextMatchStyle:"substring",

//> @attr TileGrid.initialCriteria   (Criteria : null :IR)
// @include dataBoundComponent.initialCriteria
// @visibility external
//<

//> @attr tileGrid.dataFetchMode (FetchMode : "paged" : IRW)
// @include dataBoundComponent.dataFetchMode
//<

//>	@attr tileGrid.selectionType		(SelectionStyle : isc.Selection.MULTIPLE : [IRW])
// Defines a tileGrid's clickable-selection behavior.
//
// @group	selection, appearance
// @see type:SelectionStyle
// @visibility external
// @example tilingEditing
//<
selectionType: isc.Selection.MULTIPLE,    

//autoChildren 

//> @attr tileGrid.tile (MultiAutoChild SimpleTile : null : IR)
// A TileGrid automatically creates one tile per record in the dataset, via the
// +link{AutoChild} pattern.
// <P>
// By default, the +link{SimpleTile} class will be used, and will contain content as rendered
// by a +link{DetailViewer}, based on the provided +link{TileGrid.fields} (or on the default
// set of fields).
// <P>
// <smartclient>
// To create a completely different appearance, override +link{tileConstructor} with the name
// of the custom SmartClient class to use for each tile.  For example, subclass
// +link{SimpleTile} and override +link{Canvas.getInnerHTML,getInnerHTML()}, returning custom
// HTML for each tile.
// <pre>
//     isc.defineClass("MyCustomTile", "SimpleTile").addProperties({
//        getInnerHTML : function () {
//           return this.Super("getInnerHTML", arguments) +
//                this.getRecord().width + " x " + this.getRecord().height;
//        }
//     });
//
//     isc.TileGrid.create({
//        tileConstructor:"MyCustomTile"
//     });
// </pre>
// <P>
// Note that you can also override tile behaviors on a per-record basis, via
// +link{tileRecord.tileConstructor} and +link{tileRecord.tileProperties}.
// </smartclient>
// <smartgwt>
// To create a completely different appearance set +link{tileConstructor} to custom Canvas
// class name. You can use SmartGWT classes too with help of Reflection. For example, you can
// extend a DynamicForm with any layout:
// <pre>
//   public class CarTile extends DynamicForm {
//     public CarTile() {
//       StaticTextItem name = new StaticTextItem("name");
//       name.setShowTitle(false);
//       StaticTextItem price = new StaticTextItem("price");
//       price.setShowTitle(false);
//       this.setFields(pictureItem, name, price);
//     }
//   }
// </pre>
// Register the class factory and use it by passing it's class to tileConstructor
// <pre>
//   public interface CarTileMetaFactory extends BeanFactory.MetaFactory {
//     BeanFactory<CarTile> getCarTileFactory();
//   }
//   public void onModuleLoad() {
//     GWT.create(CarTileMetaFactory.class);
//     TileGrid tileGrid = new TileGrid();
//     tileGrid.setTileConstructor(CarTile.class.getName());
//     tileGrid.draw();
//   }
// </pre>
// </smartgwt>
//
// @visibility external
//<

//> @attr tileGrid.tileProperties (Canvas Properties : null : IRW)
// Common properties to use when creating every tile.
// @visibility external
//<

//> @attr tileGrid.tileConstructor (SCClassName : "SimpleTile" : [IRW])
// Classname of a SmartClient component to use for each tile rendered by this TileGrid.  Tiles
// are created by the +link{AutoChild} pattern; see +link{tileGrid.tile}.
// <P> 
// Any subclass of Canvas is allowed, but typically any custom class will derive from
// +link{SimpleTile}.
// <p>
// When using a custom component for tileConstructor, DataBoundComponents that display multiple
// Records (ListGrid, DetailViewer) will have data provided via +link{listGrid.setData()}, and
// components that display a single Record (DynamicForm) will have
// +link{dynamicForm.setValues()} called on them.
// <p>
// If the component is not a recognized DataBoundComponent subclass, the Record can be
// accessed via 
//  <smartclient><code>this.record</code>.</smartclient>
//  <smartgwt><code>this.getAttributeAsRecord("record").</code></smartgwt>
// <p>
// If you implement particularly simple or particularly complex tile interfaces, you may wish
// to adjust the property +link{tileGrid.drawAllMaxTiles}.
// @example fullyCustomTiles
// @visibility external
//<
tileConstructor: "SimpleTile",

//> @attr tileGrid.detailViewer (AutoChild DetailViewer : null : IR)
// DetailViewer instance used to render the content shown in Tiles by default.
// @visibility external
//<
detailViewerDefaults: {
    _constructor: "DetailViewer",
    visibility: "hidden"
},

//> @attr tileGrid.detailViewerProperties (DetailViewer Properties : null : IR)
// Properties for the +link{DetailViewer} that is automatically created to render the contents of tiles
// by default.
// @visibility external
//<

//> @attr tileGrid.recycleTiles (Boolean : true : IR)
// This property determines whether tiles that are no longer visible (due to scrolling) are
// recycled, allowing a large number of records to be displayed using a (potentially) much
// smaller set of tiles.
// <P>
// Recyling tiles may significantly reduce the number of live tile widgets needed to support
// a particular TileGrid, but may also result in extra work when the TileGrid is scrolled, as
// a scroll that brings off-screen tiles into view will require recycling tiles that have
// left the view, even if the new tiles have been visited before (in previous scrolling).
// <P>
// Recycling will occur when +link{tileGrid.getTile()} is called, unless the supplied record
// (or record specifed by index) is currently bound to an existing tile.  Even if recycling
// is not enabled, the record associated with a given tile may change if the TileGrid data
// changes.
// <P>
// For more control over the tile creation and recycling process, 
// see +link{tileGrid.createTile()} and +link{tileGrid.updateTile()}.
// 
// @visibility external
//<
recycleTiles: true,

//> @method tileGrid.createTile()
// If defined, this method will be called when a new tile is required.  Note that
// this method is in complete control of how the tile is constructed, so that
// properties such as +link{tileGrid.tileProperties} and others needed by TileGrid
// will be applied only after this method returns.
//
// @param record (Record) record that will be associated with new tile
// @param tileIndex (Integer) index of the record in the tileGrid
// @return (Canvas) return the new tile that will hold the record (cannot be null)
// @see tileGrid.recycleTiles
// @see tileGrid.tileProperties
// @visibility external
//<

//> @method tileGrid.updateTile()
// If both this method and +link{tileGrid.createTile()} are defined and
// +link{tileGrid.recycleTiles} is true, this method will be called when the
// framework needs to recycle a tile to be used with a new record.  This
// notification provides an opportunity to update any widget properties that
// depend on the specifics of the record.
//
// @param record (Record) record that will be associated with the recycled tile
// @param tileIndex (Integer) index of the record in the tileGrid
// @param reclaimedTile (Canvas) the tile to be recycled
// @see tileGrid.recycleTiles
// @visibility external
//<

//> @attr tileGrid.showAllRecords (Boolean : false : IR)
// Whether tiles are created and drawn for all records, or only for those currently visible.
//
// @group basics
// @visibility external
//<

//> @attr tileGrid.drawAllMaxTiles (integer : 25 : IRWA)
// If drawing all tiles would cause no more than <code>drawAllMaxTiles</code> tiles to be
// rendered, the full dataset will instead be drawn even if +link{tileGrid.showAllRecords}
// is false and incremental rendering would have otherwise been used.
// <P>
// The <code>drawAllMaxTiles</code> setting prevents incremental rendering from being used in
// situations where it's really unnecessary, such as a 25 record dataset which happens to be
// in a grid with a viewport showing only 15 or so tiles.  Incremental rendering causes a brief
// "flash" during scrolling as the visible portion of the dataset is redrawn, and a better 
// scrolling experience can be obtained in this situation by drawing the entire dataset up
// front, which in this example would have negligible effect on initial draw time.
// <P>
// <code>drawAllMaxTiles:0</code> disables this features.  You may want to disable this feature
// if performance is an issue and:
// <ul>
// <li> you very frequently redraw a grid
// <li> you do a lot of computation when rendering each tile
// <li> you are showing many grids on one screen and the user won't scroll most of them
// </ul>
//
// @see tileContructor
// @group performance
// @visibility external
//<
drawAllMaxTiles: 25,

//> @attr tileGrid.animateTileChange (Boolean : true : IRWA) 
// If set, when the dataset changes due to filtering, sorting or other actions, any tiles that
// were showing before and after the change will animate from their old positions to their new
// positions.
//
// @group appearance
// @visibility external
//<
animateTileChange: true,

//> @attr tileGrid.styleName (CSSStyleName : "tileGrid" : IR)
// Style for the overall TileGrid component.
// @group appearance
// @visibility external
//<
styleName:"tileGrid",

// set this flag to false so that databound tiles can't be hidden by an explicit call to 
// hide()
_enableUserHiding: false,

//> @object TileRecord
// A TileRecord is a JavaScript Object whose properties contain values for each
// TileField. A TileRecord may have additional properties which affect the record's
// appearance or behavior, or which hold data for use by custom logic or other, related
// components. 
//
// @treeLocation Client Reference/Grids/TileGrid
// @visibility external
//<

//> @attr tileRecord.tileConstructor (String : null : IRW)
// SmartClient Class to use to construct the tile for this particular record.
//
// @visibility external
//<

//> @attr tileRecord.tileProperties (Canvas Properties : null : IRW)
// Additional properties to be passed when creating a tile for this record.
//
// @visibility external
//<

init : function () {
    if (this.disableTouchScrollingForDrag &&
        (this.canDragTilesOut || this.canReorderTiles) &&
        this.useTouchScrolling == null)
    {
        this.logInfo("Automatically disabling touch scrolling", "scrolling");
        this.useTouchScrolling = false;
    }
    this.Super("init", arguments);
},

initWidget : function () {
    this._enforceLegalLayoutPolicy();

    // disable summary and formula fields if the required components aren't present
    if (isc.FormulaBuilder == null) this.canAddFormulaFields = false;
    if (isc.SummaryBuilder == null) this.canAddSummaryFields = false;

    if (this.layoutPolicy == "flow") {
        isc.logWarn("TileGrid does not support layoutPolicy 'flow'; there may be unexpected behavior. " + 
                    "Use a TileLayout instead for flow layout.");    
    }

    this._setUpDragProperties();
    // skip tileLayout init; we want to completely replace that here
    this.invokeSuper(isc.TileLayout, "initWidget");
    if (!this.tiles) this.tiles = [];
    // make sure we don't try to recycle tiles when we're showing all records
    if (this.showAllRecords) this.recycleTiles = false;

    // set up tile map for record <-> tile mapping when databound
    var ds = this.getDataSource();
    if (isc.isA.DataSource(ds)) {
        this._tileMap = {};
        if (ds.getPrimaryKeyFieldNames().length == 0) {
            // don't animate tiles if there is no primary key because there's no way of knowing
            // that the tiles will be the same
            this.animateTileChange = false;
        }
    }

    //TODO: don't create this if it won't be used (if we're not using SimpleTile)
    // internal detailViewer for creating SimpleTiles
    this.detailViewer = this.createAutoChild("detailViewer", {
            tileGrid: this,
            showLabel: this.showLabels,
            showBorder: false, 
            cellStyle: this.tileValueStyle,
            labelStyle: this.tileLabelStyle,
            blockStyle: "normal",
            wrapValues: this.wrapValues,
            cellPadding: 0,
            valueAlign: this.tileValueAlign,
            // to force detailViewer table width to be 100%
            // NOTE 6/29/09 this needs to be false for the tiles to have their
            // content centered properly. Seems to not break the strict example.
            useInnerWidth: false,
            clipValues: true,
            // width and height should be set in makeTile
            width: 10,
            height: 10,
            data: [],
            dataSource: ds,
            getCellStyle: function (value, field, record, viewer) {
                var base = (field.cellStyle || this.cellStyle);
                if (this.tileGrid.valuesShowRollOver && this.currentTile.state == isc.StatefulCanvas.STATE_OVER) {
                    base += this.currentTile.getStateSuffix();    
                } else if (this.tileGrid.valuesShowDown && this.currentTile.state == isc.StatefulCanvas.STATE_DOWN) {
                    base += this.currentTile.getStateSuffix();    
                } else if (this.tileGrid.valuesShowSelected && this.currentTile.isSelected()) {
                    base += this.currentTile.getStateSuffix();    
                }
                return base;
            }
    }, this.detailViewerProperties);

    this._tileIndexToTileMap = [];

    // set field state if necessary, call setFields otherwise
    if (this.fieldState != null) this.setFieldState(this.fieldState);
    else this.setFields(this.fields, true);

    this.membersMargin = this.tileMargin;

    this.setData(this.data);
},

setDataSource : function (dataSource, fields) {
    this.Super("setDataSource", arguments);    
    // set up tile map for record <-> tile mapping when databound
    var ds = this.getDataSource();
    if (isc.isA.DataSource(ds)) {
        this._tileMap = {};
        if (ds.getPrimaryKeyFieldNames().length == 0) {
            // don't animate tiles if there is no primary key because there's no way of knowing
            // that the tiles will be the same
            this.animateTileChange = false;
        }
    }
},


// return whether this component wants to use the field when binding to a DataSource.  
// TileGrid-specific override of the DBC method to account for the fact that a TileGrid
// always wants the icon field if there is one
shouldUseField : function (field, ds) { 

    if (this.Super("shouldUseField", arguments)) return true;

    ds = isc.DS.get(ds);
    if (isc.isA.DataSource(ds)) {
        var iconField = ds.getIconField();
        if (field == iconField || field.name == iconField || 
            (iconField && field.name == iconField.name)) 
        {
            return true;
        }
    }

    return false;
},


setFields : function (newFields, cancelLayout) {
    var ds = this.getDataSource();
    if (!newFields && isc.isA.DataSource(ds)) {
        // if the DataSource has an icon field, show just the icon and the title
        var iconField = ds.getIconField();
        if (iconField) {
            newFields = [];
            newFields.add({name:iconField, type: iconField.type});
            newFields.add({name:ds.getTitleField()});
        }
    }

    if (this.completeFields == null) this.fields = [];

	// bind the passed-in fields to the DataSource and store
    this.completeFields = this.bindToDataSource(newFields);

    if (this.completeFields == null) this.completeFields = [];
    // tilegrid was crashing without this line:
    if (!this.completeFields) return;

	this.deriveVisibleFields();
    this.invalidateUserCache();

    this.detailViewer.fields = this.completeFields.duplicate();
    if (!cancelLayout) {
        this.logDebug('calling layoutTiles from setFields', "TileGrid");
        this.layoutTiles();
    }
},

deriveVisibleFields : function () {
	// NOTE: we use setArray() so that this.fields remains the same array instance.
    this.fields.setArray(this.getVisibleFields(this.completeFields));
},

getVisibleFields : function (fields) {
	var viewer = this.detailViewer,
        valueList = this.getData(),
        returnFields = fields.duplicate();
    for (var i=0; i<fields.length; i++) {
        var field = fields.get(i);
        if (!viewer.fieldShouldBeVisible(field, valueList) || 
            field.visible==false) returnFields.remove(field);
    }
	return returnFields;
},

computeTileDimensions : function (forceCompute) {
    // don't compute tile dimensions if they're already known
    if (((this.tileHeight && this.tileWidth) || (this.tileSize)) && !forceCompute) return;
    // don't compute if we don't have all the data on hand.
    if (!((isc.ResultSet && isc.isA.ResultSet(this.data) 
            && this.data.resultSize >= this.data.getLength())
         || isc.isAn.Array(this.data))) {
         return;
    }
    // only get dimensions when layoutPolicy is 'fit' and we have a tile array
    if (this.layoutPolicy != "fit") return;
    // iterate through tiles collection and find the greatest width and height
    var maxHeight = 0, maxWidth = 0;
    // very important; we don't want clipping
    this.detailViewer.clipValues = false;
    for (var i=0; i < this.data.getLength(); i++) {
        // render the tile so its sized by its own content
        var t = this.getTile(i);
        var currOverflow = t.overflow;
        t.setOverflow("visible");
        t.redraw();
        t.show();
        var tHeight = t.getVisibleHeight();
        var tWidth = t.getVisibleWidth();
        if (tHeight > maxHeight) maxHeight = tHeight;
        if (tWidth > maxWidth) maxWidth = tWidth;
        // reset the tile to its prior state
        t.setOverflow(currOverflow);
        t.hide();
    }
    // set the detailViewer back to its default state
    this.detailViewer.clipValues = true;
    if (!this.tileHeight && maxHeight > 0) this.tileHeight = maxHeight;
    if (!this.tileWidth && maxWidth > 0) this.tileWidth = maxWidth;
    
},

// get/setTileID ensure that tile-to-record mapping remains stable when databound. 
// The expando approach doens't work when databound because the expando gets wiped out
// on update.
getTileID : function (record) {
    if (!record) return null;
    var ds = this.getDataSource();
    if (isc.isA.DataSource(ds) && ds.getPrimaryKeyFieldNames().length > 0) {
        var pks = ds.getPrimaryKeyFields();
        var pk = "";
        for (var pkName in pks) {
            pk += record[pkName];        
        }
        return this._tileMap[pk];
    } else {
        return record._tileID;    
    }
},

setTileID : function (record, tileID) {
    var ds = this.getDataSource();
    if (isc.isA.DataSource(ds) && ds.getPrimaryKeyFieldNames().length > 0) {
        var pks = ds.getPrimaryKeyFields();
        var pk = "";
        for (var pkName in pks) {
            pk += record[pkName];        
        }
        this._tileMap[pk] = tileID;
    } else {
        record._tileID = tileID;    
    }
},

//> @method TileGrid.getTileRecord()
// Given a tile within this this tile-grid, this method will return the associated record.
//
// @param tile (Canvas) Tile you want to get the record for
// @return (TileRecord) Record associated with the specified tile
// @visibility external
//<

getTileRecord : function (tile) {
    return tile.record;
},

checkTileRecord : function (tile) {
    // new dataset load in progress, don't try to access data or fetches will be triggered
    if (isc.isA.ResultSet(this.data) && !this.data.lengthIsKnown()) return false;

    var dataIndex = tile.tileNum;
    if (dataIndex == null) return false;

    var tileRecord = tile.record,
        dbcRecord = this.data.get(dataIndex);

    // is tile record consistent with the DBC's record
    return this._tileRecordsEqual(tileRecord, dbcRecord);
},

setData : function (newData) {
    
    // if we're animating prevent new data from arriving. See _layoutAfterDataChange()
    if (this._animating) {
        return false;    
    }
    
    if (!newData) return;

    if (this.data) {
        this.ignore(this.data, "dataChanged"); 
        this.ignore(this.data, "dataArrived");
    }
    // if newData was passed in, remember it
	if (newData) this.data = newData;
    
	// if data is not set, bail
	if (!this.data) return;
    
    //isc.logEchoAll(this.data);
    if (this.data) {
        if (isc.ResultSet && isc.isA.ResultSet(this.data)) {
            this.observe(this.data, "dataArrived",
                            "observer.dataArrived(arguments[0],arguments[1])");
            this.observe(this.data, "dataChanged", 
                 "observer.dataChanged(operationType, originalRecord, rowNum, updateData)");
        } else {
            // dataChanged has no params for an array
            this.observe(this.data, "dataChanged", "observer.dataChanged()");
        }
    }
    
    // create a new selection if we don't have one or if we receive new data
	if (!this.selection || (this.data != this.selection.data)) {
        
        this.createSelectionModel();
	}
    // fire dataChanged here. If the resultset is empty, dataChanged won't do anything
    this.dataChanged();
},

getData : function () {
    return this.data;    
},

// getPrimaryKeys() - Returns unique primary keys for a record.
// Use 'comparePrimaryKeys()' to compare against some record.

getPrimaryKeys : function (record) {
    var data = this.data;
    if (!isc.ResultSet || !isc.isA.ResultSet(data)) return record;

    var ds = this.getDataSource(),
        keys = {};
    if (isc.isA.DataSource(ds)) {
        var pkArray = ds.getPrimaryKeyFieldNames();
        

        for (var i = 0; i < pkArray.length; i++) {
            keys[pkArray[i]] = record[pkArray[i]]
        }
    }

    return keys;
},


// setRecordValues()
// Method to update client-side data in place
// This is called directly by DynamicForms when saving valuess if this is acting as the selection
// component for a form.
setRecordValues : function (pks, values) {
   
    if (!this.data) return;
    
    var rowNum = this.data.indexOf(pks);
    if (rowNum == -1) return;
    var record = this.data.get(rowNum);
    isc.combineObjects(record, values);

    this.invalidateUserCache(record);
    
    // if we have a valuesManager, explicitly notify it about the change
    if (this.valuesManager != null) {
        this.valuesManager._updateMultipleMemberValue(rowNum, null, record, this);
    }
    
    // refresh display.
    this.logDebug('calling layoutTiles from setRecordValues', "TileGrid");
    this.layoutTiles();
},



// no-op function to be overridden
dataArrived : function (startRecord, endRecord) {
},

dataChanged : function (operationType, originalRecord, rowNum, updateData) {
    this.invalidateUserCache();

    if (!this.data || 
        (isc.ResultSet && isc.isA.ResultSet(this.data) && !this.data.lengthIsKnown())) 
    {
        this.logDebug("dataChanged: returning due to no data yet", "TileGrid");
        return;
    }
    // compute tile dimensions here. We need data to have arrived to do this.
    this.computeTileDimensions();
    // track data to help determine if deleting tiles is needed
    if (!this._oldDataLength) this._oldDataLength = 0;

    this.detailViewer.setHilites(this.hilites);

    // the following terms will be used in the subsequent comments:
    // recycle = when recycleTiles is true and tiles are reused
    // incremental = when incremental rendering is occuring, and tiles are drawn on demand, and
    // remain even when scrolled offscreen
    // showall = see tileGrid.showAllRecords
   
    // Add
    // recycle - layoutTiles() should suffice to pickup any changes resulting from the new
    //      tile in the flow.
    // incremental - layoutTiles() will refresh the visible tiles, and subsequent scrolls should
    //      refresh tiles as well.
    // showAll - since a new tile is added, we may need to reflow a significant number of tiles,
    //      so layoutTiles() should be called, even though its expensive in this case
    if (operationType == "add") {
        this.logDebug("add", "TileGrid");
        this.layoutTiles();
    // Remove
    // recycle - delete the tile if it exists i.e. if it were a visible tile, then layoutTiles()      
    // incremental - delete the tile if its been created, then layoutTiles()
    // showall - delete the tile if its been created, then layoutTiles()
    } else if (operationType == "remove") {
        this.logDebug("remove", "TileGrid");
        if (!this.recycleTiles || 
            this.data.getLength() > this.getDrawnEndIndex() - this.getDrawnStartIndex())
        {
            if (originalRecord) this.setTileID(originalRecord, null);
            // after removing last tile will not be used, but removed tile will be reused
            // and all tiles will be shifted
            var oldTile = this.tiles[this.tiles.length - 1];
            this._updateTileIndex(oldTile.tileNum);
            this._removeTileFromReclaim(oldTile);
            this.tiles.remove(oldTile);
            oldTile.destroy();
        }

        this.layoutTiles();
    // Update
    // recycle, incremental, showall - tried to be smart about this by only redrawing the updated
    // tile, but we lose tile & selection pointers on update, so its better just to layoutTiles()
    } else if (operationType == "update") {
        this.logDebug("update", "TileGrid");
        this.layoutTiles();
    // Filter, sort, etc., and the new data is as long or longer than the old data
    // recycle, incremental, showall - just layoutTiles() should be neccessary, as calling getTile()
    // will create new tiles as needed, and scrolling should take care of updating out-of-sync offscreen
    // tiles.
    } else if (this.data.getLength() >= this._oldDataLength) {
        this.logDebug("filter or sort, new data same or longer", "TileGrid");
        
        // only trigger animations if we had data before
        if (this._oldDataLength > 0) this._layoutAfterDataChange();
        else this.layoutTiles();
        
    // Filter, sort, etc., and the new data is shorter than the old data
    // recycle - only delete tiles if new data length < visibleTiles.length, then layoutTiles
    // incremental, showall - delete extra tiles, then layoutTiles
    } else {
        this.logDebug("filter or sort, new data shorter", "TileGrid");
        this.selection.deselectAll();
        this.fireSelectionUpdated();
        // doesn't seem like this is necessary, as we call cleanUpExtraTiles() at the end
        // of tileLayout(), but we'll leave this here for now just in case...
        /*
        if (this.recycleTiles && !this.showAllRecords) {
            var start = this.data.getLength();
            if (start < this.getDrawnEndIndex() - this.getDrawnStartIndex()) {
                this.cleanupExtraTiles(start);
            }
        } else {
            var start = this.data.getLength();
            this.cleanupExtraTiles(start);
        }
        */

        
        // here we bank on the fact that getDrawnEndIndex returns a cached value from the
        // previous data, since layoutTiles() hasn't been called yet.
        var prevLastDrawnNum = this.getDrawnEndIndex();
        var newLastDrawnNum = prevLastDrawnNum > this.data.getLength() ? this.data.getLength() :
                              prevLastDrawnNum;
        var tpl = this.getTilesPerLine();
        // at first glance it seems like if data.length is less than prevLastDrawnNum, scrolling 
        // will always be invalid; however there is the case when data length is smaller, but 
        // still produces the same number of lines, in which case the scrollTop is still valid.
        // Also scrollTop == 0 is a special case...always valid
        if (Math.floor(prevLastDrawnNum / tpl) > Math.floor(newLastDrawnNum / tpl)
            && this.getScrollTop() != 0 && this.recycleTiles) {
            this.scrollToTop();
            this.layoutTiles();
        } else {
            this._layoutAfterDataChange();    
        }
       
    }
    // set oldData length now, as next time this method is called, this.data may be new data
    this._oldDataLength = this.data.getLength();
    //isc.logWarn('dataChanged:' + [operationType, originalRecord, rowNum]);
    // need to clean up potential extra tiles when data length is zero (i.e. after a filter that 
    // returns no results), because layoutTiles won't actually run in that case. 
    if (this.data.getLength() == 0) {
        this.cleanupExtraTiles(true);    
    }

},

// helper function to handle tile animations, only called when data operation is not singular
// (not add, update, or remove)
_layoutAfterDataChange : function () {
    if (this.destroying) return;
    if (this.animateTileChange) {
        
        if (this._animating) {
            var arr = this._animationIDs;
            for (var i = 0; i < arr.length; i++) {
                this.finishAnimation(arr[i].ID);
                arr[i].tile.hide();
            }
            // for now just return - seems to work fine, but if problems arise look into
            // setting a flag that will trigger _layoutAfterDataChange again from _finishAnimating
            return;
        }
        this.fireOnPause("tileGridAnimate", this._animateChange);    
    } else {
        this.logDebug('calling layoutTiles from layoutAfterDataChange', "TileGrid");
        this.layoutTiles();    
    }
},

cleanupExtraTiles : function (hideAll) {
    
    var tileArray = this.tiles;
    for (var i = 0; i < tileArray.length; i++) {
        var tile = tileArray[i];
        if (!tile._processed || hideAll) {
            tile.hide();
            // absolutely essential: without this, the tileGrid thinks its scrollable area is
            // larger than it really should be.
            tile.moveTo(0, 0);
        }
    }
},

destroy : function () {
    if (this.data){
        this.ignore(this.data, "dataChanged"); 
        this.ignore(this.data, "dataArrived");
        // if the data was autoCreated, destroy it to clean up RS<->DS links
        if (this.data._autoCreated && isc.isA.Function(this.data.destroy))
            this.data.destroy();
    }
    
    this.Super("destroy", arguments);
},

_getTileID : function (tileNum) {
    
    if (this.createTile != null && this.tiles && this.tiles.length > tileNum) {
        return this.tiles[tileNum].ID;
    }
    return this.ID + "_tile_" + tileNum;        
},

getLength : function () {
    if (!this.data  
        || (isc.ResultSet && isc.isA.ResultSet(this.data) && !this.data.lengthIsKnown())) return 0;
    else return this.data.getLength();
},

makeTile : function (record, tileNum) {
    var grid = this;

    // properties to be applied to each tile
    var props = {
        canHover: true,

        
        tileGrid: grid,

        //canDragReposition: true,
        handleHover : function () {
            if (grid.itemHover) grid.fireCallback("itemHover", "item", [this]);
            return this.Super("handleHover", arguments);
        },
        
        handleMouseDown : function () {
            grid._tileMouseDown(this);
            grid.focus();
            return this.Super("handleMouseDown", arguments);
        },
        handleRightMouseDown : function () {
            var returnVal = grid._tileRightMouseDown(this);
            if (returnVal == false) return false;
            grid.focus();
            return this.Super("handleRightMouseDown", arguments);
            return returnVal;
        },
        handleMouseUp : function () {
            grid._tileMouseUp(this);    
            return this.Super("handleMouseUp", arguments);
        },
        handleClick : function () {
            grid._tileClick(this);
            return this.Super("handleClick", arguments);
            
        },
        handleShowContextMenu : function () {
            var returnVal = grid._tileContextClick(this);
            if (returnVal != false) {
                returnVal = this.Super("handleShowContextMenu", arguments);
            }
            return returnVal;

        },
        handleDoubleClick : function () {
            var tileRecord = grid.getTileRecord(this);
            var returnVal = grid.recordDoubleClick(grid, this, tileRecord);
            if (returnVal != false) {
                returnVal = this.Super("handleDoubleClick", arguments);
            }
            return returnVal;
        }
    };

    if (record.tileProperties) isc.addProperties(props, record.tileProperties);

    
    var newTile;    
    if (this.createTile != null) {
        newTile = this.createTile(record, tileNum);
        newTile.addProperties(props, this.tileDefaults, this.tileProperties);
    } else {
        props.ID = this._getTileID(this.tiles.length);
        var theConstructor = record.tileConstructor ? record.tileConstructor : 
                                                        this.tileConstructor;
        // store new tile in a local var for debug purposes
        newTile = this.createAutoChild("tile", props, theConstructor);
    }

    //newTile.setWidth(this.getTileWidth());
    //newTile.setHeight(this.getTileHeight());
    // HACK this is neccessary to avoid the tile being sized by its content. Otherwise, 
    // the tile will grow past its set width when css borders are used. 
    this.detailViewer.setWidth(newTile.getInnerWidth());
    this.detailViewer.setHeight(newTile.getInnerHeight());

    return newTile;
},

setHilites : function (hilites) {
    this.Super("setHilites", arguments);
    this.dataChanged();
},

// detect whether two tile records are the same
_tileRecordsEqual : function (record1, record2) {

    if (record1 == null) return record2 == null;
    if (record2 == null) return record1 == null;

    // use == for equality if there's no datasource
    var dataSource = this.getDataSource();
    if (!dataSource) return record1 == record2;

    var pks = dataSource.getPrimaryKeyFieldNames();
    for (var i = 0; i < pks.length; ++i) {
        if (record1[pks[i]] != record2[pks[i]]) {
            return false;
        }
    }
    return true;
},

//> @method tileGrid.getTileHTML()
// When using the default +link{SimpleTile} class as +link{tileGrid.tileConstructor}, this
// method provides the HTML to be displayed within each tile.  See +link{tileGrid.tile}.
// 
// @param tileRecord (TileRecord) the tile for which HTML should be retrieved
// @return (HTML) HTML contents for the tile, as a String
// @visibility external
//<
getTileHTML : function (tileRecord) {
    return this.detailViewer.getBlockHTML([tileRecord]);
},

//> @method tileGrid.getTile()
// Returns the tile for the passed record or record index.
// <P>
// Note that this method may be overridden but developers should be aware that this
// method may be called repeatedly for the same record each time the TileGrid refreshes
// that row. If you override this API, you will need to cache and re-use the same
// tile objects per record. Typically this would be achieved by storing a pool of Tile
// objects that are re-used if a Record with the same primaryKey is passed to getTile().
// <P>
// When calling this method directly, if +link{showAllRecords} is false, this may 
// return null for records that are not currently visible.
//
// @param tile (TileRecord or int) record or index of record in this.data
// @return (Canvas) tile for this record
//
// @visibility external
//<
getTile : function (tile) {
    var tileID, record, tileIndex;
    
    if (isc.isAn.Object(tile)) { // record is passed in 
        record = tile;
        tileIndex = this.data.indexOf(tile);
        tileID = this.getTileID(tile);
    } else { // index is passed in, get the record
        if (!this.data) return null;
        record = this.data.get(tile);
        if (!record) return null;
        tileID = this.getTileID(record);
        tileIndex = tile;
    }
    if (tileIndex == null) return null;

    
    var startIndex = this.getDrawnStartIndex(),
        endIndex   = this.getDrawnEndIndex();

    if (tileIndex < startIndex || tileIndex >= endIndex) {
        this.logWarn("canReclaimTile() has been called with an index " + tileIndex +
                     " outside the visible range [" + startIndex + ", " + endIndex + "]");
        return null;
    }

    
    var cachedTile = tileID && window[tileID];
    if (cachedTile && this._tileRecordsEqual(record, cachedTile.record)) {
        if (cachedTile.tileNum != tileIndex) {
            this._updateTileIndex(tileIndex, cachedTile);
        }
        return cachedTile;
    }

    // instead of reusing the tile that the record was pointing to, use the tile that is pointed
    // to by forming an ID from the current index. This creates a smoother reuse of tiles, 
    // e.g. when filtering you won't have disjointed tiles reclaimed, but rather tiles 
    // will be sequentially reclaimed. With the other approach, when filtering a 
    // bottom-scrolled tileGrid we were getting overlapping tiles:
    //tileID = this._getTileID(tile);

    // set the tileID here to what it would be if created from scratch, so that it will fall into
    // the second else if branch below and we can cleanly reclaim it if a tile w/ that ID exists
    // Case which spawned this: adding a record to a sorted list with showAllRecords: false
    // overwrote the new tile because of an ID conflict
    //if (!tileID) tileID = this._getTileID(tileIndex);
    
    var isReclaimed = true, recTile = null;
    if (this.canReclaimTile(tileIndex) && !record.tileConstructor) {
        //this._limitLog('recycling tile:' + [tileIndex, record.commonName], "a");
        recTile = this._reclaimTile(tileIndex);

    // if there is a tileID, return the tile. Otherwise, make the new tile
    } else if (tileID && window[tileID]) {
        //this._limitLog('reclaiming existing tile:' + [tileIndex, tileID, record.commonName], "b");
        // pass the actual tile into _reclaimTile, to handle the bookeeping stuff
        recTile = this._reclaimTile(tileIndex, window[tileID]);

    } else { // create a new tile
        //this._limitLog('creating new tile:' + [tileIndex,record.commonName], "c");
        if (!this.tiles) this.tiles = [];
        recTile = this.makeTile(record, tileIndex);
        this.setTileID(record, recTile.ID);
        this._updateTileIndex(tileIndex, recTile);

        if (recTile.setFields && recTile.getFields && (recTile.getFields() == null ||
                                                       recTile.getFields().length == 0))
        {
            recTile.setFields(isc.clone(this.fields));
        }
        // add the created tile to tiles[] so it can be reclaimed
        this.tiles.add(recTile);
        isReclaimed = false;
    }

    // call any notification APIs that are present on tile
    if      (recTile.setValues) recTile.setValues(record);
    else if (recTile.setData)   recTile.setData([record]);

    recTile.record = record;

    if (isReclaimed) {
        if (recTile.isA("SimpleTile")) {
            // check if the record is selected and sync the tile state with that. Since tiles
            // are reclaimed, selection will be out of sync without this check.
            recTile.setSelected(this.selection.isSelected(record));
        }
        
        if (recTile.isDirty()) recTile.redraw("tile bound to new record");
    }

    return recTile;
},

//> @method TileGrid.getTileIndex()
// Returns the index of the specified tile.
// @param tile (Canvas) Tile you want to get the index for
// @return (int) index of the tile in this tileGrid. Will return -1 if the specified
// tile is not displayed within this grid.
// @visibility external
//<
getTileIndex : function (tile) {
    return this.tiles.indexOf(tile);
},

//> @method TileGrid.getCurrentTile()
// Returns the tile currently under the mouse.
// @return (SimpleTile) the tile currently under the mouse
// @visibility external
//<
getCurrentTile : function () {
    return this.detailViewer.currentTile;
},

// debug functions to limit the number of logwarns produced in long loops
_logs: [],
_logLimit: 10,
_clearLogs : function () {this._logs = [];},
_limitLog : function (message, key) {
    if (!this._logs.find("key", key)) {
        this._logs.add({key:key, logs:this._logLimit});    
    }
    if (this._logs.find("key", key).logs > 0) {
        isc.logWarn(message);
        this._logs.find("key", key).logs -= 1;
    }        
},

layoutTiles : function () {
    this.requestVisibleRows();   
    this.computeTileDimensions();
    //this._clearLogs();
    this.invokeSuper(isc.TileGrid, "layoutTiles");
    // in the case of scrolling to the end of the list when recycling tiles, its possible that
    // there will be leftover tiles from a previous call to layoutTiles(). These need to be hidden
    // or else they will be superimposed on the newer tiles and interfere with selection.
    // Theres also a case (non specific to recycling tiles) where tiles from before a filter
    // are superimposed on the new tiles, left over from the animation. Clean those up here.
    var tilesLen = this.tiles ? this.tiles.length : 0;
    var visLen = this._numTilesProcessed;
    //isc.logInfo('laying out tiles:' + [visLen, this._animating]);
    if (!this._animating && visLen < tilesLen) this.cleanupExtraTiles();
    // update emptyMessage label state
    this.updateEmptyMessageLabel();
    //this._checkConsistency();
},

invalidateCache : function () {
    this.Super("invalidateCache", arguments);
    this.requestVisibleRows();
},

requestVisibleRows : function () {
    var data = this.data;
    if (data == null || !isc.isA.ResultSet(data)) return;
    // Kick off an initial fetch if the data length is unknown [has never fetched].
    // This is a no-op if the length is known. In that case we allow
    // 'hasAllVisibleTiles', called from logic in TileLayout.js, to fetch as-yet-unloaded
    // individual records required for each tile.
    if (!data.lengthIsKnown()) {
        // if we don't have a set tileSize, just fetch the first record 
        if (this.tileSize == null) data.get(0);
        else {
            var tileRange = this.getVisibleTiles();
            data.getRange(tileRange[0], tileRange[1]);
        }
    }
},


_updateTileIndex : function (tileIndex, tile) {
    var indexToTileMap = this._tileIndexToTileMap,
        oldTile = indexToTileMap[tileIndex];
    if (oldTile != tile && oldTile != null && oldTile.tileNum == tileIndex) {
        this._addTileToReclaim(oldTile);
        oldTile.tileNum = null;
    }
    indexToTileMap[tileIndex] = tile;
    if (tile) tile.tileNum = tileIndex;
},
_getTileForTileIndex : function (tileIndex) {
    var indexToTileMap = this._tileIndexToTileMap,
        oldTile = indexToTileMap[tileIndex];
    return oldTile != null && oldTile.tileNum == tileIndex ? oldTile : null;
},


_addTileToReclaim : function (tile) {
    var pool = this._tileReclaimPool;
    if (pool != null) pool.push(tile);
},
_removeTileFromReclaim : function (tile) {
    var pool = this._tileReclaimPool;
    if (pool != null) pool.remove(tile);
},
_canReclaimTile : function (tile, startIndex, endIndex) {
    var tileNum = tile.tileNum;
    return tileNum == null || tileNum < startIndex || tileNum >= endIndex;
},
_getTileToReclaim : function () {
    var startIndex = this.getDrawnStartIndex(),
        endIndex   = this.getDrawnEndIndex();

    // rebuild pool - once per scroll
    var pool = this._tileReclaimPool;
    if (pool == null) {
        pool = this._tileReclaimPool = [];
        for (var i = 0; i < this.tiles.length; i++) {
            var tile = this.tiles[i];
            if (this._canReclaimTile(tile, startIndex, endIndex)) pool.push(tile);
        }
    }

    
    while (pool.length > 0) {
        var tile = pool.pop();
        if (this._canReclaimTile(tile, startIndex, endIndex)) return tile;
    }

    
    return null;
},
_lastVisibleTilesChanged : function () {
    delete this._tileReclaimPool;
},




// Tile reclamation 
// whenever a new tile is created, it is added to this.tiles. When a tile is reclaimed, the index 
// of the record it represents is mapped to this.tiles, and that tile is returned.

_reclaimTile : function (tileIndex, tile) {
    var record = this.data.get(tileIndex)
    //this._limitLog('_reclaimTile:' + [tileIndex, tile, this.getDrawnStartIndex()], "e");

    
    var reclaimedTile = tile || this._getTileForTileIndex(tileIndex) ||
                                this._getTileToReclaim();

    
    if (this.updateTile != null && this.recycleTiles) {
        this.updateTile(record, tileIndex, reclaimedTile);
    }
    
    // remove the pointer from the reclaimed tiles' current record to itself, if the record exists.
    // this is important for record<->tile integrity. If we don't do this, its possible to have
    // more than one record pointing to the same tile, which we don't want for obvious reasons
    var oldRec = reclaimedTile.record;
    if (oldRec) this.setTileID(oldRec, null);
    // set up record -> tile pointer
    this.setTileID(record, reclaimedTile.ID);
    // store tileNum for selection 
    this._updateTileIndex(tileIndex, reclaimedTile);
     
    return reclaimedTile;
},

canReclaimTile : function (tileIndex) {
    if (!this.recycleTiles) return false;

    var startIndex = this.getDrawnStartIndex(),
        endIndex   = this.getDrawnEndIndex(),
        visibleLength = endIndex - startIndex;

    
    return this.tiles.length >= visibleLength;
},

_tileRightMouseDown : function (tile) {
    // allow selection on mouse down
    return this._tileMouseDown(tile);
},

_tileMouseDown : function (tile) {
    var tileRecord = this.getTileRecord(tile);

    if (tileRecord) this.selection.selectOnMouseDown(this, tile.tileNum);

    // check that the tile is scrolled into view
    // scrolled off the top edge
    var xPos, yPos;
    if (tile.getTop() <  this.getScrollTop()) {
        yPos = "top";
    // scrolled off the bottom edge
    } else if (tile.getTop() + tile.getVisibleHeight() > this.getScrollTop() + this.getInnerHeight()) {
        yPos = "bottom";
    }
    // scrolled off the left edge
    if (tile.getLeft() < this.getScrollLeft()) {
        xPos = "left";
    // scrolled off the right edge
    } else if (tile.getLeft() + tile.getVisibleWidth() > this.getScrollLeft() + this.getInnerWidth()) {
        xPos = "right";
    }
    // if there is some portion of the tile offscreen, scroll it into view
    if (xPos || yPos) {
        this.scrollIntoView(tile.getLeft(), tile.getTop(), tile.getVisibleWidth(),
           tile.getVisibleHeight(), xPos, yPos, true);        
    }
},

_tileMouseUp : function (tile) {
    this.selection.selectOnMouseUp(this, tile.tileNum);        
},

_tileClick : function (tile) {
    var tileRecord = this.getTileRecord(tile);
    this.recordClick(this, tile, tileRecord);
},

_tileContextClick : function (tile) {
    var tileRecord = this.getTileRecord(tile);
    return this.recordContextClick(this, tile, tileRecord);
},

//>	@method	tileGrid.recordClick()    
// Executed when the tileGrid receives a 'click' event on a
// tile. The default implementation does nothing -- override to perform some action
// when any record is clicked.<br>
// A record event handler can be specified either as
// a function to execute, or as a string of script to evaluate. If the handler is defined
// as a string of script, all the parameters below will be available as variables for use
// in the script.<br>
// If you want to cancel the click based on the parameters, return false. Otherwise, return 
// true so that the click event be registered with the tile.
//
// @group	events
//
// @param viewer (TileGrid) the TileGrid itself
// @param tile (Canvas) the tile that was clicked on
// @param record (TileRecord) the record that was clicked on
//
// @example tilingEditing
// @visibility external
//<
recordClick : function (viewer, tile, record) {
},

//>	@method	tileGrid.recordDoubleClick()    
// Executed when the tileGrid receives a 'doubleclick' event on a
// tile. The default implementation does nothing -- override to perform some action
// when any record is doubleclicked.<br>
// A record event handler can be specified either as
// a function to execute, or as a string of script to evaluate. If the handler is defined
// as a string of script, all the parameters below will be available as variables for use
// in the script.<br>
// If you want to cancel the doubleclick based on the parameters, return false. Otherwise, return 
// true so that the doubleclick event be registered with the tile.
//
// @group	events
//
// @param viewer (TileGrid) the TileGrid itself
// @param tile (Canvas) the tile that was doubleclicked on
// @param record (TileRecord) the record that was doubleclicked on
//
// @example tilingEditing
// @visibility external
//<
recordDoubleClick : function (viewer, tile, record) {
},

//>	@method	tileGrid.recordContextClick()    
// Executed when the tileGrid receives a context-click (right mouse button) event on a
// tile. The default implementation does nothing -- override to perform some action
// when any record is right-clicked.<br>
// <smartclient>Return <code>false</code> to cancel the native behavior (suppressing
// the browser context menu).</smartclient>
// <smartgwt>Cancel the event to suppress the native browser context menu.</smartgwt>
// <P>
// A record event handler can be specified either as
// a function to execute, or as a string of script to evaluate. If the handler is defined
// as a string of script, all the parameters below will be available as variables for use
// in the script.<br>
// If you want to cancel the click based on the parameters, return false. Otherwise, return 
// true so that the click event be registered with the tile.
//
// @group	events
//
// @param viewer (TileGrid) the TileGrid itself
// @param tile (Canvas) the tile that was clicked on
// @param record (TileRecord) the record that was clicked on
// @return (boolean) return false to suppress the native browser context menu.
//
// @visibility external
//<
recordContextClick : function (viewer, tile, record) {
    return true;
},

// Selection
// --------------------------------------------------------------------------------------------

//> @method	tileGrid.selectionChanged() ([A])
// Called when selection changes within this tileGrid. Note this method fires for
// each record for which selection is modified - so when a user clicks inside a tileGrid this
// method will typically fire twice (once for the old record being deselected, and once for
// the new record being selected).
//
// @param	record  (Record)	record for which selection changed
// @param	state   (boolean)	New selection state (true for selected, false for unselected)
// @group selection
// @visibility external
//<    
    
// DONE-make sure to change _rowSelectionChanged in GridRenderer, also change _cellSelectionChanged
// called from within the selection object via the target property
selectionChange : function (record, state) {
    //isc.logWarn('selectionChange:' + [record.title, this.getTileID(record), state]);
    // call user-defined handler and bail (don't hilite rows) if it returns false
	if (this.selectionChanged && (this.selectionChanged(record, state) == false)) return false;

    // refresh the affected records to visually indicate selection
    var selection = this.selection,
        lastItem = selection.lastSelectionItem;

    var selTile = window[this.getTileID(lastItem)];
    if (selTile && selTile.setSelected) {
        selTile.setSelected(state);
    }
},

_$ArrowUp:"Arrow_Up", _$ArrowDown:"Arrow_Down",
_$ArrowLeft:"Arrow_Left", _$ArrowRight:"Arrow_Right",
// This method provides for arrow-key handling in TileGrid without interfering with normal 
// Canvas keyPress handling or user overrides of keyPress()
widgetHandleKeyPress : function (event, eventInfo) {
    // don't let keypresses happen until we're done animating
    if (this.isAnimating("scroll")) return false;
    var lastItem = this.selection.lastSelectionItem;
    if (!lastItem) return;
    var keyName = event.keyName,    
        lastItemIndex = this.selection.data.indexOf(lastItem),
        isHoriz = this.orientation == "horizontal",
        newIndex
    ; 
    if (keyName == this._$ArrowUp) {
        newIndex = isHoriz ? this._adjacentTileIndex(lastItemIndex, "above")
                            : lastItemIndex - 1;
    } else if (keyName == this._$ArrowDown) {
        newIndex = isHoriz ? this._adjacentTileIndex(lastItemIndex, "below")
                            : lastItemIndex + 1; 
        
    } else if (keyName == this._$ArrowLeft) {
        newIndex = isHoriz ? lastItemIndex - 1 :
                   this._adjacentTileIndex(lastItemIndex, "above"); 
    } else if (keyName == this._$ArrowRight) {
        newIndex = isHoriz ? lastItemIndex + 1 :
                   this._adjacentTileIndex(lastItemIndex, "below");
    } else {
        return;    
    }
    
    // need the data length check for when we're at the end and adding 1 to the index above
    if (newIndex == -1 || newIndex > this.data.getLength() - 1) return;
    // prevent errors getting thrown from multiple keypresses + scroll animation that make it 
    // through the cracks (between the isAnimating check and layoutTiles completing)
    if (newIndex == null) return false;
    var newRec = this.selection.data.get(newIndex),
        newTile = window[this.getTileID(newRec)];
    if (newTile) {
        this._tileMouseDown(newTile);
        
    }
    
    return false;
},

// get the tile that is most adjacent to the passed in tile (via startIndex). 
_adjacentTileIndex : function (startIndex, direction) {
    // first find the next line from the currently selected tile, in the passed in direction
    var data = this.selection.data,
        lineIndex = startIndex,
        startTile = window[this.getTileID(data.get(lineIndex))],
        isHoriz = this.orientation == "horizontal",
        startTileLengthPos = isHoriz ? startTile.getTop() : startTile.getLeft(),
        startBreadthPos = isHoriz ? startTile.getLeft() : startTile.getTop(),
        startBreadth = isHoriz ? startTile.getVisibleWidth() : startTile.getVisibleHeight(),
        currTile = startTile
    ;
    //lineIndex = direction == "above" ? lineIndex - 1 : lineIndex + 1;
    //var currTile = window[data.get(lineIndex)._tileID];
    while (startTileLengthPos == (isHoriz ? currTile.getTop() : currTile.getLeft())) {
        lineIndex = direction == "above" ? lineIndex - 1 : lineIndex + 1;
        // last row special cases: if we're at an edge row, return -1 to trigger a cancel 
        // of the selection
        if (lineIndex < 0 || lineIndex > data.getLength() - 1) {
            return -1;
        }
        currTile = window[this.getTileID(data.get(lineIndex))];
        // prevent any errors when an arrow key is pressed right when a scroll animation ends
        // but before layoutTiles has completed
        if (!currTile) return -1;
    }  
    // find the tile that is most adjacent to the start tile
    // this may be tricky for different tile sizes, so we have to iterate the data looking
    // for the most adjacent tile
    var linePos = isHoriz ? currTile.getTop() : currTile.getLeft();
    var bestMatchIndex = -1, bestMatchPixels = 0;
    while ((isHoriz ? currTile.getTop() : currTile.getLeft()) == linePos) {
        // get the number of pixels that the current tile overlaps the start tile by, and keep
        // track of it if its the max so far
        var currBreadthPos = isHoriz ? currTile.getLeft() : currTile.getTop(), 
            currBreadth = isHoriz ? currTile.getVisibleWidth() : currTile.getVisibleHeight(),
            range = this._getCommonRange([startBreadthPos, startBreadthPos + startBreadth],
                                          [currBreadthPos, currBreadthPos + currBreadth])
        ;
        if (range > bestMatchPixels) {
            bestMatchIndex = lineIndex;
            bestMatchPixels = range;
        }
        lineIndex = direction == "above" ? lineIndex - 1 : lineIndex + 1;
        // last row special cases: don't do anything if we're already on an edge row 
        if (lineIndex < 0 || lineIndex > data.getLength() - 1) break;
        currTile = window[this.getTileID(data.get(lineIndex))];
        // special case for recycled tiles: the next row may not exist,  which will cause the
        // while loop check to throw an error.
        if (!currTile) break;
        
    }
    
    return bestMatchIndex;
},
 
// first and second are arrays in the form of: [range start, range end]
// what will be returned is how much of second is within first.
_getCommonRange : function(first, second) {
    // make sure there is a actually an intersection
    if ((second[0] >= first[0] && second[0] <= first[1])
        || (second[1] >= first[0] && second[1] <= first[1])
        || (second[0] <= first[0] && second[1] >= first[1]))
    {
        // get the first range start or second range start, whichever is greater
        var start = second[0] > first[0] ? second[0] : first[0];        
        // get the first range end or second range end, whichever is less
        var end = second[1] > first[1] ? first[1] : second[1];
        // the difference between the start and end is the intersection
        return end - start;
    }
    return 0;
},

//> @method tileGrid.addTile()
// This is not allowed for tileGrid. Instead, use +link{tileGrid.addData}.
//
// @visibility external
//<
addTile : function () {
     return false;
},

//> @method tileGrid.removeTile()
// This is not allowed for tileGrid. Instead, use +link{tileGrid.removeData}.
//
// @visibility external
//<
removeTile : function () {
    return false;
},

//> @method tileGrid.addData()
// @include dataBoundComponent.addData()
// @group dataBoundComponentMethods
// @visibility external
// @example databoundAdd
//<

//> @method tileGrid.removeData()
// @include dataBoundComponent.removeData()
// @group dataBoundComponentMethods
// @visibility external
// @example databoundRemove
//<

getRecordTile : function (recordIndex) {
    if (recordIndex == null) return null;
    
    // avoid logWarn. 
    // maybe should consider making that logwarn (out of bounds get()) a logDebug.
    if (!this.data || recordIndex >= this.data.getLength()) return null;
    var tId = this.getTileID(this.data.get(recordIndex));
    if (!tId) return null;
    else return window[tId];
},

childVisibilityChanged : function (child, newVisibility) {
    // skip the tileLayout implementation of this method
    this.invokeSuper(isc.TileLayout, "childVisibilityChanged", child, newVisibility);
},

// @method tileGrid.hasAllVisibleTiles()
// @param range (array) data range to check for
// @param fetch (boolean) should we fetch the range if not present
//<
hasAllVisibleTiles : function (range, fetch) {
    if (isc.isA.ResultSet(this.data)) {

        // new dataset load in progress
        if (!this.data.lengthIsKnown()) return false;

        var rangeEnd = range[1] + 1;
        if (rangeEnd > this.data.getLength()) rangeEnd = this.data.getLength();
        if (this.data.rangeIsLoaded(range[0], rangeEnd)) {
            return true;    
        } else {
            if (fetch) {
                this.logDebug("in hasAllVisibleTiles, fetching range: " + range[0] + 
                              " to " + rangeEnd + ", total length: " + this.data.getLength(), 
                             "TileGrid");
                this.data.getRange(range[0], rangeEnd);
            }
            //isc.logWarn('data loading, returning:' + [range[0], range[1]]);  
            return false;
        }
        
    } else {
        return true;    
    }
},

// --------------------------Drag and Drop-----------------------------------------------------
dragAppearance:isc.EH.TRACKER,
dragTrackerMode: "title",

_setUpDragProperties : function () {
    
	// set up our specific drag-and-drop properties
	
	// set these properties before we set canDrag internally. If canDrag has
	// been set by the user, we want these props to be set as well.
	this.canReorderTiles = (this.canDrag || this.canReorderTiles);
	this.canDragTilesOut = (this.canDrag || this.canDragTilesOut);
	// like above, if canAcceptDrop is explicitly set, make sure that canAcceptDroppedRecords
	// is also set to keep backwards compatability
	this.canAcceptDroppedRecords = (this.canAcceptDrop || this.canAcceptDroppedRecords)

	this.canDrag = (this.canDrag || this.canDragTilesOut || this.canReorderTiles);
	//this.canDrop = (this.canDrop || this.canDragRecordsOut || this.canReorderRecords);
	this.canAcceptDrop = (this.canAcceptDrop || this.canAcceptDroppedRecords || this.canReorderTiles);

},

//>	@attr	TileGrid.tileDragAppearance		(DragAppearance : isc.EventHandler.TRACKER : IRWA)
// Visual appearance to show when the tile is being dragged.
// @visibility external
// @see Canvas.dragAppearance
// @group dragdrop
//<
tileDragAppearance: isc.EH.TRACKER,

//> @attr tileGrid.canReorderTiles (Boolean : false : [IRW])
// Indicates whether tiles can be reordered by dragging within this <code>TileGrid</code>.
// <p>
// <strong>NOTE:</strong> If <code>canReorderTiles</code> is initially enabled or might be
// +link{TileGrid.setCanReorderTiles(),dynamically enabled} after the grid is created,
// it may be desirable to disable +link{Canvas.useTouchScrolling,touch scrolling}
// so that touch-dragging a tile starts a reorder operation rather than a scroll. If
// +link{Canvas.disableTouchScrollingForDrag} is set to <code>true</code>, then touch
// scrolling will be disabled automatically. However, for +link{group:accessibility,accessibility}
// reasons, it is recommended to leave touch scrolling enabled and provide an alternative
// set of controls that can be used to perform drag-reordering of tiles.
// @visibility external
// @setter setCanReorderTiles()
// @group dragging
//<
//>	@method tileGrid.setCanReorderTiles()
// Setter for +link{tileGrid.canReorderTiles}.
// @visibility external
// @group dragging
//<
setCanReorderTiles : function (canReorderTiles) {
    this.canReorderTiles = canReorderTiles;
    this._setUpDragProperties();
},

//>	@attr tileGrid.canDragTilesOut (Boolean : false : [IRW])
// Indicates whether tiles can be dragged from this <code>TileGrid</code> and dropped elsewhere.
// <p>
// <strong>NOTE:</strong> If <code>canDragTilesOut</code> is initially enabled or might be
// +link{TileGrid.setCanDragTilesOut(),dynamically enabled} after the grid is created,
// it may be desirable to disable +link{Canvas.useTouchScrolling,touch scrolling}
// so that touch-dragging a tile starts a drag operation rather than a scroll. If
// +link{Canvas.disableTouchScrollingForDrag} is set to <code>true</code>, then touch
// scrolling will be disabled automatically. However, for +link{group:accessibility,accessibility}
// reasons, it is recommended to leave touch scrolling enabled and provide an alternative
// set of controls that can be used to perform drag and drop of tiles out of the grid.
// @visibility external
// @setter setCanDragTilesOut()
// @group dragging
//<
//>	@method tileGrid.setCanDragTilesOut()
// Setter for +link{tileGrid.canDragTilesOut}.
// @visibility external
// @group dragging
//<
setCanDragTilesOut : function (canDragTilesOut) {
    this.canDragTilesOut = canDragTilesOut;
    this._setUpDragProperties();
},

//>	@attr tileGrid.canAcceptDroppedRecords (Boolean : false : [IRW])
// Indicates whether records can be dropped into this TileGrid.
// @visibility external
// @group dragging
// @setter setCanAcceptDroppedRecords()
// @example dragListMove
//<

//>	@method tileGrid.setCanAcceptDroppedRecords()
// Setter for +link{tileGrid.canAcceptDroppedRecords}.
// @visibility external
// @group dragging
//<
setCanAcceptDroppedRecords : function (canAcceptDroppedRecords) {
    this.canAcceptDroppedRecords = canAcceptDroppedRecords;
    this._setUpDragProperties();
},
	
//> @method tileGrid.setDragTracker()
// @include dataBoundComponent.setDragTracker()
// @visibility external
//<

//> @method tileGrid.getDragTrackerProperties()
// @include dataBoundComponent.getDragTrackerProperties()  
//<

//> @attr tileGrid.dragTrackerStyle (CSSStyleName : "gridDragTracker" : IRW)
// @include dataBoundComponent.dragTrackerStyle
//<

//> @method tileGrid.getTitleField()
// @include dataBoundComponent.getTitleField() 
//<

//> @method tileGrid.getDragTrackerTitle()
// Return "title" HTML to display as a drag tracker when the user drags some record.<br>
// Default implementation will display the cell value for the title field (see 
// +link{listGrid.getTitleField()}) for the record(s) being dragged (including any
// icons / custom formatting / styling, etc).
// <p>
// Note: Only called if +link{listGrid.dragTrackerMode} is set to <code>"title"</code>.
// @param record (ListGridRecord) First selected record being dragged
// @param rowNum (number) row index of first record being dragged 
// @return (string) Title for the row. Default implementation looks at the value of the
//                  title-field cell for the row.
// @group dragTracker
// @visibility external
//<
getDragTrackerTitle : function (record) {     
    //if (this.tileDragAppearance == "tracker") {
    var titleField = this.getTitleField(),
        value = record[titleField];
    //if (!value) value = record[0];
    return "<nobr>" + value + "</nobr>";
  
},

//>	@method	tileGrid.drop()	(A)
//			handle a drop event
//		@return	(boolean)	true if the list can't reorder or dragging did not begin from the list body;
//							false if disabled, no selection, or otherwise
//		@group	events, dragging
//<
drop : function () {   
    var index = this._lastDropIndex || 0;    
    // the check below fixes an issue that occurs when dropping multiple tiles by dragging from 
    // an empty area of the grid. _lastDropIndex would get set to current data length
    // (see tileLayout.showDragLineForRecord()), but when the tiles were dropped and removed, 
    // this index is no longer valid.
    if (index > this.data.getLength()) index = 0;
    var source = this.ns.EH.dragTarget;

    var dragStartIndex = this._dragStartIndex;
    // reset _dragStartIndex so the next drag will start over
    this._dragStartIndex = null;      
    // don't check willAcceptDrop() this is essentially a parallel mechanism, so the developer 
    // shouldn't have to set that property directly.

    var dropRecords = source.cloneDragData();

    var targetRecord = this.data.get(index);

    this.transferRecords(dropRecords, targetRecord, index, source);
    
},

dropMove: function () {     
    // if the TileGrid can't be reordered, bail
	if (!this.canReorderTiles) return true;
    // bail on drops from foreign widgets if not configured to accept foreign drops
    if (!this.canAcceptDroppedRecords && isc.EH.dragTarget != this) return true;
	 
    this.showDragLineForRecord();
    
},

dragStart : function () {
    // if you're not allowed to drag tiles out, cancel dragging over anything that isn't this
    // grid
    var dropTarget = isc.EH.dropTarget;
    if (!this.canDragTilesOut && dropTarget != null && dropTarget != this) {
        return false;
    }
    var record = this.getSelectedRecord();
    if (record == null) return false;
},

dragMove : function () {
    var record = this.getSelectedRecord();
    
    if (this.tileDragAppearance == "outline") {
        var EH = this.ns.EH;
        var tId = this.getTileID(record);

        var tile = window[tId];

        //EH.dragMoveTarget = EH.getDragOutline(tile);
        //if (!EH.dragMoveAction) EH.dragMoveAction = EH._moveDragMoveTarget;
        var trackerHTML = "<div style='width:" + tile.getVisibleWidth() +  
                            ";height:" + tile.getVisibleHeight() + "'>" + 
                            EH.getDragOutline(tile).getInnerHTML() + "</div>";
        EH.setDragTracker(trackerHTML);
    } else if (this.tileDragAppearance == "target") {
        var EH = this.ns.EH;
        var tId = this.getTileID(record);
        // create the tracker html by wrapping the tile innerHTML in a div
        // that creates a fixed size boundary for the tile html
        // The approach of just using the tile itself as the dragMoveTarget
        // didn't work well because the tile was stuck inside of its parent
        // and wouldn't follow the mouse outside of this tilegrid.
        var tile = window[tId];
        this.detailViewer.clipValues = false;
        var trackerHTML = "<div style='width:" + tile.getVisibleWidth() +  
                            ";height:" + tile.getVisibleHeight() + "'>" + 
                            tile.getInnerHTML() + "</div>";
        this.detailViewer.clipValues = true;

        EH.setDragTracker(trackerHTML);
        // hide the tile to simulate what it would look like if the tile itself
        // were the dragMoveTarget
        tile.hide();
        this._hiddenDragTile = tile;

        if (!EH.dragMoveAction)  EH.dragMoveAction = EH._moveDragMoveTarget;

        // If the canvas wants to show a shadow on drag, show it now.
        if (EH.dragTarget.showDragShadow) EH._showTargetDragShadow();

        // If the canvas should change opacity on drag, handle this now.
        if (EH.dragTarget.dragOpacity != null) EH._setTargetDragOpacity();
    }
},

willAcceptDrop : function () {
    
    var EH = this.ns.EH;

    // If calling Super returns false or null, return it
    var superAccept = this.Super("willAcceptDrop", arguments);
    if (!superAccept) return superAccept;

    var theTarget = EH.dragTarget;
   
    //isc.logWarn('willAcceptDrop:' + theTarget);
    if (theTarget == this) {
        // Bail if we're attempting to drag records within TileGrid, and we can't reorder
        if (!this.canReorderTiles) return false;
    } else {
        // Bail if we're attempting to drag from elsewhere and canAcceptDroppedRecords is false
        // This gives us the granularity to allow drag reording (which will always set
        // canAcceptDrop to true, see _setUpDragProperties), but disable dropping from
        // external sources. Do allow bubbling in this case.
        if (!this.canAcceptDroppedRecords) return null;
    }

	// if the 'getDragData' for the dragTarget doesn't give us a suitable object (Array or
    // Object), bail -- but allow bubbling
    if (!isc.isAn.Object(theTarget.getDragData())) return null;
    
    return true;
},

dragStop : function () {
    this.Super("dropOut",arguments);
    // clean up potentially hidden tile. This happens when tileDragAppearance is
    // 'target'; we set _hiddenDragTile to point to the tile being dragged, because
    // that tile is manually hidden to properly simulate the 'target' appearance.
    // This means we have to manually show() it again after drag completes, if the
    // drop is not allowed.        
    if (this._hiddenDragTile) {
        this._hiddenDragTile.show();
        this._hiddenDragTile = null;
    }
},

//>	@method	tileGrid.transferDragData()
// @include dataBoundComponent.transferDragData()
//<

//>	@method	tileGrid.getDragData()
// @include dataBoundComponent.getDragData()
//<

//>	@attr	tileGrid.dragDataAction		
// @include dataBoundComponent.dragDataAction
//<

//> @method tileGrid.transferSelectedData()
// @include dataBoundComponent.transferSelectedData()
//<

// Formula/Summary builder required methods
getCellValue : function (record, field) {
    return this.detailViewer.getStandaloneFieldValue(record, field[this.fieldIdProperty]);
},

// DBC level override to call local getCellValue implementation - Formula/Summary builders
getStandaloneFieldValue : function (record, fieldName) {
    var value = this.getCellValue(record, this.getField(fieldName));
	return value;
},

// Formula/summary -related overrides from DBC
getTitleFieldValue : function (record) {
    var ds = this.getDataSource(),
        titleField = ds.getTitleField(),
        title = this.getCellValue(record, ds.getField(titleField));

    return title;
},

// basic show and hide methods
hideField : function (fieldName) {
    this.getField(fieldName).showIf = "false";
    this.getField(fieldName).hidden = true;
    this.fieldStateChanged();
},
showField : function (fieldName) {
    this.getField(fieldName).showIf = "true";
    this.getField(fieldName).hidden = false;
    this.fieldStateChanged();
},

//>	@method	tileGrid.getField()	(A)
//			return a field by fieldName
//
//		@return	(DetailViewerField) requested field or null
//<
getField : function (fieldId) { 
    if (!this.fields) return null;
    return isc.Class.getArrayItem(fieldId, this.fields, this.fieldIdProperty);
},
getFields : function () {
    return this.fields;
},
getAllFields : function () {
    return this.fields;
},

// ---------------------------------------------------------------------------------------
// FieldState
//

//>	@method	tileGrid.setFieldState() 
// Sets some presentation properties (visibility, width, userFormula and userSummary) of the 
// grid fields based on the +link{type:ListGridFieldState} object passed in.<br>
// Used to restore previous state retrieved from the grid by a call to +link{tileGrid.getFieldState()}.
//
// @group viewState
// @param fieldState (ListGridFieldState) state to apply to the grid's fields.
// @visibility external
// @see tileGrid.getFieldState()
//<
setFieldState : function (fieldState) {
    if (isc.isA.String(fieldState)) fieldState = this.evalViewState(fieldState, "fieldState")
    if (fieldState) {
        this.completeFields = this._setFieldState(fieldState);
        this.setFields(fieldState);
        this.markForRedraw();
        this.fieldStateChanged();
    }
},

//>	@method	tileGrid.getFieldState() 
// Returns a snapshot of the current presentation of this grid's fields as 
// a +link{type:ListGridFieldState} object.
// <P>
// This object can be passed to +link{tileGrid.setFieldState()} to reset this grid's fields to
// the current state.
// <P>
// Note that the information stored includes the current width and visibility of each of this 
// grid's fields.
//
// @return (ListGridFieldState) current state of this grid's fields.
// @group viewState
// @see tileGrid.setFieldState();
// @visibility external
//<


// ----------------------------------------------------------------------------
// panelHeader related methods
// ----------------------------------------------------------------------------
// panelHeader related methods

showActionInPanel : function (action) {
    // specifically add the "sort" action, which is not added by default
    if (action.name == "sort") return true;
    return this.Super("showActionInPanel", arguments);
},

// ---------------------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// printing
// ---------------------------------------------------------------------------
getPrintHTML : function () {
    var isAResultSet = isc.ResultSet && isc.isA.ResultSet(this.data);

    // new dataset load in progress
    if (isAResultSet && !this.data.lengthIsKnown()) {
        isc.logWarn("Attempt to print TileGrid " + this.ID + " while data is loading will be ignored");
        return "";
    }
    
    var len = this.data.getLength();
    // bail if we are trying to print a partially loaded resultset
    if (isAResultSet && !this.data.rangeIsLoaded(0, len)) {
        isc.logWarn("Make sure all data is loaded before attempting to print " +
            "TileGrid: " + this.ID);
        return "";
    }
    var table;
    var tpl = this.printTilesPerLine ? this.printTilesPerLine : this.getTilesPerLine();
    if (this.orientation == "horizontal") {
        var width = this.getInnerWidth();
        table = "<table width='" + width + "'>";
        
        for (var i = 0; i < len; i++) {
            var currTile = this.getTile(i);
            // for every time we lay out <tilesPerLine> tiles, create a new row
            if (i % tpl == 0) {
                // first row
                if (i == 0) table += "<tr>";
                // middle row with tiles left
                else if (i < len - 1) table += "</tr><tr>";
               
            }
            table += "<td>" + currTile.getPrintHTML() + "</td>";
        }
        table += "</tr></table>";
    } else {        
        //var height = this.getInnerHeight();
        table = "<table>";
        // number of rows is determined by tpl
        for (var i = 0; i < tpl; i++) {
            table += "<tr>";
            // for each row, layout tiles by skipping every tpl number of tiles,
            // starting at the current row number
            for (var j = i; j < len; j += tpl ) {
                var currTile = this.getTile(j);
                table += "<td>" + currTile.getPrintHTML() + "</td>"        
            }
            table += "</tr>";
        }
        table += "</table>";
    }
    return table;
},

// Empty and loading messages
// --------------------------------------------------------------------------------------------

//> @attr tileGrid.loadingMessage (HTMLString : null : IR)
// If you have a databound tileGrid and you scroll out of the currently loaded dataset, by
// default you will see blank tiles until the server returns the data for those rows.  The
// loadingMessage attribute allows you to specify arbitrary html that will be shown in each
// such "blank" tile while the data for that tile is loading.
// (e.g. "&lt;DIV ALIGN='CENTER'&gt;LOADING&lt;/DIV&gt;")
//
// @group emptyMessage, i18nMessages
// @visibility external
//<

//> @attr tileGrid.showEmptyMessage (Boolean : true : [IRW])
// @include gridRenderer.showEmptyMessage
//<
showEmptyMessage: true,

//> @attr tileGrid.emptyMessage (HTMLString : "No tiles to show." : [IRW])
// The string to display in the body of a tileGrid with an empty data array, if
// +link{showEmptyMessage} is true.
// @group emptyMessage, i18nMessages
// @visibility external
// @see tileGrid.showEmptyMessage
// @see tileGrid.emptyMessageStyle
//<
emptyMessage: "No tiles to show.",

//> @attr tileGrid.emptyMessageStyle (CSSStyleName : "emptyMessage" : [IRW])
// The CSS style name applied to the +link{emptyMessage} if displayed.
// @group emptyMessage
// @visibility external
//<
emptyMessageStyle: "emptyMessage",

// Label AutoChild used to implement the emptyMessage
emptyMessageLabelConstructor: "Label",
emptyMessageLabelDefaults: {
    width: "100%", height: 1,
    align: "center",
    snapTo: "T"
},


setShowEmptyMessage : function (show) {
    this.showEmptyMessage = show;
    this.updateEmptyMessageLabel();
},
setEmptyMessage : function (message) {
    this.emptyMessage = message;
    var label = this.emptyMessageLabel;
    if (label) label.setContents(message);
},
setEmptyMessageStyle : function (style) {
    this.emptyMessageStyle = style;
    var label = this.emptyMessageLabel;
    if (label) label.setStyleName(style);    
},

// update the emptyMessage label AutoChild to be shown or hidden as appropriate
updateEmptyMessageLabel : function () {
    var showEmptyMessage = this.showEmptyMessage,
        hasVisibleTiles = this._visibleTiles && this._visibleTiles.length > 0;

    if (this.emptyMessageLabel == null) {
        if (showEmptyMessage) this.addAutoChild("emptyMessageLabel", {
            contents: this.emptyMessage,
            visibility: hasVisibleTiles,
            styleName: this.emptyMessageStyle
        });
    } else {
        if (hasVisibleTiles || !showEmptyMessage) this.emptyMessageLabel.hide();
        else                                      this.emptyMessageLabel.show();
    }
}

});

isc.ClassFactory.defineClass("SimpleTile", "StatefulCanvas");

//> @class SimpleTile
// Default class used by a +link{TileGrid} to render each tile.  See +link{tileGrid.tile}.
// <P>
// SimpleTiles should not be created directly, instead, use a TileGrid and provide data and
// SimpleTile instances are created for you.
//
// @treeLocation Client Reference/Grids/TileGrid
// @visibility external
//<
isc.SimpleTile.addProperties({
    //> @attr simpleTile.baseStyle (CSSClassName : "simpleTile" : IR)
    // CSS style for the tile as a whole.  As with +link{StatefulCanvas.baseStyle}, suffixes
    // are appended to this style to represent various states ("Over", "Selected", etc).
    //
    // @visibility external
    //<
    baseStyle: "simpleTile",
    
    overflow:"hidden",

    showRollOver: true,

    redrawOnStateChange: true,
    
    _redrawWithParent: false,
    
    //> @method simpleTile.getInnerHTML()
    // The default implementation will call +link{tileGrid.getTileHTML()}.
    // @return (HTML) HTML contents for the tile, as a String
    // @visibility external
    //< 
    getInnerHTML : function () {
        var tileGrid = this.tileGrid;
        if (!tileGrid) return this.Super("getInnerHTML");

        tileGrid.detailViewer.currentTile = this;
        this.showDown = tileGrid.valuesShowDown;

        
        var tileRec = this.getRecord();
        if (!tileRec) return null;

        if (tileGrid.loadingMessage != null && Array.isLoading(tileRec)) {
            return tileGrid.loadingMessage;
        }
        return tileGrid.getTileHTML(tileRec);    
    },

    setValues : function (values) {
        // minimum implementation; force redraw
        this._dirty = true;
    },

    //> @attr simpleTile.creator (TileGrid : null : IR)
    // The +link{TileGrid} that created this SimpleTile.  This property will be null
    // if the tile was created by a user-provided +link{tileGrid.createTile()} method.
    // @deprecated  As of SmartClient 10.0, use +link{simpleTile.tileGrid}.
    // @see simpleTile.tileGrid
    // @visibility external
    //<

    //> @attr simpleTile.tileGrid (TileGrid : null : IR)
    // The +link{TileGrid} that created this SimpleTile.  
    // @visibility external
    //<

    //> @method simpleTile.getRecord()
    // Return the record that this tile should render.
    // <P>
    // NOTE: a TileGrid that is doing data paging may reuse tiles with different records, so a
    // subclass of SimpleTile should not cache the record returned by getRecord().
    // <P>
    // @return (TileRecord) the TileRecord associated with this tile
    // @visibility external
    //<
    getRecord : function () {
        
        return this.tileGrid.getTileRecord(this);
    }

});

isc.TileGrid.registerStringMethods({

    //> @method tileGrid.dataArrived() (A)
    // Notification method fired when new data arrives from the server to be displayed in this
    // tileGrid, (for example in response to the user scrolling a new set of tiles into view).
    // Only applies to databound tileGrid where the +link{tileGrid.data,data} attribute is a
    // +link{ResultSet}.
    // This method is fired directly in
    // response to +link{ResultSet.dataArrived(),dataArrived()} firing on the data object.
    // @param startRecord (int) starting index of the newly loaded set of records
    // @param endRecord (int) ending index of the newly loaded set of records (non inclusive).
    // @visibility external
    //<
    dataArrived:"startRecord,endRecord",
    
    selectionChanged:"record,state",
    
    itemHover : "item",
    itemClick : "item",
    recordClick : "viewer,tile,record",
    recordDoubleClick : "viewer,tile,record",
    recordContextClick: "viewer,tile,record",
	fieldStateChanged : ""
});

