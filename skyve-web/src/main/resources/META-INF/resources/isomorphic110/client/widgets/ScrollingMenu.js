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




//>	@class	ScrollingMenu
//
//	Implements a scrollable, user-selectable, menu
//
//  @treeLocation Client Reference/Control
//<


// define us as a subclass of the ListGrid
isc.ClassFactory.defineClass("ScrollingMenu", "ListGrid");

isc.defer("isc.ScrollingMenu.addProperties({ enableSelectOnRowOver: !isc.Browser.isTouch });");

isc.ScrollingMenu.addProperties({

    
    useBackMask:true,
    
    // Explicitly default canFocus to true.
    canFocus:true,
    
    showHeader:false,
    // Avoid showing edges.
    showEdges:false,
    
    autoDraw:false,
    
    // don't use the default ListGrid component/body styles, which usually have partial borders 
    className:"scrollingMenu",
    bodyStyleName:"scrollingMenuBody",

    selectionType:"single",
    leaveScrollbarGap:false,

    // keyboard - we respond to clicks and Enter keypresses identically. 
    // Space has no meaning.
    generateClickOnSpace : false,
    generateDoubleClickOnEnter : false,
    generateClickOnEnter : true,
    // By default show as a modal component.
    showModal:true,

    // arrowKeyAction will select by default
    
    arrowKeyAction:"select",    
    //enableSelectOnRowOver: null, // !isc.Browser.isTouch

    // default to filter on keypress if we show a filter editor
    filterOnKeypress:true

});

isc.ScrollingMenu.changeDefaults("filterEditorDefaults", {
        // If the filter editor is showing, explicitly give it a solid bg color.
        // This prevents things showing through under the transparent background of
        // the filter button image
        backgroundColor:"white",
        
        // Override editor keypress -- allow the user to move around and select
        // records as expected
        editorKeyPress : function (item, keyName, characterValue) {
            if (keyName == "Arrow_Down") {
                this.sourceWidget._navigateToNextRecord(1);    
                return false;
            }
            if (keyName == "Arrow_Up") {
                this.sourceWidget._navigateToNextRecord(-1);
                return false;
            }
            if (keyName == "Enter") {
                this.sourceWidget._generateFocusRecordClick();
                return;
            }
            return this.Super("editorKeyPress", arguments);
        }
});

isc.ScrollingMenu.changeDefaults("bodyDefaults", {
    
    _readyToSetFocus : function () {
        return this.creator._readyToSetFocus.apply(this.creator, arguments);
    }
});

isc.ScrollingMenu.addMethods({
    show : function () {
        
        if (this.showModal) this.showClickMask({target:this, methodName:"cancel"}, false, [this]);

        this.Super("show", arguments);
        if (this.showModal) this.body.focus();
    },

    // override recordClick to fire the 'itemClick' method.
    recordClick : function (viewer,record,recordNum,field,fieldNum,value,rawValue) {
        // hide before firing itemClick.
        // This avoids issues with focus, where the itemClick action is expected to put focus
        // somewhere that is masked until this menu hides.
        this.hide();
        // add support for click handlers on the individual rows
        // make itemClick a stringMethod?
        if (record != null) this.itemClick(record);
    },

    // override this for click handling behavior
    itemClick : function (record) {},
    
    // On RowOver change selection. The user can then use arrow keys to further modify selection
    // This matches behavior in native select item drop-downs.
    
    rowOver : function (record,rowNum,colNum) {
        if (this.enableSelectOnRowOver) {
            this.selection.selectSingle(record);
            this.fireSelectionUpdated();
        }
    },
    createSelectionModel : function (a,b,c,d,e) {
        var returnVal = this.invokeSuper("ScrollingMenu", "createSelectionModel", a,b,c,d,e);
        // Override selection so we can tell the difference between selection from rollOver and
        // from keyboard events / clicks.
        // This is required so we can do the right thing on Enter keypress
        this.selection.addProperties({
            selectOnRowOver : function (record) {
                this.selectSingle(record);
                this.selectionFromMouse = true;
            },
            
            setSelected : function (record, state) {
                this.selectionFromMouse = false;
                return this.Super("setSelected", arguments);
            }
        });
        
        return returnVal;
   },
    // Keyboard handling:

    // Override bodyKeyPress to handle firing 'cancel()' on escape click.    
    bodyKeyPress : function (event, eventInfo) {
        var keyName = event.keyName;
        
        if (keyName == this._$Enter) {
            var selection = this.selection;
            if (selection && selection.selectionFromMouse) {
                this.cancel();
                return false;
            }
        }
        if (keyName == "Escape") {
            this.cancel();

            // stop bubbling!
            return false;
        } 
        return this.Super("bodyKeyPress", arguments);
    },    
    
    
    cancel : function () {
        this.hide();
    },
    
    // Override hide to ensure that the clickMask gets hidden too
    hide : function () {    
        this.hideClickMask();
        return this.Super("hide", arguments);
    },
    
    // Always select the first item in the list *IF* nothing is selected
    
    _selectFirstOnDataChanged:true,
    dataChanged : function () {
        var returnVal = this.Super("dataChanged", arguments);
        if (!this._selectFirstOnDataChanged) return;
        
        if (this.data && this.data.getLength() > 0 && this.selection && !this.selection.anySelected() && 
            (isc.isA.ResultSet==null || !isc.isA.ResultSet(this.data) || this.data.rowIsLoaded(0))) 
        {
            this.selection.selectItem(0);
            this.fireSelectionUpdated();
        }
        return returnVal;
    }
                          
});

}
