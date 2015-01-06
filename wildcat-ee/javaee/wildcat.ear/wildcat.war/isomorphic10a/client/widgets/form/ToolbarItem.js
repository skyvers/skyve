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

 





//>	@class	ToolbarItem
//
// Set of horizontally arranged buttons.
//
// @visibility external
//<
isc.ClassFactory.defineClass("ToolbarItem", "CanvasItem");

isc.ToolbarItem.addProperties({

    // Override canFocus -- even though toolbars have no data element, they can accept focus.
    canFocus:true,
    
    // Avoid attempting to save this item's value in the values array.
    shouldSaveValue:false,
    
    // Toolbars are non editable
    isEditable : function () {
        return false;
    },

	//>	@attr	toolbarItem.buttons		(Array of StatefulCanvas : null : IRW)
	//  List of buttons for the toolbar.
    //  Each buttons should be specified as a simple JS object with properties to apply to the
    //  button to be displayed. Note that any <code>click</code> stringMethod applied to the
    //  button will be passed 2 parameters: <code>form</code> and <code>item</code>.
	//		@group	items
    // @visibility external
	//<

    //>	@attr	toolbarItem.showTitle		(Boolean : false : IRW)
	//	Don't show a title for toolbars
	//		@group	appearance
    // @visibility external
	//<
	showTitle:false,						


    //>	@attr	toolbarItem.buttonSpace     (number : 4 : IRW)
	//  Space between buttons.
	//		@group	appearance
	//<
	buttonSpace:4,

    //>	@attr	toolbarItem.startRow		(Boolean : true : IRW)
	//			these items are in a row by themselves by default
	//		@group	appearance
    // @visibility external
	//<
	startRow:true,

	//>	@attr	toolbarItem.endRow			(Boolean : true : IRW)
	//			these items are in a row by themselves by default
	//		@group	appearance
    // @visibility external
	//<
	endRow:true,

    //>	@attr	toolbarItem.colSpan			(boolean : true : IRW)
	//			these items span all columns
	//		@group	appearance
    // @visibility external
	//<
	colSpan:"*",
    
    //> @attr toolbarItem.canvas (AutoChild Canvas : null : R)
    //      This item is an autoChild generated +link{class:Canvas} displayed by
    // the ToolbarItem and is an instance of +link{class:Toolbar} by defaut, cuztomizeable 
    // via the +link{attr:toolbarItem.canvasConstructor} attribute.
    // @visibility external
    //<

    //>@attr    toolbarItem.canvasConstructor  (Class : Button : IRA)
    //  Our toolbar will be rendered out as a toolbar widget.
    //  @visibility internal
    //<
    canvasConstructor:isc.Toolbar,

    //>@attr    toolbarItem.vertical    (Boolean    : false : IRA)
    //  Should the toolbar stack its buttons vertically or horizontally?
    //  @visibility external
    //<    
    vertical:false,
    
    //>@attr    toolbarItem.buttonConstructor   (Class : isc.AutoFitButton : IRWA)
    //  Use AutoFitButtons rather than simple Buttons for this toolbar's buttons by default.
    //  @visibility external
    //<
    buttonConstructor:isc.AutoFitButton
    
    //>@attr    toolbarItem.buttonProperties   (object : null : IRA)
    //  Default properties for this toolbar's buttons.
    //  @visibility external
    //<
    
    
    //>	@attr	toolbarItem.buttonBaseStyle        (CSSStyleName : null : IRW)
	//  If specified this baseStyle will be applied to the buttons in this toolbar.
	//		@group	appearance
    //      @visibility external
	//<
	//buttonBaseStyle:null,
    
    //>	@attr	buttonItem.buttonTitleStyle       (CSSStyleName : "buttonItem" : IRW)
	//  CSS class to apply to the buttons' titles.
    //      @visibility internal
	//<
    


});

isc.ToolbarItem.addMethods({

    _createCanvas : function () {
    
        var buttons = (this.buttons || this.items || []),
            buttonProperties = this.buttonProperties || {};
            
        isc.addProperties(buttonProperties, {
            // override handleClick to fire 'click' passing in 'form' and 'item' as params
            handleActivate : function () {
                var item = this.parentElement.canvasItem,
                    form = item.form;
                if (this.click != null) return this.click(form, item);
            }
        });
        

        if (this.buttonBaseStyle && !buttonProperties.baseStyle) {
            buttonProperties.baseStyle = this.buttonBaseStyle;
        }
        if (this.buttonTitleStyle && !buttonProperties.titleStyle) {
            buttonProperties.titleStyle = this.buttonTitleStyle;
        }
        
        // Update properties on the individual button init blocks as required.
        this._updateButtons(buttons);
        
        this.canvas = { overflow:isc.Canvas.VISIBLE, 
                        buttons:buttons,
                        membersMargin:this.buttonSpace,
                        vertical:this.vertical,
                        // Button defaults are applied via Toolbar.button property
                        buttonProperties:buttonProperties };

        var height = this.height,
            width = this.width;
        if (!isc.isA.Number(width) && this._size) 
            width = isc.isA.Number(this._size[0]) ? this._size[0] : null;
        if (!isc.isA.Number(height) && this._size) 
            height = isc.isA.Number(this._size[1]) ? this._size[1] : null;

        if (height) this.canvas.height = height;
        if (width) this.canvas.width = width;
        
        if (this.buttonConstructor != null) 
            this.canvas.buttonConstructor = this.buttonConstructor;

        return this.Super("_createCanvas", arguments);
    },
    
    // Helper function to update the button init blocks before creating them as button instances
    _updateButtons : function (buttons) {
        if (!buttons || buttons.length == 0) return;
        // override 'click' on button init blocks to always be functions taking 'form,item'
        // as params
        for (var i = 0; i < buttons.length; i++) {
            if (buttons[i].click && isc.isA.String(buttons[i].click)) {
                buttons[i].click = isc.Func.expressionToFunction("form,item", buttons[i].click);
            }
        }   
        
    },

    //>EditMode dynamic addition/removal of buttons
    getButton : function (button) {
        return isc.Class.getArrayItem(button, this.buttons, "name");
    },
    addButton : function (button, index) {
        this.buttons = this.buttons || [];
        this.buttons.addAt(button, index);
        this.setButtons(this.buttons);
    },
    removeButton : function (button) {
        var index = isc.Class.getArrayItemIndex(button, this.buttons, "name");
        if (index != -1) {
            this.buttons.removeAt(index);
            this.setButtons(this.buttons);
        }
    },
    //<EditMode
    
    setButtons : function (buttons) {
        this._updateButtons(buttons);
        this.buttons = buttons;
        // if we're not initialized, no-op since the buttons will get set up with _createCanvas
        // when we init
        if (!this.canvas) return;
        
        this.canvas.setButtons(buttons);
    },

    // Allow setItems as well as setButtons for back-compat
    setItems : function (buttons) {
        return this.setButtons(buttons);
    }
    
});


