/*
 * Isomorphic SmartClient
 * Version v9.1p_2014-03-26 (2014-03-26)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */

 






//>	@class	PopUptextAreaItem
//	A FormItem that displays an uneditable (static) value, with an icon to show a floating
//  text area, which can be used to edit the value.
// @visibility popUpTextAreaItem
//<

isc.ClassFactory.defineClass("PopUpTextAreaItem", "StaticTextItem");

isc.PopUpTextAreaItem.addProperties({

    // Override canFocus - pop up text areas can accept focus
    canFocus:true,

    //>	@attr	popUpTextAreaItem.wrap		(boolean : false : IRW)
	// Don't wrap the specified text - this allows it to be truncated more easily
	//		@group	appearance
	//<
	wrap:false,
    
    //>	@attr	popUpTextAreaItem.width		(number : 150 : IRW)
	//			Default width for fields.
	//		@group	appearance
	//<
	width:150,

    //>	@attr	popUptextAreaItem.clipValue (boolean : true : IRW)
	// Override clipValue to force any text displayed to be truncated.
	//		@group	appearance
	//<
    clipValue:true,
    
    //>	@attr	popUpTextAreaItem.popUpOnEnter (boolean : false : IRW)
	// Should the text area pop up when the user tabs into this field.
	//		@group	appearance
	//<
    
    popUpOnEnter:false,

    // whether to show the pop-up on a click anywhere in the item (as opposed to just on the
    // icon)
    popUpOnAnyClick:true,
    
    //>	@attr	popUpTextAreaItem.textAreaWidth (number : 100 : IRW)
	// How wide should the pop up textArea be drawn?
	//		@group	appearance
	//<
    textAreaWidth : 100,

    //>	@attr	popUpTextAreaItem.textAreaHeight (number : 100 : IRW)
	// How tall should the pop up textArea be drawn?
	//		@group	appearance
	//<
    textAreaHeight : 100,
    
    //>	@attr	popUpTextAreaItem.iconOnly (boolean : false : IRW)
	// If true, display the icon to launch the pop up with no text.
	// @group appearance
    // @visibility popUpTextAreaItem
	//<
    iconOnly : false,
    
    //>	@attr	popUpTextAreaItem.popUpIconSrc (string : [SKIN]/DynamicForm/PopUpTextAreaEditor_icon.gif : IRW)
	// If specified, use this src for the icon that launches to the pop up text area.
	// @group appearance
    // @visibility popUpTextAreaItem
	//<
    popUpIconSrc : "[SKIN]/DynamicForm/PopUpTextAreaEditor_icon.gif",

    //>	@attr	popUpTextAreaItem.popUpIconWidth (number : 20 : IRW)
	// Width for the popUp launcher icon.
	// @group appearance
    // @visibility popUpTextAreaItem
	//<
    popUpIconWidth:20,
    
    //>	@attr	popUpTextAreaItem.popUpIconHeight (number : 20 : IRW)
	// Height for the popUp launcher icon.
	// @group appearance
    // @visibility popUpTextAreaItem
	//<
    popUpIconHeight:20,
    
    // Setting iconVAlign to "center" ensures that if the icon height exceeds the content's height
    // (EG a single line of text) the text will be centered wrt the the icon.
    
    iconVAlign:isc.Canvas.CENTER
    
    
});

//!>Deferred
isc.PopUpTextAreaItem.addMethods({

    _setUpIcons : function () {
        if (this.icons == null) this.icons = [];
        
        var icon = {
            name:"popUpIcon",

            src:this.popUpIconSrc,
            
            showOver:false,
            
            width:this.popUpIconWidth,
            height:this.popUpIconHeight,
            
            
            click:this._popUpIconClick
            
        };
        
        // Add this to the icons array.
        this.icons.addAt(icon, 0);
        
        this.Super("_setUpIcons", arguments);
    },
    
    // click handler for the pop up icon - (applied directly to the icon so not fired in the
    // scope of the item)
    _popUpIconClick : function (form,item,icon) {
        if (item.popUpOnAnyClick || item.isDisabled()) return;
        item.showPopUp(true);
    },
    
    // Support showing the pop up text area from a click on the item (either on the icon or the
    // static text)
    
    handleCellClick : function () {
        if (this.Super("handleCellClick") == false) return false;
        if (this.popUpOnAnyClick && !this.isDisabled()) this.showPopUp(true);
    },

    // showPopUp - method to actually show the pop up.
    showPopUp : function (shouldFocus) {
        var value = this.getValue();

        if (!this._popUpForm) this.setupPopUpForm();
        
        this.placePopUp();

        var item = this._popUpForm.getItem("textArea");        
        item.setValue(value);
        
        this._popUpForm.bringToFront();
        this._popUpForm.show();
        if (shouldFocus) this._popUpForm.focusInItem("textArea");

        // Show a clickMask to hide the pop up form on click-outside
        
        this._popUpForm.showClickMask({target:this, methodName:"hidePopUp"}, true, 
                                        [this._popUpForm])
        
    },

    // If the item is hidden, ensure we also hide the pop up form.    
    visibilityChanged : function () {
        if (!this.isVisible()) this._hiddenObservation();
    },

    _hiddenObservation : function () {
        var pUF = this._popUpForm;
        if (!pUF || !(pUF.isVisible() && pUF.isDrawn())) return;
        
        pUF.hide();
    },
        
    // Whenever the item is moved by its container widget, we need to update the popUpForm's
    // position (if it is drawn).
    moved : function () {
        this._movedObservation();
    },
    
    _movedObservation : function () {
        var pUF = this._popUpForm;
        if (!pUF || !(pUF.isVisible() && pUF.isDrawn()) ) return;
        
        // If we've been moved out of the viewport, hide the TA temporarily (will reshow if
        // scrolled back into view, unless something else gets focus)
        var top = this.getTop(), left = this.getLeft(), 
            width = this.getInnerWidth(), height = this.getInnerHeight(),
            container = this.containerWidget,
            scrollTop = container.getScrollTop(), scrollLeft = container.getScrollLeft(),
            viewportWidth = container.getViewportWidth(), 
            viewportHeight = container.getViewportHeight()
        ;

        if (top < scrollTop || (top+height) > (scrollTop+viewportHeight) ||
            left < scrollLeft || (left+width) > (scrollLeft + viewportWidth) )
        {
            pUF.hide();
        } else {
            this.placePopUp();
        }
        
    },

    // if the ZIndex is modified, we need to ensure that if the pop up form is visible it
    // continues to float above the form item.    
    zIndexChanged : function () {
        var pUF = this._popUpForm;
        if (!pUF || !(pUF.isVisible() && pUF.isDrawn()) ) return;
        pUF.bringToFront();
    },

    
    placePopUp : function () {
        var top = this.getTextAreaTop(),
            left = this.getTextAreaLeft(),
            width = this.getTextAreaWidth(),
            height = this.getTextAreaHeight();
    
        
        this._popUpForm.moveTo(left,top);
        this._popUpForm.resizeTo(width,height);
        
        var item = this._popUpForm.getItem("textArea");
        item.setWidth(width);
        item.setHeight(height);
    },
    
    setupPopUpForm : function () {
        if (this._popUpForm != null) return;
        var PUF = isc.DynamicForm.create({
            autoDraw:false,
            ID:this.getID() + "_popUpForm",
            _generated:true,
            
            
            separateContentInsertion: false,
            
            
            cellPadding:0,
            
            // Hang a pointer back to this item on both the pop up form and the text area.
            targetItem:this,
            
            
            values : {textArea:this.getValue()},
            //numCols:1,
            items:[
                {name:"textArea", 
                    showTitle:false, 
                    type:"textArea",
                    //width:"*",
                    //height:"100%",
                    selectOnFocus:true,
                    // if the user tabs out of the item, hide it.
                    // Note: we set up the rowNum / colNum vars below

                    targetItem:this,
                    
                    focus : function (suppressHandlers) {
                        this.targetItem.textAreaFocus();
                    },
                    
                    // "Dirty" our targetItem on keypress so updateValue gets called at the 
                    // right moments
                    keyDown : function (item, form, keyName, characterValue) {
                        this.targetItem._markValueAsDirty();
                        return this.targetItem.textAreaKeyDown(item, keyName, characterValue);
                    },
                    
                    keyPress : function (item, form, keyName, characterValue) {
                        return this.targetItem.textAreaKeyPress(item, keyName, characterValue);
                    },
                    
                    blur: function () {
                        this.targetItem.textAreaBlur();
                    }
                }
            ],

            //showEdges:true,
            //canDragResize:true,
            
            hide : function (a,b,c,d) {
                var returnVal = this.invokeSuper(isc.DynamicForm, "hide", a,b,c,d);
                this.hideClickMask();
                return returnVal;
            }
        });
        
        this._popUpForm = PUF;
        
        var container = this.containerWidget;

        // Destroy if the item is destroyed
        PUF.observe(container, "destroy", "observer.hide();observer.destroy()");
    },
    
    // hidePopUP - method to hide the pop up (and ensure our value is up to date)
    hidePopUp : function () {
        if (this._popUpForm) {
            // updateValue will look at the value of the pop up form textArea
            this.updateValue();            
            this._popUpForm.hide();            
        }
    },
    
    // Override 'destroy' to ensure we also destroy the pop-up textArea's form
    destroy : function () {
        if (this._popUpForm) {
            this._popUpForm.destroy();
            delete this._popUpForm;
        }
        return this.Super("destroy", arguments);
    },
    

    //>	@method     popUptextAreaItem.getTextAreaTop() 
	// Method to determine where the pop up text area should be shown.  Default implementation
    // will display the pop up over this item.
	//	@return  (number)   Page level top coordinate for the text area in px.
	//<
    getTextAreaTop : function () {
        // by default look at the position of this form item
        var top = this.getPageTop();
        
        if (isc.Browser.isIE) top -= 1;
        return top;
    },

    //>	@method     popUptextAreaItem.getTextAreaLeft() 
	// Method to determine where the pop up text area should be shown.  Default implementation
    // will display the pop up over this item.
	//	@return  (number)   Page level left coordinate for the text area in px.
	//<    
    getTextAreaLeft : function () {
        return this.getPageLeft();
    },
    
    //>	@method     popUptextAreaItem.getTextAreaWidth() 
	// How wide should the text area be drawn.  Default implementation looks at 
    // <code>this.textAreaWidth</code>.
	//	@return  (number)   Width to apply to this textArea
	//<
    getTextAreaWidth : function () {
        return Math.max(this.textAreaWidth, this.getInnerWidth());
    },
    
    //>	@method     popUptextAreaItem.getTextAreaHeight() 
	// How tall should the text area be drawn.  Default implementation looks at 
    // <code>this.textAreaHeight</code>.
	//	@return  (number)   Height to apply to this textArea
	//<
    getTextAreaHeight : function () {
        return this.textAreaHeight;
    },
    
    // MapValueToDisplay - override to show blank if necessary
    mapValueToDisplay : function () {
        if (this.iconOnly) return "";
        return this.Super("mapValueToDisplay", arguments);
    },
    
    
    // By Default, on blur of the T.A., hide the T.A.
    textAreaBlur : function () {
        this.hidePopUp();  
    },
    
    textAreaFocus : function () {},
    

    //>	@method popUptextAreaItem.textAreaKeyPress() 
	// Handler for keypress occurring on the textarea.
    // <P>
    // The TextAreaItem from the pop-up is passed is as "item", while "this" is the
    // PopUpTextAreaItem.
    // @param item (formItem) item that recieved the event (a pointer to the text area itself)
    // @param keyName (keyName) which key was pressed
    // @param characterValue (number) numeric value of the character produced by the keypress
    //  (will be null for non character keys)
	//	@return  (boolean)   Return false to cancel the key event 
	//<
    // No-op by default.
    textAreaKeyPress : function (item, keyName, characterValue) { },
    textAreaKeyDown : function (item, keyName, characterValue) { },
    
	//Override setValue -- if we're showing the pop up, update its value 
    setValue : function (newValue) {
        var oldDisplayValue = this.mapValueToDisplay(this.getValue());
        this.Super("setValue", arguments);
        var newDisplayValue = this.mapValueToDisplay(this.getValue());
        
        if (oldDisplayValue != newDisplayValue) {
            this.setElementValue(newDisplayValue)
            if (this._popUpForm && this._popUpForm.isVisible()) {
                this._popUpForm.setValue("textArea", newDisplayValue);
            }
        }
    },

    // Override 'updateValue' -- if we're showing the pop-up, get the value from the pop up
    // [ensuring it's up to date first]
    updateValue : function () {
               
        
        if (this._popUpForm && this._popUpForm.isVisible() && 
            !this._popUpForm._setValuesPending) 
        {
            var item = this._popUpForm.getItem("textArea");
            item.updateValue();
            var newValue = this._popUpForm.getValue("textArea");
            this.setElementValue(this.mapValueToDisplay(newValue));
            this._updateValue(newValue);
          
        } else {
            return this.Super("updateValue", arguments);
        }
        
    },
    
    // Override setElementValue to set the value of the static text 
    setElementValue : function (newVal) {
        if (this.iconOnly) return;
        return this.Super("setElementValue", arguments);
    },
    
    // override 'focusInItem' - by default we'll want to put focus in the textArea if it's
    // visible - otherwise on the icon that launches it.
    focusInItem : function () {
        if (this._popUpForm && this._popUpForm.isVisible()) {
            this._popUpForm.focusInItem('textArea');
        } else if (this.showIcons) {
            this.focusInIcon(this.icons[0]);
        } else {
            // showIcons:false - in the absence of a visible icon the pop-up must show
            // immediately on focus, as there would otherwise be no element to hold/show the
            // current focus
            this.showPopUp(true);
        }
    },
    
    // override setElementTabIndex() -- we want to reset the tabIndex of the icons and avoid
    // redrawing the form.
    _setElementTabIndex : function (tabIndex) {

        this._elementTabIndex = tabIndex;
        
        if (!this.isVisible() || !this.containerWidget.isDrawn()) return;
        
        this._updateIconTabIndices();
    },

    //> @method popUpTextAreaItem.setElementReadOnly()
    // Change the read-only state of the form element immediately.
    //<
    setElementReadOnly : function (readOnly) {
        // All disabling is related to the icons and clickability. This override prevents
        // redrawing the item.
    }

});
//!<Deferred

