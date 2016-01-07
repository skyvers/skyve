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



//>	@interface PickList
// Interface to show a drop-down list of pickable options. Used by the +link{SelectItem} and
// +link{ComboBoxItem} classes. The generated drop down list of options must be an instance of
// +link{class:PickListMenu}, or a subclass thereof. 
// @treeLocation Client Reference/Forms/Form Items
// @visibility external
//<

isc.ClassFactory.defineInterface("PickList");

//>	@class PickListMenu
// +link{class:ListGrid} subclass used, by default, by FormItems which implement
// +link{interface:PickList} to display
// a list of selectable options.  Can be subclassed, customized and assigned to FormItems 
// via the +link{comboBoxItem.pickListConstructor, pickListConstructor} attribute.
// @treeLocation Client Reference/Forms/Form Items
// @visibility external
//<


isc.ClassFactory.defineClass("PickListMenu", "ScrollingMenu");

isc.PickListMenu.addClassProperties({
    // object to hold cached pickListMenu instances for databound pickList items
    _cachedDSPickLists:{},
    // Don't cache more than 50 DS pickListMenus    
    pickListCacheLimit:50
});

isc.PickListMenu.addProperties({
    
    // explicitly doc the default pickList empty message (may be overridden by pickListEmptyMessage
    // on a per-instance basis too)
    //> @attr PickListMenu.emptyMessage (string : "No Items To Show" : IRW) 
    // @include ListGrid.emptyMessage
    // @visibility external
    // @group i18nMessages
    //<
    
    // explicitly doc dataProperties - useful to be able to customzie
    //> @attr PickListMenu.dataProperties
    // @include ListGrid.dataProperties
    // @visibility external
    //<
    
    // Don't get fields from the DS.
    useAllDataSourceFields:false,
    
    // disallow tabbing to the pickList
    tabIndex:-1,
    
    // Don't allow fields to be resized, if multiple are showing
    
    canResizeFields:false,
    // Since we don't support drag resize of fields, turn canFreezeFields off by default
    canFreezeFields:false,
    
    //> @attr pickListMenu.styleName (CSSStyleName : "pickListMenu" : IRW)
    // @include listGrid.styleName
    // @visibility external
    //<
    styleName:"pickListMenu",
    //> @attr pickListMenu.bodyStyleName (CSSStyleName : "pickListMenuBody" : IRW)
    // @include listGrid.bodyStyleName
    // @visibility external
    //<
    bodyStyleName:"pickListMenuBody",
    
    
    //> @attr pickListMenu.normalCellHeight (number : 16 : IRWA)
    // @include listGrid.normalCellHeight
    //<
    normalCellHeight:16,

    scrollToCellXPosition: "left",

    scrollToCellYPosition: "top",

    animateTransitions: !isc.Browser.isMobileIE,

    skinUsesCSSTransitions: false,

    animateDuration: 350
});

isc.PickListMenu.addMethods({

    getTransformCSS : function () {
        if (!this._fillScreenContainer || !isc.Browser._supportsCSSTransitions || !this.animateTransitions || !this.skinUsesCSSTransitions) {
            return null;
        } else {
            var y = !this._showing ? "100%" : "0px";
            return ";" + isc.Element._transformCSSName + ": translateY(" + y + ");";
        }
    },

    handleTransitionEnd : function (event, eventInfo) {
        if (eventInfo.target === this && this._fillScreenContainer != null) {
            this._enableOffsetCoordsCaching();

            if (!this._showing) {
                this._fillScreenContainer.hide();
            } else {
                
                if (this.showModal) this.body.focus();
            }
        }
    },

    _readyToSetFocus : function (focus) {
        
        if (focus &&
            isc.Browser._supportsCSSTransitions && this.animateTransitions && this.skinUsesCSSTransitions &&
            this._showing)
        {
            return false;
        }
        return this.Super("_readyToSetFocus", arguments);
    },

    _animateShow : function () {
        
        if (!isc.Canvas._isInFront(this._fillScreenContainer)) {
            this._fillScreenContainer.bringToFront();
        }

        this._fillScreenContainer.show();

        if (!isc.Browser._supportsCSSTransitions || !this.animateTransitions || !this.skinUsesCSSTransitions) {
            this.show();
            this.moveTo(0, this.getInnerHeight());
            this.animateMove(0, 0, function () {}, this.animateDuration);
        } else {
            this._showing = true;
            this.show();
            
            this.delayCall("__animateShow");
        }
    },

    __animateShow : function () {
        this._disableOffsetCoordsCaching();
        isc.Element._updateTransformStyle(this, "translateY(0px)");
    },

    _animateHide : function () {
        if (!isc.Browser._supportsCSSTransitions || !this.animateTransitions || !this.skinUsesCSSTransitions) {
            var container = this._fillScreenContainer;
            this.animateMove(0, this.getInnerHeight(), function () {
                container.hide();
            }, this.animateDuration);
        } else {
            this._showing = false;
            // Delay the start of the hide transition slightly so that touch browser users can
            // see the rollOver effect on the tapped option.
            this.delayCall("__animateHide", null, 100);
        }
    },

    __animateHide : function () {
        this._disableOffsetCoordsCaching();
        isc.Element._updateTransformStyle(this, "translateY(100%)");
    },

    // Pick up valueIcons from this form item, if specified
    getValueIcon : function (field, value, record) {
        var formItem = this.formItem;

        // If this is a databound pickList, we have 2 sets of valueIcons to consider - 
        // those specified by the formItem and those specified on the field definition itself.
        // - If the form item specifies any valueIcons, we typically want to show them on the 
        //   column that matches the display-value for the formItem.
        //   - This will be the result of formItem.getDisplayField() or formItem.getValueField()
        // - If the developer specified explicit pickListFields for this item, this field may
        //   not be showing. In this case we allow the developer to explicitly specify a 
        //   field to show the form item valueIcons via the property "formItem.valueIconField".
        //   If this property is set, always respect it.
        // - For all other fields, and if the form item has no custom valueIcons, just fall
        //   through to the standard ListGrid implementation, so we can pick up valueIcons
        //   specified on the datasource fields.
        var hasCustomValueIcons = formItem && !formItem.suppressValueIcons &&
                                  (formItem.valueIcons != null || formItem.getValueIcon !=null);
        
        if (hasCustomValueIcons) {
            var valueField = formItem.getValueFieldName(),
                valueIconField = formItem.valueIconField || 
                                 formItem.getDisplayFieldName() || valueField;
            if (this.getFieldName(field) == valueIconField) {
                // The form item expects the value passed to getValueIcon to be the 'valueField' 
                // value, not the value from whatever field is being displayed in the pickList
                return formItem._getValueIcon(record[valueField]);
            }
        }
        
        return this.Super("getValueIcon", arguments);
    },

    // arrowKeyAction - for single selects we change selection as the user moves
    // around with arrow keys
    // for multi select, we change hilight only and allow the user to select with
    // "space" keypress    
    getArrowKeyAction : function () {
        return this.allowMultiSelect ? "focus" : "select";
    },

    // showOverAsSelected - will remap all "Over" styles to use the
    // "Selected" class name.
    
    showOverAsSelected:true,

    // override rowClick / recordClick - we don't want to hide on keyboard navigation
    // Note that generateClickOnSpace is set up dynamically as 
    // part of the 'show' override.
    rowClick : function (record, recordNum, fieldNum, keyboardGenerated) {
        this._keyboardRowClick = keyboardGenerated;
        this.Super("rowClick", arguments);
        delete this._keyboardRowClick;
    }, 
    recordClick : function (viewer,record,recordNum,field,fieldNum,value,rawValue) {
        // Skip 'canSelect:false' records. Don't hide the viewer or fire the itemClick handler
        if (record[viewer.recordCanSelectProperty] == false) return;

        var shouldDismiss;
        if (this._keyboardRowClick) {
            // Nothing to do here on Space key press.
            // On Arrow key press we should update the 'aria-activedescendant' attribute on
            // the formItem if running in screen reader mode.
            
            var keyName = this.ns.EH.getKey(),
                isEnter = (keyName == "Enter");
            if (!isEnter) {
                var formItem = this.formItem;
                if (isc.Canvas.ariaEnabled() &&
                    formItem != null &&
                    formItem.ariaRole === "combobox" &&
                    this.body != null)
                {
                    var startRow = this.body.getDrawArea()[0];
                    var rowElementId = this.getRowElementId(recordNum, recordNum - startRow);
                    formItem.setAriaState("activedescendant", rowElementId);
                }
                if (keyName != "Space" || !formItem.allowMultiSelect) return;
                shouldDismiss = false;

            // On Enter keypress, always dismiss the pickList
            } else {
                shouldDismiss = true;
            }
        } else {
            shouldDismiss = !this.allowMultiSelect
        }

        // hide before firing itemClick.
        // This avoids issues with focus, where the itemClick action is expected to put focus
        // somewhere that is masked until this menu hides.
        if (shouldDismiss) this.hide();
        // itemClick handles updating the formItem with the selected value(s) from the
        // pickList.
        if (record != null) this.itemClick(record);
    },
    
    // This method is fired when the user hits Space or Enter and 
    // generateClickOnSpace/Enter is set to determine whether we should toggle 
    // selection on the row.
    // Overridden to disable toggling selection on Enter keypress in a multi-select
    // pickList, since we dismiss the pickList in this case and it'd be confusing to
    // have the highlighted value be toggled when the user is really attempting
    // to accept the current selection and dismiss the pickList.
    selectOnGeneratedCellClick : function (record, rowNum, colNum, body) {
        if (this.allowMultiSelect && isc.EH.getKey() == "Enter") return false;
        return this.Super("selectOnGeneratedCellClick", arguments);
    },
    
    headerClick : function (fieldNum, header) {
        var rv = this.Super("headerClick", arguments);
        var field = this.getField(fieldNum);
        // check if the checkbox column header was clicked
        if (this.isCheckboxField(field) && this.allowMultiSelect) {
            this.multiSelectChanged();
        }
        return rv;
    },

    multiSelectChanged : function () {
        var formItem = this.formItem,
            fieldName = formItem.getValueFieldName(),
            sel = this.getSelection(),
            value = !sel || sel.length == 0 ? null : sel.getProperty(fieldName)
        ;
        
        formItem.pickValue(value);
    },

    // 'pick' the selected value on click.  Matches Windows ComboBox behavior
    itemClick : function (record) {
        if (this.allowMultiSelect) {
            this.multiSelectChanged();
        } else {
            var formItem = this.formItem,
                fieldName = formItem.getValueFieldName();       
         	var value = record[fieldName];
            if (isc.PickList.emptyStoredValue && value == isc.PickList.emptyStoredValue) value = null;
            formItem.pickValue(value);
        }
    },
    
    hide : function (a,b,c,d) {
        var isVisible = this.isVisible() && this.isDrawn();
        if (this._fillScreenContainer) {
            this._animateHide();
        } else {
            this.invokeSuper(isc.PickListMenu, "hide", a,b,c,d);                    
        }
        
        // If we're being hidden as part of a formItem.destroy(), this.formItem will have been 
        // cleared out.
        if (!this.formItem) return;
        
        // put focus back in the item if this was a modal pickList 	 
        if (isVisible && this.showModal) this.formItem.focusInItem();
                 
        // Clear out the showingPickList flag
        this.formItem._showingPickList = null;                 
        
        // fire a notification for observing / overriding the pick list being hidden
        if (isVisible) this.formItem._pickListHidden();
        
        
        delete this.formItem._showOnFilter;

        // Clear the last hilite to reset the "last position" for the purpose
        // of arrow key navigation. Once the PickList is hidden, we don't want
        // it to remember where it was last if re-shown.
        this.clearLastHilite();
    },

    show : function () {
        // If the pickList is already showing we could arguably bail here, but this isn't
        // how Canvas.show() behaves (still calls 'setVisibility()')...
        // Instead we'll just avoid firing the _pickListShown notification function 
        var alreadyShowing = this.isVisible() && this.isDrawn();

        // keyboard generated click handling:
        // "Enter" keypress always generates the "click" event.
        // This accepts the selected values and dismisses the editor
        this.generateClickOnEnter = true;

        // "Space" keypress
        // - no action for single-selects (selection already achieved
        //   on arrow keypress)
        // - Toggles selection for multi-selects, without dismissing the editor
        this.generateClickOnSpace = this.allowMultiSelect;
        // See also recordClick modifications / selectOnGeneratedCellClick

        
        if (this.parentElement == null) this.bringToFront();

        this.Super("show", arguments);
        // fire a notification for observing / overriding the pick list being shown                    
        if (!alreadyShowing) {
            this.formItem._pickListShown();
        }
    },

    // Override showClickMask() - if this is a modal PickList, ensure that when the pickList is
    // hidden focus goes to the form item that spawned the pickList.    
    showClickMask : function () {
        if (this._fillScreenContainer) {
            // we don't need mask in this case
            return;
        }
        if (!this.clickMaskUp(this.getID())) {
            // Actually cmID will match this.getID() since that's the default ID for a CM
            // associated with a widget.
            var cmID = this.Super("showClickMask", arguments);
            if (this.formItem) {
                var form = this.formItem.form,
                    mask = isc.EH.clickMaskRegistry.find("ID", cmID);
    
                // Suppress the default behavior of putting focus into whatever had focus before
                // this CM was shown. We'll explicitly put focus into the appropraite item when
                // hiding the pickList
                if (mask._maskedFocusCanvas) mask._maskedFocusCanvas = null;
            }
        }
    },

    getBaseStyle : function (record, rowNum, colNum, d,e,f) {
        var baseStyle = this.invokeSuper(isc.PickListMenu, "getBaseStyle", record, rowNum, colNum, d,e,f);
        // Because of pickList menu sharing, we won't know until runtime whether the pickList
        // menu is bound to a SelectItem.
        var formItem = this.formItem;
        if (isc.isA.SelectItem && isc.isA.SelectItem(formItem) && formItem.multiple &&
            !formItem._defaultPendingStatusChangedBehaviorCanceled && formItem._getShowPending())
        {
            var field = this.getField(colNum),
                fieldName = formItem.getValueFieldName();
            if (this.getField(fieldName) === field) {
                var valuesSet = formItem._valuesSet;
                if (this.isSelected(record)) {
                    if (!valuesSet.has(record[fieldName])) baseStyle += "Pending";
                } else {
                    if (formItem._getShowDeletions() && valuesSet.has(record[fieldName])) {
                        baseStyle += "Deselected";
                    }
                }
            }
        }
        return baseStyle;
    },

    _$_backgroundColor:"background-color:",
    _$_color:"color:",
    getCellCSSText : function (record, rowNum, colNum) {
        // if it's selected apply the hilite color, if specified
        // Otherwise we rely on regular css class type styling.
        if (this.selection != null && record == this.selection.getSelectedRecord()) {
            var cssText = [];
            if (this.hiliteColor != null) {
                cssText[0] = this._$_backgroundColor;
                cssText[1] = this.hiliteColor;
                cssText[2] = isc.semi;
            }

            if (this.hiliteTextColor != null) {
                cssText[3] = this._$_color;
                cssText[4] = this.hiliteTextColor;
                cssText[5] = isc.semi;
            }
            return cssText.join(isc.emptyString);
        }
    },

    // override keyDown to catch tabs and hide the pickList.
    
    
    keyDown : function () {
        var keyName = isc.EH.lastEvent.keyName;
        if (keyName == "Tab") {
            this.hide();
            return false;
        }
    },
            
    // Override _formatCellValue to call formatPickListValue() if defined (allowing for customized
    // formatting within the pickList).
    
    _formatCellValue : function (value, record, field, rowNum, colNum) {
        if (this.formItem == null) return this.Super("_formatCellValue", arguments);
        
        var fieldName = this.getFieldName(colNum);
        value = this.formItem.formatPickListValue(value, fieldName, record);
        
        return this.Super("_formatCellValue", [value,record,field,rowNum,colNum]);
    },
    
    // override keyPress to allow for navigation to different items by typing
    // the first letter of the option.
    bodyKeyPress : function (event, eventInfo) {
        var keyName = isc.EH.lastEvent.keyName;
        
        // Catch shift+tab in safari in keyPress rather than keydown
        if (isc.Browser.isSafari) {
            if (keyName == "Tab") {
                this.hide();
                return false;
            }
        }

        var charVal = isc.EH.getKeyEventCharacterValue();
        if (charVal != null) {

            var data = this.formItem.getAllLocalOptions();
                  
            if (isc.isAn.Array(data) && data.length > 1) {
    
                var typedChar = String.fromCharCode(charVal),
                    // Normalize to a lowercase string for comparison.
                    typedChar = typedChar.toLowerCase(),
                    formItem = this.formItem,
                    valueField = formItem.getValueFieldName(),
                    currentIndex = data.indexOf(this.getSelectedRecord()),
                    newIndex = currentIndex < (data.length -1) ? currentIndex + 1 : 0
                ;

                while (newIndex != currentIndex) {
                    if (currentIndex < 0) currentIndex = 0;

                    var value = data[newIndex][valueField];
                    value = formItem.mapValueToDisplay(value);
                    if (isc.isA.String(value) && value.length > 0 && 
                        value.charAt(0).toLowerCase() == typedChar) 
                    {
                            if (!formItem.multiple) this.deselectAllRecords();
                            var navStyle = this.getArrowKeyAction();
                            this.scrollRecordIntoView(newIndex);
                            if (navStyle == "focus") {
                                this._hiliteRecord(newIndex);
                            } else {
                                this.selectRecord(newIndex);
                            }
                            return;
                    }
                    newIndex += 1;
                    if (newIndex >= data.length) newIndex = 0;
                }
            }
        }

        // If the "Enter" key was pressed, but no record was selected, dismiss the menu
        // (this is really useful for the ComboBox item when showAllRecords is true and
        // the user has entered a value that isn't in the c-box).
        if (this.getFocusRow() == null && keyName == "Enter") {
            this.cancel();
            return false;
        }
        
        // The superclass implementation will handle cancelling on escape click / selecting
        // on enter click
        return this.Super("bodyKeyPress", arguments);
    },

    // Override dataChanged -- avoid redrawing to show temp. loading rows - wait
    // for the rows to come back from the server instead.
    
    dataChanged : function (operation, record, row, lastUpdateData) {
        var data = this.data;
        if (!data) return;
        
        var data = this.requestVisibleRows();
        if (data && Array.isLoading(data[0])) {
            // this.logWarn("not redrawing since data still loading");
            return;
        }
        this.Super("dataChanged", arguments);
        
        // If the currently selected record changed, we should refresh our value to pick up the
        // change.
        var formItem = this.formItem;
        if (record && this.getSelectedRecord() == record && formItem) {
            var index = this.data.indexOf(record),
                modifiedRecord = index == -1 ? null : this.data.get(index);
            if (modifiedRecord) {
                var fieldName = formItem.getValueFieldName();
                formItem.setValue(modifiedRecord[fieldName]);
            } else {
                formItem.clearValue();
            }
        }
    },
                                
    
    createBodies : function () {
        if (this.body && this.body._reused) delete this.body._reused;
        this.Super("createBodies", arguments);
    }
});

isc.PickListMenu.changeDefaults("bodyDefaults", {

    // Override getCellStyleName() - historically we'd simply select cells to indicate
    // highlighting - no separate over styling required.
    // Now we use standard ListGrid hilighting APIs, so there is a separate Over vs
    // Selected state for cells. However older skins will commonly not have the "Over"
    // styles in place. Therefore we have a flag "showOverAsSelected" which will 
    // show the same styling for hilighted "Over" cells as for "Selected" cells.
    remapOverStyles:[
            0, // 0 = baseStyle
            2, // 1 = Over(1) --> "Selected"
            2, // 2 = Selected(2)
            2, // 3 = Selected(2) + Over(1) --> "Selected"
            4, // 4 = Disabled(4)
            6, // 5 = Disabled(4) + Over(1) --> "Disabled + Selected"
            6, // 6 = Disabled(4) + Selected(2)
            6, // 7 = Disabled(4) + Selected(2) + Over(1) --> "Disabled + Selected"
            8, // 8 = Dark(8)
            10, // 9 = Dark(8) + Over(1) --> "Dark + Selected"
            10, // 10 = Dark(8) + Selected(2)
            10, // 11 = Dark(8) + Selected(2) + Over(1) --> "Dark + Selected"
            12 // 12 = Dark(8) + Disabled(4)
    ],
    getCellStyleName : function (styleIndex, record, rowNum, colNum) {
        if (this.grid && this.grid.showOverAsSelected) {
            styleIndex = this.remapOverStyles[styleIndex];
        }
        return this.Super("getCellStyleName", [styleIndex, record, rowNum, colNum], arguments);
    }
});


//> @type PanelPlacement
// Possible placements for pop-up choosers, menus, dialogs or other temporary UIs,
// which may need to expand to take up additional room for smaller screens.
// @value "nearOrigin" classic placement for menus, pop-up lists and pickers in desktop
//                     interfaces: near the control that was clicked (a search field,
//                     +link{MenuButton}, etc).  Note: this setting does not apply when there is
//                     no originating control (such as a dialog that appears due to session
//                     timeout), in which case centering will generally be used
// @value "fillPanel" fill the nearest containing panel managed by a device-aware layout such as
//                    +link{SplitPane}, which will generally be equivalent to "fillScreen" for a
//                    +link{Browser.isHandset,handset-sized device}.  Note: this setting does not
//                    apply if there is no clear container for the component originating the UI,
//                    in which case, "fillScreen" will generally be used.
// @value "fillScreen" fill the entire screen
// @value "halfScreen" fill the bottom half of the screen.  This is the default behavior on iOS6/7
//                     for plain HTML &lt;select&gt;, but note that native apps rarely use
//                     this interface for picking from lists and it is not generally recommended.
// @visibility external
//<




isc.PickList.addInterfaceProperties({
    
    //> @attr PickList.pickListHeight (number : 300 : IRW)
    // Maximum height to show the pick list before it starts to scroll.
    // Note that by default the pickList will be sized to the height required by its content
    // so it will be taller when more rows are available as selectable options
    // @visibility external
    //<
    pickListHeight:300,
    
    //> @attr PickList.emptyPickListHeight (number : 100 : IRW)
    // Height for an empty pick list (showing the empty message), if the pick list has no
    // records and +link{PickList.hideEmptyPickList} is <code>false</code>.
    // @visibility external
    //<
    emptyPickListHeight:100,
    
    //> @attr PickList.emptyPickListMessage (string : "No items to show" : IRWA)
    // Empty message to display in the pickList if 
    // +link{PickList.hideEmptyPickList} is <code>false</code>.
    // @group i18nMessages
    // @visibility external
    //<
    emptyPickListMessage:"No items to show",
    
    //> @attr PickList.hideEmptyPickList (boolean : null : IRW)
    // If this pickList contains no options, should it be hidden?
    // If unset, default behavior is to allow the empty pickList to show if it is databound.
    // @visibility external
    //<
    //hideEmptyPickList:null,

    //> @attr PickList.pickListWidth (number : null : IRW)
    // Default width to show the pickList.
    // If not specified, the width of this form item's element will be used instead.
    // <P>
    // Note that this is a minimum value - by default if the values displayed in this pickList
    // are wider than the specified width the list will expand to accomodate them.
    // @visibility external
    // @example listComboBox
    //<
    //pickListWidth : null,
    
    //> @attr PickList.pickListMaxWidth (number : 400 : IRW)
    // Maximum width for this item's pickList.
    // By default if the values displayed in this pickList are wider than the specified
    // +link{pickList.pickListWidth} the pickList will render wide enough to accomodate them.
    // This property allows the developer to limit how wide the pickList will render.
    // @visibility external
    //<
    pickListMaxWidth:400,

    // Styling.
    // Users must be able to style pickLists

    //> @attr pickList.pickListBaseStyle (CSSStyleName : "pickListCell" : IR)
    // Base Style for pickList cells.  See +link{group:cellStyleSuffixes} for details on how
    // stateful suffixes are combined with the pickListBaseStyle to generate stateful cell styles.
    // <p>
    // Note: if +link{pickListTallBaseStyle} is specified, this property will be used as the
    // +link{listGrid.normalBaseStyle,normalBaseStyle} and that property will be applied
    // to cells that do not match the default cell height, or if +link{listGrid.fastCellUpdates}
    // is true for the pickList in Internet Explorer.
    //
    // @visibility external
    //<
    pickListBaseStyle: "pickListCell",

    //> @attr pickList.pickListTallBaseStyle (CSSStyleName : null : IR)
    // Optional +link{listGrid.tallBaseStyle,tallBaseStyle} for pickList cells. If unset,
    // +link{pickListBaseStyle} will be applied to all cells.
    //
    // @visibility external
    //<
    //pickListTallBaseStyle: null,

    //> @attr pickList.pickListApplyRowNumberStyle (boolean : false : IRWA)
    // Default value for +link{listGrid.applyRowNumberStyle} for this item's generated
    // pickList.
    // @visibility external
    //<
    
    pickListApplyRowNumberStyle:false,



    //> @attr    PickList.pickListHiliteColor  (string : null : IR)
    // If specified this color will be applied to hilited rows in the pickList, when the
    // user rolls over them.  This color will be applied on top of the "over" CSS Style 
    // specified by <code>pickListBaseStyle</code>
    //<
    //pickListHiliteColor:null,
    
    //> @attr    PickList.pickListHiliteTextColor  (string : null : IR)
    // If specified this color will be applied to the text of hilited rows in the pickList, 
    // when the user rolls over them.  This color will be applied on top of the "over" CSS Style 
    // specified by <code>pickListBaseStyle</code>
    //<
    //pickListHiliteTextColor:null,

    // autoSizePickList
    // If true the pickList will expand horizontally to accommodate its widest item.
    // Not supported for databound lists (since the items load incrementally)
    //autoSizePickList : false,

    //>Animation 
    //> @attr PickList.animatePickList (boolean : false : IRWA)
    // If true, when the pickList is shown, it will be shown via an animated reveal effect
    // @visibility animation
    //<
    animatePickList: null, // isc.Browser.isHandset || isc.Browser.isTablet
                           

    //> @attr PickList.animationTime (number : 200 : IRWA)
    // If this.animatePickList is true - this specifies the duration of the animation effect
    // applied when showing the pickList
    // @visibility animation
    //<
    pickListAnimationTime:200,
    //<Animation

    //> @attr PickList.fetchDelay (number : 200 : IRWA)
    // For a ComboBox or other pickList that accepts user-entered criteria, how many
    // milliseconds to wait after the last user keystroke before fetching data from the server.
    // The default setting will initiate a fetch if the user stops typing or pauses briefly.
    //
    // @visibility external
    //<
    fetchDelay:200,
    
    //> @attr PickList.pickListCellHeight (number : 16 : IRW)
    // Cell Height for this item's pickList.
	// @visibility external
    //<
    pickListCellHeight:16,
    
    //> @attr PickList.pickListProperties (ListGrid Properties : null : IR)
    // If specified this properties block will be applied to the +link{PickListMenu,pickList}
    // created for this FormItem.
    // <P>
    // <i>Note</i>: Not every ListGrid property is supported when assigned to a pickList.
    // Where there is a dedicated API on the form item (such as +link{PickList.pickListCellHeight,pickListCellHeight}),
    // we recommend that be used in favor of setting the equivalent property on the 
    // <code>pickListProperties</code> block.
    // <P>
    // <i>PickLists and +link{listGrid.showFilterEditor}:</i>
    // +link{ComboBoxItem,ComboBoxItems} do not support setting <code>showFilterEditor</code> 
    // to true on pickListProperties. This combination of settings leads to an ambiguous user
    // exprience as the two sets of filter-criteria (those from the text-box and those from the 
    // pickList filter editor) interact with each other.<br>
    // +link{SelectItem.pickListProperties}property.
    // is a valid way to create a filterable pickList, on a SelectItem, but
    // this setting is not supported on a SelectItem with +link{selectItem.multiple} set to
    // true - this combination of settings can cause a selected value to be filtered out of
    // view at which point further selection changes will discard that value.<br>
    // In general
    // we recommend the ComboBoxItem class (with +link{comboBoxItem.addUnknownValues} set
    // as appropriate) as a better interface for filtering pickList data.
    //
    // @visibility external
    //<
    
    //pickListProperties : null,
    
    //> @attr PickList.pickListHeaderHeight (number : 22 : IRW) 
    // If this pick list is showing multiple fields, this property determines the height of
    // the column headers for those fields. Set to zero to suppress the headers entirely.
    // @see pickListFields
    // @visibility external
    //<
    pickListHeaderHeight:22,

    
    allowMultiSelect: true,

    //> @attr pickList.separateValuesList (AutoChild ListGrid : null : IR)
    // The +link{ListGrid} shown at the top of the pickList menu when +link{PickList.specialValues}
    // are enabled.
    //<
    separateValuesListDefaults: {
        showHeader: false,
        autoFitData: "vertical",
        height: 1,

        _constructor: isc.ListGrid,
        width:"100%",
        
        // the scrollbar gap introduces a styling artefact - row over selection is cut short by
        // the scrollbar gap - but for this list, we almost always show all values, so disable
        // the gap
        leaveScrollbarGap: false,
        alternateRecordStyles: false,
        className:"scrollingMenu",
        styleName:"pickListMenu",
        bodyStyleName:"pickListMenuBody",
        normalCellHeight:16,
        // showOverAsSelected - will remap all "Over" styles to use the
        // "Selected" class name.
        
        showOverAsSelected:true,
        bodyDefaults: {
            remapOverStyles:[
                     0, // 0 = baseStyle
                     2, // 1 = Over(1) --> "Selected"
                     2, // 2 = Selected(2)
                     2, // 3 = Selected(2) + Over(1) --> "Selected"
                     4, // 4 = Disabled(4)
                     6, // 5 = Disabled(4) + Over(1) --> "Disabled + Selected"
                     6, // 6 = Disabled(4) + Selected(2)
                     6, // 7 = Disabled(4) + Selected(2) + Over(1) --> "Disabled + Selected"
                     8, // 8 = Dark(8)
                     10, // 9 = Dark(8) + Over(1) --> "Dark + Selected"
                     10, // 10 = Dark(8) + Selected(2)
                     10, // 11 = Dark(8) + Selected(2) + Over(1) --> "Dark + Selected"
                     12 // 12 = Dark(8) + Disabled(4)
             ],
             getCellStyleName : function (styleIndex, record, rowNum, colNum) {
                 if (this.grid && this.grid.showOverAsSelected) {
                     styleIndex = this.remapOverStyles[styleIndex];
                 }
                 return this.Super("getCellStyleName", [styleIndex, record, rowNum, colNum], arguments);
             }
        },
        recordClick : function (viewer,record,recordNum,field,fieldNum,value,rawValue) {
            // Skip 'canSelect:false' records. Don't hide the viewer or fire the itemClick handler
            if (record[viewer.recordCanSelectProperty] == false) return;
            
            // hide before firing itemClick.
            // This avoids issues with focus, where the itemClick action is expected to put focus
            // somewhere that is masked until this menu hides.
            viewer.pickList.hide();
            // itemClick handles updating the formItem with the selected value(s) from the
            // pickList.
            if (record != null) viewer.pickList.itemClick(record);
        },
        rowOver : function (record,rowNum,colNum) {
            // kill annoying artefacting: when you roll from the pickList to the
            // separateValuesList, the row over effect appears to stick on both the topmost
            // pickList record and the currently-over separateValuesList record.  
            //
            // This is because the pickList actually selects records on rollOver by default
            // (enableSelectOnRowOver) and so you're seeing selection persist vs a rowOver
            // effect on the separateValuesList.  But the end effect is that you don't know
            // which record you're selecting (because two are highlighted in exactly the same
            // fashion).
            if (this.pickList.enableSelectOnRowOver) this.pickList.deselectAllRecords();
        }
    },

    fillScreenContainerDefaults: {
        width: "100%",
        height: "100%",
        overflow: "hidden",
        autoDraw: false
    },

    fillScreenContainerConstructor: "Layout",
    
    
    // --------------------------------------------------------------------------------------
    // Data / databinding
    
    //> @attr    PickList.valueField  (string : null : IRA)
    // @include FormItem.valueField
    //<

    //> @attr   PickList.displayField   (string : null : IRW)
    // @include FormItem.displayField
    //<


    //> @attr PickList.pickListConstructor (SCClassName : "PickListMenu" : IR)
    // The Class to use when creating the pickList for a FormItem.  Must be a subclass of the
    // builtin default, +link{class:PickListMenu, PickListMenu}.
    // @visibility external
    //<
    pickListConstructor: "PickListMenu",

    getPickListClass : function () {
        if (isc.isA.ClassObject(this.pickListConstructor)) {
            return this.pickListConstructor;
        } else if (isc.isA.String(this.pickListConstructor)) {
            var plc;
            plc = isc.ClassFactory.getClass(this.pickListConstructor);
            if (!isc.isAn.Object(plc)) plc = isc.PickListMenu;
            return plc;
        }
    }

    //> @attr PickList.pickListFields (Array of ListGridField : null : IRA)
    // This property allows the developer to specify which field[s] will be displayed in the 
    // drop down list of options.
    // <P>
    // Only applies to databound pickLists (see +link{PickList.optionDataSource}, or pickLists
    // with custom data set up via the advanced +link{pickList.getClientPickListData()} method.
    // <P>
    // If this property is unset, we display the +link{PickList.displayField}, if specified, 
    // otherwise the +link{PickList.valueField}.
    // <P>
    // If there are multiple fields, column headers will be shown for each field, the
    // height of which can be customized via the +link{pickList.pickListHeaderHeight} attribute.
    // <P>
    // Each field to display should be specified as a +link{ListGridField} object. Note that
    // unlike in +link{ListGrid,listGrids}, dataSource fields marked as 
    // +link{DataSourceField.hidden,hidden:true} will be hidden by default in pickLists. To
    // override this behavior, ensure that you specify an explicit value for 
    // +link{ListGridField.showIf,showIf}.
    // 
    //  @see valueField
    //  @see pickList.pickListHeaderHeight
    //  @visibility external
    //<
    
    //> @attr   PickList.valueIconField (string : null : IRWA) 
    // For Databound formItems, this property determines which column 
    // +link{formItem.valueIcons} should show up in for this formItem's pickList.<br>
    // If unset, valueIcons show up in the +link{pickList.displayField} column if specified, 
    // otherwise the +link{pickList.valueField} column.<br>
    // In most cases only the <code>displayField</code> or <code>valueField</code> will be visible.
    // This property is typically only required if custom +link{PickList.pickListFields} 
    // have been specified for this item. 
    // @see FormItem.valueIcons
    // @see PickList.pickListFields
    // @visibility external
    //<
    
    //> @attr   PickList.pickListCriteria (Criteria : null : IRWA)
    // If this item has a databound pickList (for example +link{PickList.optionDataSource} is
    // set), this property can be used to provide static filter criteria when retrieving the data
    // for the pickList.
    // @visibility external
    //<
    
    //> @attr PickList.optionDataSource        (DataSource | String : null : IRA)
	// If set, this FormItem will derive data to show in the PickList by fetching records from
    // the specified <code>optionDataSource</code>.  The fetched data will be used as a
    // +link{formItem.valueMap,valueMap} by extracting the
    // +link{formItem.valueField,valueField} and +link{formItem.displayField,displayField} in
    // the loaded records, to derive one valueMap entry per record loaded from the
    // optionDataSource.  Multiple fields from the fetched data may be shown in the pickList by
    // setting +link{pickListFields}.
    // <P>
    // The data will be retrieved via a "fetch" operation on the DataSource, passing the 
    // +link{PickList.pickListCriteria} (if set) as criteria, and passing
    // +link{optionFilterContext} (if set) as DSRequest Properties.
    // <P>
    // The fetch will be triggered when the pickList is first shown, or, you can set
    // +link{SelectItem.autoFetchData,autoFetchData:true} to fetch when the FormItem is
    // first drawn.  
    // <P>
    // Note that providing an initial value when
    // +link{FormItem.fetchMissingValues,fetchMissingValues} is enabled, or enabling
    // +link{SelectItem.defaultToFirstOption,defaultToFirstOption}, can also cause a fetch to
    // be initiated immediately upon form creation.  You can also call +link{PickList.fetchData()}
    // at any time to manually trigger a fetch.
    // <P>
    // Data paging is automatically enabled if the optionDataSource supports it.  As the
    // pickList is scrolled by the user, requests for additional data will be automatically
    // issued.
    // <P>
    // For a pickList attached to a +link{class:ComboBoxItem, ComboBoxItem}, new fetches are 
    // issued as the user types, with criteria set as described under 
    // +link{comboBoxItem.getPickListFilterCriteria()}.
    // If your dataSource is not capable of filtering results by search criteria (eg, the
    // dataSource is backed by an XML flat file), you can set +link{filterLocally} to have the
    // entire dataset loaded up front and filtering performed in the browser.  This disables
    // data paging.
    // <P>
    // Note that if a normal, static +link{formItem.valueMap,valueMap} is <b>also</b> specified
    // for the field (either directly in the form item or as part of the field definition in
    // the dataSource), it will be preferred to the data derived from the optionDataSource for
    // whatever mappings are present.
    //
    // @visibility external
    //<

    //> @attr PickList.showOptionsFromDataSource  (boolean : null : IRWA)
    // If this item is part of a databound form, and has a specified <code>valueMap</code>,
    // by default we show the valueMap options in the pickList for the item.
    // Setting this property to true will ensure that the options displayed in our pickList
    // are derived from the form's <code>dataSource</code>.
    // @group databinding
    // @visibility external
    //<
    

    //> @attr PickList.fetchDisplayedFieldsOnly     (boolean: null : IRA)
    // If this item has a specified <code>optionDataSource</code> and this property is
    // <code>true</code>, the list of fields used by this pickList will be passed to
    // the datasource as +link{dsRequest.outputs}. If the datasource supports this feature
    // the returned fields will be limited to this list. A custom datasource will need to
    // add code to implement field limiting.
    // <P>
    // This list of used fields consists of the values of +link{formItem.valueField,valueField},
    // +link{formItem.displayField,displayField} and +link{pickListFields,pickListFields}.
    // <P>
    // NOTE: When enabled, +link{formItem.getSelectedRecord,getSelectedRecord} will only include the
    // fetched fields.
    // @visibility external
    //<

    //> @attr PickList.optionFilterContext     (DSRequest Properties : null : IRA)
    // If this item has a specified <code>optionDataSource</code>, and this property is not
    // null, this will be passed to the datasource as +link{dsRequest} properties when
    // performing the filter operation on the dataSource to obtain the set of options for the
    // list.  This provides, among other capabilities, a way to trigger the server to return
    // summary records.
    // @see group:serverSummaries
    // @visibility external
    //<
    // This gives the developer the option of specifying (for example) an operation name.

    //> @attr PickList.filterLocally (Boolean : false : IRA) 
    // If <code>filterLocally</code> is set for this item, and this item is showing options 
    // from a dataSource, fetch the entire set of options from the server, and use these values
    // to map the item value to the appropriate display value. Also use <code>"local"</code>
    // type filtering on drop down list of options.
    // <P>
    // This means data will only be fetched once from the server, and then filtered on the
    // client.
    // <P>
    // Note - when this property is set to <code>false</code>, filtering will still be 
    // performed on the client if a complete set of data for some criteria has been cached
    // by a fetch, and a subsequent fetch has more restrictive criteria. To explicitly
    // disable client-side filtering set the +link{useClientFiltering} property to false.
    //
    // @see FormItem.filterLocally
    // @visibility external
    //<

    //> @attr PickList.filterDisplayValue (boolean : null : IRA)
    // When performing the filter on the data displayed in the pickList, for valueMapped fields,
    // should the filter criteria be compared to the raw data values in the source list, or the
    // values mapped to display.
    // <P>
    // This setting only has an effect on non-databound pick lists.
    //
    // @visibility internal
    //<
    

    //> @attr pickList.sortField (String | Array of String | int : null : IR)
    // Specifies one or more fields by which this item should be initially sorted.  It can be 
    // a +link{listGridField.name,field name}, or an array of field names - but note that, if
    // multiple fields are supplied, then each will be sorted in the same 
    // +link{listGrid.sortDirection, direction}.
    // <P>
    // For full sorting control, set +link{pickList.initialSort, initialSort} to a list of
    // custom +link{SortSpecifier, sortSpecifiers}.
    // <P>
    // This attribute can also be set to the index of a field in the fields array, but note 
    // that it will be converted to a string (field name) after initialization.
    //
    // @group sorting
    // @example sort
    // @visibility external
    //<

    //> @attr pickList.initialSort (Array of SortSpecifier : null : IR)
    // An array of +link{SortSpecifier} objects used to set up the initial sort configuration 
    // for this pickList. If specified, this will be used instead of any 
    // +link{pickList.sortField} specified.
    // @group sorting
    // @visibility external
    //<

});


isc.PickList.addInterfaceMethods({

    // showPickList
    // API to be called by the form item when the pickList is to be shown
    // Note that positioning of the pickList should be set up by modifying 
    // getPickListPosition().
    // If 'waitForData' is passed, don't show the list until the filter completes.
    
    showPickList : function (waitForData, queueFetches) {
    
        // Set a flag to note that we've started to show the pickList.
        
        this._showingPickList = true;
        
        
        
        var pickListChangedItem = !this.pickList || (this.pickList.formItem != this);

        // Only pass in the param to show when the filter is complete if we're waiting for
        // data - otherwise we'll show the pickList explicitly (below).
        if (!this.pickList) this.makePickList(waitForData, null, queueFetches);
        // This will ensure the pickList is associated with this form item and set up its
        // data and fields.        
        
        else this.setUpPickList(waitForData, queueFetches);

        // call _showPickList to actually show the pickList.
        
        
        if (!waitForData && (pickListChangedItem || !this._isPickListVisible())) {
            this._showPickList();
        }

                
    },
    
    // Actually show the pick list. on the page.
    _showPickList : function () {
        // don't show the PL if it's got no data
        
        // also don't show if we're not drawn - this can happen if we're showing in response
        // to an asynch event like fetching data.
        var list = this.pickList;
        if (!this.isDrawn() || (this.shouldHideEmptyPickList() && list.getTotalRows() < 1)) {
            return;
        }

        // size and place the picklist
        this.placePickList();

        if (!this._isPickListVisible()) {
            if (this.separateSpecialValues) {
                if (this.separateValuesList.data && this.separateValuesList.data.length > 0) {
                    this._setSeparateValuesListVisibility(true);
                } else if (this.separateValuesList.isVisible()) {
                    this._setSeparateValuesListVisibility(false);
                }
            } else {
                this._setSeparateValuesListVisibility(false);
            }
            //>Animation
            // Show, or animate-show
            if (this.animatePickList) {
                if (list._fillScreenContainer) {
                    list._animateShow();
                } else {
                    list.bringToFront();
                    list.animateShow("wipe", null, this.pickListAnimationTime);                    
                }
            } else    //<Animation 
                list.show();
        }
    },
    
    _setSeparateValuesListVisibility : function (show) {
        var list = this.pickList,
            svList = this.separateValuesList,
            parent = list.children.find("_isSeparateValuesListParent", true);
        
        // Sanity check only:
        // We always build the separateValuesList, and always inject the container
        // into the gridComponents of the pickList, so these widgets should exist.
        if (parent == null) {
            this.logWarn("Unable to find container for 'separateValuesList'");
            return;
        }
        if (svList == null) {
            this.logWarn("SeparateValues list not defined for this item");
        }
        if (show) {
            if (!parent.contains(svList)) {
                if (parent.children.length > 0) {
                    parent.children[0].deparent();
                }
                parent.addChild(svList);
            }
            parent.setVisibility("inherit");
        } else {
            parent.setVisibility("hidden");
        }
        
    },

    setSeparateSpecialValues : function (specialValues) {
        this.separateValuesList.setData(specialValues);
        if (this._isPickListVisible()) {
            this._setSeparateValuesListVisibility(true);
        }
    },

    //>@method  PickList.fetchData() 
    // Only applies to databound items (see +link{PickList.optionDataSource}).<br>
    // Performs a fetch type operation on this item's DataSource to retrieve the set of valid
    // options for the item, based on the current +link{PickList.pickListCriteria}.
    // @param [callback] (DSCallback) Callback to fire when the fetch completes. Callback will 
    //              fire with 4 parameters:<ul>
    //  <li><code>item</code> a pointer to the form item
    //  <li><code>dsResponse</code> the +link{dsResponse} returned by the server
    //  <li><code>data</code> the raw data returned by the server
    //  <li><code>dsRequest</code> the +link{dsRequest} sent to the server
    //  </ul> 
    // @param [requestProperties] (DSRequest properties) properties to apply to the
    //              dsRequest for this fetch.
    // @visibility external
    //<                         
    // @param maintainCache (boolean) By default when this method is called we drop any
    //            cached rows and re-fetch. Pass in this parameter to suppress this behavior.
    //            Note that if the fetch operation does not hit the server (which will occur
    //            if the data is already cached), the callback will not fire
    fetchData : function (callback, requestProperties, maintainCache) {
        if (this.getOptionDataSource() == null) {
            this.logWarn("fetchData() called for a non-databound pickList. Ignoring");
            return;
        }

        // Store the callback passed in on the request's internalClientContext object.
        // This will be picked up as part of filterComplete()
        if (requestProperties == null) requestProperties = {};
        
        if (callback != null) {
            requestProperties.internalClientContext = {
                _callback: callback
            };
        }
        // add componentContext for developer console rpc history tab
        
        requestProperties.componentContext = this.form.ID + "." + this.name;
        if (!this.pickList) {
            this.makePickList(false, requestProperties, false, true);
        } else {
            this.setUpPickList(false, false, requestProperties, !maintainCache);
        }
    },
    
    mapValueToDisplay : function (internalValue, a, b, c) {
        if (this.isSelectOther) {
            if (internalValue == this.otherValue) return this.otherTitle;
            if (internalValue == this.separatorValue) return this.separatorTitle;
        }

        return this.invokeSuper(isc.SelectItem, "mapValueToDisplay", internalValue,a, b, c);
	},
	
    //> @attr pickList.useClientFiltering (Boolean : null : IRA)
    // For +link{optionDataSource,databound} items, this property will be passed
    // to the generated ResultSet data object for the pickList as +link{resultSet.useClientFiltering}.
    // Setting to false will disable filtering on the client and ensure criteria are
    // always passed to the DataSource directly.
    // @visibility external
    //<

	
    // Create the pickList for this widget.
    
    makePickList : function (show, request, queueFetches, dropCache) {
        //>DEBUG
        var startTime = isc.timeStamp();
        //<DEBUG
        
        if (!this.separateValuesList) {
            var fields = [];
            if (this.displayField) fields.add({ name: this.displayField });
            else if (this.valueField) fields.add({ name: this.valueField });
            else fields.add({ name: this.name });

            this.separateValuesList = this.createAutoChild("separateValuesList",
                    { fields: fields,
                        formItem: this,
                        showModal: false,
                        normalBaseStyle:this.pickListBaseStyle,
                        // If a tall style is defined use it, otherwise assume the normal version
                        // can stretch
                        tallBaseStyle:(this.pickListTallBaseStyle || this.pickListBaseStyle),
        
                        hiliteColor:this.pickListHiliteColor,
                        hiliteTextColor:this.pickListHiliteTextColor
                    }
                );
        }

        // Wherever possible, we reuse pickList menus across multiple items
        // If explicit pickListProperties have been specified for this item, use a unique
        // pickList instead - this avoids us having to correctly apply (and then clear) freeform 
        // properties to a shared pickList object.
        // If we're databinding we cache pickLists on a per-dataSource basis.
        // If the dataSource and the specified set of fields match, we reuse the list - otherwise
        // create a new one (rather than re-binding)
        var reusePickList = this.reusePickList();
        if (reusePickList) {
            // only reuse the pickList if it has the same className as the stored one
            var pl = this.getSharedPickList();
            if (pl && pl.Class == this.getPickListClass().Class) this.pickList = pl;
        }

        // We have to build a new pickList if the global one doesn't exist, or if we're 
        // not sharing pickLists.
        var reusingPL = this.pickList != null;
        if (!this.pickList) {
            // The pickList is a pickListMenu - subclass of ScrollingMenu with some 
            // overrides specific to form item pickLists.
            // The pattern we will use is to set the pickList up here, then override the
            // properties to allow us to reuse the list for other 

            // Determine desired properties from the various init params.
            var pickListProperties = this.pickListProperties || {};

            if (this.multiple) {
                // if we're a multiple: true pickList and noDoubleClicks isn't specified,
                // switch it ON now
                if (pickListProperties.noDoubleClicks == null) {
                    pickListProperties.noDoubleClicks = true;
                }
            } else {
                // if we're a multiple: false pickList and noDoubleClicks isn't specified,
                // switch it OFF now
                if (pickListProperties.noDoubleClicks == null) {
                    pickListProperties.noDoubleClicks = false;
                }
            }

            

            
            var pickerNavigationBarParent = isc.Canvas.create({
                autoDraw:false,
                height:1,
                overflow:"visible"
            });
            if (this.hasPopOutPicker()) {
                if (!this.pickerNavigationBar) this.createPickerNavigationBar();
                pickerNavigationBarParent.addChild(this.pickerNavigationBar);
            } else {
                pickerNavigationBarParent.visibility = "hidden";
            }
            var separateValuesListParent = isc.Canvas.create({
                _isSeparateValuesListParent:true,
                autoDraw:false,
                height:1,
                overflow:"visible",
                visibility:"hidden"
            });
            separateValuesListParent.addChild(this.separateValuesList);

            var gridComponents = [
                pickerNavigationBarParent,
                separateValuesListParent,
                "filterEditor",
                "header",
                "body",
                "summaryRow"
            ];


            this.pickList = this.getPickListClass().create(
                // no need to set up showPickList - this is done with setFields
                { headerHeight:this.pickListHeaderHeight,
                    gridComponents: gridComponents
                },
                pickListProperties
            );
            this.pickList.pickerNavigationBarParent = pickerNavigationBarParent;

            // apply local fetchMode if 'filterLocally' was explicitly specified.
            var data = this.pickList.dataProperties || {};
            if (this.filterLocally) data.fetchMode = "local";
            if (this.useClientFiltering != null) {
                data.useClientFiltering = this.useClientFiltering;
            }
            
            // Override fetchRemoteDataReply
            // For shared pickLists we sometimes get into a state where a fetch
            // request will be issued to get data for some item, and then 
            // while that is still pending, a new item needs to issue the same
            // fetch request.
            // Obviously we can't get at the callback passed to the fetch request,
            // so use flags on the data object to indicate this state, and 
            // when it occurs notify the new item.
            
            data.fetchRemoteDataReply = function (dsResponse, data, request) {
                this.Super("fetchRemoteDataReply", arguments);
                // If necessary notify the latest item we've been assigned to that
                // fresh data has arrived.
                
                if (this._notifyNewItemOnFetchComplete) {
                    isc.PickList._sharedPickListFetchComplete(this, dsResponse, data, request);
                }
            }

            this.pickList.dataProperties = data;
            
            // If this is a shared pickList, store it in a publically accessible place
            if (reusePickList) this.storeSharedPickList();
        
        } 
        // Note - no need to attempt to customize a re-used pickList 
        // here - that's handled in 'setupPickList()'

        this.separateValuesList.pickList = this.pickList;
        
        // If limiting the fetch fields, build the correct optionFilterContext.outputs
        if (this.fetchDisplayedFieldsOnly && this.optionDataSource &&
            (!this.optionFilterContext || !this.optionFilterContext.outputs))
        {
            var fields = this.pickListFields || [];
            if (this.valueField) fields.add(this.valueField);
            if (this.displayField) fields.add(this.displayField);
            if (fields.length > 0) {
                if (!this.optionFilterContext) this.optionFilterContext = {};
				var fieldNames = [];
				for(var i=0;i<fields.length;i++) {
 					if(isc.isA.String(fields[i])) {
					 	fieldNames.add(fields[i]);
					 } else {
					 	fieldNames.add(fields[i].name);
					 }
				}
				this.optionFilterContext.outputs = fieldNames.getUniqueItems().join(',');
            }
        }

        // fire 'setUpPickList' to set up the specific properties relevant to this form item
        this.setUpPickList(show, queueFetches, request, dropCache);
        
        //>DEBUG
        if (this.logIsInfoEnabled("timing"))
            this.logInfo("Time to initially create pickList:" + (isc.timeStamp() - startTime), "timing");
        //<DEBUG
    },
    
    //>@attr pickList.cachePickListResults (boolean : true : IR)
    // For databound pickLists (see +link{pickList.optionDataSource}), by default SmartClient
    // will cache and re-use datasets shown by pickLists in an LRU (least recently used) caching
    // pattern.
    // <P>
    // Setting this flag to false avoids this caching for situations where it is too aggressive.
    // @visibility external
    //<
    // Note: if true, this actually uses a central, shared picklist rather than just caching the
    // results
    cachePickListResults:true,
    
    // Can this item use a cached pickList menu instance?
    reusePickList : function () {    
        // we can reuse the pickList if the pickListProperties are null
        // For databound pickLists we create a cache of pickLists to reuse based on the
        // datasource ID and pickList fields
        // For client-only pickLists we have a single central reusable pickList.
        return this.pickListProperties == null && this.cachePickListResults;
    },

    // Retrieves the cached pickList menu for an item
    getSharedPickList : function () {
        if (this._getOptionsFromDataSource()) {
            
            // Store pickList menus for databound pickLists on a per DS basis.
            // cache of lists looks like this
            // isc._cachedDSPickLists = {
            //  dataSourceID:[
            //      {_pickList:..., +fields:...}
            //  ]
            // }
                       
            var ds = this.getOptionDataSource().getID(),
                cachedLists = isc.PickListMenu._cachedDSPickLists[ds];                
            if (cachedLists) {
                for (var i = 0; i < cachedLists.length;i++) {
                    if (cachedLists[i]._fields == this.pickListFields) {
                        // Note when we last used the pickList so we can
                        // destroy on a leastRecentlyUsed basis
                        cachedLists[i]._lastAccess = isc.timeStamp();
                        
                        var list = cachedLists[i]._pickList;
                        // Lazily catch the case where the list was destroyed by
                        // external code.
                        if (list.destroyed) {
                            cachedLists.removeAt(i);
                            this._clearSharedPickListItems(list);
                            i--;
                            continue;
                        }
                        
                        return list;
                    }
                }
            }
            return null;    
        } else {
            // As in the dataSource case, catch the case where somehow the shared pickList
            // got externally destroyed.
            if (isc.PickList._pickListInstance && isc.PickList._pickListInstance.destroyed) {
                this._clearSharedPickListItems(isc.PickList._pickListInstance);
                isc.PickList._pickListInstance = null;
                return null;
            }

            return isc.PickList._pickListInstance;
        }
    }, 
    _clearSharedPickListItems : function (pickList) {
        if (pickList._formItems != null) {
            for (var formItem in pickList._formItems) {
                if (window[formItem] && window[formItem].pickList == pickList) {
                    delete window[formItem].pickList;
                }
            }
        }
    },
    
    storeSharedPickList : function () {
        if (this._getOptionsFromDataSource()) {
            
            var ds = this.getOptionDataSource().getID(),
                cachedLists = isc.PickListMenu._cachedDSPickLists
            ;
            if (!cachedLists[ds]) cachedLists[ds] = [];

            var newMenu = {_pickList:this.pickList, _fields:this.pickListFields,
                                 _lastAccess:isc.timeStamp()}
            cachedLists[ds].add(newMenu);
             if (isc.PickListMenu._DSPickListCacheSize == null) {
                 isc.PickListMenu._DSPickListCacheSize = 1;
             } else {
                 isc.PickListMenu._DSPickListCacheSize += 1;
                 if (isc.PickListMenu._DSPickListCacheSize > isc.PickListMenu.pickListCacheLimit) {
                     // If we've exceeded our pickListCacheLimit, destroy a pickList to make room
                     // for the new one.
                     // We store last access timestamps on each pickList we create so we
                     // can destroy them in a least-recently-used order.
                      
                     var oldMenu, ts = isc.timeStamp();
                     for (var ds in cachedLists) {
                         var dsLists = cachedLists[ds];
                         for (var i = 0; i < dsLists.length; i++) {
                             var entry = dsLists[i];
                             if (entry._lastAccess <= ts && (entry != newMenu)) {
                                 oldMenu = entry;
                                 ts = entry._lastAccess;
                             }
                         }
                     }
                     if (oldMenu) {
                         isc.PickListMenu._DSPickListCacheSize -= 1;
                         var pickList = oldMenu._pickList;
                         
                         var dsLists = cachedLists[pickList.getDataSource().getID()];
                         dsLists.remove(oldMenu);                         
                         if (pickList._formItems != null) {                             
                             for (var formItem in pickList._formItems) {
                                 if (window[formItem] && window[formItem].pickList == pickList) 
                                     delete window[formItem].pickList
                             }
                         }
                         // destroy on a delay so it doesn't slow this method down
                         oldMenu._pickList.delayCall("destroy");
                         
                     }
                     
                 }
             }
            
        } else {
            isc.PickList._pickListInstance = this.pickList;
        }
    },
    
    getPickListCellHeight : function () {
        var cellHeight = this.pickListCellHeight;
        // If a developer specifies pickListProperties.cellHeight, respect that too
        if (this.pickListProperties && this.pickListProperties.cellHeight != null) {
            cellHeight = this.pickListProperties.cellHeight;
        }
        if (this.valueIcons != null || this.getValueIcon != null) {
            var valueIconHeight = this.getValueIconHeight();
            if (valueIconHeight > cellHeight) cellHeight = valueIconHeight;
        }
        return cellHeight;
    },
    
    _getInitialSort : function () {
        if (this.initialSort) {
            // just return the initialSort, after making it an array if necessary
            if (!isc.isAn.Array(this.initialSort)) this.initialSort = [this.initialSort];
            return this.initialSort.duplicate();
        }
        // get any specified sortField and build a sortSpecifier array from it
        var sortField = this.sortField == null ? this.sortFieldNum : this.sortField;
        if (sortField == null) return null;
        if (isc.isA.Number(sortField)) {
            // force a numeric sortField to it's fieldName
            sortField = this.sortField = this.getFieldName(sortField);
        }
        // make sure sortField is an array
        if (!isc.isAn.Array(sortField)) sortField = [sortField];
        var initialSort = [];
        for (var i=0; i<sortField.length; i++) {
            // add a sortSpecifier for each sortField, each using the public sortDirection
            initialSort.add({ property: sortField[i], direction: this.sortDirection });
        }
        return initialSort.duplicate();
    },

    // Set Up Pick List - apply properties to the pickList to link it to this form item.
    // Called every time the pick-list is shown.
    // For form items that re-use the same pickList this method must set properties connecting
    // the pickList to this form item.
    // Otherwise simply ensure the displayed set of data is up to date.
    setUpPickList : function (show, queueFetches, requestProperties, dropCache) {
        var pickList = this.pickList;

        
        if (pickList.formItem !== this && pickList.isDrawn()) {
            pickList.clear();
        }

        var cellHeight = this.getPickListCellHeight();
        pickList.setCellHeight(cellHeight);     
        if (this.separateValuesList) {
            this.separateValuesList.setCellHeight(cellHeight);
        }

        // These methods both no-op if the pickList is already applied to this item
        // and already showing the correct set of fields
        this._applyPickListToItem();
        this.setUpPickListFields();

        // apply custom empty message if we have one - if not ensure orginal empty
        // message shows up rather than potentially picking up the empty message from
        // another pickList based item.
        if (!pickList.originalEmptyMessage) pickList.originalEmptyMessage = pickList.emptyMessage;
        pickList.emptyMessage = this.emptyPickListMessage || pickList.originalEmptyMessage;

        // always refilter
        
        var sortSpec = this._getInitialSort();
        if (sortSpec && sortSpec.length > 0) {            
            
            var useDS = this._getOptionsFromDataSource();
            var dropResultSet = false;
            if (this.pickList.data && isc.isA.ResultSet(this.pickList.data)) {
                var ds = this.getOptionDataSource();
                if (this.pickList.getDataSource() != ds) {
                    dropResultSet = true;
                } else {
                    
                    var criteria = this.getPickListFilterCriteria(),
                        context = {
                            textMatchStyle:this.textMatchStyle,
                            showPrompt:false
                        };
                    if (this.optionFilterContext != null) {
                        isc.addProperties(context, this.optionFilterContext);
                    }
                    if (this.optionOperationId != null) {
                        context.operationId = this.optionOperationId;
                    }
                    if (requestProperties != null) {
                        isc.addProperties(context, requestProperties);
                    }
                    if (!this.pickList.useExistingDataModel(
                            criteria, context.operationId, context))
                    {
                        dropResultSet = true;
                    }
                }
            }
            if (dropResultSet) this.pickList.setData([]);
            this.pickList.setSort(sortSpec);
        }
        this.filterPickList(show, queueFetches, requestProperties, dropCache);

        // Auto size on both width and height axes by default.
        // The pickList data is updated (re-filtered) for the new item before attempting an auto-size.
        // If (undocumented) autoSizePickList property is explicitly false, auto size on height
        // axis only.
        // Treat this.pickListWidth (or this.getElementWidth()) as a minimum
        // Treat this.pickListHeight as a maximum
        this.pickList.emptyMessageHeight = this.emptyPickListHeight;
        if (!this.hasPopOutPicker()) {
            this.pickList.setWidth(Math.max(1,this.pickListWidth || this.getElementWidth()));
             
            var autoFitWidth, props = this.pickListProperties;
            if (props && props.autoFitFieldWidths != null) autoFitWidth = props.autoFitFieldWidths;
            else autoFitWidth = this.autoSizePickList && !this.pickList.showHeader;
            this.pickList.autoFitFieldWidths = autoFitWidth;
            this.pickList.setAutoFitData(autoFitWidth ? "both" : "vertical");
            var minHeight = 1;
            if (this.pickList.showHeader) minHeight += this.pickList.headerHeight;
            if (this.pickList.showFilterEditor) minHeight += this.pickList.filterEditorHeight;
            this.pickList.setHeight(minHeight);
            this.pickList.setAutoFitMaxHeight(this.pickListHeight);
            this.pickList.setAutoFitMaxWidth(this.pickListMaxWidth);
        }
    },

    _applyPickListToItem : function () {
        var oldFormItem = this.pickList.formItem;    
        if (oldFormItem == this) return;

        // Determine desired properties from the various init params.
        var pickListProperties = {};
        isc.addProperties(pickListProperties, {
            // Ensure there's a pointer back to the form item
            formItem:this,
            
            normalBaseStyle:this.pickListBaseStyle,
            // If a tall style is defined use it, otherwise assume the normal version
            // can stretch
            tallBaseStyle:(this.pickListTallBaseStyle || this.pickListBaseStyle),
            
            applyRowNumberStyle:this.pickListApplyRowNumberStyle,
            
            hiliteColor:this.pickListHiliteColor,
            hiliteTextColor:this.pickListHiliteTextColor,
            
            // Allow components to show the pickList as a modal list
            showModal:this.modalPickList,
            
            // Pass this.dateFormatter through to the pickList.
            // This will ensure that date type fields are displayed the same in the
            // pickList cells as in the text box for this item if a dateFormatter is specified
            dateFormatter:this.dateFormatter,
        
            // notify the new item whenever we get fresh data (replaces previous dataArrived
            // implementation which would notify the previous form item)
            dataArrived : function (startRow, endRow) {
                if (isc._traceMarkers) arguments.__this = this;
                this.Super("dataArrived", arguments);
                if (this.formItem) this.formItem.handleDataArrived(startRow,endRow,this.data);
            },
            // notify the new item whenever an item is removed
            dataChanged : function (operationType,originalRecord,rowNum,updateData,filterChanged,dataFromCache) {
                this.Super("dataChanged", arguments);
                if (operationType == "remove" && originalRecord && this.formItem) this.formItem.handleDataRemoved(originalRecord);
            }
        });
        if (this.multiple && this.multipleAppearance == "picklist" 
            && this.allowMultiSelect) 
        {
            pickListProperties.selectionAppearance = "checkbox";
            pickListProperties.allowMultiSelect = true;

            pickListProperties.enableSelectOnRowOver = false;
            pickListProperties.selectionType = "simple";
            pickListProperties._selectFirstOnDataChanged = false;

            pickListProperties.styleName = "listGrid";
            pickListProperties.bodyStyleName = "gridBody";

        } else {
            // rowStyle is the default selectionAppearance
            pickListProperties.selectionAppearance = "rowStyle";
            pickListProperties.allowMultiSelect = false;

            pickListProperties.enableSelectOnRowOver = this.pickList.getClass().getInstanceProperty("enableSelectOnRowOver"); // restore to default
            pickListProperties.selectionType = "single";
            pickListProperties._selectFirstOnDataChanged = true;

            pickListProperties.styleName = "scrollingMenu";
            pickListProperties.bodyStyleName = "pickListMenuBody";
        }
        if (this.animatePickList) pickListProperties.styleName += "Animated";
        // apply this.pickListProperties on top of defaults so advanced developers can customize
        // the pickList directly.
        if (this.pickListProperties) {
            isc.addProperties(pickListProperties, this.pickListProperties);
        }

        this.pickList.setProperties(pickListProperties);

        // Add a reference from the data object back to the current formItem
        if (this.pickList.dataProperties == null) this.pickList.dataProperties = {};
        this.pickList.dataProperties.formItem = this;
        if (this.pickList.data != null) {
            this.pickList.data.formItem = this;
        }

        // Keep track of every form item for which 'this.pickList' points to this pickList
        // Required for shared pickLists - allows us to clear up these pointers if the cached
        // pickList gets removed to make room for more lists in the cache
        
        if (!this.pickList._formItems) this.pickList._formItems = {};
        this.pickList._formItems[this.getID()] = true;
        
        // If we're re-using a pickList, clear it's observations on the previous form item
        // it was associated with.
        if (oldFormItem) {
            
            if (this.pickList.isObserving(oldFormItem.containerWidget, "hide")) {
                this.pickList.ignore(oldFormItem.containerWidget, "hide");
            }
            if (this.pickList.isObserving(oldFormItem.containerWidget, "clear")) {
                this.pickList.ignore(oldFormItem.containerWidget, "clear");
            }
        }


        // Clean up the pickList if the form goes away
        
        if (!this.pickList.isObserving(this.containerWidget, "hide")) {
            this.pickList.observe(this.containerWidget, "hide", "observer.hide();");
        }
        
        if (!this.pickList.isObserving(this.containerWidget, "clear")) {
            this.pickList.observe(this.containerWidget, "clear", 
                                                    "if(observer.isDrawn())observer.clear();");
        }
        
        // Show our pickerNavigationBar as (a child of) the top gridComponent if
        // necessary.
        var pnbp = this.pickList.pickerNavigationBarParent;
        if (this.hasPopOutPicker()) {
            if (!this.pickerNavigationBar) {
                this.createPickerNavigationBar();
            }
            var icons = this.getPickerNavigationBarIcons();
            // (This method defined in the picker-nav-bar directly)
            
            this.pickerNavigationBar.setIcons(icons);

            if (!pnbp.contains(this.pickerNavigationBar)){
                if (pnbp.children != null && pnbp.children.length  == 1) {
                    // deparenting will clear the child but not destroy it. The
                    // previous form item the pickList was associated with can
                    // subsequently re-use it.
                    pnbp.removeChild(pnbp.children[0]);
                }
                pnbp.addChild(this.pickerNavigationBar);
            }
            pnbp.setVisibility("inherit");
        } else {
            pnbp.setVisibility("hidden");
        }
        
        // always mark the pickList as dirty. This ensures any cell formatter set up on this
        // item will be applied even if the data is unchanged
        this.pickList.markForRedraw();
        
        
    },

    //> @type PickListItemIconPlacement
    // For PickList items with +link{pickListItemIconPlacement} set such that the pickList does
    // not render near-origin, possible location for rendering formItemIcons.
    // @value "pickerNavigationBar" 
    //  icon will be displayed in the +link{ComboBoxItem.pickerNavigationBar,pickerNavigationBar} only (and not rendered
    //  inline within the formItem itself)
    // @value "formItem"
    //  icon will be displayed inline within the form item itself (and not within the
    //  +link{ComboBoxItem.pickerNavigationBar,pickerNavigationBar}
    // @value "both"
    //  icon will be displayed both inline (within the form item itself) and within the
    //  +link{ComboBoxItem.pickerNavigationBar,pickerNavigationBar}
    //
    // @visibility external
    //<

    //> @attr FormItemIcon.iconPlacement (PickListItemIconPlacement : null : IR)
    // For PickList items with +link{pickListItemIconPlacement} set such that the pickList does
    // not render near-origin, should this icon be rendered inline within
    // the formItem itself, or within the +link{ComboBoxItem.pickerNavigationBar}.
    // <P>
    // If not explicitly specified at the icon level, this will be picked up from
    // +link{pickList.iconPlacement}.
    // <P>
    // For mobile browsing with limited available screen space, icons rendered in
    // the navigation bar may be easier for the user to interact with.
    // @visibility external
    //<

    //> @attr pickList.iconPlacement (PickListItemIconPlacement : "both" : IR)
    // For PickList items with +link{pickListItemIconPlacement} set such that the pickList does
    // not render near-origin, should specified +link{formItem.icons,icons} be rendered inline within
    // the formItem itself, or within the +link{ComboBoxItem.pickerNavigationBar,pickerNavigationBar}.
    // <P>
    // May be overridden at the icon level via +link{FormItemIcon.iconPlacement}.
    // <P>
    // For mobile browsing with limited available screen space, icons rendered in
    // the navigation bar may be easier for the user to interact with.
    // @visibility external
    //<
    iconPlacement:"both",
    getPickerNavigationBarIcons : function () {
        if (!this.icons) return null;
        var navBarIcons = [];
        for (var i = 0; i < this.icons.length; i++) {
            var icon = this.icons[i];
            // Note: "shouldShowIcon" overridden to handle the enum determining whether
            // the icon will show in the picker nav bar vs in the formItem
            
            if (!this._shouldShowIcon(icon, true) || this._writeIconIntoItem(icon)) continue;
            navBarIcons.add(icon);
        }
        return (navBarIcons.length > 0 ? navBarIcons : null);
    },
    
    _getSpecialValues : function (includeEmptyValue) {
        // Returned cached version if available
        if (this._specialValues) return this._specialValues;

        var values = null;
        if (this.specialValues) {
            // Return copy of specialValues
            values = isc.PickList.optionsFromValueMap(this, this.specialValues);
            this._specialValues = values;
        } else if (includeEmptyValue && this.allowEmptyValue) {
            var valueField = this.getValueFieldName(),
                displayField = this.getDisplayFieldName()
            ;
            // If allowEmptyValue is true, add an empty entry to the top of the list            
            // Note that this is independant of addUnknownValues - we don't treat the null value
            // as an 'unknown' values - it's more like lack of a value.
            
            var emptyRecord = {};
            // Apply explicit null for the valueField. Avoids confusion about whether the
            // value is actually set at the form level.
            
            emptyRecord[valueField] = null;
            
            values = [ emptyRecord ];
        }

        return values;
    },

    // ------------
    // Data Management
    // ------------
    
    //>@method pickList.getOptionDataSource()
    // PickLists can derive their data directly from a valueMap, or retrieve data from a 
    // dataSource to display as options.
    // <P>
    // This method will return the dataSource used to populate the pickList, or null if 
    // none is specified (implies this list will derive its data from the valueMap for the item).
    // Default implementation will return +link{pickList.optionDataSource} if specified,
    // otherwise if this is a field with a specified <code>foreignKey</code> in a databound
    // form, returns the dataSource for the <code>foreignKey</code>.
    // Otherwise picks up <code>this.form.dataSource</code> if set.
    //
    // @return (DataSource) DataSource to use for fetching options
    // @visibility external
    //<
    
    
    // getPickListFields() 
    getPickListFields : function () {
        // Allow the developer to specify a set of fields 
        // Only really has meaning if the select item is databound, where multiple fields are
        // available, or if custom client pickList data was supplied.
        // For databound lists, properties such as valueMaps will be picked up from
        // the dataSource.
        if (this.pickListFields) {

            
            var value = this.emptyDisplayValue;
            if (value != null) {
                var fields = this.pickListFields,
                    valueField   = this.getValueFieldName(),
                    displayField = this.getDisplayFieldName(),
                    object, undef;
                if      ((object = fields.find("name", displayField)) != null &&
                    object.emptyCellValue === undef) object.emptyCellValue = value;
                else if ((object = fields.find("name",   valueField)) != null && 
                    object.emptyCellValue === undef) object.emptyCellValue = value;
            }
            return this.pickListFields;
        }
        
        
        var displayField = this.getDisplayFieldName(),
            fieldObj;
        if (displayField != null) {
            fieldObj = {width:"*", name:displayField}

        } else {
            fieldObj = {width:"*", name:this.getValueFieldName(),
                        // apply the same valueMap to this field so the display values show up
                        // correctly
                        
                        valueMap:this.getAllValueMappings()
                       }
        }
        // if this is a SelectItem and escapeHTML is set, apply it to the
        // pickList field too (this is supported at the LG level already)
        if (this.canEscapeHTML && 
                // outputAsHTML / asHTML are old and deprecated
                (this.escapeHTML || this.outputAsHTML || this.asHTML))
        {
            fieldObj.escapeHTML = true;
        }

        // If an empty display value is specified, apply it to the field as well so we show
        // empty options with the correct title.
        if (this.emptyDisplayValue != null) {
            // For consistency, unless canEscapeHTML is true for this item type (which indicates
            // that the item supports displaying HTML), escape the emptyDisplayValue.
            fieldObj.emptyCellValue = (this.canEscapeHTML ? this.emptyDisplayValue : String(this.emptyDisplayValue).asHTML());
        }

        // If we have an explicitly specified dateFormatter for this item, it will be passed to 
        // the pickList but only respected for fields of type "date" there.
        // Assume this is what the user intended and default the type to "date" for the pickList
        // field. This'll be unnecessary if the dataSource field is already of type:"date" of
        // course.
        if (this.dateFormatter != null) {
            fieldObj.type = "date"
        }
        
        // Apply imageURLPrefix and imageURLSuffix to the field if specified
        if (this.imageURLPrefix)    fieldObj.imageURLPrefix = this.imageURLPrefix;
        if (this.imageURLSuffix)    fieldObj.imageURLSuffix = this.imageURLSuffix;
        
        // hang a flag on the field object as being auto-generated.
        // in this case we'll assign our custom formatter to it.
        // Otherwise the user is on their own.
        fieldObj._isGeneratedField = true;
        
        return [fieldObj]
    },
    
    formatPickListValue : function (value,fieldName,record) {
        
        // apply standard formatter to the value in the single generated field for
        // standard pick lists.
        // This handles formatters applied via simpleType as well as any
        // 'formatValue()' method applied to this item
        if (this.pickList.getField(fieldName)._isGeneratedField) {
            return this._formatDataType(value);
        }
        return value;
    },
        
    
    //> @method  PickList.getPickListFilterCriteria()  (A)
    // +link{group:stringMethods,StringMethod} to return a set of filter criteria to be applied to
    // the data displayed in the pickList when it is shown.
    // <P>
    // If this is a databound item the criteria will be passed as criteria to
    // +link{dataSource.fetchData()}.  Otherwise an equivalent client-side filter will be
    // performed on the data returned by +link{getClientPickListData()}.
    // <P>
    // By default combines +link{formItem.optionCriteria} with
    // +link{pickList.pickListCriteria} if specified, otherwise an empty 
    // set of criteria so all records will be displayed.
    //
    // @return (Criteria) criteria to be used for databound or local filtering
    // @visibility external
    // @example databoundDependentSelects
    //<
    getPickListFilterCriteria : function () {
        var baseCrit = this.getOptionCriteriaCopy(),
            criteria = isc.DataSource.combineCriteria(baseCrit, this.pickListCriteria, null, this.textMatchStyle);
        if (criteria && this.form) {
            criteria = isc.DataSource.resolveDynamicCriteria(criteria, this.form.getRuleContext());
        }
        return criteria;
    },

    // This is a helper method to return the set of 'local' options.
    // default implementation returns null for databound pickLists, or the array of values for
    // non databound picklists.
    // Used by logic to navigate around via keyboard
    // Overridden on the Select item to handle databound pickLists with all rows cached
    getAllLocalOptions : function () {
        return this._getOptionsFromDataSource() ? null : this.getClientPickListData();
    },
    
    //> @method PickList.getValueFieldName()
    // @include FormItem.getValueFieldName()
    //<
    
    // Display fields -----------------------------------------------------------------
    
    //> @method   pickList.getDisplayFieldName()
    // @include FormItem.getDisplayFieldName()
    //<
 
    
    // For databound items, an explicitly specified displayField means we'll be displaying
    // values from one field of our dataSource, and returning data values from a different field.
    // (Something like a valueMap using the resultSet from the server as a mapping object).
    // This helper method will perform the conversion between display and data values based
    // on the pickList's data set, if necessary.
    // Retrieving the display value directly from the pickList dataSet often avoids us having to 
    // run the fetchMissingValues logic in setValue() whereby we perform a fetch against the
    // dataSource when setValue() is called
    // Note that in contrast if this is a freeform data entry type field (EG a combobox)
    // and the user enters a value that is not present in the data-set, it will be stored as
    // the data value for the item, even though the user is theoretically setting the
    // display value.
    // Form items that use the pickList interface will typically call this method from
    // mapValueToDisplay() and mapDisplayToValue()
    // returnNull parameter tells us to return null if we can't find an entry in our
    // pickList data that matches the value passed in - used to detect "unknown" values in
    // the ComboBoxItem. This param is actually only used in the case where displayField matches
    // valueField - otherwise we can bypass the "find" logic in that case and just return
    // the value passed in.
    _translateValueFieldValue : function (value, toValueField, returnNull) {
        // If we were not passed a defaultValue (value to return if no match was found),
        // return 
        // This method is only for use with databound pickLists.
        var ods = this.getOptionDataSource(),
            displayField = this.getDisplayFieldName(),
            valueField = this.getValueFieldName();
            
        // If we have no ODS and no displayField, just bail
        if (displayField == null && ods == null) {
            
            return;
        }
        
        // No need to translate if the fields are the same!
        // If passed the 'returnNull' param we actually do want to verify the entry exists
        // in the data however.
        if ((ods == null || !returnNull) && 
            (displayField == null || displayField == valueField)) return value;
        
        var resultSet = this.getPickListResultSet();
        var cache;
        if (resultSet != null) {
            // simple array of data
            if (isc.isAn.Array(resultSet)) {
                cache = resultSet;

            // ResultSet - reach into the cache
            // If we have a complete cache use it - this enables us to map data values that don't
            // match the current filter criteria.
            } else {
                cache = resultSet.allRows || resultSet.localData;
            }
        }

        
        var odsCacheData = (ods && !this.suppressOptionDSCacheAccess ? ods.getCacheData() : null);
        if (cache == null) {
            cache = odsCacheData;
            if (cache == null) return;
        }
            
        var toField = (toValueField ? valueField 
                                    // if there's no explicit displayField name, use the
                                    // valueField for both to and from fields.
                                    : displayField || valueField),
            fromField = (toValueField ? displayField || valueField
                                      : valueField);
        var retVal;
        // if the stored value is an array, it means we have to construct the return value
        // from the array of values passed in. (when selectItem.multiple == true)
        if (isc.isAn.Array(value)) {
            retVal = "";
            var keys = isc.shallowClone(value);            
		    for (var i = 0; i < value.length; i++) {
                var currVal = keys[i];
                // find the record that matches the current value
                var record = cache.find(fromField, value[i]);
                if (record == null && odsCacheData != null && cache != odsCacheData) {
                    record = odsCacheData.find(fromField, value[i]);
                }

                if (record != null) {
                    retVal += record[toField];
                } else continue;
                
		        if (i != value.length - 1) retVal += this.multipleValueSeparator;                
		    }
        } else {
            var record = cache.find(fromField, value);
            if (record == null && odsCacheData != null && cache != odsCacheData) {
                record = odsCacheData.find(fromField, value);
            }
            if (record != null) {
                 retVal = record[toField];
             }
        }
        return retVal;
    },

    // Gets the result set in use by the pick list.  This method is internal and is used by
    // form items to copy records from the pick list's result set to the form item's displayField
    // cache.
    getPickListResultSet : function () {
        var resultSet = this.pickList && this.pickList.formItem == this && !this.pickList.destroyed ? 
                            // originalData will have been set if this pickList's data is
                            // grouped.
                            (this.pickList.originalData || this.pickList.data) : null;
        return resultSet;
    },

    _$true:"true",
    setUpPickListFields : function () {
        var pickList = this.pickList,
            fields = this.getPickListFields(),
            currentFields = pickList.fields;
        // Verify that the fields are not already up to date. If so just bail.
        var fieldsChanged = !currentFields || (currentFields.length != fields.length);
        if (!fieldsChanged) {
            for (var i= 0; i < fields.length; i++) {
                var field = fields[i], plField = currentFields[i];
                for (var prop in field) {
                    if (field[prop] != plField[prop]) {
                        fieldsChanged = true;
                        break;
                    }
                }
                if (fieldsChanged) break;
            }
        }

        if (!fieldsChanged) return;
        
        // Ensure that all fields are visible by default. This avoids the gotcha where
        // detail fields
        // would otherwise fail to show up in pickLists.
        for (var i = 0; i < fields.length; i++) {
            if (fields[i].showIf == null) {
                fields[i].showIf = this._$true;
            }
        }
        
        // For the display field (which will display the valueIcons specified for this item)
        // pick up valueIcon sizing etc from this item unless it was explicitly overridden
        if (this.valueIcons != null || this.getValueIcon != null) {
            for (var i = 0; i < fields.length; i++) {
                var field = fields[i];
                if (field[this.form.fieldIdProperty] == this.getValueFieldName()) {
                    if (field.valueIconHeight == null) 
                        field.valueIconHeight = this.valueIconHeight;
                    if (field.valueIconWidth == null) 
                        field.valueIconWidth = this.valueIconWidth;
                    if (field.valueIconSize == null) 
                        field.valueIconSize = this.valueIconSize;
                    if (field.imageURLPrefix == null) 
                        field.imageURLPrefix = this.imageURLPrefix || this.baseURL || this.imgDir;
                    if (field.imageURLSuffix == null) 
                        field.imageURLSuffix = this.imageURLSuffix;
                }
            }
        }
        pickList.setFields(fields);
        var showHeader;
        if (this.pickListHeaderHeight == 0) showHeader = false;
        else if (this.pickListProperties) {
            if (this.pickListProperties.showHeader != null) {
                showHeader = this.pickListProperties.showHeader;
            } else if (this.pickListProperties.headerHeight == 0) {
                showHeader = false;
            }
        };

        if (showHeader == null) {
            var visibleFields = pickList.getFields();
            // if we're showing a checkbox column and only 1 other field, hide the header
            var hideHeaderThresh = (this.multiple && this.multipleAppearance == "picklist" 
                                   && this.allowMultiSelect == true) ? 2 : 1;
            // Show the header if there are multiple fields.
            showHeader = (visibleFields.length > hideHeaderThresh);
        }
        if (showHeader) {
            // we are likely to be sharing a pickList across items, each of which may have a
            // different header height, so reset each time the header is shown
            // (No ops if no change anyway)
            pickList.setHeaderHeight(this.pickListHeaderHeight);
            pickList.setShowHeader(true);
        } else {
            pickList.setShowHeader(false);
        }
    },

    // If we have an explicitly specified optionDataSource for the field, we will databind the
    // list to that dataSource.
    // Otherwise, we'll use the valueMap to get a set of client-only options if we have one
    // - or attempt to derive an optionDataSource if not.
    _getOptionsFromDataSource : function () {        
        // explicit datasource
        if (this.optionDataSource) return true;
        // check for auto-derived optionDataSource (this will be non-null if the field has a
        // foreignKey or the form as a whole has a DataSource)
        if ((this.showOptionsFromDataSource || !this.valueMap) && 
            this.getOptionDataSource() != null) return true;
        return false;
    },


    
    
    _$fetch: "fetch",
    _$filterPickList: "_filterPickList",
    filterPickList : function (show, queueFetches, request, dropCache) {    
        if (!queueFetches) 
            this._filterPickList(show, request, null, dropCache);
        else {
            this._queuedFetch = true;
            this._showOnDelayedFilter = show;
            this.fireOnPause(this._$fetch,
                             {target:this, methodName:this._$filterPickList,
                              args:[null, request, true, dropCache]}, 
                             this.fetchDelay);
        }
    },

    _pendingFetchOnPause : function () {
        return this.pendingActionOnPause(this._$fetch);
    },

    _filterPickList : function (show, request, delayed, dropCache) {   
        this._queuedFetch = null;
        
        if (delayed) show = this._showOnDelayedFilter;
        delete this._showOnDelayedFilter;
        this._showOnFilter = show;
        var useDS = this._getOptionsFromDataSource();        
        if (useDS) {
            var ds = this.getOptionDataSource();

            // Pass the (already set up) fields into the 'setDataSource' method.
            if (this.pickList.getDataSource() != ds) {
                this.pickList.setDataSource(ds, (this.pickList.completeFields || this.pickList.fields));
            }                
            // Will fall through to filterComplete() when the filter op. returns.        
            this.filterDataBoundPickList(request, dropCache);
        } else {
            // Ignore any requestProperties passed in for a client-only filter.
            var records = this.filterClientPickListData();           
            
            if (this.pickList.data != records) this.pickList.setData(records);

            // explicitly fire filterComplete() as we have now filtered the data for the 
            // pickList
            this.filterComplete();
        }
        
        
    },
    
    // _pickListNeedsRefilter()
    // This method determines whether the pickList passed in has been filtered to match
    // this item's criteria.
    //
    // Used by getFirstOptionValue() and to avoid unnecessary refilters when the shared pickList is
    // assigned to different formItems
    // returns a boolean - true if a refilter will be required (pickList not showing data that will
    // match this item's option set).
    
    _pickListNeedsRefilter : function (pickList) {
        // Only applies to databound pickLists
        if (!this._getOptionsFromDataSource() || !pickList) return;

        var ds = this.getOptionDataSource();
        if (pickList.getDataSource() == ds && pickList.data) {
            var context = pickList.data.context,
                crit = pickList.data.criteria;
            if (context && context.textMatchStyle != this.textMatchStyle) return true;
            if (this.optionFilterContext != null) {
                for (var field in this.optionFilterContext) {
                    if (this.optionFilterContext[field] != context[field]) return true;
                }
            }
            // if criteria match we don't need a refilter
            var criteria = this.getPickListFilterCriteria();
            if (ds.compareCriteria(crit, criteria, context) == 0) {
                return false;
            }
        }
        return true;
    },
    
    // getFirstOptionValue - used by SelectItem / ComboBoxItem if defaultToFirstOption is true
    getFirstOptionValue : function () {
        var value;
        if (this._getOptionsFromDataSource()) {
            var pickList = this.pickList || 
                            (this.reusePickList() ? this.getSharedPickList() : null);
            if (pickList && !this._pickListNeedsRefilter(pickList)) {
                
                var record = pickList.data.get(0);
                // Don't attempt to default to the loading row marker. Note that handleDataArrived
                // already handles setting to the default value.
                if (record == null || Array.isLoading(record)) {
                    value = null;
                } else {
                    value = record[this.getValueFieldName()];
                }
            } else {
                // In this we don't yet have a pickList showing options that will
                // match this form item, so can't get our first optionValue.
                // Kick off a fetch. When it completes we'll have a valid first option
                // Pass in the parameter to avoid dropping cache and forcing a server turnaround
                // (We may be able to refilter on the client if it's just a criteria change)
                
                this.fetchData(null,null,true);
            }
            
        } else {
            var map = this.valueMap;
            if (isc.isAn.Array(map)) value = map[0];
            else if (isc.isAn.Object(map)) {
                // use for...in to pick up first defined property on the object
                for (var field in map) {
                    value = field;
                    break;
                }
            }
        }
        return value;
    },
    
    //>@method pickList.getClientPickListData() [A]
    // Returns the set of data to be displayed in this item's PickList.
    // <P>
    // This method will be called for non-databound form items implementing the PickList
    // interface.  The default implementation will derive data from the item's valueMap - 
    // can be overridden to allow a custom set of options to be displayed.
    // <P>
    // Note that for PickLists that filter data based on user input
    // (+link{ComboBoxItem,ComboBox}), this method should return the data <b>before
    // filtering</b>.  To customize the data returned after filtering, override
    // +link{filterClientPickListData()} instead.
    // <P>
    // As an example, for a formItem with +link{valueField} set to "valueFieldName", the
    // default implementation would take a valueMap like the following:
    // <pre>
    //     valueMap: { value1: "display 1", value2: "display 2" }
    // </pre>
    // .. and returning the following set of records: 
    // <pre>
    //     [
    //          { valueFieldName : "value1" },
    //          { valueFieldName : "value2" }
    //     ]
    // </pre>
    // Due to the valueMap, these records will appear as a two row pickList displayed as
    // "display 1" and "display 2".
    //
    // @return (Array of ListGridRecord) Array of record objects to be displayed in the
    //           pickList. Note that when a user picks a record from the list, the value of the
    //           field matching <code>item.valueField</code> will be picked. Also note that the
    //           fields to be displayed can be customized via <code>item.pickListFields</code>
    //                  
    // @visibility external
    //<
    getClientPickListData : function () {        
        return isc.PickList.optionsFromValueMap(this);       
    },

    // configure this pickList to observe/ignore established set of methods
    _manageObservers : function (script) {
        var targets = [ "moved", "parentMoved", "scrolled", "parentScrolled" ];

        for (var i = 0; i < targets.length; i++) {
            var observing = this.pickList.isObserving(this.containerWidget, targets[i]);
            if (script) {
                if (!observing) this.pickList.observe(this.containerWidget, targets[i], script);
            } else {
                if (observing) this.pickList.ignore(this.containerWidget, targets[i]);
            }
        }
    },

    // Override point to notify the item that the pickList has been shown / hidden
    _pickListHidden : function () {
        if (isc.Canvas.ariaEnabled()) {
            this.setAriaState("expanded", false);
            this.clearAriaState("owns");
            
            this.clearAriaState("activedescendant");
        }
        this._manageObservers();
        if (this.pickListHidden) this.pickListHidden();
    },

    _pickListShown : function () {
        if (isc.Canvas.ariaEnabled()) {
            
            this.setAriaState("expanded", true);
            this.setAriaState("owns", this.pickList._getAriaHandleID());
        }
        this._manageObservers("observer.moveBy(deltaX,deltaY)");
        if (this.pickListShown) this.pickListShown();
    },

    // selectDefaultItem
    // When the pickList is initially shown / re-filtered update the selection.
    // For select items the current value will be selected.  
    // [Overridden by the comboBox class to always select the first record].
    
    selectDefaultItem : function () {
        if (this.pickList == null) return false;
        // Select the value currently displayed in the form item
        return this.selectItemFromValue(this.getValue());        
    },

    selectItemFromValue : function (value, dontScroll) {

        
        if (this.pickList) this.pickList.clearLastHilite();
        
        if (!isc.isAn.Array(value)) value = [value];
        var records = this.pickList.getSelection(),
            valueField = this.getValueFieldName(),
            allValuesFound = true,
            lastFoundRec,
            specialValueSelected = false;
        for (var i = 0; i < value.length; i++) {
            var currVal = value[i], 
                record;

            // Select value in specialValues if present
            if (this.separateValuesList) {
                var data = this.separateValuesList.getData(),
                    match = data != null && data.find(valueField, currVal)
                ;
                if (match) {
                    this.separateValuesList.selectSingleRecord(match);
                    specialValueSelected = true;
                }
            }

            // If the value is already selected we can just return. This is much quicker for most
            // cases since we don't have to iterate through the pickList data array   
            if (records.find(valueField, currVal)) continue;

            var data = this.pickList.getData();
            if (isc.ResultSet && isc.isA.ResultSet(data)) {
                
                var cache = data.localData;
                if (cache) record = cache.find(valueField, currVal);
            } else {
                record = data.find(valueField, currVal);            
            }
            if (record && record != Array.LOADING) {
                
                if (this.pickList.allowMultiSelect) {
                    this.pickList.selectRecord(record);
                } else {
                    this.pickList.selection.selectSingle(record);
                    this.pickList.fireSelectionUpdated();
                }
                lastFoundRec = data.indexOf(record);
            } else {
                allValuesFound = false;    
            }
        }
        if (!dontScroll && lastFoundRec != null) this.pickList.scrollRecordIntoView(lastFoundRec);
        if (!specialValueSelected && this.selection != null) {
            this.separateValuesList.deselectAllRecords();
        }
        // Return a boolean to indicate whether we successfully found and selected all records
        return allValuesFound;
    },

    
    // -- Data bound filtering --
    
    // filterComplete - callback fired when the data to be displayed has been filtered.
    // (Will be called for both databound and non-databound lists)
    filterComplete : function (response, data, request, fromSharedPickList) {
        
        if (!fromSharedPickList && request != null && request.clientContext != null) {
            // if we get back out of sequence responses, don't allow the earlier one to clobber the
            // more recent one.
            var lastID = this._lastFetchID,
                newID = request.clientContext.fetchID;
            if (lastID == null || lastID < newID) {
                this._lastFetchID = newID;
            } else {
                this.logWarn("Server returned out of order responses for databound fetch requests." +
                    " Ignoring superceded request results");
                
                return;
            }
        }
        this._fetchingPickListData = false;

        // If the present call to filterComplete() was for a different item to which the
        // shared pickList menu was applied, then don't update the shared pickList menu.
        if (this.pickList._fetchingForItem != null &&
            this.getID() !== this.pickList._fetchingForItem)
        {
            

        // Otherwise, update the pickList
        } else {
            if (this.getID() === this.pickList._fetchingForItem) {
                this.pickList._fetchingForItem = null;
            }

            this._processingFilterComplete = true;

            this._updatePickListForFilterComplete(response,data,request);

            this._updateValueForFilterComplete(response,data,request);

            this._processingFilterComplete = false;

            if (isc.Canvas.ariaEnabled()) {
                
                if (this.pickList._fetchingForItem == null ||
                    this.getID() === this.pickList._fetchingForItem ||
                    fromSharedPickList)
                {
                    this.pickList.clearAriaState("busy");
                }
            }
        }

        // If a callback was passed in when the filter was intialized, fire it now, passing in
        // the resultSet as a single parameter.
        
        var callback = (request && request.internalClientContext ?
                            request.internalClientContext._callback : null);
        if (callback) {
            this.fireCallback(
                callback,
                "item,dsResponse,data,dsRequest",
                [this, response, data, request]
            );
            
            if (request && request.internalClientContext) {
                delete request.internalClientContext._callback;
            }
        }
    },

    _updatePickListForFilterComplete : function (response, data, request) {

        var list = this.pickList;
        
        if (!list || list.destroyed) return;
        
        // The pick-list may have been shown by another item while the fetch was in progress
        // If so, nothing to do here.
        if (list.formItem != this) return;
        
        var hasFocus = list.hasFocus || (list.body && list.body.hasFocus);
        var data = list.getData();
        
        // If showing an empty picklist because the entry is too short
        // update emptyMessage to indicate this. After a complete filter
        // with no actual matches the emptyMessage will be reset automatically.
        var tooShort = (this.isEntryTooShortToFilter && this.isEntryTooShortToFilter());
        if (tooShort) {
            list.emptyMessage = this.getEntryTooShortMessage();
        }
        
        // Pop Out Picker has fixed size (fills panel / screen, etc)
        if (this.hasPopOutPicker()) return;
        
        if (data.getLength() == 0 && this._isPickListVisible()) {
			// no matches, so hide the dropdown, place focus in the form item itself
            if (this.hideEmptyPickList) {
                list.hide();
                if (hasFocus) this.focusInItem();
            } else if (this.allowPickListToClip) {
                var position = this.getPickListPosition();
                list.setRect([position[0], position[1]]);
            } else {
                isc.PickList._placeAdjacent(this, list, this.separateValuesList);
            }
		} else {
            // if we set the flag to show the list after the filter, show it now!
            if (this._showOnFilter) this._showPickList();
            // If the list is already showing, call placePickList to ensure it resizes to
            // accommodate content if required.
            else if (this._isPickListVisible()) this.placePickList();

            // recalculate autoFit height/width so the grid will shrink as approriate
            list.setHeight(1);
            list.setAutoFitData(list.autoFitData); 

            
            delete this._showOnFilter;
        }
    },
    
    // Fired from filterComplete - handles updating display value etc.
    // Overridden in comboBoxItem to handle the cases where focus has been taken from the
    // item mid-fetch and we need to completeOnTab or addUnknownValues is false.
    _updateValueForFilterComplete : function (response,data,request) {
        // Always select the default item at this point since we have the latest data
        this.selectDefaultItem();
        
        // Fold the pickList data into the cache of records set up at the form item level by the
        // fetchMissingValue flow.
        // This means if the pickList is subsequently associated with another item / its data
        // changes, we can still get at all the records this item has seen.
        // This code flow is used for synchronous filter (client-side data / valueMaps) - we only
        // care if this actually came from a fetch so check for response / data being non-null
        
        if (response != null && data != null) {
            this._addDataToDisplayFieldCache(data);
            // rebuild the "display field valueMap" - suppress refresh- we'll handle that later.
            this.updateDisplayValueMap(false);

        }
        
        // _checkDisplayFieldValueOnFilterComplete
        // This flag is set up by the ComboBox / SelectItem class as part of checkDisplayFieldValue
        // if a pickList filter operation is running while that method is called.
        // In this case we need to recheck now the pickList data has loaded.
        // If the pickList data loaded a display value for this fields value, we're done,
        // otherwise we need to perform another fetch
        if (this._checkDisplayFieldValueOnFilterComplete) {
            delete this._checkDisplayFieldValueOnFilterComplete;
            this._checkForDisplayFieldValue(this._value);
        } 
        
        // helper - if we're currently showing a data value and we just loaded the associated
        // display value, display it.
        this._updateDisplayValueForNewData();
        
    },
    
    // Helper to determine if a user-entered value matches something in the 
    // pickList for this comboBoxItem. Note that the value passed in is the display value,
    // not the data value.
    
        
    isUnknownValue : function (value) {
        var vm = this.getValueMap();
        if (vm != null) {
            if (isc.isAn.Array(vm)) {
                if (vm.contains(value)) return false;
            
            } else if (isc.isAn.Object(vm)) {
                for (var key in vm) {
                    if (vm[key] == value) return false;
                }
            }
        }
        
        // At this point we didn't find the value in the valueMap - check for it
        // being present in the loaded optionDataSource data.
        var ODS = this.getOptionDataSource();
        if (ODS != null) {
            return this._translateValueFieldValue(value, true, true) == null;
        }
        return true;
    },
    // If this item has a databound pickList, and a 'displayField',
    // If the value has been set to some data value, which doesn't correspond
    // to a row loaded in the pickList, and we haven't mapped to a display value
    // (EG fetchMissingValues is false, etc), if the data does subsequently get loaded in 
    // the pickList, we want to update the element value to show the display value.
    // We do this in response to new data arriving for the pickList (a filter operation completes)
    
    _updateDisplayValueForNewData : function () {
        if (this.isDrawn() && this.getValueFieldName() != null 
            && this._getOptionsFromDataSource())
        {
            
            if (this.isA("ComboBoxItem")) {
                // in a ComboBox where addUnknownValues is true, do an immediate 'updateData'
                // Scenario: 
                // - the user enters a valid display value but it hasn't been loaded yet so we don't
                //   know this. Therefore the comboBoxItem stores out the entered value as a data 
                //   value
                // - pickList fetch for data returns, containing the entry that matches this display 
                //   value. Running updateValue() again will ensure the data value is switched from
                //   the entered string to the appropriate data value for which that is a valid 
                //   display value.
                //
                // NOTE: addUnknownValuesFalse is handled separatedly by 
                // updateValueForFilterComplete override in ComboBoxItem class
                
                if (this.changeOnKeypress && this.addUnknownValues == true &&
                        !this.isUnknownValue(this.getElementValue())) 
                {
                    this.updateValue();
                }

                // If the text box has focus, never change the display value.
                
                if (this.hasFocus) return;
            }
                
            
            var currentVal;
            if (!this._itemValueIsDirty()) currentVal = this.getValue();
            else {
                if (this.isA("SelectItem")) currentVal = this._localValue;
                else currentVal = this.mapDisplayToValue(this.getElementValue());
            }
            // only update if we retrieved a record that matches the current value
            // This avoids us dropping a good display value when we lose it from the pickList cache
            var record = this.getSelectedRecord();
            if (record) {
                var displayVal = this.mapValueToDisplay(currentVal);
                
                if (this._displayValue != displayVal) {
                    this._clearPendingMissingValue(currentVal);
                    this.setElementValue(displayVal);
                    // If field was set to read-only during Loading message, make it editable now
                    this._clearLoadingDisplayValue();
                }
            }
        }
    },
    
    // This will only be called on a databound pickList
    _databoundFetchID:0,
    filterDataBoundPickList : function (requestProperties, dropCache) {
        if (isc._traceMarkers) arguments.__this = this;

        // If user has not entered enough characters to filter return empty data
        if (this.isEntryTooShortToFilter && this.isEntryTooShortToFilter()) {
            var data = this.pickList.originalData || this.pickList.data;
            if (data && isc.ResultSet && isc.isA.ResultSet(data)) {
                // A previous match was found but entry is now too short.
                // Clear previous matches.
                this.pickList.setData([]);
            }
            this.filterComplete();
            return;
        }

        var criteria = this.getPickListFilterCriteria(),
            context = {
                

                textMatchStyle:this.textMatchStyle,

                
                showPrompt:false
            };

        if (this.pickList.ignoreEmptyCriteria) {
            criteria = isc.DataSource.filterCriteriaForFormValues(criteria);
        }

        // this.optionFilterContext - an entry point to add properties to the operation context,
        // such as the name of a defined operation.
        if (this.optionFilterContext != null) isc.addProperties(context, this.optionFilterContext);

        // respect optionOperationId if specified
        if (this.optionOperationId != null) context.operationId = this.optionOperationId;

        // Additional properties to be applied to the request can be passed as a parameter.
        // This code path is used for the userVisible 'fetchData()' api
        if (requestProperties != null) {
            isc.addProperties(context, requestProperties);
        }
        
        
        
        
        
        var alreadyFetching = false,
            synchronousFilter = false;
        // originalData will be set if grouping has been applied to the pickList.
        var data = this.pickList.originalData || this.pickList.data;
        if (data && isc.ResultSet && isc.isA.ResultSet(data)) {
            data._ignoreInvalidatedRequests = true;
            if (dropCache) {
                // invalidateCache doesn't have a way to apply a callback.
                // Instead call '_invalidateCache' to drop the cache (without re-fetching)
                // Then continue with filterData() as normal
                data._invalidateCache();
            } else {
                if (!data.willFetchData(criteria, this.textMatchStyle)) {
                    // If we're currently fetching, but willFetchData returned false,
                    // we can't just call fetchData() and expect to be notified on callback.
                    
                    if (!data.lengthIsKnown() || (data.getLength() != 0 && !data.rowIsLoaded(0))) {
                        alreadyFetching = true;
                    } else {
                        synchronousFilter = true;
                    }
                }   
            }
            
            
            if (!alreadyFetching) {
            
                context.fetchID = this._databoundFetchID++;
                data._notifyNewItemOnFetchComplete = false;
            }
            
        }
        this.pickList.filterData(criteria, {target:this, methodName:"filterComplete"}, context);
        
        if (synchronousFilter && this.pickList.data.getLength() > 0 && 
            (this.pickList.data.rowIsLoaded && !this.pickList.data.rowIsLoaded(0))) 
        {
            this.logInfo("filterData with new criteria caused async fetch even though " +
                "data.willFetchData() returned false.", "pickListFilter");
            synchronousFilter = false;
        }

        if (synchronousFilter) this.filterComplete();
        else {
            this._fetchingPickListData = true;
            if (isc.Canvas.ariaEnabled()) {
                this.pickList.setAriaState("busy", true);
                this.clearAriaState("activedescendant")
            }

            var newData = this.pickList.data;
            
            if (!alreadyFetching) {
                this.pickList._fetchingForItem = this.getID();
                this.pickList._fetchingRequest = newData._requestIndex;
            } else {

                // This flag will ensure fetchRemoteDataReply() calls filterComplete for this item
                // Also clear the flag on the old data object.
                
                newData._notifyNewItemOnFetchComplete = true;
                if (data != newData) data._notifyNewItemOnFetchComplete = false;
 
                this.pickList._fetchingForItem = this.getID();
                this.pickList._sharedPickListFetchRequest = newData._requestIndex;
            }
        }
    },

    // defaultToFirstOption: whenever new data arrives for the first row, default to it if 
    // our value is currently unset.
    handleDataArrived : function (startRow,endRow,data) {
        if (this.defaultToFirstOption && this.getValue() == null && startRow == 0) {
            this.setToDefaultValue();
        }
         // helper - if we're currently showing a data value and we just loaded the associated
        // display value, display it.
        this._updateDisplayValueForNewData();
        // update our local "displayFieldValueMap" cache (used at the FormItem level)    
        this._addDataToDisplayFieldCache(data.getRange(startRow,endRow));
        var pfc = this._processingFilterComplete;
        // setting this flag avoids 'updateValueMap' from hiding the PickList if its showing
        // even though this isn't technically a "filterComplete" flow
        this._processingFilterComplete = true;
        this.updateDisplayValueMap(false);
        this._processingFilterComplete = pfc;

        if (this.dataArrived) this.dataArrived(startRow,endRow,data);
    },
    //> @method PickList.dataArrived()
    // If this item is showing a dataBound pickList, this notification method will be fired 
    // when new data arrives from the server.
    // @param startRow (int) index of first row returned by the server
    // @param endRow (int) index of last row returned by the server
    // @param data (ResultSet) pointer to this pickList's data
    // @visibility external
    //<
    dataArrived : function (startRow, endRow, data) {
    },

    // update specialValues list if matching record is removed
    handleDataRemoved: function (record) {
        if (this._specialValues == null) return;

        var valueField = this.getValueFieldName(),
            displayField = this.getDisplayFieldName(),
            removedValue = record[valueField],
            specialValues = this._specialValues
        ;

        for (var i = 0; i < specialValues.length; i++) {
            var specialValue = specialValues[i];
            if (specialValue[valueField] == removedValue) {
                specialValues.removeAt(i);

                if (this.pickList.isVisible() && this.pickList.isDrawn()) {     
                    if (!this.separateSpecialValues) {
                        this.filterPickList();
                    }
                }
                break;
            }
        }
    },
    
    // -- Client side filtering --
    
    //> @attr PickList.textMatchStyle (TextMatchStyle : "startsWith" : IR)
    // When applying filter criteria to pickList data, what type of matching to use.
    // <P>
    // For a databound pickList (+link{optionDataSource} set), <code>textMatchStyle</code> is
    // sent to the server as +link{dsRequest.textMatchStyle}.
    // <P>
    // For a non-databound pickList, <code>textMatchStyle</code> is applied by 
    // +link{pickList.filterClientPickListData(),filterClientPickListData()}.
    //
    // @visibility external
    //<
    textMatchStyle : "startsWith",

    //> @attr PickList.showAllOptions (boolean : null : IR)
    // If true, even non-matching options will be shown, with configurable 
    // +link{separatorRows,separator rows} in between.  Not valid for
    // +link{optionDataSource,databound pickLists}.
    //
    // @visibility external
    //<

    //> @attr PickList.separatorRows (Array of ListGridRecord : [{isSeparator:true}] : IR)
    // Array of records to show between matching and non-matching rows in the PickList.
    // <P>
    // Not valid for +link{optionDataSource,databound pickLists}.
    //
    // @visibility external
    //<

    //> @method PickList.filterClientPickListData()
    // Returns the data to display in the pick list.
    // <P>
    // The default implementation applies the criteria returned by 
    // +link{PickList.getPickListFilterCriteria()} to the data returned by
    // +link{PickList.getClientPickListData()}.  A record passes the filter if it has a
    // matching value for all fields in the criteria object.  Matching is performed according
    // to +link{textMatchStyle}.
    // <P>
    // If +link{PickList.showAllOptions} is set, all values are shown, with matching values
    // shown below a +link{PickList.separatorRows,separator}.
    //
    // @return (Array of ListGridRecord) array of record objects to display in the pickList
    //
    // @visibility external
    //<
    _$substring : "substring",
    separatorRows : [{ isSeparator:true }],
    filterClientPickListData : function () {
        // If the user has not entered enough characters to filter return no matched data
        if (this.isEntryTooShortToFilter && this.isEntryTooShortToFilter()) return null;

        var data = this.getClientPickListData();        
        var criteria = this.getPickListFilterCriteria();  
            
        if (criteria == null || isc.isA.emptyObject(criteria)) return data;

        var matches = [],
            nonMatches;

        if (this.showAllOptions) nonMatches = this.separatorRows.duplicate();
        
        var validCriterion = false;
        for (var fieldName in criteria) {
            var fieldCriterion = criteria[fieldName];
            if (!fieldCriterion || isc.isA.emptyString(fieldCriterion)) continue;

            validCriterion = true;
            
            // Handle being passed an array
            
            if (!isc.isAn.Array(fieldCriterion)) fieldCriterion = [fieldCriterion];
            
            for (var stringIndex = 0; stringIndex < fieldCriterion.length; stringIndex++) {
                var searchString = fieldCriterion[stringIndex];
                
                // Do a raw conversion to strings if passed non string values
                
                if (!isc.isA.String(searchString)) searchString += isc.emptyString;
                
                searchString = searchString.toLowerCase();
                
                var dataLength = data.getLength(),
                    valueFieldName = this.getValueFieldName();
                for (var i = 0; i < dataLength; i++) {
                    var possibleMatch = data[i][fieldName];
                    // for the valueField, run the record values through mapValueToDisplay() so
                    // they match what the users sees.  XXX running through the valueMap is
                    // appropriate here, but a formatter that returns HTML may screw this up
                    if (this.filterDisplayValue && fieldName == valueFieldName) {
                        possibleMatch = this.mapValueToDisplay(possibleMatch);
                    }
                    
                    // For now we'll do a stringwise comparison regardless of the data type of
                    // the possible match
                    if (!isc.isA.String(possibleMatch)) possibleMatch += "";
                
                    possibleMatch = possibleMatch.toLowerCase();
                    // Remove any mismatches from the list of options
                    if ((this.textMatchStyle == this._$substring &&
                         !possibleMatch.contains(searchString)) ||
                        (this.textMatchStyle != this._$substring && // assume startsWith
                         !isc.startsWith(possibleMatch, searchString))) 
                    {
                        if (this.showAllOptions) nonMatches.add(data[i]);
                    } else {
                        matches.add(data[i]);
                    }
                }
            }
        }
        
        if (!validCriterion) matches = data.duplicate();
        if (this.showAllOptions && nonMatches.length > 1) matches.addList(nonMatches);
        //this.logWarn("returning: " + this.echoAll(matches));
        return matches;
    },
        
    
    // --------------------------------------------------------------------------------------
    // PickList appearance
    // --------------------------------------------------------------------------------------
    

    // hide empty pickLists?
    shouldHideEmptyPickList : function () {   
        if (this.hideEmptyPickList != null) return this.hideEmptyPickList;
        return !this._getOptionsFromDataSource();
    },    

    _isPickListVisible : function () {
        var list = this.pickList;
        return (list == null ? false : list.isDrawn() && list.isVisible());
    },

    // Position / sizing

    // getPickListPosition;
    getPickListPosition : function () {
        var left = this.getPageLeft();
        // RTL - align right of PickList with right of the item.
        if (this.containerWidget.isRTL() && this.pickList) {
            left += this.getWidth();
            left -= this.pickList.getVisibleWidth();
        }
        return [this.getPageLeft(), this.getPageTop() + this.getHeight()];
    },

    // placePickList: This sets the position in the page scope (Page-level coords)
    // Called when the pickList is about to be shown (or to reposition when already drawn)
    
    placePickList : function () {
        var pickList = this.pickList;
        var placement = this.getPickListPlacement();
        if (placement != "nearOrigin") {
            var rect;
        
            if (placement == "halfScreen") {
                
                rect = [
                    isc.Page.getScrollLeft(), 
                    isc.Page.getScrollTop() + parseInt(isc.Page.getHeight()/2),
                    isc.Page.getWidth(),
                    isc.Page.getHeight()/2
                ]
            } else {
        
                var widget;
                if (isc.isA.Canvas(placement)) widget = placement;
                else if (placement == "fillPanel") {
                    widget = this.containerWidget;
                    while (widget != null) {
                        if (isc.isA.SplitPane(widget)) {
                        
                            

                            if (widget.navigationPane && 
                                widget.navigationPane.contains(this.containerWidget)) 
                            {
                                widget = widget.navigationPane;
                                
                            } else if (widget.listPane && 
                                widget.listPane.contains(this.containerWidget)) 
                            {
                                widget = widget.listPane;
                                
                            } else if (widget.detailPane && 
                                widget.detailPane.contains(this.containerWidget)) 
                            {
                                widget = widget.detailPane;
                            }

                            break;
                        } else {
                            widget = widget.getParentCanvas();
                        }
                    }
                }
            
                // Fill the target widget (may be the parent 'panel')
                if (widget != null) {
                    rect = widget.getPageRect();
                
                // Default behavior - fillScreen [or fillPanel, but we didn't find a
                // parent panel]
                } else {
                    rect = [
                        isc.Page.getScrollLeft(), 
                        isc.Page.getScrollTop(),
                        isc.Page.getWidth(),
                        isc.Page.getHeight()
                    ];
                    // container for animation during show/hide
                    var container = this._getFillScreenContainer();
                    if (container) {
                        container.setRect(rect);
                    }
                }
                
            }
            
           pickList.setRect(rect);
        } else {
    
            // if the pickList is dirty, redraw now. This ensures the reported visible height is correct
            // so isc.PickList._placeAdjacent() does the right thing.
            if (pickList.isDirty() || (pickList.body && pickList.body.isDirty())) {
                pickList.redraw("Refreshing stale pickList content before positioning");
            
            } else if (!pickList.isDrawn()) {
                isc.Canvas.moveOffscreen(pickList);
                pickList.setVisibility("hidden");
                pickList.draw();
            }

            // Note: using isc.PickList._placeAdjacent() will keep the pickList onscreen if possible.
            if (this.allowPickListToClip) {
                var position = this.getPickListPosition(),
                    left = position[0],
                    top = position[1];
                pickList.setRect([left, top]);
            } else {
                isc.PickList._placeAdjacent(this, pickList, this.separateValuesList);
            }
        }
    },

    _getFillScreenContainer : function () {
        if (this.animatePickList && this.pickList._fillScreenContainer == null) {
            this.pickList._fillScreenContainer = this.createAutoChild("fillScreenContainer", {
                children: [this.pickList]
            });
        }
        return this.pickList._fillScreenContainer;
    },

    // pickValue
    // This method is fired when a value is selected from the pick list.
    // Implementation will vary depending on the form item to which it is applied
    pickValue : function (value) {},
    
    // ------------------------------------
    // PickList Panel-Placement (for mobile UI)
    // Documented at the ComboBoxItem / SelectItem level
    
    // We implement this as a formItem.picker, so rely on standard "showPicker" behavior
    // to actually show it, with available customizations to position it as desired.
    // Building the picker is handled by standard autoChild pattern.
    
    getPickListPlacement : function () {
        if (this.pickListPlacement != null) {
            var placement = this.pickListPlacement;
            // normalize canvas ID to live canvas
            if (isc.isA.String(placement)) {
                var canvas = isc.Canvas.getById(placement);
                if (canvas != null) placement = canvas;
            }
            return placement;
        }
        // Default to fillPanel for mobile browsers
        return isc.Browser.isHandset || isc.Browser.isTablet ? "fillPanel" : "nearOrigin";
    },

    pickerNavigationBarConstructor:isc.NavigationBar,

        
    pickerNavigationBarDefaults:{
    
        getIconButton : function (icon) {
            // create a cache and lazily create only when necessary
            if (this._iconButtonCache == null) this._iconButtonCache = [];
            for (var i = 0; i < this._iconButtonCache.length; i++) {
                if (icon == this._iconButtonCache[i].icon) {
                    return this._iconButtonCache[i].iconButton;
                }
            }    
        
            // reference back to the formItem
            var item = this.creator;
            
            // stateful information
            var over = false,
                focused = item._iconShouldShowFocused(icon, true),
                disabled = item.iconIsDisabled(icon);
            
            var iconButton = this.createAutoChild("iconButton", {

                _constructor:isc.Img,

                icon:icon,

                src:item.getIconURL(icon, over, disabled, focused),
                styleName:item.getIconStyle(icon, over, disabled, focused),
                backgroundColor:icon.backgroundColor,                
                height:item.getIconHeight(icon),
                width:item.getIconWidth(icon),
                
                // Prompt of course unlikely to be displayed in touch interfaces
                prompt:item.getIconPrompt(icon),
                
                // Ignoring iconVAlign stuff - that is more about how icons should
                // line up against a tall text-box and doesn't particularly apply when
                // they're embedded in a nav-bar
                layoutAlign:"center",
                
                canFocus:!icon.imgOnly,
                click : function () {
                    // item available via closure
                    item._iconClick(this.icon);
                },
                keyPress : function () {
                    // item available via closure
                    item._iconKeyPress(this.icon);
                }
                
                
            });
            this._iconButtonCache.add({icon:icon, iconButton:iconButton});

            return iconButton;
          
        },
        // show icons within the navigation bar
        
        setIcons : function (icons) {
        
            if (this.iconButtons != null) {
                this.removeMembers(this.iconButtons);
                for (var i = 0; i < this.iconButtons.length; i++) {
                    var button = this.iconButtons[i];
                    // Destroy all spacers (trivial to rebuild if necessary)
                    if (isc.isA.LayoutSpacer(button)) button.destroy();
                    // Destroy any buttons which aren't associated with icons in our
                    // array.
                    // Others will be reused.
                    if (!this.icons || !this.icons.contains(button.icon)) {
                        var icon = button.icon;
                        button.destroy();
                        this._iconButtonCache.removeWhere("icon", icon);
                    }
                }
            }
            if (icons == null) {
                this.iconButtons = null;
            } else {
                this.iconButtons = [];
                for (var i = 0; i < icons.length; i++) {
                    // If there's specified hspace - achieve this with a spacer before the
                    // iconButton
                    
                    if (icons[i].hspace) {
                        this.iconButtons.add(
                            isc.LayoutSpacer.create({ 
                                width: icons[i].hspace 
                            })
                        )
                    }
                
                    this.iconButtons.add(this.getIconButton(icons[i]));
                }
                this.addMembers(this.iconButtons);
            }
            
            
        }
    },

    createPickerNavigationBar : function () {
        this.pickerNavigationBar = this.createAutoChild("pickerNavigationBar");
    },
    
    // If the placement is anything other than the standard "nearOrigin", we will be
    // showing the pop out picker    
    hasPopOutPicker : function () {
        var placement = this.getPickListPlacement();
        return (placement != "nearOrigin");
    }
    
});

isc.PickList.addClassProperties({

    //> @attr pickList.specialValues (ValueMap : null : IR)
    // A set of "special" values such as "All", "None" or "Invalid" that do not appear in the normal
    // +link{valueMap} or in the data returned by the +link{optionDataSource}.
    // <p>
    // Like other uses of +link{type:ValueMap}, either a list of values or a mapping from stored to
    // display value can be provided.
    // <p>
    // These values can either be shown at the top of the list of values (in the order specified), or
    // can be shown in a separate, non-scrolling region - the setting
    // +link{selectItem.separateSpecialValues,separateSpecialValues} controls this.
    // <p>
    // If <code>specialValues</code> are configured, +link{selectItem.allowEmptyValue,allowEmptyValue} is ignored - an empty
    // value, if desired, must be included in the <code>specialValues</code>.  To provide a
    // <code>specialValue</code> which clears the value of the field, use the special constant
    // +link{PickList.emptyStoredValue}.
    // <p>
    // <code>specialValues</code> can also be used to take a value that <i>does</i> appear in the
    // normal data and redundantly display it at the top of the list to make it more accessible.  Note
    // that in this case it is expected that the special value appears <i>both</i> at the top of the
    // list <i>and</i> in it's normal position in the list, so this works best with
    // +link{selectItem.separateSpecialValues,separateSpecialValues} mode enabled.
    // <p>
    // Also, if an +link{optionDataSource} is used, +link{specialValues} that appear in the normal
    // dataset <i>will</i> be updated by automatic +link{group:cacheSync,cache synchronization} (if 
    // the +link{displayField} is updated).  However when using a distinct +link{valueField} and
    // +link{displayField}, you are required to provide +link{specialValues} as a map (there is no
    // support for +link{formItem.fetchMissingValues,fetchMissingValues} automatically fetching appropriate display values).
    // <P>
    // Note that specialValues are not supported in conjunction with
    // +link{SelectItem.multiple,selectItem.multiple:true} or +link{MultiComboBoxItem}.
    //
    // @visibility external
    //<        

    //> @classAttr PickList.emptyStoredValue (String : "**emptyValue**" : IR)
    // Special constant used to indicate that choosing this value from the +link{specialValues} list
    // should result in clearing the value of the field.  Only for use with <code>specialValues</code>
    // - cannot be used elsewhere.
    //
    // @visibility external
    //<
    emptyStoredValue: "**emptyValue**",
    
    // Interface methods don't override existing methods, but in some cases
    // we want to override FormItem level behavior in implementing classes
    // To do this without duplication, make a static list of overrides here and
    // have the implementing classes pick this up explicitly when they are defined.
    
    
    _instanceMethodOverrides:{
        
        // Override _shouldShowIcon
        // Suppress icons at the formItem level if they're marked as showing in the nav bar
        // only.
        _shouldShowIcon : function pickListInterface_shouldShowIcon (icon, inNavBar) {
            if (!this.Super("_shouldShowIcon", arguments)) {
                return false;
            }
            if (icon == this.getPickerIcon()) return !inNavBar;
            
            var showInPickerEnum = icon.iconPlacement;
            if (showInPickerEnum == null) showInPickerEnum = this.iconPlacement;
            if (showInPickerEnum == "both") return true;
            return inNavBar ? (showInPickerEnum == "pickerNavigationBar") 
                            : (showInPickerEnum == "formItem");
        }
    }

});

// add some static methods to handle default data-management.
isc.PickList.addClassMethods({

    // Central notification when the shared pickList fetch completes.
    // Rely on the stored target form item, and current request index to 
    // determine whether to fire filterComplete()
    _sharedPickListFetchComplete : function (resultSet, dsResponse, data, request) {
        var pickListID = request.componentId,
            pickList = window[pickListID];
        if (pickList == null) {
            this.logWarn("_sharedPickListFetchComplete - unable to get shared pickList which initialized fetch");
            return;
        }

        // If a subsequent fetch request has been initiated, this response is to an
        // old request - ignore it
        if (pickList._sharedPickListFetchRequest != request.internalClientContext.requestIndex) {
            return;
        }
        var currentItem = pickList._fetchingForItem;
        if (!currentItem) {
            this.logWarn("_sharedPickListFetchComplete - unable to determine current target formItem");
            return;
        }
        currentItem = window[currentItem]; // it's an ID
        // Avoid running through this method on next fetch complete
        resultSet._notifyNewItemOnFetchComplete = false;
        currentItem.filterComplete(dsResponse,data,request,true);
    },


    // optionsFromValueMap()
    // Method to determine set of data to display in the pickList based on the valueMap of
    // a (non databound) pickList item.
    
    optionsFromValueMap : function (item,map,emptyValue) {
        var valueMap = map || item.getValueMap(),
            records = [];
           
        if (valueMap == null) valueMap = [];
        
        // We have to turn the valueMap into an array of record type objects.
        
        var valueField = item.getValueFieldName(),
            displayField = item.getDisplayFieldName();
        
        if (isc.isAn.Array(valueMap)) {
            for (var i = 0; i < valueMap.length; i++) {
                records[i] = {};
                var value = valueMap[i];
                if (emptyValue && value == emptyValue) value = null;
                records[i][valueField] = value;
                if (displayField != null) records[i][displayField] = value;
            }
            records._derivedFromValueMapObject = true;
        
        } else if (isc.isAn.Object(valueMap)) {
            var i = 0;
            var type = item.getType(),
                convertToInt, convertToFloat, convertToBoolean;
                
            if (type != null) {
                if (isc.SimpleType.inheritsFrom(type, "integer")) 
                {
                    convertToInt = true;
                } else if (isc.SimpleType.inheritsFrom(type, "float")) {
                    convertToFloat = true;
                } else if (isc.SimpleType.inheritsFrom(type, "boolean")) {
                    convertToBoolean = true;
                }
            }

            for (var j in valueMap) {
                records[i] = {};
                var key = j;
                if (convertToInt) {
                    var intVal = parseInt(key);
                    if (intVal == key) key = intVal;
                } else if (convertToFloat) {
                    var floatVal = parseFloat(key);
                    if (floatVal == key) key = floatVal;
                } else if (convertToBoolean) {
                    var boolVal = (key == "true" ? true : (key == "false" ? false : null));
                    if (boolVal != null) key = boolVal;
                }
                
                records[i][valueField] = key;
                if (displayField != null) records[i][displayField] = valueMap[j];
                i++;
            }
            records._derivedFromValueMapObject = true;
        }
       
        return records;
    },

    
    _placePickListRect : function (width, height, adjacentRect, minWidth, minHeight, isRTL) {
        var pageWidth = isc.Page.getWidth(),
            pageHeight = isc.Page.getHeight(),
            // param will give us negative origin coords if we're in RTL mode
            pageScrollLeft = isc.Page.getScrollLeft(true),
            pageScrollTop = isc.Page.getScrollTop(),
            pageScrollRight = (pageWidth + pageScrollLeft),
            pageScrollBottom = (pageHeight + pageScrollTop),

            adjacentRectLeft = Math.max(pageScrollLeft, Math.min(pageScrollRight,
                adjacentRect[0])),
            adjacentRectTop = Math.max(pageScrollTop, Math.min(pageScrollBottom,
                adjacentRect[1])),
            adjacentRectRight = Math.max(pageScrollLeft, Math.min(pageScrollRight,
                adjacentRect[0] + adjacentRect[2])),
            adjacentRectBottom = Math.max(pageScrollTop, Math.min(pageScrollBottom,
                adjacentRect[1] + adjacentRect[3]));

        var bestI = 0,
            maxArea = null,
            pass = 1;
        for (var i = 0; i < 4; ++i) {
            var boxWidth = ((i % 2 == 0) != isRTL ?
                    pageScrollRight - adjacentRectLeft : adjacentRectRight - pageScrollLeft),
                boxHeight = (i < 2 ?
                    pageScrollBottom - adjacentRectBottom : adjacentRectTop - pageScrollTop),
                newWidth = Math.min(width, boxWidth),
                newHeight = Math.min(height, boxHeight);
            // Pick whatever fits on-screen.
            if (newWidth == width && newHeight == height) {
                bestI = i;
                break;
            }

            // If the rectangle could be resized to fit on-screen then calculate the area that the
            // rectangle would occupy.
            if (newWidth >= minWidth && newHeight >= minHeight) {
                var area = newWidth * newHeight;
                if (maxArea == null || area > maxArea) {
                    maxArea = area;
                    bestI = i;
                }
            }
        }

        var bestBoxWidth = ((bestI % 2 == 0) != isRTL ?
                pageScrollRight - adjacentRectLeft : adjacentRectRight - pageScrollLeft),
            bestBoxHeight = (bestI < 2 ?
                pageScrollBottom - adjacentRectBottom : adjacentRectTop - pageScrollTop),
            bestWidth = Math.min(width, bestBoxWidth),
            bestHeight = Math.min(height, bestBoxHeight),
            bestLeft = ((bestI % 2 == 0) != isRTL ? adjacentRectLeft : adjacentRectRight - bestWidth),
            bestTop = (bestI < 2 ? adjacentRectBottom : adjacentRectTop - bestHeight);

        return [bestLeft, bestTop, bestWidth, bestHeight];
    },

    _placeAdjacent : function (widget, pickList, separateValuesList) {
        var pickListRect = pickList.getPeerRect(),
            pickListWidth = pickListRect[2],
            pickListHeight = pickListRect[3],
            adjacentRect = widget.getPeerRect(),
            separateValuesHeight = (separateValuesList
                                     && separateValuesList.data 
                                     && separateValuesList.data.getLength() > 0 ? 
                                        separateValuesList.getVisibleHeight() : 0),
            emptyPickListHeight = (
                (!widget.shouldHideEmptyPickList() &&
                pickList.getTotalRows() < 1 &&
                widget.emptyPickListHeight) || 0),
            minPickListHeight = Math.max(
                1, emptyPickListHeight, pickList.getHeaderHeight() + separateValuesHeight),
            minPickListWidth = Math.max(1, widget.pickListWidth || 0),
            isRTL = widget.containerWidget.isRTL(),
            newPickListRect = isc.PickList._placePickListRect(
                pickListWidth, pickListHeight, adjacentRect,
                minPickListWidth, minPickListHeight, isRTL);
        var newPickListWidth = newPickListRect[2],
            newPickListHeight = newPickListRect[3];
        // assert pickListWidth >= newPickListWidth
        // assert pickListHeight >= newPickListHeight
        if (pickListWidth > newPickListWidth) {
            pickList.setAutoFitMaxWidth(newPickListWidth);
        }
        if (pickListHeight > newPickListHeight) {
            pickList.setAutoFitMaxHeight(newPickListHeight - separateValuesHeight);
        } else {
            newPickListRect[3] -= separateValuesHeight;
        }
        pickList.setPageRect(newPickListRect);
    }
});




} // end of if (isc.ListGrid)...
