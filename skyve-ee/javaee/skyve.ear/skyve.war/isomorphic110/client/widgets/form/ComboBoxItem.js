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
//> @class ComboBoxItem
// The Combobox is a text input field which can show a list of options via a drop-down
// PickList.
// <p>
// The set of options will be filtered based on the current value in the text field, so only
// options that match what has been typed so far will be displayed.
// The set of options can be derived from a ValueMap or dynamically retrieved from a
// dataSource.  See the +link{interface:PickList} interface for further settings.
// <P>
// The two most common use cases for ComboBoxItems are:
// <ul><li>With +link{comboBoxItem.addUnknownValues} set to true, the ComboBoxItem acts as a
//  freeform text entry field with the picklist providing essentially a set of suggested completions
//  similar to a URL bar in a web browser.</li>
//     <li>With +link{comboBoxItem.addUnknownValues} set to false, the ComboBoxItem acts similarly
//  to a SelectItem where a fixed set of options is available to the user and the text entry field
//  is essentially used to filter which of these options are visible</li></ul>
// <P>
// Other commonly used settings to configure ComboBoxItem behavior are:<br>
// - +link{ComboBoxItem.defaultToFirstOption} - this will select the first option from the pickList
// as a default value for the item - and<br>
// - +link{ComboBoxItem.completeOnTab} which causes the
// current selection in the pickList (if there is one) to be chosen when the user tabs out of the
// field, allowing a user to type a few characters and hit tab to auto-complete to the first matched
// option. <code>completeOnTab</code> is automatically set to true if +link{comboBoxItem.addUnknownValues, addUnknownValues} is 
// false.
// <P>
// ComboBoxItem does not provide built-in support for multiple selection.  For a Combobox
// that does provide such a multiple-select feature use +link{MultiComboBoxItem}.
//
// @see interface:PickList
// @implements PickList
// @treeLocation Client Reference/Forms/Form Items
// @example listComboBox    
// @visibility comboBox
//<
// Example of a Combo-Box type item in Windows applications: The URL bar for most browsers.
// ComboBox is used for:
// - "search": ComboBox is in a search form which is bound to the DataSource being searched
//   - eg: autoCompletion on names in an address book
// - "find-related": ComboBox is in an editing form and is being used to pick related records
//   from other DataSources
//   - eg: when editing an Account, find a User record to use as account.owner
//   - in this case, we want to
//     - default to displaying identifying fields from the related records
//     - store the primary key of the related record, NOTE however:
//       - the fieldName in the record being edited may not be the same as the fieldName
//         of the PK in the related record
//       - we may not actually store the PK if field.foreignKey indicates some other field.
//         Eg, the primaryKey might be a meaningless internal value and we may store instead
//         some other unique value
isc.defineClass("ComboBoxItem", "TextItem", "PickList");

isc.defer("if (isc.ComboBoxItem._instancePrototype.animatePickList == null) isc.ComboBoxItem.addProperties({ animatePickList: isc.Browser.isHandset || isc.Browser.isTablet });");

// Pick up "instanceMethodOverrides from the PickList interface
// These are methods which override the FormItem implementation and so won't get picked up
// by the 'mixInInterface' flow
if (isc.PickList) isc.ComboBoxItem.addMethods(isc.PickList._instanceMethodOverrides);

isc.ComboBoxItem.addMethods({
    
    init : function () {
        if (this.specialValues && this.allowEmptyValue) {
            // Default is allowEmptyValue:true so a warning will be unhelpful
            this.allowEmptyValue = false;
        }
        return this.Super("init", arguments);    
    },

    //>@attr ComboBoxItem.defaultValue (boolean : null : IRW)
    // Static default value for this ComboBoxItem. To default to the first option use
    // +link{ComboBoxItem.defaultToFirstOption} instead.
    // @visibility external
    //<
    
    //> @method ComboBoxItem.defaultDynamicValue() (A)
    // Expression evaluated to determine the +link{ComboBoxItem.defaultValue} when no value is 
    // provided for this item. To default to the first option use
    // +link{ComboBoxItem.defaultToFirstOption} instead.
    // @visibility external
    //<
    
    //> @attr ComboBoxItem.defaultToFirstOption (Boolean : false : IRW) 
    // Select the first option as the default value for this ComboBoxItem. If options are derived
    // from a dataSource, the first value returned by the server will be used, otherwise the first
    // value in the valueMap. If enabled, this setting overrides +link{ComboBoxItem.defaultValue}
    // and +link{ComboBoxItem.defaultDynamicValue}.
    // @visibility external
    //<
    
    //>@attr ComboBoxItem.showHintInField (boolean : null : IRWA)
    // If showing a hint for this form item, should it be shown within the field?
    // <P>CSS style for the hint is +link{selectItem.textBoxStyle} with the suffix
    // "Hint" appended to it. 
    // @group appearance
    // @see FormItem.hint
    // @visibility external
    //<

    //> @attr   comboBoxItem.mask  (string : null : IRWA)
    // Not applicable to a ComboBoxItem.
    // @visibility  external
    //<    
    //> @attr   comboBoxItem.maskSaveLiterals   (boolean : null : IRWA)
    // Not applicable to a ComboBoxItem.
    // @visibility  external
    //<    
    //> @attr   comboBoxItem.maskPadChar   (string : " " : IRWA)
    // Not applicable to a ComboBoxItem.
    // @visibility  external
    //<    
    //> @attr   comboBoxItem.maskPromptChar   (string : "_" : IRWA)
    // Not applicable to a ComboBoxItem.
    // @visibility  external
    //<    
    //> @attr   comboBoxItem.maskOverwriteMode   (boolean : null : IRWA)
    // Not applicable to a ComboBoxItem.
    // @visibility  external
    //<    
    
    // Default to auto-sizing pickList
    // This means the pickList will expand to the size required to accomodate its content.
    // As the user types and filters occur, this means the pickList may resize horizontally but
    // seems to have no obvious performance impact
    autoSizePickList:true,


    // --------------------------------------------------
    // PickList PanelPlacement behavior (for mobile apps)

    //> @attr comboBoxItem.pickListPlacement (PanelPlacement | Canvas | String : null : IR)
    // Controls where the +link{PickList} is placed.  
    // Can be specified as a +link{type:PanelPlacement}
    // or a specific widget that should be filled (by specifying an actual Canvas or
    // +link{Canvas.ID}).
    // <p>
    // Default behavior is to <code>"fillPanel"</code> if +link{Browser.isHandset} or
    // +link{Browser.isTablet}, to better accomodate the smaller screen real estate and 
    // less precise
    // pointing ability on such devices.
    // <p>
    // When filling the whole screen, part of the screen or a specific panel, the expanded
    // interface is created as a +link{FormItem.picker,standard FormItem picker}, and 
    // incorporates a +link{pickerNavigationBar,navigation bar} and 
    // +link{pickerExitButton,cancel button} that hides the expanded interface, as well 
    // as a separate +link{pickerSearchField,search field}.
    // @group panelPlacement
    // @visibility external
    //<
    
    
    // pickListPlacement:null,

    //> @attr comboBoxItem.iconPlacement (PickListItemIconPlacement : "both" : IR)
    // @include pickList.iconPlacement
    //<

    // In "fillPanel" / "fillScreen" / "halfScreen" pickList mode (or targeting a specific
    // canvas), render out our element in readOnly mode. We have a separate active text 
    // item as part of the picker.
    _elementIsReadOnly:function () {
        var readOnly = this.Super("_elementIsReadOnly", arguments);
        
        if (!readOnly && this.hasPopOutPicker()) {
            readOnly = true;
        }
        return readOnly;
    },

    //> @attr comboBoxItem.pickerSearchField (AutoChild TextItem : null : IR)
    // The <code>pickerSearchField</code> is a separate +link{TextItem} created for 
    // search string entry when +link{pickListPlacement} indicates that the search 
    // interface takes over an entire panel or the entire screen.
    // <p>
    // The following +link{group:autoChildUsage,passthroughs} apply:
    // <ul>
    // <li>+link{ComboBoxItem.pickerSearchFieldHint,pickerSearchFieldHint} for +link{FormItem.hint}</li>
    // </ul>
    //
    // @group panelPlacement
    // @visibility external
    //<
    pickerSearchFieldDefaults:{
        width: "*",
        height: "100%",
        editorType:"TextItem",
        showTitle:false,
        showHintInField:true,
        
        browserAutoCorrect:false,
        getHint:function () {
            return this.creator.pickerSearchFieldHint;
        }
    },

    //> @attr comboBoxItem.pickerSearchFieldHint (HTMLString : "Search" : IR)
    // +link{formItem.hint} for the +link{pickerSearchField}.
    // @group i18nMessages
    // @group panelPlacement
    // @visibility external
    //<
    pickerSearchFieldHint:"Search",


    //> @attr comboBoxItem.pickerSearchForm (AutoChild DynamicForm : null : IR)
    // Form that contains the +link{pickerSearchField}.
    // @group panelPlacement
    // @visibility external
    //<
    pickerSearchFormConstructor:isc.DynamicForm,

    pickerSearchFormDefaults:{
        numCols:1,
        width:"100%",
        height:25,
        cellPadding:0,
        cellSpacing:0,
        layoutAlign:"middle",
        leftPadding:5,
        rightPadding:5,
        overflow:"hidden"
    },

    //> @attr comboBoxItem.pickerExitButton (AutoChild NavigationButton : null : IR)
    // +link{NavigationButton} to dismiss the picker interface, created when 
    // +link{pickListPlacement} indicates that the search interface takes over the
    // entire panel or screen.
    // <p>
    // The following +link{group:autoChildUsage,passthroughs} apply:
    // <ul>
    // <li>+link{ComboBoxItem.pickerExitButtonTitle,pickerExitButtonTitle} for +link{Button.title}</li>
    // </ul>
    //
    // @group panelPlacement
    // @visibility external
    //<
    pickerExitButtonConstructor:isc.NavigationButton,

    //> @attr comboBoxItem.pickerExitButtonTitle (HTMLString : "Cancel" : IR)
    // The title for the +link{pickerExitButton}.
    //
    // @group i18nMessages
    // @group panelPlacement
    // @visibility external
    //<
    pickerExitButtonTitle:"Cancel",

    //> @attr comboBoxItem.pickerSaveButton (AutoChild NavigationButton : null : IR)
    // +link{NavigationButton} to dismiss the picker interface and store out the
    // value entered in the +link{pickerSearchField}, created when 
    // +link{pickListPlacement} indicates that the search interface takes over the
    // entire panel or screen.
    // <P>
    // This button will only be shown when +link{ComboBoxItem.addUnknownValues} is true.
    // <p>
    // The following +link{group:autoChildUsage,passthroughs} apply:
    // <ul>
    // <li>+link{ComboBoxItem.pickerSaveButtonTitle,pickerSaveButtonTitle} for +link{Button.title}</li>
    // </ul>
    //
    // @group panelPlacement
    // @visibility external
    //<
    pickerSaveButtonConstructor:isc.NavigationButton,

    //> @attr comboBoxItem.pickerSaveButtonTitle (HTMLString : "Accept" : IR)
    // The title for the +link{pickerSaveButton}.
    //
    // @group i18nMessages
    // @group panelPlacement
    // @visibility external
    //<
    pickerSaveButtonTitle:"Accept",

    //> @attr comboBoxItem.emptyPickListMessage (string : "No items to show" : IRWA)
    // Empty message to display in the comboboxItem if +link{PickList.hideEmptyPickList}
    // is <code>false</code>.
    // @group i18nMessages
    // @visibility external
    //<
    emptyPickListMessage: "No items to show",

    //> @attr comboBoxItem.pickerNavigationBar (AutoChild NavigationBar : null : IR)
    // +link{NavigationBar} created when +link{pickListPlacement} indicates that the search
    // interface takes over the entire panel or screen.
    //
    // @group panelPlacement
    // @visibility external
    //<

    createPickerNavigationBar : function () {
        var pickerSearchFieldProperties = isc.addProperties(
                {},
                this.pickerSearchFieldDefaults,
                this.pickerSearchFieldProperties,
                {
                 
                 name:"s",
                 handleKeyPress:function () {
                    // allow some dev custom keyPress handler to kill default behavior
                    // if appropriate!
                    var rv = this.Super("handleKeyPress", arguments);
                    if (rv != false) rv = this.creator.pickerSearchFieldKeyPress(this);
                    return rv;
                 },
                 handleChanged : function (newValue, oldValue) {
                    var returnVal = this.Super("handleChanged", arguments);
                    this.creator.pickerSearchFieldChanged(this);
                    return returnVal;
                 },
                 creator:this
                });

        this.pickerSearchForm = this.createAutoChild(
                                    "pickerSearchForm",
                                    { layoutAlign:"center",
                                      items:[pickerSearchFieldProperties]});
        this.pickerSearchField = this.pickerSearchForm.getItem("s");

        this.pickerExitButton = this.createAutoChild(
                                    "pickerExitButton", 
                                    {title:this.pickerExitButtonTitle,
                                     click:function() {
                                        this.creator.pickerExitButtonClick();
                                     }
                                    });
        this.pickerSaveButton = this.createAutoChild(
                                    "pickerSaveButton", 
                                    {title:this.pickerSaveButtonTitle,
                                     click:function() {
                                        this.creator.pickerSaveButtonClick();
                                     },
                                     disabled:true,
                                     visibility:
                                        this.addUnknownValues ? isc.Canvas.INHERIT 
                                                            : isc.Canvas.HIDDEN
                                    });
        this.pickerNavigationBar = this.createAutoChild("pickerNavigationBar",
                                    // We're totally overriding the controls so really
                                    // we're just using the styling of the Nav Bar class
                                    {controls:[
                                        this.pickerSearchForm, 
                                        this.pickerSaveButton, 
                                        this.pickerExitButton]});
    },

    // This method is executed when the user types into the picker search field
    pickerSearchFieldKeyPress : function (item, a,b,c,d) {
        return this.handleSearchItemKeyPress(item);
    },
    
    // Changed handler for the item.
    pickerSearchFieldChanged : function (item) {
        this._refreshPickList(item.getEnteredValue());
        if (this.pickerSaveButton && this.pickerSaveButton.isVisible()) {
            this.pickerSaveButton.setDisabled(false);
        }
    },


    // Click handler for exit button click
    pickerExitButtonClick : function () {
        // exit without picking.
        // Standard "hide" handles clearing the clickMask etc too
        this.pickList.hide();
    },

    pickerSaveButtonClick : function () {
        var textBoxVal = this.pickerSearchForm.getValue("s");
        this.setElementValue(textBoxVal == null ? "" : textBoxVal);
        this.pickList.hide();
        this.updateValue();
    },

    // ---

    //>@attr ComboBoxItem.progressiveLoading (Boolean : true : IRW)
    // Indicates whether or not this ComboBoxItem will load its list of options 
    // +link{DataSource.progressiveLoading,progressively}.  This property is copied onto the
    // underlying +link{class:PickList}.
    // @see DataSource.progressiveLoading
    // @group progressiveLoading
    // @visibility external
    //<
        
    
    // Override drawn() - if this is a databound pickList we want to perform a filter before
    // the pickList itself ever gets shown.
    
    drawn : function (a,b,c,d) {
        this.invokeSuper(isc.ComboBoxItem, "drawn", a,b,c,d);
        if (this.autoFetchData && this._getOptionsFromDataSource()) {
            
            this.filterWithValue = false;        
            this.fetchData(null, null, true);
        }
    },

    //> @attr comboBoxItem.showPickerIcon (Boolean : true : IRW)
    // @include FormItem.showPickerIcon
    // @visibility external
    //<
    showPickerIcon:true,

    //> @attr comboBoxItem.pickerIconWidth (int : 15 : IRWA)
    // @include FormItem.pickerIconWidth
    // @visibility external
    //<
    pickerIconWidth:15,

    //> @attr comboBoxItem.pickerIconHeight (int : null : IRWA)
    // Don't specify an explicit height for the picker icon - instead have it size to match the
    // height of the combo box item.
    // @visibility external
    //<
    pickerIconHeight:null,

    // Have native and synthetic selects' text styling match
    textBoxStyle:"selectItemText",

    //> @attr comboBoxItem.pickerIconSrc (SCImgURL : "[SKIN]/DynamicForm/ComboBoxItem_PickButton_icon.gif" : IRWA)
    // If +link{showPickerIcon,showPickerIcon} is true for this item, this property governs the
    // +link{FormItemIcon.src,src} of the picker icon image to be displayed.
    // <P>
    // When +link{group:skinning,spriting} is enabled, this property will not 
    // be used to locate an image, instead, the image is drawn via CSS based on the 
    // +link{FormItem.pickerIconStyle} property.
    // @include FormItem.pickerIconSrc
    // @visibility external
    //<
    pickerIconSrc:"[SKIN]/DynamicForm/ComboBoxItem_PickButton_icon.gif",

    // Apply some default properties to the picker icon.
    pickerIconProperties:{
        // We don't want it to be imgOnly because we need click handling, but we don't want it
        // in the page's tab order
        tabIndex:-1,
        showOver:true
    },
    
    // override modalPickList - we don't want to take focus from the text item when the
    // pickList is shown.
    modalPickList:false,
    
    //> @attr ComboBoxItem.showPickListOnKeypress  (Boolean : true : IRW)
    // Should the list of options be displayed whenever the user types into the 
    // combo-box textArea, or only when the user clicks on the pick button or uses the 
    // explicit <code>Alt+Arrow Down</code> key combination?
    // @visibility comboBox
    //<
    showPickListOnKeypress:true,
    
    //> @attr comboBoxItem.saveOnEnter (Boolean : true : IRW)
    // ComboBox items will submit their containing form on enter keypress 
    // if +link{DynamicForm.saveOnEnter,saveOnEnter} is true. Setting this property to
    // <code>false</code> will disable this behavior.
    // <P>
    // Note that if the drop down list of options (pickList) is visible an
    // <code>Enter</code> keypress is used to select a value from the available set of
    // options and will not automatically cause form submission.
    //
    // @visibility external
    //<
    // default implementation of formItem.shouldSaveOnEnter() returns this
    saveOnEnter: true,

    
    //> @attr ComboBoxItem.completeOnTab (boolean : null : IRW)
    // If true, when the pickList is showing, the user can select the current value by hitting
    // the <code>Tab</code> key.
    // <P>
    // Note that <code>completeOnTab</code> is not compatible with +link{formatOnBlur}
    // @visibility comboBox
    //<
    //completeOnTab:null,
    
    //> @attr ComboBoxItem.formatOnBlur (Boolean : false : IRW)
    // With <code>formatOnBlur</code> enabled, this comboBoxItem will format its value
    // according to the rules described in +link{formItem.mapValueToDisplay} as long as the 
    // item does not have focus.  Once the user puts focus into the item
    // the formatter will be removed. This provides a simple way for developers to
    // show a nicely formatted display value in a freeform text field, without the need
    // for an explicit +link{formItem.formatEditorValue()} 
    // and +link{formItem.parseEditorValue()} pair.
    // <P>
    // Note that this attribute is not compatible with +link{completeOnTab}
    // @visibility external
    //<
    

    // --------------------
    // Special Values
    // --------------------

    
    
    //> @attr ComboBoxItem.specialValues (ValueMap : null : IR)
    // @include pickList.specialValues
    // @visibility external
    //<

    //> @attr ComboBoxItem.separateSpecialValues (boolean : null : IR)
    // If true, +link{specialValues} special values such as the empty value will be shown in a
    // separate non-scrolling area, in the +link{separateValuesList}.  Aside from making these values
    // more easily accessible, showing them in a separate list allows data paging to be used, which is
    // disabled if the separateValues are shown in the normal drop-down list along with other values.
    //
    // @visibility external
    //<
    
    //> @attr ComboBoxItem.separateValuesList (AutoChild ListGrid : null : IR)
    // AutoChild used to show +link{specialValues}.
    //
    // @visibility external
    //<
    
    // Implement pickListShown to set up a page level mousedown handler to hide the pickList.
    
    pickListShown : function () {
        if (this.pickList.isVisible()) {
            // Don't pass in the 'fire once' param - if the user clicks outside we'll clear the
            // click event in response to that click. If the user clicks on the pickList
            // (or it's scrollbar, for example) we want to continue listening for future clicks
            // outside.
            this._pageClickID = 
                this.ns.Page.setEvent("mouseDown", this, null, "_mouseDownWithPickList");
        }
    },
    
    // Handle a mouseDown while the pickList is visible.
    // If the user clicked outside, dismiss the pickList
    // Otherwise ensure keyboard focus remains in the comboBoxItem on mouseUp so
    // the user can keep typing.
    
    _mouseDownWithPickList : function () {
        var pl = this.pickList;
        if (!pl || !pl.isVisible()) return;
        var target = isc.EH.lastEvent.target;
        // If the pickList doesn't contain the event, hide it now
        // Special-cases:
        // - if the user showed the header of the PL and clicked on the header menu
        //   we don't want to dismiss the menu
        // - if the user showed the filter of the PL and clicked in the filter row
        //   we don't want to dismiss the menu
        // - if the user show the filter row of the PL, and clicked a pickList from a
        //   selectItem within that filter form we don't want to dismiss...
        if (!pl.contains(target, true) &&
            (!pl._cornerMenu || !pl._cornerMenu.contains(target, true)) &&
            (!pl.filterEditor || pl.filterEditor.getEditForm() != target)
           )
        {
            var hitInnerPickList = false;
            if (pl.filterEditor) {
                var innerPickLists = pl.filterEditor.getEditForm().getItems().getProperty("pickList");
                innerPickLists.addList(pl.filterEditor.getEditForm().getItems().getProperty("picker"));

                for (var i = 0; i < innerPickLists.length; i++) {
                    if (innerPickLists[i] && innerPickLists[i].contains(target)) {
                        hitInnerPickList = true;
                        break;
                    }
                }
            }
            if (!hitInnerPickList) {
//                this.logWarn("target:" + target);
                pl.hide();
            }
        }

        // What to do on mouseUp?
        // - If the click occurred over the pickerIcon we don't want to re-show in response to 
        //   the click that follows this mouseDown.
        // - If the click occurred over the PickList, (or its scrollbar), in IE native focus will
        //   be taken from the textbox, even though the scrollbar is non-focusable.
        //   Restore focus on mouse up so the user can continue typing / do arrow-key navigation
        //   from the textbox.
        if (!this._mouseUpWithPickListEvent) {
            this._mouseUpWithPickListEvent = 
              this.ns.Page.setEvent("mouseUp", this, isc.Page.FIRE_ONCE, "_mouseUpWithPickList");
        }
        // Note: if the user is dragging the scrollbar, we'll see a dragStop event, with no
        // mouseUp
        if (!this._refocusOnDragStop) {
            this._refocusOnDragStop = 
              this.ns.Page.setEvent("dragStop", this, isc.Page.FIRE_ONCE, "_refocusFromPLMouseUp");
        }
    },
    _mouseUpWithPickList : function () {
        // If the click happened over this item, cancel it. this will prevent the 
        // pickList from showing when it was hidden on a mousedown and the user clicks this item.
        if (this.form._getEventTargetItemInfo().item == this) {
            this.ns.Page.clearEvent("dragStop", this._refocusOnDragStop);
            // no need to clear the _mouseUpWithPickListEvent since it's marked as FIRE_ONCE
            delete this._refocusOnDragStop;
            delete this._mouseUpWithPickListEvent;
            return false;
        }
        this._refocusFromPLMouseUp(true);
    },
    
    // If a mouseUp event occurs within the pickList, put focus into the text-box so
    // the user can keep typing.
    _refocusFromPLMouseUp : function (fromClick) {
        // Only clear the event we're not responding to since the events are marked as FIRE_ONCE
        
        if (fromClick == true) {
            this.ns.Page.clearEvent("dragStop", this._refocusOnDragStop);
        } else {
            this.ns.Page.clearEvent("mouseUp", this._mouseUpWithPickListEvent);
        }
        delete this._refocusOnDragStop;
        delete this._mouseUpWithPickListEvent;
        
        // Exceptions:
        // - we don't want to refocus if the user put focus into a filterEditor item
        //   within the pickList [not an issue since the target will be reported as
        //   the filter-edit form which isn't actually a child of the pickList]
        // - We don't want to refocus if the user put focus into the pickerSearchField
        var target = isc.EH.getTarget();
        if (this.pickList && this.pickList.isVisible() && 
            this.pickList.contains(target, true) &&
            (!this.pickerSearchForm || !this.pickerSearchForm.contains(target, true)))
        {
            this.focusInItem();
        }
    },
    
    // Implement pickListHidden to clear the page level click event to hide the pickList, if
    // we haven't already.
    pickListHidden : function () {
        if (this._pageClickID) this.ns.Page.clearEvent("mouseDown", this._pageClickID);
        delete this._pageClickID;
        delete this._lastFilterValue;
    },
    
    // Override handkeKeyPress - as the user types, we want to re-filter the list.
    // Also proxy Arrow key presses to the PickList to navigate through options.
    // Behavior:
    // - We don't want the list showing if 
    //      - there are no matches
    //      - the textItem is empty
    // - Otherwise we want the list to show.
    _$ArrowUp:"Arrow_Up", _$ArrowDown:"Arrow_Down",
    _$PageUp:"Page_Up", _$PageDown:"Page_Down",
    _$Escape:"Escape", _$Enter:"Enter", _$Tab:"Tab",
    handleKeyPress : function () {
        if (!this.hasFocus || this.isReadOnly()) return this.Super("handleKeyPress", arguments);
        var rv = this.handleSearchItemKeyPress(this);
        if (rv != false) rv = this.Super("handleKeyPress", arguments);
        return rv;
    },
    
    // Called from keypress on our text box, or on our pickerSearchItem
    handleSearchItemKeyPress : function (item) {
        var keyName = isc.EH.lastEvent.keyName,
            pickList = this.pickList,
            pickListVisible = (pickList ? (pickList.isDrawn() && pickList.isVisible()) : false);            

        // deliver PageUp/Down to the body to cause scrolling
        if (pickListVisible && (keyName == this._$PageDown || keyName == this._$PageUp)) {
            return pickList.body.handleKeyPress(isc.EH.lastEvent);
        }
        var enteredValue = item.getEnteredValue(),
            value = this.getValue(),
            isEmpty = (!enteredValue || enteredValue == isc.emptyString);
        if (item == this && keyName == this._$ArrowDown && isc.EH.altKeyDown()) {
            // By default we hide the picklist whenever we get a keypress and the value is
            // empty (this is for the case where the user clears out the combobox text value).
            // Set a flag here to avoid hiding the combobox unless the value has subsequently
            // changed.
            this._shownWithValue = enteredValue;
            this._shownOnEmpty = isEmpty;
            
            this.filterWithValue = false;
            this.showPickList();
            return false;
        }

        // when addUnknownValues is false, we don't actually update the value unless the
        // user picks something from the pickList or takes focus from the field.
        // Allow an Escape keypress to reset the display value to match the underlying
        // data value again.
        if (keyName == this._$Escape && this.addUnknownValues == false) {
            var displayValue = this.getDisplayValue(value);
            if (isc.isAn.emptyString(displayValue) && 
                this._getShowHintTextInDataElement()) 
            {
                this._showInFieldHint();
            } else {
                this.setElementValue(displayValue);
            }
        }

        // Should we complete (select the current item from the PickList) on Enter keypress?
        // - If addUnknownValues is true, always select the hilighted row in the pickList on 
        //   enter keypress (complete on enter)
        // - If addUnknownValues is false:
        //  - if the user has moved up or down the list using the arrow keys then hits enter,
        //    complete on enter
        //  - otherwise, if completeOnEnter (currently undocumented) is explicitly set, respect it
        
        //
        //  - otherwise complete on enter unless we're embedded in a search form and have
        //    an option dataSource set - in which case assume the user is actually trying to do
        //    a substring match and enter should kick off a search.
        var isEnter = keyName == this._$Enter,
            completeOnEnter = false;
        if (isEnter && (pickListVisible || this.addUnknownValues == true)) {
            completeOnEnter = this.shouldCompleteOnEnter();
        }

        var navigate = pickListVisible && (keyName == this._$ArrowDown || keyName == this._$ArrowUp);
        this._userNavigated = navigate;

        if (pickListVisible) {
            // pass navigation keys (arrows) through to the drop-down list
            // so the user can navigate through the selection
            if (navigate ||
                // also pass "Enter" up to the list to trigger 'itemClick' behavior
                // and "Escape" to trigger 'cancel' behavior
                (isEnter && completeOnEnter) || keyName == this._$Escape) 
            {
                pickList.bodyKeyPress(isc.EH.lastEvent);
                // kill the native behavior - shifting the cursor in the text field
                return false;
            }
            // hide the pickList on enter if completeOnEnter is false.
            
            if (isEnter) pickList.hide();
        } else if (this.addUnknownValues) {
            if (isEnter && completeOnEnter) this.updateValue();
        }

        // We want to style "pending" values differently from actually chosen values
        // when addUnknownValues is false
        
        if (!this.addUnknownValues && item == this) this._markPending();
    },

    // check for Tab keypress (for completeOnTab) on keyDown rather than keypress
    // This is required for Safari where the focus shift to the next item occurs before
    // any Tab keypress event actually fires.
    shouldCompleteOnTab : function () {
        return this.completeOnTab || (this.addUnknownValues == false);
    },
    shouldCompleteOnEnter : function () {
        if (this.addUnknownValues == false) return true;
        else if (this._userNavigated) return true;
        else if (this.completeOnEnter != null) return this.completeOnEnter;
        else {
            return ((this.form && !this.form.isSearchForm) ||
                    !this._getOptionsFromDataSource());
        }
    },
    handleKeyDown : function () {
        if (!this.hasFocus || this.isReadOnly()) return this.Super("handleKeyDown", arguments);

        var keyName = isc.EH.lastEvent.keyName,
            isTab = keyName == this._$Tab,
            isEnter = keyName == this._$Enter,
            pickList = this.pickList,
            pickListVisible = this._isPickListVisible();
        this._completeOnTabValue = null;

        var shouldComplete = isTab   && this.shouldCompleteOnTab() ||
                             isEnter && this.shouldCompleteOnEnter();
        
        if ((isTab   && this.fetchValueOnTab) ||
            (isEnter && this.fetchValueOnEnter))
        {
            var pendingOrFetchingData = this._pendingFetchOnPause() || this._loadingData();

            if (shouldComplete && pickListVisible && !pendingOrFetchingData && 
                pickList != null && pickList.data != null && !pickList.data.isEmpty())
            {
                this._fireTabCompletion();
            } else {
                var elementValue = this.getEnteredValue(),
                    displayField = this.getDisplayFieldName(),
                    shouldFetchMissingValue = this.shouldFetchMissingDisplayValue(elementValue);

                // Optimization: If all rows are cached, or we aren't fetching data and all matching
                // rows are cached, then search localData.
                if (displayField != null && pickList && (isc.isA.ResultSet && isc.isA.ResultSet(pickList.data)) &&
                    (pickList.data.allRowsCached() ||
                     (!pendingOrFetchingData &&
                      pickList.data.localData &&
                      pickList.data.allMatchingRowsCached())))
                {
                    shouldFetchMissingValue = false;

                    var rs = pickList.data,
                        record = rs.localData.find(displayField, elementValue);
                    if (record != null) this._addRecordToDisplayFieldCache(record);
                }

                if (shouldFetchMissingValue) {
                    this._checkForValueFieldValue(elementValue);
                    if (this.clearEnteredValuePendingReply) {
                        this.setElementValue("");
                    }
                } else {
                    this._updateValue(elementValue, true);
                }

                if (pickList) pickList.hide();
            }

        } else if (shouldComplete) {
            // If we're currently pending a filter completion, wait for the filter to
            // complete before completing. Hang onto a 'completeOnTabValue' which we'll
            // check when the fetch completes.
            if (this._loadingData()) {
                this._completeOnTabValue = this.getEnteredValue();
                if (this.clearEnteredValuePendingReply) {
                    this.setElementValue("");
                }
            } else if (pickListVisible) {
                this._fireTabCompletion();
            }
        }

        return this.Super("handleKeyDown", arguments);
    },
    _loadingData : function () {
        return (this._queuedFetch || this._fetchingPickListData);
    },

    chooseCurrentPickListValue : function () {
        var pickList = this.pickList;
        var selection = pickList.getSelectedRecord();
        if (selection != null) pickList.itemClick(selection);
    },
    _fireTabCompletion : function () {
        this.chooseCurrentPickListValue()
    },

    // TextItem will call 'refreshDisplayValue' on blur to display the current data-value
    // remapped to display value.
    // This is intended to handle cases where there's a custom parser / formatter pair that
    // are not 1:1 [EG a forgiving date parser]. In this case a user may tab into a field,
    // modify the textual value to something that parses back to the underlying data value, then
    // tab out again and this method is responsible for displaying the formatted value again.
    // However -this interferes with our completeOnTab behavior since we would clobber the
    // user entered value and when the fetch completes _completeOnTabValue would no longer match
    // the result of 'getEnteredValue()'. -- therefore if we're currently
    // "_loadingData()" - IE performing asynchronous keypress handling or fetch - 
    // don't clobber the user-entered value.
    refreshDisplayValue : function () {
        if (this._loadingData()) return;
        
        if (this._pendingElementValue) return;
        return this.Super("refreshDisplayValue", arguments);
    },
    
    // Expose getEnteredValue() - commonly useful for pickLists
    //> @method comboBoxItem.getEnteredValue()
    // @include textItem.getEnteredValue()
    // @visibility external
    //<

    //> @attr comboBoxItem.addUnknownValues (Boolean : true : IRW)
    // This property controls whether the user can enter a value that is not present in the
    // set of options for this item.
    // <P>
    // If set to false, the value the user enters in the text box is essentially used to filter the 
    // set of options displayed in the pickList.
    // <P>
    // In this mode, when focus is taken from the field, if the entered value does not match
    // any entries in the +link{valueMap} or +link{optionDataSource}, it will be discarded. Note 
    // that in this mode, +link{comboBoxItem.completeOnTab} behavior is automatically enabled
    // so if the user enters a valid partial value such that one or more options is displayed in 
    // the pickList, and hits the Tab key, the first matching option will be chosen automatically.
    // In this mode the user may also hit the <code>"Escape"</code> key to discard their edits.
    // <P>
    // Note also that when <code>addUnknownValues</code> is set to false, the underlying value
    // returned by +link{formItem.getValue(),getValue()} will not be updated until a value is explicitly chosen. This
    // means any change or changed handlers will not fire directly in response to the user typing
    // in the field - they will fire when the user actually selects a value, or takes focus from 
    // the field.
    // <P>
    // If this property is set to true, the user is not limited to entering values present in
    // the set of options for the item. Instead the set of options essentially become a set of
    // suggestions that may be used, or the user can enter an entirely new value.
    // @visibility external
    //<
    // Notes: 
    // - If the valueMap is an object mapping data values to display values, or there
    //   is an optionDataSource specified with both display and value fields, should we allow
    //   the user to enter *data* values rather than display values?
    //   o if addUnknownValues is false, this is easy. We would ignore an entered value that
    //     didn't match a display value, even if it happened to be a data value.
    //     However it can be handled (if there's an optionDataSource) by
    //     setting the filterFields for the item to include the valueField - that way if the
    //     developer enters a data value, it'll show up in the pickList, and complete on tab.
    //   o if addUnknownValues is true this is less likely to be an issue since you'd be unlikely
    //     to be applying a valueMap on a field that supported freeform text entry. However we
    //     have to treat an entered value that doesn't match any display value as a data value of
    //     of course -- so if there is a matching data value in the valueMap already we would 
    //     select it. The trick is that we'll not update the item to show the display value
    //     until the user takes focus from the field. This is handled by logic in handleEditorExit()
    //     to call _checkForDisplayFieldValue() if necessary.
    
    
    addUnknownValues:true,
    
    //> @attr comboBoxItem.allowEmptyValue (Boolean : true : IR)
    // If +link{comboBoxItem.addUnknownValues} is <code>false</code>, this property 
    // determines whether
    // the user can clear the comboBoxItem value, or whether they are constrained to
    // choosing one of the available options (in which case clearing the text box will
    // simply revert to the last picked value when the user leaves the field).
    // <P>
    // See also +link{specialValues} as a way of providing several different special values in
    // addition to an empty value, such as "Invalid".  Note that setting
    // <code>specialValues</code> disables the use of <code>allowEmptyValue</code> - see
    // details of how to have an empty value while using <code>specialValues</code> in 
    // in +link{specialValues,the <code>specialValues</code> documentation}.
    //
    // @visibility external
    //<
    allowEmptyValue:true,
    
    // mapEmptyDisplayValue - if the user enters the emptyDisplayValue in the text-box,
    // should we attempt to match the value in the valueMap / optionDataSource?
    
    mapEmptyDisplayValue:false,
    
    
    // On editorExit:
    // - if addUnknownValues is false, check to see if we have a valid displayValue in our
    //   text box - if so, update our value (pick the appropriate explicitly entered option,
    //   or drop the value if there isn't one).
    //   - If we are currently fetching data for the pickList there are 2 exceptions here:
    //   1) User hit tab - in this case we will have set up _completeOnTabValue and when the
    //      fetch completes we'll auto-select the first option. Nothing else to do here
    //   2) User didn't hit tab but just took focus from the item. In this case we'll detect
    //      we don't have focus and check for the validity of the entered value on filterComplete,
    //      so again nothing to do here.
    //   Both these cases are ultimately handled by _updateValueForFilterComplete
    // 
    // - if addUnknownValues is true updateValue() should already have stored the appropriate 
    //   data value.
    //   If the value entered matched some entry in our pickList, and we have a displayField,
    //   we should have already mapped back to a data value and stored.
    //   Otherwise we have stored the entered value as a display value (User entered an "unknown"
    //   value).
    //   In this case, if we have both a displayField and a dataField, the
    //   entered value may be the data field value for some entry in our optionDataSource.
    //   This is a tricky case - we have to store the user-entered value as a data value
    //   so to keep it consistent with a call to 'setValue()' we should display the
    //   displayValue for the option if there is one.
    //   *We avoid this while focus is actually in the item to avoid changing user entered
    //    partial filter values etc.
    //   - call mapValueToDisplay to determine if we have a loaded entry where the entered value
    //     matches a data value, and if so display it.
    //   If not:
    //   1) We may have a pending fetch - allow _updateDisplayValueForNewData() to update the
    //   display value when the fetch completes
    //   2) Otherwise we need to kick off a new fetch to get the display value - in this case
    //   use "checkForDisplayFieldValue()" to kick off that new fetch.
    

    handleEditorExit : function () {
        // Assertion: all the logic below handles editor exit due to click outside, tab keypress and
        // essentially handles resolving the entered value to a live data value and displaying it
        // if necessary.
        // If the user actually clicked on the pickList we'll get an elementBlur ==> editorExit
        // on mouseDown, before itemClick has had a chance to fire.
        // Skip all this logic in that case.
        
        if (this._mouseDownInPickList()) return;

        // clear the _userNavigated flag used to determine whether to complete on enter keypress
        this._userNavigated = false;

        var elementValue = this.getEnteredValue(),
            value = this.getValue();
        
        if (this._completeOnTabValue != null &&
            (this._showingInFieldHintAsValue || this._completeOnTabValue == elementValue) &&
            !this._loadingData()) 
        {
            this._fireTabCompletion();
            this._completeOnTabValue = null;
            // re-fetch the elementValue - it should now be good if there was a valid completion
            elementValue = this.getEnteredValue();
        }

        // If completeOnTabValue is populated we're waiting for a fetch to complete to
        // update our value so skip logic to update value and/or displayed value on exit.
        // Also if we've been set to the "Loading..." marker value, this is not a user-entered
        // value and we don't want to issue a fetch against the ODS displayField to get
        // back an associated data value!
         
        if (this._completeOnTabValue == null && !this._showingLoadingDisplayValue) {
            if (this.addUnknownValues) {

                var ods = this.getOptionDataSource(),
                    displayField = this.getDisplayFieldName();

                
                if (value == elementValue &&
                    (ods == null || (displayField != null && value != null && 
                    !this.shouldFetchMissingValue(value)))) 
                {
                    // map the value passed to the visible value as necessary
                    var displayValue = this.mapValueToDisplay(value);
                    if (displayValue != value) {
                        if (isc.isAn.emptyString(displayValue) && 
                            this._getShowHintTextInDataElement()) 
                        {
                            this._showInFieldHint();
                        } else {
                            this.setElementValue(displayValue);
                            
                        }
                    }

                // Otherwise, consider the value to be a display value. If we have an ODS and
                // display field, check whether we have a data value for it. If not, try to fetch
                // a data value for this display value using _checkForValueFieldValue().
                } else if (ods != null && displayField != null
                           && elementValue != null && elementValue != "" &&
                           this.shouldFetchMissingDisplayValue(elementValue)
                          )
                {
                    
                    this._checkForValueFieldValue(elementValue);
                }
            } else {
                // If we're currently loading data wait for that to complete - otherwise
                // validate the entered value against our live pickList data and drop it
                // if its an "unknown" value now.
                if (!this._loadingData()) {
                    // If the user entered something in the text box that actually matches
                    // a value in the pickList, update the data value. Otherwise reset to
                    // original value.
                    
                    if (this.isUnknownValue(elementValue)) {
                        this.setElementValue(this.getDisplayValue());
                    } else {
                       this._updateValue(elementValue, true);
                    }
                }
            }
        }
        // Don't validateOnExit if we're waiting on a complete-on-tab from an asynch fetch.
        this._suppressValidateOnEditorExit = (this._completeOnTabValue != null);
        var returnVal = this.Super("handleEditorExit", arguments);
        this._suppressValidateOnEditorExit = null;
        return returnVal;
    },
    
    // On redraw, if addUnknownValues is false and the user was entering an (unsaved)
    // value in the text box when the redraw occurred, we still want to retain the pending
    // value in the text-box
    // Override showValueAfterDraw to handle this
    _showValueAfterDraw : function (redraw) {
        if (redraw && this._pendingElementValue) {
            this.setElementValue(this._pendingEnteredValue);
        } else return this.Super("_showValueAfterDraw", arguments);
    },

    // Override filterComplete to 
    // - Add specialValues to the top of the list if this.specialValues and not separateSpecialValues
    filterComplete : function () {
        // If we are allowing empty or special values we always to 'basic' filtering, which means
        // we can directly add the entries to the ResultSet's cache

        if (this._getOptionsFromDataSource()) {
            var specialValues = this._getSpecialValues(false);
            if (specialValues) {
                if (this.specialValues && this.separateSpecialValues) {
                    this.setSeparateSpecialValues(specialValues);
                } else {
                    var data = this.pickList.getOriginalData(); // handle the pl being grouped
                    if (isc.isA.ResultSet(data)) {
                        var recordAdded = data.addSpecialValueRecords(this.getValueFieldName(), this.getDisplayFieldName(), specialValues);
                        if (recordAdded && this.pickList) this.pickList.markForRedraw();
                    }
                }
            }
        }
        var interfaceFilterComplete = isc.PickList.getPrototype().filterComplete;
        interfaceFilterComplete.apply(this, arguments);
    },

    // override updateValueMap to update the list and update the displayed value if necessary.
    updateValueMap : function (refreshDisplay) {
        this.Super("updateValueMap", arguments);

        if (this._clientPickListData) delete this._clientPickListData;
    },

    // Override the method to get pickList data to add special values
    getClientPickListData : function () {
        if (this._clientPickListData) return this._clientPickListData;
        
        var records = isc.PickList.optionsFromValueMap(this),
            valueField = this.getValueFieldName();

        var specialValues = this._getSpecialValues(false);
        if (specialValues) {
            if (this.separateSpecialValues) {
                this.setSeparateSpecialValues(specialValues);
            } else {
                records.addListAt(specialValues, 0);
            }
        }

        this._clientPickListData = records;
        
        return records;
    },

    // Reimplement/override _updateValueForFilterComplete()
    // If completeOnTab is true and the user hit tab mid-filter, we can complete now.
    // Also if addUnknownValues is false and focus was taken from the field mid
    // filter (without a tab keypress), accept or reject the typed value now.
    _$none: "none",
    _updateValueForFilterComplete : function (response,data,request) {

        // Always select the default item at this point since we have the latest data
        this.selectDefaultItem();
        if (!this.hasFocus) {
            var elementValue = this.getEnteredValue();            
            
            if (this._completeOnTabValue != null &&
                (this._showingInFieldHintAsValue ||
                 (this.hasFocus && this.clearEnteredValuePendingReply && !elementValue) ||
                 this._completeOnTabValue == elementValue))
            {
                this._fireTabCompletion();
                this._completeOnTabValue = null;
            }
            // Check for 'unknown' value even if _completeOnTabValue was true.
            // We may have not returned any matches in which case _fireTabCompletion() would no-op
            // and we should now clear out the element value and restore any previously saved value
            if (!this.addUnknownValues) {
                // If the value is an unknown value...
                if (this.isUnknownValue(elementValue)) {
                    // If this ComboBoxItem no longer has focus, restore the previously-saved value,
                    // which should either be empty or a confirmed known value.
                    if (!this.hasFocus) {
                        var displayValue = this.getDisplayValue();
                        if (isc.isAn.emptyString(displayValue) && 
                            this._getShowHintTextInDataElement())
                        {
                            this._showInFieldHint();
                        } else {
                            this.setElementValue(displayValue);
                        }

                        // Explicitly clear the "pending" flag
                        
                        this._markNotPending();
                    }

                // Otherwise, the value is known. In most cases, we update this ComboBoxItem's
                // value according to the entered value. However, there is a special exception:
                // in the case of a MultiComboBoxItem, saving the value will result in a new button
                // for the value (or flash an existing button if the value has already been selected).
                // We only want buttons to be added for values that have been explicitly selected
                // by the user, whether the user tab-completed the value, pressed the Enter/Return
                // key, or clicked on/activated an entry of the pickList menu. So, in the case
                // of the embedded comboBoxItem of an MCBI, require that this ComboBoxItem *not* be
                // focused.
                } else if (!isc.isA.MultiComboBoxItem || !isc.isA.MultiComboBoxItem(this.creator) ||
                           !this.hasFocus)
                {
                    // if there was a selected record before this filter ran, and it has the 
                    // same value and elementValue, just reselect the record
                    var r = this.getSelectedRecord();
                    if (r && r[this.valueField] == this._value && r[this.displayField] == elementValue) {
                         
                        this.pickList.selection.selectSingle(r);
                    } else {
                        this._updateValue(elementValue, true);
                        
                        elementValue = this.getEnteredValue();
                        if (this._getShowHintTextInDataElement() &&
                            (elementValue == null || isc.is.emptyString(elementValue)))
                        {
                            this._showInFieldHint();
                        }
                    }

                    // Clear the "pending" flag because the pending state denotes when the
                    // entered value does not match the display value for the data value, and
                    // we just updated the value to match the entered value.
                    this._markNotPending();
                }
            }

            // If validateOnExit is true, we suppressed it in handleEditorExit as we didn't
            // want to validate the partially entered value.
            // In this case, force validation now.
            this._performValidateOnEditorExit(this.getValue());
        }

        // call _updateDisplayValueForNewData()
        // This is overridden in PickList.js
        // For ComboBoxItems, if addUnknownValues is true, and we just loaded a record
        // for which the entered value is the display value, this method will call 
        // 'updateValue()' which will save out the relevant data value.
        // Otherwise this method will update the textbox to show the displayValue for the
        // item (standard behavior) iff the text box doesn't currently have focus - which
        // ensures we don't change the display value mid text-entry.
        this._updateDisplayValueForNewData();
    },

    // Override the method to update display values in special values
    updateDisplayValueMap : function (needsRefresh) {
        this.Super("updateDisplayValueMap", arguments);

        // SpecialValues can be used to pull existing records
        // to the top for easy selection. The original record
        // remains in place as well. When the display field
        // changes on the existing record it must be updated
        // in the special values array as well.
        if (this._specialValues && this._displayFieldCache) {
            var displayField = this.getDisplayFieldName();
            if (displayField) {
                var cache = this._displayFieldCache,
                    valueField = this.getValueFieldName(),
                    specialValues = this._specialValues,
                    updatedSpecialValues = false
                ;
                for (var i = 0; i < specialValues.length; i++) {
                    var specialValue = specialValues[i],
                        value = specialValue[valueField],
                        record = cache.find(valueField, value)
                    ;
                    if (record != null) {
                        specialValue[displayField] = record[displayField];
                        updatedSpecialValues = true;
                    }
                }
                if (updatedSpecialValues && this.separateSpecialValues) {
                    if (this.separateValuesList) this.separateValuesList.markForRedraw();
                }
            }
        }
    },

    //> @attr comboBoxItem.pendingTextBoxStyle (CSSStyleName : null : IRW)
    // Optional "pending" style for this item's text box.
    // <P>
    // If +link{comboBoxItem.addUnknownValues} is false, when the user modifies the
    // value displayed in the combobox item text box, the underlying data value (as returned
    // from item.getValue()) is not immediately updated - instead the value is used to filter
    // the set of results displayed in the comboBoxItem pickList.
    // <P>
    // While the comboBoxItem is in this pending state (where the result of getEnteredValue() will
    // not necessarily match the display value for whatever is returned by getValue()), the 
    // pendingTextBoxStyle may be applied to the text box for the item.
    // <P>
    // When the element value is updated to display the actual value for the item (typically due
    // to the user selecting a value from the pickList), the standard 
    // +link{TextItem.textBoxStyle} will be reapplied.
    // <P>
    // May be left unset in which case the standard text box style is always applied.
    // Has no effect if +link{addUnknownValues} is true.
    // @visibility external
    //<
    
    _markPending : function () {
        var wasPending = this._pendingElementValue;
        this._pendingElementValue = true;
        this._pendingEnteredValue = this.getEnteredValue();
        if (!wasPending) this.updateState();
    },
    _markNotPending : function () {
        if (!this._pendingElementValue) return;
        this._pendingElementValue = null;
        this._pendingEnteredValue = null;
        this.updateState();
    },
    getTextBoxStyle : function () {
        if (this.pendingTextBoxStyle == null || !this._pendingElementValue || this._isPrinting()) {
            if (this._isPrinting()) return isc.TextItem.getInstanceProperty("textBoxStyle");
            return this.Super("getTextBoxStyle", arguments);
        }
        return this.pendingTextBoxStyle;
    },
    
    // Modify remappedDisplayValueUnchanged to check for the case where a value with 
    // explicitly picked and has a display value which happens to match our current display value
    // In this case we'll return false to allow _updateValue to proceed.
    _remappedDisplayValueUnchanged : function (newValue) {
        if (this._pickedReverseValueMap) {
            for (var i in this._pickedReverseValueMap) {
                if (i == newValue) {
                    return false;
                }
            }
        }
        return this.Super("_remappedDisplayValueUnchanged", arguments);
    },


    // Override _updateValue()
    // If addUnknownValues is false, don't update the data value based on a change to the
    // text-box value unless the value was explicitly picked
    // from the pickList (this includes enter keypress, completeOnTab), or
    // focus is being taken from the item (in which case we pass in the forceSave param).
    _updateValue : function (value, forceSave) {
        // Clicking on the pickList first blurs the data element, causing
        // _updateValue() to be called. If the mouse is down in the pickList
        // then suppress the save.
        var mouseDownInPickList =  this._mouseDownInPickList(),
            suppressSave = (!forceSave &&
                            !this._valuePicked &&
                            (this.addUnknownValues == false || mouseDownInPickList));
                
        // If allowEmptyValue is true, and the user actually clears the text value,
        // actually save out the empty value, even if addUnknownValues is false.
        var updateFilterForEmptyValue = false;
        if (this.addUnknownValues == false && 
            suppressSave && this.allowEmptyValue && (value == "")) 
        {
            suppressSave = false;
            // If addUnknownValues is true, a user may enter a character, then
            // delete it.
            // in this case suppressSave will be marked true (due to this conditional), but
            // we'll compare against the stored value and avoid calling Super.
            // Catch this case and update the filter so the pickList doesn't continue to
            // reflect the filter for the character the user entered.
            if (!mouseDownInPickList) updateFilterForEmptyValue = true;
        }
        if (!suppressSave) {
            this._markNotPending();
            // Catch the case where we're already updated
            var dataValue = this.mapDisplayToValue(value);
            
            if (this._valuePicked) this.explicitChoice = this._valuePicked;
            
            if (this.compareValues(dataValue, this._value)) {
                if (updateFilterForEmptyValue) {   
                    this.refreshPickList(value);
                }
                return true;
            }
            this.explicitChoice = this._valuePicked;
            return this.Super("_updateValue", arguments);
        
        // If addUnknownValues is false just refilter the pickList
        } else {
            
            if (this.changeOnKeypress && 
                this.length != null && isc.isA.String(value) && value.length > this.length) {
                value = value.substring(0, this.length);
                this.setElementValue(value);
            }
            

            // continue to filter the picklist based on the user-entered value
            
            if (this._mouseDownInPickList()) return;
            
            if (!this._pendingElementValue) return;

            
            if (this._pendingEnteredValue != this.getElementValue()) {
                this._pendingEnteredValue = this.getElementValue();
            }

            
            if (this._pendingEnteredValue == this._lastFilterValue) return;
            this._lastFilterValue = this._pendingEnteredValue;
            
            this.refreshPickList(value);
        }
    },

    _minimalUpdateValue : function (newValue) {
        this.refreshPickList(newValue);
    },

    // Override handleChanged to re filter the list on every change (which occurs on every 
    // keypress that changed the entered value).  
    // Note handleChanged() is not called if addUnknownValues is false and the user
    // entered value has no completion - in that case, pickList refersh is triggered directly from
    // _updateValue()
    handleChanged : function (newValue, oldValue) {
        //this.logWarn("handleChange: newValue: " + newValue + " oldValue: " + oldValue);
        var returnVal = this.Super("handleChanged", arguments);
        this.refreshPickList(this.getEnteredValue());
        return returnVal;
    },
    // refresh the pickList for a change in the user-entered value
    // Note that newValue is a display value.
    refreshPickList : function (newValue) {
    
//         this.logWarn("refreshPickList called with newValue: " + newValue +
//                      ", valuePicked: " + this._valuePicked + this.getStackTrace());
        
        var isEmpty = (!newValue || newValue == isc.emptyString);
        
        // clear out the shownOnEmpty string if we're not empty any more. This means if
        // the user clicks the picker icon [pick list shows], then enters a character,
        // [pick list filters], then deletes that character, the pick list will hide
        if (!isEmpty) delete this._shownOnEmpty;

        // if the text field is empty, ensure the list is hidden unless it was explicitly
        // shown with an empty value (Arrow down keypress / picker click)
        var pickList = this.pickList,
            pickListVisible = this._isPickListVisible();
        if (isEmpty && !this._shownOnEmpty) {
            if (pickListVisible) pickList.hide();
        } else if (this.showPickListOnKeypress || pickListVisible) {
            // showPickList will set up the pickList initially, or if already set up
            // will re-filter
            // Note - if our value changed in response to the user picking something from the
            // list we don't want to show the pickList again.
            // Note - pass in the second 'queueFetches' parameter - we don't want to kick off
            // multiple server fetches while the user is rapidly typing in the text field
            
            if (!this._valuePicked && this.hasFocus) {
                this._refreshPickList(newValue);
            }
        }
    },
    
    _refreshPickList : function (newValue) {
        
        if (!this.filterWithValue && (this._shownWithValue != newValue)) {
            this.filterWithValue = true;
            delete this._shownWithValue;
        }
        this.showPickList(true, true);

    },

    // Override selectDefaultItem to always select the first item in the list.
    // This will happen on every re-filter.
    selectDefaultItem : function () {    
        if (this.pickList == null || this.pickList.destroyed) return;
        
        var selection = this.pickList.selection;
        
        // If we have a record matching our value (or our pending, user-typed value)
        // select it as the default. This ensures that if the user hits tab or enter
        // we auto-complete correctly
        var record;
        
        if (!this.addUnknownValues && this._pendingEnteredValue != null) {
            record = this.getPickListRecordForValue(this._pendingEnteredValue)
        } else {
            record = this.getSelectedRecord();
        }
        if (record) {
            this.pickList.clearLastHilite();
            this.delayCall("selectItemFromValue", [record[this.getValueFieldName()]]);
            return;
        }

        // Otherwise select the first record in the list by default.
        var record = this.pickList.getRecord(0);
        // Don't attempt to select null / loading / separator rows
        if (record == null || Array.isLoading(record) || 
            record[this.pickList.isSeparatorProperty]) return;

        selection.selectSingle(record);
        // Clear last hilite - required so keyboard navigatioin will pick up the current position
        // from the selection, not the last hilite position.
        this.pickList.clearLastHilite();
        this.pickList.scrollRecordIntoView(0);
    },
    
    // In TextItem we turn off the 'shouldFetchMissingValue' logic while the item is 
    // editable (see docs for TextItem.shouldFetchMissingValue).
    // Turn it back on for ComboBoxItem where we do need to handle mapping between
    // display-field and value-field values even for freeform user-entered values.
    _suppressFetchMissingValueIfEditable:false,

    // Override getSelectedRecord to look at the pickList if present
    getSelectedRecord : function () {
        var record = this.Super("getSelectedRecord", arguments);

        // If we didn't get selected record via 'fetchMissingValues',
        // use the pickList to try to find the selectedRecord in the pickList data
        if (record == null && this._value != null) {
            record = this.getPickListRecordForValue(this._value);
            if (record != null && this.optionDataSource) {
                // Store the record from the pickList in our displayField cache
                
                var isNewRecord = this._addDataToDisplayFieldCache([record]);
                if (isNewRecord) {
                    this._updateSelectedRecord();
                    
                    if (this.getDisplayFieldName() != this.getValueFieldName()) {
                        this.updateDisplayValueMap();
                    }
                }
            }
        }
        return record;
    },

    // Given a value, reach into the pickList and find the associated record.
    getPickListRecordForValue : function (value, fieldName) {
        var record;
        if (this.pickList == null || this.pickList.destroyed) {
            if (this.progressiveLoading === true || this.progressiveLoading === false) {
                if (this.pickListProperties == null) this.picklistProperties = {};
                this.picklistProperties.progressiveLoading = this.progressiveLoading;
            }
            this.makePickList(false);
        }
        if (fieldName == null) fieldName = this.getValueFieldName();
        if (this.pickList && this.pickList.data) {
            record = this.pickList.data.find(fieldName, value);
        }
        return record;
    },

    
    // Override _shouldFetchMissingValue to look in the pickList if we don't find the
    // value in our valueField cache.
    
    _shouldFetchMissingValue : function (value, fieldName) {
        if (this.fetchMissingValues == false) return false;
        var ods = this.getOptionDataSource();
        if (ods == null) return false;
        
        var superShouldFetch = this.Super("_shouldFetchMissingValue", arguments);
        if (superShouldFetch == false) return false;
        if (value != null && ods) {
            
            var recordFromPickList = (this.pickList && this.pickList.data &&
                                        !this.pickList.data._derivedFromValueMapObject) ?  
                                        this.getPickListRecordForValue(value, fieldName) : null;
            if (recordFromPickList != null) {
                // Store the record from the pickList in our displayField cache
                
                this._addDataToDisplayFieldCache([recordFromPickList]);
                // This method will update the valueMap and selectedRecord to pick up
                // the changes.
                this.updateDisplayValueMap(true);
                return false;
            }
        }
        return superShouldFetch;
    },

    // ------------------------
    // Filtering data
    // ------------------------

    // Include useful JSDoc from pickList

    //> @attr comboBoxItem.pickListConstructor (SCClassName : "PickListMenu" : IR)
    // @include PickList.pickListConstructor
    //<

    //> @attr ComboBoxItem.optionDataSource (DataSource | String : null : IRA)
    // @include PickList.optionDataSource
    //<

    //> @attr ComboBoxItem.pickListFields (Array of ListGridField : null : IRA)
    // @include PickList.pickListFields
    //<

    //> @method ComboBoxItem.fetchData()
    // @include PickList.fetchData()
    //<

    //> @attr comboBoxItem.cachePickListResults (boolean : true : IR)
    // For databound pickLists (see +link{pickList.optionDataSource}), by default SmartClient
    // will cache and re-use datasets shown by different pickLists displayed by different
    // SelectItems in an LRU (least recently used) caching pattern.
    // <P>
    // Setting this flag to false avoids this caching for situations where it is too
    // aggressive.
    // <p>
    // Note that this does not control re-use of data <b>within a single pickList</b>.  To
    // control when client-side filtering is used in ComboBoxItem, see
    // +link{ComboBoxItem.useClientFiltering} and +link{ComboBoxItem.filterLocally}.
    //
    // @visibility external
    //<
    
    //> @attr ComboBoxItem.fetchDisplayedFieldsOnly (boolean : null : IRA)
    // @include PickList.fetchDisplayedFieldsOnly
    //<
 
    //> @attr ComboBoxItem.optionFilterContext (DSRequest Properties : null : IRA)
    // @include PickList.optionFilterContext
    //<
 
    //> @attr ComboBoxItem.optionOperationId (string : null : [IR])
    // @include FormItem.optionOperationId
    // @visibility external
    //<
 
    
    //> @attr ComboBoxItem.displayField (string : null : IRW)
    // @include PickList.displayField
    // @visibility external
    //<
    
    //> @attr ComboBoxItem.valueField   (string : null : IRW)
    // @include PickList.valueField
    // @visibility external
    //<
    
    //> @method ComboBoxItem.getDisplayFieldName() ([A])
    // @include PickList.getDisplayFieldName()
    // @visibility external
    //<
        
    //> @method ComboBoxItem.getValueFieldName()
    // @include PickList.getValueFieldName()
    // @visibility external
    //<
    
    //> @attr ComboBoxItem.filterLocally
    // @include PickList.filterLocally
    // @visibility external
    //<

    //> @method ComboBoxItem.getSelectedRecord()
    // @include FormItem.getSelectedRecord()
    // @visibility external
    //<
    
    //> @attr ComboBoxItem.pickListCriteria (Criteria : null : IRWA)
    // @include PickList.pickListCriteria
    // @visibility external
    //<
    
    //> @attr ComboBoxItem.showOptionsFromDataSource (boolean : null : IRWA)
    // @include PickList.showOptionsFromDataSource
    // @visibility external
    //<
    
    //> @attr ComboBoxItem.pickListProperties (ListGrid Properties : null : IRA)
    // @include PickList.pickListProperties
    // @visibility external
    //<

    //> @attr ComboBoxItem.sortField (String | Array of String | int : null : IR)
    // @include PickList.sortField
    // @visibility external
    //<

    //> @attr ComboBoxItem.initialSort (Array of SortSpecifier : null : IR)
    // @include PickList.initialSort
    // @visibility external
    //<
    
    // Default pickList interface 'filtering' basically returns every record (this is the
    // desired behavior for select items).
    // For pickLists we want to show only the subset of options that matches the string
    // currently in the combo box text element.

    // Set filterDisplayValues to ensure that if we're working with client side options,
    // the filter criteria are recognized as the display values for the data not the raw values.
    filterDisplayValue:true,
    
    //> @attr ComboBoxItem.autoFetchData   (Boolean : false : [IRA])
    // If this combo box retrieves its options from a <code>dataSource</code>, should options
    // be fetched from the server when the item is first written out, or should this fetch be
    // delayed until the user opens the pickList.
    //
    // @visibility external
    // @see PickList.optionDataSource
    //<
    autoFetchData:false,

    //> @attr ComboBoxItem.filterWithValue (boolean : varies : [RA])
    // Read-only property set by the ComboBoxItem to indicate whether we should use the 
    // current typed-in value as part of the filter criteria returned by 
    // +link{ComboBoxItem.getPickListFilterCriteria()}.  You can check this flag in order to
    // mimic the ComboBoxItem's default behavior if you provide a custom implementation of
    // <code>getPickListFilterCriteria()</code>.
    // @see comboBoxItem.getPickListFilterCriteria()
    // @see comboBoxItem.filterFields
    // @visibility external
    //<
    
    filterWithValue:false,
    
    //> @attr ComboBoxItem.alwaysFilterWithValue (boolean : false : [RA])
    // If set to true, the default implementation of 
    // +link{comboBoxItem.getPickListFilterCriteria()} will always add the current item's value
    // to any specified pickListFilterCriteria, regardless of the current value of 
    // +link{ComboBoxItem.filterWithValue}.
    // <P>
    // This was the default behavior for SmartClient version 5.6 and earlier.
    // @visibility internal
    //<
    
    //> @attr ComboBoxItem.filterFields (Array of String : null : IR)
    // As the user types into this item's textBox, a comboBoxItem will show the
    // pick-list of options, and filter the set of results displayed by the current value in the
    // text box. For a databound comboBoxItem, by default the entered value is filtered against
    // the +link{comboBoxItem.displayField,displayField} if one is specified, otherwise the
    // +link{comboBoxItem.valueField,valueField}.
    // <P>
    // This attribute allows the developer to explicitly change which fields to filter against,
    // causing the user-entered text to be matched against any of the specified set of fields from 
    // the +link{optionDataSource}.
    // <P>
    // This essentially causes +link{getPickListFilterCriteria()} to return an +link{AdvancedCriteria}
    // object representing "field1 starts with value or field2 starts with value or ...".  The
    // +link{OperatorId,operator} used is controlled by +link{textMatchStyle} as usual, that is,
    // "startsWith" implies the operator "iStartsWith, "substring" implies "iContains" and "exact"
    // implies "iEquals".
    // <P>
    // The most common use case for this setting would be when a comboBoxItem is showing multiple
    // +link{comboBoxItem.pickListFields} - if the same set of fields is specified as
    // +link{comboBoxItem.filterFields}, the user can use the text-box to filter against
    // whichever fields are visible in the pickList.
    // <P>
    // <smartclient>For finer grained control over comboBoxItem filtering, the 
    // +link{comboBoxItem.getPickListFilterCriteria()} method may be overridden.</smartclient>
    // <smartgwt>For finer grained control over comboBoxItem filtering, the
    // +sgwtLink{comboBoxItem.setPickListFilterCriteriaFunction()} may be specified.</smartgwt>
    // @visibility external
    //<
    
    //> @groupDef comboBoxFiltering
    // The criteria used to decide which options should appear in
    // the drop-down +link{PickList} shown by a ComboBox are determined as follows.
    // <P>
    // While the user is typing in a value, the
    // +link{comboBoxItem.getPickListFilterCriteria()} method will return
    // the typed-in value as part
    // of the criteria, so that only matching values are shown.  Matching is determined by the
    // +link{comboBoxItem.textMatchStyle,textMatchStyle}.  Note that the +link{comboBoxItem.filterFields}
    // attribute may be used to determine which fields filtering is performed against for
    // databound comboBoxItems.
    // <P>
    // If the user explicitly shows the down-down pickList, via either clicking on the drop
    // down icon or using the <i>Ctrl+Arrow Down</i> key combination, the typed-in value is 
    // ignored for filtering.
    // <P>
    // If included in the criteria, the typed-in value will be included as a value for the 
    // +link{ComboBoxItem.displayField,displayField} (or for the
    // +link{ComboBoxItem.valueField,valueField} if <code>this.displayField</code> is
    // unspecified).
    // <P>
    // Static criteria, specified via +link{formItem.optionCriteria, optionCriteria} or
    // +link{comboBoxItem.pickListCriteria,pickListCriteria}, will always be included by the
    // default implementation (combined with the typed in value if appropriate).
    // <P>
    // <smartgwt>For custom pick list filter criteria, we recommend 
    // calling the 
    // {@link com.smartgwt.client.widgets.form.fields.ComboBoxItem.setPickListFilterCriteriaFunction}
    // method.</smartgwt>
    // <smartclient><code>getPickListFilterCriteria()</code> may be overridden for
    // custom behavior.</smartclient>
    // If you are implementing your own pickList filter criteria, the
    // <b>read-only</b> property +link{ComboBoxItem.filterWithValue,this.filterWithValue} can
    // be read to determine whether the ComboBox would ordinarily ignore the typed-in value for
    // filtering.  Note that in addition to cases where the user explicitly shows the pickList,
    // <code>filterWithValue</code> will also be <code>true</code> during a call to 
    // +link{ComboBoxItem.fetchData()} on a databound comboBox.
    // <P>
    // <b>NOTE:</b> The defaut implementation of this method will return an 
    // +link{AdvancedCriteria} object if multiple +link{comboBoxItem.filterFields} are specified, or
    // if there are field collisions between any specified static +link{formItem.optionCriteria, optionCriteria},
    // +link{comboBoxItem.pickListCriteria,pickListCriteria} and the entered value. AdvancedCriteria are not supported by
    // all DataSource types, including the built-in server-side SQL dataSources in SmartClient Pro
    // edition (though they are supported by SQL dataSources in Power and Enterprise editions).
    // <P>
    // <b>Client-Side Filtering</b><br>
    // By default, the ComboBoxItem will automatically use client-side filtering whenever
    // it receives a complete set of results for a given search string, and then the 
    // user types more letters (so reducing the results further).
    // <p>
    // Client-side filtering may malfunction if the server filtering behavior can't be 
    // replicated client-side (for example, Google Search).  To disable client-side 
    // filtering so that the comboBox always contacts the server for data whenever the 
    // use changes the search string, set +link{comboBoxItem.useClientFiltering} to false.
    // <p>
    // However, disabling client-side filtering will slow down the UI and cause more 
    // round-trips to the server, so if client-side filtering is malfunctioning but
    // <i>should work</i>, try to correct the problem rather than disable the feature.
    // <p>
    // For example, if the initial search works correctly but adding more letters
    // always causes zero matches, most likely the Records returned by the server lack
    // values for the field(s) targeted by the filter criteria, or the field values 
    // returned by the server don't match the criteria values. 
    // <P>
    // View the returned data in the RPC tab in the Developer Console and enable the
    // "ResultSet" log category in the "Results" tab to troubleshoot how the filter 
    // criteria are being applied to data, and look closely at your
    // settings for +link{comboBoxItem.valueField, valueField} and +link{comboBoxItem.displayField,displayField}.
    //
    // @title ComboBoxItem PickList Filtering
    // @visibility external
    //<
    
    //> @attr comboBoxItem.useClientFiltering (Boolean : null : IRA)
    // @include pickList.useClientFiltering
    // @visibility external
    //<
    
    //> @method  ComboBoxItem.getPickListFilterCriteria()    (A)
    // +link{group:stringMethods,StringMethod} to return filter criteria for options displayed for
    // this item.  
    // <P>
    // See +link{group:comboBoxFiltering} for details on how pickList filter criteria
    // are calculated by default for a comboBoxItem.
    //    
    // @return (Criteria) criteria to be used for databound or local filtering
    // @visibility external
    //<
    getPickListFilterOperator : function () {
        return (this.textMatchStyle == "startsWith" ? "iStartsWith" : 
                                  this.textMatchStyle == "exact" ? "iEquals" : "iContains")
    },
    getPickerSearchValue : function () {
        if (this.pickerSearchForm && this.pickerSearchForm.isVisible()) {
            return this.pickerSearchForm.getValue("s");
        }

        var value = this._completeOnTabValue;
        if (value == null) {
            // if we've explicitly set our "loading..." prompt, ignore that and
            // use our underlying value [which presumably was the element value when
            // the fetch was kicked off]
            
            if (this._showingLoadingDisplayValue) {
                value = this.getValue();
            } else value = this.getEnteredValue();
        }        
        return value
    },
    getPickListFilterCriteria : function () {
        var crit = this.getOptionCriteriaCopy(),
            pickListCriteria = this.pickListCriteria;

        if (pickListCriteria != null) {
            if (crit == null) crit = pickListCriteria;
            else {
                crit = isc.DataSource.combineCriteria(crit, pickListCriteria);
            }
        }
        if (this.alwaysFilterWithValue || this.filterWithValue) {
            // if filterFields are specified, explicitly use them.
            // Otherwise filter against the field that will actually be displayed
            // in the text box - this will be the displayField if specified, otherwise
            // the valueField.
            var filterFields = this.filterFields;
            if (filterFields == null) {
                filterFields = [this.getDisplayFieldName() || this.getValueFieldName()]
            }

            var value = this._completeOnTabValue || this.getPickerSearchValue();
            var liveCrit;
            if (filterFields.length == 1) {
                liveCrit = {};
                liveCrit[filterFields[0]] = value;

            // Multiple fields: Build an advanced criteria object and do 'OR' 
            // matches to filter on each field passed in.
            } else {
                liveCrit = {
                    _constructor:"AdvancedCriteria",
                    operator:"or",
                    criteria:[]
                }
                for (var i = 0; i < filterFields.length; i++) {
                    liveCrit.criteria.add({
                        fieldName:filterFields[i],
                        value:value,
                        operator:this.getPickListFilterOperator()
                    });
                }
            }
            if (crit == null) crit = liveCrit;
            else crit = isc.DataSource.combineCriteria(crit, liveCrit);
        }

        if (crit && this.form) {
            crit = isc.DataSource.resolveDynamicCriteria(crit, this.form.getRuleContext());
        }

        return crit || {};
    },

    //> @attr ComboBoxItem.minimumSearchLength   (integer : null : [IRA])
    // Minimum length in characters before a search is performed. If too few characters
    // are entered the pick list shows +link{ComboBoxItem.searchStringTooShortMessage,
    // searchStringTooShortMessage}.
    //
    // @visibility external
    //<

    //> @attr ComboBoxItem.searchStringTooShortMessage   (String : "Enter a longer search string to search" : [IRA])
    // Message to display in pick list when +link{ComboBoxItem.minimumSearchLength,
    // minimumSearchLength} characters have not been entered.
    //
    // @visibility external
    //<
    searchStringTooShortMessage: "Enter a longer search string to search",

    isEntryTooShortToFilter : function () {
        var value = this.getEnteredValue();
        var tooShort = (value != null && value.length < this.minimumSearchLength);
        var result = ((this.filterWithValue || this.alwaysfilterWithValue) && tooShort);
        return result;
    },
    getEntryTooShortMessage : function () {
        return this.searchStringTooShortMessage;
    },
    // Never refresh for fetched value while focus is in our text box if the user has typed
    // If the user entered a display value we're already showing the right thing
    // If the user entered a data value, we'll remap to display on editor exit.
    _refreshForDisplayValueChange : function () {
        if (!this.hasFocus) return true;
        // Pending --> user has entered something that we haven't stored as this.value
        if (!this.addUnknownValues) return !this._pendingElementValue;
        
        // addUnknownValues:true - in this case we store out the value as the user types and
        // don't store a pending state. Use explicitChoice to determine whether the value was
        // set programmatically or picked from the list as opposed to just being typed.
        return !!this.explicitChoice;
    },
    

    //> @method comboBoxItem.getClientPickListData()
    // @include PickList.getClientPickListData()
    //<

    //> @method comboBoxItem.filterClientPickListData()
    // @include PickList.filterClientPickListData()
    //<

    //> @attr comboBoxItem.textMatchStyle (TextMatchStyle : "startsWith" : IR)
    // @include PickList.textMatchStyle
    //<

    //> @attr comboBoxItem.showAllOptions (boolean : null : IR)
    // @include PickList.showAllOptions
    //<
    
    //> @method ComboBoxItem.dataArrived()
    // @include PickList.dataArrived()
    // @visibility external
    //<

    //> @attr comboBoxItem.separatorRows (Array[] of ListGridRecord : [{isSeparator:true}] : IR)
    // @include PickList.separatorRows
    //<
    
    // When the user blurs from the combo box item we want to ensure we hide the pickList.
    
    elementBlur : function () {
        // note that handleEditorExit() / editorExit are called from super impl of elementBlur
        this.Super("elementBlur", arguments);
        
        
        if (isc.Browser.isMobile) {
            this.delayCall("hidePickListOnBlur", [true], 100);    
        } else {
            this.hidePickListOnBlur();
        }
    },
    hidePickListOnBlur : function (checkFocus) {

        
        delete this._lastFilterValue;

        if (checkFocus && ((this.hasFocus && this.containerWidget.hasFocus) || 
                            (this.pickList && this.pickList.body.hasFocus)) 
            )
        {
            return;
        }
        
        // if we show the header context menu we'll blur the native text item.
        // Don't hide the menu in response to this.
        if (this.pickList && this.pickList._showingHeaderContextMenu) return;

        var pickList = this.pickList,
            EH = isc.EH,
            event = EH.lastEvent;
        //this.logWarn("eventType is: " + event.eventType + 
        //             " event.target: " + event.target +
        //             " pickList.isAncestor: " + pickList.contains(event.target,true) +
        //             ", activeElement: " + this.echoLeaf(document.activeElement));

        // don't hide the pickList if it recieved the mouseDown event that tripped this blur.
        if (this._mouseDownInPickList() || 
                // ignore a delayed blur firing in IE when focus is still in the text input element
                (isc.Browser.isIE && this.getActiveElement() == this.getDataElement())
           ) 
        {
                return;
        }

        // ensure we don't show when 
        // - delayed filter is kicked off
        // - asynchronous filter completes
        delete this._showOnFilter;
        delete this._showOnDelayedFilter;

        // bail if the pickList isn't showing or has been passed to another form item already
        if (!pickList || !pickList.isVisible() || pickList.formItem != this) return;
        
        // hide the pickList
        pickList.hide();
    },
    _mouseDownInPickList : function () {
        var pickList = this.pickList;
        if (!pickList || !pickList.isVisible() || !pickList.isDrawn()) return false;
        var EH = isc.EH, event = isc.EH.lastEvent;
        if (   // the preceding event was a selectionChange event, which fires right after
               // mouseDown in the pickList, strangely triggered by some action we take in
               // Element._getElementFromSelection()
               // Special case because we can't look at event.target to determine if it was
               // a click on the pickList                   
               ( event.eventType == "selectionChange" && 
                 pickList.contains(isc.EH.mouseDownTarget()) )
               ||
    
               // it's a mouseDown/click on the pickList or one of it's subcomponents
               // have to specially check for click in a filter editor
               ((
                    event.eventType == EH.MOUSE_DOWN || event.eventType == EH.CLICK ||
                    event.eventType == EH.POINTER_DOWN || event.eventType == EH.POINTER_CANCEL ||
                    // last event was a mouseMove and the mouse is still over the mouseDown target
                    (event.eventType == EH.MOUSE_MOVE && EH.stillWithinMouseDownTarget())
                ) && 
                (pickList.contains(event.target,true) || (pickList.filterEditor && pickList.filterEditor.getEditForm() == event.target)) 
               )
           ) 
        {
            return true;
        }
        return false;
    },

    editorEnter : function (form, item, value) {
        // Hide in-field hint if being shown
        this._hideInFieldHint();
    },
    editorExit : function (form, item, value) {
        if (this._getShowHintTextInDataElement() &&
            (value == null || isc.isAn.emptyString(value)))
        {
            this._showInFieldHint();
        }
    },

    // Override showPicker to ensure we have focus, and show the pick list
    showPicker : function () {

        var popOutPicker = this.hasPopOutPicker();
        if (!popOutPicker) {
            this.focusInItem();        
        }
        
        this.filterWithValue = false;
        
        var value = this.getEnteredValue(),
            isEmpty = (!value || value == isc.emptyString);
        this._shownWithValue = value;
        this._shownOnEmpty = isEmpty;
        this.showPickList();
        if (popOutPicker && this.pickerSearchForm) {
            if (!isEmpty && 
                this.addUnknownValues && this.isUnknownValue(value)) 
            {
                this.pickerSearchForm.setValues({search:value});
            } else {
                this.pickerSearchForm.clearValues();
            }
            if (this.pickerSaveButton) this.pickerSaveButton.disable();

            
            

        }
        
    },
    
    handleClick : function () {
        if (!this.isDisabled() && !this.isReadOnly() && this.hasPopOutPicker()) {
            this.showPicker();
        }
        return this.Super("handleClick", arguments);
    },

    // pickValue
    // This method is fired when a value is selected from the pick list.
    // update the value of this item, and fire the change handler.
    pickValue : function (value) {

        // Make sure in-field hint is hidden
        this._hideInFieldHint();

        var displayValue = this.mapValueToDisplay(value);
        this.setElementValue(displayValue);
        // If we're focused, select the value so the user can single-click edit it.
        if (this.hasFocus) this.selectValue();
        
        // Hang a flag on the item so that we don't re-show the pick list
        this._valuePicked = true;
        this._pickedReverseValueMap = {};
        this._pickedReverseValueMap[displayValue] = value;
        // Note: updateValue() will fire change handlers and store the new data value.
        // The data value is derived back from the new display value via 'mapDisplayToValue()'
        
        this.updateValue();
        delete this._valuePicked;
    },

    // override setValue map to update the list and update the displayed value if necessary.
    setValueMap : function () {
        this.Super("setValueMap", arguments);
        if (this.pickList) {
            
            if (this.pickList.isVisible()) this.pickList.hide();
            
            // clear out the formItem property - ensures data / fields get re-set up when
            // the pickList is next shown.
            delete this.pickList.formItem
            // dont show the pickList if it's not already visible
            this.setUpPickList(this.pickList.isVisible());
        }
        // The display-version of the value is likely to have changed, so update the element
        // value at this point (but never update the typed-in value when addUnknownValues is
        // false)
        if (this.addUnknownValues) {
            var displayValue = this.getDisplayValue();
            if (isc.isAn.emptyString(displayValue) && this._getShowHintTextInDataElement()) {
                this._showInFieldHint();
            } else {
                this.setElementValue(displayValue);
            }
        }
    },

    // Override setValue and mapValueToDisplay to ensure that on an explicit setValue() to
    // a value that's not currently loaded in the pickList dataSet, we show the display value
    // rather than the underlying data value.
    setValue : function (newValue, allowNullValue, partialValue,a,b) {
        // if setValue() is called on this item, but doesn't change the item value, 
        // and addUnknownValues is false, and we have
        // a pending value entered in our text box right now, reset to display the pending
        // value rather than the display value for the specified value.
        // This means if a form item is redrawn we won't drop the pending value the
        // user entered.
        
        var valueChanged = this._value != newValue;
        if (!valueChanged) {
            if (this._pendingElementValue && 
                (this._completeOnTabValue != null || this.hasFocus)) 
            {
                this.setElementValue(this._pendingEnteredValue);
                return;
            }
        }
        
        // clear the pending style
        this._markNotPending();
        
        this._programmaticSetValue = true;
        this.invokeSuper(isc.ComboBoxItem, "setValue", newValue, allowNullValue, partialValue, a, b);
        delete this._programmaticSetValue;

        // See if the in-field hint needs to be shown
        if (!this.hasFocus && this._getShowHintTextInDataElement() && this.getHint()) {
            var undef;
            if (newValue === undef || newValue == null || isc.is.emptyString(newValue)) {
                this._showInFieldHint();
            }
        }
        // treat a call to setValue like the user picking a value from the form
        
        if (partialValue || newValue == null) {
            this.explicitChoice = false;
        } else if (valueChanged) {
            this.explicitChoice = true;
        }
    },
    
    // override getDefaultValue to support defaultToFirstOption
    // override getDefaultValue to pick up the first option if defaultToFirstOption is true
    // getDefaultValue should not be able to return a value that is not included
    // in the valueMap for this select.
    getDefaultValue : function () {
        
        var dV = this.Super("getDefaultValue", arguments);
        if (dV == null && this.defaultToFirstOption) {
            dV = this.getFirstOptionValue();
        }
        return dV;
    },
    makePickList : function (show) {
        
        // setting 'showFilterEditor' is unsupported for ComboBoxItem - actually catch this case and warn
        // about it
        // (one time warning only - we don't want to spam them for every ComboBoxItem created off a common
        // editorProperties block or similar)
        
        if (!isc.ComboBoxItem._showFilterEditorWarningShown && this.pickListProperties != null &&
           this.pickListProperties.showFilterEditor) 
        {
            this.logWarn("ComboBoxItem.pickListProperties specified with 'showFilterEditor' set to true. " +
                         "The 'showFilterEditor' property is not supported for the pickList of a ComboBoxItem " +
                         "and may result in unexpected user experience.");
            isc.ComboBoxItem._showFilterEditorWarningShown = true;
            
        }
        
        // Create cached special values if applicable
        this._getSpecialValues(false);

        if (this.progressiveLoading === true || this.progressiveLoading === false) {
            if (this.pickListProperties == null) this.picklistProperties = {};
            this.picklistProperties.progressiveLoading = this.progressiveLoading;
        }

        if (!this.filterLocally && 
            this.specialValues && !this.separateSpecialValues &&
            this._getOptionsFromDataSource())
        {
            if (this.pickListProperties == null) 
                this.pickListProperties = {};
            if (this.pickListProperties.dataProperties == null) 
                this.pickListProperties.dataProperties = {};
            // Using basic rather than local means if we do have outstanding filter criteria
            // we'll fetch fewer rows from the server, while still being able to manipulate
            // the cache to insert the empty row at the top.
            this.pickListProperties.dataProperties.fetchMode = "basic";
        }
        var interfaceMakePickList = isc.PickList.getPrototype().makePickList;
        return interfaceMakePickList.apply(this, arguments);
    },

    
    // Overrides to logic to generate / parse criteria for use in search forms / filter editors
    // ---
    // If addUnknownValues is false and we have a displayField, and we are set to an "unknown"
    // value (IE we don't have a selected record), the most likely scenario is that the user
    // has entered a partial displayField value in the text box.
    // In this case use the displayField as the criteria fieldName and the pickList filter operator
    // as the search operator.
    // Also handle being passed criterion for the display field value and entering them in the
    // text box.
    
   
    //> @attr comboBoxItem.generateExactMatchCriteria (boolean : null : IRWA)
    // When a comboBoxItem is used to generate search criteria in a SearchForm this property
    // governs whether, if the user explicitly chose an option from the pickList, we explicitly
    // generate criteria that will search for an exact match against the chosen value.
    // <P>
    // In order to achieve this, when this property is set to true, this item will generate
    // +link{AdvancedCriteria} in <smartclient>its +link{getCriterion()} method</smartclient>
    // <smartgwt>the default <code>FormItemCriterionGetter</code>'s <code>getCriterion()</code>
    // method</smartgwt>.
    // <P>
    // See +link{shouldGenerateExactMatchCriteria()} for behavior when this flag is unset.
    // @visibility external
    //<
    
    //> @method comboBoxItem.shouldGenerateExactMatchCriteria() [A]
    // When a comboBoxItem is used to generate search criteria in a SearchForm, 
    // if the user explicitly chose an option from the pickList, should the criterion generated
    // by <smartclient>+link{getCriterion()}</smartclient>
    // <smartgwt>the <code>FormItemCriterionGetter</code>'s <code>getCriterion()</code> method</smartgwt>
    // enforce a search for an exact match against the chosen value?
    // <P>
    // In order to achieve this, when this property is set to true, this item will generate
    // +link{AdvancedCriteria} in <smartclient>its +link{getCriterion()}</smartclient>
    // <smartgwt>the default <code>FormItemCriterionGetter</code>'s <code>getCriterion()</code></smartgwt>
    // method.
    // <P>
    // Default implementation will return +link{generateExactMatchCriteria} if specified, otherwise
    // true if the DataSource for this item 
    // +link{DataSource.supportsAdvancedCriteria(),supports advanced criteria}, false if it
    // does not.
    // @return (Boolean) should getCriterion() generate exact-match search criteria when
    //   a value was explicitly chosen from this item's set of options?
    // @visibility external
    //<
    shouldGenerateExactMatchCriteria : function () {
        if (this.generateExactMatchCriteria != null) return this.generateExactMatchCriteria;
        var DS = this.form.getDataSource();
        
        if (DS && DS.supportsAdvancedCriteria()) return true;
        return false;
    },
    
    //> @method comboBoxItem.hasAdvancedCriteria()
    // Will this item return advancedCriteria if +link{DynamicForm.getValuesAsCriteria()} is 
    // called on this item's form?
    // Overridden for ComboBoxItem to return true if +link{generateExactMatchCriteria} is true
    // - in this case if an exact value is chosen from our set of options (always the
    // case if <code>addUnkownValues</code> is false), we will use advancedCriteria to ensure
    // the generated search criteria exactly matches the chosen value for this item.
    // <P>
    // Note that +link{AdvancedCriteria} are not supported by all dataSources. When a form is bound
    // to a dataSource, we therefore default <code>generateExactMatchCriteria</code> to false unless
    // the dataSource is known to support AdvancedCriteria.
    // <P>
    // As with formItem.hasAdvancedCriteria() this will also return true if a +link{operator}
    // was explicitly specified for this item
    // @return (Boolean) true if the result of getCriterion() will be an AdvancedCriteria object.
    // @group criteriaEditing
    // @visibility external
    //<
    // Desired behavior is really to have explicitly chosen values always do an exact match
    // against the value field, whereas partially entered values will do a partial match against
    // the display field (as they would with the pickList). For this to happen reliably we would
    // essentially always return advanced criteria - but AC are not supported by all dataSources
    // so we don't want this to be our default behavior.
    hasAdvancedCriteria : function () {
        if (this.Super("hasAdvancedCriteria", arguments)) return true;
        if (this.addUnknownValues && this.explicitChoice && this.shouldGenerateExactMatchCriteria())
            return true;
        return false;
    },
    //> @method comboBoxItem.getCriterion()
    // Returns criterion derived from the current value of this item.
    // <P>
    // If +link{addUnknownValues} is true for this item, we implement the following behavior.<br>
    // If the user explicitly selected an item from the pickList, we treat this as an attempt
    // to explicitly match the data value. In this case returned criteria will match the
    // selected (data) value against this item's fieldName.<br>
    // If the user typed a value into the text field, we treat this as an attempt to do a substring
    // type filter. In this case returned criteria will match the entered text value against the
    // displayField for this item if one is specified.
    // <P>
    // If +link{addUnknownValues} is false we always match the chosen data value against the item's 
    // fieldName.
    // <P>
    // Note that +link{shouldGenerateExactMatchCriteria()} will be called in the case when a
    // value was explicitly picked from the set of options. If that method returns true, we will
    // return AdvancedCriteria with an operator specified to ensure an exact match occurs.
    // 
    // @return (Criterion) criterion object based on this fields current edited value(s).
    // @group criteriaEditing
    // @visibility external
    //<
    getCriteriaFieldName : function () {
        if (this.criteriaField != null) return this.criteriaField;
    
        if (this.displayField != null && this.addUnknownValues && !this.explicitChoice) {
            return this.displayField;
        }

        // Explicit choice from pickList - return the valueField for the item.
        // Note: DO NOT CALL Super.getCriteriaFieldName() - we subclass TextItem which returns
        // displayField if set
        return this.getDataPath() || this.getFieldName();
    },
    
    //> @method comboBoxItem.getCriteriaValue()
    // Overridden to return the text box value if a displayField is specified for this item
    // and the user explicitly entered a display value in the text box field rather than
    // picking a specific value from the set of available options in the pickList.
    // @return (any) filter criterion value for this field
    //<
    getCriteriaValue : function () {
        if (this.displayField != null && this.addUnknownValues && 
            !this._showingLoadingDisplayValue &&
            !this.explicitChoice) 
        {
            var enteredValue = this.getEnteredValue();
            if (enteredValue != this.emptyDisplayValue) {
                return enteredValue;
            }
        }
        
        return this.Super("getCriteriaValue", arguments);
    },
    getOperator : function (textMatchStyle) {
        var operator;
        // Note if addUnknownValues is false, return the filter operator if we're showing an
        // 'unknown' value, even if we don't have a displayField specified. In this case we
        // still want to do a substring match rather than an exact match.
        if (this.addUnknownValues && !this.explicitChoice) {
            operator = this.getPickListFilterOperator();
        } else {
            operator = this.Super("getOperator", arguments);
        }
        return operator;
    },
    
    //> @method comboBoxItem.canEditSimpleCriterion() [A]
    // Overridden to support editing simple criterion on the display field if one is specified
    // as well as the value field.
    // @return (boolean) true if this item can edit the specified fieldName
    //<
    canEditSimpleCriterion : function (fieldName) {
        if (this.getDataPath() && fieldName == this.getDataPath()) return true;
        if (this.criterionField) return fieldName == this.criterionField;
        if (this.displayField && fieldName == this.displayField) return true;
        return this.getFieldName() == fieldName;
    },
    
    // Override setSimpleCriterion - if the crit passed in is for the display field, we don't want
    // 'getCriterion()' to return a criterion on the data field.
    // Achieve this via the special "partialValue" param on setValue()
    setSimpleCriterion : function (value, fieldName) {
        if (this.criterionField == null && this.displayField != null && fieldName == this.displayField) {
            this.setValue(value, null, true);
        } else {
            this.setValue(value);
        }
    },

        
    //> @method comboBoxItem.canEditCriterion()
    // This method is overridden in comboBoxItem. When addUnknownValues is true, 
    // comboBoxItems allow the user to edit substring match type criteria applied to the
    // display field (if one is specified).
    // <P>
    // The user can also edit criteria attempting to match exactly against the item's field name.
    //
    // @group criteriaEditing
    // @visibility external
    //<
    canEditCriterion : function (criterion, warnOnField) {
        if (!this.addUnknownValues) {
            return this.Super("canEditCriterion", arguments);
        }
        
        if (this.getDataPath() && criterion.fieldName == this.getDataPath()) return true;

        var critField = criterion.fieldName,
            valueField = this.criterionField || this.getFieldName(),
            // We have to invoke super implementation of getOperator to pick up
            // our standard operator (typically just "equals" as we have an option dataSource)
            // as we override getOperator() to look at this.getSelectedRecord() which is
            // irrelevant to whether we can handle new criteria passed in.
            defaultOperator = this.Super("getOperator", []);
            
        if (critField != null) {
            if (this.displayField == null) {
                if (critField == valueField) {
                    return criterion.operator == defaultOperator
                            || criterion.operator == this.getPickListFilterOperator();
                }
            } else {
            
                if (critField == valueField) {
                    return criterion.operator == defaultOperator;
                } else if (critField == this.displayField) {
                    return criterion.operator == this.getPickListFilterOperator();
                }
            }
        }
        return false;
    },
    
    //> @method comboBoxItem.setCriterion()
    // Overridden to support editing criterion against the display field or value field when
    // +link{addUnknownValues} is true.
    // @group criteriaEditing
    // @visibility external
    //<
    setCriterion : function (criterion) {
        if (!this.addUnknownValues || this.displayField == null ||
            criterion.fieldName != this.displayField)
        {
            return this.Super("setCriterion", arguments);
        }
        
        // This is essentially setValue() but taking an explicit displayValue rather than
        // data value.
        var displayVal = criterion.value;
        this.setElementValue(displayVal);
        this._checkForDisplayFieldValue(displayVal);        
        var dataValue = this.mapDisplayToValue(displayVal);
        if (!this.compareValues(dataValue, this._value)) this.saveValue(dataValue);
    },
    
    // Override checkForDisplayFieldValue()
    // This is the method that, if we have a displayField specified, kicks off a fetch against
    // our optionDataSource to load the appropriate display value from the server.
    // In PickList based items we use the pickList data (if present) to map data to display 
    // values. 
    // Catch the case where checkForDisplayFieldValue is called when we are in the process of
    // fetching our pickList data from the server.
    // In this case we want to wait for the pickList data to be returned rather than kicking off
    // an additional fetch as our data value will usually be present in that pickList data.
    // When the pickList data returns we re-check this method. If the data is present, we're 
    // done, otherwise we need to kick off another fetch as we didn't find our data value in
    // the pickList data array. This can happen if the pickList data is paged, for instance.
    
    _checkForDisplayFieldValue : function (newValue, delayed) {
        var inValueMap = (this._mapKey(newValue, true) != null);
        
        if (inValueMap) {
            return; 
        }
        if (this._fetchingPickListData) {
            this._checkDisplayFieldValueOnFilterComplete = true;
            return;
        }
        // We can also bypass this if the pickList has a *complete* cache
        if (this.pickList != null && this.pickList.formItem == this && 
            isc.isA.ResultSet(this.pickList.data) && this.pickList.data.allRowsCached()) 
        {
            return;
        }
        this.invokeSuper(isc.ComboBoxItem, "_checkForDisplayFieldValue", newValue);
    },
    
    // Map valueToDisplay and mapDisplayToValue need to pick up
    // the mapping between displayField and valueField, if there is one.
    // We implement this by overriding _mapKey() / mapDisplayToValue() to check for the value in
    // our pickList's dataSet, in addition to checking against any explicitly specified valueMap
    
    
    _mapKey : function (value, dontReturnKey, a,b,c,d) {
        var displayValue = this.invokeSuper(isc.ComboBoxItem, "_mapKey", value, true ,a,b,c,d);

        // _translateFieldValue part of the pickList interface
        if (displayValue == null && this.getDisplayFieldName() != null) {
            var translatedValue = this._translateValueFieldValue(value, false),
                undef;
            if (translatedValue !== undef) {
                displayValue = translatedValue;
            } else if (this.specialValues) {
                var map = this.specialValues;
                if (isc.isA.String(map)) map = this.getGlobalReference(map);

                if (!isc.isAn.Array(map)) {
                    if (isc.PickList.emptyStoredValue && value === null) {
                        value = isc.PickList.emptyStoredValue;
                    }
                    if (map[value] !== undef) displayValue = map[value];
                }
            }
        }
        if (displayValue == null && !dontReturnKey) displayValue = value;            
        return displayValue;
    },

    
    mapDisplayToValue : function (value, a, b, c) {
        
        if (this._pickedReverseValueMap) {
            for (var i in this._pickedReverseValueMap) {
                if (i == value) {
                    return this._pickedReverseValueMap[i];
                }
            }
            delete this._pickedReverseValueMap;
        }
        if (this.getDisplayFieldName() != null) {
            var dataValue;
            // If the text-box value is our emptyDisplay value, don't attempt to pick up
            // a value from the optionDataSource.
            
            dataValue = (value == this.emptyDisplayValue) ? null 
                                : this._translateValueFieldValue(value, true);

            if (dataValue != null) value = dataValue;
        }
        return this.invokeSuper(isc.ComboBoxItem, "mapDisplayToValue", value, a,b,c);
    },

    //> @attr comboBoxItem.allowExpressions (boolean : null : IRW)
    // The standard +link{formItem.allowExpressions} behavior is always disabled for 
    // ComboBoxItem.
    // <P>
    // The interface is not compatible with the <code>allowExpressions</code> feature.
    // A ComboBoxItem normally starts fetching matches as you type, and that mixes 
    // very strangely with the idea of entering expressions like <code>"a..b"</code>
    // - you will have the ComboBox seemingly switching back and forth between treating
    // the text as a normal search string vs as a special expression on a per-keystroke
    // basis.
    // <P>
    // We recommend a normal TextItem as the correct UI element to supply for users to
    // enter filter expressions.
    // @visibility external
    //<
    _shouldAllowExpressions : function () {
        return false;
    },

    // always suppress native autoComplete
    _getAutoCompleteSetting : function () {
        return this._$none;
    }
    
});


isc.ComboBoxItem.registerStringMethods({
    dataArrived:"startRow,endRow,data",
    getPickListFilterCriteria:""
});


isc.defineClass("BoundDSFieldPicker", "ComboBoxItem");

isc.BoundDSFieldPicker.addProperties({
    width: "*",
    addUnknownValues: false
});

isc.BoundDSFieldPicker.addMethods({
    init : function () {
        if (this.requiresDSField) {
            var dsField = this.requiresDSField,
                values = this.form.getValues(),
                dsName = values[dsField]
            ;
            if (dsName) {
                var ds = isc.DS.get(dsName),
                    fieldNames = ds.getFieldNames()
                ;
                this.valueMap = fieldNames;
            }
        } else {
            this.logInfo("BoundDSFieldPicker specified without a requiresDSField attribute");
        }

        this.Super("init", arguments);
    }
});
