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
//> @groupDef accessibility
// SmartClient is a fully accessible technology which fulfills the Section 508 requirements of
// U.S. government law and similar international standards.  Specificallly:
// <ul>
// <li> components are fully keyboard navigable and the browser's native focus indicator reveals
// keyboard focus to the user
// <li> components are themable/brandable, allowing a variety of high contrast and limited color
// range look and feel options to compensate for visual acuity disabilities
// <li> the WAI-ARIA standard is supported for adding semantic markup to components to identify them to
// screen readers such as NVDA or JAWS.
// </ul>
// <P>
// <b>WAI-ARIA support</b>
// <P>
// ARIA is a standard from the WAI (Web Accessibility Institute) that allows modern Ajax applications to
// add semantic markup to the HTML used to create modern Ajax interfaces to enable screen reader support.
// This semantic markup allows a screen reader to identify the function and state of complex components
// such as load-on-demand lists and trees even though they are composed of simple elements such a &lt;div&gt;s.  
// <P>
// Note that ARIA support is the correct way to evaluate the accessibility of a web
// <i>application</i>.  Standards which apply to a web <i>site</i>, such as ensuring that all interactive
// elements are composed of native HTML anchor (&lt;a&gt;) or &lt;form&gt; controls, cannot and should
// not be applied to a web <i>application</i>.  A web application's accessibility must be evaluated in
// terms of its ARIA support.
// <P>
// By default, SmartClient components will write out limited ARIA markup sufficient to navigate basic
// menus and buttons.  Full screen reader mode is not enabled by defaut because it has a small
// performance impact and subtly changes the management of keyboard focus in a way that is slightly worse
// for unimpaired users.  
// <P>
// The limited ARIA support which is enabled by default is intended to allow a screen reader user to
// navigate to a menu to enable full screen reader support.  This is analogous to a partially visually
// impaired user ariving at a site with normal theming and needing to switch to a high-contrast skin.
// <P>
// To enable full screenReader mode, call <smartclient>+link{isc.setScreenReaderMode}</smartclient>
// <smartgwt>+link{SC.setScreenReaderMode()}</smartgwt> before any
// SmartClient components are created or drawn.  This implies that if an end user dynamically enables
// full screen reader support, the application page must be reloaded, as an any existing components will
// not have full ARIA markup.
// <P>
// For an overview of ARIA, see +externalLink{http://www.w3.org/WAI/intro/aria.php}.
// <P>
// To completely disable ARIA markup, call
// <smartclient>+link{isc.setScreenReaderMode(),isc.setScreenReaderMode(false)}</smartclient>
// <smartgwt>+link{SC.setScreenReaderMode(), SC.setScreenReaderMode(false)}</smartgwt> before any components are drawn.
// <P>
// <b>Recommended Screen Reader Configuration</b>
// <P>
// The recommended configuration for screen reader use is the most recent available release of Firefox
// and either the JAWS or NVDA screen reader.
// <P>
// While WAI-ARIA markup is provided for other browsers, support for WAI-ARIA itself is known to be
// limited in current release versions of IE and other browsers supported by SmartClient.
// <P>
// <b>Application-level concerns</b>
// <P>
// While SmartClient enables accessible web applications to be created, it is always possible for an
// application to violate accessibility standards.  The following is a brief and not exhaustive list of
// concerns for application authors:
// <ul>
// <li> for any operation that can be triggered via drag and drop, you should offer an equivalent
// keyboard-only means of performing the same operation.  For common grid to grid drags, this is easily
// accomplished using +link{ListGrid.transferSelectedData()}.
// <li> if you use a component in a way that is not typical, such as using an ImgButton as a
// non-interactive stateful display, set its +link{canvas.ariaRole} appropriately.  For a list of ARIA
// roles, see +externalLink{http://www.w3.org/WAI/PF/aria/roles#role_definitions}.
// Note that in most cases you will not need to modify the default ariaRole written out by
// the SmartClient framework with screenReader mode enabled.
// <li> for plain HTML content that is incorporated into an Ajax interface (such as an embedded help
// system), embed the HTML into an +link{HTMLFlow} (whose default ARIA role is "article") and ensure the
// HTML itself is accessible (for example, has "alt" attributes on all images which are semantically
// meaningful)
// <li> in addition to setting explicit ARIA roles per canvas, SmartClient also allows 
// developers to specify values for explicit 
// <smartclient>+link{canvas.ariaState,ARIA states}</smartclient>
// <smartgwt>+link{canvas.setAriaState(),ARIA states}</smartgwt>
// (see +externalLink{http://www.w3.org/TR/wai-aria/states_and_properties}) to be written
// out with the HTML for a component. <br>
// Note that, as with ariaRoles, in most cases the
// framework automatically writes out any appropriate aria state information based
// on the component being generated - you'd only make use of this property if
// using components in some custom way. 
// To provide a concrete example: a developer might implement a logical nested
// "menu" built from a set of Button instances. In that case, some button might have
// ariaRole set to <code>"menuitem"</code> and (if it launches a sub-menu),
// also the +externalLink{http://www.w3.org/TR/wai-aria/states_and_properties#aria-haspopup,"haspopup"}
// aria state. The code for this would be something like:
// <smartclient>
// <pre>
// isc.Button.create({
//      // ... various properties
//      
//      ariaRole:"menuitem",
//      ariaState:{haspopup:true}
// });
// </pre>
// </smartclient>
// <smartgwt>
// <pre>
//  myButton.setAriaRole("menuitem");
//  myButton.setAriaState("haspopup", true);
// </pre>
// </smartgwt>
// </ul>
// <p>
// <b>Known Screen Reader bugs / quirks</b>
// <p>
// JAWS: By default, JAWS treats a web page as a web document - text interspersed with graphics,
// links, etc. - and not as an application consisting of form controls, interactive buttons,
// lists, and so on. To enable application mode in JAWS, it is necessary to add <code>role="application"</code>
// to the &lt;body&gt; tag. See
// +externalLink{http://www.freedomscientific.com/Support/TechnicalSupport/Bulletin/1404,Freedom Scientific Bulletin 1404 - In ARIA&#44; what is the difference in how JAWS treats role="application" and role="document"?}
//
// @treeLocation Concepts
// @title Accessibility / Section 508 compliance
// @visibility external
//<

//> @classMethod isc.setScreenReaderMode()
// Enables full screen reader mode.  Must be called before any components are created.  See
// +link{group:accessibility}.
// @param newState (boolean) new setting
// @visibility external
//<
isc.setScreenReaderMode = function (newState) {
   isc.screenReader = newState;
}

//> @attr canvas.ariaRole (String : null : IR)
// ARIA role of this component.  Usually does not need to be manually set - see
// +link{group:accessibility}.
// @group accessibility
// @visibility external
//<

//> @attr canvas.ariaState (Object : null : IRA)
// ARIA state mappings for this component. Usually this does not need to be manually
// set - see +link{group:accessibility}.
// <P>
// This attribute should be set to a mapping of aria state-names to values - for example
// to have the "aria-haspopup" property be present with a value "true", you'd specify:
// <pre>
//  { haspopup : true }
// </pre>
// @group accessibility
// @visibility external
//<
 
//isc.screenReader = undefined; // initially undefined

// liteAria
// - may be explicitly set to true to minimize what Aria behaviors are enabled
// - otherwise we default to true in IE8 and earlier unless 'setScreenReaderMode(true)' has been
//   explicitly called.
isc.liteAria = null;

// internal DOM manipulation methods, don't document
isc.Canvas.addClassMethods({

    
    // this just indicates whether we write out basic ARIA attributes for most elements, not whether we
    // are in full screenReader mode
    ariaEnabled : function () {
        return isc.screenReader || isc.screenReader !== false &&
            ((isc.Browser.isIE && isc.Browser.version >=8) || !isc.Browser.isIE);
    },

    useLiteAria : function () {
        // allow "liteAria" to be explicitly specified.
        // Otherwise if screenReader is explicitly set to true, don't use liteAria
        // - otherwise use liteAria for <= IE8 since it's slow.
        if (isc.liteAria != null) return isc.liteAria;
        if (isc.screenReader == true) return false;
        return (isc.Browser.isIE && isc.Browser.version < 9);
    },

    setAriaRole : function (element, role) {
        if (!element) return;
        if (this.logIsDebugEnabled("aria")) {
            this.logDebug("ARIA role changed to: " + role + 
                          " on element: " + this.echoLeaf(element), "aria");
        }
        element.setAttribute("role", role);
    },

    setAriaState : function (element, stateName, stateValue) {
        if (!element) return;
        if (this.logIsInfoEnabled("aria")) {
            this.logInfo("ARIA state: " + stateName + ": " + stateValue +
                         ", set on element: " + isc.echoLeaf(element), "aria");
        }

        element.setAttribute("aria-" + stateName, stateValue == null ? "" : stateValue);
    },

    setAriaStates : function (element, state) {
        if (!element) return;
        if (state == null) return;
        for (var stateName in state) {
            this.setAriaState(element, stateName, state[stateName]);
        }
    },

    clearAriaState : function (element, stateName) {
        if (!element) return;
        element.removeAttribute("aria-" + stateName);
    },

    getAriaStateAttributes : function (ariaState) {
        var output = "";
        if (ariaState) {
            for (var stateName in ariaState) {
                var stateValue = ariaState[stateName];
                output += " aria-" + stateName + "='" + String.asAttValue(stateValue) + "'";
            }
        }
        return output;
    }
});

isc.Canvas.addMethods({
    // instance-level methods.  Canvases set ariaRole and ariaState on their
    // clipHandle

    //> @method canvas.setAriaRole()
    // Update the +link{canvas.ariaRole} at runtime
    // @param role (String) new ariaRole
    // @group accessibility
    // @visibility internal
    //<
    
    setAriaRole : function (role) {
        isc.Canvas.setAriaRole(this.getClipHandle(), role);
    },

    //> @method canvas.setAriaState()
    // Set a specific ARIA state for this component.
    // 
    // @param stateName (String) aria state to update
    // @param stateValue (String | Boolean | Integer | Float) value for the aria state
    // @group accessibility    
    // @visibility external
    //<
    setAriaState : function (stateName, stateValue) {
        isc.Canvas.setAriaState(this.getClipHandle(), stateName, stateValue);
    },
    setAriaStates : function (state) {
        isc.Canvas.setAriaStates(this.getClipHandle(), state);
    },
    clearAriaState : function (stateName) {
        isc.Canvas.clearAriaState(this.getClipHandle(), stateName);
    },

    // called during initial draw for non-IE browsers
    getAriaState : function () {
        var ariaState = isc.addProperties({}, this.ariaState);
        if (this.isDisabled()) ariaState.disabled = true;
        return ariaState;
    },
    getAriaStateAttributes : function () {
        return isc.Canvas.getAriaStateAttributes(this.getAriaState());
    }
});


if (isc.StatefulCanvas) {

isc.StatefulCanvas.addProperties({
    ariaState: {},
    getAriaState : function () {
        var ariaState = this.Super("getAriaState", arguments);

        // Don't set labelledby state here because we want 'aria-label' to take precedence
        // but "As required by the text alternative computation, user agents give precedence
        // to aria-labelledby over aria-label when computing the accessible name property."
        // http://www.w3.org/TR/wai-aria/states_and_properties#aria-labelledby
        //
        // ImgTab depends on labelledby *not* being set so that its override of getAriaLabel()
        // can append the ariaCloseableSuffix.

        var ariaLabel = this.getAriaLabel();
        if (ariaLabel != null) ariaState.label = ariaLabel;

        return ariaState;
    }
});

}

if (isc.Progressbar) {

isc.Progressbar.changeDefaults("ariaState", {
    valuemin: 0,
    valuemax: 100
});
isc.Progressbar.addProperties({
    ariaRole: "progressbar",
    getAriaState : function () {
        var ariaState = this.Super("getAriaState", arguments);
        ariaState.valuenow = this.percentDone;
        return ariaState;
    }
});

}


if (isc.DynamicForm) {

isc.DynamicForm.addProperties({
    rightTitlePrefix: "<span aria-hidden='true'>:&nbsp;</span>",
    titleSuffix: "<span aria-hidden='true'>&nbsp;:</span>",
    requiredRightTitlePrefix: "<b><span aria-hidden='true'>:&nbsp;</span>",
    requiredTitleSuffix: "<span aria-hidden='true'>&nbsp;:</span></b>"
});

// General support for formItems
// ---------------------------------------------------------------------------------------
// Note: FormItemIcon: in FormItem.js, given fixed role="button" and icon.prompt made into aria-label.

isc.FormItem.addMethods({

    //> @attr formItem.ariaRole (String : null : IRWA)
    // ARIA role of this formItem.  Usually does not need to be manually set - see
    // +link{group:accessibility}.
    // @group accessibility
    // @visibility external
    //<

    //> @attr formItem.ariaState (Object : null : IRWA)
    // ARIA state mappings for this formItem. Usually this does not need to be manually
    // set - see +link{group:accessibility}.
    // <P>
    // This attribute should be set to a mapping of aria state-names to values - for example
    // to have the "aria-multiline" property be present with a value "true", you'd specify:
    // <pre>
    //  { multiline : true }
    // </pre>
    // @group accessibility
    // @visibility external
    //<

    // FormItems set ariaRole and ariaState on their focus element, if any

    //> @method formItem.setAriaRole()
    // Sets the ARIA role of this FormItem.  Usually does not need to be manually set - see
    // +link{groupDef:accessibility}.
    // @param role (String) ARIA role for this item
    // @group accessibility
    // @visibility internal
    //<
    setAriaRole : function (role) {
        var focusElement = this.getFocusElement();  
        if (focusElement != null) isc.Canvas.setAriaRole(focusElement, role);
    },

    setOuterAriaRole : function (outerRole) {
        var outerElement = this.getHandle();
        if (outerElement != null) isc.Canvas.setAriaRole(outerElement, outerRole);
    },

    //> @method formItem.setAriaState()
    // Sets some ARIA state value for this FormItem.
    // Usually does not need to be manually set - see
    // +link{groupDef:accessibility}.
    // @param stateName (String) ARIA state name to set
    // @param stateValue (String | Boolean | Integer) value for the specified state
    // @group accessibility
    // @visibility internal
    //<
    setAriaState : function (stateName, stateValue) {
        var element;
        if (this.outerAriaRole) {
            element = this.getHandle();
        } else {
            element = this.getFocusElement();
        }
        if (element != null) isc.Canvas.setAriaState(element, stateName, stateValue);
    },
    setAriaStates : function (state) {
        var element;
        if (this.outerAriaRole) {
            element = this.getHandle();
        } else {
            element = this.getFocusElement();
        }
        if (element != null) isc.Canvas.setAriaStates(element, state);
    },
    clearAriaState : function (stateName) {
        var element;
        if (this.outerAriaRole) {
            element = this.getHandle();
        } else {
            element = this.getFocusElement();
        }
        if (element != null) isc.Canvas.clearAriaState(element, stateName);
    },
    getAriaState : function () {
        var state = {};

        // http://www.w3.org/WAI/PF/aria/states_and_properties#aria-required  
        if (this.required && this.form && this.form.hiliteRequiredFields) state.required = true;
 
        // http://www.w3.org/WAI/PF/aria/states_and_properties#aria-invalid
        if (this.hasErrors()) {
            state.invalid = true;
            
            // When showInlineErrors is false, a list of the errors is written out at the top
            // of the form - see DynamicForm.getErrorsHTML().
            //
            // If writing out inline errors, prefer to describe-by the error text rather than an error
            // icon because some screen readers do not handle reading the error icon's 'aria-label'.
            
            if (!this.form.showInlineErrors || this.shouldShowErrorText()) {
                var errorMessageID = this._getErrorMessageID();
                
                state.describedby = errorMessageID;

            } else if (this.form.showInlineErrors && this.shouldShowErrorIcon()) {
                var errorIconID = this.getErrorIconId();
                
                state.describedby = errorIconID;
            }

        }

        // If there is no describedby state yet, set it to the ID of the hint cell.
        if (state.describedby == null && !this._getShowHintInField()) {
            var hintHTML = this.getHint();
            if (hintHTML) {
                var hintCellID = this._getHintCellID();
                
                state.describedby = hintCellID;
            }
        }

        // Disabled also means it's not in the tab order so won't be read by default.  However the spec
        // below mentions this is the case so presumably this is for screen readers to add features to
        // allow users to have disabled fields read.
        // http://www.w3.org/WAI/PF/aria/states_and_properties#aria-disabled
        if (this.isDisabled()) state.disabled = true;

        if (isc.isA.CheckboxItem(this)) state.checked = !!this.getValue();

        return state;
    },

    // called after a FormItem is drawn
    addContentRoles : function () {
        if (!isc.Canvas.ariaEnabled() || isc.Canvas.useLiteAria()) return;

        if (!this._canFocus() || !this.ariaRole) return;

        this.setAriaRole(this.ariaRole);

        var outerElement;
        if (this.outerAriaRole) {
            outerElement = this.getHandle();
            if (outerElement != null) isc.Canvas.setAriaRole(outerElement, this.outerAriaRole);
        }

        // with a visible title, we write out <label for=>, but we need an explicit aria-label if the
        // title is either not visible or if we do not have a native HTML input element (since 
        // <label for=> is intended for true HTML input elements).
        
        
        var title = this.getTitle();
        if (title) {
            var labelElementID = this._getLabelElementID(),
                labelElement = this.getDocument().getElementById(labelElementID);

            var titleElement;
            if (this.hasDataElement()) {
                titleElement = this.getDataElement();
            } else if (this.outerAriaRole) {
                titleElement = outerElement != null ? outerElement : this.getHandle();
            } else if (isc.isA.ContainerItem(this)) {
                titleElement = this._getTableElement();
            } else {
                titleElement = this._getTextBoxElement();
            }
            if (titleElement != null) {
                // According to WAI-ARIA:
                // "If the label text is visible on screen, authors SHOULD use 'aria-labelledby'
                // and SHOULD NOT use 'aria-label'."
                // http://www.w3.org/TR/wai-aria/states_and_properties#aria-labelledby
                //
                // Thus, if we have a <label> element, use 'aria-labelledby'.
                if (labelElement != null) {
                    isc.Canvas.setAriaState(titleElement, "labelledby", labelElementID);
                } else {
                    // Because the title is an HTMLString value, we need to remove HTML.
                    isc.Canvas.setAriaState(titleElement, "label", String.htmlStringToString(title));
                    //this.logWarn("applied aria-label to: " + this.echo(titleElement));
                }
            }

            
        }

        // instance default state such as multiline:true for TextArea
        if (this.ariaState) this.setAriaStates(this.ariaState);

        // dynamic state
        this.setAriaStates(this.getAriaState());

        // If we are modeling a SelectItem as a listbox with one option, then set 'aria-selected'
        // on the text box element, which contains the SelectItem's value.
        if (isc.isA.SelectItem(this) && this.outerAriaRole === "listbox") {
            var textBox = this._getTextBoxElement();
            if (textBox != null) {
                isc.Canvas.setAriaStates(textBox, {
                    selected: true
                });
            }
        }
    }
});

isc.TextAreaItem.addProperties({
    ariaState : { multiline : true }
});



isc.RadioItem.addProperties({
    ariaRole: "radio"
});


isc.ComboBoxItem.addProperties({
    ariaState: {
        
        autocomplete: "list",

        // "The combobox must have aria-expanded = true if the list is displayed or aria-expanded = false
        // when it is not."
        // http://www.w3.org/WAI/PF/aria-practices/#combobox
        expanded: false
    },
    ariaRole: "combobox"
    //pickListAriaRole:"list", // not implemented 
    //pickListItemAriaRole:"listitem" // not implemented
});


isc.SelectItem.addProperties({
    ariaRole:"option",
    outerAriaRole:"listbox",
    ariaState: {
        expanded: false,

        // To indicate that this single-option listbox has a popup listbox, we need to apply
        // aria-haspopup = true.
        // http://www.w3.org/TR/wai-aria-practices/#relations_haspopup
        haspopup: true
    },

    // This prevents VoiceOver from reading "space" for the value of an empty SelectItem.
    emptyDisplayValue: "<span aria-hidden='true'>&nbsp;</span>"
});

isc.StaticTextItem.addProperties({
    ariaRole:"textbox",
    
    ariaState:{ disabled:true }
});

// "menu" role vs "list" role: somewhat ambiguous, as both roles have the notions of
// selectability (via "checked" for menus), but generally menus show actions and sometimes
// choices whereas lists show just choices.
// ListGrid currently advertises itself as a List, it's subclass ScrollingMenu sounds like it
// should advertise itself as a Menu, however it is not used for anything but the PickList
// NOTE: separators already handled by ListGrid superclass
//isc.PickListMenu.addProperties({
//    ariaRole:"list",
//    rowRole:"listitem"
//});
if (isc.PickListMenu) {
isc.PickListMenu.addProperties({

    // For role="combobox", the associated listbox must have role="listbox" because a listbox
    // is a required owned element of a combobox: http://www.w3.org/TR/wai-aria/roles#combobox
    // For role="listbox", the associated options must have role="option"
    // http://www.w3.org/TR/wai-aria/roles#listbox
    ariaRole: "listbox",
    rowRole: "option",

    _$rowIdTemplate: [null, "_row_", null,null,null,null,null],
    getRowElementId : function (rowNum) {
        var template = this._$rowIdTemplate;
        template[0] = this.getID();
        isc._fillNumber(template, rowNum, 2, 5);
        return template.join(isc.emptyString);
    }

});
}

isc.ContainerItem.addProperties({
    setAriaRole : function (role) {
        var tableElement = this._getTableElement();
        if (tableElement != null) isc.Canvas.setAriaRole(tableElement, role);
    },
    setAriaState : function (stateName, stateValue) {
        var tableElement = this._getTableElement();
        if (tableElement != null) isc.Canvas.setAriaState(tableElement, stateName, stateValue);
    },
    setAriaStates : function (state) {
        var tableElement = this._getTableElement();
        if (tableElement != null) isc.Canvas.setAriaStates(tableElement, state);
    },
    clearAriaState : function (stateName) {
        var tableElement = this._getTableElement();
        if (tableElement != null) isc.Canvas.clearAriaState(tableElement, stateName);
    }
});

isc.RadioGroupItem.addProperties({
    ariaRole: "radiogroup"
});

} // end if (isc.DynamicForm)

if (isc.MultiComboBoxItem) {

isc.MultiComboBoxItem.changeDefaults("buttonProperties", {
    ariaState: {
        // The buttons' 'aria-hidden' attributes are explicitly managed by the MCBI.
        hidden: true
    },
    getAriaState : function () {
        var ariaState = this.getClass()._instancePrototype.getAriaState.apply(this, arguments);
        ariaState.hidden = !this.isVisible();
        return ariaState;
    }
});

}


if (isc.GridRenderer) {

// Grids
// ---------------------------------------------------------------------------------------


// Support for row and cell roles and states
isc.GridRenderer.addMethods({
    setRowAriaState : function (rowNum, stateName, stateValue) {
        var row = this.getTableElement(rowNum);
        if (row == null) return;
        isc.Canvas.setAriaState(row, stateName, stateValue);
    },
    setRowAriaStates : function (rowNum, state) {
        var row = this.getTableElement(rowNum);
        if (row == null) return;
        isc.Canvas.setAriaStates(row, state);
    }

    

    

});

isc.ListGrid.addMethods({
    ariaRole:"list",
    rowRole:"listitem",
    getRowRole : function (rowNum, record) {
        if (record && record.isSeparator) return "separator";
        return this.rowRole;
    },
    getRowAriaState : function (rowNum, record) {
        if (!isc.Canvas.ariaEnabled() || isc.Canvas.useLiteAria()) return; // too expensive to enable by default

        // if only rendering a range of rows, need to tell the reader the total size and position
        var state;
        if (!this.showAllRecords && this.data != null) {
            state = { setsize : this.getTotalRows(), 
                      posinset : rowNum + 1 }
        }
        
        var value = record != null ? record : rowNum;
        if (this.selection && this.selection.isSelected && this.selection.isSelected(value)) {
            if (state == null) state = {}
            state.selected = true;
        }
        return state;
    }
});

isc.TreeGrid.addMethods({
    ariaRole:"tree",
    rowRole:"treeitem",
    getRowRole : function (rowNum, node) {
        return this.rowRole;
    },
    // an attempt to use the hasparent attribute to link nodes.  Not respected by FF1.5
    //getRowElementId : function (rowNum) {
    //    return this.getID() + "_row_" + rowNum;
    //},
    getRowAriaState : function (rowNum, node) {
        if (!isc.Canvas.ariaEnabled() || isc.Canvas.useLiteAria()) return; // too expensive to enable by default

        var theTree = this.data,
            selected = !!(this.selection && this.selection.isSelected && 
                            this.selection.isSelected(node, true)),
            level = theTree.getLevel(node);
    
        var state = { selected : selected, 
                      level : level,
                      // if only rendering a range of rows, need to tell the reader the 
                      // total size and position
                      setsize : this.getTotalRows(),
                      posinset : rowNum + 1
                    };

        if (theTree.isFolder(node)) state.expanded = !!theTree.isOpen(node);

        // an attempt to use the hasparent attribute to link nodes.  Not respected by FF1.5
        //var parent = theTree.getParent(node);
        //if (parent && parent != theTree.getRoot()) {  
        //    state.hasparent = this.getRowElementId(theTree.indexOf(parent));
        //}

        return state;
    }
});

// NOTE: CubeGrid support in AnalyticsScreenReader.js

// Menus / ListPickers
// ---------------------------------------------------------------------------------------

isc.Menu.addMethods({
    ariaRole:"menu",
    // get rid of the "/" cell separators since we commonly 
    // have empty cols and we don't want to render out seemingly random slashes
    screenReaderCellSeparator:null,
    getRowRole : function (rowNum, item) {
        if (!item || item.isSeparator) return "separator";
        if (item.checked || item.checkIf || item.checkable) return "menuitemcheckable";
        if (item.radio) return "menuitemradio";
        return "menuitem";
    },
    getRowAriaState : function (rowNum) {
        if (this.hasSubmenu(this.getItem(rowNum))) return { haspopup:true };
    }
});

// There is no "menubutton" role, but with aria-haspopup NVDA 2011.1.1 at least reads
// this as "menubutton submenu".
isc.MenuButton.addProperties({
    ariaRole:"button",
    ariaState:{ haspopup:true }
});
isc.MenuBar.addProperties({
    ariaRole:"menubar"
});

} // end if (isc.GridRenderer)


if (isc.RichTextEditor) {

isc.ListPropertiesSampleTile.addMethods({
    ariaState: {},
    _ariaLabelMap: {
        "disc": "Bullets",
        "circle": "Circles",
        "square": "Filled squares",
        "decimal": "Numbers",
        "upper-roman": "Uppercase Roman numerals",
        "lower-roman": "Lowercase Roman numerals",
        "upper-alpha": "Uppercase letters",
        "lower-alpha": "Lowercase letters"
    },
    _otherUnorderedListAriaLabel: "Other bulleted style",
    _otherOrderedListAriaLabel: "Other numbered style",
    getAriaState : function () {
        var state = isc.addProperties({}, this.ariaState);

        var listProperties = this._canonicalProperties,
            style = listProperties.style;
        if (style == "custom-image") {
            var image = listProperties.image;
            var lastSlashPos = image.lastIndexOf('/');
            if (lastSlashPos >= 0) {
                image = image.substring(lastSlashPos + 1);
            }
            state.label = "Custom bullet image '" + image + "'";
        } else if (this._ariaLabelMap.hasOwnProperty(style)) {
            state.label = this._ariaLabelMap[style];
        } else {
            var isUnordered = isc.ListPropertiesPane.getListType(listProperties) == "unordered";
            state.label = isUnordered ? this._otherUnorderedListAriaLabel
                                      : this._otherOrderedListAriaLabel;
        }

        return state;
    },
    getAriaStateAttributes : function () {
        return isc.Canvas.getAriaStateAttributes(this.getAriaState());
    }
});

isc.ListPropertiesPane.addProperties({
    //< @attr listPropertiesPane.sampleTileLayoutAriaLabel (String : "List style" : IR)
    // The ARIA label to use for the +link{ListPropertiesPane.sampleTileLayout,sampleTileLayout}.
    // @group i18nMessages
    //<
    sampleTileLayoutAriaLabel: "List style"
});

isc.ListPropertiesPane.changeDefaults("sampleTileLayoutDefaults", {
    ariaRole: "radiogroup",
    ariaState: {
        haspopup: false
    },
    init : function () {
        this.Super("init", arguments);
        this.ariaState = isc.addProperties({}, this.ariaState, {
            label: this.creator.sampleTileLayoutAriaLabel
        });
    }
});

isc.ListPropertiesPane.changeDefaults("sampleTileDefaults", {
    ariaRole: "radio",
    setSelected : function () {
        this.Super("setSelected", arguments);
        this.setAriaState("checked", this.isSelected());
    }
});

} // end if (isc.RichTextEditor)


//>SectionStack
if (isc.SectionStack) {


isc.SectionStack.addProperties({
    ariaRole: "tablist",
    ariaState: {
        multiselectable: false
    },
    sectionHeaderAriaRole: "tab",

    getAriaState : function () {
        var ariaState = isc.addProperties({}, this.ariaState);
        ariaState.multiselectable = (this.visibilityMode != "mutex");
        return ariaState;
    }
});

isc.SectionStack.changeDefaults("tabPanelDefaults", {
    ariaRole: "tabpanel",
    ariaState: {
        // manage our own 'aria-hidden'
        hidden: true,
        labelledby: null,
        owns: null
    },
    getAriaState : function () {
        var ariaState = isc.addProperties({}, this.ariaState);
        var tab = this._tab;
        if (tab != null) {
            ariaState.hidden = tab.hidden || !tab.expanded;
            ariaState.labelledby = tab._getAriaHandleID();
            var tabItems = tab.items;
            if (tabItems != null && tabItems.length > 0) {
                var itemIDs = tabItems.map("_getAriaHandleID");
                ariaState.owns = itemIDs.join(" ");
            }
        }
        return ariaState;
    }
});

isc._commonScreenReaderProps = {
    ariaState: {
        controls: null
    },
    getAriaState : function () {
        var ariaState;
        if (isc.isA.SectionHeader(this)) {
            ariaState = this.Super("getAriaState", arguments);
        } else {
            
            ariaState = isc.addProperties({}, ariaState); 
            if (isc.isA.StatefulCanvas(this.background) && this.background.label != null) {
                ariaState.labelledby = this.background.label._getAriaHandleID();
            } else if (this.background != null) {
                ariaState.labelledby = this.background._getAriaHandleID();
            }
        }

        var sectionStack = this.layout;
        if (isc.isA.SectionStack(sectionStack)) {
            var multiselectable = (sectionStack.visibilityMode != "mutex");
            if (multiselectable) {
                ariaState.expanded = !!this.expanded;
            } else {
                ariaState.selected = !!this.expanded;
            }
        }

        var tabPanel = this._tabPanel;
        if (tabPanel != null) {
            ariaState.controls = tabPanel._getAriaHandleID();
        }

        // To indicate that the user cannot expand/collapse the section, set 'aria-disabled' to true.
        // "Indicates that the element is perceivable but disabled, so it is not editable or otherwise operable."
        // http://www.w3.org/TR/wai-aria/states_and_properties#aria-disabled
        if (this.canCollapse == false) {
            ariaState.disabled = true;
        }

        return ariaState;
    }
};

isc.SectionHeader.addProperties(isc._commonScreenReaderProps);
isc.ImgSectionHeader.addProperties(isc._commonScreenReaderProps);

}
//<SectionStack


// SplitPane
// ---------------------------------------------------------------------------------------
if (isc.SplitPaneSidePanel) {

isc.SplitPaneSidePanel.addProperties({
    ariaState: {
        // manage our own 'aria-hidden' state
        hidden: true
    },
    getAriaState : function () {
        var state = isc.addProperties({}, this.ariaState);
        state.hidden = !this.onScreen;
        return state;
    }
});

} // end if (isc.SplitPaneSidePanel)


isc._tab_getAriaLabel = function () {
    var label = this.Super("getAriaLabel", arguments);
    var tabBar = this.parentElement;
    if (isc.isA.TabBar(tabBar)) {
        var tabSet = tabBar.parentElement;
        if (isc.isA.TabSet(tabSet)) {
            var isClosable = tabSet.canCloseTab(this);
            if (isClosable && tabSet.ariaCloseableSuffix) {
                label = label == null ? tabSet.ariaCloseableSuffix : label + tabSet.ariaCloseableSuffix;
            }
        }
    }
    return label;
};

if (isc.ImgTab) {
isc.ImgTab.addProperties({
    getAriaLabel : isc._tab_getAriaLabel
});
}
if (isc.SimpleTabButton) {
isc.SimpleTabButton.addProperties({
    getAriaLabel : isc._tab_getAriaLabel
});
}


if (isc.Window) {
isc.Window.addProperties({
    ariaState: {
        labelledby: null,
        describedby: null
    },
    getAriaState : function () {
        var state = this.Super("getAriaState", arguments);
        
        var headerLabel = this.headerLabel;
        if (headerLabel != null && headerLabel.isVisible()) state.labelledby = headerLabel._getAriaHandleID();
        var body = this.body;
        if (body != null && body.isVisible()) state.describedby = body._getAriaHandleID();
        return state;
    }
});
}

if (isc.Dialog) {
isc.Dialog.addProperties({
    
    getAriaState : function () {
        var state = this.Super("getAriaState", arguments);
        var messageLabel = this.messageLabel;
        if (messageLabel != null && messageLabel.isVisible()) state.describedby = messageLabel._getAriaHandleID();
        return state;
    }
});
}


(function () {
    var roleMap = {
        Button : "button",
        StretchImgButton : "button",
        ImgButton : "button",
        Label : "label",
        
        // Section stacks - headers are "heading"s
        
        SectionHeader:"heading",
        ImgSectionHeader:"heading",
    
        // FormItems
        CheckboxItem : "checkbox",
        Slider : "slider",

        TextItem : "textbox",

        // TextArea is textbox + plus multiple:true state 
        // http://www.w3.org/WAI/PF/aria/states_and_properties#aria-multiline
        TextAreaItem : "textbox", 

        Window : "dialog",
        Dialog : "alertdialog",
        Toolbar : "toolbar",

        // a good default.  Without this NVDA will read an HTMLFlow as just "section" and stop.  With
        // this, contents are read.
        HTMLFlow:"article",
        HTMLPane:"article",

        // not doing this by default since lots of components use Layouts in various internal
        // ways that do not correspond to a "group"
        //Layout : "group", 

        // NOTE example shows 'tablist' element surrounding 'tab's but not 'tabpanel's
        // http://www.mozilla.org/access/dhtml/class/tabpanel
        TabBar : "tablist",
        PaneContainer : "tabpanel",
        ImgTab : "tab",
        
        EdgedCanvas : "presentation",
        BackMask : "presentation"
        
    }
    for (var className in roleMap) {
        var theClass = isc.ClassFactory.getClass(className);
        if (theClass) theClass.addProperties({ariaRole:roleMap[className]});
    }
})();
