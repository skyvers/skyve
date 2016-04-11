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
//>	@class	ToolStripGroup
// 
// A widget that groups other controls for use in +link{class:ToolStrip, tool-strips}.
//
// @treeLocation Client Reference/Layout
// @visibility external
//<
isc.defineClass("ToolStripGroup", "VLayout").addProperties({

    //> @attr toolStripGroup.styleName (CSSClassName : "toolStripGroup" : IRW)
    // CSS class applied to this ToolStripGroup.
    // @visibility external
    //<
    styleName: "toolStripGroup",

    layoutMargin: 2,
    membersMargin: 1,
    
    layoutAlign: "top",
    
    autoDraw: false,
    
    height: 1,
    width: 1,
    overflow: "visible",

    //> @attr toolStripGroup.controls (Array of Widget : null : IRW)
    // The array of controls to show in this group.
    // @visibility external
    //<

    //> @attr toolStripGroup.labelLayout (AutoChild HLayout : null : IR)
    // HLayout autoChild that houses the +link{toolStripGroup.label, label} 
    // in which the +link{toolStripGroup.title, title text} is displayed.
    // <P>
    // This can be customized via the standard +link{type:AutoChild} pattern.
    // @visibility external
    //<

    labelLayoutDefaults: {
        _constructor: "HLayout",
        width: "100%",
        height: 22
    },

    //> @attr toolStripGroup.labelConstructor (String : "Label" : IRA)
    // SmartClient class for the +link{toolStripGroup.label, title label} AutoChild.
    // @visibility external
    //<
    labelConstructor: "Label",

    //> @attr toolStripGroup.label (AutoChild Label : null : IR)
    // AutoChild +link{class:Label, Label} used to display the 
    // +link{toolStripGroup.title, title text} for this group.
    // <P>
    // Can be customized via the standard +link{type:AutoChild} pattern, and various 
    // convenience APIs exist for configuring it after initial draw: see 
    // +link{toolStripGroup.setShowTitle, setShowTitle}, 
    // +link{toolStripGroup.setTitle, setTitle}, 
    // +link{toolStripGroup.setTitleAlign, setTitleAlign},
    // +link{toolStripGroup.setTitleHeight, setTitleHeight},
    // +link{toolStripGroup.setTitleOrientation, setTitleOrientation} and
    // +link{toolStripGroup.setTitleStyle, setTitleStyle}.
    // @visibility external
    //<
    labelDefaults: {
        width: "100%",
        height: 18,
        autoDraw: true,
        wrap: false,
        overflow: "visible"
    },

    //> @attr toolStripGroup.titleAlign (Alignment : "center" : IRW)
    // Controls the horizontal alignment of the group's 
    // +link{toolStripGroup.title, title-text}, within its 
    // +link{toolStripGroup.label, label}.  Setting this
    // attribute overrides the default specified by 
    // +link{toolStrip.groupTitleAlign, groupTitleAlign} on the containing 
    // +link{class:ToolStrip, ToolStrip}.
    // @setter toolStripGroup.setTitleAlign
    // @visibility external
    //<
    //titleAlign: "center",

    //> @attr toolStripGroup.titleStyle (CSSClassName : "toolStripGroupTitle" : IRW)
    // CSS class applied to the +link{toolStripGroup.label, title label} in this group.
    // @setter toolStripGroup.setTitleStyle
    // @visibility external
    //<
    titleStyle: "toolStripGroupTitle",

    //> @attr toolStripGroup.autoSizeToTitle (Boolean : true : IR)
    // By default, ToolStripGroups are assigned a minimum width that allows the entire title 
    // to be visible.  To prevent this bahavior and have group-titles cut off when they're 
    // wider than the buttons they contain, set this attribute to false
    // @visibility external
    //<
    autoSizeToTitle: true,

    //> @attr toolStripGroup.titleOrientation (VerticalAlignment : "top" : IRW)
    // Controls the +link{toolStripGroup.titleOrientation, vertical orientation} of 
    // this group's +link{toolStripGroup.label, title label}.  Setting this
    // attribute overrides the default specified by 
    // +link{toolStrip.groupTitleAlign, groupTitleOrientation} on the containing 
    // +link{class:ToolStrip, ToolStrip}.
    // @setter toolStripGroup.setTitleOrientation
    // @visibility external
    //<
    //titleOrientation: "top",

    //> @attr toolStripGroup.titleProperties (AutoChild Label : null : IRW)
    // AutoChild properties for fine customization of the 
    // +link{toolStripGroup.label, title label}.
    // @visibility external
    // @deprecated set these properties directly via the +link{toolStripGroup.label, label autoChild}
    //<
    
    //> @attr toolStripGroup.titleHeight (Number : 18 : IRW)
    // Controls the height of the +link{toolStripGroup.label, title label} in this group.
    // @setter toolStripGroup.setTitleHeight
    // @visibility external
    //<
    titleHeight: 18,

    //> @attr toolStripGroup.body (AutoChild HLayout : null : IR)
    // HLayout autoChild that manages multiple +link{toolStripGroup.columnLayout, VLayouts} 
    // containing controls.
    // @visibility external
    //<

    //> @attr toolStripGroup.bodyConstructor (String : "HLayout" : IRA)
    // SmartClient class for the body.
    // @visibility external
    //<
    bodyConstructor:"HLayout",

    bodyDefaults: {
        width: 1,
        height: "*",
        overflow: "visible",
        membersMargin: 2,
        autoDraw: false
    },

    //> @attr toolStripGroup.columnLayout (MultiAutoChild VLayout : null : IR)
    // AutoChild VLayouts created automatically by groups.  Each manages a single column of
    // child controls in the group.  Child controls that support <code>rowSpan</code> may 
    // specify it in order to occupy more than one row in a single column.  See 
    // +link{toolStripGroup.numRows, numRows} for related information.
    // @visibility external
    //<
    // some autochild defaults for the individual VLayouts that represent columns
    columnLayoutDefaults: {
        _constructor: "VLayout",
        width: 1,
        membersMargin: 2,
        height: "100%",
        overflow: "visible",
        autoDraw: false,
        numRows: 0,
        addMember : function (member, position) {
            this.Super("addMember", arguments);
    
            if (member.rowSpan == null) member.rowSpan = 1;
            var height = member.rowSpan * this.creator.rowHeight + 
                ((member.rowSpan-1) * this.membersMargin);

            if (member.orientation == "vertical") {
                member.rowSpan = this.maxRows;
                height = (member.rowSpan * this.creator.rowHeight) + 
                    ((this.maxRows-1) * this.membersMargin);
            }
            member.setHeight(height);
            this.numRows += member.rowSpan;
            if (this.numRows > this.maxRows) this.numRows = this.maxRows;
        },
        removeMember : function (member) {
            this.Super("removeMember", arguments);

            if (member._dragPlaceHolder) return;
            if (member.rowSpan == null) member.rowSpan = 1;
            this.numRows -= member.rowSpan;

            member.markForDestroy();
            member = null;
        }
    },

    //> @attr toolStripGroup.numRows (Number : 1 : IRW)
    // The number of rows of controls to display in each column.  Each control will take one
    // row in a +link{toolStripGroup.columnLayout, columnLayout} by default, but those that 
    // support the feature may specify <code>rowSpan</code> to override that.
    // <P>
    // Note that settings like this, which affect the group's layout, are not applied directly
    // if changed at runtime - a call to +link{toolStripGroup.reflowControls, reflowControls} 
    // will force the group to reflow.
    // @visibility external
    //<
    numRows: 1,

    //> @attr toolStripGroup.rowHeight (Number : 26 : IRW)
    // The height of rows in each column.
    // @visibility external
    //<
    rowHeight: 26,

    defaultColWidth: "*",

    initWidget : function () {
        this.Super("initWidget", arguments);

        var showLabel = this.showTitle != false && this.showLabel != false;

        if (showLabel) {
            this.addAutoChild("labelLayout", { height: this.titleHeight });

            var labelProps = isc.addProperties({}, this.titleProperties || {}, {
                styleName: this.titleStyle,
                height: this.titleHeight,
                maxHeight: this.titleHeight,
                align: this.titleAlign,
                contents: this.title,
                autoDraw: false
            });
            
            if (this.autoSizeToTitle == false) labelProps.overflow = "hidden";

            this.addAutoChild("label", labelProps);

            this.labelLayout.addMember(this.label);
            
            if (this.showTitle == false) this.labelLayout.hide();
            this.addMember(this.labelLayout);
        }

        this.addAutoChild("body", {
            _constructor: this.bodyConstructor,
            height: this.numRows * this.rowHeight,
            resized: function () {
                var newWidth = this.getVisibleWidth();
                if (this.parentElement.labelLayout) this.parentElement.labelLayout.setWidth(newWidth);
                if (this.parentElement.label) this.parentElement.label.setWidth(newWidth);
            }
        });

        this.addMember(this.body, showLabel ? (this.titleOrientation == "bottom" ? 0 : 1) : 0);

        if (this.controls) {
            this.addControls(this.controls, false);
        }
        
    },

    //> @attr toolStripGroup.title (String : null : IRW)
    // The title text to display in this group's 
    // +link{toolStripGroup.label, title label}.
    // @setter toolStripGroup.setTitle
    // @visibility external
    //<

    //> @method toolStripGroup.setTitle()
    // Sets the +link{toolStripGroup.title, text} to display in this group's
    // +link{toolStripGroup.label, title label} after initial draw.
    // 
    // @param title (String) The new title for this group
    // @visibility external
    //<
    setTitle : function (title) {
        this.title = title;
        if (this.label) this.label.setContents(this.title);
    },

    //> @method toolStripGroup.setShowTitle()
    // This method forcibly shows or hides this group's 
    // +link{toolStripGroup.label, title label} after initial draw.
    // 
    // @param showTitle (boolean) should the title be shown or hidden?
    // @visibility external
    //<
    setShowTitle : function (showTitle) {
        this.showTitle = showTitle;
        if (!showTitle && this.labelLayout && this.labelLayout.isVisible()) this.labelLayout.hide();
        else if (showTitle && this.labelLayout && !this.labelLayout.isVisible()) this.labelLayout.show();
    },

    //> @method toolStripGroup.setTitleAlign()
    // This method forcibly sets the horizontal alignment of the 
    // +link{toolStripGroup.title, title-text}, within the 
    // +link{toolStripGroup.label, title label}, after initial draw.
    // 
    // @param align (Alignment) the new alignment for the text, left or right
    // @visibility external
    //<
    setTitleAlign : function (align) {
        this.titleAlign = align;
        if (this.label) this.label.setAlign(this.titleAlign);
    },

    //> @method toolStripGroup.setTitleStyle()
    // This method forcibly sets the +link{toolStripGroup.titleStyle, CSS class name}  
    // for this group's +link{toolStripGroup.label, title label} after initial draw.
    // 
    // @param styleName (CSSClassName) the CSS class to apply to the 
    //                                 +link{toolStripGroup.label, title label}.
    // @visibility external
    //<
    setTitleStyle : function (styleName) {
        this.titleStyle = styleName;
        if (this.label) {
            this.label.setStyleName(this.titleStyle);
            if (this.label.isDrawn()) this.label.redraw();
        }
    },

    //> @method toolStripGroup.setTitleOrientation()
    // This method forcibly sets the 
    // +link{toolStripGroup.titleOrientation, vertical orientation} of this group's 
    // +link{toolStripGroup.label, title label} after initial draw.
    // 
    // @param orientation (VerticalAlignment) the new orientation for the title, either bottom or top
    // @visibility external
    //<
    setTitleOrientation : function (orientation) {
        this.titleOrientation = orientation;
        if (this.label && this.labelLayout) {
            if (this.titleOrientation == "top") {
                this.removeMember(this.labelLayout);
                this.addMember(this.labelLayout, 0);
            } else if (this.titleOrientation == "bottom") {
                this.removeMember(this.labelLayout);
                this.addMember(this.labelLayout, 1);
            }
        }
    },

    //> @method toolStripGroup.setTitleHeight()
    // This method forcibly sets the height of this group's 
    // +link{toolStripGroup.label, title label} after initial draw.
    // 
    // @param titleHeight (Integer) the new height for the +link{toolStripGroup.label, title label}
    // @visibility external
    //<
    setTitleHeight : function (titleHeight) {
        this.titleHeight = titleHeight;
        if (this.label) this.label.setHeight(this.titleHeight);
    },

    addColumn : function (index, controls) {
        var undef;
        if (index === null || index === undef) {
            index = this.body.members.length;
        }

        var colWidth = this.defaultColWidth;
        if (this.colWidths && this.colWidths[index] != null) colWidth = this.colWidths[index];

        var newColumn = this.createAutoChild("columnLayout", 
            { maxRows: this.numRows, numRows: 0, width: colWidth, height: this.body.getVisibleHeight()-1 }
        );
        this.body.addMember(newColumn, index);

        if (controls) newColumn.addMembers(controls);

        return newColumn;
    },

    getAvailableColumn : function (createIfUnavailable) {
        var members = this.body.members;

        if (members && members.length > 0) {
            for (var i=0; i<members.length; i++) {
                var member = members[i];
                //this.logWarn("member " + member + " numRows is " + member.numRows);
                if (member.numRows < member.maxRows) return member;
            }
        }

        if (createIfUnavailable != false) return this.addColumn();
        return null;
    },

    //> @method toolStripGroup.getControlColumn()
    // Return the +link{toolStripGroup.columnLayout, column widget} that contains the passed 
    // control.
    // 
    // @param control (Canvas) the control to find in this group
    // @return (Layout) the column widget containing the passed control
    // @visibility external
    //<
    getControlColumn : function (control) {
        var members = this.body.members;

        if (members && members.length > 0) {
            for (var i=members.length-1; i>=0; i--) {
                if (members[i].members.contains(control)) return members[i];
            }
        }

        return null;
    },

    //> @method toolStripGroup.setControls()
    // Clears the array of controls and then adds the passed array to this toolStripGroup, 
    // creating new +link{toolStripGroup.columnLayout, columns} as necessary, according to each
    // control's <code>rowSpan</code> attribute and the group's 
    // +link{toolStripGroup.numRows, numRows} attribute.
    // 
    // @param controls (Array of Canvas) an array of widgets to add to this group
    // @visibility external
    //<
    setControls : function (controls, store) {
        if (this.controls) {
            this.removeAllControls();
        }
        this.addControls(controls, store);
    },

    //> @method toolStripGroup.reflowControls()
    // Forces this group to reflow following changes to attributes that affect layout, like 
    // +link{toolStripGroup.numRows, numRows}.
    // 
    // @visibility external
    //<
    reflowControls : function () {
        if (this.controls) {
            this.removeAllControls(false);
        }
        this.addControls(this.controls, false);
    },

    //> @method toolStripGroup.addControls()
    // Adds an array of controls to this group, creating new 
    // +link{toolStripGroup.columnLayout, columns} as necessary, according to each control's 
    // <code>rowSpan</code> value and the group's 
    // +link{toolStripGroup.numRows, numRows} value.
    // 
    // @param controls (Array of Canvas) an array of widgets to add to this group
    // @visibility external
    //<
    addControls : function (controls, store) {
        if (!controls) return;
        if (!isc.isAn.Array(controls)) controls = [controls];

        for (var i=0; i<controls.length; i++) {
            this.addControl(controls[i], null, store);
        }
    },

    //> @method toolStripGroup.addControl()
    // Adds a control to this toolStripGroup, creating a new 
    // +link{toolStripGroup.columnLayout, column} as necessary, according to the control's 
    // <code>rowSpan</code> value and the group's 
    // +link{toolStripGroup.numRows, numRows} value.
    // 
    // @param control (Canvas) a widget to add to this group
    // @param [index] (Integer) optional insertion index for this control
    // @visibility external
    //<
    addControl : function (control, index, store) {
        if (!control) return null;
        
        var undef;
        if (index === null || index === undef || index >= this.numRows) index = this.numRows-1;

        var column = this.getAvailableColumn(true);
        
        if (!this.controls) this.controls = [];
        if (store != false) {
            if (!this.controls.contains(control)) this.controls.add(control);
        }
        column.addMember(control, index);
        column.reflowNow();
        if (!control.isVisible()) control.show();
    },

    //> @method toolStripGroup.removeControl()
    // Removes a control from this toolStripGroup, destroying an existing 
    // +link{toolStripGroup.columnLayout, column} if this is the last widget in that column.
    // 
    // @param control (Canvas) a widget to remove from this group
    // @visibility external
    //<
    autoHideOnLastRemove: false,
    removeControl : function (control, destroy) {
        control = isc.isAn.Object(control) ? control : this.getMember(control);
        if (!control) return null;

        var column = this.getControlColumn(control);

        if (column) {
            column.members.remove(control);
            if (destroy) this.controls.remove(control);
            else this.addChild(control);
            if (column.members.length == 0) {
                // if the column is now empty, destroy it
                column.hide();
                this.body.members.remove(column);
                column.markForDestroy();
                column = null;
            }
        }
        
        if (this.body.members.length == 0 && this.autoHideOnLastRemove) {
            // hide ourselves
            this.hide();
        }
    },

    removeAllControls : function (destroy) {
        if (!this.controls || this.controls.length == 0) return null;
        
        for (var i=this.controls.length-1; i>=0; i--) {
            var control = this.controls[i];
            control.hide();
            this.removeControl(control, destroy);
            if (destroy) {
                control.markForDestroy();
                control = null;
            }
        }
        
        // clear out nulls - that is, any controls that got destroyed
        this.controls.removeEmpty();
        
        // shrink the group's body layout, so it can overflow properly when new controls arrive
        this.body.height = 1;
        this.height = 1;
        this.redraw();
    },
    
    resized : function () {
        this._updateLabel();
    },
    
    draw : function () {
        this.Super("draw", arguments);
        
        this._updateLabel();
    },

    redraw : function () {
        this.Super("redraw", arguments);
        
        this._updateLabel();
    },

    _updateLabel : function () {
        var visibleWidth = this.getVisibleWidth(),
            margin = this.layoutMargin,
            newWidth = this.getVisibleWidth() - (this.layoutMargin*3)
        ;

        if (this.labelLayout) this.labelLayout.setWidth(newWidth);
        if (this.label) this.label.setWidth(newWidth);
    }
    
});


//>	@class	IconButton
// A Button subclass that displays an +link{iconButton.icon, icon}, 
// +link{iconButton.showButtonTitle, title} and optional +link{iconButton.menuIconSrc, menuIcon} 
// and is capable of horizontal and vertical +link{iconButton.orientation, orientation}.
//
// @treeLocation Client Reference/Layout
// @visibility external
//<
isc.defineClass("IconButton", "Button").addProperties({

width: 1,
overflow: "visible",
height: 1,

padding: 3,

autoDraw: false,

usePartEvents: true,

//> @attr iconButton.orientation (String : "horizontal" : IRW)
// The orientation of this IconButton.  The default value, "horizontal", renders 
// +link{iconButton.icon, icon}, +link{iconButton.showButtonTitle, title} and potentially 
// +link{iconButton.menuIconSrc, menuIcon}, from left to right: "vertical" does the same from 
// top to bottom.
// 
// @visibility external
//<
orientation: "horizontal",

//> @attr iconButton.rowSpan (Number : 1 : IRW)
// When used in a +link{class:RibbonBar}, the number of rows this button should occupy in a
// single +link{toolStripGroup.columnLayout, column}.
// @visibility external
//<
rowSpan: 1,

//> @attr iconButton.baseStyle (CSSClassName : "iconButton" : IRW)
// Default CSS class for this button.
//
// @visibility external
//<
baseStyle: "iconButton",

//> @attr iconButton.showMenuIcon (Boolean : false : IRW)
// Whether to show the +link{menuIconSrc, menu-icon} which fires the +link{menuIconClick} 
// notification method when clicked.
//
// @visibility external
//<
showMenuIcon: false,

//> @attr iconButton.menuIconSrc (SCImgURL : "[SKINIMG]/Menu/submenu_down.png" : IRW)
// Base URL for an Image that shows a +link{class:Menu, menu} when clicked.  See also 
// +link{iconButton.showMenuIconDisabled} and +link{iconButton.showMenuIconOver}.
//
// @visibility external
//<
menuIconSrc: "[SKINIMG]/Menu/submenu_down.png",

//> @attr iconButton.menuIconWidth (Number : 14 : IRW)
// The width of the icon for this button.
//
// @visibility external
//<
menuIconWidth: 14,

//> @attr iconButton.menuIconHeight (Number : 13 : IRW)
// The height of the icon for this button.
//
// @visibility external
//<
menuIconHeight: 13,

menuIconStyleCSS: "vertical-align:middle; border:1px solid transparent; -moz-border-radius: 3px; " +
    "-webkit-border-radius: 3px; -khtml-border-radius: 3px; border-radius: 3px;"
,

menuConstructor: isc.Menu,

//> @attr iconButton.iconOrientation (String : null : IRW)
// This attribute is not supported in this subclass.  However, RTL mode is still supported.
// 
// @visibility external
//<

//> @attr iconButton.iconAlign (String : null : IRW)
// This attribute is not supported in this subclass.  However, RTL mode is still supported.
// 
// @visibility external
//<

//> @attr iconButton.align (Alignment : null : IRW)
// Horizontal alignment of this button's content.  If unset, 
// +link{iconButton.orientation, vertical buttons} are center-aligned and horizontal
// buttons left-aligned by default.
// @group appearance
// @visibility external
//<
align: null,

//> @attr iconButton.valign (VerticalAlignment : null : IRW)
// Vertical alignment of this button's content.  If unset, 
// +link{iconButton.orientation, vertical buttons} are top-aligned and horizontal
// buttons center-aligned by default.
// @group appearance
// @visibility external
//<
valign: null,

initWidget : function () {
    if (this.orientation == "vertical") {
        this.align = this.align || "center";
        this.valign = this.valign || "top";
    } else {
        this.align = this.align || "left";
        this.valign = this.valign || "center";
    }
    
    this._originalAlign = this.align;
    this._originalVAlign = this.valign;
    
    this._originalTitle = this.title;
    this._originalIcon = this.icon;

    this.Super("initWidget", arguments);

},

//> @attr iconButton.showTitle (Boolean : null : IRW)
// showTitle is not applicable to this class - use +link{iconButton.showButtonTitle}
// instead.
//
// @visibility external
//<

//> @attr iconButton.showButtonTitle (Boolean : true : IRW)
// Whether to show the title-text for this IconButton.  If set to false, title-text is omitted
// altogether and just the icon is displayed.
//
// @visibility external
//<
showButtonTitle: true,


//> @attr iconButton.showIcon (Boolean : null : IRW)
// Whether to show an Icon in this IconButton.  Set to false to render a text-only button.
//
// @visibility external
//<

//> @attr iconButton.icon (SCImgURL : null : IRW)
// Icon to show to the left of or above the title, according to the button's +link{orientation}.
// <P>
// When specifying <code>titleOrientation = "vertical"</code>, this icon will be stretched to 
// the +link{largeIconSize} unless a +link{largeIcon} is specified.
//
// @visibility external
//<

//> @attr iconButton.iconSize (Number : 16 : IRW)
// The size of the normal icon for this button.
//
// @visibility external
//<
iconSize: 16,

//> @method iconButton.setIcon()
// Sets a new Icon for this button after initialization.
//
// @visibility external
//<
setIcon : function (icon) {
    // we don't use the regular "icon", but instead we build it into the "title" - store the 
    // new icon appropriately and rebuild the title to incorporate the new icon.
    this._originalIcon = icon;
    this.setTitle(this._originalTitle);
},

//> @attr iconButton.largeIcon (SCImgURL : null : IRW)
// Icon to show above the title when +link{orientation} is "vertical".
// <P>
// If a largeIcon is not specified, the +link{icon, normal icon} will be stretched to 
// the +link{largeIconSize}.
//
// @visibility external
//<

//> @method iconButton.setLargeIcon()
// Sets a new Large-Icon for vertical buttons after initialization - synonymous with 
// +link{iconButton.setIcon, setIcon} for normal horizontal buttons.
//
// @visibility external
//<
setLargeIcon : function (icon) {
    // set the largeIcon and rebuild the title to incorporate it.
    this.largeIcon = icon;
    this.setTitle(this._originalTitle);
},

//> @attr iconButton.largeIconSize (Number : 32 : IRW)
// The size of the large icon for this button.  If +link{largeIcon} is not specified, the
// +link{icon, normal icon} will be stretched to this size.
//
// @visibility external
//<
largeIconSize: 32,

setTitle : function (title) {
    this._originalTitle = title;
    this.Super("setTitle", arguments);
    this.getTitle();
    this.align = this._originalAlign;
    this.valign = this._originalVAlign;
    this.redraw();
},

titleSeparator: "&nbsp;",
getTitle : function () {

    var isLarge = this.orientation == "vertical",
        icon = this.showIcon == false ? null :
            (isLarge ? this.largeIcon || this._originalIcon : this._originalIcon),
        iconSize = (isLarge ? this.largeIconSize : this.iconSize),
        title = this.showButtonTitle ? this._originalTitle : ""
    ;

    if (icon == "") icon = null;

    if (icon && this.showDisabledIcon && this.isDisabled()) {
        var dotIndex = icon.lastIndexOf("."),
            tempIcon = dotIndex > 0 ? 
                        icon.substring(0, dotIndex) + "_Disabled" + icon.substring(dotIndex) :
                        icon + "_Disabled"
        ;

        icon = tempIcon;
    }

    var iconCSS = "vertical-align:middle;" + (isLarge ? "margin-bottom:5px;" : ""),
        menuIconCSS = this.menuIconStyleCSS + (isLarge ? "margin-top:4px;" : ""),
        img = icon ? this.imgHTML({
            src: icon,
            width: iconSize,
            height: iconSize,
            extraCSSText: iconCSS,
            extraStuff: " eventpart='icon'"
        }) : null
    ;

    var menuIcon = null;
    if (this.showMenuIcon) {
        var menuIconUrl = this._getMenuIconURL();

        menuIcon = this.menuIcon = this.showMenuIcon ? 
            this.imgHTML({
                src: menuIconUrl,
                width: this.menuIconWidth,
                height: this.menuIconHeight,
                name: "menuIcon",
                extraCSSText: menuIconCSS,
                extraStuff: " eventpart='menuIcon'"
            }) : null;
        ;
    }

    this.icon = null;
    
    var tempTitle = title,
        title = img || ""
    ;

    if (this.orientation == "vertical") {
        if (this.showButtonTitle) {
            if (title != "") title += "<br>";
            title += tempTitle;
        }
        if (this.showMenuIcon && menuIcon) {
            if (title != "") title += "<br>";
            title += menuIcon;
        }
    } else {
        this.valign = "center";
        if (this.showButtonTitle) {
            if (title != "") title += this.titleSeparator;
            title += "<span style='vertical-align:middle'>" + tempTitle + "</span>";
        }
        if (this.showMenuIcon && menuIcon) {
            if (title != "") title += this.titleSeparator;
            title += menuIcon;
        }
    }

    this.title = title;
    return title;
},

_getMenuIconURL : function () {
    var state = this.state,
        selected = this.selected,
        customState = this.getCustomState(),
        sc = isc.StatefulCanvas
    ;

    //this.logWarn(isc.echoFull("state is " + state));

    // ignore states we don't care about
    if (state == sc.STATE_DISABLED && !this.showMenuIconDisabled) state = null;
    else if (state == sc.STATE_OVER && (!this.showMenuIconOver || !this.showingMenuButtonOver)) 
        state = null;

    var focused = null; //this.showFocusedMenuIcon ? this.getFocusedState() : null; 
    var icon = this.menuIconSrc;
    return isc.Img.urlForState(icon, selected, focused, state, null, customState);
},

setHandleDisabled : function () {
    this.Super("setHandleDisabled", arguments);
    this.setTitle(this._originalTitle);
},

setDisabled : function (disabled) {
    // when we change disabled-status, rebuild the title and redraw
    this.Super("setDisabled", arguments);
    this.setTitle(this._originalTitle);
},

mouseOut : function () {
    this.Super("mouseOut", arguments);
    
    if (this.showingMenuButtonOver) this.menuIconMouseOut();
},

//> @method iconButton.menuIconClick()
// Notification method fired when a user clicks on the menuIcon on this IconButton.  
// <smartclient>Return false to suppress the standard click handling code.</smartclient>
// <smartgwt>call <code>event.cancel()</code> to suppress the standard 
// click handling code.</smartgwt>
//
// @return (Boolean) return false to cancel event-bubbling
// @visibility external
//<
menuIconClick : function () { return true; },

//> @method iconButton.iconClick()
// Notification method fired when a user clicks on the +link{iconButton.icon, icon} in this 
// IconButton.  
// <smartclient>Return false to suppress the standard click handling code.</smartclient>
// <smartgwt>call <code>event.cancel()</code> to suppress the standard 
// click handling code.</smartgwt>
//
// @return (Boolean) return false to cancel event-bubbling
// @visibility external
//<
iconClick : function () { return true; },

//> @method iconButton.click()
// Notification method fired when a user clicks anywhere on this button.  If the click occurred
// directly on the +link{button.icon, icon} or the +link{iconButton.menuIconSrc, menuIcon}, 
// the related notifications +link{iconButton.iconClick, iconClick} and 
// +link{iconButton.menuIconClick, menuIconClick} are fired first and must return false to 
// prevent this notification from firing.
// <P>
// If a +link{class:Menu, menu} is installed then, by default, it is only displayed when a 
// user clicks on the +link{iconButton.menuIconSrc, menuIcon}.  This can be altered via 
// +link{iconButton.showMenuOnClick, showMenuOnClick}.
//
// @return (Boolean) return false to cancel event-bubbling
// @visibility external
//<
click : function () {
    if (this.showMenuOnClick) this.showMenu();
},

//> @attr iconButton.showMenuOnClick (Boolean : null : IRW)
// If set to true, shows this button's +link{class:Menu, menu} when a user clicks anywhere 
// in the button, rather than specifically on the +link{iconButton.menuIconSrc, menuIcon}.
//
// @visibility external
//<
//showMenuOnClick: false,

//> @attr iconButton.showMenuIconOver (Boolean : true : IRW)
// Whether to show an Over version of the +link{menuIconSrc, menuIcon}.
//
// @visibility external
//<
showMenuIconOver: true,

//> @attr iconButton.showMenuIconDisabled (Boolean : true : IRW)
// Whether to show a Disabled version of the +link{menuIconSrc, menuIcon}.
//
// @visibility external
//<
showMenuIconDisabled: true,

menuIconMouseMove : function () {
    if (!this.showMenuIconOver || this.showingMenuButtonOver) return;

    var element = this.getImage("menuIcon");

    if (element) {
        this.showingMenuButtonOver = true;
        this.setTitle(this._originalTitle);
        //element.style.border = this.menuIconOverBorderCSS;
    }
},

menuIconMouseOut : function () {
    if (!this.showMenuIconOver) return;

    var element = this.getImage("menuIcon");

    if (element) {
        this.showingMenuButtonOver = false;
        this.setTitle(this._originalTitle);
        //element.style.border = "1px solid transparent";
    }
}


});

//>	@class	IconMenuButton
// A subclass of +link{IconButton} that shows a menuIcon by default and implements showMenu().
// <P>
// This class has +link{iconButton.showMenuIcon,showMenuIcon} set to <code>true</code> by default,
// and has a +link{iconButton.menuIconClick} handler which will show the specified 
// +link{iconMenuButton.menu} via a call to +link{iconMenuButton.showMenu()}.
// This menuIconClick handler cancels default click behavior, so, if a user clicks the menu 
// item, any specified +link{canvas.click,click handler} for the button as a whole will not fire.
//
// @treeLocation Client Reference/Layout
// @visibility external
//<
isc.defineClass("IconMenuButton", "IconButton").addProperties({

usePartEvents: true,

showMenuIcon: true,

menuIconClick : function () {
    this.showMenu();
    return false;
},

//>	@attr iconMenuButton.menu (Menu : null : IRW)
// The menu to show when the +link{iconButton.menuIconSrc, menu-icon} is clicked.
// <P>
// For a menu button with no menu (menu: null) the up/down arrow image can
// be suppressed by setting
// +link{menuButton.showMenuButtonImage, showMenuButtonImage}: <code>false</code>.
//
// @visibility external
//<
menu:null,

//> @attr iconMenuButton.menuAnimationEffect (string : null : IRWA)
// Allows you to specify an animation effect to apply to the menu when it is being shown.
// Valid options are "none" (no animation), "fade", "slide" and "wipe".
// If unspecified falls through to <code>menu.showAnimationEffect</code>
// @visibility animation
//<

//> @method iconMenuButton.showMenu()
// Shows this button's +link{iconMenuButton.menu}.  Called automatically when a user clicks the 
// +link{iconButton.menuIconSrc, menuIcon}.
//
// @return (Boolean) true if a menu was shown
// @visibility external
//<
showMenu : function () {
    // lazily create the menu if necessary, so we can init with, or set menu to, an object 
    // properties block
    if (isc.isA.String(this.menu)) this.menu = window[this.menu];
    if (!isc.isA.Menu(this.menu)) this._createMenu(this.menu);
    if (!isc.isA.Menu(this.menu)) return false;

    var menu = this.menu;
    
    // draw offscreen so that we can figure out what size the menu is
    // Note that we use _showOffscreen which handles figuring out the size, and
    // applying scrollbars if necessary.
    menu._showOffscreen();

    // figure out the left coordinate of the drop-down menu
    var left = this.getPageLeft();

    //left = left - (menu.getVisibleWidth() - this.getVisibleWidth()); 

    var top = this.getPageTop()+this.getVisibleHeight()+1;

    // don't allow the menu to show up off-screen
    menu.placeNear(left, top);
    menu.show(this.menuAnimationEffect);

    return true;
},

_createMenu : function (menu) {
    if (!menu) return;
    menu.autoDraw = false;

    var cons = this.menuConstructor || isc.Menu;
    this.menu = cons.create(menu);
}


});


//> @class RibbonBar
//
// A +link{class:ToolStrip, ToolStrip-based} class for showing 
// +link{class:RibbonGroup, groups} of related buttons and other controls.
//
// @treeLocation Client Reference/Layout
// @visibility external
//<
isc.defineClass("RibbonBar", "ToolStrip").addProperties({

    groupConstructor: "RibbonGroup",
    
    //> @method ribbonBar.addGroup()
    // Add a new group to this RibbonBar. You can either create your group externally and pass 
    // it in, or you can pass a properties block from which to automatically construct it.
    //
    // @param group (RibbonGroup) the new group to add to this ribbon
    // @param [position] (Integer) the index at which to insert the new group
    // @visibility external
    //<
    addGroup : function (group, position) {
        return this.addToolStripGroup(group, position);
    }

});
//> @class RibbonGroup
// 
// A widget that groups other controls for use in +link{class:RibbonBar, RibbonBars}.
// 
// @treeLocation Client Reference/Layout
// @visibility external
//<
isc.defineClass("RibbonGroup", "ToolStripGroup").addProperties({

    //> @attr ribbonGroup.newControlConstructor (Class : "IconButton" : IR)
    // Widget class for controls +link{createControl, created automatically} by this 
    // RibbonGroup.  Since +link{newControlConstructor, such controls} are created via the 
    // autoChild system, they can be further customized via the newControlProperties property.
    // 
    // @visibility external
    //<
    newControlConstructor: "IconButton",
    //> @attr ribbonGroup.newControlDefaults (MultiAutoChild IconButton : null : IR)
    // Properties used by +link{ribbonGroup.createControl, createControl} when creating new
    // controls.
    // 
    // @visibility external
    //<
    newControlDefaults: {
    },

    //> @method ribbonGroup.createControl()
    // Creates a new control and adds it to this RibbonGroup.  The control is created using the 
    // autoChild system, according to the specified
    // +link{ribbonGroup.newControlConstructor, constructor} and the passed properties are
    // applied to it.
    //
    // @param properties (Canvas Properties) properties to apply to the new control
    // @param [position] (Integer) the index at which to insert the new control
    // 
    // @visibility external
    //<
    createControl : function (properties, position) {
        var newControl = this.createAutoChild("newControl", properties);

        return this.addControl(newControl, position);
    } 

});



