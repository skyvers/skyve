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
//>	@class	ToolStrip
// 
// Base class for creating toolstrips like those found in browsers and office applications: a
// mixed set of controls including +link{ImgButton,icon buttons}, 
// +link{button.radioGroup,radio button groups}, +link{MenuButton,menus},
// +link{ComboBoxItem,comboBoxes}, +link{LayoutSpacer,spacers}, +link{Label,status displays} and 
// +link{SelectItem,drop-down selects}.  
// <P>
// All of the above components are placed in the +link{ToolStrip.members,members array} to form
// a ToolStrip.  Note that the +link{FormItem,FormItems} mentioned above (ComboBox and
// drop-down selects) need to be placed within a +link{DynamicForm} as usual.
// <P>
// <smartclient>
// The following strings can be used to add special behaviors:
// <ul>
// <li>the String "separator" will cause a separator to be created (instance of 
// +link{toolStrip.separatorClass})
// <li>the String "resizer" will cause a resizer to be created (instance of
// +link{toolStrip.resizeBarClass}).  This is equivalent to setting
// +link{canvas.showResizeBar,showResizeBar:true} on the preceding member.
// <li>the String "starSpacer" will cause a spacer to be created (instance of
// +link{class:LayoutSpacer}).
// </ul>
// </smartclient>
// <smartgwt>
// Instances of the following classes can be used to add special behaviors:
// <ul>
// <li>the +link{class:ToolStripSeparator} class will show a separator.
// <li>the +link{class:ToolStripResizer} class will show a resizer. This is equivalent to setting
// +link{canvas.showResizeBar,showResizeBar:true} on the preceding member.
// <li>the +link{class:ToolStripSpacer} class will show a spacer.
// </ul>
// See the +explorerExample{toolstrip} example.
// </smartgwt>
//
// @treeLocation Client Reference/Layout
// @visibility external
// @example toolstrip
//<

isc.defineClass("ToolStrip", "Layout").addProperties({
	
    //> @attr toolStrip.members (Array of Canvas : null : IR)
    // Array of components that will be contained within this Toolstrip, like
    // +link{Layout.members}. Built-in special behaviors can be indicated as
    // describe +link{class:ToolStrip,here}.
    // 
    // @visibility external
    // @example toolstrip
    //<
    

    //> @attr toolStrip.height (Number : 20 : IRW)
    // ToolStrips set a default +link{Canvas.height,height} to avoid being stretched by
    // containing layouts.
    // @group sizing
    // @visibility external
    //<
    height: 20,
    
    defaultWidth: 250,

    //> @attr toolStrip.styleName (CSSClassName : "toolStrip" : IRW)
    // CSS class applied to this toolstrip.
    // <P>
    // Note that if +link{toolStrip.vertical} is true for this toolStrip, 
    // +link{toolStrip.verticalStyleName} will be used instead of this value if it is non-null.
    //<
    styleName: "toolStrip",
    
    //> @attr toolStrip.verticalStyleName (CSSClassName : null : IR)
    // Default stylename to use if +link{toolStrip.vertical,this.vertical} is true.
    // If unset, the standard +link{styleName} will be used for both vertical and horizontal
    // toolstrips.
    // <P>
    // Note that this property only applies to the widget at init time. To modify the 
    // styleName after this widget has been initialized, you should
    // simply call +link{canvas.setStyleName(),setStyleName()} rather than updating this 
    // property.
    // @group appearance
    // @visibility external
    //<
    
	//>	@attr	toolStrip.vertical		(Boolean : false : IR)
	// Indicates whether the components are drawn horizontally from left to right (false), or
    // vertically from top to bottom (true).
	//		@group	appearance
    //      @visibility external
	//<
	vertical:false,

    //> @attr toolStrip.resizeBarClass (String : "ToolStripResizer" : IR)
    // Customized resizeBar with typical appearance for a ToolStrip.
    // @visibility external
    //<
    // NOTE: class definition in Splitbar.js
    resizeBarClass: "ToolStripResizer",

	//> @attr toolStrip.resizeBarSize (int : 14 : IRA)
    // Thickness of the resizeBars in pixels.
    // @visibility external
	//<
    resizeBarSize: 14,

    //> @attr toolStrip.separatorClass (String : "ToolStripSeparator" : IR)
    // Class to create when the string "separator" appears in +link{toolStrip.members}.
    // @visibility external
    //<
    separatorClass : "ToolStripSeparator",

    //> @attr toolStrip.separatorSize (int : 8 : IR)
    // Separator thickness in pixels
    // @visibility external
    //<
    separatorSize : 8,

    //> @attr toolStrip.showGroupTitle (Boolean : true : IR)
    // If set, this attribute affects whether +link{class:ToolStripGroup, ToolStripGroups}
    // in this ToolStrip show their header control.  You can override this at the 
    // +link{toolStripGroup.setShowTitle, individual ToolStripGroup} level.
    // @visibility external
    //<
    showGroupTitle : true,
    
    //> @attr toolStrip.groupTitleAlign (Alignment : "center" : IR)
    // If set, this attribute affects the alignment of the titles in 
    // +link{class:ToolStripGroup, ToolStripGroups}.  You can override this at the 
    // +link{toolStripGroup.titleAlign, individual ToolStripGroup} level.
    // @visibility external
    //<
    groupTitleAlign : "center",

    //> @attr toolStrip.groupTitleOrientation (VerticalAlignment : "top" : IR)
    // If set, this attribute affects the orientation of the titles in 
    // +link{class:ToolStripGroup, ToolStripGroups}.  You can override this at the 
    // +link{toolStripGroup.titleAlign, individual ToolStripGroup} level.
    // @visibility external
    //<
    groupTitleOrientation : "top",

    initWidget : function (a,b,c,d,e,f) {
        this.members = this._convertMembers(this.members);
        this.invokeSuper(isc.ToolStrip, this._$initWidget, a,b,c,d,e,f);
        
        if (this.vertical && this.verticalStyleName != null) {
            this.setStyleName(this.verticalStyleName);
        }
        
    },

    // support special "separator" and "resizer" strings
    _convertMembers : function (members) {
        if (members == null) return null;
        var separatorClass = isc.ClassFactory.getClass(this.separatorClass, true),
            newMembers = [];
        for (var i = 0; i < members.length; i++) {
            var m = members[i];
            if (m == "separator") {
                var separator = separatorClass.createRaw();
                separator.autoDraw = false;
                separator.vertical = !this.vertical;
                if (this.vertical) {
                    separator.height = this.separatorSize;
                } else {
                    separator.width = this.separatorSize;
                }
                separator.completeCreation();
                newMembers.add(isc.SGWTFactory.extractFromConfigBlock(separator));
            } else if (m == "resizer" && i > 0) {
                members[i-1].showResizeBar = true;
            } else if (m == "starSpacer") {
                
                var params = (this.vertical ? { height: "*" } : { width: "*" });
                newMembers.add(isc.LayoutSpacer.create(params));
            // handle being passed an explicitly created ToolStripResizer instance.
            // This is normal usage from Component XML or SGWT
            } else if (isc.isA.ToolStripResizer(m) && i > 0) {
                members[i-1].showResizeBar = true;
                m.destroy();
            } else {
                // handle being passed an explicitly created ToolStripSeparator instance.
                // This is normal usage from Component XML or SGWT
                if (isc.isA.ToolStripSeparator(m)) {
                    var separator = m;
                    separator.vertical = !this.vertical;
                    separator.setSrc(this.vertical ? separator.hSrc : separator.vSrc);
                    if (this.vertical) {
                        separator.setHeight(this.separatorSize);
                    } else {
                        separator.setWidth(this.separatorSize);
                    }
                    separator.markForRedraw();
                } else if (isc.isA.ToolStripSpacer(m)) {
                    // Grab desired size from marker class and create a new instance
                    var size = m.space >> 0 || "*";
                    m.destroy();
                    var params = (this.vertical ? { height: size } : { width: size });
                    m = isc.ToolStripSpacer.create(params);
                } else if (isc.isA.ToolStripGroup(m)) {
                    // apply some overrides here
                    if (!m.showTitle) m.setShowTitle(this.showGroupTitle);
                    if (!m.titleAlign) m.setTitleAlign(this.groupTitleAlign);
                    if (!m.titleOrientation) m.setTitleOrientation(this.groupTitleOrientation);
                } else {
                    // punt to Canvas heuristics
                    m = this.createCanvas(m);
                }

                newMembers.add(m);
            }
        }
        return newMembers;
    },
    addMembers : function (newMembers, position, dontAnimate,d,e) {
        if (!newMembers) return;
        if (!isc.isAn.Array(newMembers)) newMembers = [newMembers];

        var firstMember = newMembers[0],
            isResizerWidget = isc.isA.ToolStripResizer(firstMember);
        if (firstMember == "resizer" || isResizerWidget) {
            position = position || this.members.length;
            var precedingPosition = Math.min(position, this.members.length) -1;
            if (precedingPosition > 0) {
                var precedingMember = this.getMember(precedingPosition);
                if (precedingMember != null) {
                    precedingMember.showResizeBar = true;
                    this.reflow();
                }
            }
            var resizer = newMembers.shift();
            if (isResizerWidget) resizer.destroy();
        }

        newMembers = this._convertMembers(newMembers);
        return this.invokeSuper(isc.ToolStrip, "addMembers", newMembers,position,dontAnimate,d,e);
    },

    groupConstructor: "ToolStripGroup",
    addToolStripGroup : function (group, position) {
        if (!group) return null;

        if (!isc.isA.Class(group)) {
            var cons = this.groupConstructor;
            if (isc.isA.String(cons)) {
                cons = isc.ClassFactory.getClass(this.groupConstructor, true);
            }
            group = cons.create(group);
        }

        if (!group || !isc.isA.ToolStripGroup(group)) return null;

        // apply some overrides here
        if (group.showTitle == null) group.setShowTitle(this.showGroupTitle);
        if (!group.titleAlign) group.setTitleAlign(this.groupTitleAlign);
        if (!group.titleOrientation) group.setTitleOrientation(this.groupTitleOrientation);
        
        this.addMember(group, position);
        return group;
    },

    //> @method toolStrip.addFormItem()
    // Add a form item to this toolStrip. This method will create a DynamicForm autoChild with the
    // item passed in as a single item, based on the 
    // +link{formWrapper,formWrapper config}, and add it to the toolstrip
    // as a member.
    // Returns a pointer to the generated formWrapper component.
    // @param formItem (FormItem Properties) properties for the form item to add to this
    //  toolStrip.
    // @param [formProperties] (DynamicForm Properties) properties to apply to the generated
    //  formWrapper component. If passed, specified properties will be overlaid onto the
    //  properties derived from +link{toolStrip.formWrapperDefaults} and
    //  +link{toolStrip.formWrapperProperties}.
    // @param [position] (integer) desired position for the form item in the tools
    // @return (DynamicForm) generated wrapper containing the form item.
    // @visibility external
    //<
    addFormItem : function (formItem, formProperties, position) {
        // Sanity check - if passed a canvas, add it and return.
        if (isc.isA.Canvas(formItem)) {
            this.addMember(formItem, position);
            return formItem;
        }
        
        var wrapper = this.createAutoChild("formWrapper", formProperties);
        wrapper.setItems([formItem]);
        this.addMember(wrapper, position);
        return wrapper;
        
    },

    //> @attr toolStrip.formWrapper (MultiAutoChild DynamicForm : null : IR)
    // DynamicForm instance created by +link{addFormItem()} to contain form items for
    // display in this toolStrip. Each time addFormItem() is run, a new formWrapper
    // autoChild will be created, picking up properties according to the standard
    // +link{type:AutoChild} pattern.
    // @visibility external
    //<

    //> @attr toolStrip.formWrapperConstructor (String : "DynamicForm" : IRA)
    // SmartClient class for generated +link{toolStrip.formWrapper} components.
    // @visibility external
    //<
    formWrapperConstructor:"DynamicForm",

    //> @attr toolStrip.formWrapperDefaults (Object : ... : IR)
    // Default properties to apply to +link{formWrapper} components. Default object
    // is as follows:
    // <pre>
    // { showTitle:false,
    //   numCols:1,
    //   overflow:"visible",
    //   width:1, height:1 }
    // </pre>
    // @visibility external
    //<
    formWrapperDefaults:{
        showTitle:false,
        numCols:1,
        overflow:"visible",
        width:1, height:1
    }
    
    //> @attr toolStrip.formWrapperProperties (Object : null : IR)
    // Properties to apply to +link{formWrapper} components.
    // @visibility external
    //<

});

//> @class ToolStripSeparator
// Simple subclass of Img with appearance appropriate for a ToolStrip separator
// @treeLocation Client Reference/Layout/ToolStrip
//
// @visibility external
//<
isc.defineClass("ToolStripSeparator", "Img").addProperties({
    //> @attr toolStripSeparator.skinImgDir (URL : "images/ToolStrip/" : IR)
    // Path to separator image.
    // @visibility external
    //<
    skinImgDir:"images/ToolStrip/",

    //> @attr toolStripSeparator.vSrc (SCImgURL : "[SKIN]separator.png" : IR)
    // Image for vertically oriented separator (for horizontal toolstrips).
    // @visibility external
    //< 
    vSrc:"[SKIN]separator.png",

    //> @attr toolStripSeparator.hSrc (SCImgURL : "[SKIN]hseparator.png" : IR)
    // Image for horizontally oriented separator (for vertical toolstrips).
    // @visibility external
    //< 
    hSrc:"[SKIN]hseparator.png",

    // NOTE: we keep the default imageType:"stretch", which looks fine for the default image,
    // which is just two vertical lines.
    
    // prevents misalignment if ToolStrip is stretched vertically by members
    layoutAlign:"center",

    initWidget : function () {
        // vertical switch of hSrc/vSrc is handled by StretchImg, but not by Img
        if (isc.isA.Img(this)) this.src = this.vertical ? this.vSrc : this.hSrc;

        this.Super("initWidget", arguments);
    },

    _markerName: "separator",

    // Don't write Component XML as separate entity
    _generated: true,
    // Don't write anything but constructor in Component XML
    updateEditNode : function (editContext, editNode) {
        editContext.removeNodeProperties(editNode, ["autoDraw", "ID", "title"]);
    }
});

//> @class ToolStripSpacer
// Simple subclass of LayoutSpacer with appearance appropriate for a ToolStrip spacer
// @treeLocation Client Reference/Layout/ToolStrip
//
// @visibility external
//<
isc.defineClass("ToolStripSpacer", "LayoutSpacer").addProperties({

    //> @attr toolStripSpacer.space (Number : null : IR)
    // Size of spacer. If not specified, spacer fills remaining space.
    // @visibility external
    //<

    _markerName: "starSpacer",

    // Don't write Component XML as separate entity
    _generated: true,
    // Don't write anything but constructor in Component XML
    updateEditNode : function (editContext, editNode) {
        editContext.removeNodeProperties(editNode, ["autoDraw", "ID", "title"]);
    }
});

//> @class ToolStripButton
// Simple subclass of StretchImgButton with appearance appropriate for a ToolStrip button.
// Can be used to create an icon-only button, and icon with text, or a text only button by setting the 
// icon and title attributes as required.
// @visibility external
// @treeLocation Client Reference/Layout/ToolStrip
//<
isc.defineClass("ToolStripButton", "StretchImgButton").addProperties({
    
    showTitle:true,
    showRollOver:true,
    showDown:true,

    
    labelHPad:6, 
    labelVPad:0,
    autoFit:true,

     
    initWidget : function () {
        if (!this.title) this.iconSpacing = 0;
        this.Super("initWidget", arguments);
    },
    setTitle : function (newTitle) {
        if (!newTitle) {
            this.iconSpacing = 0;
            if (this.label) this.label.iconSpacing = 0;
        }
        this.Super("setTitle", arguments);
    },

    src:"[SKIN]/ToolStrip/button/button.png",
    capSize:3,
    height:22
});

