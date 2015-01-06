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

 


//> @class SectionStack
// A container that manages a list of sections of widgets, each with a header.  Sometimes called
// an "Accordion".
// <P>
// SectionStack can be configured so that only one section is visible at a time (similar to MS Outlook's
// left-hand Nav), or to allow multiple sections to be visible and share the available space.
//
// @treeLocation Client Reference/Layout
// @visibility external
// @example sectionsExpandCollapse
//<
isc.defineClass("SectionStack", "VLayout");

//>!BackCompat 2005.6.15 old name: "ListBar"
isc.addGlobal("ListBar", isc.SectionStack);
//<!BackCompat

isc.SectionStack.addProperties({
    //> @attr sectionStack.overflow (Overflow : "hidden" : IR)
    // Normal +link{type:Overflow} settings can be used on layouts, for example, an
    // overflow:auto Layout will scroll if sections are resized to exceed the specified size,
    // whereas an overflow:visible Layout will grow to accommodate the resized sections.
    // @visibility external
    //<
    overflow:"hidden",

    //> @attr sectionStack.styleName (CSSStyleName : "sectionStack" : IR)
    // Default CSS style for the SectionStack as a whole.
    // @visibility external
    //<
    styleName:"sectionStack",

    // Section Header Creation
	// ---------------------------------------------------------------------------------------
 
    //> @attr sectionStack.sectionHeaderClass (Classname : "SectionHeader" : IRA)
    // Widget to use for section headers.  
    // <p>
    // Must be a subclass of either +link{ImgSectionHeader} or +link{SectionHeader}.  The
    // default class used depends on the skin; +link{SectionHeader} is the simpler and
    // lighter-weight class and uses CSS styling rather than image-based styling, and is
    // recommended for most use cases.
    // <p>
    // <smartgwt>
    // If you create a custom section header class in Java, enable +link{group:reflection} to
    // allow it to be used.
    // <p>
    // Alternatively, you can use the &#83;martClient class system to create a simple
    // &#83;martClient subclass of either SectionHeader or ImgSectionHeader for use with this
    // API - see the +link{group:skinning,Skinning Guide} for details.
    // </smartgwt>
    // <smartclient>
    // Very advanced developers can use the following information to create custom header
    // classes.
    // <P>
    // The SectionStack will instantiate this class, giving the following properties on init:
    // <ul>
    // <li><code>layout</code>: the SectionStack
    // <li><code>expanded</code>: true or false
    // <li><code>hidden</code>: true or false
    // <li><code>title</code>: section title
    // </ul>
    // Whenever the section is hidden or shown, sectionHeader.setExpanded(true|false) will be
    // called if implemented.
    // </smartclient>
    // <p>
    // If you override event handlers on your custom SectionHeader or radically change it's
    // structure such that the default event handling no longer works, you can call 
    // +link{SectionStack.sectionHeaderClick()} to replicate the built-in expand/collapse
    // handling for clicking a section header.
    //
    // @visibility external
    //<
    sectionHeaderClass:"SectionHeader",
   
    //> @attr sectionStack.headerHeight (Number : 20 : IR)
    // Height of headers for sections.
    // @visibility external
    //<
    headerHeight:20,
    
    // sectionStack header styles for printing
    printHeaderStyleName:"printHeader",


    //> @attr sectionStack.tabPanel (MultiAutoChild Canvas : null : R)
    // In +link{isc.setScreenReaderMode(),screen reader mode}, a <code>tabPanel</code> component
    // is created for each section to own all of the section's +link{SectionStackSection.items,items}.
    // @group accessibility
    //<
    tabPanelDefaults: {
        _constructor: "Canvas",
        overflow: "hidden",
        visibility: "hidden",
        // Hide using display:none so as not to affect scrollHeight
        hideUsingDisplayNone: true,
        // Suppress adjustOverflow() because the tabPanel is always hidden using display:none,
        // so _browserDoneDrawing() is always false and this causes an infinite loop of delayed
        // adjustOverflow() attempts.
        _suppressAdjustOverflow: true
        
    },


    // All Sections
	// ---------------------------------------------------------------------------------------
    
    //> @attr SectionStack.sections (Array of SectionStackSection Properties : null : [IR])
    // List of sections of components managed by this SectionStack.
    //
    // @see sectionStack.getSections()
    // @visibility external
    // @example sectionsExpandCollapse
    //<
 
    //> @attr SectionStack.canResizeSections (Boolean : true : [IRA])
    // Whether sections can be drag resized by the user dragging the section header.
    // <P>
    // Note that, with <code>canResizeSections:true</code>, not all sections can be resized:
    // sections that contain only +link{Button.autoFit,autofitting} components or that are
    // marked with +link{SectionStackSection.resizeable,section.resizeable:false} will not be
    // resizeable.
    //
    // @visibility external
    //<
    canResizeSections:true,

    //> @attr sectionStack.canDropComponents (Boolean : true : IRA)
    // SectionStacks provide the same default implementation of drag and drop interactions as
    // +link{layout.canDropComponents, Layouts}, except that members are added as items into
    // the section over which they're dropped.
    // <P>
    // If you want to completely suppress the builtin drag and drop logic, but still receive drag
    // and drop events for your own custom implementation, set +link{Canvas.canAcceptDrop} to
    // <code>true</code> and <code>canDropComponents</code> to <code>false</code> on your
    // SectionStack.
    // 
    // @group dragdrop
    // @visibility external
    //<
    
    // whether to allow the user to change the overall size of the SectionStack by resizing
    // sections
    
    canResizeStack:true,
    
    //> @attr SectionStack.canReorderSections (Boolean : false : [IRA])
    // Whether sections can be drag reordered by the user dragging the section header.
    // <P>
    // Note that, with <code>canReorderSections:true</code>, sections with 
    // +link{SectionStackSection.canReorder,section.canReorder:false} will not be
    // able to be drag-reordered (though their index may still be changed by dropping other
    // sections above or below them in the section stack).
    //
    // @visibility external
    //<
    canReorderSections:false,
    
    //> @attr SectionStack.canTabToHeaders (boolean : null : [IRA])
    // If true, the headers for the sections (if shown) will be included in the page's tab
    // order for accessibility. 
    // May be overridden at the Section level via +link{SectionStackSection.canTabToHeader}
    // <P>
    // If unset, section headers will be focusable if +link{isc.setScreenReaderMode} has been called.
    // See +link{group:accessibility}.
    // @visibility external
    //<

    //> @attr SectionStack.scrollSectionIntoView (Boolean : true : [IR])
    // If an expanded or shown section expands past the current viewport and this property is
    // true, then the viewport will auto-scroll to fit as much of the section content into the
    // viewport without scrolling the top of the section out of the viewport.
    //
    // @visibility external
    //<
    scrollSectionIntoView: true,

    // NOTE: vertical:false (horizontal stacks) does work, however the default SectionHeader
    // class has a height of 20 which needs to be wiped to allow vertical stretching.  And, you
    // have to have a strategy for vertical text labels.
    //     isc.defineClass("MyHeader", "SectionHeader").addProperties({height:null});
    //     isc.SectionStack.create({
    //         vertical:false,
    //         sectionHeaderClass:"MyHeader",
    //vertical:true,

    // Section Header Properties
    // ---------------------------------------------------------------------------------------

    //> @object SectionStackSection
    // Section descriptor used by a SectionStack to describe a section of items which are shown
    // or hidden together along with their associated header. 
    // <P>
    // A section header (see +link{sectionStack.sectionHeaderClass}) is created from this descriptor when
    // the SectionStack is drawn. Any changes after creation  must be made to the section header:
    // +link{sectionStack.getSectionHeader}.
    // <P>
    // Additional SectionHeader properties set on the SectionStackSection not explicitly documented such as
    // "iconAlign" or "prompt" is supported<smartgwt> - use
    // <code>setAttribute()</code></smartgwt>.
    //
    // @treeLocation Client Reference/Layout/SectionStack
    // @visibility external
    //<

    //> @attr SectionStackSection.name (String : null : [IR])
    // Identifier for the section.  This can be used later in calls to +link{SectionStack} APIs such as
    // +link{sectionStack.expandSection} and +link{sectionStack.collapseSection}. Note that if no name
    // is specified for the section, one will be auto-generated when the section is created.
    // This property should be a string which may be used as a valid JavaScript identifier
    // (should start with a letter and not contain space or special characters such as "*").
    // @visibility external
    //<

    //> @attr SectionStackSection.ID (String : null : [IR])
    // Optional ID for the section. If +link{SectionStack.useGlobalSectionIDs} is true, this property will
    // be applied to the generated SectionStackHeader widget as a standard widget ID, meaning
    // it should be unique within a page.
    // <P>
    // <b>Backcompat Note</b>: Section stack sections may be uniquely identified within a stack
    // via the +link{SectionStackSection.name} attribute (introduced in Jan 2010). Prior to this,
    // the section ID attribute was used in this way (and would not be applied to the section header 
    // as a widget ID). For backwards compatibility this is still supported: If 
    // <code>section.name</code> is unspecified for a section but <code>section.ID</code> is set,
    // the ID will be used as a default name attribute for the section. For backwards compatibility
    // we also disable the standard behavior of having the <code>section.ID</code> being applied to the generated
    // section header (thereby avoiding the page-level uniqueness requirement) by defaulting 
    // +link{SectionStack.useGlobalSectionIDs} to false.
    //  
    // @visibility external
    //<

    //> @attr sectionStackSection.controls (Array of Canvas : null : IR)
    // Custom controls to be shown on top of this section header.
    // <P>
    // These controls are shown in the +link{sectionHeader.controlsLayout}.
    // <P>
    // Note that this is an init-time property. If you need to dynamically change what 
    // controls are displayed to the user, we would recommend embedding the
    // controls in a Layout or similar container. 
    // This will allow you to show/hide or add/remove members at runtime
    // by manipulating the existing control(s) set up at init time.
    // @example sectionControls
    // @visibility external
    //<
    
    //> @attr SectionStack.useGlobalSectionIDs (Boolean : false : [IR])
    // Should any specified +link{SectionStackSection.ID} be applied to the generated SectionHeader widget
    // for the section as a widget ID? If set to false, SectionStackSection.ID will behave as a synonym for
    // SectionStackSection.name.
    //
    // @visibility external
    //< 
    // Default to false for back-compat
    useGlobalSectionIDs:false,
 
    //> @attr SectionStackSection.title (String : null : [IR])
    // Title to show for the section
    // @visibility external
    //<
    //> @attr sectionStackSection.clipTitle
    // @include sectionHeader.clipTitle
    // @visibility external
    //<
    //> @attr sectionStackSection.showClippedTitleOnHover
    // @include sectionHeader.showClippedTitleOnHover
    // @visibility external
    //<

    //> @attr SectionStackSection.items (Array of Canvas : null : [I])
    // List of Canvases that constitute the section.  These Canvases will be shown and hidden
    // together.
    // @visibility external
    //<

    //> @attr SectionStackSection.showHeader (Boolean : true : [I])
    // If true, a header will be shown for this section.  If false, no header will be shown.
    // @visibility external
    //<
    
    //> @attr sectionStackSection.canTabToHeader (boolean : null : IR)
    // If true, the header for this Section will be included in the page's tab
    // order for accessibility. May also be set at the +link{SectionStack} level via 
    // +link{SectionStack.canTabToHeaders}.
    // <P>
    // See +link{group:accessibility}.
    //
    // @visibility external
    //<

    //> @attr sectionStackSection.icon   (SCImgURL : "[SKIN]SectionHeader/opener.gif" : [IR])
    // Base filename of the icon that represents open and closed states. The default settings
    // also change the icon for disabled sections, so a total of four images are required
    // (opened, closed, Disabled_opened, Disabled_closed).
    // <P>
    // Not shown if +link{sectionStackSection.canCollapse} is false.
    //
    // @visibility external
    //<
    

    //> @attr SectionStackSection.resizeable (boolean : null : [I])
    // If set to false, then the items in this section will not be resized by sectionHeader
    // repositioning.  You may also set this flag directly on any of the items in any section to
    // cause that item to not be resizeable.
    // @visibility external
    // @example resizeSections
    //<

    //> @attr SectionStackSection.canReorder (boolean : null : [I])
    // If set to false, then this sectionHeader will not be able to be dragged to perform a drag
    // reorder, if +link{SectionStack.canReorderSections} is true.
    // You can also disable dropping other sections before this one by setting 
    // +link{canvas.canDropBefore,canDropBefore} to false.
    // @visibility external
    //<

    //> @attr SectionStackSection.canDropBefore (boolean : null : [I])
    // @include Canvas.canDropBefore
    // @visibility external
    //<
    
    //> @attr SectionStackSection.expanded (boolean : false : [I])
    // Sections default to the collapsed state unless +link{SectionStackSection.showHeader} is
    // set to <code>false</code> in which case they default to the expanded state.  This
    // attribute allows you to explicitly control the expand/collapse state of the
    // section by overriding the above default behavior.  
    // @visibility external
    //<

    //> @attr SectionStackSection.hidden (boolean : false : [I])
    // Sections default to the visible state.  This
    // attribute allows you to explicitly control the visible/hidden state of the
    // section by overriding the above default behavior.  
    // @visibility external
    //<

    //> @attr SectionStackSection.canCollapse (Boolean : true : [I])
    // This attribute controls whether or not the expand/collapse UI control is shown on the
    // header of this section.  Any section can still be expanded/collapsed programmatically,
    // regardless of this setting.
    // @visibility external
    // @example sectionsExpandCollapse
    //<
    
    //>Animation
    // ---------------------------------------------------------------------------------------
    //> @attr sectionStack.animateSections (boolean : null : IRW)
    // If true, sections are animated during expand/collapse and addition/removal of
    // SectionItems is likewise animated.
    // @group animation
    // @visibility animation
    // @example animateSections
    //<

    // change layout default effect for showing/hiding members; "slide" is appropriate for
    // eg Window minimize, but "wipe" is usually the best effect for SectionStacks 
    animateMemberEffect:"wipe",
    //<Animation

    // Visibility of Sections
	// ---------------------------------------------------------------------------------------

    //> @type VisibilityMode
    // Settings for whether multiple sections can be in the expanded state simultaneously.
    //
    // @value "mutex"
    // Only one section can be expanded at a time.
    //
    // @value "multiple"
    // Multiple sections can be expanded at the same time, and will share space.
    //
    // @visibility external
    //<

    //> @attr SectionStack.visibilityMode (VisibilityMode : "mutex" : [IRW])
    // Whether multiple sections can be expanded.
    //
    // @see attr:canCollapseAll
    // @visibility external
    // @example sectionsExpandCollapse
    //<
    visibilityMode:"mutex",

    //> @attr SectionStack.canCollapseAll (Boolean : true : [IRW])
    // In +link{SectionStack.visibilityMode,visibilityMode}
    // <smartclient>"mutex"</smartclient><smartgwt>{@link com.smartgwt.client.types.VisibilityMode#MUTEX}</smartgwt>,
    // whether to allow the last remaining expanded section to be collapsed.  If false, collapsing the
    // last open section will open the next one (wrapping around at the end).
    // @visibility external
    //<
    canCollapseAll: true,

    // internal flag: if true, section stack will null out _userHeight on an eligible item when
    // hiding/collapsing sections to maintain the overall height of the SectionStack.  If
    // false, the SectionStack will grow/shrink instead.  This needs to be rolled up to Layout
    // as a policy instead.
    forceFill: true,
    
    //> @attr sectionStack.itemIndent (Number : 0 : [IRW])
    // Size, in pixels, of indentation of all member items. Items will be offset
    // and reduced in width by this amount. Overridden by
    // +link{itemStartIndent} or +link{itemEndIndent}.
    // Setting itemIndent is equivalent to setting itemStartIndent to the same amount
    // and itemEndIndent to 0.
    // @visibility external
    // @group layoutMember
    //<
    itemIndent: 0,
    
    //> @attr sectionStack.itemStartIndent (Number : undefined : [IRW])
    // Size, in pixels, of indentation of all member items relative to the start of
    // the alignment axis. For instance, for left-aligned members, 
    // itemStartIndent specifies indentation for every item from the left side of the
    // section stack. Overrides +link{itemIndent}.
    // @visibility external
    // @group layoutMember
    //<

    //> @attr sectionStack.itemEndIndent (Number : undefined : [IRW])
    // Size, in pixels, of indentation of all member items relative to the end of
    // the alignment axis. For instance, for left-aligned members, 
    // itemStartIndent specifies indentation for every item from the right side of the
    // section stack.
    // @visibility external
    // @group layoutMember
    //<

    //> @attr sectionStack.showExpandControls (Boolean : true : [IRW])
    // Whether to show the Expand/Collapse controls in the headers of sections.  If false, hides
    // the expand/collapse controls and, instead, treats a click anywhere on the header as if 
    // it were a click on the expand control.
    // @visibility external
    //<
    showExpandControls: true
});

isc.SectionStack.addMethods({

    initWidget : function () {
        this.Super(this._$initWidget, arguments);
        
        if (this.canReorderSections) this.canAcceptDrop = true;
 
        //>Animation
        if (this.animateSections != null) this.animateMembers = this.animateSections;
        //<Animation

        //>!BackCompat 2005.6.15 old name: "ListBar" with "groups"
        if (this.groups != null && this.sections == null) this.sections = this.groups;
        //<!BackCompat

        var initSections = this.sections;
        this.sections = [];
        this.addSections(initSections, null, true);
    },

    //> @method sectionStack.setVisibilityMode()
    // Setter for +link{attr:visibilityMode}.
    // @param newVisibilityMode (VisibilityMode) new <code>visibilityMode</code> setting.
    // If this is <smartclient>"mutex"</smartclient><smartgwt>{@link com.smartgwt.client.types.VisibilityMode#MUTEX}</smartgwt>
    // then all but the first expanded section is collapsed.
    // @visibility external
    //<
    setVisibilityMode : function (newVisibilityMode) {
        this.visibilityMode = newVisibilityMode;
        if (newVisibilityMode == "mutex") {
            var expandedSections = this.getExpandedSections();
            if (expandedSections != null && expandedSections.length >= 2) {
                this.collapseSection(expandedSections.slice(1));
            }
        }
        if (isc.Canvas.ariaEnabled()) {
            var multiselectable = (newVisibilityMode != "mutex");
            this.setAriaState("multiselectable", multiselectable);
            var sections = this.sections;
            if (sections != null) {
                for (var i = 0, numSections = sections.length; i < numSections; ++i) {
                    var section = sections[i];
                    section.setAriaState((multiselectable ? "expanded" : "selected"), !!section.expanded);
                }
            }
        }
    },

    _doPopOutDragMember : function (placeHolder, member) {
        var section = this.sectionForItem(member);

        if (section) {
            var index = this.getMemberNumber(member)-(this.getMemberNumber(section)+1);
            //this.logWarn("member index " + this.getMemberNumber(member) + "\n" + 
            //    "header index " + this.getMemberNumber(section) + "\n" +
            //    "offset into section " + index
            //);
            this.addItem(section, placeHolder, index);
        } else {
            this.addMember(placeHolder, this.getMemberNumber(member), true);
        }
    },

    // replace one member with another, without an intervening relayout, and without animation
    replaceMember : function (oldMember, newMember) {
        
        var oldMemberPos = this.getMemberNumber(oldMember),
            section = this.sectionForItem(oldMember)
        ;
        
        if (!section) {
            return this.Super("replaceMember", arguments);
        }
        
        var oldSetting = this.instantRelayout;
        this.instantRelayout = false;
        
        this._finishDropAnimation();
        
        var sectionIndex = this.getMemberNumber(section);
        this.removeItem(section, oldMember);

        this.addItem(section, newMember, (oldMemberPos-sectionIndex)-1);
        
        this.instantRelayout = oldSetting;
        if (oldSetting) this.reflowNow();
        
    },
   
    _dragIsSectionReorder : function () {
        if (this.canReorderSections) {
            var target = this.ns.EH.dragTarget;
            return (this.sections != null && this.sections.contains(target));
        }
        return false;
    },
    
    willAcceptDrop : function () {
        if (this._dragIsSectionReorder()) {
            var target = this.ns.EH.dragTarget;
            return (target.canReorder != false);
        }
        // otherwise allow default implementation to continue - may allow some more
        // elaborate, unrelated customization
        return this.Super("willAcceptDrop", arguments);
    },
    
    getStackDropPosition : function () {
        var coord = this.vertical ? this.getOffsetY() : this.getOffsetX();
        
        // before beginning 
        if (coord < 0) return 0;

        var totalSize = this.vertical ? this._topMargin : this._leftMargin,
            visibleMemberCount = 0
        ;
        
        for (var i = 0; i < this.members.length; i++) {
            var member = this.members[i];
            if (!member) continue;
            
            var section = member.isSectionHeader ? member : this.sectionForItem(member),
                sectionIsExpanded = this.sectionIsExpanded(section),
                memberIsVisible = member.isVisible() && sectionIsExpanded
            ;
            
            if (memberIsVisible || (member == section)) {
                if (coord < (totalSize + (size/2))) {
                    // respect an explicit canDropBefore setting, which prevents dropping before a
                    // member
                    if (member.canDropBefore === false) return false;
                    return i;
                }
                
                var size = this.memberSizes[visibleMemberCount];
                totalSize += size + this.membersMargin + this.getMemberGap(member);
                visibleMemberCount++;
            }
        }
        // last position: past halfway mark on last member
        return this.members.length;
    },

    getDropPosition : function (dropType, visibleOnly) {
        //>EditMode
        if (!this._dragIsSectionReorder()) {
            if (this.editingOn && this.editProxy) {
                return this.editProxy.getDropPosition(dropType);
            } else {
                return this.getStackDropPosition();
            }
        }
        //<EditMode
        
        var coord = this.vertical ? this.getOffsetY() : this.getOffsetX();
        // before beginning 
        if (coord < 0) return 0;
        
        var totalSize = this.vertical ? this._topMargin : this._leftMargin,
            section = this.sections[0],
            sectionIndex = 0,
            // Note: hidden members have no entries in the this.memberSizes array so we need
            // to track both visible and hidden members as we iterate through
            visibleMemberIndex = 0,
            memberIndex = 0;
        while (memberIndex < this.members.length) {
            var sectionSize = 0,
                member = this.members[memberIndex],
                currentMemberMargin = 0
            ;

            // determine the size of the entire section (header + all expanded items)
            while (member != null && 
                (member == section || (section.items && section.items.contains(member)))) {
                if (member.isVisible()) {
                    sectionSize  += this.memberSizes[visibleMemberIndex];
                    currentMemberMargin = this.membersMargin + this.getMemberGap(member);
                    sectionSize += currentMemberMargin;
                    visibleMemberIndex++;
                } 
                
                memberIndex += 1
                member = this.members[memberIndex];

            }
            
            // At this point we know how tall the section is
            if (coord < (totalSize + ((sectionSize-currentMemberMargin)/2))) {
                // respect an explicit canDropBefore setting, which prevents dropping before a
                // section
                if (section && section.canDropBefore === false) return false;
                return this.members.indexOf(section);
            }
            
            // Otherwise, increase the total size and look at the next section
            totalSize += sectionSize;
            sectionIndex += 1;
            section = this.sections[sectionIndex];            
       }
       // At this point we've gone through all members -- dropping at end
       return this.members.length;
    
    },
    
    drop : function () {
        if (!this.willAcceptDrop()) return;
        var dropPosition = this.getDropPosition(),
            dropComponent = this.getDropComponent(isc.EventHandler.getDragTarget(), dropPosition),
            isSection = this.sections && this.sections.contains(dropComponent),
            section = dropComponent
        ;

        if (!isSection) {
            if (this.canDropComponents) {
                // dropping some widget into the stack - add the widget into the items array of
                // the relevent section at an appropriate location
                section = this.sectionForMemberIndex(dropPosition);
                var indexInSection = dropPosition - (this.getMemberNumber(section) + 1);
                this.addItem(section, dropComponent, indexInSection);
            } else {
                // canDropComponents is false - just do a normal addMember()
                this.addMember(dropComponent, dropPosition);
            }
        } else if (isSection && this.canReorderSections) {
            // section-reorder
            var currentSectionIndex = this.sections.indexOf(section),
                newSectionIndex;
            
            var dropMember = this.members[dropPosition];
            if (dropMember == null) {
                newSectionIndex = this.sections.length;
            } else {
                for (var i = 0; i < this.sections.length; i++) {
                    if (dropMember == this.sections[i] || 
                        (this.sections[i].items && this.sections[i].items.contains(dropMember)))
                    {
                        newSectionIndex = i;
                    }
                }
            }
            // There's an offset to consider: if dropping *after* our current position we'll
            // be removing this section from the sections array (and all the members from the
            // members array) and re-adding in the new spot so
            // If a section is initially at index 2:
            // - Dropping at 0, or 1 will slot into those positions
            // - Dropping at 2 is a drop onto current position (no change)
            // - Dropping at 3 is a drop between self and next item - so also no change
            // - Dropping at 4 or 5 will remove us from slot 2, meaning we actually want to
            //   drop at 3 or 4.
            var dropAfter = newSectionIndex > currentSectionIndex;
            if (dropAfter) {
                newSectionIndex -=1;
            }
            
            if (newSectionIndex == currentSectionIndex) {
                return;
            }
            this.sections.slide(currentSectionIndex, newSectionIndex);
            
            var start = this.members.indexOf(section),
                end = start +1,
                items = section.items || [];
            for (var i = 0; i < items.length; i++) {
                if (this.members.contains(items[i])) end += 1;
            }
            if (dropAfter) dropPosition -= (end-start);
            
            this.logInfo("Drag reorder of sections - section:" +
                section + " moved to:" + newSectionIndex + " - reordering members from " + start +
                " to " + end + " target position:" + dropPosition);
                
            this.reorderMembers(start, end, dropPosition);
        }
   
    },
    
    //> @method sectionStack.addItem
    // Add a canvas as an item to a section.
    // @param section (String or Number) ID or index of the section to add item to
    // @param item (Canvas) Item to insert into the section
    // @param index (Number) Index into section to insert item
    // @visibility external
    //<
    addItem : function (section, item, index) {
        var canvas = this.createCanvas(item);
        if (!isc.isA.Canvas(canvas)) {
            this.logWarn("addItem passed:" + this.echo(item) + 
                    " cannot be resolved to a Canvas - ignoring");
            return
        }
        var sectionHeader = this.getSection(section);
        if (index  == null) index = 0;
        if (index >= sectionHeader.items.length) index = sectionHeader.items.length;

        // make sure that items being added have their resizeable flag set appropriately
        if (canvas.resizeable == null) {
            if (!this.canResizeSections) canvas.resizeable = false;
            else if (section.resizeable != null) {
                // allow both an explicit true and explicit false value.
                // - false allows fixed-sized sections
                // - true forces inherent height members to be resizeable
                canvas.resizeable = section.resizeable;
            }
        }

        sectionHeader.items.addAt(canvas, index);

        if (this.isDrawn() && this.sectionIsExpanded(sectionHeader)) {
            var memberIndex = 1 + this.members.indexOf(sectionHeader) + index;
            this.addMember(canvas, memberIndex);

            if (isc.Canvas.ariaEnabled()) {
                section = this.getSectionHeader(section);
                if (isc.isA.Canvas(section)) {
                    var itemIDs = section.items.map("_getAriaHandleID");
                    section._tabPanel.setAriaState("owns", itemIDs.join(" "));
                }
            }
        } else if (canvas.isDrawn()) {
            canvas.clear();
            canvas.deparent();
            // we'll lazily add it as a member when the section gets expanded!
        }
    },

    //> @method sectionStack.removeItem
    // Remove an item from a section.
    // @param section (String or Number) ID or index of the section to remove item from
    // @param item (Canvas) Item to remove
    // @visibility external
    //<
    removeItem : function (section, item) {
        if (!section) section = this.sectionForItem(item);
        if (!section) return;

        var sectionHeader = this.getSection(section);
        sectionHeader.items.remove(item);
        
        if (this.members.contains(item)) this.removeMember(item, item._isPlaceHolder);

        if (isc.Canvas.ariaEnabled()) {
            section = this.getSectionHeader(section);
            if (isc.isA.Canvas(section)) {
                var itemIDs = section.items.map("_getAriaHandleID");
                section._tabPanel.setAriaState("owns", itemIDs.join(" "));
            }
        }
    },

    //> @method sectionStack.setSectionProperties()
    // Set arbitrary properties for a particular section in this SectionStack. Properties will
    // be applied to the sectionHeader for the section.
    // <P>
    // Note that where APIs exist to explicitly manipulate section properties these should be
    // used in preference to this method. For example, to add or remove items in a section use
    // +link{sectionStack.addItem()} or +link{sectionStack.removeItem()}. To change the title of
    // a section, use +link{sectionStack.setSectionTitle}.
    // <P>
    // Also note that to modify properties of items within a section, call
    // the appropriate setter methods directly on the item you want to modify.
    //
    // @param section (String | int) ID or index of the section to modify
    // @param properties (SectionStackSection Properties) properties to apply to the section.
    // @visibility external
    //<
    setSectionProperties : function (section, properties) {
        var section = this.getSection(section);
        if (section != null) {
            if (isc.isA.Canvas(section)) {
                section.setProperties(properties);
            } else {
                isc.addProperties(section, properties);
            }
        }
    },
    
    
    // override removeChild to properly remove items / sections
    removeChild : function (child, name) {
        
        
        isc.Layout._instancePrototype.removeChild.call(this, child, name);
        //this.Super("removeChild", arguments);
        
        var sections = this.sections;
        if (sections) {
            for (var i = 0; i < sections.length; i++) {
                var section = sections[i];
                if (child == section) {
                    this.removeSection(child);
                    break;
                } else if (section.items && section.items.contains(child)) {
                    this.removeItem(section, child);
                    break;
                }
            }
        }
    },

    // sectionNameIndex, used for auto-generated section names per stack.
    sectionNameIndex:0,
    addSections : function (sections, position, expandOne) {
        if (sections == null) return;
        if (!isc.isAn.Array(sections)) sections = [sections];

        if (position == null || position > this.sections.length) {
            position = this.sections.length;
        }

        var ariaEnabled = isc.Canvas.ariaEnabled();

        for (var i = 0; i < sections.length; i++) {
            var section = sections[i];

            // support sparse arrays
            if (!section) continue;

            if (section.showHeader == null) section.showHeader = true;
            if (section.canHide == null) section.canHide = true;

            // use section.expanded if explicitly set.  Otherwise default to collapsed
            // unless showHeader is false or autoShow is true (backcompat).
            var expanded = section.expanded != null ? section.expanded :
                    // previous logic was
                    // isOpen = section.autoShow || section.showHeader == false;
                    section.autoShow || section.showHeader == false;
            if (section.hidden == null) section.hidden = false;

            // normalize items to Arrays
            if (section.items == null) section.items = [];
            else if (!isc.isA.Array(section.items)) section.items = [section.items];
            
            for (var j = 0; j < section.items.length; j++) {
                if (isc.isAn.Object(section.items[j])) section.items[j]._containerID = this.ID;
            };

            // create a header for each section, which will also serve as the section itself.
            // NOTE: if showHeader is false, we still create a header object to represent the
            // section and track it's position in the members array, but it will never be
            // show()n, hence never drawn
            var headerClass = isc.ClassFactory.getClass(this.sectionHeaderClass, true),
                sectionHeader = headerClass.createRaw();
            if (this.sectionHeaderAriaRole != null) sectionHeader.ariaRole = this.sectionHeaderAriaRole;
            sectionHeader.autoDraw = false;
            sectionHeader._generated = true;
            sectionHeader.expanded = expanded;
            sectionHeader.isSectionHeader = true;

            // if you specify hidden:true and expanded: true, then expanded wins
            sectionHeader.visibility = (section.hidden || section.showHeader == false) ? 
                isc.Canvas.HIDDEN : isc.Canvas.INHERIT;
            // a section header drag is an internal resize, never an external drop (until we
            // implement tear-offs)
            sectionHeader.dragScrollType = "parentsOnly";
            sectionHeader.dragScrollDirection = 
                this.vertical ? isc.Canvas.VERTICAL : isc.Canvas.HORIZONTAL;

            sectionHeader.layout = this;
            if (this.vertical) sectionHeader.height = this.headerHeight;
            else sectionHeader.width = this.headerHeight;
            
            
            // Name vs ID
            // As of Jan 2010, sections within a section stack may be referenced by the "name"
            // property. This is a unique identifier for the section within the stack.
            // A developer may also specify an ID for the section, which will by default be
            // passed through to the generated SectionHeader widget 
            // (meaning it must be unique within the page, not just within the sectionStack)

            // BackCompat: 20100115
            // Previous behavior was that the ID property behaved exactly like the "name"
            // property of a section -- it was an identifier that could be used to retrieve
            // a section and was not applied to the widget as the widget-ID so could be
            // unique within a section-stack but not within the page.
            // This proved tricky to work with especially in SmartGWT where it was hard to
            // retrieve this ID from a generated SectionHeader widget (as getID() returned the
            // widget ID).
            // For backwards compatibility:
            // - if a section has a specified ID but no name, the ID will be copied over to the
            //   name slot, so getSection() et al will continue to work with the specified ID
            // - if this.useGlobalSectionIDs is false, we will not apply the specified ID 
            //   property to the generated widget (so it doesn't have to be page-unique).
            var name = null, resetID = null, undef;
            if (section.name != null) name = section.name;  
            if (section.ID != null) {
                if (name == null) name = section.ID;
                // If an ID was specified, support passing it through to the generated
                // widget (will do this by default)
                if (!this.useGlobalSectionIDs) {
                    resetID = section.ID;
                    
                    if (isc.Browser.isSGWT) {
                        delete section.ID;
                        delete section._autoAssignedID;
                    } else {
                        section.ID              = undef;
                        section._autoAssignedID = undef;
                    }
                } else {
                    // detect anything with a matching global ID - this'll trip a collision
                    // which may be quite confusing in a live app.
                    var collision = window[section.ID];
                    if (collision != null) {
                        this.logWarn("Note: Section Stack Section has ID specified as '" + 
                            section.ID + "'. This collides with an existing " +
                            (isc.isA.Canvas(collision) ? "SmartClient component ID." : 
                                                        "object reference.") +
                            " The existing object will be replaced by the generated header" +
                            " for this section. To avoid applying section IDs to their" +
                            " corresponding section headers, you can set" +
                            " sectionStack.useGlobalSectionIDs to false");
                    }
                }
            }

            // If no name was specified, auto-generate one. This will allow methods like
            // getExpandedSections() to return something useful and reliable
            if (name == null) {
                name = "section" + this.sectionNameIndex++;
                //this.logWarn("name/sne:" + [name,this.sectionNameIndex]);                
            }

            var oldName = name,
            	collidingSection = this.sections.find("name", name);            	
            while (collidingSection != section && collidingSection != null) {
                name = "section" + this.sectionNameIndex++;
                collidingSection = this.sections.find("name", name);
            }
            if (oldName != name) {
                this.logWarn("Specified name for section:" + oldName + " collided with name for " +
                      "existing section in this stack. Replacing with auto-generated name:"+ name);
            }
            // actually hang onto the name (which may have changed)
            section.name = name;

            isc.addProperties(sectionHeader, section);
            
            
            sectionHeader.__ref = null;
            delete sectionHeader.__module;

            // store the section config object directly on the section header and vice versa
            sectionHeader._sectionConfig = section;
            
            // section header dragging - governable via canReorderSections
            if (this.canReorderSections && sectionHeader.canReorder != false) {
                sectionHeader.canDragReposition = true;
                sectionHeader.canDrop = true;
            }
            
            sectionHeader.completeCreation();

            // Check if we need to dereference
            sectionHeader = isc.SGWTFactory.extractFromConfigBlock(sectionHeader);
            
            section._sectionHeader = sectionHeader

            // APIs to get from one to the other.
            sectionHeader.getSectionConfig=function () {
                return this._sectionConfig;
            }
            section.getSectionHeader = function () {
                return this._sectionHeader;
            }
            
            // if we cleared the ID so as not to effect the generated widget ID,
            // restore it now so the user can continue to reference the attribute on
            // the config object originally passed in if necessary.
            if (resetID != null) {
                section.ID = resetID;
            }
            
            section = sectionHeader;

            this.sections.addAt(section, position+i);

            if (ariaEnabled) {
                var tabPanel = section._tabPanel = this.createAutoChild("tabPanel", {
                    _tab: section
                });
                this.addChild(tabPanel);
            }

            this.addMember(section, this._getSectionPosition(section), true);

            // expand any non-collapsed sections.  This will add the section's items as members 
            if (expanded && !section.hidden) {
                this.expandSection(section);
            // If it's not expanded - ensure any drawn section items are cleared since they may
            // have been drawn outside the sectionStack's scope
            } else {
                for (var ii = 0; ii < section.items.length; ii++) {
                    var item = section.items[ii];
                    if (item.parentElement && item.parentElement != this) item.deparent();
                    // note: item may not have yet been created
                    if (isc.isA.Canvas(item) && item.isDrawn()) item.clear();
                }
            }

            // apply resizeability flag to items
            if (section.items) {
                if (!this.canResizeSections) section.items.setProperty("resizeable", false);
                else if (section.resizeable != null) {
                    // allow both an explicit true and explicit false value.
                    // - false allows fixed-sized sections
                    // - true forces inherent height members to be resizeable
                    section.items.setProperty("resizeable", section.resizeable);
                }
            }
        }
        
        // if we were asked to make sure one section gets shown, show the first section if none
        // were marked expanded:true
        if (expandOne && this._lastExpandedSection == null) {
            var firstSectionConfig = sections.first();
            // NOTE: avoid forcing open the first section if it's config marked it explicitly
            // not expanded
            if (firstSectionConfig && !(firstSectionConfig.expanded == false)) {
                var firstSection = this.sections.first();
                this.expandSection(firstSection);
            }
        }
    },
    

    //> @method sectionStack.addSection()
    //
    // Add a section to the SectionStack.
    //
    // @param sections  (SectionStackSection Properties | List of SectionStackSection Properties) Initialization block
    //                  for the section or a list of initialization blocks to add.
    // @param [position]    (number) index for the new section(s) (if not specified, the section
    //                      will be added at the end of the SectionStack).
    //
    // @visibility external
    // @example sectionsAddAndRemove
    //<
    addSection : function (sections, position) {
        this.addSections(sections, position);
    },
    //> @method sectionStack.removeSection()
    //
    // Remove a section or set of sections from the SectionStack.  The removed sections' header
    // and items (if any) are automatically destroyed.
    //
    // @param sections  (int | String | Array of int | Array of String)  Section(s) to remove.
    //                  For this parameter, you can pass the position of the section in the
    //                  SectionStack, the <code>name</code> of the section, or a List of 
    //                  section <code>name</code>s or indices.
    //
    // @visibility external
    // @example sectionsAddAndRemove
    //<
    removeSection : function (sections) {
        if (!isc.isAn.Array(sections)) sections = [sections];
        for (var i = 0; i < sections.length; i++) {
            var section = this.getSectionHeader(sections[i]);
            if (section != null) {
                // Remove the section from our sections array first so that the section's items
                // is not cleared.
                this.sections.remove(section);
                if (section._tabPanel != null) {
                    section._tabPanel.destroy();
                    section._tabPanel = null;
                }
                for (var ii = section.items.length-1; ii >= 0; ii--) {
                    var item = section.items[ii];

                    
                    if (this.members.contains(item)) this.removeMember(item);
                }
                if (!section.destroying && !section.destroyed) section.destroy();
            }
        }
    },

    //> @method sectionStack.getSections()
    //
    // Returns a list of all +link{SectionStackSection.name,section names} in the order in which 
    // they appear in the SectionStack.
    //
    // @return (List) list of all section names in the order in which they appear in the SectionStack.
    // @visibility external
    //<
    getSections : function () {
        return this.sections.getProperty("name");
    },

    //> @method sectionStack.reorderSection()
    //
    // Reorder the sections by shifting the specified section to a new position
    //
    // @param section  (Integer or String) Section to move.  You can pass the position 
    //                      of the section in the SectionStack or the name of the section.    
    // @param position   (number) new position index for the section.
    //
    // @deprecated As of SmartClient version 5.5, use +link{sectionStack.moveSection}.
    //
    // @visibility external
    //<
    reorderSection : function (section, newPosition) {
        this.moveSection(section, newPosition);
    },

    //> @method sectionStack.moveSection()
    //
    // Moves the specified section(s) to a new position in the SectionStack order.  If you pass
    // in multiple sections, then each section will be moved to <code>newPosition</code> in the
    // order specified by the <code>sections</code> argument.
    //
    // @param sections  (int | String | Array of int | Array of String) Section(s) to move.
    //                  For this parameter, you can pass the position of the section in the
    //                  SectionStack, the name of the section, or a List of section names/positions.
    //
    // @param position  (int) new position index for the section(s).
    //
    // @visibility external
    //<
    moveSection : function (sections, newPosition) {
        if (newPosition == null) return;

        

        if (!isc.isAn.Array(sections)) sections = [sections];

        // normalize initial positions to sections - indices will become 
        // invalid as we go through the loop manipulating
        // this.sections.
        for (var i = 0; i < sections.length; i++) {
            var section = this.getSectionHeader(sections[i]);
            if (section == null) {
                this.logInfo("moveSection(): Unable to find header for specified section:" + sections[i] + ", skipping");
                i--;
                sections.removeAt(i);
            } else {
                sections[i] = section;
                // and pull it out from this.sections
                this.sections[this.sections.indexOf(section)] = null;
            }
        }
        this.sections.removeEmpty();
        this.sections.addListAt(sections, newPosition);

        // Now update the members array.
        var currentMemberIndex = 0;
        for (var i = 0; i < this.sections.length; i++) {
            var header = this.getSectionHeader(i),
                currentStart = this.members.indexOf(header),
                currentEnd = currentStart + 1;

            var items = header.items;
            if (items != null && items.length != 0 && this.members.contains(items[0])) {
                if (currentStart == -1) {
                    currentStart = this.members.indexOf(items[0]);
                    currentEnd = currentStart;
                }
                currentEnd += items.length;
            }

            if (currentStart == -1) continue;
            this.members.slideRange(currentStart,currentEnd, currentMemberIndex);
            // next slot will be after this header and any items.
            currentMemberIndex += (currentEnd-currentStart);

        }
        this._membersReordered("moveSection() called");
    },

    //> @method Callbacks.ShowSectionCallback
    // Callback to execute after the section has been shown.
    // @visibility external
    //<
    //> @method sectionStack.showSection()
    // 
    // Shows a section or sections.  This includes the section header and its items.  If the
    // section is collapsed, only the header is shown.  If the section is expanded, the section
    // header and all items are shown.
    //
    // @param sections   (int | String | Array of int | Array of String) 
    //                      Section(s) to show.  For this parameter, you can pass the position 
    //                      of the section in the SectionStack, the name of the section, or a
    //                      List of section names / positions.
    // @param [callback] (ShowSectionCallback) callback to fire when the sections have been shown.
    //
    // @see sectionStack.expandSection
    // @see sectionStack.scrollSectionIntoView
    // @visibility external
    // @example sectionsShowAndHide
    //<
    showSection : function (sections, callback) {
        this._showSection(sections, true, false, callback);
    },

    //> @method Callbacks.ExpandSectionCallback
    // Callback to execute after the section has been expanded.
    // @visibility external
    //<
    //> @method sectionStack.expandSection()
    // 
    // Expands a section or sections.  This action shows all the items assigned to the section.
    // If the section is currently hidden, it is shown first and then expanded.  Calling this
    // method is equivalent to the user clicking on the SectionHeader of a collapsed section.
    // <smartclient>This method is called when the user clicks on SectionHeaders
    // to expand / collapse sections and so may be overridden to act as a notification method
    // for the user expanding or collapsing sections.</smartclient>
    //
    // @param sections   (int | String | Array of int | Array of String)
    //                      Section(s) to expand.  For this parameter, you can pass the position 
    //                      of the section in the SectionStack, the name of the section, or a
    //                      List of section names/positions.
    // @param [callback] (ExpandSectionCallback) callback to fire when the section has been expanded. 
    //
    // @see sectionStack.showSection
    // @see sectionStack.scrollSectionIntoView
    // @visibility external
    // @example sectionsExpandCollapse
    //<
    expandSection : function (sections, callback) {
        if (!isc.isAn.Array(sections)) sections = [sections];
        if (this.visibilityMode == "mutex") {
            // catch case where multiple sections are requested for expansion in 'mutex' mode
            if (sections.length > 1) {
                this.logWarn("expandSection(): only one section can be expanded in " +
                             "'mutex' visibility mode. Dropping all but the last.");
                sections = [sections[sections.length - 1]];
            }
            // keep only one section visible: hide the currently visible section
            var lastExpanded = this._lastExpandedSection,
                section = this.getSectionHeader(sections[0]);
            if (lastExpanded && lastExpanded != section) this.collapseSection(lastExpanded);
        }
        this._showSection(sections, false, true, callback);  
    },

    _showSection : function (sections, showSection, expandSection, callback) {
        if (sections == null) return;
        if (!isc.isAn.Array(sections)) sections = [sections];

        var ariaEnabled = isc.Canvas.ariaEnabled();

        var itemsToShow = [];
        for (var i = 0; i < sections.length; i++) {
            var section = this.getSectionHeader(sections[i]);        
            // bad section specification
            if (section == null) {
                this.logWarn("showSection(): no such section [" + i + "]: " +
                              this.echo(sections[i]));
                continue;
            }

            // If section.showHeader is true and the section isn't visible, show it
            // (whether we're expanding or showing)
            if (section.showHeader && section.hidden && (showSection || expandSection)) {
                itemsToShow.add(section);
                section.hidden = false;
                if (ariaEnabled) section._tabPanel.setAriaState("hidden", section.hidden || !section.expanded);
            }

            if (expandSection || section.expanded) {
                // Backcompat: setOpen is deprecated, but we still want to call it if
                // there's a backcompat definition. Otoh it's possible that we just have
                // setExpanded, so try that first and then call setOpen
                if (section.setExpanded && !section.setOpen) section.setExpanded(true);
                else if (section.setOpen) section.setOpen(true);

                // store the last expanded section    
                this._lastExpandedSection = section;

                // NOTE: a section with no items doesn't make much sense, but it occurs in tools
                if (section.items != null && section.items.length > 0) {
                    // normalize items specified as strings / uninstantiated objects etc
                    // to canvii
                    for (var ii = section.items.length-1; ii >= 0; ii--) {
                        
                        var itemCanvas = this.createCanvas(section.items[ii]);
                        if (!isc.isA.Canvas(itemCanvas)) {
                            this.logWarn("Section with title:"+ section.title + 
                                " contains invalid item:" + section.items[ii] +
                                " - ignoring this item.");
                            section.items.removeAt(ii);
                            continue;
                        }
                        section.items[ii] = itemCanvas;
                    }

                    // ensure the section's members are added, after the section header
                    var sectionPosition = this._getSectionPosition(section) + 1;
                    // NOTE: don't animate on add because we do the animation on showMembers
                    // instead
                    this.addMembers(section.items, sectionPosition, true);
                    itemsToShow.addList(section.items);

                    if (ariaEnabled) {
                        var itemIDs = section.items.map("_getAriaHandleID");
                        section._tabPanel.setAriaState("owns", itemIDs.join(" "));
                    }
                }
            }
        }

        var theStack = this;
        this.showMembers(itemsToShow, 
                         function () { theStack._completeShowOrExpandSection(sections, callback); }
                         );
    },

    // fired as a callback to showMembers() from showSection() and expandSection()
    _completeShowOrExpandSection : function (sections, callback) {
        // sections is always an array here because this is an internal method and sections is
        // normalized by the caller
        if (sections.length == 0) return;

        // this method jsut scrolls things into view, but if we haven't been drawn yet, then
        // there's no need to do anything.
        if (this.isDrawn()) {
    
            // scroll the first passed section into view
            var section = this.getSectionHeader(sections[0]);
    
            // bring the section that was just shown into the viewport
            if (this.vscrollOn && this.scrollSectionIntoView) {
                var firstMember = (section.showHeader ? section : section.items.first()),
                    lastMember = section.items.last();
        
                // NOTE: visible height wouldn't be correct until component is drawn
                this.delayCall("scrollIntoView", 
                                [firstMember.getLeft(), firstMember.getTop(),
                                 firstMember.getVisibleWidth(), lastMember.getVisibleHeight(),
                                 "left", "top"], 0);
            }
        }
        
        if (callback != null) this.fireCallback(callback);
    },

    //> @method sectionStack.sectionForItem()
    // 
    // Search for a section that contains passed item.
    //
    // @param item (Canvas) item to show
    // @return (SectionStackSection) section that contains passed item.
    //
    // @see sectionStack.expandSection
    // @visibility external
    //<
    sectionForItem : function (item) {
        if (this.sections) {
            for (var i = 0; i < this.sections.length; i++) {
                var section = this.sections[i];
                if (section) {
                    for (var j = 0; j < section.items.length; j++) {
                        if (section.items[j] == item) {
                            return section;
                        }
                    }
                }
            }     
        }
    },

    sectionForMemberIndex : function (index) {
        var sectionIndex = -1;
        if (this.sections) {
            for (var i = this.sections.length-1; i>=0; i--) {
                var section = this.sections[i];
                sectionIndex = this.getMemberNumber(section);
                if (sectionIndex < index) return section;
            }       
        }
    },

    //> @method Callbacks.HideSectionCallback
    // Callback to execute after the section has been hidden.
    // @visibility external
    //<
    //> @method sectionStack.hideSection()
    // 
    // Hides a section or sections.  This includes the section header and its items.  The space
    // vacated by this action is reassigned to the nearest visible section item above this
    // section.  If there are no visible section items above this section, the space is
    // reassigned to the nearest visible section item below this section.
    //
    // @param sections (int | String | Array of int | Array of String) 
    //                      Section(s) to hide.  For this parameter, you can pass the position 
    //                      of the section in the SectionStack, the name of the section, or a
    //                      List of section names / positions.
    // @param [callback] (HideSectionCallback) to fire when the section has been hidden    
    //
    // @see sectionStack.collapseSection
    // @visibility external
    // @example sectionsShowAndHide
    //<
    hideSection : function (sections, callback) {
        this._hideSection(sections, true, false, callback);
    },

    //> @method Callbacks.CollapseSectionCallback
    // Callback to execute after the section has been collapsed.
    // @visibility external
    //<
    //> @method sectionStack.collapseSection()
    // 
    // Collapse a section or sections.  This action hides all the items assigned to the
    // section.  Calling this method is equivalent to the user clicking on the SectionHeader of
    // an expanded section.
    // <smartclient>This method is called when the user clicks on SectionHeaders
    // to expand / collapse sections and so may be overridden to act as a notification method
    // for the user expanding or collapsing sections.</smartclient>
    //
    // @param sections   (int | String | Array of int | Array of String) 
    //                      Section(s) to collapse.  For this parameter, you can pass the position 
    //                      of the section in the SectionStack, the name of the section, or a
    //                      List of section positions / names
    //
    // @param [callback] (CollapseSectionCallback) callback to fire when the section has been collapsed    
    // @see sectionStack.hideSection
    // @visibility external
    // @example sectionsExpandCollapse
    //<
    collapseSection : function (sections, callback) {
        this._hideSection(sections, false, true, callback);
    },
    
    _hideSection : function (sections, hideSection, collapseSection, callback) {
        if (sections == null) return;
        if (!isc.isAn.Array(sections)) sections = [sections];

        var ariaEnabled = isc.Canvas.ariaEnabled();

        var itemsToHide = [];
        for (var i = 0; i < sections.length; i++) {
            var section = this.getSectionHeader(sections[i]);

            // bad section specification
            if (section == null) {
                this.logWarn("hideSection(): no such section [" + i + "]: " +
                            this.echo(sections[i]));                
                continue;
            }

            if (hideSection && !section.hidden) {
                section.hidden = true;
                if (ariaEnabled) section._tabPanel.setAriaState("hidden", section.hidden || !section.expanded);
                itemsToHide.add(section);
            }

            if (collapseSection || section.expanded) {
                // Backcompat: setOpen is deprecated, but we still want to call it if there's a
                // backcompat definition. Otoh it's possible that we just have setExpanded, so try
                // that first and then call setOpen
                if (collapseSection) {
                    if (section.setExpanded && !section.setOpen) section.setExpanded(false);
                    else if (section.setOpen) section.setOpen(false);
                }

                // clear the last expanded section if appropriate
                if (this._lastExpandedSection == section) this._lastExpandedSection = null;

                // some items may not have yet been added as members, so don't try to hide()
                // those or we'll crash in Layout
                if (section.items) {
                    for (var j = 0; j < section.items.length; j++) {
                        if (this.members.contains(section.items[j])) itemsToHide.add(section.items[j]);
                    }
                }
            }
        }
        
        // forceFill: override recent user resizes to fill available space.  NOTE: don't
        // forceFill if we're overflowed, as this would shrink us further after a collapse,
        // which is unexpected (this feature should be moved up to Layout as an optional
        // reaction to a hide)
        if (this.forceFill && this.getVisibleHeight() <= this.getHeight()) {
            // we want to make sure that some section(s) expand to fill the space vacated by this
            // hide/collapse.  We scan through the members array to see if one of the items
            // will be resized by the layout automatically.  If not, we pick one to forcibly
            // resize to fill the vacated space.

            // We're going to scan back from the first sectionHeader and then forward to try to
            // find an auto-resizable member and at the same time, we'll flag one that we can
            // forcefully resize if no auto-resizeable members are found.
            var startIndex = this.getMemberNumber(this.getSectionHeader(sections[0]));
            
            var forceResizeTarget;
            var layoutWillReflow = false;
            // scan back
            for (var i = startIndex-1; i >= 0; i--) {
                var member = this.members[i];
                if (itemsToHide.contains(member)) continue;
                if (this.memberIsDragResizeable(member)) {
                    if (this.memberHasAutoResizeableHeight(member)) {
                        layoutWillReflow = true;
                        break;
                    } else if (forceResizeTarget == null) {
                        forceResizeTarget = member;
                    }
                }
            }

            if (!layoutWillReflow) {
                // scan forward            
                for (var i = startIndex+1; i < this.members.length; i++) {
                    var member = this.members[i];
                    if (itemsToHide.contains(member)) continue;
                    if (this.memberIsDragResizeable(member)) {
                        if (this.memberHasAutoResizeableHeight(member)) {
                            layoutWillReflow = true;
                            break;
                        } else if (forceResizeTarget == null) {
                            forceResizeTarget = member;
                        }
                    }
                }
            }

            if (!layoutWillReflow && forceResizeTarget != null) {
//                this.logWarn("layout will not reflow, forceResizing: " + forceResizeTarget.ID);
                forceResizeTarget._userHeight = null;
//            } else {
//                if (layoutWillReflow) this.logWarn("layout will reflow");
//                else this.logWarn("layout will not reflow and no forceResizeTarget");
            }
        }
        
        this.hideMembers(itemsToHide, callback);
    },

    //> @method sectionStack.sectionIsVisible()
    //
    // Returns true if the specified section is visible, false if it is not.  A section is
    // visible if it shows a header and the header is visible or if it has items and the first
    // item is visible.
    //
    // @param section (int | String)
    //                      Section for which you want to obtain visibility information.
    //                      For this parameter, you can pass the position of the section in the
    //                      SectionStack, or the name of the section.
    //
    // @return (boolean)      true if the section is visible, false if it is not.
    //
    // @visibility external
    //<
    sectionIsVisible : function (section) {
    
        section = this.getSectionHeader(section);
        if (!section) return false;

        if (section.showHeader && section.isVisible()) return true;

        // NOTE: have to consider lazy initialization case
        var sectionMember = section.items.first();
        if (sectionMember == null || !isc.isA.Canvas(sectionMember) || 
            !sectionMember.isDrawn() || 
            sectionMember.visibility == isc.Canvas.HIDDEN) return false;
        return true;
    },

    //> @method sectionStack.getVisibleSections()
    //
    // Returns the list of currently visible sections.  The list items are section names.
    //
    // @return (List)      list of visible sections
    //
    // @visibility external
    //<
    getVisibleSections : function() {
        var visibleSections = [];
        for (var i = 0; i < this.sections.length; i++)
            if (this.sectionIsVisible(this.sections[i])) visibleSections.add(this.sections[i].name);
        return visibleSections;
    },

    //> @method sectionStack.sectionIsExpanded()
    //
    // Returns true if the specified section is expanded, false if it is collapsed.
    //
    // @param section (int | String)
    //                      Section for which you want to obtain information.
    //                      For this parameter, you can pass the position of the section in the
    //                      SectionStack, or the name of the section.
    //
    // @return (boolean)      true if the section is expanded, false if it is not.
    //
    // @visibility external
    //<
    sectionIsExpanded : function (section) {
        return this.getSectionHeader(section).expanded;
    },

    //> @method sectionStack.getExpandedSections()
    //
    // Returns the list of currently expanded sections.  The list items are section IDs.
    //
    // @return (List)      list of currently expanded sections
    //
    // @visibility external
    //<
    getExpandedSections : function () {
        var expandedSections = this.sections.findAll("expanded", true);
        return expandedSections == null ? [] : expandedSections.getProperty("name");
    },

    //> @method sectionStack.setSectionTitle()
    // Changes the title of a SectionHeader.
    //
    // @param section (String or Number) ID or index of the section whose title you want to change
    // @param newTitle (String) new title for the SectionHeader
    // @visibility external
    //<
    setSectionTitle : function (section, newTitle) {
        var sectionHeader = this.getSectionHeader(section);
        if (sectionHeader) sectionHeader.setTitle(newTitle);
    },

    //> @method sectionStack.getSectionHeader()
    // Return the SectionHeader for a section.
    // <P>
    // This will be an instance of the +link{sectionHeaderClass}.  Since different
    // SectionStacks may use different header classes, be careful about what APIs you rely on
    // for the section header unless you have explicitly set the
    // <code>sectionHeaderClass</code>.  In particular, use APIs such as
    // +link{setSectionTitle()} to manipulate headers indirectly wherever possible, as high
    // performance SectionStacks designed for very large numbers of sections may cache and
    // re-use headers or use other strategies that would make it invalid to store a pointer to
    // a section header, assume the header is a layout member, etc.
    //
    // @param section (String or Number) ID or index of the section for which you want the header
    // @return (SectionHeader) the section header indicated
    // @visibility external
    //<
    getSectionHeader : function (section) {
        return isc.Class.getArrayItem(section, this.sections, "name");
    },
     
    getSection : function (section) { return this.getSectionHeader(section) },
    
    // Retrieves the section config object passed in when a section stack section was first
    // created.
    
    getSectionConfig : function (section) {
        var sectionHeader = this.getSectionHeader(section);
        if (!isc.isA.Canvas(sectionHeader)) return sectionHeader;
        return sectionHeader._sectionConfig;
    },

    //> @method sectionStack.getSectionNumber()
    //
    // Returns the position of the specified section in the SectionStack.  The numbering is
    // zero-based.  
    //
    // @param sectionName     (string) name of a section for which you want to obtain the position.
    //
    // @return (number)     Position of the section in the SectionStack or -1 if the specified
    //                      section is not a member of this SectionStack.
    //
    // @visibility external
    //<    
    getSectionNumber : function (section) {
        if (isc.isA.String(section)) {
            return this.sections.findIndex("name", section);
        // handle being passed a pointer to a section directly
        } else {
            return this.sections.indexOf(section);
        }
    },

    // returns the position in the members array where the first item (header or first item in the
    // section.items array if showHeader = false) in this section should be
    // 
    // for external interfaces we only care about the index of the section in this.sections,
    // because that's what all external methods take as a section identifier (among others) and
    // end users shouldn't be directly modifying the underlying Layout.
    _getSectionPosition : function (section) {
        // if the section header has already been added as a member, it's position is
        // straightforward.
        var headerPosition = this.getMemberNumber(section);
        if (headerPosition != -1) return headerPosition;

        // otherwise look for the position of the last item in the previous section
        var sectionIndex = this.sections.indexOf(section);

        // if we're the first section we start at zero
        if (sectionIndex <= 0) return sectionIndex;

        // otherwise we start after the last item of the preceding section
        var previousSection = this.sections[sectionIndex-1],
            lastMember = previousSection.items ? previousSection.items.last() : null;
        if (this.hasMember(lastMember)) {
            return this.getMemberNumber(lastMember) + 1;
        } else {
            // NOTE: sections without headers always have their items added immediately, since
            // there's no way to hide them
            return this.getMemberNumber(previousSection) + 1;
        }
    },

    //> @method SectionStack.sectionHeaderClick (A)
    // Method intended to be called by the sectionHeader when it is clicked on.
    //
    // @param sectionHeader (Canvas) the sectionHeader clicked on
    // @visibility external
    //<
    sectionHeaderClick : function (section) {
        // If onSectionHeaderClick exists, allow it to cancel default behavior
        
        if (this.onSectionHeaderClick && (this.onSectionHeaderClick(section) == false)) {
            return false;
        }
        
        // hide the currently visible pane and show the pane for the header that got clicked on
        if (this.visibilityMode == "mutex") {
            // if this section is expanded, collapse it and expand the following section
            if (this.sectionIsExpanded(section)) {
                // collapse this section
                this.collapseSection(section);
                // open next section if canCollapseAll is false
                if (!this.canCollapseAll) {
                    var sectionIndex = this.sections.indexOf(section);
                    // if last section, next section will revert back to first
                    if (sectionIndex == this.sections.getLength() - 1) sectionIndex = 0;
                    else sectionIndex += 1;
                    var nextSection = this.sections[sectionIndex];
                    // expand next section
                    this.expandSection(nextSection);
                }
                return false;
            }
            // show the new section
            this.expandSection(section);
        } else {
            // just toggle expanded/collapsed
            if (!this.sectionIsExpanded(section)) {
                this.expandSection(section);
            } else {
                this.collapseSection(section);
            }
        }
        return false; // cancel event bubbling
    },
    
    getSectionCursor : function (sectionHeader) {
        var cursor;
        var config = this.getSectionConfig(sectionHeader);
        if (config == null) cursor = isc.Canvas.DEFAULT;
        else if (config.cursor) cursor = config.cursor;
        else {
            if (config.canCollapse != false) {
                cursor = isc.Canvas.HAND;
            
            } else if (this.canRorderSections && config.canReorder != false) {
                cursor = "move";
            } else {
                cursor = isc.Canvas.DEFAULT;
            }
        }
        return cursor;

    },

    // For a given member, return the closest resizeable member _before_ us in the members
    // array.  See memberIsResizeable() for what constitutes a resizeable member.
    getDragResizeTarget : function (member) {
        var myIndex = this.getMemberNumber(member);

        // look for a member preceding us that can be resized
        var resizeTarget;
        this._resizeIgnore = 0;
        for (var i = myIndex-1; i >= 0; i--) {
            var member = this.getMember(i);
            if (this.memberIsDragResizeable(member)) {
                resizeTarget = member;
                break;
            }
            // as we pass non-resizeable members, store up their total height, which we will
            // use as offset when using the coordinate of the dragged section header to resize
            // whatever member actually gets chosen as the resize target.
            // NOTE: if we pass a section header, don't resize if the preceding member is
            // another section header, detected via the isSectionHeader flag rather than
            // isc.isA.SectionHeader since section header implementation is pluggable
            if ((member.isSectionHeader && this.sectionIsVisible(member)) || (!member.resizeable && member.isVisible()))
                this._resizeIgnore += member.getVisibleHeight();
        }

        // if there are no preceeding resizeable members, never allow resize (eg, no 
        // resize is possible if you are grabbing the first section header, or a section header
        // after a series of collapsed or fixed-size sections)
        if (!resizeTarget) return null;

        
        // Read as: 
        // - if canResizeStack is true (default), always allow resize if there is a preceeding,
        //   resizeable member, even though this *may* change the overall stack size if there
        //   isn't also a resizeable member after this section header
        // - if canResizeStack is false, only allow a resize if there is *also* a member
        //   after us that can resize, because only then will all available space still be
        //   filled.
        if (this.canResizeStack) return resizeTarget;

        // look for a member after us that can resize
        var numMembers = this.getMembers().length;
        for (var i = myIndex+1; i < numMembers; i++) {
            var member = this.getMember(i);
            // some member after the sectionHeader is resizeable, so go ahead and return the
            // resizeTarget we previously determined
            if (this.memberIsDragResizeable(member)) return resizeTarget;
        }
        
        return null;
    },
    
    
    memberIsDragResizeable : function (member) {
        if (!member.isSectionHeader 
            && member.resizeable !== false 
            && member.isVisible()
            && (!this.memberHasInherentLength(member) || member.resizeable)
            ) return true;
    },

    memberHasAutoResizeableHeight : function (member) {
        var uh = member._userHeight;
        return uh == null || (isc.isA.String(uh) && (uh == "*" || isc.endsWith(uh, "%")));
    },

    getMemberDefaultBreadth : function (member, defaultBreadth) {
        var breadth = defaultBreadth;
        if (!member.isSectionHeader) {
            if (this.itemStartIndent != null || this.itemEndIndent != null)
                breadth -= (this.itemStartIndent==null?0:this.itemStartIndent) + 
			   (this.itemEndIndent==null?0:this.itemEndIndent);
            else breadth -= this.itemIndent;
        }
        return breadth;
    },
    
    getMemberOffset : function (member, defaultOffset, alignment) {
        var offset = this.itemIndent;

        if (member.isSectionHeader) return defaultOffset;
        if (this.itemStartIndent != null) offset = this.itemStartIndent;
        if (alignment == isc.Canvas.RIGHT || alignment == isc.Canvas.BOTTOM)
            offset *= -1;

        return defaultOffset + offset;
    }
    
});

// SectionHeader classes
// ---------------------------------------------------------------------------------------

isc._commonMediaProps = {
    icon:"[SKIN]SectionHeader/opener.gif",
    overflow:"hidden",
    baseStyle:"sectionHeader",
 
    // Show the disabled style in both image based headers and label-based headers
    showDisabled:true,

    // expanded/collapsed styling
    // ---------------------------------------------------------------------------------------
    expanded: false,
    //>!BackCompat 2005.12.22
    setOpen : function (isOpen) { 
        this.setExpanded(isOpen);
    },
    //<!BackCompat
    getCustomState : function () { return this.expanded ? "opened" : "closed"; }
};

isc._commonHeaderProps = {
    overflow:"hidden",
    
    //> @attr sectionHeader.clipTitle (Boolean : true : IR)
    // If the title for this section header is too large for the available space, should the title be
    // clipped?
    // <p>
    // This feature is supported only in browsers that support the CSS UI text-overflow
    // property (IE6+, Firefox 7+, Safari, Chrome, Opera 9+).
    // @visibility external
    //<
    clipTitle:true,
    //> @attr sectionHeader.showClippedTitleOnHover (Boolean : true : IRW)
    // If true and the title is clipped, then a hover containing the full title of this section header
    // is enabled.
    // @group hovers
    // @visibility external
    //<
    showClippedTitleOnHover:true,
    wrap:false, // actually only needed for the Label-based "SectionHeader" class
    height:20,
    expanded: false,
    canCollapse: true,

    //>@method SectionHeader.getSectionStack()
    // For a SectionHeader embedded in a SectionStack, this method will return 
    // a pointer to the +link{SectionStack} in which this section header is
    // embedded.
    // @return (SectionStack) Section Stack containing this section header
    // @visibility external
    //<
    //>@method ImgSectionHeader.getSectionStack()
    // @include SectionHeader.getSectionStack()
    // @visibility external
    //<
    getSectionStack : function () {
        // we store the attribute as "layout" when addSection runs
        var layout = this.layout;
        if (layout) return isc.isA.String(layout) ? window[layout] : layout;
        else return null;
    },
    
    // Collapse behavior
    // ---------------------------------------------------------------------------------------

    // Snap open/closed on  "space" / "enter" keypress
    // Allow resizing via ctrl+arrow keys
    keyPress : function () {
        var layout = this.getSectionStack();
        if (layout == null) return;
        
        var keyName = isc.EH.getKey();
        if (keyName == "Enter" || keyName == "Space") {
            if (this.canCollapse) return layout.sectionHeaderClick(this);
        } else if (keyName == "Arrow_Up" || keyName == "Arrow_Down") {
            var target = layout.getDragResizeTarget(this);
            // NOTE: don't resize if the preceding member is another section header, detected
            // via marker rather than class since section header is pluggable
            if (target == null) return false;
            var resizeStep = (keyName == "Arrow_Up" ? -5 : 5);
            this.bringToFront(); // so we aren't occluded by what we will resize
            this.resizeTarget(target, true, this.resizeInRealTime, 0, 0,
                                (this.getPageTop() + resizeStep))
            // set a flag so we know to kill the when the user releases the ctrl key
            this._keyResizeTarget = target;
        }
	},
    
    keyUp : function () {
        if (this._keyResizeTarget) {
            var keyName = isc.EH.getKey();
            if (keyName == "Arrow_Up" || keyName == "Arrow_Down") {
                this.finishTargetResize(this._keyResizeTarget, true, this.resizeInRealTime);
                this._keyResizeTarget = null;
            }
        }
    },

    _canFocus : function () {
        // Support setting section.canTabToHeader explicitly.
        if (this.canTabToHeader != null) return this.canTabToHeader;
        
        var layout = this.getSectionStack();

        // layout will be either a SectionStack or a SectionItem - support canTabToHeader and its plural
        if (layout) {
            if (layout.canTabToHeaders != null) return layout.canTabToHeaders;
            if (layout.canTabToHeader != null) return layout.canTabToHeader;
            if (isc.SectionItem && isc.isA.SectionItem(layout)) {
                var form = layout.form;
                if (form && form.canTabToSectionHeaders != null) return form.canTabToSectionHeaders;
            }
            // If canTabToHeader isn't explicitly set, allow tab to header if isc.screenReader is
            // set.
            return !!isc.screenReader;
        }
        else return true;
    },
	
    _hasLayout : function () {
        var layout = this.getSectionStack();
        return layout ? true : false;
    },
    // Editing
    // ---------------------------------------------------------------------------------------
    //>EditMode
    schemaName : "SectionStackSection", // schema to use when editing and serializing
    addItem : function (item, index) { 
        if (!this._hasLayout()) return;

        var layout = this.getSectionStack();
        layout.addItem(this, item, index);
        // Visual Builder expects addItem to also expand this section
        layout.expandSection(this);
    },
    removeItem : function (item) {
        if (!this._hasLayout()) return;
        this.getSectionStack().removeItem(this, item);
    },
    //<EditMode

    // Resize interaction
    // ---------------------------------------------------------------------------------------
    canDrag:true,
    dragAppearance:"none",

    isSectionHeader:true,
    dragStart : function () {
        if (!this._hasLayout()) return;
        var target = this.getSectionStack().getDragResizeTarget(this);
        this._sectionDragTarget = target;
        if (target == null) return false;
        this.bringToFront(); // so we aren't occluded by what we will drag resize
    },
	dragMove : function () {
        if (!this._hasLayout()) return;
        // resizeIgnore is calculated in getDragResizeTarget(), called from dragStart();
        var resizeIgnore = this.getSectionStack()._resizeIgnore;
        var offset = 0 - isc.EH.dragOffsetY;
        this.resizeTarget(this._sectionDragTarget, true, this.resizeInRealTime, offset, resizeIgnore);
	},
	dragStop : function () {
        this.finishTargetResize(this._sectionDragTarget, true, this.resizeInRealTime);
	},
    
    // When a section gets destroyed, ensure all items (including those that have never been
    // added as a member to the layout) also get cleared out.
    destroy : function () {
        if (!this.expanded && this.items) {
            var items = this.items;
            for (var i = 0; i< items.length; i++) {
                if (isc.isA.Canvas(items[i]) && items[i].parentElement != this.parentElement) {
                    items[i].destroy();
                }
            }
        }
        // Destroy any specified controls for the section unless they are
        // already present in the hierarchy under the parent [happens on draw()]
        var controls = this.controls,
            cLayout = this.controlsLayout;
        if (controls) {
            if (!isc.isAn.Array(controls)) controls = [controls];
            for (var ii = 0; ii < controls.length; ii++) {
                if (controls[ii].destroy && !controls[ii].destroyed &&
                    (cLayout == null || controls[ii].parentElement != cLayout)) 
                {
                    controls[ii].destroy();
                }
            }
        }
        return this.Super("destroy", arguments);
    },

    // Custom Controls
    // ---------------------------------------------------------------------------------------

    //> @attr sectionHeader.controls (Array of Canvas : null : IR)
    // Custom controls to be shown on top of this section header.
    // <P>
    // These controls are shown in the +link{controlsLayout}.
    // <P>
    // Note that this is an init-time property. If you need to dynamically change what 
    // controls are displayed to the user, we would recommend embedding the
    // controls in a Layout or similar container. 
    // This will allow you to show/hide or add/remove members at runtime
    // by manipulating the existing control(s) set up at init time.
    // @example sectionControls
    // @visibility external
    //<

    //> @attr imgSectionHeader.controls (Array of Canvas : null : IR)
    // @include sectionHeader.controls
    //<

    //> @attr sectionHeader.controlsLayout (AutoChild Layout : null : IR)
    // A +link{Layout} containing specified +link{controls} if any.  Sets
    // +link{layout.membersMargin}:5, +link{layout.defaultLayoutAlign}:"center", and
    // RTL-sensitive +link{layout.align} (right by default).
    // @visibility external
    //<

    //> @attr imgSectionHeader.controlsLayout (AutoChild Layout : null : IR)
    // @include sectionHeader.controlsLayout
    //<
    controlsLayoutDefaults : {
        _constructor:isc.HStack,
        defaultLayoutAlign:"center",
        membersMargin:5,
        layoutEndMargin:5,
        addAsChild:true
    },

    _getAfterPadding : function () {
        return (this.controlsLayout == null ? null : this.controlsLayout.getVisibleWidth());
    },

    addControls : function () {
        if (!this.controls) return;

        var isRTL = this.isRTL();
        this.addAutoChild("controlsLayout", {
            height:this.getInnerHeight(),
            align:isRTL ? "left" : "right",
            snapTo:isRTL ? "L" : "R",
            members:this.controls,
            resized : function () {
                var label = this.creator,
                    background = this.creator.background;
                if (background != null) label = background.label;
                label.markForRedraw();
            }
        });
        this.allowContentAndChildren = true;
    },

    refreshControls : function () {
        if (!this.controls) return;
        if (!this.controlsLayout) this.addControls();

        var layout = this.controlsLayout;
        layout.addMembers(this.controls);

        this.allowContentAndChildren = true;
    },

    // Printing
    // ------------------------------------------------------------------------------------------
    // When printing, pick up the printStyleName from our sectionStack if it's set
    // Note that SectionHeaders are used in sectionItems as well. In this case the parentElement
    // will be the DynamicForm which may not have printHeaderStyleName set
    getPrintStyleName : function () {        
        var sectionStack = this.parentElement;
        if (sectionStack && sectionStack.printHeaderStyleName != null) {
            this.printStyleName = sectionStack.printHeaderStyleName;
        }
        return this.Super("getPrintStyleName", arguments);
    },
    
    // force section headers to print even though they're controls
    shouldPrint:true
    
    
};

//> @class SectionHeader
// Simple SectionHeader class based on a Label with an icon, skinnable via CSS.
//
// @treeLocation Client Reference/Layout/SectionStack
// @visibility external
//<
isc.defineClass("SectionHeader", "Label").addMethods(isc._commonHeaderProps, 
                                                     isc._commonMediaProps, 
{
    setExpanded : function (expanded) {
        this.expanded = expanded;
        if (isc.Canvas.ariaEnabled()) {
            if (this._tabPanel != null) this._tabPanel.setAriaState("hidden", this.hidden || !expanded);
            var sectionStack = this.layout;
            if (isc.isA.SectionStack(sectionStack)) {
                var multiselectable = (sectionStack.visibilityMode != "mutex");
                if (multiselectable) {
                    this.setAriaState("expanded", !!expanded);
                } else {
                    this.setAriaState("selected", !!expanded);
                }
            }
        }
        this.stateChanged();
    },

    // We use this.title, not this.contents for the section header title
    useContents:false,
    
    //> @attr sectionHeader.icon   (SCImgURL : "[SKIN]SectionHeader/opener.gif" : [IRA])
    // Base filename of the icon that represents open and closed states. The default settings
    // also change the icon for disabled sections, so a total of four images are required
    // (opened, closed, Disabled_opened, Disabled_closed).
    // <P>
    // Not shown if +link{sectionStackSection.canCollapse} is false.
    //
    // @visibility external
    //<
    

    //> @attr sectionHeader.baseStyle    (CSSStyleName : "sectionHeader" : [IRA])
    // CSS class for the section header.
    // @visibility external
    //<

    //> @attr sectionHeader.noDoubleClicks (Boolean : true : IRA)
    // By default doubleClicks are disabled for SectionHeaders. All mouse click
    // events will be handled as single clicks. Set this property to <code>false</code>
    // to enable standard double-click handling.
    // @visibility external
    //<
    noDoubleClicks: true,

    // call our layout on click
    click : function () {
        // for certain skins (e.g. fleet) a widget inside of the sectionheader, when clicked,
        // will also cause the sectionheader to fire a click event. In that case we don't
        // want the sectionheader to register the click.
        if (this.contains(isc.EH.lastTarget)) return;
        if (!this.canCollapse || !this._hasLayout()) return;
        return this.getSectionStack().sectionHeaderClick(this);
    },

    draw : function (a,b,c,d) {
        if (isc._traceMarkers) arguments.__this = this;
        if (!this.readyToDraw()) return;

        

        // if the section cannot be collapsed, or SectionStack.showExpandControls: false, don't
        // show the expand/collapse icons and allow clicks anywhere to expand and collapse
        if (!this.canCollapse || (this._hasLayout() && this.getSectionStack() && 
            this.getSectionStack().showExpandControls == false))
        {
            this.icon = null;
            this.showIconState = false;
        }
        this.setCursor(this.getCurrentCursor());
        
        this.invokeSuper(isc.SectionHeader, "draw", a,b,c,d);

        this.addControls();

        
        if (this.headerControls != null) {
            this.headerLayout = isc.HLayout.create({
                autoDraw:false, width:this.getInnerWidth(), height:this.getInnerHeight(),
                members:this.headerControls
            });
            // Has to be a child, not a peer, so it will bubble clicks etc through if appropriate
            this.addChild(this.headerLayout);
            this.allowContentAndChildren = true;
        }
    },
    getCurrentCursor : function () {
        var cursor = this.cursor;
        // sections may be rendered outside of true sectionStacks
        // (for example in SectionItems)
        if (this.getSectionStack() && this.getSectionStack().getSectionCursor != null) {
            cursor = this.getSectionStack().getSectionCursor(this);
        }
        return cursor;
    }

    //> @method sectionHeader.titleClipped() (A)
    // Is the title of this section header clipped by +link{SectionHeader.controls,section controls}
    // or the edge of the header?
    // @return (boolean) whether the title is clipped.
    // @see attr:SectionHeader.clipTitle
    // @visibility external
    //<

    //> @method sectionHeader.titleHoverHTML()
    // Returns the HTML that is displayed by the default +link{SectionHeader.titleHover(),titleHover}
    // handler. Return null or an empty string to cancel the hover.
    // <smartgwt><p>Use <code>setTitleHoverFormatter()</code> to provide a custom
    // implementation.</smartgwt>
    // @param defaultHTML (HTMLString) the HTML that would have been displayed by default
    // @return (HTMLString) HTML to be displayed in the hover. If an empty string, then the hover
    // is canceled. If null, then the default HTML is used.
    // @visibility external
    //<

    //> @method sectionHeader.titleHover()
    // Optional stringMethod to fire when the user hovers over this section header and the title is
    // clipped. If +link{SectionHeader.showClippedTitleOnHover} is true, the default behavior is to
    // show a hover canvas containing the HTML returned by +link{SectionHeader.titleHoverHTML()}.
    // Return false to suppress this default behavior.
    // @return (boolean) false to suppress the standard hover
    // @see attr:SectionHeader.clipTitle
    // @see SectionHeader.titleClipped()
    // @group hovers
    // @visibility external
    //<

});

//> @class ImgSectionHeader
// SectionHeader class based on an HLayout with +link{StretchImg} background.
// @treeLocation Client Reference/Layout/SectionStack
// @visibility external
//<
isc.defineClass("ImgSectionHeader", "HLayout").addMethods({
    //> @attr imgSectionHeader.clipTitle
    // @include sectionHeader.clipTitle
    // @visibility external
    //<
    //> @attr imgSectionHeader.showClippedTitleOnHover
    // @include sectionHeader.showClippedTitleOnHover
    // @visibility external
    //<

    _canHover: true,

    //> @attr imgSectionHeader.icon
    // @include statefulCanvas.icon
    // @visibility external
    //<
    //> @attr imgSectionHeader.iconAlign
    // @include statefulCanvas.iconAlign
    // @visibility external
    //<
    //> @attr imgSectionHeader.iconSize
    // @include statefulCanvas.iconSize
    // @visibility external
    //<
    //> @attr imgSectionHeader.iconHeight
    // @include statefulCanvas.iconHeight
    // @visibility external
    //<
    //> @attr imgSectionHeader.iconWidth
    // @include statefulCanvas.iconWidth
    // @visibility external
    //<
    //> @attr imgSectionHeader.iconOrientation
    // @include statefulCanvas.iconOrientation
    // @visibility external
    //<
    //> @attr imgSectionHeader.prompt
    // @include canvas.prompt
    // @visibility external
    //<

    //> @attr imgSectionHeader.noDoubleClicks (Boolean : true : IRA)
    // By default doubleClicks are disabled for SectionHeaders. All mouse click
    // events will be handled as single clicks. Set this property to <code>false</code>
    // to enable standard double-click handling.
    // @visibility external
    //<
    noDoubleClicks: true,

    //> @attr ImgSectionHeader.background (AutoChild StretchImg : null : R)
    // Background of the section header, based on a StretchImg.
    // @visibility external
    //<
    backgroundDefaults : isc.addProperties({
        titleStyle:"sectionHeaderTitle",
        
        // These images now live in SectionHeader/ in the provided skins, but SectionStack/
        // is left as the default for backcompat with customer skins.
        src:"[SKIN]SectionStack/header.gif",

        
        backgroundColor:"#a0a0a0",

        setExpanded : function (expanded) {
            this.expanded = expanded;
            this.stateChanged();
        },

        // call our layout on click.  Note this function is placed on the background element so
        // that clicks on headerControls floating above the background do not trigger
        // expand/collapse
        click : function () {
            //>EditMode
            if (this.parentElement && this.parentElement.editingOn) {
                return this.Super("click", arguments);
            }
            //<EditMode
            if (this.parentElement.canCollapse) {
                if (this.parentElement.getSectionStack())
                    return this.parentElement.getSectionStack().sectionHeaderClick(this.parentElement);
            }
        },
        width:"100%", height:"100%", addAsChild:true,
        
        // pick up printStyleName from the header
        getPrintStyleName : function () {
            if (this.parentElement) return this.parentElement.getPrintStyleName();
            return this.Super("getPrintStyleName", arguments);
        }
    }, isc._commonMediaProps),

    getCanHover : function (a, b, c) {
        return this._canHover || this.invokeSuper(isc.ImgSectionHeader, "getCanHover", a, b, c);
    },

    setExpanded : function (expanded) {
        this.expanded = expanded;
        if (isc.Canvas.ariaEnabled()) {
            if (this._tabPanel != null) this._tabPanel.setAriaState("hidden", this.hidden || !expanded);
            var sectionStack = this.layout;
            if (isc.isA.SectionStack(sectionStack)) {
                var multiselectable = (sectionStack.visibilityMode != "mutex");
                if (multiselectable) {
                    this.setAriaState("expanded", !!expanded);
                } else {
                    this.setAriaState("selected", !!expanded);
                }
            }
        }
        if (this.background != null) this.background.setExpanded(expanded);
    },
    //>!BackCompat 2005.12.22
    setOpen : function (isOpen) {
        this.setExpanded(isOpen);
    },
    //<!BackCompat
    setTitle : function (title) {
        this.title = title;
        if (this.background) this.background.setTitle(title);
    },
    
    //> @method imgSectionHeader.setIcon()
    // Change the icon being shown for the header.
    // @param icon (URL) URL of new icon
    // @visibility external
    //<
    setIcon : function (icon) { 
        this.icon = icon;
        if (this.background) this.background.setIcon(icon); 
    },
    //> @method imgSectionHeader.setIconOrientation()
    // If this header is showing an icon should it appear to the left or right of the title?
    // Valid options are "left" and "right".
    // @param orientation (String) the new orientation
    // @visibility external
    //<
    setIconOrientation : function (orientation) { 
        this.orientation = orientation;
        if (this.background) this.background.setIconOrientation(orientation); 
    },


    //> @method imgSectionHeader.setAlign()
    // Sets the horizontal alignment of the title.
    // @param align (String) the new alignment
    // @visibility external
    //<
    setAlign : function (align) { 
        this.align = align;
        if (this.background) this.background.setAlign(align); 
    },

    //> @method imgSectionHeader.setPrompt()
    // Sets the text shown as a tooltip for the header.
    // @param prompt (HTMLString) the new tooltip
    // @visibility external
    //<
    setPrompt : function (prompt) { 
        this.prompt = prompt;
        if (this.background) this.background.setPrompt(prompt); 
    },

    draw : function (a,b,c,d) {    
        if (isc._traceMarkers) arguments.__this = this;
        if (!this.readyToDraw()) return;
        
        this.setupBackground();
        
        this.addControls();

        
        this.addAutoChildren(this.headerControls);

        this.background.sendToBack();

        this.invokeSuper(isc.ImgSectionHeader, "draw", a,b,c,d);
    },
    setupBackground : function () {

        var props = {
            title: this.title,
            clipTitle: this.clipTitle,
            // handle the clipped title hover ourselves
            showClippedTitleOnHover: false,
            _canHover: false,

            expanded: this.expanded,
            // handle focus on the header itself rather than this button.
            canFocus:false
        };
        if (this.align) {
            props.align = this.align;
        } else {
            var defaultAlign = isc.SectionHeader.getInstanceProperty("align");
            if (defaultAlign != null) {
                props.align = defaultAlign;
            }
        }
        if (this.prompt) props.prompt = this.prompt;
        if (this.icon) props.icon = this.icon;
        if (this.iconSize) props.iconSize = this.iconSize;
        if (this.iconHeight) props.iconHeight = this.iconHeight;
        if (this.iconWidth) props.iconWidth = this.iconWidth;
        if (this.iconAlign) props.iconAlign = this.iconAlign;
        if (this.iconOrientation) props.iconOrientation = this.iconOrientation;

        // if the section cannot be collapsed, or SectionStack.showExpandControls: false, don't
        // show the expand/collapse icons and allow clicks anywhere to expand and collapse
        if (!this.canCollapse || (this._hasLayout() && this.getSectionStack() &&
            this.getSectionStack().showExpandControls == false))
        {
            props.icon = null;
            props.showIconState = false;
        }
        
        // Make the background draggable so canReorderSections works automatically with this
        // sectionHeader class
        props.canDragReposition = this.canDragReposition;
        props.canDrop = this.canDrop;
        props.dragTarget = this;

        var cursor = this.getCurrentCursor();
        this.setCursor(cursor);
        props.cursor = cursor;

        props._getAfterPadding = function () {
            var controlsLayout = this.creator.controlsLayout;
            return (controlsLayout == null ? null : controlsLayout.getVisibleWidth());
        };

        if (this.background == null) {
            this.addAutoChild("background", props, isc.StretchImgButton);
        } else {
            this.background.setProperties(props);
        }
    },
    getCurrentCursor : function () {
        var cursor = this.cursor;
        // sections may be rendered outside of true sectionStacks
        // (for example in SectionItems)
        if (this.getSectionStack() && this.getSectionStack().getSectionCursor != null) {
            cursor = this.getSectionStack().getSectionCursor(this);
        }
        return cursor;
    },
    
    // Override getPrintHTML to just return the title HTML with the appropriate styling
    getPrintHTML : function (props) {
        if (this.background == null) this.setupBackground();
        return this.background.getPrintHTML(props);
    },

    //> @method imgSectionHeader.titleClipped() (A)
    // Is the title of this section header clipped by +link{ImgSectionHeader.controls,section controls}
    // or the edge of the header?
    // @return (boolean) whether the title is clipped.
    // @see attr:ImgSectionHeader.clipTitle
    // @visibility external
    //<
    titleClipped : function () {
        return (this.background == null ? false : this.background.titleClipped());
    },

    defaultTitleHoverHTML : function () {
        return (this.background == null ? null : this.background.defaultTitleHoverHTML());
    },

    //> @method imgSectionHeader.titleHoverHTML()
    // Returns the HTML that is displayed by the default +link{ImgSectionHeader.titleHover(),titleHover}
    // handler. Return null or an empty string to cancel the hover.
    // <smartgwt><p>Use <code>setTitleHoverFormatter()</code> to provide a custom
    // implementation.</smartgwt>
    // @param defaultHTML (HTMLString) the HTML that would have been displayed by default
    // @return (HTMLString) HTML to be displayed in the hover. If null or an empty string, then the hover
    // is canceled.
    // @visibility external
    //<
    titleHoverHTML : function (defaultHTML) {
        return defaultHTML;
    },

    handleHover : function (a, b, c) {
        // If there is a prompt, prefer the standard hover handling.
        if (this.canHover == null && this.prompt) return this.invokeSuper(isc.ImgSectionHeader, "handleHover", a, b, c);

        if (!this.showClippedTitleOnHover || !this.titleClipped()) {
            if (this.canHover) return this.invokeSuper(isc.ImgSectionHeader, "handleHover", a, b, c);
            else return;
        }

        if (this.titleHover && this.titleHover() == false) return;

        var HTML = this.titleHoverHTML(this.defaultTitleHoverHTML());
        if (HTML != null && !isc.isAn.emptyString(HTML)) {
            var hoverProperties = this._getHoverProperties();
            isc.Hover.show(HTML, hoverProperties, null, this);
        }
    }
});


isc.ImgSectionHeader.addMethods(isc._commonHeaderProps);

isc.ImgSectionHeader.registerStringMethods({
    //> @method imgSectionHeader.titleHover()
    // Optional stringMethod to fire when the user hovers over this section header and the title is
    // clipped. If +link{ImgSectionHeader.showClippedTitleOnHover} is true, the default behavior is to
    // show a hover canvas containing the HTML returned by +link{ImgSectionHeader.titleHoverHTML()}.
    // Return false to suppress this default behavior.
    // @return (boolean) false to suppress the standard hover
    // @see attr:ImgSectionHeader.clipTitle
    // @see ImgSectionHeader.titleClipped()
    // @group hovers
    // @visibility external
    //<
    titleHover : ""
});

isc.SectionStack.registerStringMethods({
    //> @method sectionStack.onSectionHeaderClick()
    // Notification method fired when the user clicks on a section header.
    // Returning false will cancel the default behavior (expanding / collapsing the section)
    // @param section (SectionHeader) SectionHeader clicked by the user
    // @return (boolean) returning false cancels the default behavior
    // @visibility sgwt
    //<
    
    onSectionHeaderClick:"sectionHeader"
});

isc.SectionStack.registerDupProperties(
    "sections",
    // second array is sub-properties!
    ["items"]);



