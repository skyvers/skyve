/*
 * Isomorphic SmartClient
 * Version v10.0p_2014-09-10 (2014-09-10)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */

 





//>	@class	TabSet
//
// The TabSet class allows components on several panes to share the same space. The tabs at 
// the top can be selected by the user to show each pane. 
// <P>
// Tabs are configured via the <code>tabs</code> property, each of which has a
// <code>pane</code> property which will be displayed in the main pane when that tab is
// selected.
//
//  @treeLocation Client Reference/Layout
//  @visibility external
//<

isc.ClassFactory.defineClass("TabSet", "Canvas");

isc.TabSet.addProperties({

    // NOTE: Setting both the paneContainer and TabSet to overflow:"visible" results in an
    // auto-expanding TabSet.  This may be appropriate as a top-level page layout when an
    // application is more web-style than desktop-style, eg, allows and utilizes browser-level
    // scrolling.
    overflow:"hidden",

	// TabBar
	// ----------------------------------------------------------------------------------------
    //>	@attr	tabSet.tabs		(Array of Tab : null : IRW)
    //
    // An array of tab objects, specifying the title and pane contents of each tab in the
    // TabSet.  When developing in JavaScript, tabs are specified as an array of object
    // literals, not instances - see +link{Tab}.
    // <p>
    // You can add and remove tabs after creating the TabSet by calling +link{TabSet.addTab}
    // @visibility external
    // @example tabsOrientation
    //<
    
    //> @object Tab
    // Tabs are specified as objects, not class instances.  For example, when
    // developing in JavaScript, a typical initialization block for a TabSet would look like
    // this:
    // <pre>
    // TabSet.create({
    //     tabs: [
    //         {title: "tab1", pane: "pane1"},
    //         {title: "tab2"}
    //     ]
    // });
    // </pre>
    // And in XML:
    // <pre>
    // &lt;TabSet&gt;
    //    &lt;tabs&gt;
    //        &lt;Tab title="tab1" pane="pane1"/&gt;
    //        &lt;Tab title="tab2"/&gt;
    //    &lt;/tabs&gt;
    // &lt;/TabSet&gt;
    // </pre>
    // 
    // @treeLocation Client Reference/Layout/TabSet
    // @visibility external
    //<

    //> @attr tab.title (HTMLString : null : IRW)
    // Specifies the title of the this tab.  To change the title after the TabSet has been
    // created, call +link{TabSet.setTabTitle}.
    //
    // @see TabSet.setTabTitle
    // @visibility external 
    //<

    //> @attr tab.canEditTitle (boolean : null : IRW)
    //
    // If specified, overrides the +link{TabSet.canEditTabTitles} setting, for this one tab
    // only.
    // <p>
    // Note that the TabSet's +link{TabSet.titleEditEvent,titleEditEvent} must be set to a
    // supported +link{TabTitleEditEvent} in order for users to be able to edit this tab's
    // title.
    //
    // @see TabSet.canEditTabTitles
    // @visibility external
    // @example userEditableTitles
    //<

    //> @attr tab.prompt (HTMLString : null : IRW)
    // Specifies the prompt to be displayed when the mouse hovers over the tab.
    //
    // @visibility external
    //<

    //> @attr tab.pickerTitle (HTMLString : null : IRW)
    // If +link{tabSet.showTabPicker} is true for this TabSet, if set this property will determine
    // the title of the picker menu item for this tab. If unset, +link{tab.title} will be used
    // instead
    // @see TabSet.showTabPicker
    // @see tab.title
    // @group tabBarControls
    // @visibility external
    //<

    //> @attr tab.pane (ID or Canvas: null : IRW)
    //
    // Specifies the pane associated with this tab.  You have two options for the value of
    // the pane attribute:
    // <ul>
    // <li><b>ID</b> - The global ID of an already created Canvas (or subclass).
    // <li><b>Canvas</b> - A live instance of a Canvas (or subclass).
    // </ul>
    // You can change the pane associated with a given tab after the TabSet has been created by
    // calling +link{TabSet.updateTab}.
    // 
    // @see TabSet.updateTab
    // @visibility external
    //<

    //> @attr tab.paneMargin (int : null : IR)
    // Space to leave around the pane within this Tab.
    // If specified, this property takes precedence over +link{tabSet.paneMargin}
    // @visibility external
    //<

    //> @attr tab.ID (identifier : null : IRW)
    // Optional ID for the tab, which can later be used to reference the tab.
    // APIs requiring a reference to a tab will accept the tab's ID 
    // [including  +link{tabSet.selectTab()}, +link{tabSet.updateTab()}, +link{tabSet.removeTab()}].<br>
    // The ID will also be passed to the +link{tabSet.tabSelected()} and +link{tabSet.tabDeselected()}
    // handler functions, if specified.
    // <p>
    // Note that if you provide an ID, it must be globally unique.  If you do not want a
    // globally unique identifier, set +link{tab.name} instead.
    //
    // @visibility external
    //< 

    //> @attr tab.name (identifier : null : IRW)
    // Optional name for the tab, which can later be used to reference the tab.
    // APIs requiring a reference to a tab will accept the tab's name
    // [including  +link{tabSet.selectTab()}, +link{tabSet.updateTab()}, +link{tabSet.removeTab()}].<br>
    // This name will also be passed to the +link{tabSet.tabSelected()} and +link{tabSet.tabDeselected()}
    // handler functions, if specified.
    // <p>
    // This identifier is requred to be locally unique to the TabSet and cannot be used to get
    // a global reference to the Tab.  If you want a global reference, set +link{tab.ID} instead.
    //
    // @visibility external
    //< 

    //> @attr tab.width (number : 100 : IRW)
    // You can specify an explicit width for the tab using this property.  Note that tabs
    // automatically size to make room for the full title, but if you want to e.g. specify a
    // uniform width for all tabs in a TabSet, this property enables you to do so.
    //
    // @visibility external
    //< 
    
    //> @attr tab.disabled (boolean : null : IRW)
    // If specified, this tab will initially be rendered in a disabled state. To enable or
    // disable tabs on the fly use the +link{tabSet.enableTab()}, and +link{tabSet.disableTab()}
    // methods.
    // @visibility external
    //<

    //> @attr tab.icon (SCImgURL : null : IRW)
    // If specified, this tab will show an icon next to the tab title.  
    // <p>
    // <b>NOTE:</b> if you enable +link{tabSet.canCloseTabs,closeable tabs},
    // <code>tab.icon</code> is used for the close icon.  +link{tabSet.canCloseTabs} describes
    // a workaround to enable both a <code>closeIcon</code> and a second icon to be shown.
    // <p>
    // Use +link{tabSet.tabIconClick} to add an event handler specifically for clicks on the icon.
    // <p>
    // If a tab +link{tab.disabled,becomes disabled}, a different icon will be loaded by adding
    // a suffix to the image name (see +link{Button.icon}).
    // <p>
    // You should specify a size for the icon via +link{tab.iconSize} or +link{tab.iconWidth}
    // and +link{tab.iconHeight}. Without an explicitly specified size, tabs may be drawn
    // overlapping or with gaps the first time a page is loaded, because the icon is not cached
    // and therefore its size isn't known.
    //
    // @visibility external
    // @example tabsOrientation
    // @see tabSet.tabIconClick
    //<
    
    //> @attr tab.iconSize (integer : 16 : IRW)
    // If +link{tab.icon} is specified, this property may be used to specify a size for the icon.
    // Per side sizing may be specified instead via +link{tab.iconWidth} and +link{tab.iconHeight}.
    // @visibility external
    //<
    defaultTabIconSize: 16,    
    
    //> @attr tab.iconWidth (integer : null : IRW)
    // If +link{tab.icon} is specified, this property may be used to specify a size for the icon
    // @visibility external
    //<
    
    //> @attr tab.iconHeight (integer : null : IRW)
    // If +link{tab.icon} is specified, this property may be used to specify a size for the icon
    // @visibility external
    //<
    
    //> @attr tab.canReorder (Boolean : null : IR)
    // If +link{tabSet.canReorderTabs} is set to <code>true</code>, setting <code>canReorder</code>
    // explicitly to <code>false</code> for some tab will disallow drag-reordering of
    // this tab. Has no effect if <code>canReorderTabs</code> is not true at the tabSet level.
    // <P>
    // Note that this setting also disallows a reorder of another tab into the slot before
    // or following this tab. This means for tabs located at the beginning or end of the 
    // tab-bar, users cannot changing the index of the tab by dropping another
    // before or after it. However if you have a <i><code>canReorder:false</code></i>
    // tab which is not at the beginning or end of the tab bar, users can
    // drag reorder other tabs around it which may ultimately change its position.
    // @visibility external
    // @see TabSet.canReorderTabs
    //<
    
    //> @attr tab.canClose (boolean : null : IRW)
    // Determines whether this tab should show a close icon allowing the user to dismiss the tab
    // by clicking on the close icon directly. The URL for the close icon's image will be derived from 
    // +link{tabSet.closeTabIcon} by default, but may be overridden by explicitly specifying
    // +link{tab.closeIcon}.
    // <p>
    // If unset or null, this property is derived from +link{tabSet.canCloseTabs}.
    // <p>
    // Note that setting <code>canClose</code> means that +link{tab.icon} cannot be used,
    // because it's used for the +link{tab.closeIcon,closeIcon} - see
    // +link{tabSet.canCloseTabs} for a workaround.
    // <p>
    // After the TabSet has been created, you can change a tab's canClose property by calling
    // +link{TabSet.setCanCloseTab()}.
    //
    // @visibility external
    // @example closeableTabs
    // @see TabSet.closeClick()
    //<

    //> @attr tab.closeIcon (SCImgURL : null : IRW)
    // Custom src for the close icon for this tab to display if it is closeable.
    // See +link{tab.canClose} and +link{tabSet.canCloseTabs}.
    // @visibility external
    //<
    
    //> @attr tab.closeIconSize (number : null :IRW)
    // Size of the +link{tab.closeIcon} for this tab. If unspecified the icon will be sized
    // according to +link{tabSet.closeTabIconSize}
    // @visibility external
    //<

    // ---------------------------------------------------------------------------------------

    //> @attr tabSet.tabBar (AutoChild TabBar : null : R)
    // TabBar for this TabSet, an instance of +link{TabBar}.
    // @visibility external
    //<
    // NOTE: tabBar is actually not created via autoChild system, but supports the same
    // defaults.

    //>	@attr tabSet.tabProperties (Tab Properties : null : IR)
	// Properties to apply to all Tabs created by this TabSet.
    // @visibility external
	//<
    tabProperties:{},

    //> @attr tabSet.defaultTabWidth (number : null : IR)
    // If set, is passed as "width" to all tabs when +link{tabBarPosition} is set to 
    // <code>"top"</code> or <code>"bottom"</code>.
    // <P>
    // If unset, width will be picked up from
    // the Tab constructor class defaults. Tabs expand to fit their content, so 
    // this width acts as a minimum.
    // Setting width:1 will result in tabs that are
    // only as wide as their titles. May be customized by individual
    // +link{group:skinning,skins}.
    // @visibility external
    //<

    //> @attr tabSet.defaultTabHeight (number : null : IR)
    // If set, is passed as "height" to all tabs when +link{tabBarPosition} is set to 
    // <code>"left"</code> or <code>"right"</code>. 
    // <P>
    // If unset, height will be picked up from
    // the Tab constructor class defaults. Note that tabs expand to fit their content so
    // this height acts as a minimum. May be customized by individual
    // +link{group:skinning,skins}.
    // @visibility external
    //<
    
    // Simple Tabs
    // ---------------------------------------------------------------------------------------

    //> @attr tabSet.useSimpleTabs (Boolean : false : IRA)
    // Should we use simple button based tabs styled with CSS rather than
    // image based +link{class:ImgTab} tabs?
    // <P>
    // <smartclient>
    // If set to true the +link{tabSet.simpleTabButtonConstructor} will be used and tabs will
    // by styled according to +link{tabSet.simpleTabBaseStyle}.
    // </smartclient>
    // <smartgwt>
    // If set to true tabs will instances of +link{class:Button}, styled according to the
    // +link{tabSet.simpleTabBaseStyle}.
    // </smartgwt>
    // @visibility external
    //<
    //useSimpleTabs:false,
    
    //> @attr tabSet.simpleTabBaseStyle (CSSStyleName : "tabButton" : [IRW])
    //  If this.useSimpleTabs is true, simpleTabBaseClass will be the base style used to 
    //  determine the css style to apply to the tabs.<br>
    //  This property will be suffixed with the side on which the tab-bar will appear, followed
    //  by with the tab's state (selected, over, etc), resolving to a className like 
    //  "tabButtonTopOver"
    // @visibility external
    //<
    simpleTabBaseStyle:"tabButton",
 
    // TabBar placement and sizing   
    // ---------------------------------------------------------------------------------------

    //> @attr tabSet.tabBarPosition (Side : isc.Canvas.TOP : IR)
    // Which side of the TabSet the TabBar should appear on.
    // @group tabBar
    // @visibility external
    // @example tabsOrientation
    //<
    tabBarPosition:isc.Canvas.TOP,

    //> @attr tabSet.tabBarAlign (Side | Alignment : see below : IR)
    // Alignment of the tabBar.
    // <P>
    // If the +link{tabSet.tabBarPosition, tabBarPosition} is "top" or "bottom", then 
    // this attribute may be set to "left", "right" or "center".  The default is "left", or
    // "right" in +link{isc.Page.isRTL,RTL mode}.
    // <P>
    // If the +link{tabSet.tabBarPosition, tabBarPosition} is "left" or "right", then this
    // attribute may be set to "top", "bottom" or "center".  The default is "top".
    // 
    // @group tabBar
    // @visibility external
    // @example tabsAlign
    //<
    

    //> @attr tabSet.tabBarThickness (number : 21 : IRW)
    // Thickness of tabBar, applies to either orientation (specifies height for horizontal,
    // width for vertical orientation).  Note that overriding this value for TabSets that are
    // skinned with images generally means providing new media for the borders.
    // @group tabBar
    // @visibility external
    //<
    tabBarThickness:21,

    // ---------------------------------------------------------------------------------------

    //>	@attr	tabSet.selectedTab		(number : 0 : IRW)
    // Specifies the index of the initially selected tab.
    // @group tabBar
    // @visibility external
    //<
	selectedTab:0,
    
    // ---------------------------------------------------------------------------------------

    //> @attr tabSet.canCloseTabs (boolean : null : IRW)
    // Should tabs in this tabSet show an icon allowing the user to dismiss the tab by
    // clicking on it directly. May be overridden for individual tabs by setting 
    // +link{tab.canClose}.
    // <P>
    // The URL for this icon's image will be derived from  +link{tabSet.closeTabIcon} by 
    // default, but may be overridden by explicitly specifying +link{tab.closeIcon}.
    // <P>
    // <b>Note</b>: Currently, tabs can only show a single icon, so a closable tab will show
    // the close icon only even if +link{tab.icon} is set.  To work around this, add the icon
    // as an HTML &lt;img&gt; tag to the +link{tab.title} property, for example:
    // <smartclient>
    // <pre>
    //    title : "&lt;span&gt;" + isc.Canvas.imgHTML("path/to/icon.png") + " Tab Title&lt;/span&gt;"
    // </pre>
    // </smartclient>
    // <smartgwt>
    // <pre>
    //    tab.setTitle("&lt;span&gt;" + Canvas.imgHTML("path/to/icon.png") + " Tab Title&lt;/span&gt;");
    // </pre>
    // </smartgwt>
    //
    // @see TabSet.closeClick()
    // @visibility external
    //<
    
    //> @attr tabSet.closeTabIcon (SCImgURL : [SKIN]/TabSet/close.png : IR)
    // Default src for the close icon for tabs to display if +link{tabSet.canCloseTabs} is true.
    // @visibility external
    //<
    closeTabIcon:"[SKIN]/TabSet/close.png",

    //> @attr tabSet.closeTabIconSize (int : 16 : IR)
    // Size in pixels of the icon for closing tabs, displayed when +link{canCloseTabs} is true.
    // @visibility external
    //<
    closeTabIconSize:16,

    //> @attr tabSet.canReorderTabs (boolean : null : IR)
    // If true, tabs can be reordered by dragging on them.
    // <P>
    // To disallow drag-reorder of a specific tab, see +link{tab.canReorder}.
    // @group dragdrop
    // @visibility external
    //<
    
    //> @attr tabSet.showMoreTab (boolean : null : IR)
    // @include tabBar.showMoreTab
    // @visibility external
    //<

    //> @attr tabSet.moreTabCount (number : 5 : IR)
    // @include tabBar.moreTabCount
    // @visibility external
    //<
    moreTabCount:5,

    //> @attr tabSet.moreTabTitle (String : "More" : IR)
    // Title for the "More" tab.
    // @visibility external
    //<
    moreTabTitle:"More",

    //> @attr tabSet.moreTabImage (SCImgURL : "[SKINIMG]/iOS/more.png" : IR)
    // If +link{showMoreTab} is enabled this property determines the image to display on
    // the "More" tab button.
    // @visibility external
    //<
    moreTabImage:"[SKINIMG]/iOS/more.png",

    //> @attr tabSet.moreTab (AutoChild Tab : null : R)
    // +link{object:Tab} to be shown when +link{showMoreTab} is enabled
    // more than +link{moreTabCount} tabs are provided.
    // @visibility external
    //<

    moreTabDefaults: { ariaRole:"tab" },

    //>	@attr tabSet.moreTabProperties (Tab Properties : null : IR)
    // Properties to apply to the "more" tab created by this TabSet.
    // @visibility external
    //<
    moreTabProperties:{},

    //> @attr tabSet.moreTabPane (AutoChild VLayout : null : R)
    // Pane contents for the "more" tab based on a VLayout. Typically contains
    // a +link{NavigationBar} and +link{TableView}.
    // @visibility external
    //<

    //>	@attr tabSet.moreTabPaneProperties (Canvas Properties : null : IR)
    // Properties to apply to the "more" tab's pane created by this TabSet.
    // @visibility external
    //<
    moreTabPaneProperties:{},

    //>	@attr tabSet.moreTabPaneDefaults (Canvas Properties : null : IR)
    // Default properties for the "more" tab's pane.
    // <p>
    // Currently constructs a VLayout with a +link{NavigationBar} and +link{TableView}.
    // @visibility external
    //<
    moreTabPaneDefaults:{
        _constructor: "VLayout",
        width: "100%",
        height: "100%",
        setData : function (newData) {
            this.creator.moreTabPaneTable.setData(newData);
        }
    },

    moreTabPaneNavBarDefaults:{
        _constructor: "NavigationBar",
        controls: ["titleLabel"],
        autoParent: "moreTabPane"
    },

    moreTabPaneTableDefaults:{
        _constructor: "TableView",
        width: "100%",
        height: "100%",
        recordNavigationClick : function (record) {
            this.creator._tabSelected(record.button);
        },
        autoParent: "moreTabPane"
    },

    // -----------------------------------------------------------
    // Tab bar controls

    //> @attr tabSet.tabBarControls (Array : "tabScroller"|"tabPicker"|Canvas: IRA)
    // This property determines what controls should show up after the tabBar for this TabSet.
    // Standard controls can be included using the strings <code>"tabScroller"</code> and 
    // <code>"tabPicker"</code>. These correspond to the +link{TabSet.scroller} and +link{TabSet.tabPicker}
    // AutoChildren, respectively. The <code>"tabScroller"</code> standard control shows two
    // buttons for scrolling through the tabs in order and the <code>"tabPicker"</code> standard
    // control allows tabs to be picked directly from a menu. The standard controls show up only if
    // +link{tabSet.showTabScroller} or +link{tabSet.showTabPicker} is true and there is not
    // enough space available to show all of the tabs in the tabBar.
    // <P>
    // +explorerExample{layout_tabs_custom_controls, This sample} illustrates the usage of this property
    // <P>
    // Additional controls can be included by adding any widget to this array.  Controls will
    // show up in the order in which they are specified.  For example, the following code would
    // add a button in the tabBar area, while preserving the normal behavior of the tabScroller
    // and tabPicker:
    // <smartclient>
    // <pre>
    // isc.TabSet.create({
    //     width:300,
    //     tabs : [
    //         { title: "Tab one" }
    //     ],
    //     tabBarControls : [
    //         isc.ImgButton.create({
    //             src:"[SKINIMG]/actions/add.png",
    //             width:16, height:16,
    //             layoutAlign:"center"
    //         }),
    //         "tabScroller", "tabPicker"
    //     ]
    // });
    // </pre>
    // </smartclient>
    // <smartgwt>
    // <pre>
    //		ImgButton addButton = new ImgButton();
    //		addButton.setSrc("[SKINIMG]/actions/add.png");
    //		addButton.setTitle("Add");
    //		addButton.setWidth(16);
    //		addButton.setHeight(16);
    //		addButton.setAlign(Alignment.CENTER);
    //		TabSet ts = new TabSet();
    //		ts.setWidth(300);
    //		ts.setHeight(32);
    //		ts.setTabs(new Tab("Tab one"));
    //		ts.setTabBarControls(addButton, TabBarControls.TAB_SCROLLER, TabBarControls.TAB_PICKER);
    //		contentLayout.addMember(ts);
    // </pre>
    // </smartgwt>
    // You can also refer to the default tabPicker/tabScroll controls
    // from Component XML:
    // <pre>
    // <TabSet width="300"> 
    //    <tabBarControls>
    // 	     <Button title="Custom Button"/>
    //       <value xsi:type="string">tabPicker</value>
    //       <value xsi:type="string">tabScroller</value>
    // 	  </tabBarControls>
    //    <tabs>
    //       <tab title="Foo"/>
    //       <tab title="Bar"/>
    //    </tabs>
    // </TabSet>
    // </pre>
    // <p>
    // When +link{Browser.isTouch} is <code>true</code> and native touch scrolling is supported,
    // then by default, only the <code>"tabPicker"</code> is shown. The <code>"tabScroller"</code>
    // control is omitted by default on touch devices because the tabs in the tab bar are native
    // touch-scrollable, so the <code>"tabScroller"</code> control is unnecessary. To override
    // the omission of the <code>"tabScroller"</code>, simply add
    // <smartclient>"tabScroller"</smartclient>
    // <smartgwt>{@link com.smartgwt.client.types.TabBarControls#TAB_SCROLLER}</smartgwt>
    // to the <code>tabBarControls</code> array.
    //
    // @group tabBarControls
    // @visibility external
    //<
    tabBarControls: ["tabScroller", "tabPicker"],


    //> @attr   tabSet.showTabScroller  (Boolean : true : [IR])
    // If there is not enough space to display all the tab-buttons in this tabSet, should 
    // scroller buttons be displayed to allow access to tabs that are clipped?
    // @visibility external
    // @group tabBarControls
    //<
    showTabScroller:true,

    //> @attr   tabSet.showTabPicker    (Boolean : true : [IR])
    // If there is not enough space to display all the tab-buttons in this tabSet, should
    // a drop-down "picker" be displayed to allow selection of tabs that are clipped?
    // @visibility external
    // @group tabBarControls
    //<    
    showTabPicker:true,

    //> @attr tabSet.tabBarControlLayout (AutoChild Layout : null : IR)
    // +link{AutoChild} of type +link{Layout} that holds the +link{tabBarControls} as well as
    // the built-in controls such as the +link{showTabPicker,tab picker menu}.
    // @visibility external
    //<
    tabBarControlLayoutConstructor:"Layout",
    tabBarControlLayoutDefaults:{},
    
    //>Animation
    //> @attr   tabSet.animateTabScrolling  (Boolean : true : [IR])
    // If +link{tabSet.showTabScroller} is true, should tabs be scrolled into view via an 
    // animation when the user interacts with the scroller buttons?
    // @visibility external
    // @group tabBarControls
    //<
    animateTabScrolling:true,
    //<Animation

    //> @attr tabSet.scroller (AutoChild StretchImgButton : null : R)
    // A component containing back and forward buttons for scrolling through all of the tabs
    // of the TabSet. The scroller is created automatically when needed and when <code>"tabScroller"</code>
    // is specified in the +link{TabSet.tabBarControls}.
    // <p>
    // By default, the scroller constructor is +link{StretchImgButton}. Note that the scroller
    // +link{StretchImg.items,items} are determined automatically, so any items set in
    // scrollerProperties will be ignored.
    // @group tabBarControls
    // @visibility external
    //<
    // @see TabSet.getScrollerBackImgName()
    // @see TabSet.getScrollerForwardImgName()
    scrollerDefaults: {
        _constructor: isc.StretchImgButton,

        // set noDoubleClicks - this means if the user clicks repeatedly on the
        // scroller we'll move forward 1 tab for each click rather than appearing
        // to swallow every other click
        noDoubleClicks: true,

        // Disable normal over/down styling as that would style both buttons at once
        showRollOver: false,
        showDown: false,

        mouseMove : function () {
            if (!this.creator.showScrollerRollOver) return;
            var currPart = this.inWhichPart();
            var otherPart = currPart == this.backPartName ? this.forwardPartName : this.backPartName;
            this.setState(isc.StatefulCanvas.STATE_UP, otherPart);
            this.setState(isc.StatefulCanvas.STATE_OVER, currPart);
        },
        mouseOut : function () {
            if (!this.creator.showScrollerRollOver) return;
            this.setState(isc.StatefulCanvas.STATE_UP, this.forwardPartName);
            this.setState(isc.StatefulCanvas.STATE_UP, this.backPartName);
        },
        mouseDown : function () {
            this.clickPart = this.inWhichPart();
            this.setState(isc.StatefulCanvas.STATE_DOWN, this.clickPart);
        },
        mouseUp : function () {
            this.setState(isc.StatefulCanvas.STATE_UP, this.clickPart);
        },
        mouseStillDown : function () {
            this.click();
        },
        click : function () {
            var back = this.clickPart == this.backPartName;
            
            if (this.isRTL()) back = !back;
            // figure out which part they clicked in and remember it
            if (back) this.creator.scrollBack();
            else this.creator.scrollForward();

            return false;
        }
    },

    //> @attr   tabSet.scrollerButtonSize   (number : 16 : [IR])
    // If +link{tabSet.showTabScroller} is true, this property governs the size of scroller
    // buttons. Applied as the width of buttons if the tabBar is horizontal, or the height
    // if tabBar is vertical. Note that the other dimension is determined by 
    // +link{tabBarThickness,this.tabBarThickness}
    // @group tabBarControls
    // @visibility external
    //<
    scrollerButtonSize:16,

    //> @attr tabSet.tabPicker (AutoChild ImgButton : null : R)
    // A button control that allows tabs to be picked directly from a popup menu. The tabPicker
    // is created automatically when needed and when <code>"tabPicker"</code> is specified in
    // the +link{TabSet.tabBarControls}.
    // @group tabBarControls
    // @visibility external
    //<
    // @see TabSet.getTabPickerSrc()
    tabPickerDefaults: {
        _constructor: isc.ImgButton,
        showRollOver: false,

        click : function () {
            this.creator.showTabPickerMenu();
        }
    },

    //> @attr tabSet.pickerButtonSize (int : 16 : IR)
    // If +link{TabSet.showTabPicker,showTabPicker} is <code>true</code> and +link{Browser.isTouch}
    // is <code>false</code>, this property governs the size of the tab picker button. This value
    // is applied as the width of the tab picker button if the +link{TabSet.tabBar,tabBar} is
    // horizontal, or the height if the <code>tabBar</code> is vertical. Note that the other
    // dimension is determined by +link{tabBarThickness,this.tabBarThickness}.
    // <p>
    // On touch browsers (where +link{Browser.isTouch} is <code>true</code>),
    // +link{TabSet.touchPickerButtonSize,touchPickerButtonSize} is used instead.
    // @group tabBarControls
    // @visibility external
    //<
    pickerButtonSize:16,

    //> @attr tabSet.touchPickerButtonSize (int : 16 : IR)
    // The size of the tab picker button when +link{Browser.isTouch} is <code>true</code>.
    // @see TabSet.pickerButtonSize
    // @group tabBarControls
    // @visibility external
    //<
    touchPickerButtonSize:16,

    //> @attr   tabSet.skinImgDir (string : "images/TabSet/" : [IR])
    // @include Canvas.skinImgDir
    //<
	skinImgDir:"images/TabSet/",

    //> @attr tabSet.symmetricScroller (Boolean : true : [IR])
    // If this TabSet is showing +link{tabSet.showTabScroller,tab scroller buttons}, this property 
    // determines whether the +link{tabSet.scrollerHSrc} and +link{tabSet.scrollerVSrc} media
    // will be used for vertical and horizontal tab-bar scroller buttons, or whether separate
    // media should be used for each possible +link{tabSet.tabBarPosition,tabBarPosition} based
    // on the +link{tabSet.scrollerSrc} property for this tabSet.
    // @group tabBarScrolling
    // @visibility external
    //<
    symmetricScroller:true,
    
    //> @attr   tabSet.scrollerSrc (SCImgURL : "[SKIN]/scroll.gif" : [IR])
    // If this TabSet is showing +link{tabSet.showTabScroller,tab scroller buttons}, and 
    // +link{tabSet.symmetricScroller,symmetricScroller} is false, this property governs the base
    // URL for the tab bar back and forward scroller button images.
    // <P>
    // Note that if +link{tabSet.symmetricScroller,symmetricScroller} is true, 
    // +link{tabSet.scrollerHSrc} and +link{tabSet.scrollerVSrc} will be used instead.
    // <P>
    // To get the path to the image to display, this base URL will be modified as follows:
    // <ul>
    // <li>If appropriate a state suffix of <code>"Down"</code> or <code>"Disabled"</code> will be
    //     appended.</li>
    // <li>The +link{tabSet.tabBarPosition,tabBarPosition} for this tabSet will be appended.</li>
    // <li>A suffix of <code>"forward"</code> or <code>"back"</code> will be appended for the
    //     forward or backward scrolling button.</li>
    // </ul>
    // For example - if the scrollerSrc is set to <code>"[SKIN]scroll.gif"</code>, the image
    // displayed for the back-scroller button on a tabSet with <code>tabBarPosition</code> set to
    // "top" and <code>symmetricScroller</code> set to false would be one of 
    // <code>"[SKIN]scroll_top_back.gif"</code>, <code>"[SKIN]scroll_Down_top_back.gif"</code>,
    // and <code>"[SKIN]scroll_Disabled_top_back.gif"</code>.
    // <P>
    // Note that for best results the media should be sized to match the scroller button sizes, 
    // determined by +link{tabSet.tabBarThickness} and +link{tabSet.scrollerButtonSize}.
    // @see tabSet.symmetricScroller
    // @group tabBarScrolling
    // @visibility external
    //<
    scrollerSrc:"[SKIN]/scroll.gif",
    
    //> @attr   tabSet.scrollerHSrc (SCImgURL :"[SKIN]hscroll.gif" : [IR])
    // If this TabSet is showing +link{tabSet.showTabScroller,tab scroller buttons}, and 
    // +link{tabSet.symmetricScroller,symmetricScroller} is true, this property governs the base
    // URL for the tab bar back and forward scroller button images for horizontal tab bars [IE for
    // tab sets with +link{tabSet.tabBarPosition,tabBarPosition} set to "top" or "bottom"].
    // <P>
    // Note that if +link{tabSet.symmetricScroller,symmetricScroller} is false, 
    // +link{tabSet.scrollerSrc} will be used instead.
    // <P>
    // To get the path to the image to display, this base URL will be modified as follows:
    // <ul>
    // <li>If appropriate a state suffix of <code>"Down"</code> or <code>"Disabled"</code> will be
    //     appended.</li>
    // <li>A suffix of <code>"forward"</code> or <code>"back"</code> will be appended for the
    //     forward or backward scrolling button.</li>
    // </ul>
    // For example - if the scrollerHSrc is set to <code>"[SKIN]hscroll.gif"</code>, the image
    // displayed for the back-scroller button on a tabSet with <code>tabBarPosition</code> set to
    // "top" and <code>symmetricScroller</code> set to true would be one of 
    // <code>"[SKIN]hscroll_back.gif"</code>, <code>"[SKIN]hscroll_Down_back.gif"</code>,
    // and <code>"[SKIN]hscroll_Disabled_back.gif"</code>.
    // <P>
    // Note that for best results the media should be sized to match the scroller button sizes, 
    // determined by +link{tabSet.tabBarThickness} and +link{tabSet.scrollerButtonSize}.
    // @see tabSet.symmetricScroller
    // @group tabBarScrolling
    // @visibility external
    //<
    scrollerHSrc:"[SKIN]hscroll.gif",
    
    //> @attr   tabSet.scrollerVSrc (SCImgURL :"[SKIN]vscroll.gif" : [IR])
    // If this TabSet is showing +link{tabSet.showTabScroller,tab scroller buttons}, and 
    // +link{tabSet.symmetricScroller,symmetricScroller} is true, this property governs the base
    // URL for the tab bar back and forward scroller button images for vertical tab bars [IE for
    // tab sets with +link{tabSet.tabBarPosition,tabBarPosition} set to "left" or "right"].
    // <P>
    // Note that if +link{tabSet.symmetricScroller,symmetricScroller} is false, 
    // +link{tabSet.scrollerSrc} will be used instead.
    // <P>
    // To get the path to the image to display, this base URL will be modified as follows:
    // <ul>
    // <li>If appropriate a state suffix of <code>"Down"</code> or <code>"Disabled"</code> will be
    //     appended.</li>
    // <li>A suffix of <code>"forward"</code> or <code>"back"</code> will be appended for the
    //     forward or backward scrolling button.</li>
    // </ul>
    // For example - if the scrollerVSrc is set to <code>"[SKIN]vscroll.gif"</code>, the image
    // displayed for the back-scroller button on a tabSet with <code>tabBarPosition</code> set to
    // "left" and <code>symmetricScroller</code> set to true would be one of 
    // <code>"[SKIN]vscroll_back.gif"</code>, <code>"[SKIN]vscroll_Down_back.gif"</code>,
    // and <code>"[SKIN]vscroll_Disabled_back.gif"</code>.
    // <P>
    // Note that for best results the media should be sized to match the scroller button sizes, 
    // determined by +link{tabSet.tabBarThickness} and +link{tabSet.scrollerButtonSize}.
    // @see tabSet.symmetricScroller
    // @group tabBarScrolling
    // @visibility external
    //<
    scrollerVSrc:"[SKIN]vscroll.gif",
    
    //> @attr tabSet.showScrollerRollOver (boolean : false : [IR])
    // set this to true to show scroller rollover images when the mouse is over the scroller 
    // buttons
    // @group tabBarScrolling
    //<
    
    //> @attr tabSet.scrollerProperties (Object : null : [IR])
    // Properties set here override those supplied by default when creating
    // the scroller control.
    // @group tabBarScrolling
    //<
    
    
    //> @attr tabSet.symmetricPickerButton (Boolean : true : [IR])
    // If this TabSet is showing a +link{tabSet.showTabPicker,tab picker button}, this
    // property determines whether the +link{tabSet.pickerButtonHSrc} and
    // +link{tabSet.pickerButtonVSrc} media will be used for vertical and horizontal tab-bar
    // picker buttons, or whether separate media should be used for each possible 
    // +link{tabSet.tabBarPosition,tabBarPosition} based on the +link{tabSet.pickerButtonSrc}
    // property  for this tabSet.
    // @group tabBarScrolling
    // @visibility external
    //<
    symmetricPickerButton:true,
    
    //> @attr   tabSet.pickerButtonSrc (SCImgURL : "[SKIN]/picker.gif" : [IR])
    // If +link{tabSet.showTabPicker} is true, this property governs the base URL for the picker
    // button image, when +link{tabSet.symmetricPickerButton} is set to false
    // <P>
    // Note that if <code>symmetricPickerButton</code> is true, the +link{tabSet.pickerButtonHSrc} 
    // and +link{tabSet.pickerButtonVSrc} properties will be used instead.
    // <P>
    // To get the path to the image to display, this base URL will be modified as follows:
    // <ul>
    // <li>If appropriate a state suffix of <code>"Down"</code> or <code>"Disabled"</code> will be
    //     appended.</li>
    // <li>The +link{tabSet.tabBarPosition,tabBarPosition} for this tabSet will be appended.</li>
    // </ul>
    // @see tabSet.symmetricPickerButton    
    // @group tabBarScrolling
    // @visibility external
    //<    
    pickerButtonSrc:"[SKIN]/picker.gif",
    
    //> @attr   tabSet.pickerButtonHSrc (SCImgURL : "[SKIN]hpicker.gif" : [IR])
    // If +link{tabSet.showTabPicker} is true, and +link{tabSet.symmetricPickerButton} is 
    // set to true, this property governs the base URL for the picker
    // button image, when displayed in a horizontal tab-bar [IE +link{tabSet.tabBarPosition} is
    // set to <code>"top"</code> or <code>"bottom"</code>].
    // <P>
    // Note that if <code>symmetricPickerButton</code> is false, the +link{tabSet.pickerButtonSrc}
    // property will be used instead.
    // <P>
    // This base URL will have a suffix of <code>"Down"</code> appended when the user holds the
    // mouse down over the button, and <code>"Disabled"</code> if the tabset as a whole is 
    // disabled.
    // @see tabSet.symmetricPickerButton    
    // @group tabBarScrolling
    // @visibility external
    //<    
    pickerButtonHSrc:"[SKIN]hpicker.gif",
    
    //> @attr   tabSet.pickerButtonVSrc (SCImgURL : "[SKIN]vpicker.gif" : [IR])
    // If +link{tabSet.showTabPicker} is true, and +link{tabSet.symmetricPickerButton} is 
    // set to true, this property governs the base URL for the picker
    // button image, when displayed in a verricaL tab-bar [IE +link{tabSet.tabBarPosition} is
    // set to <code>"LEFT"</code> or <code>"right"</code>].
    // <P>
    // Note that if <code>symmetricPickerButton</code> is false, the +link{tabSet.pickerButtonSrc}
    // property will be used instead.
    // <P>
    // This base URL will have a suffix of <code>"Down"</code> appended when the user holds the
    // mouse down over the button, and <code>"Disabled"</code> if the tabset as a whole is 
    // disabled.
    // @see tabSet.symmetricPickerButton    
    // @group tabBarScrolling
    // @visibility external
    //<    
    pickerButtonVSrc:"[SKIN]vpicker.gif",

	// PaneContainer
	// ----------------------------------------------------------------------------------------

    //> @attr tabSet.paneContainer (AutoChild VLayout : null : R)
    // Container where the component specified by +link{tab.pane} is shown.
    // <P>
    // Note: paneContainer and showEdges:true for rounded tabsets: you can enable decorative
    // image-based edges on the paneContainer by setting +link{Canvas.showEdges,showEdges:true}
    // via paneContainerDefaults (to skin all tabsets) or paneContainerProperties (to use
    // edges on one instance).  In this structure, the +link{group:baseLine} should use media
    // that matches the appearance of the decorative edges and fully overlaps the edge of the
    // paneContainer that it is adjacent to.  In the most typical appearance (symmetric edges
    // on all 4 sides), both +link{tabBar.baseLineCapSize} and +link{tabBar.baseLineThickness}
    // match the +link{canvas.edgeSize,edgeSize} set on the paneContainer.  See the
    // load_skin.js file for the "SmartClient" skin for an example of setting all relevant
    // properties.
    // <P>
    // To disable edges for a particular TabSet, which you may want to do for a TabSet that
    // is already within a clearly defined container, configure the paneContainer to show only
    // it's top edge:
    // <pre>
    //      paneContainerProperties : { customEdges:["T"] },
    // </pre>
    // To completely flatten even the top edge of the TabSet:
    // <pre>
    //      paneContainerProperties : { customEdges:["T"] },
	//      tabBarProperties :{ baseLineCapSize:0 },
    // </pre>
    // This "flattens" the baseLine so that only the center image is used.
    //
    // @visibility external
    //<
    // XXX: advice above suboptimal:
    // - in general, the StretchImg baseline is using different media names for the same media.
    //   Could be fixed by passing custom sib.items to the baseline
    // - when we "flatten" as above, the paneContainer is still rendering a top edge and still
    //   using 3 pieces of media, it's just occluded by the baseline.  Ideally, we'd turn the
    //   edges off entirely, but by default this would cause the baseline to actually overlap
    //   widgets show in the paneContainer, so a margin would need to be set in CSS to
    //   compensate - more complicated to explain

	paneContainerConstructor:"PaneContainer",

	//>	@attr	tabSet.paneContainerClassName		(CSSStyleName : null : IRW)
	// CSS style used for the paneContainer.
    // @visibility external
	//<
	paneContainerClassName:"tabSetContainer",

    //>	@attr	tabSet.paneContainerOverflow	(Overflow : isc.Canvas.AUTO : IRWA)
	// Specifies the overflow of the pane container (the component that holds the pane contents
    // for all tabs).  By default this is set to "auto", meaning the pane container will
    // automatically introduce scrolling when the pane contents exceed the TabSet's specified
    // size.
    // <p>
    // For other values and their meaning, see +link{Overflow}
    //
    // @visibility external
	//<
	paneContainerOverflow:isc.Canvas.AUTO,

    //> @method tabSet.setPaneContainerOverflow()
    // Update +link{paneContainerOverflow} after creation.
    //
    // @param newOverflow (Overflow) new overflow setting
    // @visibility external
    //<
    setPaneContainerOverflow : function (newOverflow) {
        this.paneContainerOverflow = newOverflow;
        if (this.paneContainer) this.paneContainer.setOverflow(newOverflow);
    },
    
    //> @attr tabSet.symmetricEdges (Boolean : true : IR)
    // If this tabSet will +link{tabSet.showPaneContainerEdges,show edges} for the paneContainer,
    // this property determines whether the same edge media will be used regardless of the tab
    // bar position, or whether different media should be used (necessary if the edge appearance is
    // not symmetrical on all sides).
    // <P>
    // If this property is set to false the paneContainer edge image URLs will be prefixed with
    // the tabBarPosition of the tabSet - for example <code>"[SKIN]edge_top_T.gif"</code> rather
    // than just <code>"[SKIN]edge_T.gif"</code>.
    // <P>
    // When <code>symmetricEdges</code> is false, custom edge sizes for the pane container may be
    // specified via +link{tabSet.topEdgeSizes} et al, and custom edge offsets via 
    // +link{tabSet.topEdgeOffsets} et al.
    // @visibility external
    //<
    symmetricEdges:true,
    
    //> @type EdgeSizes
    // Object used to specify custom edge sizes or offsets.
    // Specified as an object where <code>defaultSize</code> will map to the default edge size or 
    // offset for the canvas (+link{canvas.edgeSize}, or +link{canvas.edgeOffset} and
    // <code>top</code>, <code>left</code>, <code>right</code> and
    // <code>bottom</code> will map to the
    // +link{edgedCanvas.edgeTop,edgeTop}/+link{edgedCanvas.edgeOffsetTop,edgeOffsetTop}, 
    // +link{edgedCanvas.edgeLeft,edgeLeft}/+link{edgedCanvas.edgeOffsetLeft,edgeOffsetLeft},
    // +link{edgedCanvas.edgeRight,edgeRight}/+link{edgedCanvas.edgeOffsetRight,edgeOffsetRight},
    // and +link{edgedCanvas.edgeBottom,edgeBottom}/+link{edgedCanvas.edgeOffsetBottom,edgeOffsetBottom}
    // attributes on the paneContainer respectively. Note that not all these properties have to be
    // set - if unset standard edge sizing rules will apply. 
    // @visibility external
    //<
       
    //> @attr tabSet.leftEdgeSizes (EdgeSizes : null : IR)
    // If this tabSet will +link{tabSet.showPaneContainerEdges,show edges} for the paneContainer,
    // and +link{tabSet.symmetricEdges} is set to false, the <code>leftEdgeSizes</code>, 
    // <code>rightEdgeSizes</code>, <code>topEdgeSizes</code> and <code>bottomEdgeSizes</code> 
    // properties allow the sizes of edges for the paneContainer to be customized depending on
    // the +link{tabSet.tabBarPosition}.
    // <P>
    // The attribute should be specified an +link{type:EdgeSizes,edgeSizes map}, specifying the
    // desired edge sizes where for the appropriate +link{tabSet.tabBarPosition}.
    // @visibility external
    //<
    
    //> @attr tabSet.topEdgeSizes (EdgeSizes : null : IR)
    // @include tabSet.leftEdgeSizes
    // @visibility external
    //<
    
    //> @attr tabSet.bottomEdgeSizes (EdgeSizes : null : IR)
    // @include tabSet.leftEdgeSizes
    // @visibility external
    //<
    
    //> @attr tabSet.rightEdgeSizes (EdgeSizes : null : IR)
    // @include tabSet.leftEdgeSizes
    // @visibility external
    //<
    
    //> @attr tabSet.leftEdgeOffsets (EdgeSizes : null : IR)
    // If this tabSet will +link{tabSet.showPaneContainerEdges,show edges} for the paneContainer,
    // and +link{tabSet.symmetricEdges} is set to false, the <code>leftEdgeOffsets</code>, 
    // <code>rightEdgeOffsets</code>, <code>topEdgeOffsets</code> and <code>bottomEdgeOffsets</code> 
    // properties allow the offsets of edges for the paneContainer to be customized depending on
    // the +link{tabSet.tabBarPosition}.
    // <P>
    // The attribute should be specified an +link{type:EdgeSizes,edgeSizes map}, specifying the
    // desired edge offsets where for the appropriate +link{tabSet.tabBarPosition}.
    // @visibility external
    //<
    
    //> @attr tabSet.rightEdgeOffsets (EdgeSizes : null : IR)
    // @include tabSet.leftEdgeOffsets
    // @visibility external
    //<
    
    //> @attr tabSet.topEdgeOffsets (EdgeSizes : null : IR)
    // @include tabSet.leftEdgeOffsets
    // @visibility external
    //<
    
    //> @attr tabSet.bottomEdgeOffsets (EdgeSizes : null : IR)
    // @include tabSet.leftEdgeOffsets
    // @visibility external
    //<
    
    //>	@attr	tabSet.showPaneContainerEdges (boolean : null : IRWA)
    // Should the paneContainer for this tabset show +link{Canvas.showEdges,edges}.
    //
    // @visibility external
    //<
    // set to null not false by default so we pick up the value from paneContainerDefaults
    // for backCompat (pre 6.1) 

    //> @attr tabSet.paneMargin (int : 0 : IR)
    // Space to leave around the panes in our paneContainer
    // <P>
    // Note that this property may be specified on a per-tab basis via +link{tab.paneMargin}.
    // @visibility external
    //<
    //paneMargin:0

    //>	@attr tabSet.canEditTabTitles (Boolean : false : IRW)
	// If true, users can edit the titles of tabs in this TabSet when the 
    // +link{titleEditEvent,titleEditEvent} fires.  You can override this behavior per tab 
    // with the +link{Tab.canEditTitle} property.
    // <p>
    // Note that this TabSet's +link{TabSet.titleEditEvent,titleEditEvent} must be set to a
    // supported +link{TabTitleEditEvent} in order for users to be able to edit the titles of
    // tabs.
    // @visibility external
    // @example userEditableTitles
	//<

    //>	@attr tabSet.titleEditEvent (TabTitleEditEvent : null : IRW)
	// The event that triggers title editing on this TabSet.
    // @see canEditTabTitles
    // @see Tab.canEditTitle
    // @visibility external
    // @example userEditableTitles
	//<

    //> @type TabTitleEditEvent
    // An event that triggers title editing in a TabSet.
    // @value "click"       Start editing when the user single-clicks a tab title
    // @value "doubleClick" Start editing when the user double-clicks a tab title
    // @visibility external
    //<

    //> @attr tabSet.titleEditor (AutoChild TextItem : null : R)
	// TextItem we use to edit tab titles in this TabSet.  You can override this property 
    // using the normal +link{AutoChild} facilities.
    // @see canEditTabTitles
    // @see Tab.canEditTitle
    // @see TabSet.editTabTitle
    // @visibility external
	//<
	
	// Explicitly call out titleEditorProperties as TextItem config so it gets 
	// picked up in SGWT
	//> @attr tabSet.titleEditorProperties (TextItem properties : null : IR)
	// Properties for the auto-generated +link{tabSet.titleEditor}. This is the text item
	// we use to edit tab titles in this tabSet.
	// @see tabSet.titleEditor
	// @see canEditTabTitles
	// @visibility external
	//<

    //>	@attr tabSet.titleEditorLeftOffset (Integer : null : IRW)
	// If set, offsets the tab title editor further in from the left-hand edge of the tab, by
    // the number of pixels set in this property.  Note that the editor is always offset to
    // avoid overlapping the endcaps of the tab; this property is applied on top of that 
    // default offset.
    // @see titleEditorRightOffset
    // @see titleEditorTopOffset
    // @visibility external
	//<

    //>	@attr tabSet.titleEditorRightOffset (Integer : null : IRW)
	// If set, offsets the tab title editor further in from the right-hand edge of the tab, by
    // the number of pixels set in this property.  Note that the editor is always offset to
    // avoid overlapping the endcaps of the tab; this property is applied on top of that 
    // default offset.
    // @see titleEditorLeftOffset
    // @see titleEditorTopOffset
    // @visibility external
	//<

    //>	@attr tabSet.titleEditorTopOffset (Integer : null : IRW)
	// If set, offsets the tab title editor further down from the top edge of the tab, by the
    // number of pixels set in this property.  You can use this property, together with the 
    // left and right offset properties, to fine tune positioning of the editor within or 
    // around the tab button.<p>
    // <b>Note:</b> The height of the editor is an attribute of the editor itself, and can be
    // set by specifying a "height" property in +link{titleEditor,titleEditorDefaults}.
    // @see titleEditorLeftOffset
    // @see titleEditorRightOffset
    // @visibility external
	//<
    
    titleEditorDefaults: {
        name: "title", type: "text", 
        showTitle: false
    },
    
    //> @attr tabSet.useIOSTabs (boolean : null : IRW)
    //
    // Setting this to true turns on a different appearance for tabs, similar to iOS tabs from 
    // the "Music" app, where the tab.icon is enlarged and shown as a black and white mask.  
    // This mode does not support a clickable icon - clicking the enlarged icon just switches 
    // tabs.
    // <P>
    // This attribute only has an effect when +link{TabSet.canCloseTabs, canCloseTabs} is 
    // false, and only for Mobile WebKit, by default.
    //
    // @visibility external 
    //<    
    useIOSTabs: isc.Browser.isWebKit && isc.Browser.isMobile

});

// Have an explicit subclass of Button for tabs when useSimpleTabs is true.
// This allows us to include "pane" in the schema - required for the visual builder.
// It also would allow for simpler skinning customizations.

//> @class SimpleTabButton
// Simple subclass of +link{Button} used for tabs in a +link{TabSet} if +link{tabSet.useSimpleTabs}
// is true. See also +link{tabSet.simpleTabButtonConstructor}.
// @treeLocation Client Reference/Layout/TabSet
// @visibility external
//<
isc.defineClass("SimpleTabButton", "Button");

isc.SimpleTabButton.addProperties({

    // Override the default width of 100 set on button
    
    width:null,
    height:null,
    
    setIcon : function (icon) {
        var tabset = this.parentElement ? this.parentElement.parentElement : null;
        if (tabset && !tabset.canCloseTabs && tabset.useIOSTabs) {
            // Make sure a previous icon is replaced
            this.iOSIcon = null;
        }
        this.Super("setIcon", arguments);
    },
    getTitle : function () {
        var tabset = this.parentElement ? this.parentElement.parentElement : null;
        if (tabset && !tabset.canCloseTabs && tabset.useIOSTabs) {
            if (!this.iOSIcon && this.icon) {
                this.iOSIcon = this.icon;
                this.icon = null;
            }
            var imgHTML = (tabset.iOSIcon == null
                ? "<span style='height: 30px'>&nbsp;</span>"
                : isc.Canvas.imgHTML("[SKIN]blank.gif", 30, 30, null, 
                      "style='-webkit-mask-box-image: url(" +
                            isc.Page.getImgURL(this.iOSIcon) + 
                      ");",
                      null, null)),
                titleHTML = "<span>" + this.title + "</span>"
            ;
            return imgHTML + titleHTML;
        }
        return this.Super("getTitle", arguments);
    },


    //>EditMode 
    // needed so that we can autodiscover this method to update the pane.
    setPane : function (pane) {
        this.parentElement.parentElement.updateTab(this, pane);
    }, 
    // needed to allow a zero-parameter action for selecting a tab
    selectTab : function () {
        this.parentElement.parentElement.selectTab(this);
    }
    //<EditMode
    
});

isc.TabSet.addMethods({

//> @attr tabSet.simpleTabButtonConstructor (Class : SimpleTabButton : IRA)
// Tab button constructor if +link{tabSet.useSimpleTabs} is true.
// @visibility external
//<
simpleTabButtonConstructor: isc.SimpleTabButton,

//>	@method	tabSet.initWidget()	(A)
// Initialize the TabSet object 
//<
initWidget : function () {

    // disallow 'showEdges:true' on tabSets - this is an effect the user essentially never wants
    // as edges would encompass the tab-bar as well as the (rectangular) pane container.
    
    this.showEdges = false;

	// call the superclass function
	this.Super("initWidget",arguments);

    if (this.tabs == null) this.tabs = [];
    if (this.tabBarDefaults == null) this.tabBarDefaults = {};
    // NOTE: tabInstanceDefaults is old name
    this.tabProperties = this.tabProperties || this.tabInstanceDefaults || {};
    // Set up some dynamic defaults to apply to all tabs (without modifying the
    // tabProperties object directly, which is shared across all TabSets!)
    this.dynamicTabProperties = {};
    
    var pos = this.tabBarPosition;
	var  vTabs = (pos == "left") || (pos == "right");

    // if tabBarAlign is unset, set default based on tabBarPosition 
    if (this.tabBarAlign == null) {
        this.tabBarAlign = (vTabs ? "top"
                            : (this.isRTL() ? "right" : "left"));
    }
    
    // If this has the 'useSimpleTabs' property set to true, create buttons rather than imgTabs
    // as tabs in the tab bar.  Saves on creating a number of widgets for performance.
    
    if (this.useSimpleTabs) {
        // also update the styling
        this.tabBarDefaults.buttonConstructor = this.simpleTabButtonConstructor;
        // eg base + "Right" (derived from "right")
        this.dynamicTabProperties.baseStyle = this.simpleTabBaseStyle + 
                pos.substring(0,1).toUpperCase() + pos.substring(1);

        this.dynamicTabProperties.ariaRole = "tab";
    }    

	// defaultTabWidth / Height only apply on the "length" axis of tabs
	// since the thickness is determined by the tab-bar width.
    if (this.defaultTabWidth && !vTabs) {
        this.dynamicTabProperties.width = this.defaultTabWidth;
    }
    if (this.defaultTabHeight && vTabs) {
        this.dynamicTabProperties.height = this.defaultTabHeight;
    }

    if (this.defaultTabIconSize) {
        this.dynamicTabProperties.iconSize = this.defaultTabIconSize;
    }

    // Per the documentation, on touch devices, the default tabBarControls omits the "tabScroller"
    // because the tabs are native touch-scrollable. If the tabBarControls array instance is
    // unchanged, then the app is using the default controls.
    if (this._browserSupportsNativeTouchScrolling &&
        this.tabBarControls === isc.TabSet.getInstanceProperty("tabBarControls"))
    {
        this.tabBarControls = ["tabPicker"];
    }

	this.makeTabBar();

	this.makePaneContainer();

    this.createPanes();
},


tabBarConstructor:isc.TabBar,

//> @attr tabSet.tabBarProperties (TabBar Properties : null : IR)
// This attribute allows developers to specify custom properties for this tabset's
// +link{tabset.tabBar}
//
// @visibility external
//<

//>	@method	tabSet.makeTabBar()	(A)
//	Instantiates a tabBar for this tabSet, and then adds it as a child of
//	the tabSet. starts with tabBarDefaults and adds additional, tabSet-specific properties
// @visibility internal
//<
makeTabBar : function () {
	if (this.tabs == null) return;

    
    var barPos = this.tabBarPosition,
        tabBarIsVertical = (barPos == isc.Canvas.LEFT || barPos == isc.Canvas.RIGHT),
        align = this.tabBarAlign;

    
    var tabs = this.tabs.duplicate(),
        undef;
    var tabProperties = isc.addProperties({}, this.tabProperties, this.dynamicTabProperties);
    for (var i = 0; i < tabs.length; i++) {
        for (var j in tabProperties) {
            if (tabs[i][j] === undef) tabs[i][j] = tabProperties[j];
        }
    }    

	// assemble tabBar properties
	var tabBarProperties = isc.addProperties({
        // selectTabOnContextClick: we suppress this behavior by default - this is an undocumented
        // flag to allow selection of tabs on context click 
        
        selectTabOnContextClick:this.selectTabOnContextClick,

        ID:this.getID() + "_tabBar",

        // see "fixLayout" method for where this gets updated dynamically at runtime.
        width: (tabBarIsVertical ? this.tabBarThickness : "100%"),
        height: (tabBarIsVertical ? "100%" : this.tabBarThickness),

        // Default the tab bar to having the same accessKey as the tabSet
        accessKey: this.accessKey,

        // If the user has specified a tabIndex for the tabSet, apply it to the tabBar as well
        tabIndex: this.tabIndex,

        // Passes in the user-specified tabs array. 
        // This is a simple way for the developer to specify title / size / etc. for each tab
        // Note - we copy the tabs array rather than pointing at the same array.
        // the tabSet should manage the tabs and call the appropriate actions on the tabBar.
        tabs:tabs,

        align:this.tabBarAlign,
				 
        // tabBar is set vertical or not depending on the value of tabBarPosition.
        vertical: tabBarIsVertical ? true : false,
				 
        // the initially selectedTab is passed in.
        selectedTab:this.selectedTab,

        // More tab settings
        showMoreTab:this.showMoreTab,
        moreTabCount:this.moreTabCount,
        moreTab:this.createMoreTab(),
        // When showing a "more" button, allow buttons to be re-selected.
        allowButtonReselect: this.showMoreTab ? true : false,


        // Override buttonSelected() to fire _tabSelected() on this widget
        // Note: this method is only fired on actual selection change - repeated clicks on
        // the buttons should not fire these methods.
        // _tabSelected will handle firing the public tabSelected/tabDeselected handlers
        // as well as hiding/showing panes.
        // Note that standard TabBar buttonSelected/deselected already handles moving deselected
        // tab behind the baseline image, etc.
		buttonSelected : function (button) {
            
            this.Super("buttonSelected", arguments);
            
            //call _tabSelected() on this tabSet to trigger any selection actions
            if (this.parentElement != null) {
                this.parentElement._tabSelected(button);
            }
        },
        
        // notify the tabset if a tab resizes
        childResized : function (child, deltaX, deltaY, reason) {
            this.Super("childResized", arguments);
            // Don't run 'tabResized' if we're in mid layout.
            
            if (reason == "Overflow on initial draw") {
            
                return;
            }

            if (this.parentElement != null) {
                this.parentElement._tabResized();
            }
        },
        
        // Override showContextMenu -- if this event was bubbled up  a right click on one of our tabs,
        // fire the special showTabContextMenu method
        showContextMenu : function () {
            var target = isc.EH.getTarget();
            if (this.getButtons().contains(target)) {
                var tabSet = this.parentElement,
                    tabObj = tabSet.getTabObject(target);
                if (tabSet.showTabContextMenu(tabSet, tabObj) == false) return false;
            }
            return this.Super("showContextMenu", arguments);
        
        },

        // If drag reordering of tabs is enabled configure the tabbar and
        // trap the notification so we handle the actual reordering
        canReorderItems: this.canReorderTabs,
        reorderOnDrop : !this.canReorderTabs,
        itemDragReordered : function (startPosition, currentPosition) {
            if (this.parentElement != null) {
                this.parentElement.reorderTab(startPosition, currentPosition);
            }
        },

		// other properties
		tabBarPosition:this.tabBarPosition,
        tabBarAlign:this.tabBarAlign,
		autoDraw:false

	}, this.tabBarDefaults, this.tabBarProperties);

    if (this._browserSupportsNativeTouchScrolling) {
        tabBarProperties.overflow = "auto";
        tabBarProperties.overflowStyle = "none";
    }

	// create tabBar and add as child.  NOTE: make available as this.tabBar as well since it's
    // declared as an autoChild.  For the same reason, add a "creator" property
    tabBarProperties.creator = this;
    
    // tabBar is not a real autoChild, so setting showTabBar to false needs special handling -
    // this is used by Calendars to hide the tabset on mobile devices
    if (this.showTabBar == false) tabBarProperties.visibility = "hidden";
    var tb = this.tabBar = this._tabBar = isc.ClassFactory.newInstance(this.tabBarConstructor, tabBarProperties);
    this.addChild(tb);


    // TabBar baseline: If we create a controlLayout, we're truncating the tabBar in order to
    // draw the controlLayout after it.
    // The controlLayout is as thick as the tabs, excluding the baseLine (this is appropriate -
    // we want control buttons to appear above the baseLine). However since the baseLine
    // is written into the tabBar rather than being a direct child of the tabSet, it will be
    // truncated along with the tabs, so the space under the control layout will be empty (the
    // baseLine will not extend underneath the controls).
    // Therefore if we are showing the controlLayout, create a new baseLine image to
    // sit below it so the baseLine extends beyond the (truncated) tabs in the tab-bar.
    // Note that we're not destroying the existing tab-bar baseline
    // (set up via tabBar.makeBaseline) - we're essentially duplicating it with some different
    // defaults and adding it to a different position in the DOM.
    
    
    var tbThickness = (this.tabBarThickness - tb.baseLineThickness),
        snapTo,
        snapOffsetLeft = 0,
        snapOffsetTop = 0,
        baseLineWidth,
        baseLineHeight;
    if (barPos === isc.Canvas.TOP) {
        snapTo = "T";
        baseLineWidth = "100%";
        baseLineHeight = tb.baseLineThickness;
        snapOffsetTop = tbThickness;
    } else if (barPos === isc.Canvas.RIGHT) {
        snapTo = "R";
        baseLineWidth = tb.baseLineThickness;
        baseLineHeight = "100%";
        snapOffsetLeft = -tbThickness;
    } else if (barPos === isc.Canvas.BOTTOM) {
        snapTo = "B";
        baseLineWidth = "100%";
        baseLineHeight = tb.baseLineThickness;
        snapOffsetTop = -tbThickness;
    } else {
        
        snapTo = "L";
        baseLineWidth = tb.baseLineThickness;
        baseLineHeight = "100%";
        snapOffsetLeft = tbThickness;
    }
    this._tabBarBaseLine = tb.createAutoChild("baseLine", {
        width: baseLineWidth,
        height: baseLineHeight,
        vertical: (barPos === isc.Canvas.LEFT || barPos === isc.Canvas.RIGHT),
        skinImgDir:tb.skinImgDir,
        src:tb.baseLineSrc,
        capSize:tb.baseLineCapSize,
        imageType:isc.Img.STRETCH,
        overflow:"hidden", // since the baseline can be a Canvas if it doesn't need to display images
        snapTo: snapTo,
        snapOffsetLeft: snapOffsetLeft,
        snapOffsetTop: snapOffsetTop
    }, isc.StretchImg);
    this.addChild(this._tabBarBaseLine);

    // Always position the tabBarBaseLine behind the tabBar so we only see the edge that protrudes
    // past the end of the tabs.
    this._tabBarBaseLine.moveBelow(tb);
},
// Documented under registerStringMethods
showTabContextMenu:function () {},

createMoreTab : function () {
    if (!this.showMoreTab) return null;

    // Hold onto pane independently of the tab because the pane will change
    // to show tab panes of the selected "more" tab.
    this.moreTabPane = this.createAutoChild("moreTabPane", this.moreTabPaneProperties);
    this.addAutoChild("moreTabPaneNavBar", {title: this.moreTabTitle});
    this.moreTabPaneTable = this.addAutoChild("moreTabPaneTable");
    
    var moreTab = isc.addProperties({
        title: this.moreTabTitle,
        icon: this.moreTabImage,
        pane: this.moreTabPane,
        // Mark more tab so it can be recognized in the tabbar
        moreTab: true
    }, this.moreTabDefaults, this.moreTabProperties);

    
    var undef;
    var tabProperties = isc.addProperties({}, this.tabProperties, this.dynamicTabProperties);

    for (var j in tabProperties) {
        if (moreTab[j] === undef) moreTab[j] = tabProperties[j];
    }
    this.moreTab = moreTab;
    
    return moreTab;
},

rebuildMorePane : function () {
    this.moreTabPane.setData(this.getMorePaneRecords());
},

getMorePaneRecords : function () {
    var tabSet = this,
        records = []
    ;
    for (var i = 0; i < this.tabs.length; i++) {
        var tabButton = this.getTab(this.tabs[i]);
        if (tabButton.isVisible()) continue;
        var tabObject = this.getTabObject(tabButton);

        var icon = (tabObject.icon != null ? isc.Page.getImgURL(tabObject.icon) : null);
        records[records.length] = {
            icon: icon,
            title: tabObject.title,
            pane: tabObject.pane,
            button: tabButton
        };
    }    
    return records;
},

// override setAccessKey and setTabIndex to manage the accessKey / tabIndex of the 
// tab-bar

setTabIndex : function (index) {
    this.Super("setTabIndex", arguments)

    if (this._tabBar != null) this._tabBar.setTabIndex(index);
},

// setAccessKey()
// apply the accessKey to the tabBar, which will in turn apply it to the focus-tab.
setAccessKey : function (accessKey) {
    this.Super("setAccessKey", arguments);
    if (this._tabBar != null) this._tabBar.setAccessKey(accessKey);
},


//>	@method	tabSet.createPanes()
//      converts any tab.pane object literals to canvii
// @visibility internal
//<
createPanes : function () {
    for (var i = 0; i < this.tabs.length; i++) {
	    var tab = this.tabs[i],
			pane = tab.pane
		;
		if (pane == null) continue;
        
        tab.pane = this.createPane(pane, tab);
        
    }
},

//> @attr tabSet.disablePaneWithTab (boolean : true : IRW)
// If true when a tab is enabled or disabled it's pane will also be enabled / disabled.
// @visibility internal
//<

disablePaneWithTab:true,

//>	@method	tabSet.createPane()
//      (Internal method)
//      Given a pane object, create a canvas from it, and prepare it to be made a pane of this
//      object.
//      Creates canvas from properties object.
//      Ensures canvas is deparented / hidden.
//      Returns canvas.
//  @param  pane (object | canvas) object literal / canvas to be made into a pane
// @param tab (object | ImgTab) tab to which the pane is being applied
// @visibility internal
//<
createPane : function (pane, tab) {
    if (pane == null) return pane;

    // handle string name, autoChild, props object
    if (!isc.isA.Canvas(pane)) pane = this.createCanvas(pane);

    if (pane == null) return pane;

    // make sure the pane is hidden before we add it to the pane container - otherwise it will
    // draw before the tab is actually selected
    pane.hide();

    // If the tab is disabled, disable the pane (if appropriate)
    if (this.disablePaneWithTab && tab && tab.disabled) {
        pane.setDisabled(tab.disabled);
    }

    this.paneContainer.ignoreMember(pane);
    
    pane.moveTo(this.isRTL() ? 9999 : -9999, -9999);

    // add the pane as a member to the paneContainer right away.
    
    
    this.paneContainer.addMember(pane);
    pane._containerID = this.ID;
    return pane;
},

makePaneContainer : function () {

    var props = {   
            ID: this.getID() + "_paneContainer",
            _generated: false,
            styleName:this.paneContainerClassName,
            layoutMargin:(this.paneMargin || 0),
            overflow:this.paneContainerOverflow,

            _createEdgedCanvas : function () {
                var edgedCanvas = this.Super("_createEdgedCanvas", arguments);
                edgedCanvas.addMethods({
                    _asymmetricEdgePrefixes:{top:"_top",left:"_left",bottom:"_bottom",right:"_right"},
                    getEdgePrefix : function (edgeName) {
                        var pc = this.eventProxy,
                            tabSet = pc ? pc.creator : null;
                        if (tabSet && !tabSet.symmetricEdges) {
                            return this._asymmetricEdgePrefixes[tabSet.tabBarPosition];
                        }
                    }
                });
                return edgedCanvas;
            }
        };
    // NOTE: these dynamic defaults will override any static defaults defined in
    // this.paneContainerDefaults, (but may be overridden by attributes in
    // this.paneContainerProperties)
    // For back-compat, if showPaneContainerEdges / getPaneContainerCustomEdges() don't have
    // an explicit value, don't apply them to this object so we continue to pick up
    // showEdges/customEdges from the paneContainerDefaults block  
    if (this.showPaneContainerEdges != null) props.showEdges = this.showPaneContainerEdges;        
    if (this.getPaneContainerEdges && this.getPaneContainerEdges() != null) {
        props.customEdges = this.getPaneContainerEdges();
    }
    // asymmetricEdges needs support for asymmetric edge sizes and offsets

    if (!this.symmetricEdges) {
        var sizes = this[this._asymmetricEdgeSizePropertyMap[this.tabBarPosition]];
        if (sizes && sizes.defaultSize != null) props.edgeSize = sizes.defaultSize;
        if (sizes && sizes.bottom != null) props.edgeBottom = sizes.bottom;
        if (sizes && sizes.top != null) props.edgeTop = sizes.top;
        if (sizes && sizes.left != null) props.edgeLeft = sizes.left;
        if (sizes && sizes.right != null) props.edgeRight = sizes.right;

        var offsets = this[this._asymmetricEdgeOffsetPropertyMap[this.tabBarPosition]];
        if (offsets && offsets.defaultSize != null) props.edgeOffset = offsets.defaultSize;
        if (offsets && offsets.bottom != null) props.edgeOffsetBottom = offsets.bottom;
        if (offsets && offsets.top != null) props.edgeOffsetTop = offsets.top;
        if (offsets && offsets.left != null) props.edgeOffsetLeft = offsets.left;
        if (offsets && offsets.right != null) props.edgeOffsetRight = offsets.right;
    }

    this.addAutoChild("paneContainer", props);
},

// For efficiency avoid assembling asymmetric edge size / offset property names on the fly
_asymmetricEdgeSizePropertyMap : {
    top:"topEdgeSizes", bottom:"bottomEdgeSizes", left:"leftEdgeSizes", right:"rightEdgeSizes"
},
_asymmetricEdgeOffsetPropertyMap : {
    top:"topEdgeOffsets", bottom:"bottomEdgeOffsets", left:"leftEdgeOffsets",
    right:"rightEdgeOffsets"
},

//> @attr tabSet.showPartialEdges (Boolean : false : [IRA])
// If the paneContainer for this tab set is showing +link{Canvas.showEdges,edges}, setting this
// attribute to <code>true</code> will set the paneContainer to show
// +link{canvas.customEdges,customEdges} for the three sides opposing the tabBarPosition.
// @visibility external
//<
 
//>	@method tabSet.getPaneContainerEdges() [A]
// If the paneContainer for this tab set is showing +link{Canvas.showEdges,edges}, this 
// method can be used to specify (dynamically) which +link{canvas.customEdges,customEdges} to
// show. Called when the pane creator is created.
// <P>
// Default implementation will return null unless +link{tabSet.showPartialEdges,showPartialEdges}
// is true, in which case it will return the three edges opposite the
// +link{tabSet.tabBarPosition,tabBarPosition}.
// @return (array) array of custom edges to show
// @visibility external
//<
getPaneContainerEdges : function () {
    if (this.showPartialEdges) {
                if (this.tabBarPosition == "bottom") return ["T","L","R"];
                else if (this.tabBarPosition == "left") return ["T","B","R"];
                else if (this.tabBarPosition == "right") return ["T","B","L"];
                else return ["B","L","R"];
    }
    return null;
},

// override draw to make sure we have a tab selected, and to fire 'tabSelected()' on the tab
draw : function (a,b,c,d) {
    if (this.tabs && this.tabs.length > 0) {    
        var selectedTab = this.getSelectedTabNumber();
        // Don't allow a bad selectedTab value to persist.
        if (!isc.isA.Number(selectedTab) || selectedTab < 0) selectedTab = this.selectedTab = 0;
        // Ensure it's selected in the tab-bar - will no op if already selected, otherwise
        // will perform selection and fire our handlers
        this._tabBar.selectTab(selectedTab);
    }
    this.invokeSuper(isc.TabSet, "draw", a,b,c,d);
    this.fixLayout();
},

//>	@method	tabSet.setTabTitle()	(A)
// Changes the title of a tab
// @param	tab      (Tab | number | ID | name)
// @param	title    (HTML)  new title
// @visibility external
// @example titleChange
//<
setTabTitle : function (tab, title) {
    this.getTabObject(tab).title = title;
    this.getTab(tab).setTitle(title);
    // reset the menu to pick up the new title
    this.resetTabPickerMenu();
},

//>	@method	tabSet.setTabIcon() (A)
// Changes the icon for a tab
// @param tab (Tab | number | ID | name) tab to update
// @param icon (SCImgURL) new icon
// @visibility external
//<
setTabIcon : function (tab, icon) {
    this.setTabProperties(tab, {icon:icon});
},

//>@method tabSet.enableTab()  
// If the specified tab is disabled, enable it now.
// @param   tab (Tab | number | ID | name)
// @see tab.disabled
// @visibility external
//<
enableTab : function (tab) {
    this.setTabDisabled(tab, false);
},

//>@method tabSet.disableTab()  
// If the specified tab is enabled, disable it now.
// @param   tab (Tab | number | ID | name)
// @see tab.disabled
// @visibility external
//<
disableTab : function (tab) {
    this.setTabDisabled(tab, true);
},

//>@method tabSet.setTabProperties() (A)
// Apply properties to an existing tab in a tabSet.
// @param tab (Tab | number | ID | name) Identifier for the tab to be modified
// @param properties (object) Javascript object containing the set of properties to be applied
//  to the tab.
// @visibility external
//<
setTabProperties : function (tab, properties) {
    if (!properties) return;
    
    if (properties.ID != null) {
        this.logWarn("setTabProperties(): Unable to modify ID for an existing tab - ignoring " +
                    "this property");
        delete properties.ID;
    }
    
    // A couple of properties require special APIs
    if (properties.pane != null) {
        this.updateTab(tab, properties.pane);
        delete properties.pane;
    }
    if (properties.disabled != null) {
        this.setTabDisabled(tab, properties.disabled);
        delete properties.disabled;
    }
    
    var tabObject = this.getTabObject(tab),
        tab = this.getTab(tab);
    if (!tabObject) return;
    isc.addProperties(tabObject, properties);
    
    if (tab) {
        tab.setProperties(properties);
    }
    
    // If we have a pickerMenu, destroy it so it gets rebuilt when next required
    // Ensures we pick up title / icon etc changes
    this.resetTabPickerMenu();
},

// Actually set the disabled property on a tab. Handled by just disabling the button.
setTabDisabled : function (tab, disabled) {
    var tabObject = this.getTabObject(tab);
    if (tabObject) tabObject.disabled = disabled;
    
    var tab = this.getTab(tab);
    if (tab) {
        // disable the tab so you can't access it.
        tab.setDisabled(disabled);
        // Also disable the pane in case it's showing.
        // Alternative approach would be to deselect the tab, if selected. The problem with 
        // this is we may only have one tab in the tabSet.
        var pane = tab.pane;
        if (pane && this.disablePaneWithTab) {
            if (isc.isA.Canvas(pane)) pane.setDisabled(disabled);
            else pane.disabled = disabled;
        }
    }
    // rebuild the picker menu so the item in question shows up disabled
    this.resetTabPickerMenu();
},

//>	@method	tabSet.addTab()	(A)
// Add a tab
// @param	tab      (Tab)   new tab
// @param	[position] (number)  position where tab should be added
// @see TabSet.addTabs
// @visibility external
// @example tabsAddAndRemove
//<
addTab : function (tab, position) {
    return this.addTabs(tab, position);
},

//>	@method	tabSet.addTabs()	(A)
// Add one or more tabs
// @param	tabs      (Tab or Array of Tab)   new tab or tabs
// @param	position (number)  position where tab should be added (or array of positions)
// @see TabSet.addTab
// @visibility external
//<
addTabs : function (newTabs, position) {
    if (!isc.isAn.Array(newTabs)) newTabs = [newTabs];
    var oldSelectedTab = this.getTabObject(this.getSelectedTabNumber()),
        forceSelection = (this.getSelectedTabNumber() == -1);
    
    if (position == null || position > this.tabs.length) position = this.tabs.length;
    for (var i = 0; i < newTabs.length; i++) {
        // use 'createPane' to turn the pane into a hidden, deparented canvas.
        newTabs[i].pane = this.createPane(newTabs[i].pane, newTabs[i]);
        
        // apply tabProperties (see comment in makeTabBar)
        var undef;
        var tabProperties = isc.addProperties({}, this.tabProperties, this.dynamicTabProperties);
        
        for (var propName in tabProperties) {
            if (newTabs[i][propName] === undef) {
                newTabs[i][propName] = tabProperties[propName];
            }
        }
        
        // Actually add the tab to the config
        this.tabs.addAt(newTabs[i], (position + i))
    }
    this._tabBar.addTabs(newTabs, position);
    
    // If we have a pickerMenu, destroy it so it gets rebuilt when next required
    this.resetTabPickerMenu();
    
    // call fixLayout on a delay
    // Necessary in case the new tabs introduced clipping of the tab-bar
    // Delay required as layout reflow is asynch
    this.delayCall("fixLayout");
    
    if (forceSelection) {
        // If we didn't have a selected tab at the start of this method, ensure we select the
        // first of the new tabs
        this.selectTab(0);
    } else {
        // otherwise, update this.selectedTab (an index) in case tabs were added before the old
        // selected tab
        this.selectedTab = this.getTabNumber(oldSelectedTab);
    }
    
    //>EditMode
    if (this.editProxy) this.editProxy.addTabsEditModeExtras(newTabs);
    //<EditMode
    
    
    return position;
},

//> @method tabSet.setTabPane()
// Apply a new +link{tab.pane,pane} to an existing tab in this tabSet
// @param tab (number | string | Tab) Tab to update (may be referenced by ID or index)
// @param pane (Canvas) new Pane for the tab
// @visibility external
//<
setTabPane : function (tab, pane) {
    return this.updateTab(tab,pane);
},

//> @attr tabSet.destroyPanes (boolean : null : IR)
// Whether +link{canvas.destroy,destroy()} should be called on +link{tab.pane} when it a tab is
// removed via +link{removeTab()}.  
// <P>
// With the default setting of <code>null</code> panes will be automatically destroyed.
// An application might set this to false in order to re-use panes in different tabs or in
// different parts of the application.
//
// @visibility external
//<

//>	@method	tabSet.removeTab()	(A)
// Remove a tab.
// <P>
// The pane associated with the removed tab is automatically destroyed when you
// call this method.  To avoid this, call +link{updateTab()} with <code>null</code> as the new
// pane immediately before removing the tab, or set +link{tabSet.destroyPanes} to false.
// 
// @param	tabs      (Tab | ID | name | number | Array of Tab)  list of tabs, tabIDs, or tab numbers
// 
// @see TabSet.removeTabs
// @visibility external
// @example tabsAddAndRemove
//<
removeTab : function (tab, dontDestroy) {
    return this.removeTabs(tab, dontDestroy);
},

//>	@method	tabSet.removeTabs()	(A)
// Remove one or more tabs.  The pane(s) associated with the removed tab(s) is automatically
// destroyed when you call this method.
//
// @param	tabs      (Tab | ID | name | number)   list of tabs, tabIDs, tab names, or tab numbers
//
// @see TabSet.removeTab
// @visibility external
//<
removeTabs : function (tabs, dontDestroy) {
    if (!isc.isAn.Array(tabs)) tabs = [tabs];
    
    // get the actual tab button object from whatever was passed in.
    // We can pass this to tabBar.removeTabs()
    tabs = this.map("getTab", tabs);
    
    var removedSelected = false,
        selectedTab = this.getSelectedTab(),
        autoSelectTab = 0;
    
    for (var i = 0; i < tabs.length; i++) {
        
        // remove the tab from the config
        var tab = tabs[i],
            index = this.getTabNumber(tab)
        ;
        if (index == -1) continue; // can't find specified tab

        var tabObject = this.tabs[index];
        
        // if we remove the selected tab we want to just select another one near it
        if (tabObject == selectedTab) {
            removedSelected = true;
            // auto-select the next tab to the left if there is one, or the current 
            // index otherwise
            if (index > 0) autoSelectTab = index - 1;
            else if (index < this.tabs.length + 1) autoSelectTab = index;
            
        // otherwise we may need to update our internal 'selectedTab' index value
        // to reflect the new position of the already selected tab
        } else {
            if (index < this.selectedTab) {
                this.selectedTab -= 1;
            }
        }

        this.tabs.removeAt(index);

        if (tabObject) {
            // remove the pane
            var pane = tabObject.pane;
            if (pane != null && pane.parentElement === this.paneContainer) {
                this.paneContainer.removeChild(pane);
                if (!dontDestroy && this.destroyPanes !== false) {
                    pane.destroy();
                }
            }
        }
        // remove the tab button
        this._tabBar.removeTabs(tab);
    }
    
    // if the selected tab was removed, select the first tab if we have any
    if (removedSelected && this.tabs.length > 0) {
        // if the new selected-tab index is beyond the tab-count, select the last tab
        if (autoSelectTab >= this.tabs.length) autoSelectTab = this.tabs.length - 1;
        this.selectTab(autoSelectTab);
    }
    // If we have a pickerMenu, destroy it so it gets rebuilt when next required
    this.resetTabPickerMenu();

    // call fixLayout on a delay
    // Necessary in case the removed tabs get rid of clipping of the tab-bar
    // Delay required as layout reflow is asynch
    this.delayCall("fixLayout", 0);
    
    //>EditMode
    if (this.editProxy) this.editProxy.removeTabsEditModeExtras();
    //<EditMode

},

//>	@method	tabSet.reorderTab()
// Move a tab to another location in the tabset.
// @param tab (Tab | ID | name | number) tab to move
// @param [moveToPosition] (number) the index to move the tab to - defaults to the end of the
//                                  tabset if not passed
// @visibility external
//<
reorderTab : function (tab, moveToPosition) {
    if (moveToPosition == null || moveToPosition > this.tabs.length) moveToPosition = this.tabs.length;

    var tab = this.getTab(tab);
    if (tab) {
        var index = this.getTabNumber(tab);
        if (index == moveToPosition) return;

        var tabObject = this.getTabObject(tab),
            selectedTab = this.getSelectedTab()
        ;

        // Move the tab button
        this._tabBar.reorderTab(tab, moveToPosition);

        // Resync our matching tab list and selected tab
        this.tabs.removeAt(index);
        this.tabs.addAt(tabObject, moveToPosition);
        if (this.selectedTab == index) {
            this.selectedTab = moveToPosition;
        } else if (index < this.selectedTab && this.selectedTab <= moveToPosition) {
            this.selectedTab--;
        } else if (index > this.selectedTab && this.selectedTab >= moveToPosition) {
            this.selectedTab++;
        }

        // If we have a pickerMenu, destroy it so it gets rebuilt when next required
        this.resetTabPickerMenu();

        // call fixLayout on a delay
        // Necessary in case the new tabs introduced clipping of the tab-bar
        // Delay required as layout reflow is asynch
        this.delayCall("fixLayout");
        
        //>EditMode
        if (this.editProxy) this.editProxy.reorderTabsEditModeExtras(index, moveToPosition);
        //<EditMode

        this.tabsReordered();
    }
    
},

//> @method tabSet.canCloseTab()
// Returns true if this tab is closeable. Determined by checking +link{tab.canClose} and
// +link{tabSet.canCloseTabs}.
// @param tab (int | ID | name | Tab) tab to check
// @return (boolean) true if tab is closeable
//<
canCloseTab : function (tab) {
    tab = this.getTabObject(tab);
    if (tab && tab.canClose != null) return tab.canClose;
    return !!this.canCloseTabs;
},

//> @method tabSet.setCanCloseTab()
// Sets the given tab's +link{tab.canClose,canClose} property to the boolean parameter canClose.
// If canClose is null, this will have the effect of causing the tab to fall back on +link{tabSet.canCloseTabs}.
// @param tab (Tab | ID | name | number) tab to change
// @param canClose (boolean) new value for the tab's canClose property, or null to clear it
// @visibility external
//<
setCanCloseTab : function (tab, canClose) {
    tab = this.getTabObject(tab);
    var liveTab = this.getTab(tab);
    tab.canClose = canClose;
    if (liveTab) {
        liveTab.setProperties(this.getTabBar().getCloseIconProperties(tab, this.canCloseTab(tab)));
    }
},

//> @method tabSet.setCanCloseTabs()
// Changes this TabSet's +link{TabSet.canCloseTabs,canCloseTabs} property.
// @param canCloseTabs (boolean) the new value for canCloseTabs.
// @visibility external
//<
setCanCloseTabs : function (canCloseTabs) {
    canCloseTabs = !!canCloseTabs;
    this.canCloseTabs = canCloseTabs;

    var tabs = this.tabs;
    if (!tabs) return;

    // Go through each tab, updating the tab buttons whose corresponding tab object has an
    // unspecified or null canClose property.
    var tb = this.getTabBar();
    for (var i = 0, len = tabs.length; i < len; ++i) {
        var tab = tabs[i];
        if (tab.canClose != null) continue;

        var liveTab = this.getTab(tab);
        if (liveTab) {
            liveTab.setProperties(tb.getCloseIconProperties(tab, canCloseTabs));
        }
    }
},

setCanReorderTabs : function (canReorderTabs) {
    this.canReorderTabs = canReorderTabs;
    this.tabBar.canReorderItems = canReorderTabs;
    this.tabBar.reorderOnDrop = !canReorderTabs;
},

_tabIconClick : function(tab) { 
    var shouldClose = this.canCloseTab(tab);
    if (shouldClose) {
        this.closeClick(tab);
        return false;
    } else return this.tabIconClick(tab); 
    
},

//> @method tabSet.closeClick()
// When +link{canCloseTabs} is set, method fired when the user clicks the "close" icon for a
// tab.
// <P>
// Default implementation will remove the tab from the tabSet via +link{removeTab()}.
//
// @param tab (Tab) tab to close
// @visibility external
//<
closeClick : function (tab) {
    // if "onCloseClick" exists, allow it to cancel the default behavior
    
    if (this.onCloseClick && (this.onCloseClick(tab) == false)) {
        return;
    }
    this.removeTab(tab);
},

//> @method tabSet.tabIconClick()
// Method fired when the user clicks the icon for a tab, as specified via +link{tab.icon}.
// <P>
// Default behavior will fire <code>icon.click()</code> if specified, with two parameters
// <code>tab</code> (a pointer to the tab object and <code>tabSet</code> a pointer to the tabSet
// instance.
// @param tab (Tab) with click handler being fired
// @visibility external
//<
tabIconClick : function (tab) {
    var icon = tab.icon;
    if (icon && icon.click) return this.fireCallback(icon.click, 'tab,tabSet', [tab,this]);
},

//> @method tabSet.getTabObject()
// Get the tab Object originally passed to +link{tabSet.tabs}, by index, name or ID.
// If passed a tab Object, just returns it.
//
// @param	tab   (int | ID | name | Tab)
// @return (Tab) the tab, or null if not found
// @visibility external
//<
// NOTE: this returns the tab configuration object, not the button, since there may not be a
// Button.
getTabObject : function (tab) {
    // passed the tab button - determine it's index (use this below)
    tab = this.getTabNumber(tab);
    if (tab >= this.tabs.length) {
        var button = this.tabBar.getButton(tab);
        if (button && button.moreTab) return this.moreTab;
    }
    return this.tabs[tab];
},

//> @method tabSet.getTab()
// Get the live Canvas representing a tab by index, ID, reference, or name.  
// If passed a tab Canvas, just returns it.
// <P>
// Note that live Tab instances are not available until +link{Canvas.draw,draw()}.
// <P>
// The returned Tab is considered an internal component of the TabSet.  In order to maximize
// forward compatibility, manipulate tabs through APIs such as a +link{setTabTitle()} instead.
// Also note that a super-lightweight TabSet implementation may not use a separate Canvas per
// Tab, and code that accesses an manipulates Tabs as Canvases won't be compatible with that
// implementation.
//
// @param	tab   (int | ID | name | Canvas)
// @return (Tab) the tab Canvas, or null if not found or TabSet not drawn yet
//
// @visibility external
//<
getTab : function (tab) {
    
    // already the tab button, return it
    if (isc.isAn.Canvas(tab)) return tab;

    if (!this.tabs) return null;

    // if we have a tab-config block, convert it to an index, since the tabBar doesn't see our 
    // 'tabs' array
    if (this.tabs.contains(tab)) tab = this.tabs.indexOf(tab);

    // getButton on the tabBar handles the various possible types of the tab identifier passed in
    tab = this.getTabBar().getButton(tab);
    
    return tab;
},

//> @method tabSet.getTabPane()
// Returns the pane for a given tab.
//
// @param	tab   (object | number | ID | name | Tab)
// @return (Canvas) the tab pane
// @visibility external
//<
getTabPane : function (tab) {
    return this.getTabObject(tab).pane;
},

//> @method tabSet.findTab()
// Returns a the first tab in the list that matches the user-passed property name/value pair.
//
// @param	propertyName   (String) name of the property to look for
// @param	propertyValue  (Any) value of the property
//<
findTabObject : function (propertyName, propertyValue) {
    return this.tabs.find(propertyName, propertyValue);
},

//> @method tabSet.getTabNumber()
// Get the index of a tab, from the tab, tab ID or tab name.  If passed a number, just returns it.
// @param	tab   (number | ID | name | tab)
// @return (number) the index of the tab, or -1 if not found 
// @visibility external
//<
// Note - we don't call this 'getTabIndex', even though it is an index, because of the conflict
// with the 'tabIndex' of the widget as a whole
getTabNumber : function (tab) {
    if (isc.isA.Number(tab)) return tab;
    if (!this.tabs) return null;
    var index = this.tabs.indexOf(tab);
    if (index != -1) return index;
    
    
    if (isc.isA.String(tab)) {
        var index = this.tabs.findIndex("name", tab);
        if (index == -1) index = this.tabs.findIndex("ID", tab);
        return index;
    }
    
    // At this point it must be a pointer to the tab button, so fall through to 
    // tabBar.getButtonNumber()
    return this.getTabBar().getButtonNumber(this.getTab(tab));
},

//> @method tabSet.updateTab()
// Set the pane for a tab.
// <P>
// Pass in the index of a tab (or a tab object), and a new pane.
// <P>
// NOTE: the old pane for the tab is not destroy()d
// 
// @param	tab   (number | ID | name | Tab) tab to update
// @param	pane  (Canvas | ID) new pane for the tab
// @visibility external
//<
updateTab : function (tab, pane) {
    // if we were passed a tab init block, for a new tab, call addTabs instead
    if (isc.isAn.Object(tab) && !isc.isA.Canvas(tab) &&
        this.tabs.indexOf(tab) == -1) 
    {
        if (pane != null) tab.pane = pane;
        return this.addTabs(tab);
    }

    // get the index for the tab (whatever way the "tab" is passed)
    var tabIndex = this.getTabNumber(tab);
    // bad tab specification
    if (tabIndex == -1) {
        this.logWarn("no such tab: " + this.echo(tab));
        return;
    }

    // get rid of the old pane
    var tabObject = this.getTabObject(tabIndex),
        oldPane = tabObject ? tabObject.pane : null;

    if (tabObject != null && tabObject.pane === pane) return; // no-op

    if (oldPane != null) {
        oldPane.hide();
        oldPane.deparent();
    }

    // NOTE: keep tabCanvas.pane and tabObject.pane in sync for EditMode where the Tab needs to
    // be able to respond to getProperty("pane")
    var tabCanvas = this.getTab(tab);

    // if the new pane is null, we're done
    if (pane == null) {
        if (tabCanvas != null) tabCanvas.pane = null;
        return tabObject.pane = null;
    }

    // add the new pane to init block (Using createPane to instantiate as a Canvas if necessary)
    // this makes sure the pane is hidden and not a child of anything except the paneContainer    
    pane = tabObject.pane = this.createPane(pane, tabObject);

    // tabCanvas won't exist if we're not drawn yet
    if (tabCanvas != null) tabCanvas.pane = pane;

    // if the currently visible tab is being updated, ensure the new pane is
    // a member of the paneContainer with the appropriate visibility
    // (If undrawn it'll show up when the tabSet as a whole gets drawn)
    if (this.getSelectedTabNumber() == tabIndex) {
        if (!this.paneContainer.hasMember(pane)) {
            this.paneContainer.addMember(pane);
        // We may have added as a member and suppressed the draw due to the
        // "ignoreMember" logic in createPane - if so stop ignoring - will force
        // a reflow / draw.
        } else if (this.paneContainer.isIgnoringMember(pane)) {
            this.paneContainer.stopIgnoringMember(pane);
        }
        pane.setVisibility(isc.Canvas.INHERIT);
    }
},

//>	@method	tabSet.fixLayout()	(A)
//			lay out the children of the tabSet. 
//			this method takes into account the position of the tabBar in the tabSet, 
//			and lays out the tabBar and the paneContainer accordingly.
//<
fixLayout : function () {
	// abbreviations
	var tb = this._tabBar,
        // round corners: for layout only, manipulate the edgedCanvas instead of the
        // paneContainer
		pc = this._edgedCanvas || this.paneContainer
	;

	// check for nulls, and exit if found.
	// this method requires that both the tabBar and the paneContainer be instantiated before
	// it is called.
	if (tb == null || pc == null) return;

	// make sure paneContainer is below _tabBarBaseLine
    if (pc.getZIndex(true) >= this._tabBarBaseLine.getZIndex(true)) pc.moveBelow(this._tabBarBaseLine);

    
    var tbOverlap = this._firstNonNull(this.tabBarOverlap, tb.borderThickness,
                                       tb.baseLineThickness);

	// lay out the tabBar and paneContainer, depending on where the tabBar is.
    var vertical;
    switch (this.tabBarPosition) {
    	case isc.Canvas.TOP :
            vertical = false;
    		pc.setRect(0, 
                       tb.getHeight() - tbOverlap,
                       this.getWidth(),
                       this.getHeight() - tb.getHeight() + tbOverlap
                      );
	        break;
        case isc.Canvas.BOTTOM :
            vertical = false;
            tb.setTop(this.getHeight() - tb.getHeight());
	    	pc.setRect(0,
                       0,
                       this.getWidth(), 
                       this.getHeight() - tb.getHeight() + tbOverlap
                      );
            break;
        case isc.Canvas.LEFT :
            vertical = true;
    		pc.setRect(tb.getWidth() - tbOverlap,
                       0,
                       this.getWidth() - tb.getWidth() + tbOverlap,
                       this.getHeight()
                      );
            break;
        case isc.Canvas.RIGHT :
            vertical = true;
    		tb.setLeft(this.getWidth() - tb.getWidth());
	    	pc.setRect(0,
                       0,
                       this.getWidth() - tb.getWidth() + tbOverlap,
                       this.getHeight()
                      );
            break;
    }

    // showControls will show (or hide) the control layout, and return true if showing.
    var showControls = this.showControls();
    
    // If we're showing the control layout adjust our tab-bar size to take it into account
    if (showControls) {
        // Force clipping so we can scroll the tb as expected
        // Required even if we were already showing the scroller - we may have resized
        if (vertical) tb.setHeight(this.getViewportHeight() - this.tabBarControlLayout.getHeight());
        else {
            tb.setWidth(this.getViewportWidth() - this.tabBarControlLayout.getWidth());
            if (this.isRTL()) tb.setLeft(this.tabBarControlLayout.getWidth());
        }        
        this.tabBarControlLayout.bringToFront();
    } else {
        tb.resizeTo(vertical ? null : "100%", vertical ? "100%" : null);
        if (this.isRTL() && !vertical) {
            tb.setLeft(0);
        }
    }
    // If the tab bar is currently scrolled, but there is enough space to display all its
    // tabs, force a scroll back to zero/zero
    
    var totalTabs = this._getTabSizes();
    if (vertical) {
        if (tb.getScrollTop() > 0 && totalTabs <= tb.getViewportHeight()) tb.scrollTo(null,0,"descrollTabs");
    } else {
        if (tb.getScrollLeft() > 0 && totalTabs <= tb.getViewportWidth()) tb.scrollTo(0,null,"descrollTabs");
    }
},

//>@method  tabSet.shouldShowControl()
// Should a specific control as specified in +link{tabSet.tabBarControls} be displayed?
// Default implementation will evaluate the +link{Canvas.showIf()} property for custom controls
// included as canvases. Standard controls for scrolling the tabBar will be included if 
// the relevant +link{tabSet.showTabScroller} or +link{tabSet.showTabPicker} property is not
// false, and there is not enough space in the tab-bar to display all the tabs.
// @parameter (control) control from the +link{tabSet.tabBarControls} array
// @return  (boolean)   true if the control shoudl be displayed
// @group tabBarControls
//<

shouldShowControl : function (control) {
    // The standard controls only show if the tabs are clipped
    if ((control == "tabScroller") || (control == "tabPicker")) {
        if (this.showMoreTab) return false;
        if (!this.showTabScroller && control == "tabScroller") return false;
        if (!this.showTabPicker && control == "tabPicker") return false;
        // If the member width exceeds the available space for the tab-bar we need to show
        // scroller buttons 
        var contentSize = this._getTabSizes();
        if (contentSize == 0) return false;

        
        var otherControlSize=0;
        for (var i = 0; i < this.tabBarControls.length; i++) {
            var otherControl = this.tabBarControls[i];
            if (otherControl == "tabScroller" || otherControl == "tabPicker") continue;
            if (this.shouldShowControl(otherControl)) {
                if (!isc.isA.Canvas(otherControl)) otherControl = this.getControl(otherControl);
                otherControlSize += vertical ? otherControl.getVisibleHeight() : otherControl.getVisibleWidth();
            }
        }

        var vertical = (this._tabBar.orientation == isc.Layout.VERTICAL),
            clipTabs = (contentSize > (vertical ? (this.getViewportHeight() - otherControlSize)
                                                : (this.getViewportWidth() - otherControlSize)));                                       
        return clipTabs;
    }

    var control = this.getControl(control);

    if (isc.isA.Canvas(control) &&
        !this.tabBarControlLayout._shouldIgnoreMember(control))
    {
        return true;
    }
    return false;
},

_getTabSizes : function () {
    if (!this._tabBar) return 0;
    var contentSize = this._tabBar.getMemberSizes(),
        vertical = this._tabBar.vertical;
    if (contentSize == null || contentSize.length == 0) return 0;
    
    contentSize = contentSize.sum();
        
    
    var sizeAdjustment = (vertical ? (this._tabBar._topMargin || 0) + (this._tabBar._bottomMargin || 0)
                                  : (this._tabBar._leftMargin || 0) + (this._tabBar._rightMargin || 0));
    return contentSize + sizeAdjustment;
},

scrollerBackHMarginSize: 0,
scrollerBackVMarginSize: 0,
scrollerForwardHMarginSize: 0,
scrollerForwardVMarginSize: 0,

//> @method tabSet.getScrollerBackImgName() (A)
// Returns the +link{StretchItem.name} to use for the back button part of the <code>"tabScroller"</code>
// standard control.
// @return (String) scrollerBackImg name
// @see TabSet.scroller
//<
getScrollerBackImgName : function () {
    return this.symmetricScroller ? "back" : this.tabBarPosition + "_back";
},

//> @method tabSet.getScrollerForwardImgName() (A)
// Returns the +link{StretchItem.name} to use for the forward button part of the <code>"tabScroller"</code>
// standard control.
// @return (String) scrollerForwardImg name
// @see TabSet.scroller
//<
getScrollerForwardImgName : function () {
    return this.symmetricScroller ? "forward" : this.tabBarPosition + "_forward";
},

//> @method tabSet.getTabPickerSrc() (A)
// Returns the +link{ImgButton.src} to use for the +link{TabSet.tabPicker} button.
// @return (SCImgURL) URL of the tabPicker's src.
//<
getTabPickerSrc : function () {
    var vertical = (this._tabBar.orientation == isc.Layout.VERTICAL);
    if (this.symmetricPickerButton) {
        return vertical ? this.pickerButtonVSrc : this.pickerButtonHSrc;
    } else {
        return this.pickerButtonSrc;
    }
},

//>@method  tabSet.getControl()
// Given an entry in the +link{tabSet.tabBarControls} array, this method will return a pointer
// to the actual widget to display in the control layout.<br>
// If passed a canvas, it will be returned intact.<br>
// Will also map the special strings <code>"tabPicker"</code> and <code>"tabScroller"</code> to
// standard tab picker and scroller controls.
// @param control (string or canvas)    Control from +link{tabSet.tabBarControls} array.
// @return (canvas) Control widget to include in the control layout for this tabset
// @group tabBarControls
//<

getControl : function (control) {
    if (isc.isA.Canvas(control)) return control;
    var vertical = (this._tabBar.orientation == isc.Layout.VERTICAL);

    if (control == "tabScroller") {
        if (!this.scroller) {

            // Make the scroller a stretchImgButton with 2 "buttons"
            var sbsize = this.scrollerButtonSize;

            var scrollerSrc;
            if (this.symmetricScroller) {
                scrollerSrc = vertical ? this.scrollerVSrc : this.scrollerHSrc;
            } else {
                scrollerSrc = this.scrollerSrc;
            }
            var backName = this.getScrollerBackImgName(),
                forwardName = this.getScrollerForwardImgName();

            this.scroller = this.createAutoChild("scroller", {
                vertical:vertical,
                width:vertical ? (this.tabBarThickness - this._tabBar.baseLineThickness) : (2*sbsize),
                height:vertical ? (2*sbsize) : (this.tabBarThickness - this._tabBar.baseLineThickness),
                items:[isc.addProperties({name:this.getScrollerBackImgName(),
                              width:vertical ? null : sbsize - this.scrollerForwardHMarginSize,
                              height:vertical ? sbsize - this.scrollerForwardVMarginSize : null}, this.scrollerBackImg),
                       isc.addProperties({name:this.getScrollerForwardImgName(),
                              width:vertical ? null : sbsize - this.scrollerBackHMarginSize,
                              height:vertical ? sbsize - this.scrollerBackVMarginSize : null}, this.scrollerForwardImg)],
                scrollerPosition:this.tabBarPosition,
                skinImgDir:this.skinImgDir,

                src:scrollerSrc,

                backPartName:backName,
                forwardPartName:forwardName
            }, this.scrollerProperties);
        }

        return this.scroller;

    } else if (control == "tabPicker") {
        var tabPickerSize = (isc.Browser.isTouch ? this.touchPickerButtonSize : this.pickerButtonSize);
        if (!this.tabPicker) {
            var tabSrc = this.getTabPickerSrc();
            this.tabPicker = this.createAutoChild("tabPicker", {
                // use customState to append the tab bar position if necessary
                customState:this.symmetricPickerButton ? null : this.tabBarPosition,
                pickerPosition:this.tabBarPosition,
                skinImgDir:this.skinImgDir,
                src:tabSrc,
                height:(vertical ? tabPickerSize : (this.tabBarThickness - this._tabBar.baseLineThickness)),
                width:(vertical ? (this.tabBarThickness - this._tabBar.baseLineThickness) : tabPickerSize)
            });
        }

        return this.tabPicker;
    }

    // If the control is a string, check for it being a widget's global ID
    if (isc.isA.String(control) && isc.isA.Canvas(window[control])) return window[control];

    // At this point we don't recognize the controller - log a warning and bail
    this.logWarn("Unable to resolve specified tabBarControl:" + isc.Log.echo(control) + 
                   " to a valid control. Not displaying.");
    return null;
},

// For autoTest: if we are showing tabBarControlLayout, access it directly by name
namedLocatorChildren:["tabBarControlLayout"],

// Method to actually show the controlLayout if required.
// If no controls are to be displayed this method falls through to hideControls()
// Returns true if any controls are displayed, false otherwise
showControls : function () {
    var controlSet = this.tabBarControls,
        controlSize = 0,
        barPos = this.tabBarPosition, 
        vertical = barPos == isc.Canvas.RIGHT || barPos == isc.Canvas.LEFT,
        visibleControlIndex = 0;

    var controlLayout = this.tabBarControlLayout;
    // controls should all be housed in a layout
    if (!controlLayout) {
        // create the tabBarControls as an autoChild
        this.tabBarControlLayout = controlLayout =
                                   this.createAutoChild("tabBarControlLayout",
                                   {styleName:this.tabBarControlLayoutDefaults.styleName ||
                                              this.tabBar.styleName,
                                    _shouldIgnoreMember : function (control) {
                                        if (this.Super("_shouldIgnoreMember", arguments)) return true;
                                        if (control.showIf) return !control.fireCallback(control.showIf, [control]);
                                        return false;
                                    },
                                    // if a control is resized while visible, ensure the tabSet 
                                    // is notified so it can keep us right-aligned in the tab-bar
                                    childResized : function () {
                                        this.Super("childResized", arguments);
                                        this.creator._controlLayoutChildResized();
                                    },
                                    // if the visibility of a tabBar control changes, re-layout
                                    // the tabBarControlLayout
                                    childVisibilityChanged : function (child) {
                                        this.Super("childVisibilityChanged", arguments);
                                        this.creator._controlLayoutChildResized();
                                    },
                                    vertical:vertical

                                    // For autoTest APIs
                                    ,locatorParent:this
                                   });
    }

    for (var i = 0; i< controlSet.length; i++) {
        var control = controlSet[i],
            shouldShowControl = this.shouldShowControl(control);
        // Turn the control identifier into a pointer to a Canvas if necessary
        control = this.getControl(control);
        if (!control) continue;

        if (!shouldShowControl && (control == this.scroller || control == this.tabPicker)) {
            continue;
        }

        // At this point the control should be a pointer to a canvas -
        // Ensure the layout is showing, and that the control shows up in the right spot
        if (controlLayout.getMemberNumber(control) != visibleControlIndex) {
            controlLayout.addMember(control, visibleControlIndex);
        }
        visibleControlIndex ++;

        if (shouldShowControl) {
            // Remember how much space the controls take up
            controlSize += vertical ? control.getVisibleHeight() : control.getVisibleWidth();
        }
    }

    // remove any members of the controlLayout beyond the end of the current set of visible
    // controls
    var membersToRemove = [];
    for (var i = visibleControlIndex; i < controlLayout.members.length; i++) {
        membersToRemove.add(i);
    }
    controlLayout.removeMembers(membersToRemove);
    // Note: we're not destroying these members, just deparenting them

    // If we are NOT showing any controls, hide the layout and return false
    if (controlSize == 0) {
        this.hideControls();
        return false;
    }

    this.placeControlLayout(controlSize);

    if (!controlLayout.isDrawn()) {
        if (this.getDrawnState()          != isc.Canvas.UNDRAWN && 
            controlLayout.getDrawnState() == isc.Canvas.UNDRAWN) controlLayout.draw();
    } else if (!controlLayout.isVisible()) controlLayout.show();

    return true;
},

placeControlLayout : function (controlSize) {
    
    // Now figure out the desired sizing / position of the controlLayout and put it in the right
    // place
    var left,top,width,height,
        // Ensure that we don't cover the baseline
        tb = this._tabBar,
        // TabBar.getBreadth() != tabBarThickness if an app explicitly sets the tabbar's height
        // differently in properties/defaults. Notably, this occurs in the Feature Explorer,
        // where the skin switcher is thicker than the tabBarThickness under Enterprise and
        // related skins. getBreadth() is the more accurate distance
        tbThickness = tb.getBreadth() - tb.baseLineThickness,
        barPos = this.tabBarPosition;

    if (barPos == isc.Canvas.LEFT) {
        left = 0;
        top = this.getHeight() - controlSize;
        width = tbThickness;
        height = controlSize;
    } else if (barPos == isc.Canvas.RIGHT) {
        left = this.getWidth() - tbThickness;
        top = this.getHeight() - controlSize;
        width = tbThickness;
        height = controlSize;
    } else if (barPos == isc.Canvas.BOTTOM) {
        width = controlSize;
        left = this.isRTL() ? 0 : (this.getWidth() - controlSize);
        top = this.getHeight() - tbThickness;
        height = tbThickness;
    // Last possibility is TOP
    } else {
        width = controlSize;
        left = this.isRTL() ? 0 : this.getWidth() - controlSize;
        top = 0;
        height = tbThickness;
    }

    this.tabBarControlLayout.setRect(left, top, width, height);
    if (!this.children.contains(this.tabBarControlLayout)) this.addChild(this.tabBarControlLayout);

},

_controlLayoutChildResized : function () {
    var layout = this.tabBarControlLayout;
    if (!layout) return;
    this.showControls();

    var tb = this.tabBar;
    if (tb) {
        var vertical = (this.tabBarPosition == isc.Canvas.LEFT || 
                        this.tabBarPosition == isc.Canvas.RIGHT);
        if (vertical) {
            tb.setHeight(this.getViewportHeight() - this.tabBarControlLayout.getVisibleHeight());
        } else {
            tb.setWidth(this.getViewportWidth() - this.tabBarControlLayout.getVisibleWidth());
        }
    }
},

// Hide the controlLayout
hideControls : function () {
    if (this.tabBarControlLayout && this.tabBarControlLayout.isVisible()) this.tabBarControlLayout.hide();
},

//>@method  tabSet.scrollForward()
// If there is not enough space to display all the tabs in this tabSet, this method will 
// scroll the next tab (that first tab that is clipped at the end of the tab-bar) into view.
// @visibility external
//<
scrollForward : function () {
    this._tabBar.scrollForward(this.animateTabScrolling);
},

//>@method  tabSet.scrollBack()
// If there is not enough space to display all the tabs in this tabSet, this method will 
// scroll the previous tab (that first tab that is clipped at the beginning of the tab-bar) 
// into view.
// @visibility external
//<
scrollBack : function () {
    this._tabBar.scrollBack(this.animateTabScrolling);
},

// Called from click on the tabPicker control. Displays a menu with options to select
// a tab from the tabSet
showTabPickerMenu : function () {
    
    if (!this._pickerMenu) {
        var tabs = this.tabs,
            items = [];
        for (var i = 0; i < tabs.length; i++) { 
            items[i] = {index:i,
                        enabled:!this.tabs[i].disabled,
                        checkIf:"menu.tabSet.getSelectedTabNumber() == " + i,
                        title:tabs[i].pickerTitle || tabs[i].title, 
                        // Note: We show the tab's icon in the menu, if there is one.
                        // This will show instead of the check-mark which we normally use to 
                        // indicate selection
                        
                        icon:(this.canCloseTab(tabs[i]) ? null : tabs[i].icon),
                        
                        // Calling selectTab will automagically scroll the tab into view if
                        // necessary
                        click:"menu.tabSet.selectTab(item.index)"}
        }
        this._pickerMenu = this.getMenuConstructor().create({tabSet:this, data:items})
    }
    
    // Show it under the button
    
    this._pickerMenu._showOffscreen();        
    this._pickerMenu.placeNear(this.tabPicker.getPageLeft(), this.tabPicker.getPageBottom())
    this._pickerMenu.show();
},

// resetTabPickerMenu - helper to destroy the tab picker menu so it will be rebuilt when next shown
// This ensures it picks up new details from the current set of tabs.
resetTabPickerMenu : function () {
    if (this._pickerMenu) {
        this._pickerMenu.destroy();
        delete this._pickerMenu;
    }
}, 

// fix layout on a change of size
layoutChildren : function (reason,b,c,d) {
    this.invokeSuper(isc.TabSet, "layoutChildren", reason,b,c,d);
    this.fixLayout();
},

_tabResized : function () {
    this.fixLayout();
},

// NOTE: this is internal because it only shows a new tab, it does not hide the previous tab.
// The external API is selectTab();
_showTab : function (tab) {
    

    if (tab == this.moreTab) {
        this.rebuildMorePane();
    }
	this.paneContainer.scrollTo(0,0,"showTab");

    if (tab != null && tab.pane != null) {
        if (!this.paneContainer.hasMember(tab.pane)) this.paneContainer.addMember(tab.pane);
        var paneMargin = ((tab.paneMargin != null ? tab.paneMargin : this.paneMargin) || 0);
        this.paneContainer.setLayoutMargin(paneMargin);
        tab.pane.show();
        this.paneContainer.stopIgnoringMember(tab.pane);
    }

	this.paneContainer.adjustOverflow();
},

//>	@method	tabSet._tabSelected(tab)	(A)
// Perform actions when a tab is selected. 
// This method is "bound" to the tabBar's buttonSelected method, so that is will fire
// whenever a button on the tabBar is seleced. it performs the following functions:
// 			 - show the associated pane
// 			 - scroll to (0,0)
//
//		@see this.tabBar.buttonSelected
//		@param	tab	(tab) tab that has been selected.
//<

_tabSelected : function (tab) {
    // fire handler (fire it first so it has an opportunity to alter the tab, eg add a pane on
    // the fly)

    var cancelSelection;

    var currentTabObject = this.getSelectedTab(),
        currentTabNum = this.getSelectedTabNumber(),
        tabNum = this._tabBar.getButtonNumber(tab),
        tabObject = this.getTabObject(tabNum),
        tabDeselected = (currentTabObject != null) && (tabObject != currentTabObject);


    // currentTabNum may already be set to the tab being selected, before this
    // method has run.
    // This can occur on initial selection when tab is added/drawn and
    // on selection due to other tab being removed.
    // Therefore store another flag "_selectedTabObj" to indicate we've actually run
    // our tabSelected handlers and shown the pane.
    // If this flag is set to the tab passed in, no-op.
    
    var isMoreTab = this.showMoreTab && this.tabBar.isShowingMoreTab() && tabObject == this.moreTab;
    if (!isMoreTab) {
        if (tabObject == this._selectedTabObj) return;
        this._selectedTabObj = tabObject;
    }

    if (tabDeselected && !this._suppressTabSelectedHandlers) {
        // fire deselected and selected handlers.
        // Note: If this is the first time the thing is drawn we'll have tabSelected being
        // fired on the initially selected tab but the "currentTabObject" will also point to that
        // tab -- in this case don't fire the deselected handler
        // Also note: if a tab is removed programmatically it is deselected. In this case
        // currentTabObject can be expected to be unset at this point.
        if (currentTabObject.tabDeselected != null) {
            if (this.fireCallback(
                    
                    currentTabObject.tabDeselected, 
                    "tabSet,tabNum, tabPane, ID, tab, newTab, name", 
                    
                    [   this,
                        // deselected tab details
                        this.selectedTab, currentTabObject.pane, currentTabObject.ID, 
                        currentTabObject,
                        // new tab
                        tabObject,
                        currentTabObject.name
                    ]
                ) == false) 
            {
                cancelSelection = true;
            }
        }

        if (!cancelSelection && this.tabDeselected != null) {
            cancelSelection = (this.tabDeselected(this.selectedTab, 
                                currentTabObject.pane, currentTabObject.ID, currentTabObject, 
                                tabObject, currentTabObject.name) == false)
        }
        var currentPane = currentTabObject.pane;
        // hide the current pane
        if (!cancelSelection && currentPane != null) {
            currentPane.hide();
            this.paneContainer.ignoreMember(currentPane);
            currentPane.moveTo(this.isRTL() ? 9999 : -9999, -9999);
        }
    }

    // force the tab to go back to selected state but don't fire any handlers / show or hide
    // tabs, etc.
    if (cancelSelection) {
        this._suppressTabSelectedHandlers = true;

        var cancelledTabObject = tabObject;
        var tab = this.getSelectedTab();
        this.selectTab(tab);
        var tabButton = this.getTab(this.getTabNumber(tab));
        // If this came from a click on the new tab, 
        // explicitly focus back in the tab we just re-selected. This is just better UI - if
        // someone clicked a tab and it didn't select, but focus went there we don't really
        // want a dotted outline on the clicked, but not selected button.
        if (isc.EH.mouseDownTarget() == this.getTab(cancelledTabObject)) {
            // If a clickMask went up (most likely as part of the 'tabDeselected' handler showing a
            // prompt), ensure that on its dismissal focus goes to this tab, not the last clicked
            // tab!
            if (isc.EH.clickMaskUp() && isc.EH.targetIsMasked(tabButton)) {
                var topMask = isc.EH.clickMaskRegistry.last();
                isc.EH.setMaskedFocusCanvas(tabButton, topMask);
            } else {
                tabButton.focus();
            }
        }
        delete this._suppressTabSelectedHandlers;
        return;
    }

    // If pane has been destroyed drop our reference
    var pane = tabObject.pane;
    if (pane && (pane.destroyed || pane.destroying || pane.isPendingDestroy())) {
        tabObject.pane = null;
    }

    // Remember the selected tabNum - used by this.getSelectedTabNumber() etc.
    this.selectedTab = tabNum;
    if (!this._suppressTabSelectedHandlers) {
        var handlerChangedTab;
        if (tabObject.tabSelected != null) {
            this.fireCallback(
                tabObject.tabSelected, 
                "tabSet,tabNum,tabPane,ID,tab",
                [this,tabNum,tabObject.pane,tabObject.ID,tabObject]
            );

            // If this tab is no longer marked as selected, tabSelected() may have shown a 
            // different tab.  In this case don't call _showTab!
            if (this.getSelectedTabNumber() != tabNum) {
                return;
            }
        }

        // fire the notification functions
        if (this.tabSelected) {
            this.tabSelected(tabNum, tabObject.pane, tabObject.ID, tabObject, tabObject.name);

            // Once againk, if this tab is no longer marked as selected, tabSelected() 
            // may have shown a different tab.  In this case don't call _showTab!
            if (this.getSelectedTabNumber() != tabNum) {
                return;
            }
        }
    }
    this._showTab(tabObject);

    // ensure the tab button is scrolled into view
    var tb = this._tabBar;
    // leave the second param as null - tab bar will automatically scroll to appropriate
    // position
    var tabSet = this;
    tb.scrollTabIntoView(tabNum, null, this.animateTabScrolling);
},


//> @method tab.tabSelected()
// Optional handler to fire when a tab is selected. As with +link{TabSet.tabSelected()} this
// method only fires when the tabset is drawn.
//
// @param tabSet (TabSet) the tabSet containing the tab.
// @param tabNum (integer) the index of the newly selected tab
// @param tabPane (Canvas) the newly selected tab's pane if set
// @param ID (String) the ID of the newly selected tab
// @param tab (tab) pointer to the selected tab object
// @param name (String) the name of the newly selected tab
//
// @see tab.tabDeselected
// @visibility external
//<

//> @method tab.tabDeselected()
// Optional handler to fire when a tab is deselected. Returning false will cancel the
// new selection, leaving this tab selected. As with +link{TabSet.tabSelected()} this
// method only fires when the tabset is drawn.
//
// @param tabSet (TabSet) the tabSet containing the tab.
// @param tabNum (integer) the index of the deselected tab
// @param tabPane (Canvas) the deselected tab's pane if set
// @param ID (String) the ID of the deselected tab
// @param tab (tab) pointer to the tab being deselected
// @param newTab (tab) pointer to the new tab being selected
// @param name (String) the name of the deselected tab
//
// @return (boolean) return <code>false</code> to cancel the tab selection
//
// @see tab.tabSelected
// @visibility external
//<


//>	@method	tabSet.getSelectedTab() ([A])
// Returns the currently selected tab object.  This is the object literal used to configure the
// tab, rather than the tab button widget.
// @return (Tab) the currently selected Tab object
// @visibility external
//<
getSelectedTab : function () {
    if (this.selectedTab >= this.tabs.length) return this.moreTab;
    return this.tabs[this.selectedTab];
},

//>	@method	tabSet.getSelectedTabNumber() ([A])
// Returns the index of the currently selected tab object.  
// @return (number) the index of the currently selected tab object
// @visibility external
//<
getSelectedTabNumber : function () {
    if (!isc.isA.Number(this.selectedTab)) this.selectedTab = this.getTabNumber(this.selectedTab);
    // If the specified selectedTabNum doesn't correspond to a tab don't return it.
    if (!this.tabs || !this.tabs[this.selectedTab]) return -1;
    return this.selectedTab;
},



//>	@method	tabSet.selectTab()    ([])
//	Select a tab
// @param	tab   (number | ID | name | Tab) tab to select
// @visibility external
// @example tabsOrientation
//<
selectTab : function (tab) {
    var tabIndex = this.getTabNumber(tab);
    if (tabIndex != -1) {
        // calling 'selectTab()' on the tab bar will actually select the button.
        // this handles firing our tabSelected() notification functions
        if (this._tabBar) {
            this._tabBar.selectTab(tabIndex);
        }
        
        // TabBar (subclass of Toolbar) initializes its members (buttons) lazily on draw()
        // We won't get any _tabSelected notifications until after this has happened.
        // Therefore if the tab bar hasn't initialized yet, simply record this.selected tab
        // so methods like this.getSelectedTabNum() / getselectedTabObject() work
        //
        // Note that we explicitly call tabBar.selectTab(this.selectedTab) on draw() to ensure
        // the tab-bar stays in synch
        if (this._tabBar == null || !this._tabBar._buttonsInitialized) {
            this.selectedTab = tabIndex;
        }
        
    }
},

//> @method tabSet.tabForPane()
//Search for a tab that contains a pane.
//@param pane (Canvas) pane to show
//@return (Tab) tab that contains passed pane
//@visibility external
//<
tabForPane : function (pane) {
    if (this.tabs) {
        for (var i = 0; i < this.tabs.length; i++) {
            if (this.tabs[i].pane == pane) {
                return this.tabs[i];
            }
        };        
    }
},

//>	@method	tabSet.getTabBar()
// Returns handle to the TabBar used by this tabset
// @return (TabBar) the tab bar
//<
getTabBar : function () {
    return this._tabBar;
},

_editTabTitle : function (tab) {
    tab = this.getTab(tab);
    
    var canEdit;
    
    if (this.canEditTabTitles) {
        if (tab.canEditTitle !== false) {
            canEdit = true;
        }
    } else {
        if (tab.canEditTitle === true) {
            canEdit = true;
        }
    }
    
    if (canEdit) this.editTabTitle(tab);
    return canEdit;
},

//>	@method	tabSet.editTabTitle()
// Places an editor in the title of the parameter tab and allows the user to edit the title.
// Note that this programmatic method will <b>always</b> allow editing of the specified tab's
// title, regardless of the settings of +link{canEditTabTitles} or +link{Tab.canEditTitle}.
// @param	tab      (Tab | String | integer)   The tab whose title should be edited (may be
//   specified by ID or index)
// @see TabSet.canEditTabTitles
// @see Tab.canEditTitle
// @visibility external
//<
editTabTitle : function (tab) {
    tab = this.getTab(tab);
    
    if (tab == null || !this.tabBar) return;
    
    if (!isc.isA.DynamicForm(this.titleEditorForm)) {
        var titleEditorConfig =  isc.addProperties(
                {}, this.titleEditorDefaults, 
                this.titleEditorProperties, {
                     handleKeyPress : function (event,eventInfo) {
                        
                        var rv = this.Super("handleKeyPress", arguments);
                        
                        var keyName = event.keyName;
                        
                        if (keyName == "Escape") {
                            this.form.targetTabSet.cancelTabTitleEditing();
                        } else if (keyName == "Enter") {
                            this.form.targetTabSet.saveTabTitle();
                        }
                        return rv;
                    }
                }
        );
        
        
        titleEditorConfig.name = "title";
        
        this.titleEditorForm = isc.DynamicForm.create({
            autoDraw: false,
            margin: 0, padding: 0, cellPadding: 0,
            fields: [
                titleEditorConfig
            ]
        });
        
        // Make the item directly available as a read-only form item (as documented)
        this.titleEditor = this.titleEditorForm.getItem("title");
    }
        
    var editor = this.titleEditorForm;
    editor.setProperties({targetTabSet: this, targetTab: tab});
        
    var item = editor.getItem("title");
    var title = tab.title;
    item.setValue(title);
    
    // Always scroll the tab into view before showing the editor.
     
    this.tabBar.scrollTabIntoView(tab, null, this.animateTabScrolling,
                {target:this, methodName:"showTitleEditor"});
},

//> @method tabSet.cancelTabTitleEditing()
// If the user is currently editing a tab title (see +link{tabSet.canEditTabTitles}), dismiss
// the editor and discard the edit value entered by the user.
// @visibility external
//<
// We'll fire this from standard end edit event (Escape keypress) too
cancelTabTitleEditing : function () {
    if (this.titleEditorForm != null) {
        this.clearTitleEditorForm();
    }
},

//> @method tabSet.saveTabTitle()
// If the user is currently editing a tab title (see +link{tabSet.canEditTabTitles}), save
// the edited tab title and hide the editor.
// @visibility external
//<
// Also fired internally from standard end edit event (click outside / enter keypress);
saveTabTitle : function () {
    if (this.titleEditorForm != null && this.titleEditorForm.isVisible() 
        && this.titleEditorForm.isDrawn()) 
    {
        var cancelEdit = false,
            form = this.titleEditorForm,
            tab = form.targetTab,
            newTitle = form.getValue("title")
        ;
        if (newTitle != tab.title && (this.titleChanged != null)) {
            if (this.fireCallback(
                    this.titleChanged,
                    "newTitle, oldTitle, tab", 
                    [newTitle, tab.title,tab]
                ) == false) 
            {
                cancelEdit = true;
            }
        }
        if (!cancelEdit) this.setTabTitle(form.targetTab, newTitle);
    }
    // Dismiss the editor even if the titleChanged callback returned false, cancelling the
    // edit.
    // If we leave the editor up we're likely to get into tricky situations where
    // for example the developer can change tab with the editor still showing on another tab,
    
    this.clearTitleEditorForm();
},

clearTitleEditorForm : function () {
    if (this.titleEditorForm == null) return;
    this.titleEditorForm.clear();
    if (this.titleEditorForm._titleEditClickEvent != null) {
        isc.Page.clearEvent(this._titleEditClickEvent);
        delete this._titleEditClickEvent;
    }
    // Clear the 'targetTab' flag. This will allow us to avoid performing asyncronous "show"
    // due to pending animations etc. after this method has fired
    this.titleEditorForm.targetTab = null;
},

showTitleEditor : function () {
    var editor = this.titleEditorForm,
        tab = editor ? editor.targetTab : null;

    // This could happen a tab was removed, or clearTitleEditor() was called while waiting
    // for a tab to scroll (animatedly) into view, etc.
    if (tab == null || !this.getTabObject(tab)) {
        return;
    }
    // the editor will be a peer of the TabSet (shares the same parentElement)
    // The tab is a child of the TabBar
    // so left top should be tab left/top within the tabBar + tabBar left + tabBar border/margin

    var left = this.tabBar.getLeft() + this.tabBar.getLeftMargin() - this.tabBar.getScrollLeft()
            + this.tabBar.getLeftBorderSize() + tab.getLeft() + tab.capSize,
        width = tab.getVisibleWidth() - tab.capSize * 2;

    if (this.titleEditorLeftOffset) {
        left += this.titleEditorLeftOffset;
        width -= this.titleEditorLeftOffset;
    }

    if (this.titleEditorRightOffset) {
        width -= this.titleEditorRightOffset;
    }

    var item = editor.getItem("title");
    item.setWidth(width);

    // Editor form will be a peer of the tabSet - needs to float over the content of
    // the tab (nested inside the tabBar).
    var top = this.getTop() + 
              this.tabBar.getTop() + this.tabBar.getTopMargin() - this.tabBar.getScrollTop()
                + this.tabBar.getTopBorderSize() + tab.getTop();
    if (this.titleEditorTopOffset) {
        top += this.titleEditorTopOffset;
    }

    

    editor.moveTo(left, top);

    var item = editor.getItem("title");

    // make the editor a peer so it moves with us.
    // This will also handle showing / hiding / clearing / drawing with us - however
    // we'll also need to clear up the click-outside event on clear/hide so we'll
    // explicitly cancel title editing when we hide / clear instead of relying on this.
    if (editor.masterElement != this) {
        editor._moveWithMaster = true;
        editor._resizeWithMaster = false;
        editor._showWithMaster = false;
        this.addPeer(editor);

    } else {
        editor.draw();
    }
    item.focusInItem();
    item.delayCall("selectValue", [], 100);

    // Save edits on click outside title editor
    
    if (this._titleEditClickEvent == null) {
        var tabSet = this;
        var mouseDownHandler = function () {
            if (!tabSet.destroyed) {
                tabSet._clickOutsideDuringTitleEdit();
            }
        }
        this._titleEditClickEvent = isc.Page.setEvent("mouseDown", mouseDownHandler);
    }
},

_clickOutsideDuringTitleEdit : function () {
    if (isc.EH.getTarget() == this.titleEditorForm) return;
    this.saveTabTitle();
},

// On clear / hide / parent visibility change cancel title editing

// Clear is called recursively so this'll pick up parents clearing too
clear : function (a,b,c,d) {
    if (this.titleEditorForm != null && this.titleEditorForm.isDrawn()) {
        this.cancelTitleEditing();
    }
    this.invokeSuper("TabSet", "clear", a,b,c,d);
},

setVisibility : function (newVisibility, a,b,c,d) {
    this.invokeSuper("TabSet", "setVisibility", newVisibility, a,b,c,d);
    if (!this.isVisible() && this.titleEditorForm != null && this.titleEditorForm.isDrawn()) {
        this.cancelTitleEditing();
    }
},

parentVisibilityChanged : function (newVisibility, a,b,c,d) {
    this.invokeSuper("TabSet", "parentVisibilityChanged", newVisibility, a,b,c,d);
     if (!this.isVisible() && this.titleEditorForm != null && this.titleEditorForm.isDrawn()) {
        this.cancelTitleEditing();
    }
},

// documented where the string method is registered
tabsReordered : function () {}

});


isc.TabSet.registerStringMethods({
    //>	@method	tabSet.tabSelected()
    // Notification fired when a tab is selected. Note that this will only fire if 
    // this tabSet is drawn. If a tab is selected before <code>TabSet.draw()</code> 
    // is called, the <code>tabSelected()</code> notification will fire on 
    // <code>draw()</code>
    // @param tabNum (number) number of the tab
    // @param tabPane (Canvas) pane for this tab
    // @param ID (id) id of the tab
    // @param tab (tab) the tab object (not tab button instance)
    // @param name (String) the name of the newly selected tab
    // @visibility external
    //<
    
	tabSelected:"tabNum,tabPane,ID,tab,name",

    //>	@method	tabSet.tabDeselected()
    //  Notification fired when a tab is deselected.        
    // @param tabNum (number) number of the deselected tab
    // @param tabPane (Canvas) pane for this deselected tab
    // @param ID (id) id of the deselected tab
    // @param tab (tab) the deselected tab object (not tab button instance)
    // @param newTab (tab) the tab object being selected
    // @return (boolean) return false to cancel the tab deselection
    // @visibility external
    //<
	tabDeselected:"tabNum,tabPane,ID,tab,newTab,name",
    
    
    // getPaneContainerEdges - documented by default implementation
    getPaneContainerEdges:"",
    
    //> @method tabSet.onCloseClick()
    // When +link{canCloseTabs} is set, this notification method fired when the user clicks 
    // the "close" icon for a tab.
    // Return false to cancel default behavior of removing the tab from the TabSet
    // @param tab (Tab) the tab to be removed
    // @return (boolean) return false to suppress removal of the tab
    // @visibility sgwt
    //<
    
    onCloseClick : "tab",
    
    //> @method tabSet.titleChanged()
    // This notification method fired when the user changes the title of a tab in this TabSet.
    // This can happen either through user interaction with the UI if 
    // +link{canEditTabTitles,canEditTabTitles} is set, or programmatically if application 
    // code calls +link{editTabTitle,editTabTitle}.<p>
    // Return false from this method to cancel the change.
    // @param newTitle (String) the new title
    // @param oldTitle (String) the old title
    // @param tab      (Tab)    the tab whose title has changed
    // @return (boolean) return false to suppress the title change
    // @visibility external
    //<
    titleChanged : "newTitle,oldTitle,tab",
    
    //> @method tabSet.showTabContextMenu()
    // Notification fired when the user right-clicks on a tab.
    // Event may be cancelled by returning false
    // @param tabSet (TabSet) This tabset
    // @param tab (Tab) the tab object that recieved the context click event
    // @return (boolean) return false to cancel default right-click behavior
    // @visibility external
    //<
    showTabContextMenu : "tabSet,tab",

	//> @method tabSet.tabsReordered
    // Noficiation method executed when one or more tabs in the TabSet are reordered.
	// @visibility external
	//<
    tabsReordered : ""

});

isc.defineClass("PaneContainer", "VLayout").addMethods({
    // override handleKeyPress to allow for navigation between tabs when focus'd on the
    // pane container or its children (via bubbled handleKeyPress events)
    // ctrl+tab - move one pane forward (or back to the first pane)
    // ctrl+shift+tab - move one pane back
    // (This is the Windows behavior - see Windows control panel)
    
    handleKeyPress : function (event, eventInfo) {
        if (event.keyName == "Tab" && event.ctrlKey) {
            var tabSet = this.parentElement,
                lastTabIndex = tabSet.tabs.length-1,
                currentSelection = tabSet.getSelectedTabNumber();

            if (event.shiftKey) {
                if (currentSelection > 0) currentSelection -=1;
                else currentSelection = lastTabIndex;
            } else {
                if (currentSelection < lastTabIndex) currentSelection +=1;
                else currentSelection = 0;
            }

            tabSet.selectTab(currentSelection);
            tabSet.getTabBar().getButton(currentSelection).focus();
            return false;                
        }
        return this.Super("handleKeyPress", arguments);
    }		        
});

// Register "tabs" as duplicate properties
// This means if a tabset subclass is created with tabs explicitly set to a bunch of config
// objects they'll be duplicated on instances rather than copied across directly.
// Ditto if <childName>Defaults is used in the autoChild subsystem.
// Also register the 'pane' sub property so if tab.pane is set it will be duplicated
// rather than shared across tabs
isc.TabSet.registerDupProperties("tabs", ["pane"]);

