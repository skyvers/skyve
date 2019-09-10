/*============================================================
    "Tahoe" theme programmatic settings
    Copyright 2003 and beyond, Isomorphic Software
============================================================*/

isc.loadSkin = function (theWindow) {
if (theWindow == null) theWindow = window;
with (theWindow) {


//----------------------------------------
// Specify skin directory
//----------------------------------------
    // must be relative to your application file or isomorphicDir
    isc.Page.setSkinDir("[ISOMORPHIC]/skins/Tahoe/")


//----------------------------------------
// Load skin style sheet(s)
//----------------------------------------
    // if loadStyleSheet() returns false, callback will fire when the CSS is loaded
    var cssLoaded = isc.Page.loadStyleSheet("[SKIN]/skin_styles.css", theWindow, 
                                            "isc.FontLoader.loadCustomFonts()");

    isc.Page.checkBrowserAndRedirect("[SKIN]/unsupported_browser.html");
    
    isc.Class.modifyFrameworkStart();
    
    // Register icons to resize with controls / fonts
    isc.Canvas.registerIconSizingAttributes(
        "fonts",
        {
            ComboBoxItem:[
                ["pickerIconHeight","pickerIconWidth"],
                ["pickButtonHeight","pickButtonWidth"]
            ],
            SelectItem:[
                ["pickerIconHeight","pickerIconWidth"],
                ["pickButtonHeight","pickButtonWidth"]
            ],
            CheckboxItem:[
                ["valueIconHeight","valueIconWidth"]
            ],
            TreeGrid: [
                "openerIconSize"
            ],
            ListGrid:[
                ["checkboxFieldImageHeight", "checkboxFieldImageWidth"],
                ["booleanImageHeight","booleanImageWidth"],
                "removeIconSize"
            ],
            ToolStripButton:[
                "height", 
                "iconSize"
            ],
            ToolStrip:["height"],
            MenuButton:[
                ["iconHeight","iconWidth"],
                "iconSize"
            ],
            TabSet:[
                "defaultTabIconSize"
            ],
            SpinnerItem:[
                ["stackedIconsHeight", "stackedIconsWidth"]
            ]
        }
    );
    
    isc.Canvas.registerIconSizingAttributes(
        "controls",
        {
            DateItem:[
                ["pickerIconHeight","pickerIconWidth"]
            ],
            DatetimeItem:[
                ["pickerIconHeight","pickerIconWidth"]
            ],
            RelativeDateItem:[
                ["pickerIconHeight","pickerIconWidth"]
            ],
            MiniDateRangeItem:[
                ["pickerIconHeight", "pickerIconWidth"]
            ],
            ColorItem:[
                ["pickerIconHeight", "pickerIconWidth"]
            ],
            ListGrid: [
                "defaultEditableDateFieldWidth", 
                "defaultEditableDateTimeFieldWidth"
            ]
        }
    );
    
    isc.Canvas.setAutoResizeIcons(true);
    
    isc.Canvas.registerAutoChildSizingAttributes(
        "fonts",
        "Window",
        {
            headerIconDefaults:["height","width"],
            restoreButtonDefaults:["height","width"],
            closeButtonDefaults:["height","width"],
            maximizeButtonDefaults:["height","width"],
            minimizeButtonDefaults:["height","width"]
        }
    );
    
    isc.Canvas.setAutoResizeAutoChildAttributes(true);    

    // register style declarations with padding that should track font size
    isc.Canvas.registerFontScaledPaddingStyles(
        [        "tabButtonTop",         "tabButtonBottom"], 
        ["iconOnlyTabButtonTop", "iconOnlyTabButtonBottom"],
        3
    );

    // -----------------------------------------------------   
    // css3 and spriting are required for the Tahoe skin
    var useCSS3 = isc.Browser.useCSS3,
        useSpriting = isc.Browser.useSpriting;
        
    if (!useCSS3 || !useSpriting) {
        isc.logWarn("Tahoe skin makes use of HTML5 features which may be " +
            "unsupported in this browser. The appearance of components cannot " +
            "be guaranteed. See the 'Skinning' documentation topic for more information.");
    }
    // -----------------------------------------------------   
    
    //----------------------------------------
    // 1) Scrollbars
    //----------------------------------------
    
    isc.Canvas.setProperties({
        // this skin uses custom scrollbars
        groupBorderCSS: "1px solid #165fa7",
        showCustomScrollbars: true
    });

    isc.Canvas.addProperties({
        scrollbarSize:16
    });

    isc.SimpleScrollThumb.addProperties({
        imageWidth:10, imageHeight:10,
        baseStyle:"scrollThumb",
        hSrc:"[SKIN]hthumb_grip.png",
        vSrc:"[SKIN]vthumb_grip.png"
    });

    isc.Scrollbar.addProperties({
        baseStyle:"scrollbar",
        btnSize:18,
        hSrc:"[SKIN]hscroll.png",
        hThumbClass:isc.HSimpleScrollThumb,
        showRollOver:true,
        thumbInset:0,
        thumbMinSize:20,
        thumbOverlap:2,
        vSrc:"[SKIN]vscroll.png",
        vThumbClass:isc.VSimpleScrollThumb,
        endThumbOverlap:-2,
        startThumbOverlap:-2
    });
    isc.Scrollbar.changeDefaults("trackImg", {
        name:"blank",
        baseStyleKey:"vertical",
        baseStyleMap:{
            "true": "vScrollTrack",
            "false": "hScrollTrack"
        },
        baseStyle:"scrollTrack"
    });
    
    isc.Scrollbar.changeDefaults("cornerImg", { name:"blank0", baseStyle:"scrollCorner" });

    if (isc.SpritedScrollbar) {
        // use sprited scrollbars by default
        isc.Canvas.addProperties({
            scrollbarConstructor: "SpritedScrollbar"
        });

        isc.SpritedSimpleScrollThumb.addProperties({
            hSrc: "[SKINIMG]/blank.gif",
            vSrc: "[SKINIMG]/blank.gif",
            gripImgSuffix: "",
            redrawOnStateChange: true
        });
        isc.SpritedVSimpleScrollThumb.addProperties({
            imageStyle: "vScrollThumbGrip"
        });
        isc.SpritedHSimpleScrollThumb.addProperties({
            imageStyle: "hScrollThumbGrip"
        });

        isc.SpritedScrollbar.addProperties({
            hThumbClass:isc.SpritedHSimpleScrollThumb,
            vThumbClass:isc.SpritedVSimpleScrollThumb
        });
        isc.SpritedScrollbar.changeDefaults("startImg", {
            name: "blank1",
            baseStyleKey: "vertical",
            baseStyleMap: {
                "true": "vScrollStart",
                "false": "hScrollStart"
            },
            baseStyle:"scrollStart"
        });
        isc.SpritedScrollbar.changeDefaults("endImg", {
            name: "blank10",
            baseStyleKey: "vertical",
            baseStyleMap: {
                "true": "vScrollEnd",
                "false": "hScrollEnd"
            },
            baseStyle:"scrollEnd"
        });    
    }


    //----------------------------------------
    // 2) Buttons
    //----------------------------------------
    isc.Button.addProperties({
        height:22,
        width:120,//133
        baseStyle:"button",
        showFocusedAsOver:false,
        showFocusOutline:false
    });

    // define IButton so examples that support the new SmartClient skin image-based
    // button will fall back on the CSS-based Button with this skin
    isc.ClassFactory.defineClass("IButton", "Button").addProperties({
        baseStyle:"button",
        showFocusedAsOver:false,
        showFocusOutline:false
    });
    isc.ClassFactory.defineClass("IAutoFitButton", "AutoFitButton").addProperties({
        baseStyle:"button"
    });

    if (isc.IButton.markAsFrameworkClass != null) isc.IButton.markAsFrameworkClass();
    if (isc.IAutoFitButton.markAsFrameworkClass != null) isc.IAutoFitButton.markAsFrameworkClass();

    isc.ClassFactory.defineClass("HeaderMenuButton", "IButton").addProperties({
        baseStyle:"headerButton"
    });

    // Have IMenuButton be just a synonym for IMenuButton
    if (isc.MenuButton) {
        isc.ClassFactory.overwriteClass("IMenuButton", "MenuButton");

        if (isc.IMenuButton.markAsFrameworkClass != null) isc.IMenuButton.markAsFrameworkClass();

        if (isc.ITreeMenuButton) {
            isc.ClassFactory.overwriteClass("ITreeMenuButton", "TreeMenuButton");
            if (isc.ITreeMenuButton.markAsFrameworkClass != null) {
                isc.ITreeMenuButton.markAsFrameworkClass();
            }
        }

        isc.MenuButton.addProperties({
            baseStyle:"menuButton",
            flipOpenedMenuButtonImage:true,
            showOpened:true,
            iconHeight:5,
            iconWidth:12,
            menuButtonImage:"[SKIN]down.png",
            menuButtonImageUp:"[SKIN]up.png",
            showFocusedAsOver:false
        });
        
        isc.IMenuButton.addProperties({
            capSize:4,
            iconWidth:19,
            iconHeight:8,
            menuButtonImage:"[SKIN]down.png",
            menuButtonImageUp:"[SKIN]up.png",
            showFocused:true,
            showFocusedAsOver:true,
            src:"[SKIN]button/button.png",
            vertical:false,
            width:100
        });
    }


    isc.Label.addProperties({
        showFocused:false,
        showFocusOutline:true
    });

    //----------------------------------------
    // 3) Resizebars
    //----------------------------------------

    isc.Layout.addProperties({
        resizeBarSize: 7,
        resizeBarClass: "Snapbar"
    });

    isc.StretchImgSplitbar.addProperties({
        capSize:10,
        showGrip:true,
        showOver:false
    });
    

    isc.Snapbar.addProperties({
        hBaseStyle:"hSplitbar",
        vBaseStyle:"vSplitbar",
        showGrip:true,
        gripBreadth:3,
        //border:0,
        gripLength:20,
        hSrc:"[SKIN]hsplit.png",
        items:[
            {name:"blank", width:"*", height:"*"}
        ],
        showClosedGrip:false,
        showDown:false,
        showDownGrip:false,
        showRollOver:false,
        vSrc:"[SKIN]vsplit.png"
    });

    //----------------------------------------
    // 4) Dialogs
    //----------------------------------------
    if (isc.MultiGroupDialog) {
        isc.MultiGroupDialog.addProperties({
            height: 295,
            showShadow: true,
            shadowColor: "#f0f0f0",
            shadowDepth: 10
        });
    }

    if (isc.MultiSortDialog) {
        isc.MultiSortDialog.addProperties({
            height: 295,
            showShadow: true,
            shadowColor: "#f0f0f0",
            shadowDepth: 10
        });
    }

    if (isc.MultiSortPanel) {
        isc.MultiSortPanel.changeDefaults("levelUpButtonDefaults", {
            src: "[SKINIMG]TransferIcons/up.png",
            height: 22,
            width: 24
        });
        isc.MultiSortPanel.changeDefaults("levelDownButtonDefaults", {
            src: "[SKINIMG]TransferIcons/down.png",
            height: 22,
            width: 24
        });
    }


    //----------------------------------------
    // 5) TabSets
    //----------------------------------------
    if (isc.TabBar) {
        isc.TabBar.changeDefaults("baseLineDefaults", {
            _constructor: "Canvas",
            backgroundColor: "#9C9C9C"
        });
        isc.TabBar.changeDefaults("tabDefaults", {
            showFocusOutline: false
        });
    }
    
    if (isc.TabSet) {

        isc.TabSet.addProperties({
            tabBarThickness:27,
            closeTabIconSize:12,
            defaultTabIconSize:13,
            iconSpacing:20,
            paneContainerClassName:"tabSetContainer",
            paneMargin:5,
            pickerButtonSize:28,
            touchPickerButtonSize:26,
            pickerButtonSrc:"[SKIN]picker.png",
            showScrollerRollOver:false,
            scrollerButtonSize:35,
            scrollerSrc:"[SKIN]scroll.png",
            simpleTabIconOnlyBaseStyle: "iconOnlyTabButton",
            showEdges:false,
            symmetricScroller:false,
            symmetricPickerButton:false,
            defaultTabHeight:24,
            useSimpleTabs:true,
            needEmptyButton:true,
            scrollerBackHMarginSize: 1,
            scrollerBackVMarginSize: 1,
            getScrollerBackImgName : function () {
                return "blank1";
            },
            getScrollerForwardImgName : function () {
                return "blank2";
            },
            tabPickerHMarginSize: 1,
            tabPickerVMarginSize: 1,
            getTabPickerSrc : function () {
                return "sprite:cssClass:tabPicker" + (this.tabPicker ? this.tabPicker.getStateSuffix() : "") + this.tabBarPosition;
            }
        });
        isc.TabSet.changeDefaults("scrollerDefaults", {
            renderStretchImgInTable: false,
            backgroundColor: "#e6e6e6"
        });
        isc.TabSet.changeDefaults("scrollerBackImg", {
            baseStyleKey: "scrollerPosition",
            baseStyleMap: {
                "top": "tabScrollerTopBack",
                "right": "tabScrollerRightBack",
                "bottom": "tabScrollerBottomBack",
                "left": "tabScrollerLeftBack"
            },
            baseStyle: "tabScrollerBack"
        });
        isc.TabSet.changeDefaults("scrollerForwardImg", {
            baseStyleKey: "scrollerPosition",
            baseStyleMap: {
                "top": "tabScrollerTopForward",
                "right": "tabScrollerRightForward",
                "bottom": "tabScrollerBottomForward",
                "left": "tabScrollerLeftForward"
            },
            baseStyle: "tabScrollerForward"
        });
        isc.TabSet.changeDefaults("tabPickerDefaults", {
            statelessImage: true,
            redrawOnStateChange: false,
            backgroundColor: "#e6e6e6",
            imageType: "center"
        });
        isc.TabSet.changeDefaults("tabBarControlLayoutDefaults", { styleName: "tabBar" });

        // In Netscape Navigator 4.7x, set the backgroundColor directly since the css
        // background colors are not reliable
        if (isc.Browser.isNav) {
            isc.TabSet.addProperties({paneContainerDefaults:{backgroundColor:"#FFFFFF"}});
        }


        isc.TabBar.addProperties({
            baseLineThickness:1,
            bottomStyleName:"tabBarBottom",
            layoutEndMargin:0,
            layoutStartMargin:5,
            leadingMargin:5,
            leftStyleName:"tabBarLeft",
            membersMargin:5,
            rightStyleName:"tabBarRight",
            styleName:"tabBar",
            topStyleName:"tabBarTop"
        });
    }

    if (isc.ImgTab) {
        isc.ImgTab.addProperties({
            capSize:6,
            showFocusedAsOver:true
        });
    }
    if (isc.SimpleTabButton) {
        isc.SimpleTabButton.addProperties({
            showFocusedAsOver:false
        })
    }

    //----------------------------------------
    // 6) Windows
    //----------------------------------------
    
    if (isc.Window) {
        isc.Window.addProperties({
            showHeaderIcon: false,
            backgroundColor:null,
            bodyStyle:"windowBody",
            layoutBottomMargin:0,
            edgeMarginSize: 8,
            layoutMargin: 0,
            modalMaskOpacity:10,
            membersMargin:0,
            styleName:"windowBackground",
            showHeaderBackground:false,
            showFooter:false,
            footerHeight: 30
        });

        isc.Window.changeDefaults("headerProperties", {
            layoutMargin: 4
        });
        isc.Window.changeDefaults("headerDefaults", {
            height:30,
            cursor: "inherit"
        });
        
        isc.Window.changeDefaults("bodyDefaults", {
            layoutLeftMargin:2,
            layoutRightMargin:2,
            layoutBottomMargin:2,
            cursor: "inherit"
        });
        isc.Window.changeDefaults("resizerDefaults", { src:"[SKIN]/Window/resizer.png" });

        isc.Window.changeDefaults("headerIconDefaults", {
            height:16,
            src:"[SKIN]/Window/headerIcon.png",
            width:16
        });

        isc.Window.changeDefaults("restoreButtonDefaults", {
            showDown:false,
            showRollOver:true,
            src:"[SKIN]/headerIcons/cascade.png",
            width: 24
        });

        isc.Window.changeDefaults("closeButtonDefaults", {
            showDown:false,
            showRollOver:true,
            src:"[SKIN]/headerIcons/close.png",
            imageType:"stretch",
            width:21, height:21,
            margin:5
        });

        isc.Window.changeDefaults("maximizeButtonDefaults", {
            showRollOver:true,
            src:"[SKIN]/headerIcons/maximize.png",
            imageType:"stretch",
            width:21, height:21,
            margin:5
        });

        isc.Window.changeDefaults("minimizeButtonDefaults", {
            showDown:false,
            showRollOver:true,
            src:"[SKIN]/headerIcons/minimize.png",
            imageType:"stretch",
            width:21, height:21,
            margin:5
        });

        isc.Window.changeDefaults("toolbarDefaults", { buttonConstructor:"IButton" });
    
        isc.Window.changeDefaults("headerDefaults", {
            layoutMargin:0,
            layoutRightMargin: 4
        });
        
        if (isc.Dialog) {
            isc.Dialog.addProperties({
                layoutMargin: 0
            });
        }
            

    //----------------------------------------
    // 7) Pickers
    //----------------------------------------
    if (isc.ColorItem) {
        
        isc.ColorItem.addProperties({
            showEmptyPickerIcon: true,
            pickerIconHeight:22,
            pickerIconWidth:22
        });
        isc.ColorItem.changeDefaults("pickerIconDefaults", {
            showRTL: true,
            showFocused: false,
            showOver: false
        });
    }
    
    if (isc.MultiFilePicker) {
        isc.MultiFilePicker.addProperties({
            showInWindow: true
        });
    }
    
        if (isc.ColorPicker) {
            isc.ColorPicker.addProperties({
                layoutMargin:0,
                layoutLeftMargin:0,
                layoutRightMargin:0,
                layoutTopMargin:0,
                membersMargin: 0,
                colorButtonSize: 22,
                headerStyle: "colorPickerHeader",
                hiliteHeaderStyle: "colorPickerHeader",
                headerHeight: 14,
                styleName: "colorPicker",
                bodyConstructor: "VLayout",
                bodyStyle: "colorPickerBody",
                showShadow: true,
                shadowDepth: 10,
                shadowColor: "#f0f0f0"
            });
            isc.ColorPicker.changeDefaults("headerLabelDefaults", {                
                styleName: "colorPickerHeaderText"
            });
            isc.ColorPicker.changeDefaults("bodyDefaults", {
                layoutMargin: 0,
                layoutLeftMargin:0,
                layoutRightMargin:0,
                layoutTopMargin:0,
                margin: 0,
                padding: 0
            });
            
            isc.ColorPicker.changeDefaults("closeButtonDefaults", {
                src: "[SKIN]ColorPicker/close_button.png"
            });
            isc.ColorPicker.changeDefaults("headerIconDefaults", {
                width: 18, height: 18,
                top: 12,
                align: "absmiddle",
                valign: "center"
            });
            
            isc.ColorPicker.changeDefaults("okButtonDefaults", { width: 90 })
            isc.ColorPicker.changeDefaults("cancelButtonDefaults", { width: 90 })
            isc.ColorPicker.changeDefaults("modeToggleButtonDefaults", { width: 90 })

            
            isc.ColorPicker.changeDefaults("buttonLayoutDefaults", {
                membersMargin: 5,
                paddingTop: 5,
                styleName: "colorPickerButtonLayout",
                layoutAlign: "left",
                width: "100%"
            });
            isc.ColorPicker.changeDefaults("contentLayoutDefaults", {
                layoutMargin: 10,
                membersMargin: 8
            });
            isc.ColorPicker.changeDefaults("innerContentLayoutDefaults", {
                styleName: "colorPickerInnerContent",
                membersMargin: 10
            });
        }
        
        if (isc.ColorItem) {
            isc.ColorItem.addProperties({
                width: 220,
                // not clear why this is being switched off
                supportsTransparency: true,
                showOverIcons: false,
                updatePickerIconOnOver: false
            });
        }
        
        if (isc.HiliteRule) {
            isc.HiliteRule.changeDefaults("hiliteFormDefaults", {
                titleOrientation: "top",
                titleSuffix: "",
                numCols: 3,
                width: 270,
                colWidths: [105, 105, 60]
            });
            isc.HiliteRule.changeDefaults("clauseDefaults", {
                layoutAlign: "bottom",
                width: 400
            });
        }

        if (isc.AdvancedHiliteEditor) {
            isc.AdvancedHiliteEditor.changeDefaults("hiliteFormDefaults", {
                titleOrientation: "top",
                titleSuffix: "",
                numCols: 4,
                colWidths: [150, 105, 105, 60],
                width: "100%"
            });
            isc.AdvancedHiliteEditor.changeDefaults("filterBuilderDefaults", {
                fieldPickerWidth: 150
            });
        }
        
        if (isc.Canvas) {
            isc.Canvas.changeDefaults("hiliteWindowDefaults", {
                width: 850
            });
        }

    }

    if (isc.HiliteEditor) {
        isc.HiliteEditor.addProperties({
            padding: 5
        });
    }
    
    if (isc.Dialog) {
        isc.Dialog.addProperties({
            bodyColor:"#FFFFFF",
            bodyStyle:"windowBody",
            leaveHeaderGap:true,
            layoutBottomMargin:0,
            modalMaskOpacity:10,
            membersMargin:0,
            styleName:"windowBackground",
            showHeaderBackground:false,
            showFooter:false,
            footerHeight: 30
        });

        // even though Dialog inherits from Window, we need a separate changeDefaults block
        // because Dialog defines its own toolbarDefaults
        isc.Dialog.changeDefaults("toolbarDefaults", {
            buttonConstructor:"IButton",
            height:42, // 10px margins + 22px button
            membersMargin:10
        });

        
        isc.Dialog.changeDefaults("bodyDefaults", {
            layoutTopMargin:10,
            layoutLeftMargin:15,
            layoutRightMargin:15,
            layoutBottomMargin:10
        });

        if (isc.Dialog.Warn && isc.Dialog.Warn.toolbarDefaults) {
            isc.addProperties(isc.Dialog.Warn.toolbarDefaults, {
                buttonConstructor:"IButton",
                height:42,
                membersMargin:10
            });
        }

        // Modify the prompt dialog to show a header
        // In the css3-off mode header media is part of the background image, so
        // a header appears to show even though there's no true header widget.
        if (isc.Dialog.Prompt) {
            isc.addProperties(isc.Dialog.Prompt, {
                showHeader:true,
                showTitle:false,
                showCloseButton:false,
                bodyStyle:"windowBody"

            });
        }
        if (isc.Dialog.Warn) {
            if (isc.Browser.isTouch) isc.Dialog.Warn.showModalMask = true;
        }
        if (isc.Dialog.Prompt) {
            if (isc.Browser.isTouch) isc.Dialog.Prompt.showModalMask = true;
        }

    }
    
    //----------------------------------------
    // 8) Menus
    //----------------------------------------
    if (isc.Menu) {
        isc.Menu.addProperties({
            styleName: "menuBorder",
            iconFillSpaceStyleName: "menuFill"
        });
    }
    if (isc.Menu) {
        isc.Menu.addProperties({
            activeParentStyle:"Selected",
            bodyBackgroundColor:null,
            bodyStyleName:"gridBody",
            alternateFieldStyles:false,
            showSubmenuOpened:true,
            cellHeight:33,
            checkmarkDisabledImage:{src:"[SKIN]check_disabled.png", width:18, height:13},
            checkmarkImage:{src:"[SKIN]check.png", width:18, height:13},
            fastCellUpdates:false,
            iconBodyStyleName:"menuMain",
            shadowDepth:5,
            showEdges:false,
            showShadow:false,
            submenuDisabledImage:{src:"[SKIN]submenu_disabled.png", height:14, width:19},
            submenuImage:{src:"[SKIN]submenu.png", height:14, width:19},
            skinUsesCSSTransitions: true
        });

        isc.Menu.changeDefaults("iconFieldDefaults", {
            baseStyle:"menuIconField",
            width:24
        });

        isc.Menu.changeDefaults("titleFieldDefaults", {
            baseStyle: "menuTitleField"
        });
    }


    //----------------------------------------
    // 9) ListGrids
    //----------------------------------------
    if (isc.ListGrid) {
        isc.ListGrid.addProperties({
            expansionFieldImageShowRTL: true
        });

        isc.ListGrid.addProperties({
            booleanTrueImage: "sprite:cssClass:checkboxTrue;size:25,25",
            booleanFalseImage: "sprite:cssClass:checkboxFalse;size:25,25",
            booleanPartialImage: "sprite:cssClass:checkboxPartial;size:25,25",

            printBooleanBaseStyle: "printCheckbox",
            printBooleanTrueImage: "[SKINIMG]/DynamicForm/cb-checked-normal.png",
            printBooleanFalseImage: "[SKINIMG]/DynamicForm/cb-uncheck-normal.png",
            printBooleanPartialImage: "[SKINIMG]/DynamicForm/cb-anything-normal.png"
        });

        // TreeGrid does not support booleanBaseStyle.
        if (isc.TreeGrid) {
            isc.TreeGrid.addProperties({
                booleanTrueImage: "sprite:cssClass:checkboxTrue;size:25,25",
                booleanFalseImage: "sprite:cssClass:checkboxFalse;size:25,25",
                booleanPartialImage: "sprite:cssClass:checkboxPartial;size:25,25"
            });
        }
    }
    if (isc.ListGrid) {
        isc.ListGrid.addProperties({
            alternateRecordStyles:true,
            alternateFieldStyles:true,
            baseStyle:null,
            suffixEditorTypeBaseStyle:"EditorType",
            embeddedComponentIndent:34,
            embeddedComponentIndentOtherMargins:5,
            alternateBodyStyleName:null,
            backgroundColor:null,
            minimumCellHeight:24,
            cellHeight:22,
            filterEditorHeight:24,
            checkboxFieldImageHeight:22,
            checkboxFieldImageWidth:22,
            booleanImageWidth:22,
            booleanImageHeight:22,
            editFailedCSSText:"color:FF6347;",
            errorIconSrc:"[SKINIMG]actions/exclamation.png",
            expansionFieldImageHeight:16,
            expansionFieldImageWidth:16,
            expansionFieldFalseImage:"[SKINIMG]/ListGrid/row_collapsed.png",
            expansionFieldTrueImage:"[SKINIMG]/ListGrid/row_expanded.png",
            expansionFieldImageWidth: 16,
            expansionFieldImageHeight: 16,
            groupIcon:"[SKINIMG]/ListGrid/group.png",
            groupIconPadding:7,
            groupLeadingIndent:10,
            showHeaderShadow:false,
            headerBackgroundColor:null,
            headerBaseStyle:"headerButton",
            spannedHeaderBaseStyle: "spannedHeaderButton",
            spannedHeaderMenuBaseStyle: "spannedHeaderButton",
            headerHeight:28,//38
            headerMenuButtonIcon:"[SKINIMG]ListGrid/sort_descending.png",
            headerMenuButtonConstructor:"HeaderMenuButton",
            headerMenuButtonWidth:17,

            // default Date, Time, and DateTime listGridField widths
            defaultDateFieldWidth: 65,
            defaultDateTimeFieldWidth: 98,
            defaultEditableDateFieldWidth: 114,
            defaultEditableDateTimeFieldWidth: 150,
            defaultTimeFieldWidth: 75,

            normalCellHeight:22,
            showHeaderMenuButton:true,
            sortAscendingImage:{src:"[SKINIMG]ListGrid/sort_ascending.png", width:9, height:6},
            sortDescendingImage:{src:"[SKINIMG]ListGrid/sort_descending.png", width:9, height:6},
            summaryRowHeight: 24, // should be cellHeight + top/bottom borders
            // enforce baseStyle, so that tallCell doesn't get used with the rollOverCanvas
            baseStyle: "cell",
            showSelectedRollOverCanvas: true,
            showRollOverInExpansion: true,
            removeIconSize:13
        });

        isc.ListGrid.changeDefaults("defaultFieldWidthScaleFactors", { 
            EditableDate: 5, EditableDateTime: 7 
        });
        
        isc.ListGrid.changeDefaults("rollOverCanvasDefaults", { styleName: "gridSelectionOver" });
        isc.ListGrid.changeDefaults("operatorIconDefaults", { width: 18 });

        isc.ListGrid.changeDefaults("sorterDefaults", {
            baseStyle:"sorterButton",
            showRollOver:false
        });

        isc.ListGrid.changeDefaults("headerMenuButtonDefaults", {
            showFocusedAsOver:true
        });

        isc.ListGrid.changeDefaults("headerButtonDefaults", {
            showFocusedAsOver:true
        });
        isc.ListGrid.changeDefaults("headerSpanDefaults", {
            baseStyle:"headerSpanButton"
        });
    }
    
    if (isc.GridRenderer) {
        isc.GridRenderer.addProperties({
            useMinHeightForHR:"HEIGHT:1px;padding-bottom:0px;padding-top:0px;border-bottom:0px;border-top:0px;",
            showSubmenuOpened:true
        });
    }

    if (isc.RecordEditor) {
        isc.RecordEditor.changeDefaults("actionButtonDefaults", {
            border:"1px solid #b8b8b8"
        });
        isc.RecordEditor.changeDefaults("editFormDefaults", {
            // Enable auto-child pattern customization of FormItems on the edit form.
            autoChildItems:true,            
            CheckboxItemDefaults : {
                sizeToCheckboxImage:false,
                applyHeightToTextBox:true,
                clipValue:true,
                textBoxStyle:"checkboxItemLite"
            }
        });
    }
    
    //----------------------------------------
    // 10) TreeGrids
    //----------------------------------------

        
    if (isc.TreeGrid) {
        isc.TreeGrid.addProperties({
            alternateRecordStyles:false,
            alternateFieldStyles:false,
            folderIcon:"[SKIN]folder.png",
            manyItemsImage:"[SKIN]folder_file.png",
            nodeIcon:"[SKIN]file.png",
            baseStyle: "treeCell",
            normalBaseStyle:"treeCell",
            applyRowNumberStyle:false,
            openerIconSize:19,
            openerImage:"[SKIN]opener.png",
            sortAscendingImage:{src:"[SKINIMG]ListGrid/sort_ascending.png", width:9, height:6},
            sortDescendingImage:{src:"[SKINIMG]ListGrid/sort_descending.png", width:9, height:6},
            tallBaseStyle:"treeTallCell"
        });
        isc.TreeGrid.changeDefaults("rollOverCanvasDefaults", { styleName: "treeSelectionOver" });
    }
    
    //----------------------------------------
    // 11) Form controls
    //----------------------------------------
    if (isc.FormItem) {
        isc.FormItem.addClassProperties({
            defaultPickerIconSpace: 4 
        });
        isc.FormItem.addProperties({
            showRTL: true,
            showOver: true,
            showFocusedErrorState: true
        });
    }

    if (isc.CheckboxItem) {
    
        isc.CheckboxItem.addProperties({
            valueIconWidth: 22,
            valueIconHeight: 22,
            showValueIconOver: true,
            showValueIconFocused: true
        });

        isc.CheckboxItem.addProperties({
            checkedImage: "sprite:cssClass:checkboxTrue;size:25,25",
            uncheckedImage: "sprite:cssClass:checkboxFalse;size:25,25",
            // For Tahoe there is no default "unset" image
            unsetImage: "sprite:cssClass:checkboxPartial;size:25,25",
            partialSelectedImage: "sprite:cssClass:checkboxPartial;size:25,25",

            printCheckedImage: "[SKINIMG]/DynamicForm/cb-checked-normal.png",
            printUncheckedImage: "[SKINIMG]/DynamicForm/cb-uncheck-normal.png",
            printUnsetImage: "[SKINIMG]/DynamicForm/cb-anything-normal.png",
            printPartialSelectedImage: "[SKINIMG]/DynamicForm/cb-anything-normal.png",
            printBooleanBaseStyle: "printCheckbox"
        });
    }

    if (isc.ComboBoxItem) {
        isc.ComboBoxItem.addProperties({
            pickListTallBaseStyle: "tallPickListCell"
        });
        
        isc.ComboBoxItem.changeDefaults("pickerIconDefaults", {
            showOver: false,
            showRTL: false
        });
        
        isc.ComboBoxItem.changeDefaults("separateValuesListDefaults", {
            showOverAsSelected: false
        });
        
        isc.ComboBoxItem.addProperties({
            pickerIconSrc:"sprite:cssClass:comboBoxItemPicker;size:16,32;offset:0,12;",
            pickerIconHeight:20,
            pickerIconWidth:10,
            pickerIconStyle:"selectItemPickerIcon"
        });
        
        if (!isc.Browser.isIE || isc.Browser.isIE11) {
            isc.ComboBoxItem.changeDefaults("pickerSearchFormDefaults", {
                height: 30
            });
            isc.ComboBoxItem.changeDefaults("pickerSearchFieldDefaults", {
                textBoxStyle: "pickerSearchBox",
                icons: [{
                    name: "search",
                    inline: true,
                    imgOnly: true,
                    src: "[SKINIMG]DynamicForm/search_icon~2.png",
                    width: 14,
                    height: 15,
                    showRTL: true,
                    click : function (form, item, icon) {
                        item.focusInItem();
                    }
                }]
            });
        }
    }

    if (isc.MultiComboBoxItem) {
        isc.MultiComboBoxItem.addProperties({
            pendingButtonStyle: "buttonPending",
            deselectedButtonStyle: "buttonDeselected",
            valueLayoutProperties: {
                membersMargin: 1,
                tileMargin: 1
            }
        });
        isc.MultiComboBoxItem.changeDefaults("buttonDefaults", {
            icon: "[SKIN]DynamicForm/drop.png",
            iconWidth: 12,
            iconHeight: 12,
            iconSize: 12
        });
    }

    if (isc.PickTreeItem) {
        isc.PickTreeItem.addProperties({
            pendingButtonStyle: "menuButtonPending"
        });
        isc.PickTreeItem.changeDefaults("buttonDefaults", {
            height: 21
        });
    }

    if (isc.SelectItem) {
        isc.SelectItem.addProperties({
            pickListTallBaseStyle: "tallPickListCell"
        });
        isc.SelectItem.changeDefaults("pickerIconDefaults", {
            showOver: true,
            showRTL: true
        });
        isc.SelectItem.changeDefaults("separateValuesListDefaults", {
            showOverAsSelected: false
        });
                
        isc.SelectItem.addProperties({
            // Media is 14w by 8h - making the image element taller and and slightly wider
            // while maintaining scale and v-centering so there's a larger clickable area
            pickerIconSrc:"sprite:cssClass:comboBoxItemPicker;size:16,32;offset:0,12;",
            pickerIconHeight:20,
            pickerIconWidth:10,
            pickerIconStyle:"selectItemPickerIcon"
        });
    }

    if (isc.SpinnerItem) {
        isc.SpinnerItem.addProperties({
            width: 220,
            height:22,
            stackedIconsWidth:14,
            stackedIconsHeight:20,
            
            showOver:true,
            updateTextBoxOnOver:false,
            updateControlOnOver:true,
            textBoxStyle:"selectItemLiteText",
            controlStyle:"selectItemLiteControl",
            unstackedTextBoxStyle: "textItemLite",
            getUpdateTextBoxOnOver : function () {
                return !this.usingStackedMode();
            },
            getUpdateControlOnOver : function () {
                return this.usingStackedMode();
            }
        });

        isc.SpinnerItem.changeDefaults("increaseIconDefaults", {
            src:"sprite:cssClass:spinnerItemIncrease;size:16,16",
            showOver:true,
            showFocused:true,
            showFocusedWithItem:false,
            showRTL:true
        });
        isc.SpinnerItem.changeDefaults("decreaseIconDefaults", {
            src:"sprite:cssClass:spinnerItemDecrease;size:16,16",
            showOver:true,
            showFocused:true,
            showFocusedWithItem:false,
            showRTL:true
        });
        isc.SpinnerItem.changeDefaults("unstackedIncreaseIconDefaults", {
            src: "blank",
            baseStyle: "unstackedSpinnerItemIncrease",
            width: 36,
            height: 30,
            showFocused: true,
            showRTL: true
        });
        isc.SpinnerItem.changeDefaults("unstackedDecreaseIconDefaults", {
            src: "blank",
            baseStyle: "unstackedSpinnerItemDecrease",
            width: 36,
            height: 30,
            showFocused: true,
            showRTL: true
        });
    }

    if (isc.RelativeDateItem) {
        isc.RelativeDateItem.addProperties({
            width: 220,
            valueFieldWidth:190,
            quantityFieldWidth: 60,
            pickerIconWidth: 20, pickerIconHeight: 20            
        });
        isc.RelativeDateItem.changeDefaults("pickerIconDefaults", {
            neverDisable: false,
            showOver: true,
            src: "[SKIN]/DynamicForm/date_control.png"
        });
    }

    if (isc.RichTextEditor) {
        isc.RichTextEditor.addProperties({
            showEdges:false,
            styleName:"richTextEditorBorder",
            toolbarBackgroundColor: "#f0f0f0"
            ,
            defaultControlConstructor: 'ToolStripButton'
            //. styleName
        });
        isc.RichTextEditor.changeDefaults("toolbarDefaults", {
            layoutMargin: 6,
            membersMargin: 6
        });
        
        isc.RichTextEditor.changeDefaults("boldSelectionDefaults", {
            icon: "[SKIN]/RichTextEditor/text_bold.png",
            title: null
        });
        isc.RichTextEditor.changeDefaults("italicSelectionDefaults", {
            icon: "[SKIN]/RichTextEditor/text_italic.png",
            title: null
        });
        isc.RichTextEditor.changeDefaults("underlineSelectionDefaults", {
            icon: "[SKIN]/RichTextEditor/text_underline.png",
            title: null
        });
        
        isc.RichTextEditor.changeDefaults("editAreaDefaults", {
            padding: 10
        });
    }
    
    if (isc.SectionHeader) {
        isc.SectionHeader.addProperties({
            icon:"[SKIN]/SectionHeader/opener.png"
            //iconOrientation:"right"
        });
    }

    if (isc.FormItem) {
        isc.FormItem.addProperties({
            defaultIconSrc:"[SKIN]/DynamicForm/default_formItem_icon.png",
            errorIconSrc:"[SKINIMG]actions/exclamation.png",
            iconHeight:18,
            iconVAlign:"middle",
            iconWidth:18
        });
    }

    if (isc.TextItem) {
        isc.TextItem.addProperties({
            height:22,
            showFocused:true,
            textBoxStyle:"textItemLite"
        });
        isc.TextItem.addProperties({
                width: 220
        });
    }

    if (isc.TextAreaItem) {
        isc.TextAreaItem.addProperties({
            showFocused:true,
            textBoxStyle:"textAreaItemLite",
            pickerIconHeight:18
        });
        isc.TextAreaItem.addProperties({
            width: 220
        });
    }

    if (isc.SelectItem) {

        isc.SelectItem.addProperties({
            showOver:true,
            updateTextBoxOnOver:false,
            updateControlOnOver:true,
            
            height:22,
            pickListHeaderHeight:28,
            valueIconSize:20,
            showFocusedPickerIcon:false,
            textBoxStyle:"selectItemLiteText",
            controlStyle:"selectItemLiteControl"
        });
        
        
        isc.SelectItem.addProperties({
            width: 220
        });
        
        if (isc.NativeSelectItem) {
            isc.NativeSelectItem.addProperties({
                width: 220
            });
        }
    }
    
    if (isc.ComboBoxItem) {
        isc.ComboBoxItem.addProperties({
            // we have 'showOver' explicitly set to true for the picker-icon
            // This will hilight the chevron as the user rolls over that icon only
            showOver:true,
            updateTextBoxOnOver:false,
            updateControlOnOver:true,
            
            height:22,
            pickListHeaderHeight:28,
            showFocusedPickerIcon:false,
            
            pendingTextBoxStyle:"selectItemLiteTextPending",
            textBoxStyle:"selectItemLiteText",
            controlStyle:"selectItemLiteControl"
        });
    
    }
    // used by SelectItem and ComboBoxItem for flat list data
    if (isc.ScrollingMenu) {
        isc.ScrollingMenu.addProperties({
            shadowDepth:5,
            showShadow:false,
            showSelectedRollOverCanvas:false
        });
    }

    if (isc.PickListMenu) {
        isc.PickListMenu.addProperties({
            skinUsesCSSTransitions: true,
            alternateFieldStyles:false,
            alternateRecordStyles:false,
            showOverAsSelected: false,
            styleName: "listGrid",
            bodyStyleName: "gridBody"
        });
    }
    
    // used by SelectItem and ComboBoxItem for tree data
    if (isc.ScrollingTreeMenu) {
        isc.ScrollingTreeMenu.addProperties({
            shadowDepth:5,
            showShadow:false,
            showSelectedRollOverCanvas:false
        });
    }

    if (isc.PickTreeMenu) {
        isc.PickTreeMenu.addProperties({
            skinUsesCSSTransitions: true,
            alternateFieldStyles:false,
            alternateRecordStyles:false,
            showOverAsSelected: false
        });
    }

    if (isc.PickList) {
        isc.PickList.addInterfaceProperties({
            pickListCellHeight:17
        });
        if (isc.SelectItem) {
            isc.SelectItem.addProperties({
                pickListCellHeight:17
            });
        }
        if (isc.ComboBoxItem) {
            isc.ComboBoxItem.addProperties({
                pickListCellHeight:17
            });
        }
    }

    if (isc.DateItem) {
        isc.DateItem.addProperties({
            width: 220,
            height:22,
            monthSelectorProperties: {width:63},
            daySelectorProperties: {width:51},
            yearSelectorProperties: {width:69},
            pickerIconWidth:20,
            pickerIconHeight:20,
            pickerIconSrc:"[SKIN]/DynamicForm/date_control.png",
            showOver:true,
            showFocused:true
        });
        isc.DateItem.addProperties({
            textBoxStyle:"textItemLite"
        });
    if (isc.NativeDateItem) {
        isc.NativeDateItem.addProperties({
            textBoxStyle:"textItemLite"
        });
    }
    if (isc.NativeTimeItem) {
        isc.NativeTimeItem.addProperties({
            textBoxStyle:"textItemLite"
        });
    }
    if (isc.NativeDatetimeItem) {
        isc.NativeDatetimeItem.addProperties({
            textBoxStyle:"textItemLite"
        });
    }
    }
    if (isc.MiniDateRangeItem) {
        isc.MiniDateRangeItem.addProperties({
            "pickerIconSrc": "[SKIN]/DynamicForm/date_control.png",
            pickerIconWidth:20,
            pickerIconHeight:20,
            width: 218,
            height: 22,
            textBoxStyle:"spinnerItemLiteText",
            controlStyle:"spinnerItemLiteControl" 
        });
    }
    
    if (isc.TimeItem) {
        isc.TimeItem.addProperties({
            width: 220,
            height:22,
            hourItemProperties: {width:70},
            minuteItemProperties: {width:70},
            secondItemProperties: {width:70}
        });
    }

    if (isc.SliderItem) {
        isc.SliderItem.addProperties({
            width:220
        });
    }

    if (isc.PopUpTextAreaItem) {
        isc.PopUpTextAreaItem.addProperties({
            popUpIconHeight:16,
            popUpIconSrc:"[SKIN]/DynamicForm/text_control.gif",
            popUpIconWidth:16
        });
    }
    if (isc.ButtonItem && isc.IButton) {isc.ButtonItem.addProperties({
        showFocused:true,
        showFocusedAsOver:false,
        showFocusOutline:false,
        buttonConstructor:isc.IButton,
        height:22
    })}


    if (isc.ToolbarItem && isc.IAutoFitButton) {
        isc.ToolbarItem.addProperties({
            buttonConstructor:isc.IAutoFitButton,
            buttonProperties:{ autoFitDirection:isc.Canvas.BOTH }
        });
    }

    if (isc.DateRangeDialog) {
        isc.DateRangeDialog.changeDefaults("headerIconProperties", { src:"[SKIN]/DynamicForm/date_control.png" });
        isc.DateRangeDialog.changeDefaults("mainLayoutDefaults", { width: 450 });
    }

    if (isc.LinkItem) {
        isc.LinkItem.addProperties({ linkTextClass: "linkText" });
    }

    if (isc.UploadItem) {
        isc.UploadItem.addProperties({ showOver: false });
    }

    //----------------------------------------
    // 12) DateChooser
    //----------------------------------------

    if (isc.DateGrid) {
        isc.DateGrid.addProperties({
            minFieldWidth:33,
            cellHeight:27,
            alternateFieldStyles: false,
            fiscalYearColWidth: 45
        });
    }
 
    if (isc.DateChooser) {
        isc.DateChooser.addProperties({
            alternateWeekStyles:false,
            backgroundColor:null,
            baseNavButtonStyle:"dateChooserNavButton",
            baseWeekdayStyle:"dateChooserWeekday",
            baseWeekendStyle:"dateChooserWeekend",
            baseBottomButtonStyle:"button",
            headerStyle:"dateChooserButton",
            nextMonthIcon:"[SKINIMG]/DateChooser/arrow_right.png",
            nextMonthIconHeight:16,
            nextMonthIconWidth:16,
            nextYearIcon:"[SKINIMG]/DateChooser/doubleArrow_right.png",
            nextYearIconHeight:16,
            nextYearIconWidth:16,
            prevMonthIcon:"[SKINIMG]/DateChooser/arrow_left.png",
            prevMonthIconHeight:16,
            prevMonthIconWidth:16,
            prevYearIcon:"[SKINIMG]/DateChooser/doubleArrow_left.png",
            prevYearIconHeight:16,
            prevYearIconWidth:16,
            showDoubleYearIcon:false,
            showEdges:false,
            skinImgDir:"images/DateChooser/",
            weekendHeaderStyle:"dateChooserWeekendButton",
            styleName:"dateChooserBorder",
            borderCalendar:0,
            width:236,
            timeLayoutIsVisibleWidth:336,
            timeLayoutIsVisibleMinFieldWidth:47,
            monthMenuFormat:"MMMM",
            weekHeaderStyle: "dateChooserButton",
            fiscalYearHeaderStyle: "dateChooserButton"

        });
        isc.DateChooser.changeDefaults("timeLayoutDefaults", { 
            styleName:"dateChooserBorderTop", width: "100%", align:"center"
        });
        isc.DateChooser.changeDefaults("todayButtonDefaults", { 
            autoFit: false,
            overflow: "visible",
            width: 98,
            minWidth: 98
        });
        isc.DateChooser.changeDefaults("cancelButtonDefaults", { 
            autoFit: false,
            overflow: "visible",
            width: 98,
            minWidth: 98
        });
        isc.DateChooser.changeDefaults("applyButtonDefaults", { 
            autoFit: false,
            overflow: "visible",
            width: 98,
            minWidth: 98
        });
        isc.DateChooser.changeDefaults("buttonLayoutDefaults", { 
            membersMargin:10, 
            layoutMargin: 0, 
            layoutTopMargin:4, 
            styleName:"dateChooserBorderTop",
            height:44, width:"100%"
        });
        isc.DateChooser.changeDefaults("navigationLayoutDefaults", { 
            styleName:"dateChooserBorderBottom",
            height:42,
            layoutMargin: 0,
            width:"100%" 
        });
        isc.DateChooser.changeDefaults("weekChooserButtonDefaults", { height:45, minWidth: 30, width:30, align:"right", showFocusedAsOver: true });
        isc.DateChooser.changeDefaults("fiscalYearChooserButtonDefaults", { height:45, minWidth: 45, width:45, align:"right", showFocusedAsOver: true });
        isc.DateChooser.changeDefaults("previousYearButtonDefaults", { height:45, width:30, align:"right", showFocusedAsOver: true });
        isc.DateChooser.changeDefaults("previousMonthButtonDefaults", { height:45, width:30, align:"left", showFocusedAsOver: true });
        isc.DateChooser.changeDefaults("monthChooserButtonDefaults", { height:45 });
        isc.DateChooser.changeDefaults("yearChooserButtonDefaults", { height:45 });
        isc.DateChooser.changeDefaults("nextMonthButtonDefaults", { height:45, width:30, align:"right", showFocusedAsOver: true });
        isc.DateChooser.changeDefaults("nextYearButtonDefaults", { height:45, width:30, align:"left", showFocusedAsOver: true });
    }
   
    //----------------------------------------
    // 13) ToolStrip
    //----------------------------------------

    if (isc.ToolStrip) {
        isc.ToolStrip.addProperties({
            defaultLayoutAlign:"center",
            verticalStyleName: "vToolStrip",
            membersMargin: 2,
            // ToolStripButton default height is 22 - add 10 for 4px padding plus 1px border
            // per side
            height:32
        });
        isc.ToolStrip.changeDefaults("formWrapperDefaults",{cellPadding:3});
        
        if (isc.RibbonBar) {
            isc.RibbonBar.addProperties({ 
                styleName: "ribbonBar",
                layoutMargin: 8,
                membersMargin: 10
            });
        }
    }

    if (isc.ToolStripGroup) {
        // apply 4px padding around the body and between each column/control
        isc.ToolStripGroup.changeDefaults("bodyDefaults",{layoutMargin: 4, membersMargin: 4});
        isc.ToolStripGroup.changeDefaults("columnLayoutDefaults",{membersMargin: 4});
        
        if (isc.RibbonGroup) {
            isc.RibbonGroup.addProperties({ 
                styleName: "ribbonGroup"
            });
        }
    }

    if (isc.ToolStripMenuButton) {
        
        isc.overwriteClass("ToolStripMenuButton", "MenuButton").addProperties({
            autoFit:true,
            baseStyle:"toolStripButton",
            height:22,
            labelVPad:0,
            iconWidth:10,
            iconHeight: 6,
            menuButtonImage:"[SKIN]menuButton_down.png",
            menuButtonImageUp:"[SKIN]menuButton_down.png",
            showDown:true,
            showRollOver:true,
            showTitle:false
        });
    }

    if (isc.ToolStripButton) {
        
        isc.overwriteClass("ToolStripButton", "Button").addProperties({
            autoFit:true,
            baseStyle:"toolStripButton",
            iconSize: 15,
            height:17,
            labelVPad:0,
            showTitle:false,
            showRollOver:true,
            showDown:true,
            showDownIcon: true,
            showSelectedIcon: true,
            title:null
        });
    }

    if (isc.EdgedCanvas) {
        isc.EdgedCanvas.addProperties({
            edgeSize:3,
            edgeImage: "[SKINIMG]edges/edge.png"
        })
    }

    //----------------------------------------
    // 14) Sliders
    //----------------------------------------
    if (isc.Slider) {
        isc.Slider.changeDefaults("rangeLabelDefaults", {
            showDisabled: true
        });
        isc.Slider.changeDefaults("valueLabelDefaults", {
            showDisabled: true
        });
    }
    if (isc.Slider) {
        isc.Slider.addProperties({
            hThumbStyle:"hSliderThumb",
            hTrackStyle:"hSliderTrack",
            thumbConstructor:"StatefulCanvas",
            thumbThickWidth:18,
            thumbThinWidth:18,
            trackConstructor:"StatefulCanvas",
            trackWidth: 11,
            vThumbStyle:"vSliderThumb",
            vTrackStyle:"vSliderTrack",
            touchThumbThickWidth:30,
            touchThumbThinWidth:30,
            touchExtraThumbSpace:0,
            hValueStyle: "hSliderValue",
            vValueStyle: "vSliderValue",
            hLabelSpacing: 1,
            vLabelSpacing: 7,
            titleSpacing: 15,
            vTitleSpacing: 7,
            showActiveTrack: true,
            labelHeight: 22
        });
        isc.Slider.changeDefaults("thumbDefaults", {
            getCustomState : function () {
                return (isc.Browser.isTouch ? "touch" : this.customState);
            }
        });
    }

   
    //----------------------------------------
    // 15) TileGrid
    //----------------------------------------
    if (isc.TileGrid) {
        isc.TileGrid.addProperties({
            showEdges:false,
            styleName:null,
            valuesShowRollOver:true
        });
    }
    
   
    //----------------------------------------
    // 16) RichTextEditor
    //----------------------------------------
    if (isc.RichTextEditor) {
        isc.RichTextEditor.addProperties({ showGroupSeparators: false });
        isc.RichTextEditor.changeDefaults("toolAreaDefaults", { styleName: "richTextEditorToolArea" });
        isc.RichTextEditor.changeDefaults("toolbarDefaults", { layoutMargin: 0, membersMargin: 0 });
        
        isc.RichTextEditor.changeDefaults("colorDefaults", { 
            showSelected: true, 
            actionType: "checkbox",
            icon: "[SKIN]/RichTextEditor/text_color.png"
        });
        isc.RichTextEditor.changeDefaults("backgroundColorDefaults", { 
            showSelected: true, 
            actionType: "checkbox",
            icon: "[SKIN]/RichTextEditor/background_color.png"
        });
    }
   
    //----------------------------------------
    // 17) Calendar
    //----------------------------------------

    if (isc.Calendar) {
        isc.Calendar.changeDefaults("datePickerButtonDefaults", {
            showDown:false,
            showOver:false,
            src:"[SKIN]/DynamicForm/date_control.png"
        });

        isc.Calendar.changeDefaults("controlsBarDefaults", {
            height:10,
            layoutBottomMargin:10
        });

        isc.EventWindow.changeDefaults("resizerDefaults", {
            src:"[SKIN]/Window/v_resizer.png"
        });
        isc.TimelineWindow.changeDefaults("resizerDefaults", {
            src:"[SKIN]/Window/h_resizer.png"
        })

    }

    //----------------------------------------
    // 18) CubeGrid
    //----------------------------------------
    if (isc.CubeGrid) {
        isc.CubeGrid.addProperties({
            alternateFieldStyles:false,
            arrowIconSize:14
        });
        isc.CubeGrid.changeDefaults("rollOverCanvasDefaults", { styleName: "cubeSelectionOver" });
    }    
    
    // -------------------------------------------
    // 19) Printing
    // -------------------------------------------
    if (isc.PrintWindow) {
        isc.PrintWindow.changeDefaults("printButtonDefaults", {
            height: 27
        });
    }
    // -------------------------------------------
    // 20) SplitPane
    // -------------------------------------------
    if (isc.SplitPanePagedPanel) {
        isc.SplitPanePagedPanel.addProperties({
            skinUsesCSSTransitions: true
        });
    }
    if (isc.SplitPaneSidePanel) {
        isc.SplitPaneSidePanel.addProperties({
            skinUsesCSSTransitions: true
        });
    }
    if (isc.SplitPane) {
        isc.SplitPane.addProperties({
            desktopNavigationBarHeight: 35
        });
        isc.SplitPane.changeDefaults("backButtonDefaults", {
            icon: "[SKINIMG]NavigationBar/back_arrow~2.png",
            iconWidth: 14,
            iconHeight: 24,
            iconSpacing: 7,
            showRTLIcon: true
        });
        if (isc.Browser.isIPhone || isc.Browser.isIPad) {
            isc.SplitPane.changeDefaults("backButtonDefaults", {
                icon: "[SKINIMG]NavigationBar/back_arrow.svg"
            });
        }

        isc.SplitPane.changeDefaults("detailTitleLabelDefaults", {
            baseStyle: "detailPaneTitle"
        });
        isc.SplitPane.changeDefaults("listTitleLabelDefaults", {
            baseStyle: "listPaneTitle"
        });
        isc.SplitPane.changeDefaults("navigationBarDefaults", {
            navBarHeaderStyleName: "navBarHeaderPaneTitle"
        });
    }

    // -------------------------------------------
    // 21) Drawing
    // -------------------------------------------
    if (isc.Gauge) {
        isc.Gauge.addProperties({
            fontSize: 11,
            needleColor: "#4e4e4e"
        });
        isc.Gauge.changeDefaults("valueLabelDefaults", {
            fontFamily: "Arial",
            fontWeight: "normal",
            lineColor: "#4e4e4e"
        });
    }

    // -------------------------------------------
    // 22) FacetChart
    // -------------------------------------------
    if (isc.FacetChart) {
        isc.FacetChart.addProperties({
             // General Chart changes
            padding: 0,       
            titleProperties: {
                fontFamily: "Arial",
                fontSize: 12,
                fontWeight: "bold",
                fontColor: "#616161"
            },
            titleBackgroundProperties: {
                lineWidth: 0,
                lineOpacity: 0,
                lineColor: "#cccccc",
                fillColor: "#f0f0f0"
            },
            titleAlign: "left",
            titlePadding: 10,
            drawTitleBackground: true,
            drawTitleBoundary: true,
            titleBoundaryProperties: {
                lineColor: "#cccccc",
                lineWidth: 1
            }, 
            titleRectHeight: 32, 
             legendAlign: "right",
             drawLegendBoundary: true,
             legendBoundaryProperties: {
                lineColor: "#cccccc",
                lineWidth: 1
            }, 
             legendRectProperties : {
                lineWidth:1,
                lineOpacity:0,
                lineColor: "#cccccc"
             },
             legendPadding:12,
             // Just replace the border with a white border to give the impression there isn't one
             legendSwatchProperties : {
                lineWidth:0,
                lineColor:"#FFFFFF"
            },
            showLegendRect:true,
            // embed the gradation labels spacing properly
            gradationLabelPadding:10,
            chartRectMargin: 15,
            // Change the Background Banding Color
            backgroundBandProperties : {
                excludeFromQuadTree:true,
                lineOpacity: 0,
                fillColor:"#f7f7f7"
            },
            gradationLineProperties: {
                excludeFromQuadTree: true,
                lineWidth: 1,
                lineColor: "#e4e4e4"
            },
            // Don't use color gradients, shadows or borders around the chart elements
            showShadows: false,
            useAutoGradients:false,
            barProperties: {
                lineColor: null, 
                lineWidth:1
            }, 
            // Pad the Y Axis Data Label 3px from the outer container border
            yAxisLabelPadding : 3,
            matchBarChartDataLineColor: true,
            brightenPercent: 50,
            brightenAllOnHover: true
        });
    }
    
    //----------------------------------------
    // 23) FilterBuilder
    //----------------------------------------
    
    if (isc.FilterBuilder) {
        isc.FilterBuilder.addProperties({
            fieldPickerWidth:120,
            operatorPickerWidth:180
        });
        isc.FilterBuilder.changeDefaults("topOperatorFormDefaults", {
            width: 130
        });
    }

    //----------------------------------------
    // 24) SectionStack
    //----------------------------------------

    if (isc.SectionStack) {
        isc.SectionStack.addProperties({
            headerHeight:39
        });
    }

    //----------------------------------------
    // 25) NavigationBar
    //----------------------------------------
    if (isc.MiniNavControl) {
        isc.MiniNavControl.addProperties({
            src: isc.Browser.isIPhone ? "[SKIN]/miniNav.svg" : "[SKIN]/miniNav~2.png",
            showDisabled: true,
            upButtonSrc: null,
            downButtonSrc: null
        });
    }
    if (isc.NavigationBar) {
        
        isc.NavigationBar.addProperties({
            layoutMargin: 7,
            skinUsesCSSTransitions: true,
            leftButtonIcon: "[SKINIMG]NavigationBar/back_arrow~2.png"
        });
        isc.NavigationBar.changeDefaults("leftButtonDefaults", {
            iconWidth: 11,
            iconHeight: 18,
            iconSpacing: 4,
            valign: 'absmiddle',
            showRTLIcon: true
        });
        isc.NavigationBar.changeDefaults("titleLabelDefaults", {
            margin: 0
        });

        if (isc.Browser.isIPhone || isc.Browser.isIPad) {
            isc.NavigationBar.addProperties({
                leftButtonIcon: "[SKINIMG]NavigationBar/back_arrow.svg"
            });
        }
    }
    if (isc.NavigationButton) {
        isc.NavigationButton.addProperties({
            height: 1,
            overflow: "visible",
            //padding: 5,
            showDown: true,
            showDownIcon: true,
            layoutAlign: "center"
        });
    }
    
    //----------------------------------------
    // Misc
    //----------------------------------------

    if (isc.RPCManager) {
        isc.RPCManager.addClassProperties({
            promptStyle:"cursor"
        });
    }

    if (isc.Hover) {
        isc.addProperties(isc.Hover.hoverCanvasDefaults, {
            shadowDepth:5,
            showShadow:false
        });
    }

    //indicate type of media used for various icon types
    isc.pickerImgType = "png";
    isc.transferImgType = "png";
    isc.headerImgType = "png";

    // remember the current skin so we can detect multiple skins being loaded
    if (isc.setCurrentSkin) isc.setCurrentSkin("Tahoe", "Flat");

    /* Customizations for the showcase */
    if (isc.ExampleViewer) {
        // override installSkinCustomizations to set up the showcase classes for this skin - 
        // the showcase calls this method after setting up its own defaults
        isc.ExampleViewer.installSkinCustomizations = function () {
            var skin = isc.getCurrentSkinName();

            // use dedicated skin directory (icons, thumbnails, etc.)
            isc.Page.setIsomorphicDocsSkinDir("skin/" + skin + "/");

            isc.VersionLabel.addProperties({
                styleName: "versionLabel" + skin
            });

            // Title page 
            isc.TitlePagePane.addProperties({
                layoutMargin:0
            });
            isc.TitlePagePane.changeDefaults("titlePageHeaderDefaults", {
                height: isc.SectionStack.getPrototype().headerHeight,
                styleName: "explorerTitlePageTitle" + skin
            });
            isc.TitlePagePane.changeDefaults("folderListDefaults", {
                styleName: "explorerFolderList" + skin
            });

            // Example Viewer
            isc.ExampleViewer.changeDefaults("optionsMenuButtonDefaults", {
                width: 50
            });
            isc.ExampleViewer.changeDefaults("skinSwitcherDefaults", {
                margin: 3
            });
            isc.ExampleViewer.changeDefaults("densitySwitcherDefaults", {
                margin: 3
            });
            isc.ExampleViewer.changeDefaults("screenshotDefaults", {
                showEdges: false,
                border: "1px solid #ccc"
            });

            isc.ExampleViewerSectionHeader.addProperties({
                baseStyle: "centerPane" + skin + "Header"
            });
            
            // Feature Explorer
            isc.FeatureExplorer.changeDefaults("exampleTreeDefaults", {
                showConnectors:true,
                baseStyle: "etreeCell" + skin,
                styleName: "etree",
                cellHeight: 22 + isc.FeatureExplorer.getPrototype().currentSizeIncrease 
            });
            isc.FeatureExplorer.changeDefaults("homeInterfacePageDefaults", {
                styleName: "homeInterfacePage" + skin,
                border: null // use border from styleName
            });
            isc.FeatureExplorer.changeDefaults("homeInterfaceDefaults", {
                styleName: "homeInterface" + skin,
                membersMargin: 15,
                layoutTopMargin: isc.Browser.isDesktop    ? 12 : 0,
                layoutRightMargin: isc.Browser.isDesktop  ? 15 : 0,
                layoutLeftMargin: isc.Browser.isDesktop   ? 15 : 0,
                layoutBottomMargin: isc.Browser.isDesktop ? 15 : 0,
                closeIconSize: 16
            });

            // Home page
            isc.HomeInterface.changeDefaults("customTileGridDefaults", {
                valuesShowDown: true,
                // mobile height is not half of "normal" because there needs to be room for the
                // label, especially since the labels tend to wrap, so they require 2-3 lines
                tileWidth:  isc.Browser.isDesktop ? 135 : 70,
                tileHeight: isc.Browser.isDesktop ? 135 : 90
            });
            if (isc.Browser.isDesktop) {
                isc.HomeInterface.changeDefaults("customTileGridDefaults", {
                    styleName: "showcaseTileGrid" + skin
                });
            }
            isc.HomeInterface.changeDefaults("filtersFormDefaults", {
                styleName: "showcaseFilterForm" + skin,
                fixedColWidths: false
            });
            isc.HomeInterface.changeDefaults("categoriesFormDefaults", {
                styleName: "showcaseFilterForm" + skin,
                fixedColWidths: false
            });
            isc.HomeInterface.changeDefaults("searchFieldDefaults", {
                titleStyle: "explorerSearchItemTitle" + skin,
                textBoxStyle: "explorerSearchItem" + skin
            });
            isc.HomeInterface.changeDefaults("tileDefaults", {
                baseStyle: "showcaseTile" + skin
            });
            
            isc.ExplorerCheckboxItem.addProperties({
                textBoxStyle: "sampleTypeLabelAnchor" + skin
            });

            isc.CustomTile.addProperties({
                thumbnailFieldAddedHeight: 18
            });
            isc.CustomTile.changeDefaults("thumbnailFieldDefaults", {
                getValueIconStyle : function (value) {
                    return "showcaseTileIcon" + isc.getCurrentSkinName();
                },
                cellStyle: "staticTextItem"
            });
            isc.CustomTile.changeDefaults("titleFieldDefaults", {
                wrap: false,
                textBoxStyle: "showcaseTileTitle" + skin,
                cellStyle: "showcaseTileTitle" + skin,
                cellHeight: 30,
                height: "*"
            });

            isc.GridSearch.addProperties({
                styleName: "gridSearchForm" + skin,
                // align this form with the SectionStack header to its right
                height: isc.SectionStack.getPrototype().headerHeight,
                searchItemProperties: {
                    height: isc.TextItem.getPrototype().getHeight()
                }
                // The only difference between this and the default is font size isn't scaled.
                //textBoxStyle: "gridSearchItem" + skin
            });

            // Web side/bottom panels
            isc.WebRightButtonBox.addProperties({
                showGradient: false,
                styleName: "explorerButtonBoxR" + skin
            });
            isc.WebRightButton.addProperties({
                fontSize: 10,
                lineHeight: 14,
                getSkinStyle : function (style) {
                    return style + isc.getCurrentSkinName();
                }
            });
            isc.WebBottomButton.addProperties({
                getSkinStyle : function (style) {
                    return style + isc.getCurrentSkinName();
                }
            });
            isc.FeatureExplorer.changeDefaults("learnMoreButtonRDefaults", {
                
                height: 84
            });
            
            // see comment in "other skins" below
            isc.Page.setAppImgDir(isc.Page.getIsomorphicDocsDir()+"exampleImages" + skin + "/");
        }
    }

    // if CSS is loaded or being loaded, call FontLoader with same @font-face fonts from CSS
    if (cssLoaded != null) isc.FontLoader.loadCustomFonts([
        "corbel", "corbel-bold", "calibri", "RobotoLight"
    ], cssLoaded);
        
    isc.Class.modifyFrameworkDone();
}   // end with()
}   // end loadSkin()

isc.loadSkin()
