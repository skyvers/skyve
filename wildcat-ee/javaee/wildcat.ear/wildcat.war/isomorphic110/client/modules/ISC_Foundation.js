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
//
// This script will load all of the Isomorphic SmartClient Application Framework libraries for you
//
// The idea is that in your app file you can just load the script "Isomorphic_SmartClient.js" which
// in a production situation would be all of the scripts jammed together into a single file.
//
// However, it's easier to work on the scripts as individual files, this file will load all of the
// scripts individually for you (with a speed penalty).
//		
var libs = 
	[
        "debug/version",  // check for module version mismatches

        //>Animation
		"widgets/Animation",	// Animation subsystem
        //<Animation

		"widgets/StatefulCanvas",		// minor variant on Canvas allowing for statefulness.

		"widgets/Layout",				// automatically arranges its children
		//"widgets/DetailViewer",			// show attributes of one or more objects as a vertical table
		"widgets/Button",				// button with special up, down, disabled, etc. look
		"widgets/Img",					// positionable image

//		"widgets/ButtonTable",			// table of cheapie buttons (very low resource use) -- in Nav, they look like links
//		"widgets/DateChooser",			// a date picker

// evaluation tracker image loader


		"widgets/StretchImg",			// composite image, composed of many individual images
//        "widgets/Slider",             	// graphical slider widget (uses isc.Img and isc.StretchImg)

		"widgets/Label",				// moveable, changeable bit of text
        //>Progressbar
		"widgets/Progressbar",			// stretch image for showing progress of lengthy operations
        //<Progressbar
        //>Rangebar
        "widgets/Rangebar",            	// graphical rangebar widget
        //<Rangebar
		"widgets/Toolbar",				// collection of buttons
		//"widgets/Border",				// platform-independent border
		"widgets/ImgButton",			// image with button behaviors
		"widgets/StretchImgButton",		// stretch image with button behaviors			
		//"widgets/ImgTab",				// stretch image with tab behaviors
		//"widgets/TabBar",				// collection of tabs
		
		"widgets/ToolStrip",			// a narrow strip with a mixed set of controls
		"widgets/ToolStripGroup",		// a "panel" for grouping controls in a toolstrip

        //>SectionStack
        "widgets/SectionStack",         // container similar to Outlook left-hand Nav (subclass of Layout, uses Label)
        //<SectionStack

		"widgets/Scrollbar",			// horizontal and vertical scroll bars
		"widgets/NativeScrollbar",      // horizontal and vertical scrollbars based on native CSS scrollbars
        
		//"widgets/GridRenderer",			// high speed, flexible, feature-rich table
		//"widgets/ListGrid",			// multi-column viewer for a list of objects
        //"widgets/TreeGrid",			// viewer for a tree of objects
        
        //"widgets/RecordEditor",         // specialized listViewer for editing a single record

        "widgets/Splitbar",              // default resizer for layouts

		//"widgets/Finder",				// specialized tree viewer that resembles the Macintosh isc.Finder
		//"widgets/Explorer",			// specialized tree viewer that resembles the left part of a Windows Explorer
		//"widgets/ExplorerList",		// specialized tree viewer that resembles the right part of a Windows Explorer

        //"widgets/ScrollingMenu",        // specialized listViewer with menu type event-handling 
                                        // behaviour, but scrollable and ready for data-binding
        		
		//"widgets/Menu",					// pull-down or context menus
		//"widgets/MenuButton",			// button that shows a menu on click
		//"widgets/Menubar",				// set of menus shown as a menubar
		//"widgets/Window",				// window class
		//"widgets/Dialog",				// movable, modal dialog

        

		"widgets/StretchResizePolicy",	// code to resize a set of elements in a single dimension
		//"widgets/TableResizePolicy",	// code to resize a set of elements in two dimensions
		//"widgets/Hover",				// singleton that manages hover (e.g. tooltip) timing and window
		//"widgets/TabSet", 				// composite of TabBar and tab panes

		//"widgets/form/DynamicForm",		// dynamically redrawable form
		//"widgets/form/FormItem",		// abstract sub-item of a form
		//"widgets/form/FormItemFactory",	// singleton object that creates FormItems from object literals
		//"widgets/form/Validators",		// validators for form fields
		//"widgets/form/ContainerItem",	// abstract form item that can contain other formItems
		
		//"widgets/form/TextItem",		// single-line text field
		//"widgets/form/BlurbItem",		// static text display
		//"widgets/form/ButtonItem",		// button form item
		//"widgets/form/SelectItem",		// select item -- drop-down list
		//"widgets/form/CheckboxItem",	// checkbox item
		//"widgets/form/HeaderItem",		// section header
		//"widgets/form/SectionItem",		// section header for group that shows/hides group
		//"widgets/form/HiddenItem",		// hidden field
		//"widgets/form/StaticTextItem",	// static text (label)
		//"widgets/form/PasswordItem",	// password-entry field (masked characters)        
		//"widgets/form/RadioGroupItem",	// set of radio buttons acting as a group
		//"widgets/form/RadioItem",		// single radio button
		//"widgets/form/ResetItem",		// reset button
		//"widgets/form/DateItem",		// multi-part Date editor
		//"widgets/form/SpacerItem",		// spacer
		//"widgets/form/RowSpacerItem",	// separator
		//"widgets/form/SubmitItem",		// submit button
        //"widgets/form/CancelItem",        // cancel button
		//"widgets/form/TextAreaItem",	// multi-line text field
		//"widgets/form/TimeItem",		// edit a isc.Time value
		//"widgets/form/ToolbarItem",		// collection of form buttons
		//"widgets/form/UploadItem",		// file-upload widget
		//"widgets/form/ComboBoxItem",	// combobox (text field + button + filtered listViewer)
		
		//"widgets/Editor",				// editor Interface and a couple of implementations for editing listViewer cells
		//"widgets/form/SearchForm",		// simple subclass of dynamicForm to be used in filters and 
                                        // search forms in applications

		//"widgets/form/AdvancedFilter",	// advanced search form that allows the user to specify
                                        // individual fields and operators

        //>ValuesManager
        //"widgets/form/ValuesManager",   // values manager for values from multiple member forms
        //<ValuesManager

		//"language/Dictionary",		// message dictionary class
		
		//"application/DataSource",		// representation of a server data source (databse table, etc)
		//"application/RPCManager",	// framework for editing/interacting with datasources
		//"application/ResultSet",        // data model for Lists loaded incrementally from a server
		//"application/ResultTree",       // data model for Trees loaded incrementally from a server
		//"application/ActionMethods",      // flow methods for databinding-capable components

		//"widgets/EditMode",             // support for an editing mode and editing container

        //"widgets/RecordScrollbar",      
        //"widgets/MultiRecordForm",      // use a scrollbar to page through a ResultSet of records,
                                        // showing the records in a form

		//"widgets/MultiView",            // presents multiple views of a datasource and standard
                                        // actions
//        "widgets/DataPrefetch",         // interface for parallel init/draw/data load
        

        //>SimpleType
        "language/SimpleType",          // loads map of built-in types and their validators, SimpleType
        //<SimpleType

        "widgets/NavigationBar",        // iPhone/iPad -like navigationBar

        "widgets/SplitPane",            // manages a two-pane layout according to hardware type
                                        // and orientation
        "widgets/NavStack",
        "widgets/Deck",
        "widgets/NavPanel"

	];

//<STOP PARSING 

// The following code only executes if the script is being dynamically loaded.

// the following statement allows a page that is not in the standard location to take advantage of
// dynamically loaded scripts by explicitly setting the window.isomorphiDir variable itself.
if (! window.isomorphicDir) window.isomorphicDir = "../isomorphic/";

// dynamic loading
(function () {
    function loadLib(lib, hash) {
        if (hash == null) hash = "";
        document.write("<"+"script src='" + window.isomorphicDir + "client/" + lib + ".js" + hash + "' type='text/javascript' charset='UTF-8'><"+"/script>");
    }

    loadLib("language/startDefiningFramework", "#module=Foundation");
    for (var i = 0, l = libs.length; i < l; ++i) {
        if (!libs[i]) continue;
        if (window.UNSUPPORTED_BROWSER_DETECTED == true) break;
        loadLib(libs[i]);
    }
    loadLib("language/stopDefiningFramework", "#module=Foundation");
})();
