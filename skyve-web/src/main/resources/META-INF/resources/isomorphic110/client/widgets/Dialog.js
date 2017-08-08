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
//>	@class	Dialog
// Dialogs are a specialized version of +link{Window} used for small windows that contain just a text
// message or a text mesage with some standard buttons.
// <P>
// Many typical modal dialogs such as alerts and confirmations are built into the system with convenience
// APIs - see +link{classMethod:isc.say()}, +link{classMethod:isc.warn()} and +link{classMethod:isc.askForValue}.
// <P>
// Dialogs can be modal or non-modal according to +link{Window.isModal,isModal}.
// <P>
// NOTE: If you are building a dialog that will involve more than just buttons and a message, consider
// starting from the +link{Window} class instead, where arbitrary components can be added to the body
// area via +link{Window.addItem()}.
// This is an example of creating a custom dialog:
// <smartclient>
// <pre>
//  isc.Dialog.create({
//      message : "Please choose whether to proceed",
//      icon:"[SKIN]ask.png",
//      buttons : [
//          isc.Button.create({ title:"OK" }),
//          isc.Button.create({ title:"Cancel" })
//      ],
//      buttonClick : function (button, index) {
//          this.hide();
//      }
//  });
// </pre>
// </smartclient>
// <smartgwt>
// <pre>
// final Dialog dialog = new Dialog();
// dialog.setMessage("Please choose whether to proceed");
// dialog.setIcon("[SKIN]ask.png");
// dialog.setButtons(new Button("OK"), new Button("Cancel"));
// dialog.addButtonClickHandler(new ButtonClickHandler() {
//     public void onButtonClick(ButtonClickEvent event) {
//         dialog.hide();
//     }
// });
// dialog.draw();
// </pre>
// </smartgwt>
//
//  @treeLocation Client Reference/Control
//  @visibility external
//<
isc.ClassFactory.defineClass("Dialog", "Window");

// add class properties
isc.Dialog.addClassProperties({
    //>	@classAttr	Dialog._openModalDialogs		(array : [] : IRWA)
	// 			list of open modal Dialogs so we can keep track as we open them
	//		@group	modal
	//		@see	Dialog.show()
	//<
	_openModalDialogs : [],

    //> @classAttr  Dialog.OK_BUTTON_TITLE  (HTML : "OK" : IRW)
    // Title for the <code>"OK"</code> button.
    // @see type:DialogButtons
    // @group i18nMessages
    // @visibility external
    //<
    OK_BUTTON_TITLE:"OK",
    //> @classAttr  Dialog.APPLY_BUTTON_TITLE  (HTML : "Apply" : IRW)
    // Title for the <code>"Apply"</code> button.
    // @see type:DialogButtons
    // @group i18nMessages
    // @visibility external
    //<
    APPLY_BUTTON_TITLE:"Apply",
    //> @classAttr  Dialog.YES_BUTTON_TITLE  (HTML : "Yes" : IRW)
    // Title for the <code>"Yes"</code> button.
    // @see type:DialogButtons
    // @group i18nMessages
    // @visibility external
    //<
    YES_BUTTON_TITLE:"Yes",
    //> @classAttr  Dialog.NO_BUTTON_TITLE  (HTML : "No" : IRW)
    // Title for the <code>"No"</code> button.
    // @see type:DialogButtons
    // @group i18nMessages
    // @visibility external
    //<
    NO_BUTTON_TITLE:"No",
    //> @classAttr  Dialog.CANCEL_BUTTON_TITLE  (HTML : "Cancel" : IRW)
    // Title for the <code>"Cancel"</code> button.
    // @see type:DialogButtons
    // @group i18nMessages
    // @visibility external
    //<
    CANCEL_BUTTON_TITLE:"Cancel",
    //> @classAttr  Dialog.DONE_BUTTON_TITLE  (HTML : "Done" : IRW)
    // Title for the <code>"Done"</code> button.
    // @see type:DialogButtons
    // @group i18nMessages
    // @visibility external
    //<
    DONE_BUTTON_TITLE:"Done",

    // Default Titles for the prompt windows themselves

    //> @classAttr  Dialog.CONFIRM_TITLE    (HTML : "Confirm" : IRW)
    // Default title for the dialog displayed in response to the +link{classMethod:isc.confirm()} method.
    // Note that a custom title can be specified as the <code>title</code> attribute of the
    // <code>properties</code> parameter passed to that method.
    // @group i18nMessages
    // @visibility external
    //<
    CONFIRM_TITLE:"Confirm",

    //> @classAttr  Dialog.SAY_TITLE    (HTML : "Note" : IRW)
    // Default title for the dialog displayed in response to the +link{classMethod:isc.say()} method.
    // Note that a custom title can be specified as the <code>title</code> attribute of the
    // <code>properties</code> parameter passed to that method.
    // @group i18nMessages
    // @visibility external
    //<
    SAY_TITLE:"Note",

    //> @classAttr  Dialog.WARN_TITLE    (HTML : "Warning" : IRW)
    // Default title for the dialog displayed in response to the +link{classMethod:isc.warn()} method.
    // Note that a custom title can be specified as the <code>title</code> attribute of the
    // <code>properties</code> parameter passed to that method.
    // @group i18nMessages
    // @visibility external
    //<
    WARN_TITLE:"Warning",

    //> @classAttr  Dialog.ASK_TITLE    (HTML : "Question" : IRW)
    // Default title for the dialog displayed in response to the +link{classMethod:isc.ask()} method.
    // Note that a custom title can be specified as the <code>title</code> attribute of the
    // <code>properties</code> parameter passed to that method.
    // @group i18nMessages
    // @visibility external
    //<
    ASK_TITLE:"Question",

    //> @classAttr  Dialog.ASK_FOR_VALUE_TITLE    (HTML : "Please enter a value" : IRW)
    // Default title for the dialog displayed by +link{classMethod:isc.askForValue()}.
    // A custom title can alternatively be specified as the <code>title</code> attribute of the
    // <code>properties</code> parameter passed to that method.
    // @group i18nMessages
    // @visibility external
    //<
    ASK_FOR_VALUE_TITLE:"Please enter a value",

    //> @classAttr  LoginDialog.LOGIN_TITLE (HTML : "Please log in" : IRW)
    // Default title for the dialog displayed by +link{classMethod:isc.showLoginDialog()}.
    // A custom title can alternatively be specified as the <code>title</code> attribute of the
    // <code>properties</code> parameter passed to that method.
    // @group i18nMessages
    // @visibility external
    //<
    
    LOGIN_TITLE:"Please log in",

    //> @classAttr  LoginDialog.USERNAME_TITLE (HTML : "Username" : IRW)
    // Default title for the +link{loginDialog.usernameItem,"usernameItem"} field in the
    // dialog displayed by +link{classMethod:isc.showLoginDialog()}.
    // @group i18nMessages
    // @visibility external
    //<
    
    USERNAME_TITLE:"Username",

    //> @classAttr  LoginDialog.PASSWORD_TITLE (HTML : "Password" : IRW)
    // Default title for the +link{loginDialog.passwordItem,"passwordItem"} field in the
    // dialog displayed by +link{classMethod:isc.showLoginDialog()}.
    // @group i18nMessages
    // @visibility external
    //<
    
    PASSWORD_TITLE:"Password",

    //> @classAttr  LoginDialog.LOGIN_BUTTON_TITLE (HTML : "Log in" : IRW)
    // Default title for login button in the dialog displayed by
    // +link{classMethod:isc.showLoginDialog()}.
    // @group i18nMessages
    // @visibility external
    //<
    
    LOGIN_BUTTON_TITLE:"Log in",

    //> @classAttr  LoginDialog.LOGIN_ERROR_MESSAGE (HTML : "Invalid username or password" : IRW)
    // Default error message displayed on failed login in the dialog shown by
    // +link{classMethod:isc.showLoginDialog()}.
    // @group i18nMessages
    // @visibility external
    //<
    
    LOGIN_ERROR_MESSAGE:"Invalid username or password",

    
	//> @type DialogButtons
    // Default buttons that you can use in your Dialogs.
    // <p>
    // <smartgwt>
    // Each <code>DialogButtons</code> enum value has a same-named static Button on the Dialog
    // class, and these buttons can be passed to +link{dialog.buttons,Dialog.setButtons()}:
    // <pre>
    //   Dialog.setButtons(Dialog.OK, Dialog.CANCEL);
    // </pre>
    // </smartgwt>
    // <smartclient>
    // Refer to these buttons via the syntax <code>isc.Dialog.OK</code> when passing them into
    // +link{dialog.buttons} or into the <code>properties</code> argument of helper
    // methods such as +link{classMethod:isc.say()}.
    // </smartclient>
    // <p>
    // All buttons added via <code>setButtons</code> will fire the
    // +link{Dialog.buttonClick,buttonClick event} (whether they are built-in or custom
    // buttons).  Built-in buttons automatically close a Dialog, with the exception of the
    // "Apply" button.
    //
    // @value   OK  Dismisses dialog<smartclient> by calling +link{Dialog.okClick()}</smartclient>.
    //              Title derived from +link{Dialog.OK_BUTTON_TITLE}.
	OK 		: {getTitle:function () {return isc.Dialog.OK_BUTTON_TITLE},
                width:75, click: function () { this.topElement.okClick() } },
    // @value   APPLY Does not dismiss dialog.  <smartgwt>Handle via +link{Dialog.buttonClick()}</smartgwt>
    //          <smartclient>Calls +link{Dialog.applyClick()}</smartclient>
    //              Title derived from +link{Dialog.APPLY_BUTTON_TITLE}.
	APPLY 	: {getTitle:function () {return isc.Dialog.APPLY_BUTTON_TITLE},
                width:75, click: function () { this.topElement.applyClick() } },
    // @value   YES Dismisses dialog<smartclient> by calling +link{Dialog.yesClick()}</smartclient>.
    //              Title derived from +link{Dialog.YES_BUTTON_TITLE}.
	YES 	: {getTitle:function () {return isc.Dialog.YES_BUTTON_TITLE},
                width:75, click: function () { this.topElement.yesClick() } },
    // @value   NO  Dismisses dialog<smartclient> by calling +link{Dialog.noClick()}</smartclient>.
    //              Title derived from +link{Dialog.NO_BUTTON_TITLE}.
	NO	 	: {getTitle:function () {return isc.Dialog.NO_BUTTON_TITLE},
                width:75, click: function () { this.topElement.noClick() } },
    // @value   CANCEL  Dismisses dialog<smartclient> by calling +link{Dialog.cancelClick()}</smartclient>.
    //                  Title derived from +link{Dialog.CANCEL_BUTTON_TITLE}.
	CANCEL 	: {getTitle:function () {return isc.Dialog.CANCEL_BUTTON_TITLE},
                width:75, click: function () { this.topElement.cancelClick() } },
    // @value   DONE   Dismisses dialog<smartclient> by calling +link{Dialog.doneClick()}</smartclient>. 
    //                  Title derived from +link{Dialog.DONE_BUTTON_TITLE}.
    DONE    : {getTitle:function () {return isc.Dialog.DONE_BUTTON_TITLE},
                width:75, click: function () { this.topElement.doneClick() } },
    // @visibility external
    //<

    
    _defaultToolbarWidth: 20
});

// add standard instance properties
isc.Dialog.addProperties({

    //> @attr dialog.defaultWidth (int: 360 : IR)
	// @group appearance
    // @visibility external
    //<
    defaultWidth:360,

    title:"Dialog",

    //>	@attr	dialog.styleName	(CSSStyleName: "dialogBackground" : IRW)
	//			Style of the Dialog background
	//		@group	appearance
    //      @visibility external
	//<
	styleName:"dialogBackground",

	skinImgDir:"images/Dialog/",

	canDragReposition : false,
	canDragResize:false,

	//>	@attr dialog.autoCenter (boolean : true : IRW)
	// If true, this dialog will automatically be centered on the page when shown
	// If false, it will show up wherever you (or the user) last put it
	//		@group	appearance, location
	//		@see	dialog.show()
	//<
	autoCenter : true,

	// Body Settings
	// ----------------------------------------------------------------------------------------
    //>	@attr	dialog.bodyStyle	(string : "dialogBody" : IA)
	// Style of the Window body
	//		@group	appearance, header
	//		@see	Window.makeBody()
	//<
    bodyStyle:"dialogBody",

    //>	@attr	dialog.bodyColor		(CSSColor : "#DDDDDD" : IA)
	//			Color of the Window body.
	//			Overrides the background color specified in the style.
	//		@group	appearance, header
	//		@see	Window.makeBody()
    //      @see    Window.flash()
	//<
    bodyColor:"#DDDDDD",

    //>	@attr	dialog.hiliteBodyColor		(CSSColor : "#DDDDDD" : IA)
	// Highlight color for the Window body (shown when the body is flashed).
	//		@group	appearance, header
	//		@see	Window.makeBody()
    //      @see    Window.flash()
	//<
    hiliteBodyColor:"#FFFFFF",

    bodyDefaults: isc.addProperties({}, isc.Window.getInstanceProperty("bodyDefaults", true),
    {
        layoutTopMargin:15,
        layoutLeftMargin:15,
        layoutRightMargin:15,
        layoutBottomMargin:5
    }),

    // Message & Icon
    // ---------------------------------------------------------------------------------------

    //> @attr dialog.message (HTMLString : null : IR)
    // Message to show in this dialog.
    // <P>
    // If a message is set the primary purpose of the dialog will be assumed to be to show a message with
    // buttons - auto-sizing to the message text will be enabled, and, if +link{dialog.icon} has also
    // been set, the +link{Dialog.messageLabel,messageLabel} and +link{Dialog.messageIcon,messageIcon}
    // AutoChildren will be created and placed together in the +link{Dialog.messageStack,messageStack}
    // AutoChild, with the toolbar underneath as usual.  If any of these behaviors are inconvenient or
    // you want more precise control over a message and some custom widgets, start from the superclass
    // +link{Window} instead, and add controls via +link{Window.addItem()}.
    // <P>
    // The message string may contain "${loadingImage}", if so, the standard loading spinner will appear at
    // that location in the text (see +link{Canvas.loadingImageSrc}).
    // <P>
    // The message will be styled with the +link{messageStyle}.
    //
    // @visibility external
    //<

    //> @attr dialog.messageStyle (CSSStyle : "normal" : IR)
    // Style to apply to the message text shown in the center of the dialog
    // @visibility external
    //<
    messageStyle:"normal",

    //> @attr dialog.messageLabel (Label AutoChild : null : IR)
    // AutoChild that shows +link{dialog.message}.
    // @visibility external
    //<
    messageLabelDefaults : {width : "100%", canSelectText: true},

    //> @attr dialog.messageIcon (Img AutoChild : null : IR)
    // AutoChild that shows +link{dialog.icon}.
    // @visibility external
    //<
    messageIconDefaults : { width:32, height:32 },

    //> @attr dialog.messageStack (Layout AutoChild : null : IR)
    // AutoChild that combines +link{dialog.message} and +link{dialog.icon}.
    // @visibility external
    //<
    messageStackDefaults : {height : 1, layoutMargin : 10, layoutBottomMargin:5, membersMargin:10},

    autoChildParentMap: isc.addProperties({}, isc.Window.getInstanceProperty("autoChildParentMap", true),
    {
        messageStack : "body",
        messageIcon  : "messageStack",
        messageLabel : "messageStack"
    }),

    //> @attr dialog.icon (SCImgURL : null : IR)
    // Icon to show in this dialog - see +link{Dialog.message}.
    // @visibility external
    //<

    //> @attr dialog.iconSize (int : 32 : IR)
    // Size of the icon to show in this dialog.
    // @visibility external
    //<

    // Header
	// ----------------------------------------------------------------------------------------
    //>	@attr	dialog.headerStyle	(string : "DialogHeader" : IA)
	// Style of the Dialog header
	//		@group	appearance, header
	//		@see	Dialog.makeHeader()
	//<
    headerStyle:"dialogHeader",

    //>	@attr	dialog.windowHeaderHilite	(string : "WindowHeader" : IA)
	//			Highlight style for the Dialog header
	//		@group	appearance, header
	//		@see	Window.makeHeader()
	//<
    hiliteHeaderStyle:"dialogHeaderHilite",

    //>	@attr	dialog.headerLabelTextStyle	(string : "dialogHeaderText" : IA)
	//			Style of the Dialog headerLabel text
	//		@group	appearance, headerLabel
	//		@see	Dialog.makeHeaderLabel()
	//<
    
    headerLabelDefaults : isc.addProperties({},
                                            isc.Window.getInstanceProperty("headerLabelDefaults", true),
                                            {styleName:"dialogHeaderText"}),

	// Header Icon
	// ----------------------------------------------------------------------------------------
	//>	@attr	dialog.showHeaderIcon		(boolean : false : IRW)
	//			should we show a headerIcon in the header,
	//			clicking it dismisses the Dialog
	//		@group	appearance, header
	//		@see	Dialog.makeHeaderIcon()
	//<
	showHeaderIcon:false,

	// Buttons
	// ----------------------------------------------------------------------------------------
	//>	@attr	Dialog.showMinimizeButton		(boolean : false : IRW)
	// Should we show a minimizeButton in the header, clicking it dismisses the Dialog
	//		@group	appearance, header
	//		@see	Dialog.makeMinimizeButton()
	//<
	showMinimizeButton:false,

    //>	@attr	Dialog.showMaximizeButton		(boolean : false : IRW)
	// Should we show a maximizeButton in the header, clicking it dismisses the Dialog
	//		@group	appearance, header
	//		@see	Dialog.makeMaximizeButton()
	//<
	showMaximizeButton:false,

	// Footer
	// ----------------------------------------------------------------------------------------
	//>	@attr	Dialog.showFooter		(boolean : false : IRW)
	// Should we show a footer for this Dialog, including resizer, statusBar, etc?
	//		@group	appearance, footer
	//<
	showFooter:false,

	// Toolbar
	// ----------------------------------------------------------------------------------------
    //>	@attr	Dialog.showToolbar		(boolean : null : IR)
	// Whether to show a toolbar of buttons at the bottom of the Dialog.
    // Default of null will cause the value to be resolved automatically to true or
    // false when the Dialog is first drawn according as +link{Dialog.toolbarButtons}
    // contains buttons or not.
	//		@group	appearance, toolbar
    // @visibility external
	//<
	showToolbar: null,

    //>	@attr dialog.autoFocus (Boolean : true : IR)
    // If a toolbar is showing, automatically place keyboard focus in the first button.
	// @group appearance, toolbar
    // @visibility external
	//<
	autoFocus :true,

    //> @attr Dialog.toolbar (AutoChild Toolbar : null : IR)
    // +link{AutoChild} of type Toolbar used to create the +link{toolbarButtons}.
    // @visibility external
    //<

    //> @attr Dialog.buttons (Array of Button or Button Properties : null : IR)
    // Array of Buttons to show in the +link{showToolbar,toolbar}, if shown.
    // <P>
    // The set of buttons to use is typically set by calling one of the shortcuts such as
    // +link{classMethod:isc.say()} or +link{classMethod:isc.confirm()}.  A custom set of
    // buttons can be passed to these shortcuts methods via the "properties" argument, or to a
    // directly created Dialog.
    // <P>
    // In both cases, a mixture of +link{type:DialogButtons,built-in buttons}, custom buttons,
    // and other components (such as a +link{LayoutSpacer}) can be passed.  
    // <smartclient>
    // Built-in buttons can be referred to as <code>isc.Dialog.OK</code>, for example:
    // <pre>
    // isc.Dialog.create({
    //    buttons:[
    //       isc.Dialog.OK,
    //       isc.Dialog.CANCEL,
    //       isc.LayoutSpacer.create({width:50}),
    //       { title:"Not now", click:"doSomething()" }
    //    ]
    // })
    // </pre>
    // Built-in buttons will call standard methods on the Dialog itself, such as
    // +link{dialog.cancelClick()}, as explained in the
    // +link{type:DialogButtons,list of built-in buttons}.
    // </smartclient>
    // <smartgwt>
    // Built-in buttons can be referred to via static fields on the Dialog class such as
    // <code>Dialog.OK</code>, for example:
    // <pre>
    // Dialog dialog = new Dialog();
    // Canvas layoutSpacer = new LayoutSpacer();
    // layoutSpacer.setWidth(50);
    // Button notNowButton = new Button("Not now");
    // notNowButton.addClickHandler(new ClickHandler() {
    //     public void onClick(ClickEvent event) {
    //         doSomething();
    //     }
    // });
    // dialog.setButtons(Dialog.OK, Dialog.CANCEL, layoutSpacer, notNowButton);
    // dialog.draw();
    // </pre>
    // All buttons will fire the +link{buttonClick} handler.
    // </smartgwt>
    //
    // @visibility external
    //<

    //> @attr Dialog.toolbarButtons (Array of Button or Button Properties : null : IR)
    // This is a synonym for +link{Dialog.buttons}
    //
    // @visibility external
    //<

    // Body Icons
    // ---------------------------------------------------------------------------------------
    //> @attr Dialog.askIcon (SCImgURL : "[SKIN]ask.png" : IR)
    // Icon to show in the <smartclient>+link{classMethod:isc.ask()}</smartclient>
    // <smartgwt>{@link com.smartgwt.client.util.SC#ask SC.ask()}</smartgwt> dialog.
    // @visibility external
    //<
    askIcon:"[SKIN]ask.png",
    //> @attr Dialog.sayIcon (SCImgURL : "[SKIN]say.png" : IR)
    // Icon to show in the <smartclient>+link{classMethod:isc.say()}</smartclient>
    // <smartgwt>{@link com.smartgwt.client.util.SC#say SC.say()}</smartgwt> dialog.
    // @visibility external
    //<
    sayIcon:"[SKIN]say.png",
    //> @attr Dialog.warnIcon (SCImgURL : "[SKIN]warn.png" : IR)
    // Icon to show in the <smartclient>+link{classMethod:isc.warn()}</smartclient>
    // <smartgwt>{@link com.smartgwt.client.util.SC#warn SC.warn()}</smartgwt> dialog.
    // @visibility external
    //<
    warnIcon:"[SKIN]warn.png",
    //> @attr Dialog.confirmIcon (SCImgURL : "[SKIN]confirm.png" : IR)
    // Icon to show in the <smartclient>+link{classMethod:isc.confirm()}</smartclient>
    // <smartgwt>{@link com.smartgwt.client.util.SC#confirm SC.confirm()}</smartgwt> dialog.
    // @visibility external
    //<
    confirmIcon:"[SKIN]confirm.png",

    // media exists, but no global helper, you have to call eg showMessage(message, "error")
    notifyIcon:"[SKIN]notify.png",
    errorIcon:"[SKIN]error.png",
    stopIcon:"[SKIN]stop.png",

    toolbarDefaults: isc.addProperties({}, isc.Window.getInstanceProperty("toolbarDefaults", true),
    {
        layoutAlign: "center", 
        width: isc.Dialog._defaultToolbarWidth,
        overrideDefaultButtonSizes: null,

        // batch bubbled clicks that hit buttons and report them via buttonClick
        click : function (item, itemNum) {
            this.Super("click", arguments);

            var target = isc.EH.getTarget(),
            	index = this.getMemberNumber(target);
            if (target !== this && index !== -1 && isc.isA.StatefulCanvas(target)) {
                this.topElement.buttonClick(target, index);
            }
        }
    })

});	// END	isc.Dialog.addProperties()

//!>Deferred

isc.Dialog.addMethods({

initWidget : function () {
    if (this.message != null) {
        this.autoSize = true;
    }

    // call the superclass function
    this.Super("initWidget",arguments);

    if (this.buttons) {
        this.toolbarButtons = this.buttons;
    }
    
    // Convert toolbarButtons to the doc'd type of Array of Button (Properties)
    
    var buttons = this.toolbarButtons;
    if (buttons && !isc.isAn.Array(buttons)) this.toolbarButtons = [buttons];
},

createChildren : function () {

    // HACK: prevent toolbar from being created, since we want it placed in "messageStack", which
    // we can't create until Super.createChildren() creates the "body", which is "messageStack"'s
    // parent.
    var origSetting = this.showToolbar;
    this.showToolbar = false;
    this.Super("createChildren");
    this.showToolbar = origSetting;

    if (this.message != null) {
        // can't be done via defaults because policy and direction are dynamically determined
        this.body.hPolicy = "fill";

        this.addAutoChild("messageStack", null, isc.HLayout);
        if (this.icon != null) {
            var props = isc.addProperties({
                            // call getImgURL so we're in the Dialog's imgDir
                            src:this.getImgURL(this.icon)
                        });
            if (this.iconSize && this.iconSize != 0) {
                isc.addProperties(props, {
                        width:this.iconSize,
                        height:this.iconSize
                });
            }
            this.addAutoChild("messageIcon", props, isc.Img);
        }

        var message = this.message.evalDynamicString(this, {
            loadingImage: this.imgHTML(isc.Canvas.loadingImageSrc,
                                       isc.Canvas.loadingImageSize,
                                       isc.Canvas.loadingImageSize)
            });
        this.addAutoChild("messageLabel", {
            contents:message,
            baseStyle:this.messageStyle
        }, isc.Label);
        
    //} else {
    //    this.logWarn("this.message is null, not creating messageIcon...");
    }
    // automatically resolve showToolbar to true or false based on toolbarButtons
    if (this.showToolbar == null) {
        this.showToolbar = this.toolbarButtons != null && this.toolbarButtons.length > 0;
    }
    if (this.showToolbar) {
        this.makeToolbar();
    }
},

makeToolbar : function () {
    this.invokeSuper(isc.Dialog, "makeToolbar");

    var toolbar = this.toolbar;
    if (!toolbar) return;

    
    if (toolbar.overrideDefaultButtonSizes == null) {
        toolbar.overrideDefaultButtonSizes = toolbar.width != isc.Dialog._defaultToolbarWidth;
    }
},


draw : function () {
    if (!this.readyToDraw()) return this;
    this.Super("draw", arguments);
    if (this.toolbar != null && this.autoFocus) {
        var firstButton = this.toolbar.getMember(0);
        if (firstButton) firstButton.focus();
    }
    return this;
},

//>	@method	Dialog.saveData()	(A)
// Method to save this Dialog's data. Called from <code>okClick()</code>,
// <code>applyClick()</code>.
// No default implementation - override to perform some action if required.
//
//		@group	buttons
//      @visibility external
//      @see okClick()
//      @see applyClick()
//<
saveData : function () {},

//> @method Dialog.closeClick()
// @include Window.closeClick()
//<

//>	@method	Dialog.cancelClick()
// Handle a click on the 'cancel' button of this Dialog.
// Default implementation is to return null and hide the Dialog.
// Override to do something else.
//		@group	buttons
//      @visibility external
//      @see type:DialogButtons
//<
cancelClick : function () {
    return this.closeClick();
},
// reroute the close button to call cancelClick
// (This way overrides to cancelClick will get fired - still falls through to closeClick())
_closeButtonClick : function () { return this.cancelClick() },

//>	@method	Dialog.okClick()	()
// Handle a click on the 'ok' button of this Dialog.
// Default implementation is to call <code>saveData()</code>, hide the Dialog, then return
// <code>true</code>.
// Override to do something else.
//		@group	buttons
//      @visibility external
//      @see type:DialogButtons
//<
okClick : function () {
    this.saveData();
    
	this.clear();
	this.returnValue(true);
},


//>	@method	Dialog.applyClick()
// Handle a click on the 'apply' button of this Dialog.
// Default implementation is to call <code>saveData()</code>, but NOT close the Dialog.
//		@group	buttons
//      @visibility external
//      @see type:DialogButtons
//<
applyClick: function () {
    this.saveData();
},

//>	@method	Dialog.yesClick()
// Handle a click on the 'yes' button of this Dialog.
// Default implementation is to return <code>true</code>.
// Override to do something else
//		@group	buttons
//      @visibility external
//      @see type:DialogButtons
//<
yesClick : function () {
	this.returnValue(true);
},

//>	@method	Dialog.noClick()
// Handle a click on the 'no' button of this Dialog.
// Default implementation is to return <code>false</code>.
// Override to do something else.
//		@group	buttons
//      @visibility external
//      @see type:DialogButtons
//<
noClick : function () {
	this.returnValue(false);
},

//>	@method	Dialog.doneClick()
// Handle a click on the 'done' button of this Dialog.
// Default implementation is to hide the dialog then return <code>true</code>.
// Override to do something else.
//		@group	buttons
//      @visibility external
//      @see type:DialogButtons
//<
doneClick : function () {
    // refer to comment in okClick
    this.clear();
    this.returnValue(true);
},

//> @method Dialog.buttonClick(button)
// Fires when any button in this Dialog's toolbar is clicked.  Default implementation does nothing.
//
// @param button (Button) button that was clicked
// @param index (int) index of the button that was clicked
// @group  buttons
// @visibility external
//<
buttonClick : function (button, index) {
},

// for Autotest APIs
namedLocatorChildren:[
    "okButton", "applyButton", "yesButton", "noButton", "cancelButton", "doneButton"
]
});

isc.Dialog.changeDefaults("toolbarDefaults",
{

    makeButton : function (button) {
        var config = button,
            button = this.Super("makeButton", arguments);

        switch (config)
        {
        case isc.Dialog.OK:
            this.creator.okButton = button;
            button.locatorParent = this.creator;
            break;

        case isc.Dialog.APPLY:
            this.creator.applyButton = button;
            button.locatorParent = this.creator;
            break;

        case isc.Dialog.YES:
            this.creator.yesButton = button;
            button.locatorParent = this.creator;
            break;

        case isc.Dialog.NO:
            this.creator.noButton = button;
            button.locatorParent = this.creator;
            break;

        case isc.Dialog.CANCEL:
            this.creator.cancelButton = button;
            button.locatorParent = this.creator;
            break;

        case isc.Dialog.DONE:
            this.creator.doneButton = button;
            button.locatorParent = this.creator;
            break;
        }
        return button;
    }
});



//!<Deferred

//
//	Default Dialogs that we create
//


//>	@groupDef Prompting
//	Objects / methods used for displaying prompts and warnings to the user via (possibly modal)
//  isc Dialog objects.
// @treeLocation Client Reference/Control
//<


//>	@classAttr	Dialog.Prompt   (Dialog Properties : dialog instance properties : A)
//
//  The "Prompt" object on the dialog class is a singleton Dialog instance.
//  The Prompt is used to show text to the user in a modal fashion - it will expand to show
//  all the text that you put into it.
//  By default this Dialog has no end-user controls and is expected to be programmatically
//  dismissed.<br>
//  Common use-case: During server-interactions, the Prompt will be used to display a suitable
//  wait message, and suppress user input.<br><br>
//
// Notes:<br>
//  Because this is a singleton object, properties set on the Prompt directly will persist each
//  time it is shown.<br>
//  Developers should use the <code>showPrompt()</code> and <code>clearPrompt()</code> methods
//  to show and hide the prompt rather than manipulating the prompt directly.
//
// @group Prompting
// @visibility external
// @see classMethod:isc.showPrompt
// @see classMethod:isc.clearPrompt
//<
isc.Dialog.Prompt = {
	ID:"isc_globalPrompt",
    _generated:true,
	width:360,
    height:90,
    placement:"none",

    autoDraw:false,
    autoSize:true,
	isModal:true,
	showHeader:false,
	showFooter:false,
	showToolbar:false,

    dismissOnEscape:false,

    bodyStyle:"promptBody", // no border-top, since there is no header
                            // TODO autogenerate border in Window based on header visibility

    bodyDefaults: isc.addProperties({}, isc.Dialog.getInstanceProperty("bodyDefaults", true), {height:"100%"}),

    // Message & Icon

    message:"Loading...&nbsp;${loadingImage}",

    messageStackDefaults: isc.addProperties({}, isc.Dialog.getInstanceProperty("messageStackDefaults", true),
    {
        height: "100%",
        width: "100%",
        layoutAlign: "center"
    }),
    messageLabelDefaults: isc.addProperties({}, isc.Dialog.getInstanceProperty("messageLabelDefaults", true),
    {
        width:"100%", align:isc.Canvas.CENTER, valign:isc.Canvas.CENTER
    }),
    layoutMargin:0,

	//>	@method	Prompt.showMessage()
	//	Show a message in the Dialog
	//
	//	Dialog will redraw and resize to show the entire message
	//	any properties in attributes will get applied and may be visibily changed
	//
	//	@param	newMessage	(string)	message to display
	//	@param	properties (Dialog Properties)	object of name:value pairs to apply to the object
	//									properties are applied before the redraw
	//<
	showMessage : function (newMessage, properties) {

		// first add the properties specified
		this.setProperties(properties);
		if (newMessage == null) newMessage = "&nbsp;"
        this.message = newMessage.evalDynamicString(this, {
            loadingImage: this.imgHTML(isc.Canvas.loadingImageSrc,
                                       isc.Canvas.loadingImageSize,
                                       isc.Canvas.loadingImageSize)
            });
        // Note: we lazily create children on draw, so verify that the items have been
        // initialized before manipulating the label
        if (!this._isInitialized) this.createChildren();

        this.messageLabel.setContents(this.message == null ? "&nbsp;" : this.message);

		this.show();
	},

	// clear the prompt message -- just clear the prompt
    
	clearMessage : function () {
        if (this.pendingFade) {
            isc.Timer.clearTimeout(this.pendingFade);
            delete this.pendingFade;
        }
        if (this.isAnimating(this._$hide)) {
            this.finishAnimation(this._$hide);
        }
		this.clear();
        if (this._clearPromptCallback) {
            this.fireCallback(this._clearPromptCallback);
            delete this._clearPromptCallback;
        }
	},

    fadeDuration:2000,
    fadeMessage : function () {
        delete this.pendingFade;
        this.animateHide("fade", {target:this, methodName:"clearMessage"});
    },

    // If the prompt gets destroyed, remove the pointer to it.
    
    destroy : function () {
        isc.Dialog.Prompt = this._originalProperties;
        return this.Super("destroy", arguments);
    }
};



//>	@classMethod isc.showPrompt()
//
//	Method available on the isc object to show a modal prompt to the user.
//  This method will display the message using the Dialog.Prompt singleton object.<br>
//  Note: if this prompt is to be shown to the user during some slow JavaScript logic, we
//  advise calling this method, then using +link{Class.delayCall()} or +link{Timer.setTimeout}
//  to kick off the slow logic in a separate thread. This ensures that the prompt is showing
//  before the lengthy execution begins.
//  <p/>Use <code>"&#36;{loadingImage}"</code> to include +link{Canvas.loadingImageSrc,a loading image}.
//
//
//	@param	message			(string)	message to display
//	@param	[properties]	(Dialog Properties)	additional properties for the Dialog, applied before
//                                       the Dialog is shown
//
// @visibility external
// @see Dialog.Prompt
// @group Prompting
//<
isc.addGlobal("showPrompt", function (message, properties) {
    var prompt = isc.Dialog.Prompt;
	if (!isc.isA.Dialog(prompt)) {
        var props = prompt;
        // If we're being rendered in a very small screen, ensure we aren't too wide to 
        // fit.
        
        if (props.width != null && 
            isc.isA.Number(props.width) && props.width > isc.Page.getWidth()) 
        {
            props.width = isc.Page.getWidth();
        }
        
		prompt = isc.Dialog.Prompt = isc.Dialog.create(prompt);
        // If we destroy() the prompt, this allows us to essentially 'reset' ourselves to a
        // state where calling this method again will create a new prompt from the original
        // set of properties.
        
        prompt._originalProperties = props;
	}
	isc.Dialog.Prompt.showMessage(message, properties);
});

//>	@classMethod	isc.clearPrompt()
//
//	Clear the modal prompt being shown to the user.
//
//  @group Prompting
//  @visibility external
//  @see Dialog.Prompt
//<
isc.addGlobal("clearPrompt", function () {
    if (!isc.isA.Dialog(isc.Dialog.Prompt)) return; // prompt has never been shown
	isc.Dialog.Prompt.clearMessage();
});



//>	@classMethod isc.showFadingPrompt()
//
//	Method available on the isc object to show a temporary modal prompt to the user.
//  This method will display the message using the Dialog.Prompt singleton object, then hide it
//  using a fade animation effect.<br>
//  Note: if this prompt is to be shown to the user during some slow JavaScript logic, we
//  advise calling this method, then using +link{Class.delayCall()} or +link{Timer.setTimeout}
//  to kick off the slow logic in a separate thread. This ensures that the prompt is showing
//  before the lengthy execution begins.
//  <P>
//  The prompt may be cleared before the duration has elapsed via a call to +link{isc.clearPrompt()}
//  and any callback specified will still be fired even if the prompt is dismissed early.
//
//	@param	message			(string)	message to display
//  @param  duration        (number)    how long the message should appear for in milliseconds before
//    fading from view.
//  @param  [callback]      (callback) When the prompt is hidden, callback will be fired.
//	@param	[properties]	(Dialog Properties)	additional properties for the Dialog, applied before
//                                       the Dialog is shown
//
// @visibility external
// @see Dialog.Prompt
// @group Prompting
//<


isc.addGlobal("showFadingPrompt", function (message, duration, callback, properties) {
    if (isc.isA.Canvas(isc.Dialog.Prompt) && isc.Dialog.Prompt.isDrawn()) {
        isc.clearPrompt();
    }
    isc.showPrompt(message, properties);
    var prompt = isc.Dialog.Prompt;
    // this defaults to 2000 ms (though undocumented as such right now)
    if (duration == null) duration = prompt.fadeDuration;

    prompt._clearPromptCallback = callback;
    prompt.pendingFade = prompt.delayCall("fadeMessage", [], duration);
});


////////////////////////////////////////////////////////////////////////////////////////////


//>	@classAttr	Dialog.Warn (Dialog Properties : dialog instance properties : A)
//
// A singleton Dialog instance that will show text to the user and provide buttons for their
// response.  The Dialog will expand to show all the text that you put into it. This dialog
// is shown in response to calls to +link{classMethod:isc.say()}, +link{classMethod:isc.warn()}, +link{classMethod:isc.ask} and
// +link{classMethod:isc.confirm()}.
// <P>
// This can be used in cases where a developer would alternatively make use of the native
// JavaScript <code>alert()</code> and <code>confirm()</code> methods.  The main differences
// between those methods and using the Warn object are:<br>
// - The Warn object can be customized by modifying which buttons are visible, the style
//   applied to it, etc.<br>
// - The <code>isc.ask()</code> and <code>isc.warn()</code> methods are asynchronous - rather
//   than returning a value indicating the user's response, a callback method will be fired
//   when the user interacts with the dialog.<br><br>
// 
// Notes:<br>
//  Because this is a singleton object, properties set on the Warn object directly will persist
//  each time it is shown.<br>
//  Developers should use the <code>warn()</code> or <code>ask()</code> methods to show and
//  hide this object rather than manipulating the Dialog directly.
//  @group  Prompting
//  @visibility external
//  @see classMethod:isc.warn
//  @see classMethod:isc.ask
//<
isc.Dialog.Warn = {
	ID:"isc_globalWarn",
    _generated:true,
	width:360,
    height:138,

	isModal:true,
	canDragReposition:true,
    keepInParentRect:true,
    
    placement:"none",

    autoDraw:false,
    autoSize:true,
	autoCenter:true,

	buttons:[isc.Dialog.OK],
    message:"Your message here!",

	//>	@method	Warn.showMessage()
	// Show a message in the Dialog
	//
	// Dialog will redraw and resize to show the entire message
	// any properties in attributes will get applied and may be visibily changed
	//
	//	@param	newMessage	(string)	message to display
	//	@param	attributes	(Dialog Properties)	object of name:value pairs to apply to the object
	//									properties are applied before the redraw
	//<
	showMessage : function (newMessage, properties) {
	    if (newMessage == null) newMessage = "&nbsp;";

        this.message = newMessage;
        
        if (!this.icon && properties.icon) this.icon = properties.icon;
        
        var autoSize = properties.autoSize;
        if (autoSize && !this.autoSize) {
            delete properties.autoSize;
        }
		this.setProperties(properties);
        
		// if no callback was specified, clear the Dialog callback
		if (properties.callback == null) delete this.callback;

        // Note: we lazily create children on draw, so verify that the items have been
        // initialized before manipulating the label
        if (!this._isInitialized) this.createChildren();

        // Update the label in the body
        this.messageLabel.setContents(this.message == null ? "&nbsp;" : this.message);

        if (this.icon) {
            if (this.messageIcon) {
                this.messageIcon.setSrc(this.getImgURL(this.icon));
                this.messageIcon.show();
            }
        } else if (this.messageIcon) this.messageIcon.hide();

        // do immediate relayout so we don't wait for timers before we draw the new buttons,
        // especially because the destroy is immediate but the new draw is delayed, and in the
        // interim things react to the empty toolbar.
        this.toolbar.layoutChildren();
        // since we're going to try to autoCenter on show(), we go ahead and get all relayout
        // done now
        if (this.messageLabel.isDirty()) this.messageLabel.redraw();
        if (this.isDrawn()) {
            this.messageStack.layoutChildren();
            this.body.layoutChildren();
            this.layoutChildren();
        }

		this.show();
        if (autoSize && !this.autoSize) {
            this.setAutoSize(true);
        }

        // focus in the first button so you can hit Enter to do the default thing
        if (this.toolbar != null && this.autoFocus) {
            var firstButton = this.toolbar.getMember(0);
            /*
            this.logWarn("focusing on first button: " + firstButton +
                         ", drawn: " + firstButton.isDrawn() +
                         ", disabled: " + firstButton.isDisabled() +
                         ", visible: " + firstButton.isVisible() +
                         ", canFocus: " + firstButton._canFocus());
            */
            firstButton.focus();
        }
	},
    destroy : function () {
        isc.Dialog.Warn = this._originalProperties;
        return this.Super("destroy", arguments);
	}
};

//> @classMethod isc.showMessage()
// Show a modal dialog with a message, icon, and response buttons.
//<
isc.addGlobal("showMessage", function (message, messageType, callback, properties) {
    
    if ((isc.isA.String(properties) || isc.isA.Function(properties)) ||
        (properties == null && isc.isAn.Object(callback) && callback.methodName == null &&
         callback.action == null && callback.method == null))
    {
        // swap arguments
        var realCallback = properties;
        properties = callback;
        callback = realCallback;
    }

	if (!isc.isA.Dialog(isc.Dialog.Warn) ||
            (isc.isA.Function(isc.Dialog.Warn.initialized) && !isc.Dialog.Warn.initialized()))
    {
        // Useful for potentially resetting configuration
        var props = isc.addProperties({},isc.Dialog.Warn);
        // If we're being rendered in a very small screen, ensure we aren't too wide to 
        // fit.
        
        if (props.width != null && 
            isc.isA.Number(props.width) && props.width > isc.Page.getWidth()) 
        {
            props.width = isc.Page.getWidth();
        }
        
        isc.Dialog.Warn = isc.Dialog.create(isc.Dialog.Warn);
        isc.Dialog.Warn._originalProperties = props;
    }
	if (!properties) properties = {};

    // We support toolbarButtons and buttons - copy across to "buttons" attr so we can
    // easily check if they were specified on the object passed in and otherwise apply defaults.
    if (properties.toolbarButtons != null) {
        properties.buttons = properties.toolbarButtons;
        delete properties.toolbarButtons;
    }
    // messageType is one of
    // "confirm" (confirm dialog)
    // "ask" (ask dialog)
    // "say", "warn" (info / warn dialog)
    if (!properties.buttons) {
        if (messageType == "confirm") {
	        properties.buttons = [isc.Dialog.OK, isc.Dialog.CANCEL];
        } else if (messageType == "ask") {
	        properties.buttons = [isc.Dialog.YES, isc.Dialog.NO];
        } else {
	        properties.buttons = [isc.Dialog.OK];
        }
    }

    if (properties.width == null) {
        properties.autoSize = isc.Dialog.Warn._originalProperties.autoSize;
        properties.width = isc.Dialog.Warn._originalProperties.width;
    } else {
        properties.autoSize = false;
    }


    // Title: If specified in properties, respect it, otherwise show the
    // appropriate default title based on the dialog type
    if (!properties.title) {
        if (messageType == "confirm") properties.title = isc.Dialog.CONFIRM_TITLE;
        else if (messageType == "ask") properties.title = isc.Dialog.ASK_TITLE;
        else if (messageType == "warn") properties.title = isc.Dialog.WARN_TITLE;
        else properties.title = isc.Dialog.SAY_TITLE;
    }

    isc._applyDialogHandlers(properties);

    if (!properties.icon) properties.icon = isc.Dialog.getInstanceProperty(messageType + "Icon");
	if (callback) properties.callback = callback;

    isc.Dialog.Warn.showMessage(message, properties);
});

//> @classMethod isc.getLastDialog()
// Returns the last-shown isc.say/ask/warn/confirm dialog.  Do not document externally.
//<
isc.addGlobal("getLastDialog", function () {
    return isc.Dialog.Warn;
});

// shared with askForValue()
isc._applyDialogHandlers = function (properties) {
    
    var defaultHandlers = this._defaultHandlers =
        this._defaultHandlers || ["okClick", "yesClick", "noClick",
                                  "cancelClick", "closeClick", "applyClick"];
    for (var i = 0; i < defaultHandlers.length; i++) {
        var handlerName = defaultHandlers[i];
        if (!properties[handlerName]) {
            properties[handlerName] = isc.Dialog.getInstanceProperty(handlerName);
        }
    }
}

//>	@classMethod	isc.warn()
// Show a modal dialog with a message, icon, and "OK" button. See +link{dialog.warnIcon}.
// <P>
// The callback will receive boolean true for an OK button click, or null if the Dialog is
// dismissed via the close button.
//
//	@param	message			(string)	message to display
//  @param  [callback]      (Callback)  Optional Callback to fire when the user
//                                      dismisses the dialog. This has the single parameter
//                                      'value', indicating the value returned by the Warn
//                                      dialog from 'okClick()' etc.
//	@param	[properties]	(Dialog Properties)	additional properties for the Dialog.
//                                      To set +link{Dialog.buttons,custom buttons} for
//                                      the Dialog, set properties.buttons to
//                                      an array of buttons
//										eg:	{ buttons : [Dialog.OK, Dialog.CANCEL] }
// @group Prompting
// @visibility external
// @see classAttr:Dialog.Warn
// @see classMethod:isc.say()
// @see classMethod:isc.ask()
// @see method:Dialog.okClick()
// @see classAttr:Dialog.WARN_TITLE
//<
isc.addGlobal("warn", function (message, callback, properties) {
    isc.showMessage(message, "warn", callback, properties);
});

//>	@classMethod	isc.say()
// Show a modal dialog with a message, icon, and "OK" button.  Intended for notifications which
// are not really warnings (default icon is less severe). See +link{dialog.sayIcon}.
// <P>
// The callback will receive boolean true for an OK button click, or null if the Dialog is
// dismissed via the close button.
//
//	@param	message			(string)	message to display
//  @param  [callback]      (Callback)  Optional Callback to fire when the user
//                                      dismisses the dialog. This has the single parameter
//                                      'value', indicating the value returned by the Warn
//                                      dialog from 'okClick()' etc.
//	@param	[properties]	(Dialog Properties)	additional properties for the Dialog.
//                                      To set +link{Dialog.buttons,custom buttons} for
//                                      the Dialog, set properties.buttons to an array of
//                                      buttons
//										eg:	{ buttons : [Dialog.OK, Dialog.CANCEL] }
// @group Prompting
// @visibility external
// @see classAttr:Dialog.Warn
// @see classMethod:isc.warn()
// @see classMethod:isc.ask()
// @see method:Dialog.okClick()
// @see classAttr:Dialog.SAY_TITLE
//<
isc.addGlobal("say", function (message, callback, properties) {
    isc.showMessage(message, "say", callback, properties);
});


//>	@classMethod	isc.ask()
// Show a modal dialog with a message, icon, and "Yes" and "No" buttons. See +link{dialog.askIcon}.
// <P>
// The callback will receive boolean true for an OK button click, boolean false for a No button
// click, or null if the Dialog is dismissed via the close button.
//
//	@param	message			(string)	message to display
//  @param  [callback]      (Callback)  Callback to fire when the
//                                      user clicks a button to dismiss the dialog.
//                                      This has the single parameter 'value', indicating the
//                                      value returned by the Warn dialog from 'okClick()' etc.
//	@param	[properties]	(Dialog Properties)	additional properties for the Dialog.
//                                      To set +link{Dialog.buttons,custom buttons} for
//                                      the Dialog, set properties.buttons to an array
//                                      of buttons
//										eg:	{ buttons : [Dialog.OK, Dialog.CANCEL] }
//
// @group Prompting
// @visibility external
// @see Dialog.Warn
// @see classMethod:isc.warn()
// @see method:Dialog.yesClick()
// @see method:Dialog.noClick()
// @see classAttr:Dialog.ASK_TITLE
// @example dialogs
//<
isc.addGlobal("ask", function (message, callback, properties) {
    isc.showMessage(message, "ask", callback, properties);
});

//>	@classMethod	isc.confirm()
// Show a modal dialog with a message, icon, and "OK" and "Cancel" buttons. See +link{dialog.confirmIcon}.
// <P>
// The callback will receive boolean true for an OK button click, or null for a Cancel click or
// if the Dialog is dismissed via the close button.
// <P>
// Note: this does not override the native window.confirm() method.
//
//	@param	message			(string)	message to display
//  @param  [callback]      (Callback)  Callback to fire when the
//                                      user clicks a button to dismiss the dialog.
//                                      This has the single parameter 'value', indicating the
//                                      value returned by the Warn dialog from 'okClick()' etc.
//	@param	[properties]	(Dialog Properties)	additional properties for the Dialog.
//                                      To set +link{Dialog.buttons,custom buttons} for
//                                      the Dialog, set properties.buttons to an array of
//                                      buttons
//										eg:	{ buttons : [Dialog.OK, Dialog.CANCEL] }
//
// @group Prompting
// @visibility external
// @see Dialog.Warn
// @see classMethod:isc.warn()
// @see method:Dialog.okClick()
// @see method:Dialog.cancelClick()
// @see classAttr:Dialog.CONFIRM_TITLE
// @example dialogs
//<
isc.confirm = function (message, callback, properties) {
    isc.showMessage(message, "confirm", callback, properties);
};

//>	@classAttr	Dialog.Ask (Dialog Properties : dialog instance properties : A)
//
// A singleton Dialog instance that will be shown in response to a +link{isc.askForValue()} call.
//
// Notes:<br>
//  Because this is a singleton object, properties set on the Ask object directly will persist
//  each time it is shown.<br>
//  Developers should use the <code>askForValue()</code> method to show this object rather than
//  manipulating the Dialog directly.
//  @group  Prompting
//  @visibility external
//  @see classMethod:isc.askForValue
//<

//> @classMethod isc.askForValue()
// Show a modal dialog with a text entry box, asking the user to enter a value.
// <P>
// As with other convenience methods that show Dialogs, such as +link{classMethod:isc.warn()},
// the dialog is shown and the function immediately returns.  When the user responds, the
// provided callback is called.
// <P>
// If the user clicks OK, the value typed in is passed to the callback (including the empty
// string ("") if nothing was entered.  If the user clicks cancel, the value passed to the
// callback is null.
// <P>
// A default value for the text field can be passed via <code>properties.defaultValue</code>.
// <P>
// Keyboard focus is automatically placed in the text entry field, and hitting the enter key is
// the equivalent of pressing OK.
//
//	@param	message			(string)	message to display
//  @param  [callback]      (Callback)  Callback to fire when the
//                                      user clicks a button to dismiss the dialog.
//                                      This has the single parameter 'value', indicating the
//                                      user entry, or null if cancel was pressed or the window
//                                      closed
//	@param	[properties]	(Dialog Properties)	additional properties for the Dialog.
//                                      To set +link{Dialog.buttons,custom buttons} for
//                                      the Dialog, set properties.buttons to an array of
//                                      buttons
//										eg:	{ buttons : [Dialog.OK, Dialog.CANCEL] }
//
// @see method:Dialog.okClick()
// @see method:Dialog.cancelClick()
// @see classAttr:Dialog.ASK_FOR_VALUE_TITLE
// @group Prompting
// @visibility external
//<
isc.askForValue = function (message, callback, properties) {
    properties = properties || isc.emptyObject;

    var askDialog = isc.Dialog.Ask;
    if (!askDialog) {
        var askForm = isc.DynamicForm.create({
            numCols:1,
            padding:3,
            items: [
                { name:"message", type:"blurb" },
                { name:"value", showTitle:false, width:"*" }
            ],
            // fire okClick on enter
            saveOnEnter:true,
            submit : function () { this.askDialog.okClick(); }
        });
        askDialog = isc.Dialog.Ask = isc.Dialog.create({
            items : [ askForm ],
            askForm: askForm,
            canDragReposition:true,
            isModal:true,
            placement:"none",
            ariaRole:"alertdialog",
            // accomplishes vertical autoSizing
            bodyProperties : {overflow:"visible"},
            overflow:"visible"
        });
        askForm.askDialog = askDialog;

        // return the form value to the callback on okClick
        askDialog._okClickFunction = function () {
            this.clear();
            this.returnValue(this.askForm.getValue("value"));
        }
    }
    
    
    // If we were given explicit left/top coords, auto-center, otherwise respect them
    var explicitPosition = properties.left != null || properties.top != null;

    if (properties.toolbarButtons != null) {
        properties.buttons = properties.toolbarButtons;
        delete properties.toolbarButtons;
    }

    // copy properties and install defaults
    properties = isc.addProperties({
        callback: callback,
        title: properties.title || isc.Dialog.ASK_FOR_VALUE_TITLE,
        autoCenter:!explicitPosition,
        left: (explicitPosition ? properties.left || "10%" : null),
        top: (explicitPosition ? properties.top || "20%" : null),
        width: properties.width || "80%",
        height: properties.height || 20,
        buttons: properties.buttons || [ isc.Dialog.OK, isc.Dialog.CANCEL ],
        okClick : properties.okClick || askDialog._okClickFunction
    }, properties);

    // have standard handlers added to properties
    isc._applyDialogHandlers(properties);

    askDialog.setProperties(properties);

    askDialog.askForm.setValues({
        message : message || "Please enter a value:",
        value : properties.defaultValue || ""
    });
    askDialog.show();
    askDialog.askForm.focusInItem("value");
};

//> @classMethod isc.dismissCurrentDialog()
// If a dialog triggered via +link{classMethod:isc.say()}, +link{classMethod:isc.ask()},
// +link{classMethod:isc.warn()}, +link{classMethod:isc.confirm()} or +link{classMethod:isc.askForValue()}
// is currently visible, it will be dismissed.  The callback passed to the relevant method will never fire.
// <p>
// Note this is a rarely used API with very few valid use cases.  As an example, perhaps some kind of
// periodic (non-user triggered) event would cause an entire area of the UI to be removed (such as a tab)
// and the system wants to ensure that no modal dialogs are currently showing from that part of the UI.
// In this case, while <code>dismissCurrentDialog</code> could be used to ensure the part of the UI being
// removed didn't leave behind a modal dialog.
// <p>
// To clear a modal prompt shown by +link{isc.showPrompt()}, use +link{isc.clearPrompt()} instead.
//
// @group Prompting
// @visibility external
//<
isc.addGlobal("dismissCurrentDialog", function () {
    if (isc.Dialog.Warn && isc.Dialog.Warn.hide) {
        isc.Dialog.Warn.hide();
    }

    if (isc.Dialog.Ask && isc.Dialog.Ask.hide) {
        isc.Dialog.Ask.hide();
    }
});

//> @classMethod isc.showLoginDialog()
// Handle a complete login interaction with a typical login dialog asking for username and
// password credentials using the +link{LoginDialog} class.
// <P>
// As with other convenience methods that show Dialogs, such as +link{classMethod:isc.warn()},
// the dialog is shown and the function immediately returns.  When the user responds, the
// provided callback function is called.
// <P>
// If the user clicks the "Log in" button, the credentials entered by the user are passed to
// the provided "loginFunc" as an Object with properties "username" and "password" (NOTE: both
// property names are all lowercase), as the variable "credentials".  For example:
// <pre>{ username: "barney", password: "rUbbL3" }</pre>
// <P>
// The "loginFunc" should then attempt to log in by whatever means is necessary.  The second
// parameter to the loginFunc, "dialogCallback", is a function, which must be called <i>whether
// login succeeds or fails</i> with a true/false value indicating whether login succeeded.
// <P>
// If the login dialog is dismissable (settable as properties.dismissable, default false) and
// the user dismisses it, the loginFunc will be fired with null for the credentials.
// <P>
// The following code shows typical usage.  This code assumes you have created a global
// function sendCredentials() that send credentials to some authentication system and fires a
// callback function with the result:
// <pre>
// isc.showLoginDialog(function (credentials, dialogCallback) {
//     if (credentials == null) return; // dismissed
//
//     // send credentials
//     sendCredentials(credentials, function (loginSucceeded) {
//         // report success or failure
//         dialogCallback(loginSucceeded);
//     })
// })
// </pre>
// The login dialog has several built-in behaviors:
// <ul>
// <li> keyboard focus is automatically placed in the username field
// <li> hitting enter in the username field proceeds to the password field
// <li> hitting enter in the password field submits (fires the provided callback)
// </ul>
// In addition to normal properties supported by Dialog/Window, the following special
// properties can be passed:
// <ul>
// <li><code>username</code>: initial value for the username field
// <li><code>password</code>: initial value for the password field
// <li><code>usernameTitle</code>: title for the username field
// <li><code>passwordTitle</code>: title for the password field
// <li><code>errorMessage</code>: default error message on login failure
// <li><code>loginButtonTitle</code>: title for the login button
// <li><code>dismissable</code>: whether the dialog can be dismissed, default false
// <li><code>errorStyle</code>: CSS style for the error message, if shown
// </ul>
// See below for links to the default values for these properties.
//
//  @param  loginFunc       (Callback)  Function to call to attempt login.  Receives parameters
//                                      "credentials" and "dialogCallback", described above
//	@param	[properties]	(LoginDialog Properties)	additional properties for the Dialog
//
// @see classAttr:LoginDialog.LOGIN_TITLE
// @see classAttr:LoginDialog.USERNAME_TITLE
// @see classAttr:LoginDialog.PASSWORD_TITLE
// @see classAttr:LoginDialog.LOGIN_BUTTON_TITLE
// @see classAttr:LoginDialog.LOGIN_ERROR_MESSAGE
// @group Prompting
// @visibility external
//<

//>	@class	LoginDialog
// Handle a complete login interaction with a typical login dialog asking for username and
// password credentials. Use this
// class to quickly present a traditional username/password authentication mechanism in a
// SmartClient window.
// <p>
// To adapt this class to your requirements, first implement LoginDialog.loginFunc to submit
// the username and password to the authentication mechanism of your choice, calling
// dialogCallback once the authentication process completes.
//
// @see classMethod:isc.showLoginDialog
// @treeLocation Client Reference/Control
// @group Prompting
// @visibility external
//<

isc.ClassFactory.defineClass("LoginDialog", "Window");
isc.LoginDialog.registerStringMethods({
    //> @method loginDialog.register()
    // Called if the user clicks on the +link{loginDialog.registrationItem,registration link}
    // on the login form. Implement this method to allow the user to register for a
    // new account.
    // @param values (Object) Current values of form fields
    // @param form (DynamicForm) Form on which the link was clicked
    // @visibility external
    //<
    register:"values, form",

    //> @method loginDialog.lostPassword()
    // Called if the user clicks on the +link{loginDialog.lostPasswordItem,"Lost Password"} link
    // on the login form. Implement this method to allow the user to request the password
    // be resent or reset.
    // @param values (Object) Current values of form fields
    // @param form (DynamicForm) Form on which the link was clicked
    // @visibility external
    //<
    lostPassword:"values, form"
});
isc.LoginDialog.addClassProperties({
    firstTimeInit: true
});
isc.LoginDialog.addProperties({
    //> @method loginDialog.loginFunc()
    // User-supplied callback function to process login transactions.
    // <p>If the user clicks the "Log in" button, the credentials entered by the user are passed to
    // loginFunc as an Object with properties "username" and "password" (NOTE: both
    // property names are all lowercase), as the variable "credentials".  For example:
    // <pre>{ username: "barney", password: "rUbbL3" }</pre>
    // <P>
    // This function should then attempt to log in by whatever means is necessary.  The second
    // parameter to the loginFunc, "dialogCallback", is a function, which must be called <i>whether
    // login succeeds or fails</i> with a true/false value indicating whether login succeeded.
    // <P>
    // If the login dialog is dismissable (settable as properties.dismissable, default false) and
    // the user dismisses it, loginFunc will be fired with null for the credentials.
    // <P>
    // The following code shows typical usage.  This code assumes you have created a global
    // function sendCredentials() that send credentials to some authentication system and fires a
    // callback function with the result:
    // <pre>
    // ...
    // loginFunc : function (credentials, dialogCallback) {
    //     if (credentials == null) return; // dismissed
    //
    //     // send credentials
    //     sendCredentials(credentials, function (loginSucceeded) {
    //         // report success or failure
    //         dialogCallback(loginSucceeded);
    //     })
    // })
    // ...
    // </pre>
    // @param credentials (Object) Login credentials supplied by the user
    // @param dialogCallback (Function) Function that must be called once the login transaction
    // completes
    // @visibility external
    //<

    //> @attr loginDialog.dismissable (Boolean : false : [IR])
    // Whether the user should be able to dismiss the login dialog without entering
    // credentials.  Set to true if logging in is optional.  When set, a close button will be
    // present, and hitting escape will also dismiss the dialog.
    // <p>
    // If the Dialog is dismissed, +link{LoginDialog.loginFunc} is called with null arguments.
    // <p>
    // Note that this attribute overrides the dismissOnEscape and showCloseButton attributes.
    // @visibility external
    //<
    dismissable: false,

    //> @attr   loginDialog.dismissOnEscape  (boolean : null : [IRW])
    // Do not set LoginDialog.dismissOnEscape; it is controlled by the
    // +link{LoginDialog.dismissable}
    // property.
    // @visibility external
    //<

    //>	@attr	loginDialog.showCloseButton		(boolean : true : [IRW])
    // Do not set LoginDialog.showCloseButton; it is controlled by the
    // +link{LoginDialog.dismissable}
    // property.
    // @visibility external
    //<

    //> @attr loginDialog.allowBlankPassword (Boolean : false : IR)
    // If true, the login form will allow blank passwords to be submitted. Otherwise
    // the form fails to be validated until the user enters at least one character into
    // the password field.
    // @visibility external
    //<
    allowBlankPassword: false,

    //> @attr loginDialog.showLostPasswordLink (Boolean : false : IR)
    // If true, display a +link{LinkItem} (+link{LoginDialog.lostPasswordItem})
    // meant for the user to click if the account's
    // credentials are forgotten. The text of the link is controlled by
    // +link{loginDialog.lostPasswordItemTitle}. If clicked, the link will fire
    // +link{loginDialog.lostPassword()}.
    // @visibility external
    //<
    showLostPasswordLink: false,

    //> @attr loginDialog.showRegistrationLink (Boolean : false : IR)
    // If true, display a +link{LinkItem} (+link{LoginDialog.registrationItem})
    // meant for the user to click if the user wishes to register a new account.
    // The text of the link is controlled by
    // +link{loginDialog.registrationItemTitle}. If clicked, the link will fire
    // +link{loginDialog.register()}.
    // @visibility external
    //<
    showRegistrationLink: false,

    //> @attr loginDialog.title (String : Dialog.LOGIN_TITLE : IR)
    // Specifies the title of the dialog box.
    // @visibility external
    //<

    //> @attr loginDialog.errorStyle (String : "formCellError" : IR)
    // Specifies the CSS style of the error text shown for a login failure.
    // @visibility external
    //<
    errorStyle: "formCellError",

    //> @attr loginDialog.usernameItemTitle (String : Dialog.USERNAME_TITLE : IR)
    // Specifies the title of the "usernameItem" field of the +link{loginForm}.
    // @visibility external
    //<

    //> @attr loginDialog.passwordItemTitle (String : Dialog.PASSWORD_TITLE : IR)
    // Specifies the title of the "passwordItem" field of the +link{loginForm}.
    // @visibility external
    //<

    //> @attr loginDialog.loginButtonTitle (String : Dialog.LOGIN_BUTTON_TITLE : IR)
    // Specifies the contents of the login submission button of the +link{loginForm}.
    // @visibility external
    //<

    //> @attr loginDialog.lostPasswordItemTitle (String : LoginDialog.lostPasswordItemTitle : IR)
    // Specifies the contents of the password request button (if configured) on
    // the +link{loginForm}.
    // @visibility external
    //<
    lostPasswordItemTitle: "Lost Password?",

    //> @attr loginDialog.registrationItemTitle (String : LoginDialog.registrationItemTitle : IR)
    // Specifies the contents of the registration link (if configured) on
    // the +link{loginForm}.
    // @visibility external
    //<
    registrationItemTitle: "Register",

    //> @attr loginDialog.errorMessage (String : Dialog.LOGIN_ERROR_MESSAGE : IR)
    // Specifies the default error message displayed on the login form when
    // authentication fails.
    // @visibility external
    //<

    autoCenter: true,
    autoSize: true,
    isModal: true,
    showMinimizeButton:false,

    //> @attr loginDialog.items (Array of String : ["autoChild:loginForm"] : IR)
    // Specifies the dialog contents. By default, the dialog only contains
    // +link{LoginDialog.loginForm}. If desired, additional widgets may be placed before/after
    // the loginForm. To specify these widgets as +link{group:autoChildren}, use the syntax
    // "autoChild:<i>childName</i>" +link{group:autoChildren,as used for panes/items of
    // Tabs/SectionStacks}.
    // @visibility external
    //<
    items: [ "autoChild:loginForm" ],


    //> @attr loginDialog.formFields (Array of FormItem Properties : null : IR)
    // Customizes the fields present in the dialog, or specifies new fields to be
    // present, in the same manner as with +link{DynamicForm.fields}.
    //
    // @see DataBoundComponent.fields
    // @visibility external
    //<

    //> @attr loginDialog.loginFailureItem ( AutoChild BlurbItem : null : [IR] )
    // Field item containing login error message (if required) in +link{LoginDialog.loginForm}.
    // @visibility external
    //<

    //> @attr loginDialog.usernameItem ( AutoChild TextItem : null : [IR] )
    // Username field item in +link{LoginDialog.loginForm}.
    // @visibility external
    //<

    //> @attr loginDialog.lostPasswordItem ( AutoChild LinkItem : null : [IR] )
    // +link{linkItem} to page requesting forgotten password in +link{LoginDialog.loginForm}.
    // <p>To handle user clicks on this link, implement +link{loginDialog.lostPassword}.
    // <p>To handle a user click as a physical link to another page, set
    // +link{formItem.defaultValue,defaultValue} via loginDialog.lostPasswordItemProperties:
    // <code>
    // lostPasswordItemProperties: {
    //     defaultValue: "register.html"
    // },
    // </code>
    // @see loginDialog.showLostPasswordLink
    // @see loginDialog.lostPasswordItemTitle
    // @visibility external
    //<

    //> @attr loginDialog.registrationItem ( AutoChild LinkItem : null : [IR] )
    // +link{linkItem} to page requesting new user registration in +link{LoginDialog.loginForm}.
    // <p>To handle user clicks on this link, implement +link{loginDialog.register}.
    // <p>To handle a user click as a physical link to another page, set
    // +link{formItem.defaultValue,defaultValue} via loginDialog.registrationItemProperties:
    // <code>
    // registrationItemProperties: {
    //     defaultValue: "register.html"
    // },
    // </code>
    // @see loginDialog.showRegistrationLink
    // @see loginDialog.registrationItemTitle
    // @visibility external
    //<

    //> @attr loginDialog.passwordItem ( AutoChild PasswordItem : null : [IR] )
    // Password field item in +link{LoginDialog.loginForm}.
    // @see loginDialog.allowBlankPassword
    // @see loginDialog.passwordItemTitle
    // @visibility external
    //<

    //> @attr loginDialog.loginButton ( AutoChild ButtonItem : null : [IR] )
    // Login submission button in +link{LoginDialog.loginForm}.
    // @see loginDialog.loginButtonTitle
    // @visibility external
    //<

    //> @attr loginDialog.defaultValues ( Object : null : [IR] )
    // Adds default values to +link{loginDialog.loginForm}.
    // @visibility internal
    //<

    //> @attr loginDialog.loginForm ( AutoChild DynamicForm : null : R )
    // Form used to request login credentials from the user.
    // @see loginDialog.formFields
    // @visibility external
    //<

    loginFormConstructor: "DynamicForm",
    loginFormDefaults: {
        numCols: 2,
        padding: 4,
        autoDraw: false,
        saveOnEnter:true,

        submit : function () {
            var loginForm = this,
                params = [{
                    username : this.getValue("usernameItem"),
                    password : this.getValue("passwordItem")
                }];

            params[1] = function (success, errorMessage) {
                if (success) {
                    loginForm.complete(); // report success
                } else {
                    // failed login attempt - indicate failure, remain visible
                    if (errorMessage != null)
                        loginForm.setValue("loginFailureItem", errorMessage)
                    loginForm.showItem("loginFailureItem");
                    loginForm.focusInItem("passwordItem");
                }
            };

            this.fireCallback(this.loginDialog.loginFunc, "credentials,dialogCallback", params);
        },
        complete : function (dismissed) {
            this.loginDialog.hide();
            this.setValue("loginFailureItem", this.loginDialog.errorMessage);
            // reset for next time
            this.setValue("usernameItem", "");
            this.setValue("passwordItem", "");
            this.hideItem("loginFailureItem");

            // if this was a dismissal, tell the loginFunc
            if (dismissed) {
                this.fireCallback(this.loginFunc, "credentials,dialogCallback");
            } else {
                // if the server provided a loginRedirect, use it. This will be set
                // if an Authentication login page is visited without having credentials.
                
                var loginRedirect = isc.Cookie.get("loginRedirect");
                if (loginRedirect) window.location.replace(loginRedirect);
            }
        }
    },
    
    formDSDefaults: {
        clientOnly: true,
        useAllDataSourceFields: true
    },
    formDefaultFields: [
        { name: "loginFailureItem", type:"blurb", colSpan: 2, visible:false },
        { name: "usernameItem", required:true,
            // Diable spell checking etc where supported
            browserSpellCheck:false,
            browserAutoCorrect:false,
            browserAutoCapitalize:false,

            keyPress : function (item, form, keyName) {
                if (keyName == "Enter") {
                    form.focusInItem("passwordItem");
                    return false;
        }}},
        { name: "passwordItem", type: "password", required: true },
        { name: "loginButton", type:"button", type:"submit" },
        { name: "lostPasswordItem", type: "link", target: "javascript", canEdit:false,
            endRow: true, numCols:2, colSpan:2, showTitle: false,
            click: "form.loginDialog.lostPassword(form.getValues(), form)"
        },
        { name: "registrationItem", type: "link", target: "javascript", canEdit:false,
            endRow: true, numCols: 2, colSpan: 2, showTitle: false,
            click: "form.loginDialog.register(form.getValues(), form)"
        }
    ],

    getDynamicDefaults : function (child) {
        switch (child) {
        case "loginForm":
            var ret = {
                loginDialog: this,
                values: {
                    usernameItem: this.username || "",
                    passwordItem: this.password || "",
                    loginFailureItem: this.errorMessage
                },
                fields: this.formFields
            };

            // Bind form to datasource containing internally specified FormItem
            // properties. This datasource is updated with properties slurped up
            // from LoginDialog itself, ie usernameItemTitle, etc.
            // The user manipulates the form items either through
            // <item name>Properties (which ultimately affects the datasource
            // fields) or through formFields (which ultimately affects
            // form.fields itself).

            // safe to clone - not manipulated yet
            var updatedFields = isc.clone(this.formDefaultFields);

            // Build fields from this.<fieldName>Defaults + this.<fieldName>Properties +
            // this.<fieldName>Title. However, LinkItem fields need special treatment
            // as their titles specifically map to linkTitle if showTitle:false...
            for (var j=0; j<updatedFields.length; j++) {
                var field = updatedFields[j], name = field.name;

                isc.addProperties(field, this[name+"Defaults"], this[name+"Properties"]);

                if (null != this[name + "Title"]) {
                    field.title = this[name + "Title"];
                    if (field.type == 'link' && !field.showTitle)
                        field.linkTitle = this[name + "Title"];
                }

                // Go through some extra contortions so that eg "showMyField" maps to
                // field of name "myField".
                
                var showField = this["show" + name.substr(0, 1).toUpperCase() +
                    name.substr(1)];
                if (null != showField) field.visible = showField;

                // custom logic needed for some fields
                switch (name) {
                case "registrationItem": field.visible = this.showRegistrationLink; break;
                case "lostPasswordItem": field.visible = this.showLostPasswordLink; break;
                case "loginFailureItem": field.cellStyle = this.errorStyle; break;
                case "passwordItem": field.required = !this.allowBlankPassword; break;
                }
                updatedFields[j] = field;
            }
            ret.dataSource = isc.DataSource.create(this.formDSDefaults,{fields:updatedFields});

            // Note that LoginDialog.init controls initialization of some field attributes,
            // like errorStyle and values, which are controlled from uniquely named
            // LoginDialog attributes rather than <fieldName>Defaults etc.
            return ret;
        }
        return null;
    },
    cancelClick : function () { this.loginForm.complete(true) },
    init : function () {
        
        if (isc.LoginDialog.firstTimeInit) {
            isc.LoginDialog.firstTimeInit = false;
            isc.LoginDialog.addProperties({
                title: isc.Dialog.LOGIN_TITLE,
                usernameItemTitle: isc.Dialog.USERNAME_TITLE,
                passwordItemTitle: isc.Dialog.PASSWORD_TITLE,
                loginButtonTitle: isc.Dialog.LOGIN_BUTTON_TITLE,
                errorMessage: isc.Dialog.LOGIN_ERROR_MESSAGE
            });
        }
        this.dismissOnEscape = this.showCloseButton = this.dismissable;
        this.Super("init", arguments);
        this.loginForm.focusInItem("usernameItem");
        // handle initial values
        // this functionality was lost in the merge into mainline from 70RC
        if (this.username) this.loginForm.setValue("usernameItem", this.username);
        if (this.password) this.loginForm.setValue("passwordItem", this.password);
    }
});

isc.showLoginDialog = function (loginFunc, properties) {
    return isc.LoginDialog.create(isc.addProperties({}, properties, { autoDraw:true, loginFunc: loginFunc }));
}


// NOTE: unfinished dialog to confirm save when closing / exiting an application, or otherwise
// dropping edits.
// Typical Windows buttons: [*Yes*, No, Cancel]
// Typical Mac buttons: [Don't Save, separator, Cancel, *Save*]
/*
isc.confirmSave = function (message, callback, properties) {
    isc.confirm(message || isc.Dialog.saveChangesMessage, {
                    buttons:[isc.Dialog.OK,
                             {title:"Save", width:75,
                              click:"this.hide();this.topElement.returnValue('save');"},
                             isc.Dialog.CANCEL]
                }
                );
}
*/
