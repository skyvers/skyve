// This is the default implementation of a login form used when the user submits an RPC that is
// intercepted by an authenticator.  See the client reference documentation for
// RPCManager.loginRequired() for more information on how/when this is used.
//
// You can modfy this in place or use your own implementation - this filename and location are
// not special in any way.  If you want to use this implementation, simply script include this
// file in your page and be sure to apply the HTML fragments in the
// /isomorphic/system/authentication/integration directory to your
// login/maxLoginAttemptsExceeded pages.
isc.RPCManager.addClassMethods({

loginRequired : function (transactionNum, rpcRequest, rpcResponse) {
    // hang on to the transactionNum so we can resubmit it
    this.transactionToResend = transactionNum;

    if(!this.loginWindow) {
        this.loginForm = isc.DynamicForm.create({
            numCols: 2,
            autoDraw: false,
            fields : [
                { name:"loginFailure", type:"blurb", cellStyle:"formCellError",
                  defaultValue: "Invalid username or password", colSpan: 2,
                  visible:false },
                { name:"username", title:"Username", titleOrientation: "left",
                  keyPress : function (item, form, keyName) {
                    if (keyName == "Enter") {
                        form.focusInItem("password");
                        return false;
                    }
                }},
                { name:"password", title:"Password", titleOrientation: "left",
                  type:"password", keyPress : function (item, form, keyName) {
                  if (keyName == "Enter") {
                      form.loginWindow.doLogin();
                      return false;
                  }
                 }},
                 { type:"button", title:"Log in", click : "form.loginWindow.doLogin()" }
            ]
        });
        this.loginWindow = isc.LoginWindow.create({
            title: "Session expired - please log in",
            autoDraw: false,
            autoCenter: true,
            autoSize: true,
            showCloseButton: false,
            showMinimizeButton: false,
            isModal: true,
            loginForm: this.loginForm,
            items: [ this.loginForm ]
        });
        // make loginWindow available on loginForm so we can call methods on it for the Enter
        // key and the "Log in" button
        this.loginForm.loginWindow = this.loginWindow;
    }

    // only clear the form and re-focus in the username field if we're not already showing
    // it such that background RPCs occurring during our login attempt don't clear out the
    // form
   if (!(this.loginWindow.isVisible() && this.loginWindow.isDrawn())) {
        this.loginForm.clearValues(); 
        this.loginForm.delayCall("focusInItem", ["username"]);
    }

    this.loginWindow.show();
    this.loginWindow.bringToFront();
}
});


// LoginWindow - subclass of Window, adds methods for handling login responses
isc.defineClass("LoginWindow", "Window").addProperties({

// called by user pressing the login button or hitting enter in the password field.  Submit the
// request to the server.
doLogin : function () {
    isc.RPCManager.sendRequest({
        // let the RPCManager know not to delay this request and to discard this
        // request/response pair if the auth attempt fails.
        containsCredentials: true,
        // we must target the special loginSuccess.html file.  You can move it anywhere you
        // want, so long as your login attempts target it.
        actionURL: isc.RPCManager.credentialsURL,
        
        // we're not going to privide any data beyond the username, password query params
        useSimpleHttp: true,
        showPrompt: false,

        // the actual credentials, from the form
        params : {
            username: this.loginForm.getValue("username"),
            password: this.loginForm.getValue("password")
        },
        callback : this.getID()+".loginReply(rpcResponse)"
    });
},

// called when the server replies
loginReply : function (rpcResponse) {
    
    // clear the form values since either way we don't want to hold on to them
    this.loginForm.clearValues();
    var status = rpcResponse.status;
    if (status == isc.RPCResponse.STATUS_SUCCESS) {
        this.loginForm.hideItem("loginFailure");
        this.hide();

        // this resubmits all transactions that we previously delayed in loginRequired.  We do
        // this on a delay so the login.hide() renders immediately
        isc.RPCManager.delayCall("resendTransaction", [this.transactionToResend]);
        delete this.transactionsToResubmit;
    } else if (status == isc.RPCResponse.STATUS_LOGIN_INCORRECT) {
        this.loginForm.showItem("loginFailure");
    } else if (status == isc.RPCResponse.STATUS_MAX_LOGIN_ATTEMPTS_EXCEEDED) {
        isc.warn("Max login attempts exceeded.");
    }
    this.loginForm.focusInItem("username");
}

});
