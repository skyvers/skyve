// This is the default implementation of a login form used when the user submits an RPC that is
// intercepted by an authenticator.  See the client reference documentation for
// RPCManager.loginRequired() for more information on how/when this is used.
//
// You can modfy this in place or use your own implementation - this filename and location are
// not special in any way.  If you want to use this implementation, simply script include this
// file in your page and be sure to apply the HTML fragments in the
// /isomorphic/system/authentication/integration directory to your
// login/maxLoginAttemptsExceeded pages.
if (!isc.RPCManager.loginUsernameFieldName) {
    isc.RPCManager.loginUsernameFieldName = 
        window.isc_user && window.isc_user.loginUsernameFieldName ? window.isc_user.loginUsernameFieldName :
        "username";
} 
if (!isc.RPCManager.loginPasswordFieldName) {
    isc.RPCManager.loginPasswordFieldName = 
        window.isc_user && window.isc_user.loginPasswordFieldName ? window.isc_user.loginPasswordFieldName :
        "password";
}
isc.RPCManager.addClassMethods({

loginRequired : function (transactionNum, rpcRequest, rpcResponse) {
    // hang on to the transactionNum so we can resubmit it
    this.transactionToResend = transactionNum;

    if(!this.loginWindow) {
        this.loginForm = isc.DynamicForm.create(
            isc.addProperties({}, isc.RPCManager.loginFormDefaults, isc.RPCManager.loginFormProperties)
        );
        this.loginWindow = isc.LoginWindow.create(
            isc.addProperties({}, isc.RPCManager.loginWindowDefaults, isc.RPCManager.loginWindowProperties,
                {
                    loginForm: this.loginForm,
                    items: [ this.loginForm ]
                }
            )
        );
        // make loginWindow available on loginForm so we can call methods on it for the Enter
        // key and the "Log in" button
        this.loginForm.loginWindow = this.loginWindow;
    }

    // only clear the form and re-focus in the username field if we're not already showing
    // it such that background RPCs occurring during our login attempt don't clear out the
    // form
    if (!(this.loginWindow.isVisible() && this.loginWindow.isDrawn())) {
        this.loginForm.clearValues(); 
        this.loginForm.delayCall("focusInItem", [isc.RPCManager.lockReloginToCurrentUser ? 
            isc.RPCManager.loginPasswordFieldName : this.loginUsernameFieldName]);
    }

    this.loginWindow.show();
    this.loginWindow.bringToFront();
}

});


// LoginWindow - subclass of Window, adds methods for handling login responses
isc.defineClass("LoginWindow", "Window").addProperties({
    title: "Session expired - please log in",
    autoDraw: false,
    autoCenter: true,
    autoSize: true,
    showCloseButton: false,
    showMinimizeButton: false,
    isModal: true,

    // called by user pressing the login button or hitting enter in the password field.  Submit the
    // request to the server.
    doLogin : function () {
        var params = {};
        params[isc.RPCManager.loginUsernameFieldName] = this.loginForm.getValue(isc.RPCManager.loginUsernameFieldName);
        params[isc.RPCManager.loginPasswordFieldName] = this.loginForm.getValue(isc.RPCManager.loginPasswordFieldName);

        isc.RPCManager.sendRequest({
            // let the RPCManager know not to delay this request and to discard this
            // request/response pair if the auth attempt fails.
            containsCredentials: true,
            // we must target the special loginSuccess.html file.  You can move it anywhere you
            // want, so long as your login attempts target it.
            actionURL: isc.RPCManager.credentialsURL,
            
            // we're not going to provide any data beyond the username, password query params
            useSimpleHttp: true,
            showPrompt: false,

            // the actual credentials, from the form
            params : params,
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

            var txToResend = this.transactionToResend;
            if (txToResend == "isc_requestPageReload") {
                location.reload();
            } else if (txToResend != null) {
                // This resubmits all transactions that we previously delayed in loginRequired.
                // We do this on a delay so the login.hide() renders immediately.
                isc.RPCManager.delayCall("resendTransaction", [txToResend]);
            }
            delete this.transactionToResend;
        } else if (status == isc.RPCResponse.STATUS_LOGIN_INCORRECT) {
            this.loginForm.showItem("loginFailure");
        } else if (status == isc.RPCResponse.STATUS_MAX_LOGIN_ATTEMPTS_EXCEEDED) {
            isc.warn("Max login attempts exceeded.");
        }
        this.loginForm.focusInItem(isc.RPCManager.lockReloginToCurrentUser ?
            isc.RPCManager.loginPasswordFieldName : isc.RPCManager.loginUsernameFieldName);
    }

});
