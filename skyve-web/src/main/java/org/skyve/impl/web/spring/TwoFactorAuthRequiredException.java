package org.skyve.impl.web.spring;

import org.springframework.security.core.AuthenticationException;

public class TwoFactorAuthRequiredException extends AuthenticationException {

	private static final long serialVersionUID = -4848026331324090665L;

	// false : indicates that we need to redirect the user to a page with 2FA code entry 
	// true : indicates user sent 2FA code and failed authentication 
	private boolean authenticationFailure;
	
	
	public TwoFactorAuthRequiredException(String msg, boolean authenticationFailure) {
		super(msg);
		this.authenticationFailure = authenticationFailure;
	}

	public boolean isAuthenticationFailure() {
		return this.authenticationFailure;
	}
	
}
