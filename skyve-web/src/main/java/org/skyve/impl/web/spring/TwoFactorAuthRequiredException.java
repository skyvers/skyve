package org.skyve.impl.web.spring;

import org.springframework.security.core.AuthenticationException;

public class TwoFactorAuthRequiredException extends AuthenticationException {

	private static final long serialVersionUID = -4848026331324090665L;

	// false: initial 2FA challenge has been issued and the user should enter their code.
	// true: user submitted an invalid/expired 2FA code and an error should be shown.
	private final boolean invalidTwoFactorCode;
	
	
	public TwoFactorAuthRequiredException(String msg, boolean invalidTwoFactorCode) {
		super(msg);
		this.invalidTwoFactorCode = invalidTwoFactorCode;
	}

	public boolean isInvalidTwoFactorCode() {
		return invalidTwoFactorCode;
	}
	
}
