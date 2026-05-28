package org.skyve.impl.web.spring;

import org.springframework.security.core.AuthenticationException;

public class TwoFactorAuthRequiredException extends AuthenticationException {
	private static final long serialVersionUID = -4848026331324090665L;

	// false: initial 2FA challenge has been issued and the user should enter their code.
	// true: user submitted an invalid/expired 2FA code and an error should be shown.
	private final boolean invalidTwoFactorCode;
	
	
	/**
	 * Creates an authentication exception indicating that two-factor verification is required.
	 *
	 * @param msg The exception message.
	 * @param invalidTwoFactorCode Whether the submitted two-factor code is invalid or expired.
	 */
	public TwoFactorAuthRequiredException(String msg, boolean invalidTwoFactorCode) {
		super(msg);
		this.invalidTwoFactorCode = invalidTwoFactorCode;
	}

	/**
	 * Indicates whether the current two-factor challenge failed because the submitted code was invalid.
	 *
	 * @return True when the submitted two-factor code is invalid or expired.
	 */
	public boolean isInvalidTwoFactorCode() {
		return invalidTwoFactorCode;
	}
	
}
