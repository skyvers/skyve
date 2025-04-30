package org.skyve.domain.messages;

import org.skyve.impl.util.UtilImpl;
import org.skyve.util.SecurityUtil;

/**
 * Thrown when the current user does not have privileges to 
 * a perform an operation on a data entity or web resource.
 */
public class SecurityException extends DomainException {
	private static final long serialVersionUID = 2941808458696267548L;

	public SecurityException(String resource, String userName) {
		super(userName + " does not have access to " + resource);
		SecurityUtil.log(this, isEmailed());
	}

	/**
	 * Determines if this exception should trigger an email notification.
	 * 
	 * @return true if enabled in system configuration
	 */
	@SuppressWarnings("static-method")
	protected boolean isEmailed() {
		return UtilImpl.SECURITY_EXCEPTION_NOTIFICATIONS;
	}
}
