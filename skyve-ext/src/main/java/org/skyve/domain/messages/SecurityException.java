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

		// Retrieve email configuration
		boolean email;
		if (this instanceof AccessException) {
			email = UtilImpl.ACCESS_EXCEPTION_NOTIFICATIONS;
		} else {
			email = UtilImpl.SECURITY_EXCEPTION_NOTIFICATIONS;
		}

		// Log as security event
		SecurityUtil.log(this, email);
	}
}
