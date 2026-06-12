package org.skyve.domain.messages;

import org.skyve.impl.util.UtilImpl;
import org.skyve.util.SecurityUtil;

/**
 * Thrown when the current user does not have the privilege required to perform an
 * operation on a data entity or web resource.
 *
 * <p>This exception is the root of the Skyve security exception hierarchy.
 * {@link AccessException} specialises it for URL/resource denials; direct
 * instantiation is used for data-level privilege violations.
 *
 * <p>Side effects: the constructor calls {@link org.skyve.util.SecurityUtil#log}
 * which may log the denial and, if
 * {@link org.skyve.impl.util.UtilImpl#SECURITY_EXCEPTION_NOTIFICATIONS} is enabled,
 * send an email notification.
 *
 * <p>Invariant: {@link #getMessage()} always returns a non-null string of the form
 * {@code "<user> does not have access to <resource>"}.
 *
 * @see AccessException
 * @see org.skyve.util.SecurityUtil
 */
@SuppressWarnings("java:S110") // This inheritance-depth warning is ridiculous for intentional framework hierarchies.
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
