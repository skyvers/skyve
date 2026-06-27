package org.skyve.domain.messages;

import org.skyve.impl.util.UtilImpl;

/**
 * Thrown when a page or resource is requested by a user agent (browser, mobile device,
 * or API client) that the current user does not have access to according to the Skyve
 * ACL.
 *
 * <p>Access control is derived from document permissions, module role definitions,
 * and view-level access rules declared in metadata. This exception is a specialisation
 * of {@link SecurityException} that represents a URL/resource access denial rather
 * than a data-entity privilege violation.
 *
 * <p>Side effects: if {@link org.skyve.impl.util.UtilImpl#ACCESS_EXCEPTION_NOTIFICATIONS}
 * is enabled, constructing this exception sends an email notification.
 *
 * @see SecurityException
 * @see org.skyve.util.SecurityUtil
 */
@SuppressWarnings("java:S110") // This inheritance-depth warning is ridiculous for intentional framework hierarchies.
public class AccessException extends SecurityException {
	private static final long serialVersionUID = 6386350703055578L;

	public AccessException(String resource, String userName) {
		super(resource, userName);
	}

	@Override
	protected boolean isEmailed() {
		return UtilImpl.ACCESS_EXCEPTION_NOTIFICATIONS;
	}
}
