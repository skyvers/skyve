package org.skyve.domain.messages;

/**
 * Thrown when a page or resource is requested from a user agent (browser, mobile device, API client)
 * that the current user does not have access to.
 * Skyve derives an Access Control List from various metadata elements, mainly the menu and views.
 * Accesses can be defined in views or in module roles also.
 */
public class AccessException extends SecurityException {
	private static final long serialVersionUID = 6386350703055578L;

	public AccessException(String resource, String userName) {
		super(resource, userName);
	}
}
