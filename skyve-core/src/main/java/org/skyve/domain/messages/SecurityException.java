package org.skyve.domain.messages;

/**
 * Thrown when the current user does not have privileges to 
 * a perform an operation on a data entity or web resource.
 */
public class SecurityException extends DomainException {
	private static final long serialVersionUID = 2941808458696267548L;

	public SecurityException(String resource, String userName) {
		super(userName + " does not have access to " + resource);
	}
}
