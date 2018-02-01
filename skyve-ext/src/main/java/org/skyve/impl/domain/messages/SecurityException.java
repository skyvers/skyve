package org.skyve.impl.domain.messages;

import org.skyve.domain.messages.DomainException;

public class SecurityException extends DomainException {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2941808458696267548L;

	public SecurityException(String resource, String userName) {
		super(userName + " does not have access to " + resource);
	}
}
