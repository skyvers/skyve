package org.skyve.util.test;

import org.skyve.metadata.user.User;

/**
 * Extend this class in <document-name>Factory classes to expose test data.
 * @author mike
 */
public abstract class DataFactory {
	private User user;

	public User getUser() {
		return user;
	}

	public void setUser(User user) {
		this.user = user;
	}
}
