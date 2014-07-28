package org.skyve.wildcat.metadata.user;

import org.skyve.metadata.user.DocumentPermission;

public class DocumentPrivilege extends Privilege {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -7153161155834347282L;

	private DocumentPermission permission;

	public DocumentPermission getPermission() {
		return permission;
	}

	public void setPermission(DocumentPermission permission) {
		this.permission = permission;
	}
}
