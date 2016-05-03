package org.skyve.impl.metadata.user;

import org.skyve.metadata.user.DocumentPermission;
import org.skyve.impl.metadata.user.Privilege;

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
