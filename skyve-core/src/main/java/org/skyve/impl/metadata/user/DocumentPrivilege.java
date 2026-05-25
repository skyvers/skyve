package org.skyve.impl.metadata.user;

import org.skyve.metadata.user.DocumentPermission;

/**
 * A privilege granting a specific CRUD permission for a document within a role.
 *
 * <p>The {@link org.skyve.metadata.user.DocumentPermission} records the allowed
 * operations (create, read, update, delete) and the applicable
 * {@link org.skyve.metadata.user.DocumentPermissionScope} (global, customer,
 * data-group, or user).
 */
public class DocumentPrivilege extends Privilege {
	private static final long serialVersionUID = -7153161155834347282L;

	private DocumentPermission permission;

	public DocumentPermission getPermission() {
		return permission;
	}

	public void setPermission(DocumentPermission permission) {
		this.permission = permission;
	}
}
