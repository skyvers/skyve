package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.ActionPrivilege;
import org.skyve.impl.metadata.repository.module.ContentPermission;
import org.skyve.impl.metadata.repository.module.ContentRestriction;
import org.skyve.impl.metadata.repository.module.DocumentPrivilege;
import org.skyve.metadata.user.DocumentPermission;

public class FluentDocumentPrivilege {
	private DocumentPrivilege privilege = new DocumentPrivilege();
	
	public FluentDocumentPrivilege() {
		// nothing to see
	}

	public FluentDocumentPrivilege documentName(String documentName) {
		privilege.setDocumentName(documentName);
		return this;
	}

	public FluentDocumentPrivilege permission(DocumentPermission permission) {
		privilege.setPermission(permission);
		return this;
	}
	
	public FluentDocumentPrivilege addActionPrivilege(String actionName) {
		ActionPrivilege action = new ActionPrivilege();
		action.setActionName(actionName);
		privilege.getActions().add(action);
		return this;
	}

	public FluentDocumentPrivilege addContentPermission(String attributeName) {
		ContentPermission content = new ContentPermission();
		content.setAttributeName(attributeName);
		privilege.getContentPermissions().add(content);
		return this;
	}
	
	public FluentDocumentPrivilege addContentRestriction(String attributeName) {
		ContentRestriction content = new ContentRestriction();
		content.setAttributeName(attributeName);
		privilege.getContentRestrictions().add(content);
		return this;
	}

	public DocumentPrivilege get() {
		return privilege;
	}
}
