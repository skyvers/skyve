package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.ActionPrivilegeMetaData;
import org.skyve.impl.metadata.repository.module.ContentPermission;
import org.skyve.impl.metadata.repository.module.ContentRestriction;
import org.skyve.impl.metadata.repository.module.DocumentPrivilegeMetaData;
import org.skyve.metadata.user.DocumentPermission;

public class FluentDocumentPrivilege {
	private DocumentPrivilegeMetaData privilege = null;
	
	public FluentDocumentPrivilege() {
		privilege = new DocumentPrivilegeMetaData();
	}

	public FluentDocumentPrivilege(DocumentPrivilegeMetaData privilege) {
		this.privilege = privilege;
	}
	
	public FluentDocumentPrivilege from(@SuppressWarnings("hiding") DocumentPrivilegeMetaData privilege) {
		documentName(privilege.getDocumentName());
		permission(privilege.getPermission());
		for (ActionPrivilegeMetaData action : privilege.getActions()) {
			addActionPrivilege(action.getActionName());
		}
		for (ContentPermission permission : privilege.getContentPermissions()) {
			addContentPermission(permission.getAttributeName());
		}
		for (ContentRestriction restriction : privilege.getContentRestrictions()) {
			addContentRestriction(restriction.getAttributeName());
		}
		return this;
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
		ActionPrivilegeMetaData action = new ActionPrivilegeMetaData();
		action.setActionName(actionName);
		privilege.getActions().add(action);
		return this;
	}

	public FluentDocumentPrivilege removeActionPrivilege(String actionName) {
		privilege.getActions().removeIf(a -> actionName.equals(a.getActionName()));
		return this;
	}

	public FluentDocumentPrivilege clearActionPrivileges() {
		privilege.getActions().clear();
		return this;
	}
	
	public FluentDocumentPrivilege addContentPermission(String attributeName) {
		ContentPermission content = new ContentPermission();
		content.setAttributeName(attributeName);
		// NB No need to set documentName as this is done on ModuleMetaData.convert()
		privilege.getContentPermissions().add(content);
		return this;
	}

	public FluentDocumentPrivilege removeContentPermission(String attributeName) {
		privilege.getContentPermissions().removeIf(p -> attributeName.equals(p.getAttributeName()));
		return this;
	}

	public FluentDocumentPrivilege clearContentPermissions() {
		privilege.getContentPermissions().clear();
		return this;
	}

	public FluentDocumentPrivilege addContentRestriction(String attributeName) {
		ContentRestriction content = new ContentRestriction();
		content.setAttributeName(attributeName);
		// NB No need to set documentName as this is done on ModuleMetaData.convert()
		privilege.getContentRestrictions().add(content);
		return this;
	}

	public FluentDocumentPrivilege removeContentRestriction(String attributeName) {
		privilege.getContentRestrictions().removeIf(r -> attributeName.equals(r.getAttributeName()));
		return this;
	}

	public FluentDocumentPrivilege clearContentRestrictions() {
		privilege.getContentRestrictions().clear();
		return this;
	}

	public DocumentPrivilegeMetaData get() {
		return privilege;
	}
}
