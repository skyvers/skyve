package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.ActionPrivilegeMetaData;
import org.skyve.impl.metadata.repository.module.ContentPermission;
import org.skyve.impl.metadata.repository.module.ContentRestriction;
import org.skyve.impl.metadata.repository.module.DocumentPrivilegeMetaData;
import org.skyve.metadata.user.DocumentPermission;

/**
 * Builds document privilege metadata, including action and content constraints.
 */
public class FluentDocumentPrivilege {
	private DocumentPrivilegeMetaData privilege = null;
	
	/**
	 * Creates a fluent wrapper with a new metadata instance.
	 */
	public FluentDocumentPrivilege() {
		privilege = new DocumentPrivilegeMetaData();
	}

	/**
	 * Creates a fluent wrapper around an existing metadata instance.
	 *
	 * @param privilege The metadata to mutate.
	 */
	public FluentDocumentPrivilege(DocumentPrivilegeMetaData privilege) {
		this.privilege = privilege;
	}
	
	/**
	 * Copies a document privilege including action and content constraints.
	 *
	 * @param privilege The source privilege.
	 * @return this fluent instance.
	 */
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

	/**
	 * Sets the target document name for this privilege.
	 *
	 * @param documentName The document name.
	 * @return this fluent instance.
	 */
	public FluentDocumentPrivilege documentName(String documentName) {
		privilege.setDocumentName(documentName);
		return this;
	}

	/**
	 * Sets the base document permission.
	 *
	 * @param permission The permission value.
	 * @return this fluent instance.
	 */
	public FluentDocumentPrivilege permission(DocumentPermission permission) {
		privilege.setPermission(permission);
		return this;
	}
	
	/**
	 * Adds an action-level privilege by action name.
	 *
	 * @param actionName The action name.
	 * @return this fluent instance.
	 */
	public FluentDocumentPrivilege addActionPrivilege(String actionName) {
		ActionPrivilegeMetaData action = new ActionPrivilegeMetaData();
		action.setActionName(actionName);
		privilege.getActions().add(action);
		return this;
	}

	/**
	 * Removes an action-level privilege by action name.
	 *
	 * @param actionName The action name.
	 * @return this fluent instance.
	 */
	public FluentDocumentPrivilege removeActionPrivilege(String actionName) {
		privilege.getActions().removeIf(a -> actionName.equals(a.getActionName()));
		return this;
	}

	/**
	 * Clears all configured action privileges.
	 *
	 * @return this fluent instance.
	 */
	public FluentDocumentPrivilege clearActionPrivileges() {
		privilege.getActions().clear();
		return this;
	}
	
	/**
	 * Adds a content permission for the specified attribute.
	 *
	 * @param attributeName The attribute name.
	 * @return this fluent instance.
	 */
	public FluentDocumentPrivilege addContentPermission(String attributeName) {
		ContentPermission content = new ContentPermission();
		content.setAttributeName(attributeName);
		// NB No need to set documentName as this is done on ModuleMetaData.convert()
		privilege.getContentPermissions().add(content);
		return this;
	}

	/**
	 * Removes a content permission by attribute name.
	 *
	 * @param attributeName The attribute name.
	 * @return this fluent instance.
	 */
	public FluentDocumentPrivilege removeContentPermission(String attributeName) {
		privilege.getContentPermissions().removeIf(p -> attributeName.equals(p.getAttributeName()));
		return this;
	}

	/**
	 * Clears all configured content permissions.
	 *
	 * @return this fluent instance.
	 */
	public FluentDocumentPrivilege clearContentPermissions() {
		privilege.getContentPermissions().clear();
		return this;
	}

	/**
	 * Adds a content restriction for the specified attribute.
	 *
	 * @param attributeName The attribute name.
	 * @return this fluent instance.
	 */
	public FluentDocumentPrivilege addContentRestriction(String attributeName) {
		ContentRestriction content = new ContentRestriction();
		content.setAttributeName(attributeName);
		// NB No need to set documentName as this is done on ModuleMetaData.convert()
		privilege.getContentRestrictions().add(content);
		return this;
	}

	/**
	 * Removes a content restriction by attribute name.
	 *
	 * @param attributeName The attribute name.
	 * @return this fluent instance.
	 */
	public FluentDocumentPrivilege removeContentRestriction(String attributeName) {
		privilege.getContentRestrictions().removeIf(r -> attributeName.equals(r.getAttributeName()));
		return this;
	}

	/**
	 * Clears all configured content restrictions.
	 *
	 * @return this fluent instance.
	 */
	public FluentDocumentPrivilege clearContentRestrictions() {
		privilege.getContentRestrictions().clear();
		return this;
	}

	/**
	 * Returns the mutable metadata backing this fluent wrapper.
	 *
	 * @return The document privilege metadata instance.
	 */
	public DocumentPrivilegeMetaData get() {
		return privilege;
	}
}
