package org.skyve.metadata.module.fluent;

import java.util.Collections;
import java.util.Map;

import org.skyve.impl.metadata.repository.module.ActionPrivilegeMetaData;
import org.skyve.impl.metadata.repository.module.ContentPermission;
import org.skyve.impl.metadata.repository.module.ContentRestriction;
import org.skyve.impl.metadata.repository.module.DocumentPrivilegeMetaData;
import org.skyve.metadata.user.DocumentPermission;

import jakarta.annotation.Nonnull;

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
			addActionPrivilege(action.getActionName(), action.getProperties());
		}
		for (ContentPermission permission : privilege.getContentPermissions()) {
			addContentPermission(permission.getAttributeName(), permission.getProperties());
		}
		for (ContentRestriction restriction : privilege.getContentRestrictions()) {
			addContentRestriction(restriction.getAttributeName(), restriction.getProperties());
		}
		privilege.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));

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
		return addActionPrivilege(actionName, Collections.emptyMap());
	}

	/**
	 * Adds an action-level privilege by action name, carrying its custom properties.
	 *
	 * @param actionName The action name.
	 * @param properties The custom properties to copy onto the new privilege, in iteration order.
	 * @return this fluent instance.
	 */
	FluentDocumentPrivilege addActionPrivilege(String actionName, Map<String, String> properties) {
		ActionPrivilegeMetaData action = new ActionPrivilegeMetaData();
		action.setActionName(actionName);
		action.getProperties().putAll(properties);
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
		return addContentPermission(attributeName, Collections.emptyMap());
	}

	/**
	 * Adds a content permission for the specified attribute, carrying its custom properties.
	 *
	 * @param attributeName The attribute name.
	 * @param properties The custom properties to copy onto the new permission, in iteration order.
	 * @return this fluent instance.
	 */
	FluentDocumentPrivilege addContentPermission(String attributeName, Map<String, String> properties) {
		ContentPermission content = new ContentPermission();
		content.setAttributeName(attributeName);
		content.getProperties().putAll(properties);
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
		return addContentRestriction(attributeName, Collections.emptyMap());
	}

	/**
	 * Adds a content restriction for the specified attribute, carrying its custom properties.
	 *
	 * @param attributeName The attribute name.
	 * @param properties The custom properties to copy onto the new restriction, in iteration order.
	 * @return this fluent instance.
	 */
	FluentDocumentPrivilege addContentRestriction(String attributeName, Map<String, String> properties) {
		ContentRestriction content = new ContentRestriction();
		content.setAttributeName(attributeName);
		content.getProperties().putAll(properties);
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
	 * Adds or replaces a custom property on the wrapped metadata.
	 * Properties keep the order in which they were first added.
	 *
	 * @param k the property name
	 * @param v the property value
	 * @return this builder
	 */
	public @Nonnull FluentDocumentPrivilege putProperty(@Nonnull String k, @Nonnull String v) {
		privilege.getProperties().put(k, v);
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
