package org.skyve.impl.metadata.user;

import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.skyve.metadata.module.menu.Menu;

import jakarta.annotation.Nullable;

/**
 * A serialisable snapshot of the current user's identity and permissions,
 * transferred to client-side code (e.g. the SmartClient or Vue front-end).
 *
 * <p>Contains the user's biz key, contact, customer name, data-group id,
 * assigned role names, per-document CRUD permissions map, allowed action names,
 * and the rendered module-menu map.
 */
public class ClientUserData {
	private String id;
	private String name;
	private String contactId;
	private String contactName;
	private String customerName;
	private String dataGroupId;
	private Set<String> roleNames = null;

	/**
	 * Document Name -> CRUD permission
	 */
	private Map<String, Map<String, Boolean>> documentPermissions = null;

	private Set<String> actions = null;

	private Map<String, Menu> moduleMenuMap = new TreeMap<>();

	public @Nullable Set<String> getActions() {
		return actions;
	}

	public void setActions(@Nullable Set<String> actions) {
		this.actions = actions;
	}

	public @Nullable String getContactId() {
		return contactId;
	}

	public void setContactId(@Nullable String contactId) {
		this.contactId = contactId;
	}

	public @Nullable String getContactName() {
		return contactName;
	}

	public void setContactName(@Nullable String contactName) {
		this.contactName = contactName;
	}

	public @Nullable String getCustomerName() {
		return customerName;
	}

	public void setCustomerName(@Nullable String customerName) {
		this.customerName = customerName;
	}

	public @Nullable String getDataGroupId() {
		return dataGroupId;
	}

	public void setDataGroupId(@Nullable String dataGroupId) {
		this.dataGroupId = dataGroupId;
	}

	public @Nullable Map<String, Map<String, Boolean>> getDocumentPermissions() {
		return documentPermissions;
	}

	public void setDocumentPermissions(@Nullable Map<String, Map<String, Boolean>> documentPermissions) {
		this.documentPermissions = documentPermissions;
	}

	public @Nullable String getId() {
		return id;
	}

	public void setId(@Nullable String id) {
		this.id = id;
	}

	public @Nullable String getName() {
		return name;
	}

	public void setName(@Nullable String name) {
		this.name = name;
	}

	public @Nullable Set<String> getRoleNames() {
		return roleNames;
	}

	public void setRoleNames(@Nullable Set<String> roleNames) {
		this.roleNames = roleNames;
	}

	public @Nullable Map<String, Menu> getModuleMenuMap() {
		return moduleMenuMap;
	}

	public void setModuleMenuMap(@Nullable Map<String, Menu> moduleMenuMap) {
		this.moduleMenuMap = moduleMenuMap;
	}
}
