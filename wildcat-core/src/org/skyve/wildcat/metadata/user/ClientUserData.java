package org.skyve.wildcat.metadata.user;

import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.skyve.metadata.module.menu.Menu;

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

	public Set<String> getActions() {
		return actions;
	}

	public void setActions(Set<String> actions) {
		this.actions = actions;
	}

	public String getContactId() {
		return contactId;
	}

	public void setContactId(String contactId) {
		this.contactId = contactId;
	}

	public String getContactName() {
		return contactName;
	}

	public void setContactName(String contactName) {
		this.contactName = contactName;
	}

	public String getCustomerName() {
		return customerName;
	}

	public void setCustomerName(String customerName) {
		this.customerName = customerName;
	}

	public String getDataGroupId() {
		return dataGroupId;
	}

	public void setDataGroupId(String dataGroupId) {
		this.dataGroupId = dataGroupId;
	}

	public Map<String, Map<String, Boolean>> getDocumentPermissions() {
		return documentPermissions;
	}

	public void setDocumentPermissions(Map<String, Map<String, Boolean>> documentPermissions) {
		this.documentPermissions = documentPermissions;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Set<String> getRoleNames() {
		return roleNames;
	}

	public void setRoleNames(Set<String> roleNames) {
		this.roleNames = roleNames;
	}

	public Map<String, Menu> getModuleMenuMap() {
		return moduleMenuMap;
	}

	public void setModuleMenuMap(Map<String, Menu> moduleMenuMap) {
		this.moduleMenuMap = moduleMenuMap;
	}
}
