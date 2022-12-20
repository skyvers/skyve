package org.skyve.metadata.module.fluent;

import java.util.LinkedHashMap;
import java.util.List;

import org.skyve.impl.metadata.repository.module.ContentPermission;
import org.skyve.impl.metadata.repository.module.ContentRestriction;
import org.skyve.impl.metadata.repository.module.DocumentPrivilegeMetaData;
import org.skyve.impl.metadata.repository.module.ModuleRoleDocumentAggregateUserAccessMetaData;
import org.skyve.impl.metadata.repository.module.ModuleRoleMetaData;
import org.skyve.impl.metadata.repository.module.ModuleRoleModelAggregateUserAccessMetaData;
import org.skyve.impl.metadata.repository.module.ModuleRolePreviousCompleteUserAccessMetaData;
import org.skyve.impl.metadata.repository.module.ModuleRoleQueryAggregateUserAccessMetaData;
import org.skyve.impl.metadata.repository.module.ModuleRoleSingularUserAccessMetaData;
import org.skyve.impl.metadata.repository.module.ModuleRoleUserAccessMetaData;
import org.skyve.impl.metadata.user.ActionPrivilege;
import org.skyve.impl.metadata.user.DocumentPrivilege;
import org.skyve.impl.metadata.user.Privilege;
import org.skyve.impl.metadata.user.RoleImpl;
import org.skyve.metadata.user.Role;

public class FluentModuleRole {
	private ModuleRoleMetaData role = null;
	
	public FluentModuleRole() {
		this.role = new ModuleRoleMetaData();
	}
	
	public FluentModuleRole(ModuleRoleMetaData role) {
		this.role = role;
	}
	
	public FluentModuleRole from(@SuppressWarnings("hiding") Role role) {
		name(role.getName());
		description(role.getDescription());
		documentation(role.getDocumentation());
		
		RoleImpl impl = (RoleImpl) role;
		List<Privilege> privileges = impl.getPrivileges();
		LinkedHashMap<String, FluentDocumentPrivilege> map = new LinkedHashMap<>();
		for (Privilege privilege : privileges) {
			if (privilege instanceof DocumentPrivilege) {
				DocumentPrivilege document = (DocumentPrivilege) privilege;
				FluentDocumentPrivilege fluent = new FluentDocumentPrivilege().documentName(document.getName()).permission(document.getPermission());
				map.put(document.getName(), fluent);
			}
		}
		for (Privilege privilege : privileges) {
			if (privilege instanceof ActionPrivilege) {
				ActionPrivilege action = (ActionPrivilege) privilege;
				map.get(action.getDocumentName()).addActionPrivilege(action.getName());
			}
		}
		for (ContentPermission content : impl.getContentPermissions()) {
			map.get(content.getDocumentName()).addContentPermission(content.getAttributeName());
		}
		for (ContentRestriction content : impl.getContentRestrictions()) {
			map.get(content.getDocumentName()).addContentRestriction(content.getAttributeName());
		}
		
		map.values().forEach(p -> addPrivilege(p));
		
		return this;
	}
	
	public FluentModuleRole name(String name) {
		role.setName(name);
		return this;
	}
	
	public FluentModuleRole description(String description) {
		role.setDescription(description);
		return this;
	}
	
	public FluentModuleRole documentation(String documentation) {
		role.setDocumentation(documentation);
		return this;
	}

	/**
	 * Adds a new {@link FluentModuleRoleDocumentAggregateAccess} to this module role.
	 */
	public FluentModuleRole addDocumentAggregateAccess(FluentModuleRoleDocumentAggregateAccess access) {
		return addAccess(access);
	}

	/**
	 * Finds the document aggregate access with the specified document name in this module role's list of accesses.
	 */
	public FluentModuleRoleDocumentAggregateAccess findDocumentAggregateAccess(final String documentName) {
		ModuleRoleUserAccessMetaData result = role.getAccesses().stream()
				.filter(a -> a instanceof ModuleRoleDocumentAggregateUserAccessMetaData
						&& ((ModuleRoleDocumentAggregateUserAccessMetaData) a).getDocumentName().equals(documentName))
				.findFirst()
				.orElse(null);

		return result != null ? new FluentModuleRoleDocumentAggregateAccess((ModuleRoleDocumentAggregateUserAccessMetaData) result)
				: null;
	}

	/**
	 * Removes the {@link ModuleRoleDocumentAggregateUserAccessMetaData} with the specified
	 * document name if one is defined for this module role.
	 */
	public FluentModuleRole removeDocumentAggregateAccess(String documentName) {
		role.getAccesses().removeIf(a -> a instanceof ModuleRoleDocumentAggregateUserAccessMetaData
				&& ((ModuleRoleDocumentAggregateUserAccessMetaData) a).getDocumentName().equals(documentName));
		return this;
	}

	/**
	 * Adds a new {@link FluentModuleRoleModelAggregateAccess} to this module role.
	 */
	public FluentModuleRole addModelAggregateAccess(FluentModuleRoleModelAggregateAccess access) {
		return addAccess(access);
	}

	/**
	 * Finds the model aggregate access with the specified document name in this module role's list of accesses.
	 */
	public FluentModuleRoleModelAggregateAccess findModelAggregateAccess(String modelName) {
		ModuleRoleUserAccessMetaData result = role.getAccesses().stream()
				.filter(a -> a instanceof ModuleRoleModelAggregateUserAccessMetaData
						&& ((ModuleRoleModelAggregateUserAccessMetaData) a).getModelName().equals(modelName))
				.findFirst()
				.orElse(null);

		return result != null ? new FluentModuleRoleModelAggregateAccess((ModuleRoleModelAggregateUserAccessMetaData) result)
				: null;
	}

	/**
	 * Removes the {@link ModuleRoleModelAggregateUserAccessMetaData} with the specified
	 * model name if one is defined for this module role.
	 */
	public FluentModuleRole removeModelAggregateAccess(String modelName) {
		role.getAccesses().removeIf(a -> a instanceof ModuleRoleModelAggregateUserAccessMetaData
				&& ((ModuleRoleModelAggregateUserAccessMetaData) a).getModelName().equals(modelName));
		return this;
	}

	/**
	 * Adds a new {@link FluentModuleRolePreviousCompleteAccess} to this module role.
	 */
	public FluentModuleRole addPreviousCompleteAccess(FluentModuleRolePreviousCompleteAccess access) {
		return addAccess(access);
	}

	/**
	 * Finds the previous complete access with the specified document name in this module role's list of accesses.
	 */
	public FluentModuleRolePreviousCompleteAccess findPreviousCompleteAccess(final String documentName) {
		ModuleRoleUserAccessMetaData result = role.getAccesses().stream()
				.filter(a -> a instanceof ModuleRolePreviousCompleteUserAccessMetaData
						&& ((ModuleRolePreviousCompleteUserAccessMetaData) a).getDocumentName().equals(documentName))
				.findFirst()
				.orElse(null);

		return result != null ? new FluentModuleRolePreviousCompleteAccess((ModuleRolePreviousCompleteUserAccessMetaData) result)
				: null;
	}

	/**
	 * Removes the {@link ModuleRolePreviousCompleteUserAccessMetaData} with the specified
	 * document name if one is defined for this module role.
	 */
	public FluentModuleRole removePreviousCompleteAccess(String documentName) {
		role.getAccesses().removeIf(a -> a instanceof ModuleRolePreviousCompleteUserAccessMetaData
				&& ((ModuleRolePreviousCompleteUserAccessMetaData) a).getDocumentName().equals(documentName));
		return this;
	}

	/**
	 * Adds a new {@link FluentModuleRoleQueryAggregateAccess} to this module role.
	 */
	public FluentModuleRole addQueryAggregateAccess(FluentModuleRoleQueryAggregateAccess access) {
		return addAccess(access);
	}

	/**
	 * Finds the query aggregate access with the specified document name in this module role's list of accesses.
	 */
	public FluentModuleRoleQueryAggregateAccess findQueryAggregateAccess(String queryName) {
		ModuleRoleUserAccessMetaData result = role.getAccesses().stream()
				.filter(a -> a instanceof ModuleRoleQueryAggregateUserAccessMetaData
						&& ((ModuleRoleQueryAggregateUserAccessMetaData) a).getQueryName().equals(queryName))
				.findFirst()
				.orElse(null);

		return result != null ? new FluentModuleRoleQueryAggregateAccess((ModuleRoleQueryAggregateUserAccessMetaData) result)
				: null;
	}

	/**
	 * Removes the {@link ModuleRoleQueryAggregateUserAccessMetaData} with the specified
	 * query name if one is defined for this module role.
	 */
	public FluentModuleRole removeQueryAggregateAccess(String queryName) {
		role.getAccesses().removeIf(a -> a instanceof ModuleRoleQueryAggregateUserAccessMetaData
				&& ((ModuleRoleQueryAggregateUserAccessMetaData) a).getQueryName().equals(queryName));
		return this;
	}

	/**
	 * Adds a new {@link FluentModuleRoleSingularAccess} to this module role.
	 */
	public FluentModuleRole addSingularAccess(FluentModuleRoleSingularAccess access) {
		return addAccess(access);
	}

	/**
	 * Finds the singular access with the specified document name in this module role's list of accesses.
	 */
	public FluentModuleRoleSingularAccess findSingularAccess(String documentName) {
		ModuleRoleUserAccessMetaData result = role.getAccesses().stream()
				.filter(a -> a instanceof ModuleRoleSingularUserAccessMetaData
						&& ((ModuleRoleSingularUserAccessMetaData) a).getDocumentName().equals(documentName))
				.findFirst()
				.orElse(null);

		return result != null ? new FluentModuleRoleSingularAccess((ModuleRoleSingularUserAccessMetaData) result) : null;
	}

	/**
	 * Removes the {@link FluentModuleRoleSingularAccess} with the specified document name if
	 * one is defined for this module role.
	 */
	public FluentModuleRole removeSingularAccess(String documentName) {
		role.getAccesses().removeIf(a -> a instanceof ModuleRoleSingularUserAccessMetaData
						&& ((ModuleRoleSingularUserAccessMetaData) a).getDocumentName().equals(documentName));
		return this;
	}

	/**
	 * Clears all the accesses for this module role.
	 */
	public FluentModuleRole clearAccesses() {
		role.getAccesses().clear();
		return this;
	}

	public FluentModuleRole addPrivilege(FluentDocumentPrivilege privilege) {
		role.getPrivileges().add(privilege.get());
		return this;
	}

	public FluentModuleRole removePrivilege(String documentName) {
		role.getPrivileges().removeIf(p -> documentName.equals(p.getDocumentName()));
		return this;
	}

	public FluentModuleRole clearPrivileges() {
		role.getPrivileges().clear();
		return this;
	}

	public FluentDocumentPrivilege findPrivilege(String documentName) {
		DocumentPrivilegeMetaData result = role.getPrivileges().stream().filter(p -> documentName.equals(p.getDocumentName())).findAny().orElse(null);
		if (result != null) {
			return new FluentDocumentPrivilege(result);
		}
		return null;
	}
	
	public ModuleRoleMetaData get() {
		return role;
	}

	/**
	 * Adds a new {@link FluentModuleRoleSingularAccess} to this module role.
	 */
	private <T extends FluentModuleRoleAccess<?, ?>> FluentModuleRole addAccess(T access) {
		role.getAccesses().add(access.get());
		return this;
	}
}
