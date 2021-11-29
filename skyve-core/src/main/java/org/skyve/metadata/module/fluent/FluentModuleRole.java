package org.skyve.metadata.module.fluent;

import java.util.LinkedHashMap;
import java.util.List;

import org.skyve.impl.metadata.repository.module.ContentPermission;
import org.skyve.impl.metadata.repository.module.ContentRestriction;
import org.skyve.impl.metadata.repository.module.ModuleRoleMetaData;
import org.skyve.impl.metadata.user.ActionPrivilege;
import org.skyve.impl.metadata.user.DocumentPrivilege;
import org.skyve.impl.metadata.user.Privilege;
import org.skyve.impl.metadata.user.RoleImpl;
import org.skyve.metadata.user.Role;

public class FluentModuleRole {
	private ModuleRoleMetaData role = new ModuleRoleMetaData();
	
	public FluentModuleRole() {
		// nothing to see
	}
	
	public FluentModuleRole(Role role) {
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
	
	public FluentModuleRole addPrivilege(FluentDocumentPrivilege privilege) {
		role.getPrivileges().add(privilege.get());
		return this;
	}

	public ModuleRoleMetaData get() {
		return role;
	}
}
