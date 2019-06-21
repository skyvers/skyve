package modules.admin.User;

import java.util.ArrayList;
import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.user.Role;

import modules.admin.domain.User;
import modules.admin.domain.UserRole;

public class UserExtension extends User {
	private static final long serialVersionUID = 3422968996147520436L;

	private List<Bean> assignedRoles = null;

	public List<Bean> getAssignedRoles() {
		if (assignedRoles == null) {
			assignedRoles = new ArrayList<>(10);
			if (isPersisted()) {
				Repository r = CORE.getRepository();
				org.skyve.metadata.user.User metaDataUser = r.retrieveUser((UtilImpl.CUSTOMER == null) ? getBizCustomer() + "/" + getUserName() : getUserName());
				if (metaDataUser != null) {
					Customer c = metaDataUser.getCustomer();
					for (String moduleName : metaDataUser.getAccessibleModuleNames()) {
						Module m = c.getModule(moduleName);
						for (Role role : m.getRoles()) {
							String roleName = role.getName();
							if (metaDataUser.isInRole(moduleName, roleName)) {
								UserRole assignedRole = UserRole.newInstance();
								assignedRole.setRoleName(moduleName + "." + roleName);
								assignedRoles.add(assignedRole);
							}
						}
					}
				}
			}
		}
		
		return assignedRoles;
	}
		
	void clearAssignedRoles() {
		assignedRoles = null;
	}
}
