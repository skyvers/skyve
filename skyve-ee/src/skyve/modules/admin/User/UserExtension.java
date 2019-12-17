package modules.admin.User;

import java.util.List;

import org.skyve.CORE;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.util.SQLMetaDataUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.Role;

import modules.admin.domain.User;
import modules.admin.domain.UserRole;

public class UserExtension extends User {
	private static final long serialVersionUID = 3422968996147520436L;

	private boolean determinedRoles = false;

	@Override
	public List<UserRole> getAssignedRoles() {
		List<UserRole> assignedRoles = super.getAssignedRoles();
		if (! determinedRoles) {
			determinedRoles = true;
			assignedRoles.clear();
			if (isPersisted()) {
				try {
					org.skyve.metadata.user.User metaDataUser = toMetaDataUser();
	
					// Add the assigned roles
					Customer c = metaDataUser.getCustomer();
					for (Module m : c.getModules()) {
						String moduleName = m.getName();
						for (Role role : m.getRoles()) {
							String roleName = role.getName();
							if (metaDataUser.isInRole(moduleName, roleName)) {
								UserRole assignedRole = UserRole.newInstance();
								assignedRole.setRoleName(moduleName + "." + roleName);
								assignedRole.originalValues().clear();
								assignedRoles.add(assignedRole);
							}
						}
					}
				}
				catch (@SuppressWarnings("unused") Exception e) {
					// assigned roles is already empty
				}
			}
		}
		
		return assignedRoles;
	}
		
	void clearAssignedRoles() {
		determinedRoles = false;
	}
	 
	/**
	 * Return the metadata user that is this user
	 * 
	 * @return the metadata user that is this user
	 */
	public org.skyve.metadata.user.User toMetaDataUser() {
		
		UserImpl metaDataUser  = null;
		if(isPersisted()) {
			// Populate the user using the persistence connection since it might have just been inserted and not committed yet
			metaDataUser = AbstractRepository.setCustomerAndUserFromPrincipal((UtilImpl.CUSTOMER == null) ? getBizCustomer() + "/" + getUserName() : getUserName());
			metaDataUser.clearAllPermissionsAndMenus();
			SQLMetaDataUtil.populateUser(metaDataUser, ((AbstractHibernatePersistence) CORE.getPersistence()).getConnection());
		}
		
		return metaDataUser;
	}

}
