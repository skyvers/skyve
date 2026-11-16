package util.sail;

import java.util.Collection;

import org.skyve.impl.metadata.repository.customer.CustomerRoleMetaData;
import org.skyve.impl.metadata.user.RoleImpl;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.customer.CustomerRole;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.Role;
import org.skyve.metadata.user.User;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Users for SAIL test harnesses.
 * <p>
 * The interpreted executors rebuild views in the test JVM to locate widgets, so the test JVM must run as a user with the
 * same permissions as the user the browser signs in as. Otherwise actions that the browser's user cannot see
 * (such as Save on a document they cannot create) are in the rebuilt view, and widgets are located wrongly.
 *
 * @see SailViewRoles
 */
public final class SailUsers {
	private SailUsers() {
		// static only
	}

	/**
	 * Creates a user of the test user's customer with every role of every module, as Skyve's bootstrap user has
	 * (see {@code EXT.bootstrap}).
	 *
	 * @param testUser	the test's user, whose customer, name, id and contact are copied
	 * @return the user
	 */
	public static @Nonnull UserImpl bootstrapLikeUser(@Nonnull User testUser) {
		UserImpl result = copyOf(testUser);
		Customer customer = testUser.getCustomer();
		for (Module module : customer.getModules()) {
			for (Role role : module.getRoles()) {
				result.addRole((RoleImpl) role);
			}
		}
		return result;
	}

	/**
	 * Creates a user of the test user's customer with exactly the named roles, as a user the browser signs in as
	 * would have them.
	 * <p>
	 * A role name is either {@code module.Role}, a role of a module, or a name with no dot, a customer role, which
	 * gives the user each module role of the customer role. This is how Skyve reads a user's roles from the database.
	 *
	 * @param testUser	the test's user, whose customer, name, id and contact are copied
	 * @param roleNames	the role names
	 * @return the user
	 * @throws IllegalArgumentException	if a name is not a role of the test user's customer
	 */
	public static @Nonnull UserImpl userWithRoles(@Nonnull User testUser, @Nonnull Collection<String> roleNames) {
		UserImpl result = copyOf(testUser);
		Customer customer = testUser.getCustomer();
		for (String roleName : roleNames) {
			int dot = roleName.indexOf('.');
			if (dot > 0) {
				String moduleName = roleName.substring(0, dot);
				Module module = module(customer, moduleName);
				if (module == null) {
					throw new IllegalArgumentException(String.format("Role %s: customer %s has no module %s",
																		roleName, customer.getName(), moduleName));
				}
				Role role = module.getRole(roleName.substring(dot + 1));
				if (role == null) {
					throw new IllegalArgumentException(String.format("Role %s: module %s has no such role", roleName, moduleName));
				}
				result.addRole((RoleImpl) role);
			}
			else {
				CustomerRole customerRole = customer.getRole(roleName);
				if (customerRole == null) {
					throw new IllegalArgumentException(String.format("Role %s: customer %s has no such customer role " +
																		"(name a module role as module.Role)",
																		roleName, customer.getName()));
				}
				for (Role role : ((CustomerRoleMetaData) customerRole).getModuleRoles(customer)) {
					result.addRole((RoleImpl) role);
				}
			}
		}
		return result;
	}

	/**
	 * Returns the customer's module with the name.
	 *
	 * @return the module, or null if the customer has no such module
	 */
	private static @Nullable Module module(@Nonnull Customer customer, @Nonnull String moduleName) {
		for (Module module : customer.getModules()) {
			if (moduleName.equals(module.getName())) {
				return module;
			}
		}
		return null;
	}

	/**
	 * Creates a user with no roles that has the test user's customer, name, id, contact, data group, home module
	 * and language.
	 */
	private static @Nonnull UserImpl copyOf(@Nonnull User testUser) {
		UserImpl result = new UserImpl();
		result.setCustomerName(testUser.getCustomerName());
		result.setName(testUser.getName());
		result.setId(testUser.getId());
		result.setContactId(testUser.getContactId());
		result.setContactName(testUser.getContactName());
		result.setDataGroupId(testUser.getDataGroupId());
		result.setHomeModuleName(testUser.getHomeModuleName());
		result.setLanguageTag(testUser.getLanguageTag());
		return result;
	}
}
