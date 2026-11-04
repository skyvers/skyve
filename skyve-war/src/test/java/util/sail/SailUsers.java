package util.sail;

import org.skyve.impl.metadata.user.RoleImpl;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.Role;
import org.skyve.metadata.user.User;

import jakarta.annotation.Nonnull;

/**
 * Users for SAIL test harnesses.
 * <p>
 * The interpreted executors rebuild views in the test JVM to locate widgets, so the test JVM must run as a user with the
 * same permissions as the user the browser signs in as. Otherwise actions that the browser's user cannot see
 * (such as Save on a document they cannot create) are in the rebuilt view, and widgets are located wrongly.
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
		UserImpl result = new UserImpl();
		result.setCustomerName(testUser.getCustomerName());
		result.setName(testUser.getName());
		result.setId(testUser.getId());
		result.setContactId(testUser.getContactId());
		result.setContactName(testUser.getContactName());
		result.setDataGroupId(testUser.getDataGroupId());
		result.setHomeModuleName(testUser.getHomeModuleName());
		result.setLanguageTag(testUser.getLanguageTag());
		Customer customer = testUser.getCustomer();
		for (Module module : customer.getModules()) {
			for (Role role : module.getRoles()) {
				result.addRole((RoleImpl) role);
			}
		}
		return result;
	}
}
