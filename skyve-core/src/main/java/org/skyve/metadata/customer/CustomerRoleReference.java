package org.skyve.metadata.customer;

import org.skyve.CORE;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.user.AssignableRole;

import jakarta.annotation.Nonnull;

/**
 * A compile-time reference to a customer role declared in <code>customer.xml</code> - implemented
 * by the generated per-customer role enums (e.g. <code>TestCustomerRole.TEST_MANAGER</code>) so code
 * that assigns roles can name a customer role without a customer name and role name String pair.
 * <p/>
 * A customer role is a named bundle of module roles. A group or user role assignment stores its
 * bare name, which Skyve expands into the bundled module roles when the user signs in. That is why
 * {@link org.skyve.metadata.user.User#isInRole(String, String)} and its typed overloads check module
 * roles only: use this reference for assigning roles, and a {@link org.skyve.metadata.user.ModuleRole}
 * for checking them.
 * <p/>
 * Every Skyve API that accepts one also accepts the role name as a String, which remains the
 * primitive form for roles defined at run time and names held as data.
 */
public interface CustomerRoleReference extends AssignableRole {
	/**
	 * The name of the customer that declares the role.
	 *
	 * @return the customer name
	 */
	@Nonnull String customerName();

	/**
	 * The customer role name.
	 *
	 * @return the role name
	 */
	@Nonnull String roleName();

	/**
	 * The name a group or user role assignment stores for this customer role.
	 *
	 * @return the bare {@link #roleName()}
	 */
	@Override
	default @Nonnull String assignedRoleName() {
		return roleName();
	}

	/**
	 * Resolves the declaring customer through the repository.
	 *
	 * @return the customer metadata
	 * @throws MetaDataException if the repository has no such customer
	 */
	default @Nonnull Customer toCustomer() {
		String customerName = customerName();
		Customer result = CORE.getRepository().getCustomer(customerName);
		if (result == null) {
			throw new MetaDataException("Customer " + customerName + " does not exist");
		}
		return result;
	}

	/**
	 * Resolves this reference to the customer role metadata.
	 *
	 * @return the customer role metadata
	 * @throws MetaDataException if the customer does not exist or does not declare the role
	 */
	default @Nonnull CustomerRole toRole() {
		String roleName = roleName();
		CustomerRole result = toCustomer().getRole(roleName);
		if (result == null) {
			throw new MetaDataException("Customer role " + roleName + " does not exist for customer " + customerName());
		}
		return result;
	}
}
