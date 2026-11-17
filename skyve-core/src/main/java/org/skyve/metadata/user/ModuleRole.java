package org.skyve.metadata.user;

import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.ModuleMember;

import jakarta.annotation.Nonnull;

/**
 * A compile-time reference to a role declared in a module - implemented by the generated
 * per-module role enums (e.g. <code>AdminRole.AUDIT_MANAGER</code>) so Skyve APIs can accept
 * a single self-describing token in place of a module name and role name String pair.
 * <p/>
 * See {@link ModuleMember} for when the String form is still required.
 */
public interface ModuleRole extends ModuleMember {
	/**
	 * The role name within the module.
	 *
	 * @return the role name
	 */
	@Nonnull String roleName();

	/**
	 * Resolves this reference to the role metadata through the current user's customer, so
	 * per-customer module overrides are honoured.
	 *
	 * @return the role metadata
	 * @throws org.skyve.metadata.MetaDataException if the module does not declare the role for
	 * 				the customer
	 */
	default @Nonnull Role toRole() {
		return toRole(CORE.getCustomer());
	}

	/**
	 * Resolves this reference to the role metadata through the given customer, so per-customer
	 * module overrides are honoured.
	 *
	 * @param customer	the customer to resolve the role for
	 * @return the role metadata
	 * @throws org.skyve.metadata.MetaDataException if the module does not declare the role for
	 * 				the customer
	 */
	default @Nonnull Role toRole(@Nonnull Customer customer) {
		return toModule(customer).getNullSafeRole(roleName());
	}
}
