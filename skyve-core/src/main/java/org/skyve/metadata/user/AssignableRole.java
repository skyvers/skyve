package org.skyve.metadata.user;

import jakarta.annotation.Nonnull;

/**
 * A compile-time reference to a role that can be assigned to a group or user - the common parent
 * of {@link ModuleRole} (a role declared in a module) and
 * {@link org.skyve.metadata.customer.CustomerRoleReference} (a customer role declared in
 * <code>customer.xml</code>, a named bundle of module roles).
 * <p/>
 * Code that assigns roles - for example when configuring a group - can accept either kind
 * through this interface and store {@link #assignedRoleName()}.
 */
public interface AssignableRole {
	/**
	 * The name a group or user role assignment stores for this role: <code>module.Role</code> for
	 * a module role, the bare name for a customer role.
	 *
	 * @return the stored role name
	 */
	@Nonnull String assignedRoleName();
}
