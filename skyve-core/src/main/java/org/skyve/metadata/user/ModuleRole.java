package org.skyve.metadata.user;

import org.skyve.CORE;

/**
 * A compile-time reference to a role declared in a module - implemented by the generated
 * per-module role enums (e.g. <code>AdminRole.AUDIT_MANAGER</code>) so Skyve APIs can accept
 * a single self-describing token in place of a module name and role name String pair.
 */
public interface ModuleRole {
	/**
	 * The name of the module that declares the role.
	 *
	 * @return the module name; never {@code null}
	 */
	String moduleName();

	/**
	 * The role name within the module.
	 *
	 * @return the role name; never {@code null}
	 */
	String roleName();

	/**
	 * Resolves this reference to the role metadata through the current customer,
	 * so per-customer module overrides are honoured.
	 *
	 * @return the role metadata; never {@code null}
	 */
	default Role toRole() {
		return CORE.getCustomer().getModule(moduleName()).getRole(roleName());
	}
}
