package modules.whosin.domain;

import jakarta.annotation.Generated;
import org.skyve.metadata.user.ModuleRole;

/**
 * Compile-time references to the roles declared in the whosin module.
 * Generated - local changes will be overwritten.
 */
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public enum WhosinRole implements ModuleRole {
	/** The "Manager" role. */
	MANAGER("Manager"),
	/** The "StaffMember" role. */
	STAFF_MEMBER("StaffMember");

	private final String name;

	private WhosinRole(String name) {
		this.name = name;
	}

	@Override
	public String moduleName() {
		return "whosin";
	}

	@Override
	public String roleName() {
		return name;
	}
}
