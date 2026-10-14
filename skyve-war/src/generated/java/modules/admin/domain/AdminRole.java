package modules.admin.domain;

import jakarta.annotation.Generated;
import org.skyve.metadata.user.ModuleRole;

/**
 * Compile-time references to the roles declared in the admin module.
 * Generated - local changes will be overwritten.
 */
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public enum AdminRole implements ModuleRole {
	/** The "Anonymous" role. */
	ANONYMOUS("Anonymous"),
	/** The "AppUser" role. */
	APP_USER("AppUser"),
	/** The "AuditManager" role. */
	AUDIT_MANAGER("AuditManager"),
	/** The "BasicUser" role. */
	BASIC_USER("BasicUser"),
	/** The "ContactManager" role. */
	CONTACT_MANAGER("ContactManager"),
	/** The "ContactViewer" role. */
	CONTACT_VIEWER("ContactViewer"),
	/** The "DevOps" role. */
	DEV_OPS("DevOps"),
	/** The "JobMaintainer" role. */
	JOB_MAINTAINER("JobMaintainer"),
	/** The "SecurityAdministrator" role. */
	SECURITY_ADMINISTRATOR("SecurityAdministrator"),
	/** The "ViewUser" role. */
	VIEW_USER("ViewUser");

	private final String name;

	private AdminRole(String name) {
		this.name = name;
	}

	@Override
	public String moduleName() {
		return "admin";
	}

	@Override
	public String roleName() {
		return name;
	}
}
