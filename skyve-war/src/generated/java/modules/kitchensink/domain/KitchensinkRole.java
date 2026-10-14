package modules.kitchensink.domain;

import jakarta.annotation.Generated;
import org.skyve.metadata.user.ModuleRole;

/**
 * Compile-time references to the roles declared in the kitchensink module.
 * Generated - local changes will be overwritten.
 */
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public enum KitchensinkRole implements ModuleRole {
	/** The "dev" role. */
	DEV("dev");

	private final String name;

	private KitchensinkRole(String name) {
		this.name = name;
	}

	@Override
	public String moduleName() {
		return "kitchensink";
	}

	@Override
	public String roleName() {
		return name;
	}
}
