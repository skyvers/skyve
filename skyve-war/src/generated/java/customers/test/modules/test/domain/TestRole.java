package customers.test.modules.test.domain;

import jakarta.annotation.Generated;
import org.skyve.metadata.user.ModuleRole;

/**
 * Compile-time references to the roles declared in the test module for customer test.
 * Generated - local changes will be overwritten.
 */
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public enum TestRole implements ModuleRole {
	/** The "TestRole" role. */
	TEST_ROLE("TestRole");

	private final String name;

	private TestRole(String name) {
		this.name = name;
	}

	@Override
	public String moduleName() {
		return "test";
	}

	@Override
	public String roleName() {
		return name;
	}
}
