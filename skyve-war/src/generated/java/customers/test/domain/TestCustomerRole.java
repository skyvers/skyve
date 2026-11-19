package customers.test.domain;

import jakarta.annotation.Generated;
import org.skyve.metadata.customer.CustomerRoleReference;

/**
 * Compile-time references to the customer roles declared for customer test.
 * Generated - local changes will be overwritten.
 */
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public enum TestCustomerRole implements CustomerRoleReference {
	/** The "TestManager" customer role. */
	TEST_MANAGER("TestManager");

	private final String name;

	private TestCustomerRole(String name) {
		this.name = name;
	}

	@Override
	public String customerName() {
		return "test";
	}

	@Override
	public String roleName() {
		return name;
	}
}
