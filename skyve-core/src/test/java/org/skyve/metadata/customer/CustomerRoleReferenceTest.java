package org.skyve.metadata.customer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.repository.ProvidedRepository;

/**
 * Tests for the default methods of {@link CustomerRoleReference}.
 */
@SuppressWarnings("static-method")
class CustomerRoleReferenceTest {
	/** A compile-time customer role reference as the generated per-customer role enums implement it. */
	private enum AcmeCustomerRole implements CustomerRoleReference {
		MANAGER("Manager"),
		MISSING("Missing");

		private final String roleName;

		private AcmeCustomerRole(String roleName) {
			this.roleName = roleName;
		}

		@Override
		public String customerName() {
			return "acme";
		}

		@Override
		public String roleName() {
			return roleName;
		}
	}

	/** The same role declared for a customer the repository does not have. */
	private enum GhostCustomerRole implements CustomerRoleReference {
		MANAGER;

		@Override
		public String customerName() {
			return "ghost";
		}

		@Override
		public String roleName() {
			return "Manager";
		}
	}

	private ProvidedRepository previousRepository;
	private ProvidedRepository repository;

	@BeforeEach
	void installRepository() {
		previousRepository = ProvidedRepositoryFactory.get();
		repository = mock(ProvidedRepository.class);
		ProvidedRepositoryFactory.set(repository);
	}

	@AfterEach
	void restoreRepository() {
		ProvidedRepositoryFactory.set(previousRepository);
	}

	@Test
	void assignedRoleNameIsTheBareRoleName() {
		assertEquals("Manager", AcmeCustomerRole.MANAGER.assignedRoleName());
	}

	@Test
	void toCustomerResolvesTheCustomerThroughTheRepository() {
		Customer customer = mock(Customer.class);
		when(repository.getCustomer("acme")).thenReturn(customer);

		assertSame(customer, AcmeCustomerRole.MANAGER.toCustomer());
	}

	@Test
	void toCustomerThrowsWhenTheCustomerDoesNotExist() {
		MetaDataException e = assertThrows(MetaDataException.class, GhostCustomerRole.MANAGER::toCustomer);
		assertEquals("Customer ghost does not exist", e.getMessage());
	}

	@Test
	void toRoleResolvesTheCustomerRole() {
		Customer customer = mock(Customer.class);
		CustomerRole role = mock(CustomerRole.class);
		when(customer.getRole("Manager")).thenReturn(role);
		when(repository.getCustomer("acme")).thenReturn(customer);

		assertSame(role, AcmeCustomerRole.MANAGER.toRole());
	}

	@Test
	void toRoleThrowsWhenTheCustomerDoesNotDeclareTheRole() {
		when(repository.getCustomer("acme")).thenReturn(mock(Customer.class));

		MetaDataException e = assertThrows(MetaDataException.class, AcmeCustomerRole.MISSING::toRole);
		assertEquals("Customer role Missing does not exist for customer acme", e.getMessage());
	}

	@Test
	void toRoleThrowsWhenTheCustomerDoesNotExist() {
		assertThrows(MetaDataException.class, GhostCustomerRole.MANAGER::toRole);
	}
}
