package org.skyve.metadata.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;

@SuppressWarnings("static-method")
class ProvidedRepositoryObserverTest {
	private ProvidedRepository previousRepository;
	private String previousCustomer;

	@BeforeEach
	void setUp() {
		previousRepository = ProvidedRepositoryFactory.get();
		previousCustomer = UtilImpl.CUSTOMER;
	}

	@AfterEach
	void tearDown() {
		UtilImpl.CUSTOMER = previousCustomer;
		if (previousRepository == null) {
			ProvidedRepositoryFactory.clear();
		}
		else {
			ProvidedRepositoryFactory.set(previousRepository);
		}
	}

	@Test
	void notifyAllCustomersObserversNotifiesOnlyConfiguredCustomer() {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Customer customer = mock(Customer.class);
		ProvidedRepositoryFactory.set(repository);
		UtilImpl.CUSTOMER = "alpha";
		when(repository.getCustomer("alpha")).thenReturn(customer);
		List<Customer> notifiedCustomers = new ArrayList<>();

		ProvidedRepository.notifyAllCustomersObservers(notifiedCustomers::add);

		assertEquals(List.of(customer), notifiedCustomers);
		verify(repository, never()).getAllCustomerNames();
	}

	@Test
	void notifyAllCustomersObserversNotifiesAllCustomersInRepositoryOrder() {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Customer alpha = mock(Customer.class);
		Customer beta = mock(Customer.class);
		ProvidedRepositoryFactory.set(repository);
		UtilImpl.CUSTOMER = null;
		when(repository.getAllCustomerNames()).thenReturn(List.of("alpha", "beta"));
		when(repository.getCustomer("alpha")).thenReturn(alpha);
		when(repository.getCustomer("beta")).thenReturn(beta);
		List<Customer> notifiedCustomers = new ArrayList<>();

		ProvidedRepository.notifyAllCustomersObservers(notifiedCustomers::add);

		assertEquals(List.of(alpha, beta), notifiedCustomers);
	}

	@Test
	void notifyAllCustomersObserversRejectsMissingConfiguredCustomer() {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		ProvidedRepositoryFactory.set(repository);
		UtilImpl.CUSTOMER = "missing";

		IllegalStateException exception = assertThrows(IllegalStateException.class,
				() -> ProvidedRepository.notifyAllCustomersObservers(customer -> {
					throw new AssertionError("Notifier should not be called for a missing customer");
				}));

		assertEquals("UtilImpl.CUSTOMER missing does not exist.", exception.getMessage());
	}
}
