package org.skyve.metadata.repository;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.withSettings;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.ModuleMemberTestSupport.TestDocument;

/**
 * Tests that the typed-reference default methods of {@link Repository} delegate to the String
 * forms with the reference's module name and document name.
 */
@SuppressWarnings("static-method")
class RepositoryTest {
	/** Lets Mockito's subclass mock maker call the interface's default methods for real. */
	private abstract static class RepositoryDouble implements Repository {
		// no state
	}

	@Test
	void typedGetDataFactoryDelegatesToTheStringForm() {
		Repository r = mock(RepositoryDouble.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		Customer customer = mock(Customer.class);
		Object factory = new Object();
		doReturn(factory).when(r).getDataFactory(customer, "admin", "Contact");

		assertSame(factory, r.getDataFactory(customer, TestDocument.CONTACT));
	}
}
