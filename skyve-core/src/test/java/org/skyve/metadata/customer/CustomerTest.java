package org.skyve.metadata.customer;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.withSettings;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.module.ModuleMemberTestSupport.TestDocument;

/**
 * Tests that the typed-reference default methods of {@link Customer} delegate to the String
 * forms with the reference's module name and document name.
 */
@SuppressWarnings("static-method")
class CustomerTest {
	/** Lets Mockito's subclass mock maker call the interface's default methods for real. */
	private abstract static class CustomerDouble implements Customer {
		private static final long serialVersionUID = 1L;
	}

	@Test
	void typedGetConstantDomainValuesDelegatesToTheStringForm() throws Exception {
		Customer customer = mock(CustomerDouble.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		Bizlet<?> bizlet = mock(Bizlet.class);
		Attribute attribute = mock(Attribute.class);
		List<DomainValue> values = List.of(new DomainValue("a"));
		doReturn(values).when(customer).getConstantDomainValues(bizlet, "admin", "Contact", attribute);

		assertSame(values, customer.getConstantDomainValues(bizlet, TestDocument.CONTACT, attribute));
	}
}
