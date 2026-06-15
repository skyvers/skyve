package modules.test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.view.View;

import modules.test.domain.AllAttributesPersistent;

@SuppressWarnings("static-method")
class CustomerOverrideTests extends AbstractSkyveTest {

	@Test
	void testCustomerOverridesExist() {
		Customer customer = CORE.getRepository().getCustomer("test");
		assertNotNull(customer);
		Module module = customer.getModule("test");
		assertNotNull(module.getRole("TestRole"));
		Document document = module.getDocument(customer, AllAttributesPersistent.DOCUMENT_NAME);
		assertNotNull(document.getCondition("test"));
		View view = document.getView(null, customer, "edit");
		assertEquals("Test", view.getTitle());
		assertNotNull(module.getDocument(customer, "Test"));
		assertNotNull(document.getCondition("test"));
	}

	@Test
	void testCustomerOverridesDoNotExist() {
		assertNull(m.getRole("TestRole"));
		Document document = m.getDocument(c, AllAttributesPersistent.DOCUMENT_NAME);
		assertNull(document.getCondition("test"));
		View view = document.getView(null, c, "edit");
		assertNotEquals("Test", view.getTitle());
		try {
			m.getDocument(c, "Test");
			fail("Test should not exist");
		}
		catch (@SuppressWarnings("unused") Exception e) {
			// do nothing here
		}
	}
}
