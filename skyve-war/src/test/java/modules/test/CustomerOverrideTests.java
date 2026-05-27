package modules.test;

import org.junit.Assert;
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
		Assert.assertNotNull(customer);
		Module module = customer.getModule("test");
		Assert.assertNotNull(module.getRole("TestRole"));
		Document document = module.getDocument(customer, AllAttributesPersistent.DOCUMENT_NAME);
		Assert.assertNotNull(document.getCondition("test"));
		View view = document.getView(null, customer, "edit");
		Assert.assertEquals("Test", view.getTitle());
		Assert.assertNotNull(module.getDocument(customer, "Test"));
		Assert.assertNotNull(document.getCondition("test"));
	}

	@Test
	void testCustomerOverridesDoNotExist() {
		Assert.assertNull(m.getRole("TestRole"));
		Document document = m.getDocument(c, AllAttributesPersistent.DOCUMENT_NAME);
		Assert.assertNull(document.getCondition("test"));
		View view = document.getView(null, c, "edit");
		Assert.assertNotEquals("Test", view.getTitle());
		try {
			m.getDocument(c, "Test");
			Assert.fail("Test should not exist");
		}
		catch (@SuppressWarnings("unused") Exception e) {
			// do nothing here
		}
	}
}
