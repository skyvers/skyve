package modules.test;

import org.junit.Assert;
import org.junit.Test;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.view.View;

import modules.test.domain.AllAttributesPersistent;

public class CustomerOverrideTests extends AbstractSkyveTest {
	@Test
	@SuppressWarnings("static-method")
	public void testCustomerOverridesExist() throws Exception {
		Customer customer = CORE.getRepository().getCustomer("test");
		Module module = customer.getModule("test");
		Assert.assertNotNull(module.getRole("TestRole"));
		Document document = module.getDocument(customer, AllAttributesPersistent.DOCUMENT_NAME);
		Assert.assertNotNull(document.getCondition("test"));
		View view = document.getView(null, customer, "edit");
		Assert.assertEquals("Test", view.getTitle());
		document = module.getDocument(customer, "Test");
		Assert.assertNotNull(document);
		Assert.assertNotNull(document.getCondition("test"));
	}

	@Test
	public void testCustomerOverridesDoNotExist() throws Exception {
		Assert.assertNull(m.getRole("TestRole"));
		Document document = m.getDocument(c, AllAttributesPersistent.DOCUMENT_NAME);
		Assert.assertNull(document.getCondition("test"));
		View view = document.getView(null, c, "edit");
		Assert.assertNotEquals("Test", view.getTitle());
		try {
			document = m.getDocument(c, "Test");
			Assert.fail("Test should not exist");
		}
		catch (@SuppressWarnings("unused") Exception e) {
			// do nothing here
		}
	}
}
