package modules.test;

import org.junit.Before;
import org.junit.Test;
import org.skyve.CORE;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.util.BeanValidator;
import org.skyve.util.Util;

import modules.test.domain.MappedExtension;

public class BeanValidatorTests extends AbstractH2Test {
	private Persistence p;
	private User u;
	private Customer c;
	private Module m;
	private Document d;

	@Before
	public void before() throws Exception {
		p = CORE.getPersistence();
		u = p.getUser();
		c = u.getCustomer();
		m = c.getModule(MappedExtension.MODULE_NAME);
		d = m.getDocument(c, MappedExtension.DOCUMENT_NAME);
	}

	@Test
	public void testValidateBeanAgainstDocument() throws Exception {
		MappedExtension test = Util.constructRandomInstance(u, m, d, 2);
		BeanValidator.validateBeanAgainstDocument(d, test);
	}
	
	@Test(expected = ValidationException.class)
	public void testValidateBeanAgainstBaseDocument() throws Exception {
		MappedExtension test = Util.constructRandomInstance(u, m, d, 2);
		test.setColour(null);
		BeanValidator.validateBeanAgainstDocument(d, test);
	}
}
