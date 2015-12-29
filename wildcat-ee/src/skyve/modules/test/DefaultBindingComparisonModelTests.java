package modules.test;

import org.junit.Before;
import org.junit.Test;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.comparison.DefaultBindingComparisonModel;
import org.skyve.util.Util;

import modules.test.domain.AllAttributesPersistent;

public class DefaultBindingComparisonModelTests extends AbstractH2Test {
	private Customer c;
	private User u;
	private Module m;
	private Document d;

	@Before
	public void before() throws Exception {
		u = CORE.getUser();
		c = u.getCustomer();
		m = c.getModule(AllAttributesPersistent.MODULE_NAME);
		d = m.getDocument(c, AllAttributesPersistent.DOCUMENT_NAME);
	}

	@Test
	public void testSimpleGeneration() throws Exception {
		AllAttributesPersistent now = Util.constructRandomInstance(u, m, d, 1);
		AllAttributesPersistent then = Util.constructRandomInstance(u, m, d, 1);
		new DefaultBindingComparisonModel<>(c, d, then, null).getComparisonComposite(now);
	}

	@Test
	public void testNestedGeneration() throws Exception {
		AllAttributesPersistent now = Util.constructRandomInstance(u, m, d, 4);
		AllAttributesPersistent then = Util.constructRandomInstance(u, m, d, 4);
		new DefaultBindingComparisonModel<>(c, d, then, null).getComparisonComposite(now);
	}

	@Test
	public void testAsymmetricNestedGeneration() throws Exception {
		AllAttributesPersistent now = Util.constructRandomInstance(u, m, d, 4);
		AllAttributesPersistent then = Util.constructRandomInstance(u, m, d, 5);
		new DefaultBindingComparisonModel<>(c, d, then, null).getComparisonComposite(now);
	}
}
