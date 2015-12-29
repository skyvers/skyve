package modules.test;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.util.Util;
import org.skyve.wildcat.util.JSONUtil;

import modules.test.domain.AllAttributesPersistent;

public class UtilTests extends AbstractH2Test {
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
		m = c.getModule(AllAttributesPersistent.MODULE_NAME);
		d = m.getDocument(c, AllAttributesPersistent.DOCUMENT_NAME);
	}

	@Test
	public void testPopulateFully() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, d, 5);
		
		// Save and evict
		test = p.save(test);
		p.evictAllCached();

		// Got the shell of the object back
		test = p.retrieve(d, test.getBizId(), false);

		Util.populateFully(test);
		
		System.out.println("PF = " + JSONUtil.marshall(c, test, null));

		test = p.save(test);
	}
	
	@Test
	public void testAttributeHasChanged() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, d, 1);
		
		test.originalValues().clear();
		Assert.assertFalse("Should not have changed", Util.hasChanged(test));

		test.setText("TEST");
		
		Assert.assertTrue("Should have changed", Util.hasChanged(test));
	}
	
	@Test
	public void testNestedAttributeHasChanged() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, d, 2);
		test.getAggregatedCollection().clear();
		
		test.originalValues().clear();
		test.getAggregatedAssociation().originalValues().clear();
		
		Assert.assertFalse("Should not have changed", Util.hasChanged(test));

		test.getAggregatedAssociation().setText("TEST");
		
		Assert.assertTrue("Should have changed", Util.hasChanged(test));
	}

	@Test
	public void testCollectedAttributeHasChanged() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, d, 1);
		AllAttributesPersistent element = Util.constructRandomInstance(u, m, d, 1);
		test.getAggregatedCollection().add(element);

		test = p.save(test);
		
		Assert.assertFalse("Should not have changed", Util.hasChanged(test));

		test.getAggregatedCollection().get(0).setText("TEST");
		
		Assert.assertTrue("Should have changed", Util.hasChanged(test));
	}
	
	@Test
	public void testTransientCollectionHasChanged() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, d, 1);
		AllAttributesPersistent element = Util.constructRandomInstance(u, m, d, 1);
		
		test.originalValues().clear();
		element.originalValues().clear();
		
		Assert.assertFalse("Should not have changed", Util.hasChanged(test));
		Assert.assertFalse("Should not have changed", Util.hasChanged(element));
		
		test.getAggregatedCollection().add(element);

		Assert.assertTrue("Should have changed", Util.hasChanged(test));
	}

	@Test
	public void testPersistentCollectedHasChanged() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, d, 1);
		AllAttributesPersistent element = Util.constructRandomInstance(u, m, d, 1);
		test.getAggregatedCollection().add(element);

		test = p.save(test);
		
		Assert.assertFalse("Should not have changed", Util.hasChanged(test));

		test.getAggregatedCollection().get(0).setText("TEST");
		
		Assert.assertTrue("Should have changed", Util.hasChanged(test));
	}
}
