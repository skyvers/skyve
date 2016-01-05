package modules.test;

import modules.test.domain.AllAttributesPersistent;

import org.junit.Assert;
import org.junit.Test;
import org.skyve.util.Util;
import org.skyve.wildcat.util.JSONUtil;

public class UtilTests extends AbstractH2Test {
	@Test
	public void testPopulateFully() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 5);
		
		// Save and evict
		test = p.save(test);
		p.evictAllCached();

		// Got the shell of the object back
		test = p.retrieve(aapd, test.getBizId(), false);

		Util.populateFully(test);
		
		System.out.println("PF = " + JSONUtil.marshall(c, test, null));

		test = p.save(test);
	}
	
	@Test
	public void testAttributeHasChanged() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
		
		test.originalValues().clear();
		Assert.assertFalse("Should not have changed", Util.hasChanged(test));

		test.setText("TEST");
		
		Assert.assertTrue("Should have changed", Util.hasChanged(test));
	}
	
	@Test
	public void testNestedAttributeHasChanged() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 2);
		test.getAggregatedCollection().clear();
		
		test.originalValues().clear();
		test.getAggregatedAssociation().originalValues().clear();
		
		Assert.assertFalse("Should not have changed", Util.hasChanged(test));

		test.getAggregatedAssociation().setText("TEST");
		
		Assert.assertTrue("Should have changed", Util.hasChanged(test));
	}

	@Test
	public void testCollectedAttributeHasChanged() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
		AllAttributesPersistent element = Util.constructRandomInstance(u, m, aapd, 1);
		test.getAggregatedCollection().add(element);

		test = p.save(test);
		
		Assert.assertFalse("Should not have changed", Util.hasChanged(test));

		test.getAggregatedCollection().get(0).setText("TEST");
		
		Assert.assertTrue("Should have changed", Util.hasChanged(test));
	}
	
	@Test
	public void testTransientCollectionHasChanged() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
		AllAttributesPersistent element = Util.constructRandomInstance(u, m, aapd, 1);
		
		test.originalValues().clear();
		element.originalValues().clear();
		
		Assert.assertFalse("Should not have changed", Util.hasChanged(test));
		Assert.assertFalse("Should not have changed", Util.hasChanged(element));
		
		test.getAggregatedCollection().add(element);

		Assert.assertTrue("Should have changed", Util.hasChanged(test));
	}

	@Test
	public void testPersistentCollectedHasChanged() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
		AllAttributesPersistent element = Util.constructRandomInstance(u, m, aapd, 1);
		test.getAggregatedCollection().add(element);

		test = p.save(test);
		
		Assert.assertFalse("Should not have changed", Util.hasChanged(test));

		test.getAggregatedCollection().get(0).setText("TEST");
		
		Assert.assertTrue("Should have changed", Util.hasChanged(test));
	}
}
