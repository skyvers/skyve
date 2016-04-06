package modules.test;

import java.nio.charset.Charset;

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
	
	@Test
	@SuppressWarnings({ "static-method", "synthetic-access" })
	public void testUTF8Length() {
		Charset utf8 = Charset.forName("UTF-8");
		AllCodepointsIterator iterator = new AllCodepointsIterator();
		while (iterator.hasNext()) {
			String test = new String(Character.toChars(iterator.next()));
			Assert.assertEquals(test.getBytes(utf8).length, Util.UTF8Length(test));
		}
	}

	private static class AllCodepointsIterator {
		private static final int MAX = 0x10FFFF; //see http://unicode.org/glossary/
		private static final int SURROGATE_FIRST = 0xD800;
		private static final int SURROGATE_LAST = 0xDFFF;
		private int codepoint = 0;
		public boolean hasNext() {
			return codepoint < MAX;
		}
		public int next() {
			int ret = codepoint;
			codepoint = next(codepoint);
			return ret;
		}
		
		private static int next(int codepoint) {
			int result = codepoint;
			while (result++ < MAX) {
				if (result == SURROGATE_FIRST) {
					result = SURROGATE_LAST + 1;
				}
				if (! Character.isDefined(result)) {
					continue;
				}
				return result;
			}
			
			return MAX;
		}
	}
}
