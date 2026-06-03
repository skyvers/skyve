package modules.test;

import java.awt.Color;
import java.nio.charset.Charset;
import java.util.Objects;

import org.junit.Assert;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.skyve.util.JSON;
import org.skyve.util.Util;

import modules.test.domain.AllAttributesPersistent;

@SuppressWarnings({ "java:S4144", "java:S1130", "java:S1854" })
class UtilTests extends AbstractSkyveTest {

	@Test
	void testPopulateFully() throws Exception {
		AllAttributesPersistent test = Objects.requireNonNull(
				Assertions.assertDoesNotThrow(() -> Util.constructRandomInstance(u, m, aapd, 5)));
		// Save and evict
		test = Objects.requireNonNull(p.save(test));
		p.evictAllCached();

		// Got the shell of the object back
		test = Objects.requireNonNull(p.retrieve(aapd, test.getBizId()));

		Util.populateFully(test);

		JSON.marshall(c, test);

		test = p.save(test);
	}

	@Test
	void testAttributeHasChanged() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);

		test.originalValues().clear();
		Assert.assertFalse("Should not have changed", test.hasChanged());

		test.setText("TEST");

		Assert.assertTrue("Should have changed", test.hasChanged());
	}

	@Test
	void testNestedAttributeHasChanged() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 2);
		test.getAggregatedCollection().clear();
		test.getComposedCollection().clear();

		test.originalValues().clear();
		test.getAggregatedAssociation().originalValues().clear();
		test.getComposedAssociation().originalValues().clear();
		test.getEmbeddedAssociation().originalValues().clear();

		Assert.assertFalse("Should not have changed", test.hasChanged());

		test.getAggregatedAssociation().setText("TEST");

		Assert.assertTrue("Should have changed", test.hasChanged());
	}

	@Test
	void testCollectedAttributeHasChanged() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
		AllAttributesPersistent element = Util.constructRandomInstance(u, m, aapd, 1);
		test.getAggregatedCollection().add(element);

		test = p.save(test);

		Assert.assertFalse("Should not have changed", test.hasChanged());

		test.getAggregatedCollection().get(0).setText("TEST");

		Assert.assertTrue("Should have changed", test.hasChanged());
	}

	@Test
	void testTransientCollectionHasChanged() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
		AllAttributesPersistent element = Util.constructRandomInstance(u, m, aapd, 1);

		test.originalValues().clear();
		element.originalValues().clear();

		Assert.assertFalse("Should not have changed", test.hasChanged());
		Assert.assertFalse("Should not have changed", element.hasChanged());

		test.getAggregatedCollection().add(element);

		Assert.assertTrue("Should have changed", test.hasChanged());
	}

	@Test
	void testPersistentCollectedHasChanged() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
		AllAttributesPersistent element = Util.constructRandomInstance(u, m, aapd, 1);
		test.getAggregatedCollection().add(element);

		test = p.save(test);

		Assert.assertFalse("Should not have changed", test.hasChanged());

		test.getAggregatedCollection().get(0).setText("TEST");

		Assert.assertTrue("Should have changed", test.hasChanged());
	}

	@Test
	@SuppressWarnings("static-method")
	void testUTF8Length() {
		Charset utf8 = Charset.forName("UTF-8");
		AllCodepointsIterator iterator = new AllCodepointsIterator();
		while (iterator.hasNext()) {
			String test = new String(Character.toChars(iterator.next()));
			Assert.assertEquals(test.getBytes(utf8).length, Util.utf8Length(test));
		}
	}

	private static class AllCodepointsIterator {
		private static final int MAX = 0x10FFFF; // see http://unicode.org/glossary/
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
				if (!Character.isDefined(result)) {
					continue;
				}
				return result;
			}

			return MAX;
		}
	}
	
	@Test
	@SuppressWarnings("static-method")
	void testColour() {
		Assert.assertEquals("#000000", Util.htmlColourCode(Color.BLACK));
		Assert.assertEquals("#ffffff", Util.htmlColourCode(Color.WHITE));
		Assert.assertEquals("#ff0000", Util.htmlColourCode(Color.RED));
		Assert.assertEquals("#00ff00", Util.htmlColourCode(Color.GREEN));
		Assert.assertEquals("#0000ff", Util.htmlColourCode(Color.BLUE));

		Assert.assertEquals(Color.BLACK, Util.htmlColour("#000000"));
		Assert.assertEquals(Color.WHITE, Util.htmlColour("#FFFFFF"));
		Assert.assertEquals(Color.RED, Util.htmlColour("#ff0000"));
		Assert.assertEquals(Color.GREEN, Util.htmlColour("#00ff00"));
		Assert.assertEquals(Color.BLUE, Util.htmlColour("#0000ff"));
	}
}
