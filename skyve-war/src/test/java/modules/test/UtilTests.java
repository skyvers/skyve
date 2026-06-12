package modules.test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.awt.Color;
import java.nio.charset.Charset;
import java.util.Objects;

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
		assertFalse(test.hasChanged(), "Should not have changed");

		test.setText("TEST");

		assertTrue(test.hasChanged(), "Should have changed");
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

		assertFalse(test.hasChanged(), "Should not have changed");

		test.getAggregatedAssociation().setText("TEST");

		assertTrue(test.hasChanged(), "Should have changed");
	}

	@Test
	void testCollectedAttributeHasChanged() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
		AllAttributesPersistent element = Util.constructRandomInstance(u, m, aapd, 1);
		test.getAggregatedCollection().add(element);

		test = p.save(test);

		assertFalse(test.hasChanged(), "Should not have changed");

		test.getAggregatedCollection().get(0).setText("TEST");

		assertTrue(test.hasChanged(), "Should have changed");
	}

	@Test
	void testTransientCollectionHasChanged() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
		AllAttributesPersistent element = Util.constructRandomInstance(u, m, aapd, 1);

		test.originalValues().clear();
		element.originalValues().clear();

		assertFalse(test.hasChanged(), "Should not have changed");
		assertFalse(element.hasChanged(), "Should not have changed");

		test.getAggregatedCollection().add(element);

		assertTrue(test.hasChanged(), "Should have changed");
	}

	@Test
	void testPersistentCollectedHasChanged() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
		AllAttributesPersistent element = Util.constructRandomInstance(u, m, aapd, 1);
		test.getAggregatedCollection().add(element);

		test = p.save(test);

		assertFalse(test.hasChanged(), "Should not have changed");

		test.getAggregatedCollection().get(0).setText("TEST");

		assertTrue(test.hasChanged(), "Should have changed");
	}

	@Test
	@SuppressWarnings("static-method")
	void testUTF8Length() {
		Charset utf8 = Charset.forName("UTF-8");
		AllCodepointsIterator iterator = new AllCodepointsIterator();
		while (iterator.hasNext()) {
			String test = new String(Character.toChars(iterator.next()));
			assertEquals(test.getBytes(utf8).length, Util.utf8Length(test));
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
		assertEquals("#000000", Util.htmlColourCode(Color.BLACK));
		assertEquals("#ffffff", Util.htmlColourCode(Color.WHITE));
		assertEquals("#ff0000", Util.htmlColourCode(Color.RED));
		assertEquals("#00ff00", Util.htmlColourCode(Color.GREEN));
		assertEquals("#0000ff", Util.htmlColourCode(Color.BLUE));

		assertEquals(Color.BLACK, Util.htmlColour("#000000"));
		assertEquals(Color.WHITE, Util.htmlColour("#FFFFFF"));
		assertEquals(Color.RED, Util.htmlColour("#ff0000"));
		assertEquals(Color.GREEN, Util.htmlColour("#00ff00"));
		assertEquals(Color.BLUE, Util.htmlColour("#0000ff"));
	}
}
