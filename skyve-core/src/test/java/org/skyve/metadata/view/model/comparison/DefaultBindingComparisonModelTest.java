package org.skyve.metadata.view.model.comparison;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

@SuppressWarnings("static-method")
class DefaultBindingComparisonModelTest {

	@Test
	void excludedReturnsFalseWhenNullPrefixes() {
		@SuppressWarnings("null") // passes null on purpose
		DefaultBindingComparisonModel<?, ?> model = new DefaultBindingComparisonModel<>(null, null, null);
		assertFalse(model.excluded("someBinding"));
	}

	@Test
	void excludedReturnsFalseWhenEmptyPrefixes() {
		@SuppressWarnings("null") // passes null on purpose
		DefaultBindingComparisonModel<?, ?> model = new DefaultBindingComparisonModel<>(null, null, new String[0]);
		assertFalse(model.excluded("someBinding"));
	}

	@Test
	void excludedReturnsFalseWhenBindingDoesNotMatchAnyPrefix() {
		@SuppressWarnings("null") // passes null on purpose
		DefaultBindingComparisonModel<?, ?> model = new DefaultBindingComparisonModel<>(null, null, new String[]{"address", "contact"});
		assertFalse(model.excluded("name"));
	}

	@Test
	void excludedReturnsTrueWhenBindingMatchesPrefix() {
		@SuppressWarnings("null") // passes null on purpose
		DefaultBindingComparisonModel<?, ?> model = new DefaultBindingComparisonModel<>(null, null, new String[]{"address"});
		assertTrue(model.excluded("address.street"));
	}

	@Test
	void excludedReturnsTrueWhenBindingEqualsPrefix() {
		@SuppressWarnings("null") // passes null on purpose
		DefaultBindingComparisonModel<?, ?> model = new DefaultBindingComparisonModel<>(null, null, new String[]{"address"});
		assertTrue(model.excluded("address"));
	}

	@Test
	void excludedReturnsFalseForPartialPrefixMatch() {
		@SuppressWarnings("null") // passes null on purpose
		DefaultBindingComparisonModel<?, ?> model = new DefaultBindingComparisonModel<>(null, null, new String[]{"addr"});
		assertFalse(model.excluded("name"));
	}

	@Test
	void excludedStripsIndexBeforeCheckingPrefix() {
		@SuppressWarnings("null") // passes null on purpose
		DefaultBindingComparisonModel<?, ?> model = new DefaultBindingComparisonModel<>(null, null, new String[]{"items"});
		assertTrue(model.excluded("items[0].name"));
	}

	@Test
	void excludedStripsIndexInMiddleOfBinding() {
		@SuppressWarnings("null") // passes null on purpose
		DefaultBindingComparisonModel<?, ?> model = new DefaultBindingComparisonModel<>(null, null, new String[]{"contacts"});
		assertTrue(model.excluded("contacts[2].phone"));
	}

	@Test
	void excludedWithMultipleIndexesStrippedCorrectly() {
		@SuppressWarnings("null") // passes null on purpose
		DefaultBindingComparisonModel<?, ?> model = new DefaultBindingComparisonModel<>(null, null, new String[]{"orders"});
		assertTrue(model.excluded("orders[1].lines[0].sku"));
	}

	@Test
	void excludedMatchesSecondPrefixInArray() {
		@SuppressWarnings("null") // passes null on purpose
		DefaultBindingComparisonModel<?, ?> model = new DefaultBindingComparisonModel<>(null, null, new String[]{"alpha", "beta"});
		assertTrue(model.excluded("beta.value"));
	}

	@Test
	void excludedReturnsFalseForEmptyBinding() {
		@SuppressWarnings("null") // passes null on purpose
		DefaultBindingComparisonModel<?, ?> model = new DefaultBindingComparisonModel<>(null, null, new String[]{"address"});
		assertFalse(model.excluded(""));
	}
}
