package org.skyve.metadata.view.model.comparison;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

@SuppressWarnings("static-method")
class DefaultBindingComparisonModelTest {

	@Test
	void excludedReturnsFalseWhenNullPrefixes() {
		DefaultBindingComparisonModel<?, ?> model = new DefaultBindingComparisonModel<>(null, null, null);
		assertFalse(model.excluded("someBinding"));
	}

	@Test
	void excludedReturnsFalseWhenEmptyPrefixes() {
		DefaultBindingComparisonModel<?, ?> model = new DefaultBindingComparisonModel<>(null, null, new String[0]);
		assertFalse(model.excluded("someBinding"));
	}

	@Test
	void excludedReturnsFalseWhenBindingDoesNotMatchAnyPrefix() {
		DefaultBindingComparisonModel<?, ?> model = new DefaultBindingComparisonModel<>(null, null, new String[]{"address", "contact"});
		assertFalse(model.excluded("name"));
	}

	@Test
	void excludedReturnsTrueWhenBindingMatchesPrefix() {
		DefaultBindingComparisonModel<?, ?> model = new DefaultBindingComparisonModel<>(null, null, new String[]{"address"});
		assertTrue(model.excluded("address.street"));
	}

	@Test
	void excludedReturnsTrueWhenBindingEqualsPrefix() {
		DefaultBindingComparisonModel<?, ?> model = new DefaultBindingComparisonModel<>(null, null, new String[]{"address"});
		assertTrue(model.excluded("address"));
	}

	@Test
	void excludedReturnsFalseForPartialPrefixMatch() {
		DefaultBindingComparisonModel<?, ?> model = new DefaultBindingComparisonModel<>(null, null, new String[]{"addr"});
		assertFalse(model.excluded("name"));
	}

	@Test
	void excludedStripsIndexBeforeCheckingPrefix() {
		DefaultBindingComparisonModel<?, ?> model = new DefaultBindingComparisonModel<>(null, null, new String[]{"items"});
		assertTrue(model.excluded("items[0].name"));
	}

	@Test
	void excludedStripsIndexInMiddleOfBinding() {
		DefaultBindingComparisonModel<?, ?> model = new DefaultBindingComparisonModel<>(null, null, new String[]{"contacts"});
		assertTrue(model.excluded("contacts[2].phone"));
	}

	@Test
	void excludedWithMultipleIndexesStrippedCorrectly() {
		DefaultBindingComparisonModel<?, ?> model = new DefaultBindingComparisonModel<>(null, null, new String[]{"orders"});
		assertTrue(model.excluded("orders[1].lines[0].sku"));
	}

	@Test
	void excludedMatchesSecondPrefixInArray() {
		DefaultBindingComparisonModel<?, ?> model = new DefaultBindingComparisonModel<>(null, null, new String[]{"alpha", "beta"});
		assertTrue(model.excluded("beta.value"));
	}

	@Test
	void excludedReturnsFalseForEmptyBinding() {
		DefaultBindingComparisonModel<?, ?> model = new DefaultBindingComparisonModel<>(null, null, new String[]{"address"});
		assertFalse(model.excluded(""));
	}
}
