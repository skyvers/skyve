package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescriptionColumn;

/**
 * Tests for {@link FluentLookupDescriptionColumn}.
 */
@SuppressWarnings("static-method")
class FluentLookupDescriptionColumnTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertNotNull(new FluentLookupDescriptionColumn().get());
	}

	@Test
	void wrappingConstructorPreservesInstance() {
		LookupDescriptionColumn col = new LookupDescriptionColumn();
		FluentLookupDescriptionColumn fluent = new FluentLookupDescriptionColumn(col);
		assertEquals(col, fluent.get());
	}

	@Test
	void nameSetsValue() {
		FluentLookupDescriptionColumn fluent = new FluentLookupDescriptionColumn().name("myCol");
		assertEquals("myCol", fluent.get().getName());
	}

	@Test
	void filterableSetsTrue() {
		FluentLookupDescriptionColumn fluent = new FluentLookupDescriptionColumn().filterable(true);
		assertTrue(Boolean.TRUE.equals(fluent.get().getFilterable()));
	}

	@Test
	void fromCopiesNameAndFilterable() {
		LookupDescriptionColumn source = new LookupDescriptionColumn();
		source.setName("col1");
		source.setFilterable(Boolean.TRUE);
		FluentLookupDescriptionColumn fluent = new FluentLookupDescriptionColumn().from(source);
		assertEquals("col1", fluent.get().getName());
		assertTrue(Boolean.TRUE.equals(fluent.get().getFilterable()));
	}

	@Test
	void fromWithNullFilterableSkipsSet() {
		LookupDescriptionColumn source = new LookupDescriptionColumn();
		source.setName("col2");
		// filterable is null by default
		FluentLookupDescriptionColumn fluent = new FluentLookupDescriptionColumn().from(source);
		assertEquals("col2", fluent.get().getName());
		assertNull(fluent.get().getFilterable());
	}
}
