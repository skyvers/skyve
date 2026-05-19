package org.skyve;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.FormatterName;
import org.skyve.metadata.Ordering;
import org.skyve.metadata.SortDirection;

class CORETest {

	@Test
	@SuppressWarnings("static-method")
	void newOrderingReturnsOrderingWithCorrectValues() {
		Ordering ordering = CORE.newOrdering("name", SortDirection.ascending);
		assertNotNull(ordering);
		assertEquals("name", ordering.getBy());
		assertEquals(SortDirection.ascending, ordering.getSort());
	}

	@Test
	@SuppressWarnings("static-method")
	void formatWithNullValueReturnsEmptyString() {
		assertEquals("", CORE.format(FormatterName.DD_MM_YYYY, null));
	}
}
