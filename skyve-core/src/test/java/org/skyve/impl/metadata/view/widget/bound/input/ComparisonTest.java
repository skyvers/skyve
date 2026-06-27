package org.skyve.impl.metadata.view.widget.bound.input;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class ComparisonTest {

	@Test
	void getPropertiesReturnsNonNullMap() {
		assertNotNull(new Comparison().getProperties());
	}

	@Test
	void jaxbHelperGetEnabledConditionNameReturnsNull() {
		// InputWidget.getEnabledConditionName() is package-private; test via Comparison
		assertNull(new Comparison().getEnabledConditionName());
	}

	@Test
	void jaxbHelperGetVisibleConditionNameReturnsNull() {
		// InputWidget.getVisibleConditionName() is package-private; test via Comparison
		assertNull(new Comparison().getVisibleConditionName());
	}
}
