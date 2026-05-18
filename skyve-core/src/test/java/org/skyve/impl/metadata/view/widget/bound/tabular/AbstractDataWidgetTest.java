package org.skyve.impl.metadata.view.widget.bound.tabular;

import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class AbstractDataWidgetTest {

	@Test
	void jaxbHelperGetVisibleConditionNameReturnsNull() {
		// ListGrid extends AbstractDataWidget; use it as a concrete subclass
		ListGrid grid = new ListGrid();
		assertNull(grid.getVisibleConditionName());
	}
}
