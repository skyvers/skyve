package org.skyve.impl.metadata.view.widget.bound.tabular;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class DataRepeaterTest {

	@Test
	void getPropertiesReturnsNonNullMap() {
		assertNotNull(new DataRepeater().getProperties());
	}
}
