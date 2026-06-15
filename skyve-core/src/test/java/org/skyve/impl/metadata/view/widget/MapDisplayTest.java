package org.skyve.impl.metadata.view.widget;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class MapDisplayTest {

	@Test
	void jaxbHelperGetVisibleConditionNameReturnsNull() {
		MapDisplay widget = new MapDisplay();
		assertNull(widget.getVisibleConditionName());
	}

	@Test
	void getPropertiesReturnsNonNullMap() {
		MapDisplay widget = new MapDisplay();
		assertNotNull(widget.getProperties());
	}

	@Test
	void setVisibleConditionNameNegatesCondition() {
		MapDisplay widget = new MapDisplay();
		widget.setVisibleConditionName("visible");
		assertNotNull(widget.getInvisibleConditionName());
	}
}
