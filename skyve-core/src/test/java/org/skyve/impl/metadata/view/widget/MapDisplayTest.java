package org.skyve.impl.metadata.view.widget;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class MapDisplayTest {

	@Test
	void xmlVisibleConditionIsNullByDefault() {
		MapDisplay widget = new MapDisplay();
		assertNull(widget.getXmlVisibleCondition());
	}

	@Test
	void getPropertiesReturnsNonNullMap() {
		MapDisplay widget = new MapDisplay();
		assertNotNull(widget.getProperties());
	}

	@Test
	void setVisibleConditionNegatesCondition() {
		MapDisplay widget = new MapDisplay();
		widget.setXmlVisibleCondition("visible");
		assertEquals("notVisible", widget.getInvisibleCondition());
	}
}
