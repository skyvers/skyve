package org.skyve.impl.metadata.view.widget;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class StaticImageTest {

	@Test
	void showsLabelByDefaultReturnsFalse() {
		assertFalse(new StaticImage().showsLabelByDefault());
	}

	@Test
	void jaxbHelperGetVisibleConditionNameReturnsNull() {
		assertNull(new StaticImage().getVisibleConditionName());
	}

	@Test
	void getPropertiesReturnsNonNullMap() {
		assertNotNull(new StaticImage().getProperties());
	}

	@Test
	void setVisibleConditionNameNegatesCondition() {
		StaticImage img = new StaticImage();
		img.setVisibleConditionName("visible");
		assertNotNull(img.getInvisibleConditionName());
	}
}
