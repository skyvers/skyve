package org.skyve.impl.metadata.view.widget;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class BlurbTest {

	@Test
	void showsLabelByDefaultReturnsFalse() {
		assertFalse(new Blurb().showsLabelByDefault());
	}

	@Test
	void jaxbHelperGetVisibleConditionNameReturnsNull() {
		assertNull(new Blurb().getVisibleConditionName());
	}

	@Test
	void getLocalisedMarkupWithMarkupSetReturnsValue() {
		Blurb blurb = new Blurb();
		blurb.setMarkup("Hello");
		assertNotNull(blurb.getLocalisedMarkup());
	}

	@Test
	void setVisibleConditionNameSetsInvisibleNegation() {
		Blurb blurb = new Blurb();
		blurb.setVisibleConditionName("active");
		assertNotNull(blurb.getInvisibleConditionName());
	}
}
