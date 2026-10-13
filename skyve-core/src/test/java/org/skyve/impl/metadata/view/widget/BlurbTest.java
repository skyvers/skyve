package org.skyve.impl.metadata.view.widget;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class BlurbTest {

	@Test
	void showsLabelByDefaultReturnsFalse() {
		assertFalse(new Blurb().showsLabelByDefault());
	}

	@Test
	void xmlVisibleConditionIsNullByDefault() {
		assertNull(new Blurb().getXmlVisibleCondition());
	}

	@Test
	void getLocalisedMarkupWithMarkupSetReturnsValue() {
		Blurb blurb = new Blurb();
		blurb.setMarkup("Hello");
		assertNotNull(blurb.getLocalisedMarkup());
	}

	@Test
	void setVisibleConditionDerivesNegatedCondition() {
		Blurb blurb = new Blurb();
		blurb.setXmlVisibleCondition("active");
		assertEquals("notActive", blurb.getInvisibleCondition());
	}
}
