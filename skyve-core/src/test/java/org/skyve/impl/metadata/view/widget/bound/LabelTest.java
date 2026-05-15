package org.skyve.impl.metadata.view.widget.bound;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.metadata.view.TextOutput.Sanitisation;

@SuppressWarnings("static-method")
class LabelTest {

	@Test
	void showsLabelByDefaultReturnsFalse() {
		assertFalse(new Label().showsLabelByDefault());
	}

	@Test
	void propertiesMapIsNotNull() {
		assertNotNull(new Label().getProperties());
	}

	@Test
	void valueRoundTrip() {
		Label l = new Label();
		l.setValue("Hello");
		assertEquals("Hello", l.getValue());
	}

	@Test
	void valueBlankBecomesNull() {
		Label l = new Label();
		l.setValue("  ");
		assertNull(l.getValue());
	}

	@Test
	void forBindingRoundTrip() {
		Label l = new Label();
		l.setFor("name");
		assertEquals("name", l.getFor());
	}

	@Test
	void forBindingBlankBecomesNull() {
		Label l = new Label();
		l.setFor("  ");
		assertNull(l.getFor());
	}

	@Test
	void pixelWidthRoundTrip() {
		Label l = new Label();
		l.setPixelWidth(Integer.valueOf(120));
		assertEquals(Integer.valueOf(120), l.getPixelWidth());
	}

	@Test
	void pixelHeightRoundTrip() {
		Label l = new Label();
		l.setPixelHeight(Integer.valueOf(30));
		assertEquals(Integer.valueOf(30), l.getPixelHeight());
	}

	@Test
	void invisibleConditionNameRoundTrip() {
		Label l = new Label();
		l.setInvisibleConditionName("hidden");
		assertEquals("hidden", l.getInvisibleConditionName());
	}

	@Test
	void visibleConditionNameNegated() {
		Label l = new Label();
		l.setVisibleConditionName("visible");
		assertEquals("notVisible", l.getInvisibleConditionName());
	}

	@Test
	void visibleConditionNameBlankBecomesNullInvisible() {
		Label l = new Label();
		l.setVisibleConditionName("  ");
		assertNull(l.getInvisibleConditionName());
	}

	@Test
	void formattedRoundTrip() {
		Label l = new Label();
		l.setFormatted(Boolean.TRUE);
		assertEquals(Boolean.TRUE, l.getFormatted());
	}

	@Test
	void textAlignmentRoundTrip() {
		Label l = new Label();
		l.setTextAlignment(HorizontalAlignment.centre);
		assertEquals(HorizontalAlignment.centre, l.getTextAlignment());
	}

	@Test
	void escapeRoundTrip() {
		Label l = new Label();
		l.setEscape(Boolean.TRUE);
		assertEquals(Boolean.TRUE, l.getEscape());
	}

	@Test
	void sanitiseRoundTrip() {
		Label l = new Label();
		l.setSanitise(Sanitisation.relaxed);
		assertEquals(Sanitisation.relaxed, l.getSanitise());
	}

	@Test
	void bindingRoundTrip() {
		Label l = new Label();
		l.setBinding("description");
		assertEquals("description", l.getBinding());
		assertEquals("description", l.getSource());
	}

	@Test
	void localisedValueReturnsNonNull() {
		Label l = new Label();
		l.setValue("Hello");
		assertNotNull(l.getLocalisedValue());
	}
}
