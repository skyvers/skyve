package org.skyve.impl.metadata.view.widget.bound;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

/** Tests for {@link ParameterImpl} getter/setter round-trips. */
@SuppressWarnings("static-method")
class ParameterImplTest {

	@Test
	void nameRoundTrip() {
		ParameterImpl p = new ParameterImpl();
		p.setName("myParam");
		assertEquals("myParam", p.getName());
	}

	@Test
	void nameBlankBecomesNull() {
		ParameterImpl p = new ParameterImpl();
		p.setName("   ");
		assertNull(p.getName());
	}

	@Test
	void valueRoundTrip() {
		ParameterImpl p = new ParameterImpl();
		p.setValue("someValue");
		assertEquals("someValue", p.getValue());
	}

	@Test
	void valueBlankBecomesNull() {
		ParameterImpl p = new ParameterImpl();
		p.setValue("  ");
		assertNull(p.getValue());
	}

	@Test
	void valueBindingRoundTrip() {
		ParameterImpl p = new ParameterImpl();
		p.setValueBinding("contact.name");
		assertEquals("contact.name", p.getValueBinding());
	}

	@Test
	void valueBindingBlankBecomesNull() {
		ParameterImpl p = new ParameterImpl();
		p.setValueBinding("  ");
		assertNull(p.getValueBinding());
	}

	@Test
	void defaultsAreNull() {
		ParameterImpl p = new ParameterImpl();
		assertNull(p.getName());
		assertNull(p.getValue());
		assertNull(p.getValueBinding());
	}
}
