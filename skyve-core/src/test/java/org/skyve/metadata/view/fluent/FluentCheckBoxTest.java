package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;

class FluentCheckBoxTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorCreatesCheckBox() {
		FluentCheckBox c = new FluentCheckBox();
		assertNotNull(c.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void wrappingConstructorUsesProvided() {
		CheckBox check = new CheckBox();
		FluentCheckBox fc = new FluentCheckBox(check);
		assertSame(check, fc.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void fromWithTriStateNonNullCopiesTriState() {
		CheckBox check = new CheckBox();
		check.setTriState(Boolean.TRUE);
		FluentCheckBox fc = new FluentCheckBox();
		FluentCheckBox result = fc.from(check);
		assertSame(fc, result);
		assertEquals(Boolean.TRUE, fc.get().getTriState());
	}
}
