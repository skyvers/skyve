package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;

class FluentRadioTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorCreatesRadio() {
		FluentRadio r = new FluentRadio();
		assertNotNull(r.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void wrappingConstructorUsesProvided() {
		Radio radio = new Radio();
		FluentRadio fr = new FluentRadio(radio);
		assertSame(radio, fr.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void fromWithVerticalNonNullCopiesVertical() {
		Radio radio = new Radio();
		radio.setVertical(Boolean.TRUE);
		FluentRadio fr = new FluentRadio();
		FluentRadio result = fr.from(radio);
		assertSame(fr, result);
		assertEquals(Boolean.TRUE, fr.get().getVertical());
	}
}
