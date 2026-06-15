package org.skyve.metadata;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.converters.integer.SimplePercentage;

class ConverterNameTest {
	@Test
	@SuppressWarnings("static-method")
	void valueOfNullThrowsIllegalArgumentException() {
		assertThrows(IllegalArgumentException.class, () -> ConverterName.valueOf((org.skyve.domain.types.converters.Converter<?>) null));
	}

	@Test
	@SuppressWarnings("static-method")
	void valueOfKnownConverterReturnsConverterName() {
		ConverterName result = ConverterName.valueOf(new SimplePercentage());
		assertThat(result, is(ConverterName.SimplePercentage));
	}
}
