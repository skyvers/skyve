package org.skyve.domain.number;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class NumberGeneratorDefaultsTest {

	/**
	 * Minimal implementation: always returns "42" regardless of args.
	 */
	private NumberGenerator generator;

	@BeforeEach
	void setUp() {
		generator = (prefix, moduleName, documentName, fieldName, minimumLength) -> "42";
	}

	@Test
	void nextIntParsesResult() {
		assertThat(generator.nextInt("mod", "doc", "field"), is(Integer.valueOf(42)));
	}

	@Test
	void nextLongParsesResult() {
		assertThat(generator.nextLong("mod", "doc", "field"), is(Long.valueOf(42L)));
	}

	@Test
	void nextWithoutPrefixOrLength() {
		assertThat(generator.next("mod", "doc", "field"), is("42"));
	}

	@Test
	void nextWithMinimumLength() {
		assertThat(generator.next("mod", "doc", "field", 5), is("42"));
	}

	@Test
	void nextWithPrefixOnly() {
		assertThat(generator.next("INV", "mod", "doc", "field"), is("42"));
	}
}
