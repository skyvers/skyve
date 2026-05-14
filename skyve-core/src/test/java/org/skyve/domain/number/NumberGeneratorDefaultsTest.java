package org.skyve.domain.number;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class NumberGeneratorDefaultsTest {

	/**
	 * Minimal implementation: always returns "42" regardless of args.
	 */
	private NumberGenerator generator;

	@BeforeEach
	public void setUp() {
		generator = (prefix, moduleName, documentName, fieldName, minimumLength) -> "42";
	}

	@Test
	public void nextIntParsesResult() {
		assertThat(generator.nextInt("mod", "doc", "field"), is(Integer.valueOf(42)));
	}

	@Test
	public void nextLongParsesResult() {
		assertThat(generator.nextLong("mod", "doc", "field"), is(Long.valueOf(42L)));
	}

	@Test
	public void nextWithoutPrefixOrLength() {
		assertThat(generator.next("mod", "doc", "field"), is("42"));
	}

	@Test
	public void nextWithMinimumLength() {
		assertThat(generator.next("mod", "doc", "field", 5), is("42"));
	}

	@Test
	public void nextWithPrefixOnly() {
		assertThat(generator.next("INV", "mod", "doc", "field"), is("42"));
	}
}
