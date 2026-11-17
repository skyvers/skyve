package org.skyve.domain.number;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.withSettings;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.metadata.module.ModuleMemberTestSupport.TestDocument;

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

	/** Lets Mockito's subclass mock maker call the interface's default methods for real. */
	private abstract static class NumberGeneratorDouble implements NumberGenerator {
		// Intentionally empty.
	}

	@Test
	@SuppressWarnings("static-method")
	void typedOverloadsDelegateToTheStringForms() {
		NumberGenerator g = mock(NumberGeneratorDouble.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		doReturn("42").when(g).next(any(), anyString(), anyString(), anyString(), anyInt());

		assertAll(() -> assertThat(g.nextInt(TestDocument.CONTACT, "number"), is(Integer.valueOf(42))),
					() -> verify(g).nextInt("admin", "Contact", "number"),
					() -> assertThat(g.nextLong(TestDocument.CONTACT, "number"), is(Long.valueOf(42L))),
					() -> verify(g).nextLong("admin", "Contact", "number"),
					() -> assertThat(g.next(TestDocument.CONTACT, "number"), is("42")),
					() -> verify(g).next("admin", "Contact", "number"),
					() -> assertThat(g.next(TestDocument.CONTACT, "number", 5), is("42")),
					() -> verify(g).next("admin", "Contact", "number", 5),
					() -> assertThat(g.next("INV", TestDocument.CONTACT, "number"), is("42")),
					() -> verify(g).next("INV", "admin", "Contact", "number"),
					() -> assertThat(g.next("INV", TestDocument.CONTACT, "number", 5), is("42")),
					() -> verify(g).next("INV", "admin", "Contact", "number", 5));
	}
}
