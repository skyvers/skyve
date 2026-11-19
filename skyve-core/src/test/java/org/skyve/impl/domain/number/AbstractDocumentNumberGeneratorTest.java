package org.skyve.impl.domain.number;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;

import org.junit.jupiter.api.Test;
import org.skyve.domain.number.NumberGenerator;

/** Unit tests for {@link AbstractDocumentNumberGenerator#incrementAlpha} logic and {@link NumberGeneratorStaticSingleton}. */
@SuppressWarnings("static-method")
class AbstractDocumentNumberGeneratorTest {

	// ---- incrementAlpha tests ----

	@Test
	void incrementAlphaSimpleNumericIncrementsValue() {
		String result = AbstractDocumentNumberGenerator.incrementAlpha(null, "1", 4);
		assertThat(result, is("0002"));
	}

	@Test
	void incrementAlphaZeroPadsToMinLength() {
		String result = AbstractDocumentNumberGenerator.incrementAlpha(null, "9", 4);
		assertThat(result, is("0010"));
	}

	@Test
	void incrementAlphaWithNullLastNumberStartsAtOne() {
		String result = AbstractDocumentNumberGenerator.incrementAlpha(null, null, 4);
		assertThat(result, is("0001"));
	}

	@Test
	void incrementAlphaWithPrefixAppliesPrefixToResult() {
		String result = AbstractDocumentNumberGenerator.incrementAlpha("INV", "INV001", 6);
		assertThat(result, is("INV002"));
	}

	@Test
	void incrementAlphaWithNullLastNumberAndPrefixAppliesPrefix() {
		String result = AbstractDocumentNumberGenerator.incrementAlpha("INV", null, 6);
		assertThat(result, is("INV001"));
	}

	@Test
	void incrementAlphaAlphaNumericPrefixIncrementsNumericPart() {
		String result = AbstractDocumentNumberGenerator.incrementAlpha(null, "A001", 5);
		assertThat(result, is("A0002"));
	}

	@Test
	void incrementAlphaLargeNumberExceedsMinLengthStillCorrect() {
		String result = AbstractDocumentNumberGenerator.incrementAlpha(null, "9999", 4);
		assertThat(result, is("10000"));
	}

	@Test
	void incrementAlphaNumericPrefixIncrementsCorrectly() {
		// prefix is purely numeric, lastNumber is purely numeric, not "0"
		String result = AbstractDocumentNumberGenerator.incrementAlpha("10", "101", 5);
		assertThat(result, is("10002"));
	}

	@Test
	void incrementAlphaNumericPrefixWithZeroStartsAtPrefixedOne() {
		String result = AbstractDocumentNumberGenerator.incrementAlpha("10", "0", 5);
		assertThat(result, is("10001"));
	}

	@Test
	void incrementAlphaNonNumericLastNumberAppendsOne() {
		String result = AbstractDocumentNumberGenerator.incrementAlpha(null, "ABC", 4);
		assertThat(result, is("ABC1"));
	}

	@Test
	void incrementAlphaPassesTheIntegerLimit() {
		assertThat(AbstractDocumentNumberGenerator.incrementAlpha(null, "2147483647", 0), is("2147483648"));
	}

	@Test
	void incrementAlphaPassesTheIntegerLimitAfterAPrefix() {
		assertThat(AbstractDocumentNumberGenerator.incrementAlpha("INV", "INV2147483647", 0), is("INV2147483648"));
	}

	@Test
	void incrementAlphaPassesTheIntegerLimitAfterANumericPrefix() {
		assertThat(AbstractDocumentNumberGenerator.incrementAlpha("10", "102147483647", 0), is("102147483648"));
	}

	@Test
	void incrementAlphaKeepsZeroPaddingAtTheIntegerLimit() {
		assertThat(AbstractDocumentNumberGenerator.incrementAlpha("INV", "INV002147483647", 15), is("INV002147483648"));
	}

	@Test
	void incrementAlphaIncrementsAVeryLongDigitRun() {
		String nines = "9".repeat(40);
		assertThat(AbstractDocumentNumberGenerator.incrementAlpha("A", "A" + nines, 0), is("A1" + "0".repeat(40)));
		assertThat(AbstractDocumentNumberGenerator.incrementAlpha(null, nines, 0), is("1" + "0".repeat(40)));
	}

	// ---- NumberGeneratorStaticSingleton tests ----

	@Test
	void singletonGetInitiallyNullOrSet() {
		// We can only verify the API is callable without error
		NumberGenerator original = NumberGeneratorStaticSingleton.get();
		// set a mock generator
		NumberGenerator mockGenerator = mock(NumberGenerator.class);
		NumberGeneratorStaticSingleton.set(mockGenerator);
		assertThat(NumberGeneratorStaticSingleton.get(), is(mockGenerator));
		// restore
		NumberGeneratorStaticSingleton.set(original);
	}

	@Test
	void singletonSetDefaultInstallsDocumentNumberGenerator() {
		NumberGenerator original = NumberGeneratorStaticSingleton.get();
		try {
			NumberGeneratorStaticSingleton.setDefault();
			assertNotNull(NumberGeneratorStaticSingleton.get());
		}
		finally {
			NumberGeneratorStaticSingleton.set(original);
		}
	}
}
