package org.skyve.impl.domain.number;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.domain.number.NumberGenerator;
import org.mockito.Mockito;

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

	// ---- NumberGeneratorStaticSingleton tests ----

	@Test
	void singletonGetInitiallyNullOrSet() {
		// We can only verify the API is callable without error
		NumberGenerator original = NumberGeneratorStaticSingleton.get();
		// set a mock generator
		NumberGenerator mockGenerator = Mockito.mock(NumberGenerator.class);
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
