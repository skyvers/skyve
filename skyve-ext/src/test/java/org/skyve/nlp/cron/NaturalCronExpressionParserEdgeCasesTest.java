package org.skyve.nlp.cron;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Timeout.ThreadMode;

class NaturalCronExpressionParserEdgeCasesTest {

	@Test
	@SuppressWarnings("static-method")
	void testParseNullReturnsNull() {
		// string is null → returns null (covers L146)
		NaturalCronExpressionParser parser = new NaturalCronExpressionParser();
		assertThat(parser.parse(null), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void testParseUnparsableThrowsException() {
		// unparsable string → throws CronParserException (covers L140)
		assertThrows(CronParserException.class, () -> new NaturalCronExpressionParser().parse("xyz abc 123 qrs"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testValidPatternMatchesWellFormedSixFieldExpression() {
		assertTrue("0 0 0 * * *".matches(NaturalCronExpressionParser.VALID_PATTERN));
	}

	@Test
	@SuppressWarnings("static-method")
	void testValidPatternRejectsFiveFieldExpression() {
		assertFalse("0 0 0 * *".matches(NaturalCronExpressionParser.VALID_PATTERN));
	}

	@Test
	@Timeout(value = 1000, unit = TimeUnit.MILLISECONDS, threadMode = ThreadMode.SEPARATE_THREAD)
	@SuppressWarnings("static-method")
	void testValidPatternDoesNotReDoSOnAdversarialInput() {
		// Without possessive quantifiers the old pattern was O(2^n) on this input:
		// a valid-looking 6th field with many two-digit comma values ending in an
		// invalid character forces exponential backtracking via [1-9]? ambiguity.
		// With possessive quantifiers (++, ?+, *+) the match fails in O(n).
		String adversarial = "0 0 0 0 0 1" + ",12".repeat(20) + "X";
		assertFalse(adversarial.matches(NaturalCronExpressionParser.VALID_PATTERN));
	}

}
