package org.skyve.nlp.cron;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class NaturalCronExpressionParserEdgeCasesTest {

	@Test
	@SuppressWarnings("static-method")
	public void testParseNullReturnsNull() {
		// string is null → returns null (covers L146)
		NaturalCronExpressionParser parser = new NaturalCronExpressionParser();
		assertThat(parser.parse(null), is(nullValue()));
	}

	@Test(expected = CronParserException.class)
	@SuppressWarnings("static-method")
	public void testParseUnparsableThrowsException() {
		// unparsable string → throws CronParserException (covers L140)
		new NaturalCronExpressionParser().parse("xyz abc 123 qrs");
	}

	@Test
	@SuppressWarnings("static-method")
	public void testValidPatternMatchesWellFormedSixFieldExpression() {
		assertTrue("0 0 0 * * *".matches(NaturalCronExpressionParser.VALID_PATTERN));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testValidPatternRejectsFiveFieldExpression() {
		assertFalse("0 0 0 * *".matches(NaturalCronExpressionParser.VALID_PATTERN));
	}

	@Test(timeout = 1000)
	@SuppressWarnings("static-method")
	public void testValidPatternDoesNotReDoSOnAdversarialInput() {
		// Without possessive quantifiers the old pattern was O(2^n) on this input:
		// a valid-looking 6th field with many two-digit comma values ending in an
		// invalid character forces exponential backtracking via [1-9]? ambiguity.
		// With possessive quantifiers (++, ?+, *+) the match fails in O(n).
		String adversarial = "0 0 0 0 0 1" + ",12".repeat(20) + "X";
		assertFalse(adversarial.matches(NaturalCronExpressionParser.VALID_PATTERN));
	}

}
