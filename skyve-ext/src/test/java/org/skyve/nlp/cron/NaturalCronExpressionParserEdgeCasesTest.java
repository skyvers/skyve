package org.skyve.nlp.cron;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

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

}
