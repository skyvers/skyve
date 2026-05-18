package org.skyve.impl.util.json;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.lang.reflect.Constructor;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class MinifierTest {

	@Test
	void minifyNullReturnsNull() {
		assertThat(Minifier.minify(null), is(nullValue()));
	}

	@Test
	void minifyEmptyStringReturnsEmpty() {
		assertThat(Minifier.minify(""), is(""));
	}

	@Test
	void minifyAlreadyCleanInput() {
		String result = Minifier.minify("{\"key\":\"value\"}");
		assertThat(result, is("{\"key\":\"value\"}"));
	}

	@Test
	void minifyStripsLeadingWhitespace() {
		String result = Minifier.minify("   {\"a\":1}");
		assertThat(result, is("{\"a\":1}"));
	}

	@Test
	void minifyStripsTrailingWhitespace() {
		String result = Minifier.minify("{\"a\":1}   ");
		assertThat(result, is("{\"a\":1}"));
	}

	@Test
	void minifyStripsInternalWhitespace() {
		String result = Minifier.minify("{ \"a\" : 1 }");
		assertThat(result, is("{\"a\":1}"));
	}

	@Test
	void minifyRemovesSingleLineComment() {
		String result = Minifier.minify("{// comment\n\"a\":1}");
		assertThat(result, is("{\"a\":1}"));
	}

	@Test
	void minifyRemovesMultiLineComment() {
		String result = Minifier.minify("{/* comment */\"a\":1}");
		assertThat(result, is("{\"a\":1}"));
	}

	@Test
	void minifyRemovesMultiLineCommentSpanningLines() {
		String result = Minifier.minify("{/* line1\nline2 */\"a\":1}");
		assertThat(result, is("{\"a\":1}"));
	}

	@Test
	void minifyPreservesStringContents() {
		// Spaces inside strings must be preserved
		String result = Minifier.minify("{\"key\":\"hello world\"}");
		assertThat(result, is("{\"key\":\"hello world\"}"));
	}

	@Test
	void minifyPreservesSlashSlashInsideString() {
		String result = Minifier.minify("{\"url\":\"http://example.com\"}");
		assertThat(result, is("{\"url\":\"http://example.com\"}"));
	}

	@Test
	void minifyPreservesSlashStarInsideString() {
		String result = Minifier.minify("{\"v\":\"a/*b*/c\"}");
		assertThat(result, is("{\"v\":\"a/*b*/c\"}"));
	}

	@Test
	void minifyStripsNewlines() {
		String result = Minifier.minify("{\n\"a\":1\n}");
		assertThat(result, is("{\"a\":1}"));
	}

	@Test
	void minifyStripsTabs() {
		String result = Minifier.minify("{\t\"a\":\t1}");
		assertThat(result, is("{\"a\":1}"));
	}

	@Test
	void minifyMultiplePropertiesWithSpaces() {
		String result = Minifier.minify("{ \"a\": 1, \"b\": 2 }");
		assertThat(result, is("{\"a\":1,\"b\":2}"));
	}

	@Test
	void minifyCommentAtEndWithoutNewline() {
		// single-line comment at end of string with no newline - everything after // stripped
		String result = Minifier.minify("{\"a\":1}//trailing");
		assertThat(result, is("{\"a\":1}"));
	}

	@Test
	void minifyPreservesEscapedQuoteInString() {
		String result = Minifier.minify("{\"v\":\"say \\\"hi\\\"\"}");
		assertThat(result, is("{\"v\":\"say \\\"hi\\\"\"}"));
	}

	@Test
	void minifyPreservesEscapedBackslashInString() {
		String result = Minifier.minify("{\"v\":\"a\\\\b\"}");
		assertThat(result, is("{\"v\":\"a\\\\b\"}"));
	}

	@Test
	void minifyNestedObject() {
		String result = Minifier.minify("{ \"outer\": { \"inner\": true } }");
		assertThat(result, is("{\"outer\":{\"inner\":true}}"));
	}

	@Test
	void minifyArray() {
		String result = Minifier.minify("[ 1, 2, 3 ]");
		assertThat(result, is("[1,2,3]"));
	}

	@Test
	void minifyUnterminatedMultiLineCommentDoesNotThrow() {
		// Unclosed comment: /* hits end of string while looking for closer (*/)
		// This exercises the break at end-of-string inside the comment-closer loop
		String result = Minifier.minify("{\"a\":1 /* unclosed");
		assertThat(result, is("{\"a\":1"));
	}

	@Test
	void minifyCommentEndingWithPartialCloserDoesNotThrow() {
		// Inside a multi-line comment, string ends with '*' which is a partial match for '*/' closer
		// This exercises the break at end-of-string inside the comment-closer loop (x=1 branch)
		String result = Minifier.minify("{\"a\":1 /* partial*");
		assertThat(result, is("{\"a\":1"));
	}

	@Test
	void minifyInputEndingWithCommentOpenerPartiallyDoesNotThrow() {
		// Input ends with '/' which is the start of a comment indicator but no second char
		// This exercises the break at end-of-string inside the comment-opener loop
		String result = Minifier.minify("{\"a\":1}/");
		assertThat(result, is("{\"a\":1}/"));
	}

	@Test
	@SuppressWarnings("rawtypes")
	void privateConstructorIsAccessible() throws Exception {
		// Utility class has a private constructor; invoke via reflection to get line coverage
		Constructor c = Minifier.class.getDeclaredConstructor();
		c.setAccessible(true);
		Object instance = c.newInstance();
		assertThat(instance, instanceOf(Minifier.class));
	}
}
