package org.skyve.impl.bind;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;

@SuppressWarnings("static-method")
class BindUtilStringTest {

	// --- negateCondition ---

	@Test
	void negateConditionNullReturnsNull() {
		assertThat(BindUtil.negateCondition(null), nullValue());
	}

	@Test
	void negateConditionTrueReturnsFalse() {
		assertThat(BindUtil.negateCondition("true"), is("false"));
	}

	@Test
	void negateConditionFalseReturnsTrue() {
		assertThat(BindUtil.negateCondition("false"), is("true"));
	}

	@Test
	void negateConditionWithNotPrefixRemovesNot() {
		assertThat(BindUtil.negateCondition("notActive"), is("active"));
	}

	@Test
	void negateConditionWithNotPrefixDecapitalizes() {
		assertThat(BindUtil.negateCondition("notEnabled"), is("enabled"));
	}

	@Test
	void negateConditionWithoutNotPrefixAddsNot() {
		assertThat(BindUtil.negateCondition("active"), is("notActive"));
	}

	@Test
	void negateConditionSimpleNameGetsNotPrefix() {
		assertThat(BindUtil.negateCondition("enabled"), is("notEnabled"));
	}

	@Test
	void negateConditionMultiWordWithNotPrefix() {
		assertThat(BindUtil.negateCondition("notSomeCondition"), is("someCondition"));
	}

	@Test
	void negateConditionInlineExpressionPrefixesNot() {
		assertThat(BindUtil.negateCondition("{el:bean.active}"), is("not{el:bean.active}"));
	}

	@Test
	void negateConditionInverseOfInlineExpressionRemovesNot() {
		assertThat(BindUtil.negateCondition("not{el:bean.active}"), is("{el:bean.active}"));
	}

	@Test
	void negateConditionInlineExpressionKeepsLeadingCaseOfExpression() {
		// an expression is not a name, so nothing is capitalised or decapitalised
		assertThat(BindUtil.negateCondition(BindUtil.negateCondition("{URL}")), is("{URL}"));
	}

	// --- isNegatedSkyveExpression ---

	@Test
	void isNegatedSkyveExpressionInverseOfExpressionReturnsTrue() {
		assertTrue(BindUtil.isNegatedSkyveExpression("not{el:bean.active}"));
	}

	@Test
	void isNegatedSkyveExpressionExpressionReturnsFalse() {
		assertFalse(BindUtil.isNegatedSkyveExpression("{el:bean.active}"));
	}

	@Test
	void isNegatedSkyveExpressionNegatedConditionNameReturnsFalse() {
		assertFalse(BindUtil.isNegatedSkyveExpression("notActive"));
	}

	@Test
	void isNegatedSkyveExpressionUnclosedReturnsFalse() {
		assertFalse(BindUtil.isNegatedSkyveExpression("not{el:bean.active"));
	}

	@Test
	void isNegatedSkyveExpressionTooShortReturnsFalse() {
		assertFalse(BindUtil.isNegatedSkyveExpression("not{"));
	}

	// --- identifier ---

	@Test
	@SuppressWarnings("java:S9142") // regex compilation cost is irrelevant in a test
	void identifierIsLetterFirstAlphanumericOfFixedLength() {
		for (String text : new String[] {"", "a", "{el:bean.active}", "{\"moduleName\":\"admin\"}", "multi\nline \u00e9\u4e2d"}) {
			assertTrue(BindUtil.identifier(text).matches("[A-Za-z][0-9A-Za-z]{26}"), text);
		}
	}

	@Test
	void identifierIsTheWholeSHA1DigestInBase62() throws Exception {
		for (String text : new String[] {"", "a", "not an expression", "{\"moduleName\":\"admin\"}", "multi\nline \u00e9\u4e2d"}) {
			assertThat(BindUtil.identifier(text), is(base62OfSHA1(text)));
		}
	}

	@Test
	void identifierIsStableAndDiffersForDifferentText() {
		assertThat(BindUtil.identifier("some text"), is(BindUtil.identifier("some text")));
		assertNotEquals(BindUtil.identifier("some text "), BindUtil.identifier("some text"));
	}

	@Test
	void identifierOfAnExpressionIsItsConditionIdentifier() {
		assertThat(BindUtil.conditionIdentifier("{el:bean.active}"), is(BindUtil.identifier("{el:bean.active}")));
		assertThat(BindUtil.conditionIdentifier("not{el:bean.active}"), is("not" + BindUtil.identifier("{el:bean.active}")));
	}

	@Test
	void identifierDoesNotTreatConditionNamesOrLiteralsSpecially() {
		// unlike conditionIdentifier(), which passes these through
		for (String text : new String[] {"true", "active", "notActive"}) {
			assertNotEquals(text, BindUtil.identifier(text));
			assertThat(BindUtil.identifier(text), is(not(BindUtil.conditionIdentifier(text))));
		}
	}

	// --- conditionIdentifier ---

	@Test
	void conditionIdentifierBooleanLiteralsUnchanged() {
		assertThat(BindUtil.conditionIdentifier("true"), is("true"));
		assertThat(BindUtil.conditionIdentifier("false"), is("false"));
	}

	@Test
	void conditionIdentifierConditionNamesUnchanged() {
		assertThat(BindUtil.conditionIdentifier("active"), is("active"));
		assertThat(BindUtil.conditionIdentifier("notActive"), is("notActive"));
	}

	@Test
	void conditionIdentifierInlineExpressionIsLetterFirstAlphanumericOfFixedLength() {
		String identifier = BindUtil.conditionIdentifier("{el:bean.active}");
		assertTrue(identifier.matches("[A-Za-z][0-9A-Za-z]{26}"));
	}

	@Test
	void conditionIdentifierIsTheWholeSHA1DigestInBase62() throws Exception {
		for (String expression : new String[] {"{el:bean.active}", "{role:admin.BasicUser}", "{el:bean.text eq 'it''s' and bean.number > 1}", "{}"}) {
			assertThat(BindUtil.conditionIdentifier(expression), is(base62OfSHA1(expression)));
		}
	}

	@Test
	void conditionIdentifierIsStable() {
		assertThat(BindUtil.conditionIdentifier("{el:bean.active}"), is(BindUtil.conditionIdentifier("{el:bean.active}")));
	}

	@Test
	void conditionIdentifierDiffersForDifferentExpressions() {
		assertNotEquals(BindUtil.conditionIdentifier("{el:bean.inactive}"), BindUtil.conditionIdentifier("{el:bean.active}"));
	}

	@Test
	void conditionIdentifierInverseIsNotFollowedByTheIdentifierVerbatim() {
		assertThat(BindUtil.conditionIdentifier("not{el:bean.active}"), is("not" + BindUtil.conditionIdentifier("{el:bean.active}")));
	}

	/**
	 * Independent rendering of a SHA-1 digest as a letter (radix 52) followed by 26 base 62 digits.
	 */
	@SuppressWarnings("java:S4790") // SHA-1 is the fingerprint under test, not used for security
	private static String base62OfSHA1(String text) throws NoSuchAlgorithmException {
		final String letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
		final String alphanumerics = "0123456789" + letters;
		BigInteger value = new BigInteger(1, MessageDigest.getInstance("SHA-1").digest(text.getBytes(StandardCharsets.UTF_8)));
		BigInteger sixtyTwo = BigInteger.valueOf(62);
		StringBuilder result = new StringBuilder(27);
		for (int i = 0; i < 26; i++) {
			BigInteger[] quotientAndRemainder = value.divideAndRemainder(sixtyTwo);
			result.append(alphanumerics.charAt(quotientAndRemainder[1].intValue()));
			value = quotientAndRemainder[0];
		}
		result.append(letters.charAt(value.intValueExact()));
		return result.reverse().toString();
	}

	// --- toJavaTypeIdentifier ---

	@Test
	void toJavaTypeIdentifierCapitalizesFirstLetter() {
		assertThat(BindUtil.toJavaTypeIdentifier("hello"), is("Hello"));
	}

	@Test
	void toJavaTypeIdentifierAlreadyCapitalized() {
		assertThat(BindUtil.toJavaTypeIdentifier("Hello"), is("Hello"));
	}

	@Test
	void toJavaTypeIdentifierRemovesSpaces() {
		String result = BindUtil.toJavaTypeIdentifier("hello world");
		assertFalse(result.contains(" "));
	}

	// --- toJavaInstanceIdentifier ---

	@Test
	void toJavaInstanceIdentifierDecapitalizesFirstLetter() {
		assertThat(BindUtil.toJavaInstanceIdentifier("Hello"), is("hello"));
	}

	@Test
	void toJavaInstanceIdentifierAlreadyLowerCase() {
		assertThat(BindUtil.toJavaInstanceIdentifier("hello"), is("hello"));
	}

	// --- toJavaPropertyName ---

	@Test
	void toJavaPropertyNameStripsGetPrefix() {
		assertThat(BindUtil.toJavaPropertyName("getFirstName"), is("firstName"));
	}

	@Test
	void toJavaPropertyNameStripsSetPrefix() {
		assertThat(BindUtil.toJavaPropertyName("setFirstName"), is("firstName"));
	}

	@Test
	void toJavaPropertyNameStripsIsPrefix() {
		assertThat(BindUtil.toJavaPropertyName("isActive"), is("active"));
	}

	@Test
	void toJavaPropertyNameNoKnownPrefixUnchanged() {
		assertThat(BindUtil.toJavaPropertyName("firstName"), is("firstName"));
	}

	// --- toTitleCase ---

	@Test
	void toTitleCaseCamelCaseGetsSpaces() {
		String result = BindUtil.toTitleCase("helloWorld");
		assertTrue(result.contains(" "));
	}

	@Test
	void toTitleCaseSingleWordCapitalized() {
		String result = BindUtil.toTitleCase("hello");
		assertEquals('H', result.charAt(0));
	}

	// --- createCompoundBinding ---

	@Test
	void createCompoundBindingTwoBindingsJoinedWithDot() {
		assertThat(BindUtil.createCompoundBinding("a", "b"), is("a.b"));
	}

	@Test
	void createCompoundBindingThreeBindings() {
		assertThat(BindUtil.createCompoundBinding("a", "b", "c"), is("a.b.c"));
	}

	@Test
	void createCompoundBindingOneBinding() {
		assertThat(BindUtil.createCompoundBinding("field"), is("field"));
	}

	// --- createIndexedBinding ---

	@Test
	void createIndexedBindingAppendsIndexInBrackets() {
		assertThat(BindUtil.createIndexedBinding("list", 0), is("list[0]"));
	}

	@Test
	void createIndexedBindingIndex2() {
		assertThat(BindUtil.createIndexedBinding("items", 2), is("items[2]"));
	}

	// --- createIdBinding ---

	@Test
	void createIdBindingAppendsElementById() {
		String result = BindUtil.createIdBinding("contacts", "abc123");
		assertThat(result, is("contactsElementById(abc123)"));
	}

	// --- sanitiseBinding ---

	@Test
	void sanitiseBindingNullReturnsNull() {
		assertThat(BindUtil.sanitiseBinding(null), nullValue());
	}

	@Test
	void sanitiseBindingReplacesDotWithUnderscore() {
		assertThat(BindUtil.sanitiseBinding("a.b"), is("a_b"));
	}

	@Test
	void sanitiseBindingReplacesBracketsWithUnderscore() {
		assertThat(BindUtil.sanitiseBinding("list[0]"), is("list_0_"));
	}

	@Test
	void sanitiseBindingCompoundBinding() {
		assertThat(BindUtil.sanitiseBinding("a.b.c"), is("a_b_c"));
	}

	// --- unsanitiseBinding ---

	@Test
	void unsanitiseBindingNullReturnsNull() {
		assertThat(BindUtil.unsanitiseBinding(null), nullValue());
	}

	@Test
	void unsanitiseBindingRestoresDotFromUnderscore() {
		assertThat(BindUtil.unsanitiseBinding("a_b"), is("a.b"));
	}

	@Test
	void unsanitiseBindingRestoresIndexedBinding() {
		// "list_0_" -> "list[0]"
		assertThat(BindUtil.unsanitiseBinding("list_0_"), is("list[0]"));
	}

	// --- isAScalarType ---

	@Test
	void isAScalarTypeStringIsScalar() {
		assertTrue(BindUtil.isAScalarType(String.class));
	}

	@Test
	void isAScalarTypeIntegerIsScalar() {
		assertTrue(BindUtil.isAScalarType(Integer.class));
	}

	@Test
	void isAScalarTypeListIsNotScalar() {
		assertFalse(BindUtil.isAScalarType(List.class));
	}

	@Test
	void isAScalarTypePrimitiveIntIsScalar() {
		assertTrue(BindUtil.isAScalarType(int.class));
	}

	// --- isImplicit ---

	@Test
	void isImplicitBizIdIsImplicit() {
		assertTrue(BindUtil.isImplicit(Bean.DOCUMENT_ID));
	}

	@Test
	void isImplicitBizKeyIsImplicit() {
		assertTrue(BindUtil.isImplicit(Bean.BIZ_KEY));
	}

	@Test
	void isImplicitLockIsImplicit() {
		assertTrue(BindUtil.isImplicit(PersistentBean.LOCK_NAME));
	}

	@Test
	void isImplicitVersionIsImplicit() {
		assertTrue(BindUtil.isImplicit(PersistentBean.VERSION_NAME));
	}

	@Test
	void isImplicitRegularAttributeIsNotImplicit() {
		assertFalse(BindUtil.isImplicit("myCustomField"));
	}

	@Test
	void isImplicitNullIsNotImplicit() {
		assertFalse(BindUtil.isImplicit(null));
	}

	// --- toJavaStaticIdentifier ---

	@Test
	void toJavaStaticIdentifierCamelCaseBecomesUpperSnakeCase() {
		assertThat(BindUtil.toJavaStaticIdentifier("someProperty"), is("SOME_PROPERTY"));
	}

	@Test
	void toJavaStaticIdentifierAlreadyUpperCase() {
		String result = BindUtil.toJavaStaticIdentifier("name");
		assertThat(result, is("NAME"));
	}
}
