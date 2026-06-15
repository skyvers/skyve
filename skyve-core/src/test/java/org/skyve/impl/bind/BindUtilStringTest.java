package org.skyve.impl.bind;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

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
