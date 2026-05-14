package org.skyve.impl.bind;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;

import java.math.BigDecimal;
import java.util.Date;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.Attribute;

public class BindUtilTest {

	@Test
	@SuppressWarnings("static-method")
	public void testToJavaInstanceIdentifierLeavesValidIndentifier() {
		// setup the test data
		String identifier = "validIdentifier";

		// call the method under test
		String result = BindUtil.toJavaInstanceIdentifier(identifier);

		// verify the result
		assertThat(result, is(identifier));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testToJavaInstanceIdentifierRemovesInvalidCharacters() {
		// setup the test data
		String identifier = "E-Mail";

		// call the method under test
		String result = BindUtil.toJavaInstanceIdentifier(identifier);

		// verify the result
		assertThat(result, is("EMail"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testToJavaInstanceIdentifierRemovesWhitespace() {
		// setup the test data
		String identifier = "Whitespace Identifier";

		// call the method under test
		String result = BindUtil.toJavaInstanceIdentifier(identifier);

		// verify the result
		assertThat(result, is("whitespaceIdentifier"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testToJavaInstanceIdentifierPreservesTwoLeadingCapitals() {
		// setup the test data
		String identifier = "DOB";

		// call the method under test
		String result = BindUtil.toJavaInstanceIdentifier(identifier);

		// verify the result
		assertThat(result, is("DOB"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testToJavaTypeIdentifierRemovesInvalidCharacters() {
		// setup the test data
		String identifier = "E-Mail";

		// call the method under test
		String result = BindUtil.toJavaTypeIdentifier(identifier);

		// verify the result
		assertThat(result, is("EMail"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testToJavaTypeIdentifierRemovesWhitespace() {
		// setup the test data
		String identifier = "Whitespace Identifier";

		// call the method under test
		String result = BindUtil.toJavaTypeIdentifier(identifier);

		// verify the result
		assertThat(result, is("WhitespaceIdentifier"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testToJavaTypeIdentifierPreservesTwoLeadingCapitals() {
		// setup the test data
		String identifier = "DOB";

		// call the method under test
		String result = BindUtil.toJavaTypeIdentifier(identifier);

		// verify the result
		assertThat(result, is("DOB"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testToJavaStaticIdentifierRemovesWhitespace() {
		// setup the test data
		String identifier = "Whitespace Identifier";

		// call the method under test
		String result = BindUtil.toJavaStaticIdentifier(identifier);

		// verify the result
		assertThat(result, is("WHITESPACE_IDENTIFIER"));
	}

	/**
	 * Two leading capitals shouldn't have any effect on the static identifier, this
	 * just makes sure the Introspector.decapitalize has no negative effects.
	 */
	@Test
	@SuppressWarnings("static-method")
	public void testToJavaStaticIdentifierPreservesTwoLeadingCapitals() {
		// setup the test data
		String identifier = "DOB";

		// call the method under test
		String result = BindUtil.toJavaStaticIdentifier(identifier);

		// verify the result
		assertThat(result, is("DOB"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testGetImplementingTypeForGenerateDomainValidationReturnsResolvedType() {
		Attribute attribute = mock(Attribute.class);
		doReturn(String.class).when(attribute).getImplementingType();

		assertEquals(String.class, BindUtil.getImplementingTypeForGenerateDomainValidation(attribute));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testGetImplementingTypeForGenerateDomainValidationFallsBackToEnumClass() {
		Enumeration enumeration = mock(Enumeration.class);
		doThrow(new MetaDataException("Enum class is not generated yet",
				new ClassNotFoundException("Enum class not found"))).when(enumeration).getImplementingType();

		assertEquals(Enum.class, BindUtil.getImplementingTypeForGenerateDomainValidation(enumeration));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testGetImplementingTypeForGenerateDomainValidationStillThrowsForEnumNonClassloadingErrors() {
		Enumeration enumeration = mock(Enumeration.class);
		doThrow(new MetaDataException("Enum metadata error without classloading cause"))
				.when(enumeration).getImplementingType();

		assertThrows(MetaDataException.class,
				() -> BindUtil.getImplementingTypeForGenerateDomainValidation(enumeration));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testGetImplementingTypeForGenerateDomainValidationStillThrowsForNonEnums() {
		Attribute attribute = mock(Attribute.class);
		doThrow(new MetaDataException("Non-enum metadata error")).when(attribute).getImplementingType();

		assertThrows(MetaDataException.class,
				() -> BindUtil.getImplementingTypeForGenerateDomainValidation(attribute));
	}

	// ---- containsSkyveExpressions ----

	@Test
	@SuppressWarnings("static-method")
	public void containsSkyveExpressionsPlainTextReturnsFalse() {
		assertFalse(BindUtil.containsSkyveExpressions("no expressions here"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void containsSkyveExpressionsWithExpressionReturnsTrue() {
		assertTrue(BindUtil.containsSkyveExpressions("{name}"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void containsSkyveExpressionsEscapedBraceReturnsFalse() {
		assertFalse(BindUtil.containsSkyveExpressions("literal \\{value}"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void containsSkyveExpressionsMixedTextAndExpressionReturnsTrue() {
		assertTrue(BindUtil.containsSkyveExpressions("Hello {firstName} {lastName}"));
	}

	// ---- isSkyveExpression ----

	@Test
	@SuppressWarnings("static-method")
	public void isSkyveExpressionWrappedInBracesReturnsTrue() {
		assertTrue(BindUtil.isSkyveExpression("{name}"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void isSkyveExpressionNoLeadingBraceReturnsFalse() {
		assertFalse(BindUtil.isSkyveExpression("name}"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void isSkyveExpressionNoTrailingBraceReturnsFalse() {
		assertFalse(BindUtil.isSkyveExpression("{name"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void isSkyveExpressionSingleCharReturnsFalse() {
		assertFalse(BindUtil.isSkyveExpression("{"));
	}

	// ---- negateCondition ----

	@Test
	@SuppressWarnings("static-method")
	public void negateConditionNullReturnsNull() {
		assertThat(BindUtil.negateCondition(null), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void negateConditionTrueReturnsFalse() {
		assertThat(BindUtil.negateCondition("true"), is("false"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void negateConditionFalseReturnsTrue() {
		assertThat(BindUtil.negateCondition("false"), is("true"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void negateConditionNotPrefixStripsNot() {
		assertThat(BindUtil.negateCondition("notActive"), is("active"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void negateConditionSimpleNameAddsPrefixNot() {
		assertThat(BindUtil.negateCondition("active"), is("notActive"));
	}

	// ---- createCompoundBinding ----

	@Test
	@SuppressWarnings("static-method")
	public void createCompoundBindingTwoPartsJoinsWithDot() {
		assertThat(BindUtil.createCompoundBinding("a", "b"), is("a.b"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void createCompoundBindingThreePartsJoinsAllWithDot() {
		assertThat(BindUtil.createCompoundBinding("a", "b", "c"), is("a.b.c"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void createCompoundBindingSinglePartReturnsItself() {
		assertThat(BindUtil.createCompoundBinding("binding"), is("binding"));
	}

	// ---- createIndexedBinding ----

	@Test
	@SuppressWarnings("static-method")
	public void createIndexedBindingFormatsBindingWithIndex() {
		assertThat(BindUtil.createIndexedBinding("items", 0), is("items[0]"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void createIndexedBindingNonZeroIndex() {
		assertThat(BindUtil.createIndexedBinding("lines", 3), is("lines[3]"));
	}

	// ---- createIdBinding ----

	@Test
	@SuppressWarnings("static-method")
	public void createIdBindingFormatsCorrectly() {
		assertThat(BindUtil.createIdBinding("items", "abc123"), is("itemsElementById(abc123)"));
	}

	// ---- sanitiseBinding ----

	@Test
	@SuppressWarnings("static-method")
	public void sanitiseBindingNullReturnsNull() {
		assertThat(BindUtil.sanitiseBinding(null), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void sanitiseBindingReplacesDotWithUnderscore() {
		assertThat(BindUtil.sanitiseBinding("a.b"), is("a_b"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void sanitiseBindingReplacesSquareBracketsWithUnderscore() {
		assertThat(BindUtil.sanitiseBinding("items[0]"), is("items_0_"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void sanitiseBindingCompoundWithIndex() {
		assertThat(BindUtil.sanitiseBinding("a.items[2].b"), is("a_items_2__b"));
	}

	// ---- unsanitiseBinding ----

	@Test
	@SuppressWarnings("static-method")
	public void unsanitiseBindingNullReturnsNull() {
		assertThat(BindUtil.unsanitiseBinding(null), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void unsanitiseBindingRestoresDotFromUnderscore() {
		assertThat(BindUtil.unsanitiseBinding("a_b"), is("a.b"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void unsanitiseBindingRestoresIndexedBinding() {
		assertThat(BindUtil.unsanitiseBinding("items_0_"), is("items[0]"));
	}

	// ---- nullSafeConvert ----

	@Test
	@SuppressWarnings("static-method")
	public void nullSafeConvertIntegerAlreadyIntegerReturnsUnchanged() {
		Integer value = Integer.valueOf(42);
		Object result = BindUtil.nullSafeConvert(Integer.class, value);
		assertThat(result, is(value));
	}

	@Test
	@SuppressWarnings("static-method")
	public void nullSafeConvertLongToIntegerConverts() {
		Object result = BindUtil.nullSafeConvert(Integer.class, Long.valueOf(100L));
		assertEquals(Integer.valueOf(100), result);
	}

	@Test
	@SuppressWarnings("static-method")
	public void nullSafeConvertIntToLongConverts() {
		Object result = BindUtil.nullSafeConvert(Long.class, Integer.valueOf(5));
		assertEquals(Long.valueOf(5L), result);
	}

	@Test
	@SuppressWarnings("static-method")
	public void nullSafeConvertIntToBigDecimalConverts() {
		Object result = BindUtil.nullSafeConvert(BigDecimal.class, Integer.valueOf(7));
		assertTrue(result instanceof BigDecimal);
		assertEquals(new BigDecimal("7"), result);
	}

	@Test
	@SuppressWarnings("static-method")
	public void nullSafeConvertStringToDecimal2Converts() {
		Object result = BindUtil.nullSafeConvert(Decimal2.class, "1.23");
		assertEquals(0, new Decimal2("1.23").compareTo((Decimal2) result));
	}

	@Test
	@SuppressWarnings("static-method")
	public void nullSafeConvertStringToDecimal5Converts() {
		Object result = BindUtil.nullSafeConvert(Decimal5.class, "2.5");
		assertEquals(0, new Decimal5("2.5").compareTo((Decimal5) result));
	}

	@Test
	@SuppressWarnings("static-method")
	public void nullSafeConvertStringToDecimal10Converts() {
		Object result = BindUtil.nullSafeConvert(Decimal10.class, "9.99");
		assertEquals(0, new Decimal10("9.99").compareTo((Decimal10) result));
	}

	@Test
	@SuppressWarnings("static-method")
	public void nullSafeConvertDateToDateOnlyConverts() {
		Date date = new Date();
		Object result = BindUtil.nullSafeConvert(DateOnly.class, date);
		assertTrue(result instanceof DateOnly);
	}

	@Test
	@SuppressWarnings("static-method")
	public void nullSafeConvertDateToTimeOnlyConverts() {
		Date date = new Date();
		Object result = BindUtil.nullSafeConvert(TimeOnly.class, date);
		assertTrue(result instanceof TimeOnly);
	}

	@Test
	@SuppressWarnings("static-method")
	public void nullSafeConvertDateToDateTimeConverts() {
		Date date = new Date();
		Object result = BindUtil.nullSafeConvert(DateTime.class, date);
		assertTrue(result instanceof DateTime);
	}

	@Test
	@SuppressWarnings("static-method")
	public void nullSafeConvertDateToTimestampConverts() {
		Date date = new Date();
		Object result = BindUtil.nullSafeConvert(Timestamp.class, date);
		assertTrue(result instanceof Timestamp);
	}

	@Test
	@SuppressWarnings("static-method")
	public void nullSafeConvertAlreadyDecimal2ReturnsUnchanged() {
		Decimal2 value = new Decimal2("3.14");
		Object result = BindUtil.nullSafeConvert(Decimal2.class, value);
		assertThat(result, is(value));
	}

	@Test
	@SuppressWarnings("static-method")
	public void nullSafeConvertIntToFloatConverts() {
		Object result = BindUtil.nullSafeConvert(Float.class, Integer.valueOf(3));
		assertEquals(Float.valueOf(3f), result);
	}

	@Test
	@SuppressWarnings("static-method")
	public void nullSafeConvertIntToDoubleConverts() {
		Object result = BindUtil.nullSafeConvert(Double.class, Integer.valueOf(7));
		assertEquals(Double.valueOf(7d), result);
	}

	// ---- fromSerialised ----

	@Test
	@SuppressWarnings("static-method")
	public void fromSerialisedStringReturnsString() {
		Object result = BindUtil.fromSerialised(String.class, "hello");
		assertThat(result, is("hello"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void fromSerialisedIntegerReturnsInteger() {
		Object result = BindUtil.fromSerialised(Integer.class, "42");
		assertEquals(Integer.valueOf(42), result);
	}

	@Test
	@SuppressWarnings("static-method")
	public void fromSerialisedLongReturnsLong() {
		Object result = BindUtil.fromSerialised(Long.class, "9876543210");
		assertEquals(Long.valueOf(9876543210L), result);
	}

	@Test
	@SuppressWarnings("static-method")
	public void fromSerialisedFloatReturnsFloat() {
		Object result = BindUtil.fromSerialised(Float.class, "3.14");
		assertEquals(Float.valueOf(3.14f), result);
	}

	@Test
	@SuppressWarnings("static-method")
	public void fromSerialisedDoubleReturnsDouble() {
		Object result = BindUtil.fromSerialised(Double.class, "2.718");
		assertEquals(Double.valueOf(2.718d), result);
	}

	@Test
	@SuppressWarnings("static-method")
	public void fromSerialisedBigDecimalReturnsBigDecimal() {
		Object result = BindUtil.fromSerialised(BigDecimal.class, "1234.56");
		assertEquals(new BigDecimal("1234.56"), result);
	}

	@Test
	@SuppressWarnings("static-method")
	public void fromSerialisedDecimal2ReturnsDecimal2() {
		Object result = BindUtil.fromSerialised(Decimal2.class, "9.99");
		assertEquals(0, new Decimal2("9.99").compareTo((Decimal2) result));
	}

	@Test
	@SuppressWarnings("static-method")
	public void fromSerialisedDecimal5ReturnsDecimal5() {
		Object result = BindUtil.fromSerialised(Decimal5.class, "3.14159");
		assertEquals(0, new Decimal5("3.14159").compareTo((Decimal5) result));
	}

	@Test
	@SuppressWarnings("static-method")
	public void fromSerialisedDecimal10ReturnsDecimal10() {
		Object result = BindUtil.fromSerialised(Decimal10.class, "0.0000000001");
		assertEquals(0, new Decimal10("0.0000000001").compareTo((Decimal10) result));
	}

	@Test
	@SuppressWarnings("static-method")
	public void fromSerialisedBooleanTrueReturnsTrue() {
		Object result = BindUtil.fromSerialised(Boolean.class, "true");
		assertEquals(Boolean.TRUE, result);
	}

	@Test
	@SuppressWarnings("static-method")
	public void fromSerialisedBooleanFalseReturnsFalse() {
		Object result = BindUtil.fromSerialised(Boolean.class, "false");
		assertEquals(Boolean.FALSE, result);
	}

	@Test
	@SuppressWarnings("static-method")
	public void fromSerialisedDateOnlyReturnsDateOnly() {
		// DateOnly serialised format is yyyy-MM-dd
		Object result = BindUtil.fromSerialised(DateOnly.class, "2023-06-15");
		assertTrue(result instanceof DateOnly);
	}

	@Test
	@SuppressWarnings("static-method")
	public void fromSerialisedTimeOnlyReturnsTimeOnly() {
		// TimeOnly serialised format is HH:mm:ss
		Object result = BindUtil.fromSerialised(TimeOnly.class, "10:30:00");
		assertTrue(result instanceof TimeOnly);
	}

	// ---- isAScalarType ----

	@Test
	@SuppressWarnings("static-method")
	public void isAScalarTypeStringReturnsTrue() {
		assertTrue(BindUtil.isAScalarType(String.class));
	}

	@Test
	@SuppressWarnings("static-method")
	public void isAScalarTypeIntegerReturnsTrue() {
		assertTrue(BindUtil.isAScalarType(Integer.class));
	}

	@Test
	@SuppressWarnings("static-method")
	public void isAScalarTypePrimitiveIntReturnsTrue() {
		assertTrue(BindUtil.isAScalarType(int.class));
	}

	@Test
	@SuppressWarnings("static-method")
	public void isAScalarTypeListReturnsFalse() {
		assertFalse(BindUtil.isAScalarType(java.util.List.class));
	}

	// ---- isImplicit ----

	@Test
	@SuppressWarnings("static-method")
	public void isImplicitBizIdReturnsTrue() {
		assertTrue(BindUtil.isImplicit(Bean.DOCUMENT_ID));
	}

	@Test
	@SuppressWarnings("static-method")
	public void isImplicitBizKeyReturnsTrue() {
		assertTrue(BindUtil.isImplicit(Bean.BIZ_KEY));
	}

	@Test
	@SuppressWarnings("static-method")
	public void isImplicitCustomAttributeReturnsFalse() {
		assertFalse(BindUtil.isImplicit("myAttribute"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void isImplicitNullReturnsFalse() {
		assertFalse(BindUtil.isImplicit(null));
	}

	@Test
	@SuppressWarnings("static-method")
	public void isImplicitLockNameReturnsTrue() {
		assertTrue(BindUtil.isImplicit(PersistentBean.LOCK_NAME));
	}

	// ---- implicitAttributeType ----

	@Test
	@SuppressWarnings("static-method")
	public void implicitAttributeTypeBizIdReturnsString() {
		assertEquals(String.class, BindUtil.implicitAttributeType(Bean.DOCUMENT_ID));
	}

	@Test
	@SuppressWarnings("static-method")
	public void implicitAttributeTypeBizKeyReturnsString() {
		assertEquals(String.class, BindUtil.implicitAttributeType(Bean.BIZ_KEY));
	}

	@Test
	@SuppressWarnings("static-method")
	public void implicitAttributeTypeTaggedNameReturnsBoolean() {
		assertEquals(Boolean.class, BindUtil.implicitAttributeType(PersistentBean.TAGGED_NAME));
	}

	@Test
	@SuppressWarnings("static-method")
	public void implicitAttributeTypeVersionReturnsInteger() {
		assertEquals(Integer.class, BindUtil.implicitAttributeType(PersistentBean.VERSION_NAME));
	}

	@Test
	@SuppressWarnings("static-method")
	public void implicitAttributeTypeCustomAttributeReturnsNull() {
		assertThat(BindUtil.implicitAttributeType("notImplicit"), is(nullValue()));
	}

	// ---- toJavaPropertyName ----

	@Test
	@SuppressWarnings("static-method")
	public void toJavaPropertyNameStripsGetPrefix() {
		assertThat(BindUtil.toJavaPropertyName("getName"), is("name"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toJavaPropertyNameStripsSetPrefix() {
		assertThat(BindUtil.toJavaPropertyName("setName"), is("name"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toJavaPropertyNameStripsIsPrefix() {
		assertThat(BindUtil.toJavaPropertyName("isActive"), is("active"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toJavaPropertyNameNoKnownPrefixReturnsUnchanged() {
		assertThat(BindUtil.toJavaPropertyName("doSomething"), is("doSomething"));
	}

	// ---- toTitleCase ----

	@Test
	@SuppressWarnings("static-method")
	public void toTitleCaseCamelCaseSplitsWords() {
		assertThat(BindUtil.toTitleCase("firstName"), is("First Name"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toTitleCaseSingleWordCapitalisesFirst() {
		assertThat(BindUtil.toTitleCase("name"), is("Name"));
	}

	// ---- get(Map, String) --------------------------------------------------

	@Test
	@SuppressWarnings("static-method")
	public void getMapSimpleKeyReturnsValue() {
		java.util.Map<String, Object> map = new java.util.HashMap<>();
		map.put("name", "Alice");
		assertThat(BindUtil.get(map, "name"), is("Alice"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void getMapMissingKeyReturnsNull() {
		java.util.Map<String, Object> map = new java.util.HashMap<>();
		assertThat(BindUtil.get(map, "missing"), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void getMapSanitisedBindingLookup() {
		// sanitised key uses underscores for dots
		java.util.Map<String, Object> map = new java.util.HashMap<>();
		map.put("address_city", "Melbourne");
		assertThat(BindUtil.get(map, "address.city"), is("Melbourne"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void getMapNestedCompoundKeyReturnsValue() {
		java.util.Map<String, Object> inner = new java.util.HashMap<>();
		inner.put("city", "Sydney");
		java.util.Map<String, Object> outer = new java.util.HashMap<>();
		outer.put("address", inner);
		assertThat(BindUtil.get(outer, "address.city"), is("Sydney"));
	}

	// ---- prefixMessageExpressions ------------------------------------------

	@Test
	@SuppressWarnings("static-method")
	public void prefixMessageExpressionsNullReturnsNull() {
		assertThat(BindUtil.prefixMessageExpressions(null, "prefix"), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void prefixMessageExpressionsNoExpressionReturnsUnchanged() {
		String msg = "No expressions here";
		assertThat(BindUtil.prefixMessageExpressions(msg, "doc"), is(msg));
	}

	@Test
	@SuppressWarnings("static-method")
	public void prefixMessageExpressionsEscapedBraceNotPrefixed() {
		// escaped '{' should not be modified
		String msg = "Value \\{escaped}";
		String result = BindUtil.prefixMessageExpressions(msg, "doc");
		// escaped braces are left as-is (no prefix injected)
		assertThat(result, is(msg));
	}

	// ---- order -------------------------------------------------------------

	/** Simple POJO for order tests. */
	public static class NamedItem {
		private final String name;
		NamedItem(String name) { this.name = name; }
		public String getName() { return name; }
	}

	@Test
	@SuppressWarnings("static-method")
	public void orderNullListDoesNotThrow() {
		// should silently tolerate null list
		BindUtil.order(null, new org.skyve.impl.metadata.OrderingImpl("name", org.skyve.metadata.SortDirection.ascending));
	}

	@Test
	@SuppressWarnings("static-method")
	public void orderEmptyListDoesNotThrow() {
		java.util.List<Object> list = new java.util.ArrayList<>();
		BindUtil.order(list, new org.skyve.impl.metadata.OrderingImpl("name", org.skyve.metadata.SortDirection.ascending));
		assertTrue(list.isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void orderSortsByNameAscending() {
		NamedItem a = new NamedItem("zebra");
		NamedItem b = new NamedItem("apple");
		java.util.List<NamedItem> list = new java.util.ArrayList<>(java.util.Arrays.asList(a, b));
		BindUtil.order(list, new org.skyve.impl.metadata.OrderingImpl("name", org.skyve.metadata.SortDirection.ascending));
		assertEquals("apple", list.get(0).getName());
	}

	@Test
	@SuppressWarnings("static-method")
	public void orderAlreadySortedListUnchanged() {
		NamedItem a = new NamedItem("apple");
		NamedItem b = new NamedItem("zebra");
		java.util.List<NamedItem> list = new java.util.ArrayList<>(java.util.Arrays.asList(a, b));
		BindUtil.order(list, new org.skyve.impl.metadata.OrderingImpl("name", org.skyve.metadata.SortDirection.ascending));
		assertEquals("apple", list.get(0).getName());
	}
}
