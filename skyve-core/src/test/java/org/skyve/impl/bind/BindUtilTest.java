package org.skyve.impl.bind;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.function.Function;

import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.PersistentBean;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Condition;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Inverse;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.enumeration.DynamicEnumerationConverter;
import org.skyve.util.Binder.TargetMetaData;

import org.locationtech.jts.geom.Geometry;

class BindUtilTest {
	private static void withThreadLocalUser(User user, Runnable run) {
		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();
		try {
			run.run();
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	private static void clearPersistenceThreadLocal() {
		try {
			Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
			field.setAccessible(true);
			@SuppressWarnings("unchecked")
			ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
			threadLocal.remove();
		}
		catch (ReflectiveOperationException e) {
			throw new AssertionError(e);
		}
	}


	@Test
	@SuppressWarnings("static-method")
	void testToJavaInstanceIdentifierLeavesValidIndentifier() {
		// setup the test data
		String identifier = "validIdentifier";

		// call the method under test
		String result = BindUtil.toJavaInstanceIdentifier(identifier);

		// verify the result
		assertThat(result, is(identifier));
	}

	@Test
	@SuppressWarnings({"static-method", "java:S5976"})
	void testToJavaInstanceIdentifierRemovesInvalidCharacters() {
		// setup the test data
		String identifier = "E-Mail";

		// call the method under test
		String result = BindUtil.toJavaInstanceIdentifier(identifier);

		// verify the result
		assertThat(result, is("EMail"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testToJavaInstanceIdentifierRemovesWhitespace() {
		// setup the test data
		String identifier = "Whitespace Identifier";

		// call the method under test
		String result = BindUtil.toJavaInstanceIdentifier(identifier);

		// verify the result
		assertThat(result, is("whitespaceIdentifier"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testToJavaInstanceIdentifierPreservesTwoLeadingCapitals() {
		// setup the test data
		String identifier = "DOB";

		// call the method under test
		String result = BindUtil.toJavaInstanceIdentifier(identifier);

		// verify the result
		assertThat(result, is("DOB"));
	}

	@Test
	@SuppressWarnings({"static-method", "java:S5976"})
	void testToJavaTypeIdentifierRemovesInvalidCharacters() {
		// setup the test data
		String identifier = "E-Mail";

		// call the method under test
		String result = BindUtil.toJavaTypeIdentifier(identifier);

		// verify the result
		assertThat(result, is("EMail"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testToJavaTypeIdentifierRemovesWhitespace() {
		// setup the test data
		String identifier = "Whitespace Identifier";

		// call the method under test
		String result = BindUtil.toJavaTypeIdentifier(identifier);

		// verify the result
		assertThat(result, is("WhitespaceIdentifier"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testToJavaTypeIdentifierPreservesTwoLeadingCapitals() {
		// setup the test data
		String identifier = "DOB";

		// call the method under test
		String result = BindUtil.toJavaTypeIdentifier(identifier);

		// verify the result
		assertThat(result, is("DOB"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testToJavaStaticIdentifierRemovesWhitespace() {
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
	void formatMessageAppliesPostProcessorToImplicitExpression() {
		Bean bean = mock(Bean.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		doReturn(customer).when(user).getCustomer();
		doReturn("alice").when(user).getName();

		withThreadLocalUser(user, () -> {
			Function<String, String> upperCase = value -> value.toUpperCase(Locale.ROOT);
			String result = BindUtil.formatMessage("Hello {USER}", upperCase, bean);
			assertEquals("Hello ALICE", result);
		});
		}

		@Test
		@SuppressWarnings("static-method")
		void formatMessageLeavesEscapedBracesAsLiterals() {
			Bean bean = mock(Bean.class);
			String result = BindUtil.formatMessage("literal \\{name\\}", bean);

			assertEquals("literal {name}", result);
		}

		@Test
		@SuppressWarnings("static-method")
		void formatMessageThrowsForUnmatchedOpeningBrace() {
			Bean bean = mock(Bean.class);
			MetaDataException exception = assertThrows(MetaDataException.class,
					() -> BindUtil.formatMessage("Hello {name", bean));

			assertTrue(exception.getMessage().contains("matching '}'"));
		}

		@Test
		@SuppressWarnings("static-method")
		void formatMessageThrowsWhenNoBeanCanEvaluateExpression() {
			Bean firstBean = mock(Bean.class);
			Bean secondBean = mock(Bean.class);
			doReturn("FirstDoc").when(firstBean).getBizDocument();
			doReturn("SecondDoc").when(secondBean).getBizDocument();

			MetaDataException exception = assertThrows(MetaDataException.class,
					() -> BindUtil.formatMessage("Hello {unknown:abc}", firstBean, secondBean));

			assertTrue(exception.getMessage().contains("Expression {unknown:abc} cannot be evaluated against beans"));
			assertTrue(exception.getMessage().contains("FirstDoc"));
			assertTrue(exception.getMessage().contains("SecondDoc"));
		}

		@Test
		@SuppressWarnings("static-method")
		void formatMessageThrowsWithoutCauseWhenNoBeansSupplied() {
			MetaDataException exception = assertThrows(MetaDataException.class,
					() -> BindUtil.formatMessage("Hello {unknown:abc}"));

			assertTrue(exception.getMessage().contains("Expression {unknown:abc} cannot be evaluated against bean"));
			assertNull(exception.getCause());
		}

		@Test
		@SuppressWarnings("static-method")
		void validateMessageExpressionsReturnsNullWhenValidationSucceeds() {
			Customer customer = mock(Customer.class);
			Document document = mock(Document.class);
			Module module = mock(Module.class);
			doReturn("admin").when(document).getOwningModuleName();
			doReturn(module).when(customer).getModule("admin");

			assertNull(BindUtil.validateMessageExpressions("Hello {USER}", customer, document));
		}

		@Test
		@SuppressWarnings("static-method")
		void validateMessageExpressionsReturnsNullWhenNoExpressionsPresent() {
			Customer customer = mock(Customer.class);
			Document document = mock(Document.class);

			assertNull(BindUtil.validateMessageExpressions("Hello world", customer, document));
		}

		@Test
		@SuppressWarnings("static-method")
		void validateMessageExpressionsAggregatesDocumentErrors() {
			Customer customer = mock(Customer.class);
			Document firstDocument = mock(Document.class);
			Document secondDocument = mock(Document.class);
			Module firstModule = mock(Module.class);
			Module secondModule = mock(Module.class);
			doReturn("admin").when(firstDocument).getOwningModuleName();
			doReturn("crm").when(secondDocument).getOwningModuleName();
			doReturn(firstModule).when(customer).getModule("admin");
			doReturn(secondModule).when(customer).getModule("crm");

			String error = BindUtil.validateMessageExpressions("Hello {unknown:abc}", customer, firstDocument, secondDocument);

			assertEquals("Cannot find an expression evaluator for prefix unknown. Cannot find an expression evaluator for prefix unknown",
					error);
		}

		@Test
		@SuppressWarnings("static-method")
		void validateMessageExpressionsReturnsMissingBraceError() {
			Customer customer = mock(Customer.class);
			Document document = mock(Document.class);

			assertEquals("Opening '{' with no closing '}'",
					BindUtil.validateMessageExpressions("Hello {name", customer, document));
		}

		@Test
		@SuppressWarnings("static-method")
		void validateMessageExpressionsIgnoresEscapedBraces() {
			Customer customer = mock(Customer.class);
			Document document = mock(Document.class);

			assertNull(BindUtil.validateMessageExpressions("Hello \\{name\\}", customer, document));
		}

		@Test
		@SuppressWarnings("static-method")
	void testToJavaStaticIdentifierPreservesTwoLeadingCapitals() {
		// setup the test data
		String identifier = "DOB";

		// call the method under test
		String result = BindUtil.toJavaStaticIdentifier(identifier);

		// verify the result
		assertThat(result, is("DOB"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetImplementingTypeForGenerateDomainValidationReturnsResolvedType() {
		Attribute attribute = mock(Attribute.class);
		doReturn(String.class).when(attribute).getImplementingType();

		assertEquals(String.class, BindUtil.getImplementingTypeForGenerateDomainValidation(attribute));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetImplementingTypeForGenerateDomainValidationFallsBackToEnumClass() {
		Enumeration enumeration = mock(Enumeration.class);
		doThrow(new MetaDataException("Enum class is not generated yet",
				new ClassNotFoundException("Enum class not found"))).when(enumeration).getImplementingType();

		assertEquals(Enum.class, BindUtil.getImplementingTypeForGenerateDomainValidation(enumeration));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetImplementingTypeForGenerateDomainValidationStillThrowsForEnumNonClassloadingErrors() {
		Enumeration enumeration = mock(Enumeration.class);
		doThrow(new MetaDataException("Enum metadata error without classloading cause"))
				.when(enumeration).getImplementingType();

		assertThrows(MetaDataException.class,
				() -> BindUtil.getImplementingTypeForGenerateDomainValidation(enumeration));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetImplementingTypeForGenerateDomainValidationStillThrowsForNonEnums() {
		Attribute attribute = mock(Attribute.class);
		doThrow(new MetaDataException("Non-enum metadata error")).when(attribute).getImplementingType();

		assertThrows(MetaDataException.class,
				() -> BindUtil.getImplementingTypeForGenerateDomainValidation(attribute));
	}

	// ---- containsSkyveExpressions ----

	@Test
	@SuppressWarnings("static-method")
	void containsSkyveExpressionsPlainTextReturnsFalse() {
		assertFalse(BindUtil.containsSkyveExpressions("no expressions here"));
	}

	@Test
	@SuppressWarnings("static-method")
	void containsSkyveExpressionsWithExpressionReturnsTrue() {
		assertTrue(BindUtil.containsSkyveExpressions("{name}"));
	}

	@Test
	@SuppressWarnings("static-method")
	void containsSkyveExpressionsEscapedBraceReturnsFalse() {
		assertFalse(BindUtil.containsSkyveExpressions("literal \\{value}"));
	}

	@Test
	@SuppressWarnings("static-method")
	void containsSkyveExpressionsMixedTextAndExpressionReturnsTrue() {
		assertTrue(BindUtil.containsSkyveExpressions("Hello {firstName} {lastName}"));
	}

	// ---- isSkyveExpression ----

	@Test
	@SuppressWarnings("static-method")
	void isSkyveExpressionWrappedInBracesReturnsTrue() {
		assertTrue(BindUtil.isSkyveExpression("{name}"));
	}

	@Test
	@SuppressWarnings("static-method")
	void isSkyveExpressionNoLeadingBraceReturnsFalse() {
		assertFalse(BindUtil.isSkyveExpression("name}"));
	}

	@Test
	@SuppressWarnings("static-method")
	void isSkyveExpressionNoTrailingBraceReturnsFalse() {
		assertFalse(BindUtil.isSkyveExpression("{name"));
	}

	@Test
	@SuppressWarnings("static-method")
	void isSkyveExpressionSingleCharReturnsFalse() {
		assertFalse(BindUtil.isSkyveExpression("{"));
	}

	// ---- negateCondition ----

	@Test
	@SuppressWarnings("static-method")
	void negateConditionNullReturnsNull() {
		assertThat(BindUtil.negateCondition(null), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void negateConditionTrueReturnsFalse() {
		assertThat(BindUtil.negateCondition("true"), is("false"));
	}

	@Test
	@SuppressWarnings("static-method")
	void negateConditionFalseReturnsTrue() {
		assertThat(BindUtil.negateCondition("false"), is("true"));
	}

	@Test
	@SuppressWarnings("static-method")
	void negateConditionNotPrefixStripsNot() {
		assertThat(BindUtil.negateCondition("notActive"), is("active"));
	}

	@Test
	@SuppressWarnings("static-method")
	void negateConditionSimpleNameAddsPrefixNot() {
		assertThat(BindUtil.negateCondition("active"), is("notActive"));
	}

	// ---- createCompoundBinding ----

	@Test
	@SuppressWarnings("static-method")
	void createCompoundBindingTwoPartsJoinsWithDot() {
		assertThat(BindUtil.createCompoundBinding("a", "b"), is("a.b"));
	}

	@Test
	@SuppressWarnings("static-method")
	void createCompoundBindingThreePartsJoinsAllWithDot() {
		assertThat(BindUtil.createCompoundBinding("a", "b", "c"), is("a.b.c"));
	}

	@Test
	@SuppressWarnings("static-method")
	void createCompoundBindingSinglePartReturnsItself() {
		assertThat(BindUtil.createCompoundBinding("binding"), is("binding"));
	}

	// ---- createIndexedBinding ----

	@Test
	@SuppressWarnings("static-method")
	void createIndexedBindingFormatsBindingWithIndex() {
		assertThat(BindUtil.createIndexedBinding("items", 0), is("items[0]"));
	}

	@Test
	@SuppressWarnings("static-method")
	void createIndexedBindingNonZeroIndex() {
		assertThat(BindUtil.createIndexedBinding("lines", 3), is("lines[3]"));
	}

	// ---- createIdBinding ----

	@Test
	@SuppressWarnings("static-method")
	void createIdBindingFormatsCorrectly() {
		assertThat(BindUtil.createIdBinding("items", "abc123"), is("itemsElementById(abc123)"));
	}

	// ---- sanitiseBinding ----

	@Test
	@SuppressWarnings("static-method")
	void sanitiseBindingNullReturnsNull() {
		assertThat(BindUtil.sanitiseBinding(null), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void sanitiseBindingReplacesDotWithUnderscore() {
		assertThat(BindUtil.sanitiseBinding("a.b"), is("a_b"));
	}

	@Test
	@SuppressWarnings("static-method")
	void sanitiseBindingReplacesSquareBracketsWithUnderscore() {
		assertThat(BindUtil.sanitiseBinding("items[0]"), is("items_0_"));
	}

	@Test
	@SuppressWarnings("static-method")
	void sanitiseBindingCompoundWithIndex() {
		assertThat(BindUtil.sanitiseBinding("a.items[2].b"), is("a_items_2__b"));
	}

	// ---- unsanitiseBinding ----

	@Test
	@SuppressWarnings("static-method")
	void unsanitiseBindingNullReturnsNull() {
		assertThat(BindUtil.unsanitiseBinding(null), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void unsanitiseBindingRestoresDotFromUnderscore() {
		assertThat(BindUtil.unsanitiseBinding("a_b"), is("a.b"));
	}

	@Test
	@SuppressWarnings("static-method")
	void unsanitiseBindingRestoresIndexedBinding() {
		assertThat(BindUtil.unsanitiseBinding("items_0_"), is("items[0]"));
	}

	// ---- nullSafeConvert ----

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertIntegerAlreadyIntegerReturnsUnchanged() {
		Integer value = Integer.valueOf(42);
		Object result = BindUtil.nullSafeConvert(Integer.class, value);
		assertThat(result, is(value));
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertLongToIntegerConverts() {
		Object result = BindUtil.nullSafeConvert(Integer.class, Long.valueOf(100L));
		assertEquals(Integer.valueOf(100), result);
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertIntToLongConverts() {
		Object result = BindUtil.nullSafeConvert(Long.class, Integer.valueOf(5));
		assertEquals(Long.valueOf(5L), result);
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertIntToBigDecimalConverts() {
		Object result = BindUtil.nullSafeConvert(BigDecimal.class, Integer.valueOf(7));
		assertTrue(result instanceof BigDecimal);
		assertEquals(new BigDecimal("7"), result);
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertStringToDecimal2Converts() {
		Object result = BindUtil.nullSafeConvert(Decimal2.class, "1.23");
		assertEquals(0, new Decimal2("1.23").compareTo((Decimal2) result));
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertStringToDecimal5Converts() {
		Object result = BindUtil.nullSafeConvert(Decimal5.class, "2.5");
		assertEquals(0, new Decimal5("2.5").compareTo((Decimal5) result));
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertStringToDecimal10Converts() {
		Object result = BindUtil.nullSafeConvert(Decimal10.class, "9.99");
		assertEquals(0, new Decimal10("9.99").compareTo((Decimal10) result));
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertDateToDateOnlyConverts() {
		Date date = new Date();
		Object result = BindUtil.nullSafeConvert(DateOnly.class, date);
		assertTrue(result instanceof DateOnly);
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertDateToTimeOnlyConverts() {
		Date date = new Date();
		Object result = BindUtil.nullSafeConvert(TimeOnly.class, date);
		assertTrue(result instanceof TimeOnly);
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertDateToDateTimeConverts() {
		Date date = new Date();
		Object result = BindUtil.nullSafeConvert(DateTime.class, date);
		assertTrue(result instanceof DateTime);
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertDateToTimestampConverts() {
		Date date = new Date();
		Object result = BindUtil.nullSafeConvert(Timestamp.class, date);
		assertTrue(result instanceof Timestamp);
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertAlreadyDecimal2ReturnsUnchanged() {
		Decimal2 value = new Decimal2("3.14");
		Object result = BindUtil.nullSafeConvert(Decimal2.class, value);
		assertThat(result, is(value));
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertIntToFloatConverts() {
		Object result = BindUtil.nullSafeConvert(Float.class, Integer.valueOf(3));
		assertEquals(Float.valueOf(3f), result);
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertIntToDoubleConverts() {
		Object result = BindUtil.nullSafeConvert(Double.class, Integer.valueOf(7));
		assertEquals(Double.valueOf(7d), result);
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertIntToShortConverts() {
		Object result = BindUtil.nullSafeConvert(Short.class, Integer.valueOf(9));
		assertEquals(Short.valueOf((short) 9), result);
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertAlreadyShortReturnsUnchanged() {
		Short value = Short.valueOf((short) 5);
		Object result = BindUtil.nullSafeConvert(Short.class, value);
		assertEquals(value, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertWktStringToGeometryConverts() {
		Object result = BindUtil.nullSafeConvert(org.locationtech.jts.geom.Geometry.class, "POINT (1 2)");
		assertNotNull(result);
		assertTrue(result instanceof org.locationtech.jts.geom.Geometry);
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertJavaEnumFromStringConverts() {
		Object result = BindUtil.nullSafeConvert(TestBindEnum.class, "VALUE_A");
		assertEquals(TestBindEnum.VALUE_A, result);
	}

	// ---- fromSerialised ----

	@Test
	@SuppressWarnings("static-method")
	void fromSerialisedStringReturnsString() {
		Object result = BindUtil.fromSerialised(String.class, "hello");
		assertThat(result, is("hello"));
	}

	@Test
	@SuppressWarnings("static-method")
	void fromSerialisedIntegerReturnsInteger() {
		Object result = BindUtil.fromSerialised(Integer.class, "42");
		assertEquals(Integer.valueOf(42), result);
	}

	@Test
	@SuppressWarnings("static-method")
	void fromSerialisedLongReturnsLong() {
		Object result = BindUtil.fromSerialised(Long.class, "9876543210");
		assertEquals(Long.valueOf(9876543210L), result);
	}

	@Test
	@SuppressWarnings("static-method")
	void fromSerialisedFloatReturnsFloat() {
		Object result = BindUtil.fromSerialised(Float.class, "3.14");
		assertEquals(Float.valueOf(3.14f), result);
	}

	@Test
	@SuppressWarnings("static-method")
	void fromSerialisedDoubleReturnsDouble() {
		Object result = BindUtil.fromSerialised(Double.class, "2.718");
		assertEquals(Double.valueOf(2.718d), result);
	}

	@Test
	@SuppressWarnings("static-method")
	void fromSerialisedBigDecimalReturnsBigDecimal() {
		Object result = BindUtil.fromSerialised(BigDecimal.class, "1234.56");
		assertEquals(new BigDecimal("1234.56"), result);
	}

	@Test
	@SuppressWarnings("static-method")
	void fromSerialisedDecimal2ReturnsDecimal2() {
		Object result = BindUtil.fromSerialised(Decimal2.class, "9.99");
		assertEquals(0, new Decimal2("9.99").compareTo((Decimal2) result));
	}

	@Test
	@SuppressWarnings("static-method")
	void fromSerialisedDecimal5ReturnsDecimal5() {
		Object result = BindUtil.fromSerialised(Decimal5.class, "3.14159");
		assertEquals(0, new Decimal5("3.14159").compareTo((Decimal5) result));
	}

	@Test
	@SuppressWarnings("static-method")
	void fromSerialisedDecimal10ReturnsDecimal10() {
		Object result = BindUtil.fromSerialised(Decimal10.class, "0.0000000001");
		assertEquals(0, new Decimal10("0.0000000001").compareTo((Decimal10) result));
	}

	@Test
	@SuppressWarnings("static-method")
	void fromSerialisedBooleanTrueReturnsTrue() {
		Object result = BindUtil.fromSerialised(Boolean.class, "true");
		assertEquals(Boolean.TRUE, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void fromSerialisedBooleanFalseReturnsFalse() {
		Object result = BindUtil.fromSerialised(Boolean.class, "false");
		assertEquals(Boolean.FALSE, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void fromSerialisedGeometryWktReturnsGeometry() {
		Object result = BindUtil.fromSerialised(org.locationtech.jts.geom.Geometry.class, "POINT (3 4)");
		assertNotNull(result);
		assertTrue(result instanceof org.locationtech.jts.geom.Geometry);
	}

	@Test
	@SuppressWarnings("static-method")
	void fromSerialisedJavaEnumReturnsEnum() {
		Object result = BindUtil.fromSerialised(TestBindEnum.class, "VALUE_B");
		assertEquals(TestBindEnum.VALUE_B, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void fromSerialisedDateOnlyReturnsDateOnly() {
		// DateOnly serialised format is yyyy-MM-dd
		Object result = BindUtil.fromSerialised(DateOnly.class, "2023-06-15");
		assertTrue(result instanceof DateOnly);
	}

	@Test
	@SuppressWarnings("static-method")
	void fromSerialisedTimeOnlyReturnsTimeOnly() {
		// TimeOnly serialised format is HH:mm:ss
		Object result = BindUtil.fromSerialised(TimeOnly.class, "10:30:00");
		assertTrue(result instanceof TimeOnly);
	}

	@Test
	@SuppressWarnings("static-method")
	void fromSerialisedOptimisticLockReturnsOptimisticLock() {
		// Format: 17-char timestamp (yyyyMMddHHmmssSSS) followed by username
		Object result = BindUtil.fromSerialised(OptimisticLock.class, "20230101000000000admin");
		assertTrue(result instanceof OptimisticLock);
		assertEquals("admin", ((OptimisticLock) result).getUsername());
	}

	// ---- fromString date-type throw paths when customer is null and not serialized ----

	@Test
	@SuppressWarnings("static-method")
	void fromStringDateOnlyWithNullCustomerThrowsDomainException() {
		// fromSerializedFormat=false and customer=null → IllegalStateException wrapped in DomainException
		assertThrows(DomainException.class, () -> BindUtil.fromString(null, null, DateOnly.class, "2022-01-01"));
	}

	@Test
	@SuppressWarnings("static-method")
	void fromStringTimeOnlyWithNullCustomerThrowsDomainException() {
		assertThrows(DomainException.class, () -> BindUtil.fromString(null, null, TimeOnly.class, "12:00:00"));
	}

	@Test
	@SuppressWarnings("static-method")
	void fromStringDateTimeWithNullCustomerThrowsDomainException() {
		assertThrows(DomainException.class, () -> BindUtil.fromString(null, null, DateTime.class, "2022-01-01"));
	}

	@Test
	@SuppressWarnings("static-method")
	void fromStringTimestampWithNullCustomerThrowsDomainException() {
		assertThrows(DomainException.class, () -> BindUtil.fromString(null, null, Timestamp.class, "2022-01-01"));
	}

	@Test
	@SuppressWarnings("static-method")
	void fromSerialisedUnknownTypeThrowsDomainException() {
		// Object.class is not handled → IllegalStateException wrapped in DomainException
		assertThrows(DomainException.class, () -> BindUtil.fromSerialised(Object.class, "value"));
	}

	// ---- toDisplay — Boolean and arbitrary-object paths ----

	@Test
	@SuppressWarnings("static-method")
	void toDisplayBooleanFalseReturnsNo() {
		// Covers the false branch of (bool.booleanValue() ? "Yes" : "No")
		String result = BindUtil.toDisplay(mock(Customer.class), null, null, null, Boolean.FALSE);
		assertEquals("No", result);
	}

	@Test
	@SuppressWarnings("static-method")
	void toDisplayNullValueReturnsEmptyString() {
		String result = BindUtil.toDisplay(mock(Customer.class), null, null, null, null);
		assertEquals("", result);
	}

	// ---- toJavaInstanceIdentifier — leading digit replacement ----

	@Test
	@SuppressWarnings("static-method")
	void toJavaInstanceIdentifierLeadingDigitsAreReplacedWithWords() {
		assertEquals("zeroTest", BindUtil.toJavaInstanceIdentifier("0Test"));
		assertEquals("oneTest", BindUtil.toJavaInstanceIdentifier("1Test"));
		assertEquals("twoTest", BindUtil.toJavaInstanceIdentifier("2Test"));
		assertEquals("threeTest", BindUtil.toJavaInstanceIdentifier("3Test"));
		assertEquals("fourTest", BindUtil.toJavaInstanceIdentifier("4Test"));
		assertEquals("fiveTest", BindUtil.toJavaInstanceIdentifier("5Test"));
		assertEquals("sixTest", BindUtil.toJavaInstanceIdentifier("6Test"));
		assertEquals("sevenTest", BindUtil.toJavaInstanceIdentifier("7Test"));
		assertEquals("eightTest", BindUtil.toJavaInstanceIdentifier("8Test"));
		assertEquals("nineTest", BindUtil.toJavaInstanceIdentifier("9Test"));
	}

	@Test
	@SuppressWarnings("static-method")
	void toJavaInstanceIdentifierLeadingInvalidCharIsRemoved() {
		// A non-letter, non-digit first character is deleted via sb.deleteCharAt(0)
		assertEquals("hello", BindUtil.toJavaInstanceIdentifier("!hello"));
	}

	// ---- isAScalarType ----

	@Test
	@SuppressWarnings("static-method")
	void isAScalarTypeStringReturnsTrue() {
		assertTrue(BindUtil.isAScalarType(String.class));
	}

	@Test
	@SuppressWarnings("static-method")
	void isAScalarTypeIntegerReturnsTrue() {
		assertTrue(BindUtil.isAScalarType(Integer.class));
	}

	@Test
	@SuppressWarnings("static-method")
	void isAScalarTypePrimitiveIntReturnsTrue() {
		assertTrue(BindUtil.isAScalarType(int.class));
	}

	@Test
	@SuppressWarnings("static-method")
	void isAScalarTypeListReturnsFalse() {
		assertFalse(BindUtil.isAScalarType(java.util.List.class));
	}

	// ---- isImplicit ----

	@Test
	@SuppressWarnings("static-method")
	void isImplicitBizIdReturnsTrue() {
		assertTrue(BindUtil.isImplicit(Bean.DOCUMENT_ID));
	}

	@Test
	@SuppressWarnings("static-method")
	void isImplicitBizKeyReturnsTrue() {
		assertTrue(BindUtil.isImplicit(Bean.BIZ_KEY));
	}

	@Test
	@SuppressWarnings("static-method")
	void isImplicitCustomAttributeReturnsFalse() {
		assertFalse(BindUtil.isImplicit("myAttribute"));
	}

	@Test
	@SuppressWarnings("static-method")
	void isImplicitNullReturnsFalse() {
		assertFalse(BindUtil.isImplicit(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void isImplicitLockNameReturnsTrue() {
		assertTrue(BindUtil.isImplicit(PersistentBean.LOCK_NAME));
	}

	// ---- implicitAttributeType ----

	@Test
	@SuppressWarnings("static-method")
	void implicitAttributeTypeBizIdReturnsString() {
		assertEquals(String.class, BindUtil.implicitAttributeType(Bean.DOCUMENT_ID));
	}

	@Test
	@SuppressWarnings("static-method")
	void implicitAttributeTypeBizKeyReturnsString() {
		assertEquals(String.class, BindUtil.implicitAttributeType(Bean.BIZ_KEY));
	}

	@Test
	@SuppressWarnings("static-method")
	void implicitAttributeTypeTaggedNameReturnsBoolean() {
		assertEquals(Boolean.class, BindUtil.implicitAttributeType(PersistentBean.TAGGED_NAME));
	}

	@Test
	@SuppressWarnings("static-method")
	void implicitAttributeTypeVersionReturnsInteger() {
		assertEquals(Integer.class, BindUtil.implicitAttributeType(PersistentBean.VERSION_NAME));
	}

	@Test
	@SuppressWarnings("static-method")
	void implicitAttributeTypeCustomAttributeReturnsNull() {
		assertThat(BindUtil.implicitAttributeType("notImplicit"), is(nullValue()));
	}

	// ---- toJavaPropertyName ----

	@Test
	@SuppressWarnings("static-method")
	void toJavaPropertyNameStripsGetPrefix() {
		assertThat(BindUtil.toJavaPropertyName("getName"), is("name"));
	}

	@Test
	@SuppressWarnings("static-method")
	void toJavaPropertyNameStripsSetPrefix() {
		assertThat(BindUtil.toJavaPropertyName("setName"), is("name"));
	}

	@Test
	@SuppressWarnings("static-method")
	void toJavaPropertyNameStripsIsPrefix() {
		assertThat(BindUtil.toJavaPropertyName("isActive"), is("active"));
	}

	@Test
	@SuppressWarnings("static-method")
	void toJavaPropertyNameNoKnownPrefixReturnsUnchanged() {
		assertThat(BindUtil.toJavaPropertyName("doSomething"), is("doSomething"));
	}

	// ---- toTitleCase ----

	@Test
	@SuppressWarnings("static-method")
	void toTitleCaseCamelCaseSplitsWords() {
		assertThat(BindUtil.toTitleCase("firstName"), is("First Name"));
	}

	@Test
	@SuppressWarnings("static-method")
	void toTitleCaseSingleWordCapitalisesFirst() {
		assertThat(BindUtil.toTitleCase("name"), is("Name"));
	}

	// ---- get(Map, String) --------------------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void getMapSimpleKeyReturnsValue() {
		java.util.Map<String, Object> map = new java.util.HashMap<>();
		map.put("name", "Alice");
		assertThat(BindUtil.get(map, "name"), is("Alice"));
	}

	@Test
	@SuppressWarnings("static-method")
	void getMapMissingKeyReturnsNull() {
		java.util.Map<String, Object> map = new java.util.HashMap<>();
		assertThat(BindUtil.get(map, "missing"), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void getMapSanitisedBindingLookup() {
		// sanitised key uses underscores for dots
		java.util.Map<String, Object> map = new java.util.HashMap<>();
		map.put("address_city", "Melbourne");
		assertThat(BindUtil.get(map, "address.city"), is("Melbourne"));
	}

	@Test
	@SuppressWarnings("static-method")
	void getMapNestedCompoundKeyReturnsValue() {
		java.util.Map<String, Object> inner = new java.util.HashMap<>();
		inner.put("city", "Sydney");
		java.util.Map<String, Object> outer = new java.util.HashMap<>();
		outer.put("address", inner);
		assertThat(BindUtil.get(outer, "address.city"), is("Sydney"));
	}

	// ---- prefixMessageExpressions ------------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void prefixMessageExpressionsNullReturnsNull() {
		assertThat(BindUtil.prefixMessageExpressions(null, "prefix"), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void prefixMessageExpressionsNoExpressionReturnsUnchanged() {
		String msg = "No expressions here";
		assertThat(BindUtil.prefixMessageExpressions(msg, "doc"), is(msg));
	}

	@Test
	@SuppressWarnings("static-method")
	void prefixMessageExpressionsEscapedBraceNotPrefixed() {
		// escaped '{' should not be modified
		String msg = "Value \\{escaped}";
		String result = BindUtil.prefixMessageExpressions(msg, "doc");
		// escaped braces are left as-is (no prefix injected)
		assertThat(result, is(msg));
	}

	@Test
	@SuppressWarnings("static-method")
	void prefixMessageExpressionsPrefixesBindingExpression() {
		String msg = "Hello {name}";
		assertThat(BindUtil.prefixMessageExpressions(msg, "doc"), is("Hello {doc.name}"));
	}

	@Test
	@SuppressWarnings("static-method")
	void prefixMessageExpressionsLeavesImplicitExpressionUnchanged() {
		String msg = "Hello {USER}";
		assertThat(BindUtil.prefixMessageExpressions(msg, "doc"), is("Hello {USER}"));
	}

	@Test
	@SuppressWarnings("static-method")
	void prefixMessageExpressionsThrowsForMissingClosingBrace() {
		MetaDataException exception = assertThrows(MetaDataException.class,
				() -> BindUtil.prefixMessageExpressions("Hello {name", "doc"));

		assertTrue(exception.getMessage().contains("unescaped opening '{' with no closing '}'"));
	}

	@Test
	@SuppressWarnings("static-method")
	void prefixMessageExpressionsThrowsForEmptyExpression() {
		MetaDataException exception = assertThrows(MetaDataException.class,
				() -> BindUtil.prefixMessageExpressions("Hello {}", "doc"));

		assertTrue(exception.getMessage().contains("is empty"));
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
	void orderNullListDoesNotThrow() {
		// should silently tolerate null list
		assertDoesNotThrow(() -> BindUtil.order(null, new org.skyve.impl.metadata.OrderingImpl("name", org.skyve.metadata.SortDirection.ascending)));
	}

	@Test
	@SuppressWarnings("static-method")
	void orderEmptyListDoesNotThrow() {
		java.util.List<Object> list = new java.util.ArrayList<>();
		BindUtil.order(list, new org.skyve.impl.metadata.OrderingImpl("name", org.skyve.metadata.SortDirection.ascending));
		assertTrue(list.isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void orderSortsByNameAscending() {
		NamedItem a = new NamedItem("zebra");
		NamedItem b = new NamedItem("apple");
		java.util.List<NamedItem> list = new java.util.ArrayList<>(java.util.Arrays.asList(a, b));
		BindUtil.order(list, new org.skyve.impl.metadata.OrderingImpl("name", org.skyve.metadata.SortDirection.ascending));
		assertEquals("apple", list.get(0).getName());
	}

	@Test
	@SuppressWarnings("static-method")
	void orderAlreadySortedListUnchanged() {
		NamedItem a = new NamedItem("apple");
		NamedItem b = new NamedItem("zebra");
		java.util.List<NamedItem> list = new java.util.ArrayList<>(java.util.Arrays.asList(a, b));
		BindUtil.order(list, new org.skyve.impl.metadata.OrderingImpl("name", org.skyve.metadata.SortDirection.ascending));
		assertEquals("apple", list.get(0).getName());
	}

	// ---- fromSerialised (DateTime, Timestamp) --------------------------------

	@Test
	@SuppressWarnings("static-method")
	void fromSerialisedDateTimeReturnsDateTime() {
		Object result = BindUtil.fromSerialised(DateTime.class, "2023-06-15T10:30:00.000+00:00");
		assertTrue(result instanceof DateTime);
	}

	@Test
	@SuppressWarnings("static-method")
	void fromSerialisedTimestampReturnsTimestamp() {
		Object result = BindUtil.fromSerialised(Timestamp.class, "2023-06-15T10:30:00.000+00:00");
		assertTrue(result instanceof Timestamp);
	}

	// ---- convert ------------------------------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void convertNullValueReturnsNull() {
		assertNull(BindUtil.convert(Integer.class, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertNonNullValueDelegatesToNullSafeConvert() {
		// Long → Integer coercion
		Object result = BindUtil.convert(Integer.class, Long.valueOf(42L));
		assertEquals(Integer.valueOf(42), result);
	}

	// ---- getElementInCollection(List, String) --------------------------------

	@Test
	@SuppressWarnings("static-method")
	void getElementInCollectionFindsMatchingBean() {
		Map<String, Object> p = new HashMap<>();
		p.put(Bean.DOCUMENT_ID, "bean-1");
		DynamicBean bean = new DynamicBean("mod", "Doc", p);
		List<DynamicBean> list = new ArrayList<>(Arrays.asList(bean));
		DynamicBean result = BindUtil.getElementInCollection(list, "bean-1");
		assertEquals(bean, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void getElementInCollectionReturnsNullWhenNotFound() {
		Map<String, Object> p = new HashMap<>();
		p.put(Bean.DOCUMENT_ID, "bean-1");
		DynamicBean bean = new DynamicBean("mod", "Doc", p);
		List<DynamicBean> list = new ArrayList<>(Arrays.asList(bean));
		assertNull(BindUtil.getElementInCollection(list, "no-such-id"));
	}

	// ---- setElementInCollection(List, T) ------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void setElementInCollectionUpdatesMatchingElement() {
		Map<String, Object> p1 = new HashMap<>();
		p1.put(Bean.DOCUMENT_ID, "bean-1");
		DynamicBean original = new DynamicBean("mod", "Doc", p1);

		Map<String, Object> p2 = new HashMap<>();
		p2.put(Bean.DOCUMENT_ID, "bean-1");
		DynamicBean replacement = new DynamicBean("mod", "Doc", p2);

		List<DynamicBean> list = new ArrayList<>(Arrays.asList(original));
		BindUtil.setElementInCollection(list, replacement);
		assertEquals(replacement, list.get(0));
	}

	@Test
	@SuppressWarnings("static-method")
	void setElementInCollectionDoesNothingWhenNoMatchingElement() {
		Map<String, Object> p1 = new HashMap<>();
		p1.put(Bean.DOCUMENT_ID, "bean-1");
		DynamicBean inList = new DynamicBean("mod", "Doc", p1);

		Map<String, Object> p2 = new HashMap<>();
		p2.put(Bean.DOCUMENT_ID, "bean-other");
		DynamicBean notInList = new DynamicBean("mod", "Doc", p2);

		List<DynamicBean> list = new ArrayList<>(Arrays.asList(inList));
		BindUtil.setElementInCollection(list, notInList);
		assertEquals(inList, list.get(0)); // unchanged
	}

	// ---- evaluateCondition --------------------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void evaluateConditionLiteralTrueReturnsTrue() {
		Bean bean = mock(Bean.class);
		assertTrue(BindUtil.evaluateCondition(bean, "true"));
	}

	@Test
	@SuppressWarnings("static-method")
	void evaluateConditionLiteralFalseReturnsFalse() {
		Bean bean = mock(Bean.class);
		assertFalse(BindUtil.evaluateCondition(bean, "false"));
	}

	@Test
	@SuppressWarnings("static-method")
	void evaluateConditionSkyveExpressionReturnsTrue() {
		Map<String, Object> properties = new HashMap<>();
		properties.put("flag", Boolean.TRUE);
		DynamicBean bean = new DynamicBean("test", "Doc", properties);

		assertTrue(BindUtil.evaluateCondition(bean, "{flag}"));
	}

	@Test
	@SuppressWarnings("static-method")
	void evaluateConditionSkyveExpressionReturnsFalse() {
		Map<String, Object> properties = new HashMap<>();
		properties.put("flag", Boolean.FALSE);
		DynamicBean bean = new DynamicBean("test", "Doc", properties);

		assertFalse(BindUtil.evaluateCondition(bean, "{flag}"));
	}

	@Test
	@SuppressWarnings("static-method")
	void evaluateConditionSkyveExpressionNonBooleanReturnsFalse() {
		Map<String, Object> properties = new HashMap<>();
		properties.put("flag", "yes");
		DynamicBean bean = new DynamicBean("test", "Doc", properties);

		assertFalse(BindUtil.evaluateCondition(bean, "{flag}"));
	}

	@Test
	@SuppressWarnings("static-method")
	void evaluateConditionInvalidNamedConditionThrowsMetaDataException() {
		Bean bean = mock(Bean.class);
		when(bean.getBizModule()).thenReturn("test");
		when(bean.getBizDocument()).thenReturn("Doc");

		assertThrows(MetaDataException.class, () -> BindUtil.evaluateCondition(bean, "active"));
	}

	// ---- isMutable ----------------------------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void isMutableReturnsTrueForWritablePojoProperty() {
		assertTrue(BindUtil.isMutable(new SimplePojo(), "value"));
	}

	@Test
	@SuppressWarnings("static-method")
	void isMutableReturnsFalseForReadOnlyPojoProperty() {
		assertFalse(BindUtil.isMutable(new SimplePojo(), "readOnly"));
	}

	// ---- get(Object bean, String binding) ----------------------------------

	@Test
	@SuppressWarnings("static-method")
	void getPojoSimplePropertyReturnsValue() {
		SimplePojo pojo = new SimplePojo();
		pojo.setValue("hello");
		assertEquals("hello", BindUtil.get(pojo, "value"));
	}

	@Test
	@SuppressWarnings("static-method")
	void getPojoNullPropertyReturnsNull() {
		assertNull(BindUtil.get(new SimplePojo(), "value"));
	}

	@Test
	@SuppressWarnings("static-method")
	void getPojoCompoundBindingReturnsNestedValue() {
		CompoundPojo parent = new CompoundPojo();
		parent.getChild().setValue("nested");
		assertEquals("nested", BindUtil.get(parent, "child.value"));
	}

	@Test
	@SuppressWarnings("static-method")
	void getPojoCompoundBindingNullIntermediateReturnsNull() {
		CompoundPojo parent = new CompoundPojo();
		parent.setChild(null);
		assertNull(BindUtil.get(parent, "child.value"));
	}

	// ---- set(Object bean, String binding, Object value) --------------------

	@Test
	@SuppressWarnings("static-method")
	void setPojoSimplePropertySetsValue() {
		SimplePojo pojo = new SimplePojo();
		BindUtil.set(pojo, "value", "world");
		assertEquals("world", pojo.getValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void setPojoNullValueClearsProperty() {
		SimplePojo pojo = new SimplePojo();
		pojo.setValue("initial");
		BindUtil.set(pojo, "value", null);
		assertNull(pojo.getValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void setPojoEmptyStringCoercedToNull() {
		SimplePojo pojo = new SimplePojo();
		pojo.setValue("initial");
		BindUtil.set(pojo, "value", "");
		assertNull(pojo.getValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void setPojoIntegerAutoConvertedToStringProperty() {
		SimplePojo pojo = new SimplePojo();
		BindUtil.set(pojo, "value", Integer.valueOf(42));
		assertEquals("42", pojo.getValue());
	}

	// ---- getPropertyType(Object bean, String binding) ----------------------

	@Test
	@SuppressWarnings("static-method")
	void getPropertyTypePojoStringPropertyReturnsStringClass() {
		assertEquals(String.class, BindUtil.getPropertyType(new SimplePojo(), "value"));
	}

	@Test
	@SuppressWarnings("static-method")
	void getMetaDataForBindingReturnsImplicitTypeForBizId() {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		when(document.getAttribute(Bean.DOCUMENT_ID)).thenReturn(null);
		when(document.getExtends()).thenReturn(null);

		TargetMetaData target = BindUtil.getMetaDataForBinding(null, module, document, Bean.DOCUMENT_ID);

		assertSame(document, target.getDocument());
		assertNull(target.getAttribute());
		assertEquals(String.class, target.getType());
	}

	@Test
	@SuppressWarnings("static-method")
	void getMetaDataForBindingThrowsWhenLastAttributeMissing() {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		when(document.getAttribute("missing")).thenReturn(null);
		when(document.getExtends()).thenReturn(null);
		when(document.getOwningModuleName()).thenReturn("admin");
		when(document.getName()).thenReturn("User");

		MetaDataException exception = assertThrows(MetaDataException.class,
				() -> BindUtil.getMetaDataForBinding(null, module, document, "missing"));

		assertTrue(exception.getMessage().contains("last attribute not in document"));
	}

	@Test
	@SuppressWarnings("static-method")
	void getMetaDataForBindingResolvesRelationTraversal() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Module relationModule = mock(Module.class);
		Document rootDocument = mock(Document.class);
		Document relationDocument = mock(Document.class);
		Relation relation = mock(Relation.class);
		Attribute relationField = mock(Attribute.class);

		when(rootDocument.getAttribute("child")).thenReturn(relation);
		when(rootDocument.getExtends()).thenReturn(null);
		when(rootDocument.getOwningModuleName()).thenReturn("admin");
		when(customer.getModule("admin")).thenReturn(module);
		when(relation.getDocumentName()).thenReturn("ChildDoc");
		when(module.getDocument(customer, "ChildDoc")).thenReturn(relationDocument);
		when(relationDocument.getOwningModuleName()).thenReturn("sales");
		when(customer.getModule("sales")).thenReturn(relationModule);

		when(relationDocument.getAttribute("name")).thenReturn(relationField);
		when(relationDocument.getExtends()).thenReturn(null);
		when(relationField.getAttributeType()).thenReturn(Attribute.AttributeType.text);
		doReturn(String.class).when(relationField).getImplementingType();

		TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, rootDocument, "child.name");

		assertSame(relationDocument, target.getDocument());
		assertSame(relationField, target.getAttribute());
		assertEquals(String.class, target.getType());
	}

	@Test
	@SuppressWarnings("static-method")
	void validateBindingReturnsConditionBooleanType() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Condition condition = mock(Condition.class);

		when(document.getCondition("active")).thenReturn(condition);
		when(document.getOwningModuleName()).thenReturn("admin");
		when(customer.getModule("admin")).thenReturn(module);

		TargetMetaData target = BindUtil.validateBinding(customer, module, document, "active");

		assertSame(document, target.getDocument());
		assertNull(target.getAttribute());
		assertEquals(Boolean.class, target.getType());
	}

	@Test
	@SuppressWarnings("static-method")
	void validateBindingResolvesRelationThenUltimateAttribute() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Module relationModule = mock(Module.class);
		Document rootDocument = mock(Document.class);
		Document relationDocument = mock(Document.class);
		Relation relation = mock(Relation.class);
		Attribute status = mock(Attribute.class);

		when(rootDocument.getAttribute("child")).thenReturn(relation);
		when(rootDocument.getExtends()).thenReturn(null);
		when(rootDocument.getOwningModuleName()).thenReturn("admin");
		when(customer.getModule("admin")).thenReturn(module);
		when(relation.getDocumentName()).thenReturn("ChildDoc");
		when(module.getDocument(customer, "ChildDoc")).thenReturn(relationDocument);

		when(relationDocument.getOwningModuleName()).thenReturn("sales");
		when(customer.getModule("sales")).thenReturn(relationModule);
		when(relationDocument.getAttribute("status")).thenReturn(status);
		when(relationDocument.getExtends()).thenReturn(null);
		when(status.getAttributeType()).thenReturn(Attribute.AttributeType.text);
		doReturn(String.class).when(status).getImplementingType();

		TargetMetaData target = BindUtil.validateBinding(customer, module, rootDocument, "child.status");

		assertSame(relationDocument, target.getDocument());
		assertSame(status, target.getAttribute());
		assertEquals(String.class, target.getType());
	}

	// ---- convertAndSet ------------------------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void convertAndSetPojoNullClearsProperty() {
		SimplePojo pojo = new SimplePojo();
		pojo.setValue("initial");
		BindUtil.convertAndSet(pojo, "value", null);
		assertNull(pojo.getValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void convertAndSetPojoStringToStringProperty() {
		SimplePojo pojo = new SimplePojo();
		BindUtil.convertAndSet(pojo, "value", "converted");
		assertEquals("converted", pojo.getValue());
	}

	// ---- createIdBinding ----------------------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void createIdBindingFormatsWithBizId() {
		assertThat(BindUtil.createIdBinding("items", "abc-123"), is("itemsElementById(abc-123)"));
	}

	// ---- isImplicit (additional branches) -----------------------------------

	@Test
	@SuppressWarnings("static-method")
	void isImplicitParentIdReturnsTrue() {
		assertTrue(BindUtil.isImplicit(HierarchicalBean.PARENT_ID));
	}

	@Test
	@SuppressWarnings("static-method")
	void isImplicitChildParentNameReturnsTrue() {
		assertTrue(BindUtil.isImplicit(ChildBean.PARENT_NAME));
	}

	@Test
	@SuppressWarnings("static-method")
	void isImplicitCustomerNameReturnsTrue() {
		assertTrue(BindUtil.isImplicit(Bean.CUSTOMER_NAME));
	}

	@Test
	@SuppressWarnings("static-method")
	void isImplicitDataGroupIdReturnsTrue() {
		assertTrue(BindUtil.isImplicit(Bean.DATA_GROUP_ID));
	}

	@Test
	@SuppressWarnings("static-method")
	void isImplicitUserIdReturnsTrue() {
		assertTrue(BindUtil.isImplicit(Bean.USER_ID));
	}

	@Test
	@SuppressWarnings("static-method")
	void isImplicitOrdinalNameReturnsTrue() {
		assertTrue(BindUtil.isImplicit(Bean.ORDINAL_NAME));
	}

	@Test
	@SuppressWarnings("static-method")
	void isImplicitCreatedKeyReturnsTrue() {
		assertTrue(BindUtil.isImplicit(Bean.CREATED_KEY));
	}

	@Test
	@SuppressWarnings("static-method")
	void isImplicitNotCreatedKeyReturnsTrue() {
		assertTrue(BindUtil.isImplicit(Bean.NOT_CREATED_KEY));
	}

	@Test
	@SuppressWarnings("static-method")
	void isImplicitPersistedKeyReturnsTrue() {
		assertTrue(BindUtil.isImplicit(Bean.PERSISTED_KEY));
	}

	@Test
	@SuppressWarnings("static-method")
	void isImplicitNotPersistedKeyReturnsTrue() {
		assertTrue(BindUtil.isImplicit(Bean.NOT_PERSISTED_KEY));
	}

	@Test
	@SuppressWarnings("static-method")
	void isImplicitChangedKeyReturnsTrue() {
		assertTrue(BindUtil.isImplicit(Bean.CHANGED_KEY));
	}

	@Test
	@SuppressWarnings("static-method")
	void isImplicitNotChangedKeyReturnsTrue() {
		assertTrue(BindUtil.isImplicit(Bean.NOT_CHANGED_KEY));
	}

	@Test
	@SuppressWarnings("static-method")
	void isImplicitModuleKeyReturnsTrue() {
		assertTrue(BindUtil.isImplicit(Bean.MODULE_KEY));
	}

	@Test
	@SuppressWarnings("static-method")
	void isImplicitDocumentKeyReturnsTrue() {
		assertTrue(BindUtil.isImplicit(Bean.DOCUMENT_KEY));
	}

	@Test
	@SuppressWarnings("static-method")
	void isImplicitVersionNameReturnsTrue() {
		assertTrue(BindUtil.isImplicit(PersistentBean.VERSION_NAME));
	}

	@Test
	@SuppressWarnings("static-method")
	void isImplicitTaggedNameReturnsTrue() {
		assertTrue(BindUtil.isImplicit(PersistentBean.TAGGED_NAME));
	}

	@Test
	@SuppressWarnings("static-method")
	void isImplicitFlagCommentNameReturnsTrue() {
		assertTrue(BindUtil.isImplicit(PersistentBean.FLAG_COMMENT_NAME));
	}

	// ---- implicitAttributeType (additional) ---------------------------------

	@Test
	@SuppressWarnings("static-method")
	void implicitAttributeTypeParentIdReturnsString() {
		assertEquals(String.class, BindUtil.implicitAttributeType(HierarchicalBean.PARENT_ID));
	}

	@Test
	@SuppressWarnings("static-method")
	void implicitAttributeTypeParentNameReturnsBeanClass() {
		assertEquals(Bean.class, BindUtil.implicitAttributeType(ChildBean.PARENT_NAME));
	}

	@Test
	@SuppressWarnings("static-method")
	void implicitAttributeTypeOrdinalNameReturnsInteger() {
		assertEquals(Integer.class, BindUtil.implicitAttributeType(Bean.ORDINAL_NAME));
	}

	@Test
	@SuppressWarnings("static-method")
	void implicitAttributeTypeChangedKeyReturnsBoolean() {
		assertEquals(Boolean.class, BindUtil.implicitAttributeType(Bean.CHANGED_KEY));
	}

	@Test
	@SuppressWarnings("static-method")
	void implicitAttributeTypeNotChangedKeyReturnsBoolean() {
		assertEquals(Boolean.class, BindUtil.implicitAttributeType(Bean.NOT_CHANGED_KEY));
	}

	@Test
	@SuppressWarnings("static-method")
	void implicitAttributeTypeCreatedKeyReturnsBoolean() {
		assertEquals(Boolean.class, BindUtil.implicitAttributeType(Bean.CREATED_KEY));
	}

	@Test
	@SuppressWarnings("static-method")
	void implicitAttributeTypeNotCreatedKeyReturnsBoolean() {
		assertEquals(Boolean.class, BindUtil.implicitAttributeType(Bean.NOT_CREATED_KEY));
	}

	@Test
	@SuppressWarnings("static-method")
	void implicitAttributeTypePersistedKeyReturnsBoolean() {
		assertEquals(Boolean.class, BindUtil.implicitAttributeType(Bean.PERSISTED_KEY));
	}

	@Test
	@SuppressWarnings("static-method")
	void implicitAttributeTypeNotPersistedKeyReturnsBoolean() {
		assertEquals(Boolean.class, BindUtil.implicitAttributeType(Bean.NOT_PERSISTED_KEY));
	}

	@Test
	@SuppressWarnings("static-method")
	void implicitAttributeTypeLockNameReturnsOptimisticLock() {
		assertEquals(OptimisticLock.class, BindUtil.implicitAttributeType(PersistentBean.LOCK_NAME));
	}

	@Test
	@SuppressWarnings("static-method")
	void implicitAttributeTypeFlagCommentNameReturnsString() {
		assertEquals(String.class, BindUtil.implicitAttributeType(PersistentBean.FLAG_COMMENT_NAME));
	}

	@Test
	@SuppressWarnings("static-method")
	void implicitAttributeTypeCustomerNameReturnsString() {
		assertEquals(String.class, BindUtil.implicitAttributeType(Bean.CUSTOMER_NAME));
	}

	@Test
	@SuppressWarnings("static-method")
	void implicitAttributeTypeModuleKeyReturnsString() {
		assertEquals(String.class, BindUtil.implicitAttributeType(Bean.MODULE_KEY));
	}

	@Test
	@SuppressWarnings("static-method")
	void implicitAttributeTypeDocumentKeyReturnsString() {
		assertEquals(String.class, BindUtil.implicitAttributeType(Bean.DOCUMENT_KEY));
	}

	public static class SimplePojo {
		private String value;
		private final String readOnly = "immutable";

		public String getValue() {
			return value;
		}

		public void setValue(String value) {
			this.value = value;
		}

		public String getReadOnly() {
			return readOnly;
		}
	}

	public static class CompoundPojo {
		private SimplePojo child = new SimplePojo();

		public SimplePojo getChild() {
			return child;
		}

		public void setChild(SimplePojo child) {
			this.child = child;
		}
	}

	enum TestBindEnum {
		VALUE_A, VALUE_B, VALUE_C
	}

	// ---- isDynamic ----

	@Test
	@SuppressWarnings("static-method")
	void isDynamicReturnsFalseForNullAttribute() {
		assertFalse(BindUtil.isDynamic(null, mock(Module.class), (Attribute) null));
	}

	@Test
	@SuppressWarnings("static-method")
	void isDynamicReturnsFalseForNonFieldAttribute() {
		Attribute attribute = mock(Attribute.class);
		assertFalse(BindUtil.isDynamic(null, mock(Module.class), attribute));
	}

	@Test
	@SuppressWarnings("static-method")
	void isDynamicDelegatesToFieldIsDynamic() {
		org.skyve.impl.metadata.model.document.field.Field field =
			mock(org.skyve.impl.metadata.model.document.field.Field.class, CALLS_REAL_METHODS);
		assertFalse(BindUtil.isDynamic(null, mock(Module.class), field));
	}

	@Test
	@SuppressWarnings("static-method")
	void populatePropertiesReturnsWithoutErrorWhenBeanIsNull() {
		SortedMap<String, Object> properties = new TreeMap<>();
		properties.put("name", "value");

		assertDoesNotThrow(() -> BindUtil.populateProperties(mock(User.class), null, properties, false));
	}

	@Test
	@SuppressWarnings("static-method")
	void populatePropertiesReturnsWithoutErrorWhenPropertiesAreNull() {
		assertDoesNotThrow(() -> BindUtil.populateProperties(mock(User.class), mock(Bean.class), null, false));
	}

	@Test
	@SuppressWarnings("static-method")
	void populatePropertiesCollectsErrorsAndThrowsValidationException() {
		SortedMap<String, Object> properties = new TreeMap<>();
		properties.put("name", "value");

		ValidationException exception = assertThrows(ValidationException.class,
				() -> BindUtil.populateProperties(null, mock(Bean.class), properties, false));

		assertFalse(exception.getMessages().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void populatePropertySetsConvertedIntegerOnDynamicBean() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Attribute ageAttribute = mock(Attribute.class);
		User user = mock(User.class);

		Map<String, Object> properties = new HashMap<>();
		properties.put("age", Integer.valueOf(0));
		DynamicBean bean = new DynamicBean("sales", "Order", properties);

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(document);
		when(document.getAttribute("age")).thenReturn(ageAttribute);
		when(document.getExtends()).thenReturn(null);
		when(ageAttribute.getAttributeType()).thenReturn(Attribute.AttributeType.integer);
		doReturn(Integer.class).when(ageAttribute).getImplementingType();

		BindUtil.populateProperty(user, bean, "age", " 42 ", false);

		assertEquals(Integer.valueOf(42), bean.get("age"));
	}

	@Test
	@SuppressWarnings("static-method")
	void populatePropertyBlankStringSetsNullForScalarAttribute() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Attribute nameAttribute = mock(Attribute.class);
		User user = mock(User.class);

		Map<String, Object> properties = new HashMap<>();
		properties.put("name", "before");
		DynamicBean bean = new DynamicBean("sales", "Order", properties);

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(document);
		when(document.getAttribute("name")).thenReturn(nameAttribute);
		when(document.getExtends()).thenReturn(null);
		when(nameAttribute.getAttributeType()).thenReturn(Attribute.AttributeType.text);
		doReturn(String.class).when(nameAttribute).getImplementingType();

		BindUtil.populateProperty(user, bean, "name", "   ", false);

		assertNull(bean.get("name"));
	}

	@Test
	@SuppressWarnings("static-method")
	void findLastNestedIndexIgnoresDotsInsideMappedAndIndexedSegments() throws Exception {
		Method method = BindUtil.class.getDeclaredMethod("findLastNestedIndex", String.class);
		method.setAccessible(true);

		int index = ((Integer) method.invoke(null, "orders[0].itemsElementById(a.b).name")).intValue();
		assertEquals("orders[0].itemsElementById(a.b)".length(), index);
		assertEquals(-1, ((Integer) method.invoke(null, "simpleBinding")).intValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void instantiateAndGetCreatesMissingRelationInstance() throws Exception {
		Customer customer = mock(Customer.class);
		User user = mock(User.class);
		Module module = mock(Module.class);
		Document rootDocument = mock(Document.class);
		Document childDocument = mock(Document.class);
		Relation childRelation = mock(Relation.class);

		Map<String, Object> properties = new HashMap<>();
		properties.put("child", null);
		DynamicBean rootBean = new DynamicBean("sales", "Order", properties);
		DynamicBean childBean = new DynamicBean("sales", "Child", new HashMap<>());

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(rootDocument);
		when(rootDocument.getAttribute("child")).thenReturn(childRelation);
		when(rootDocument.getPolymorphicAttribute(customer, "child")).thenReturn(childRelation);
		when(rootDocument.getExtends()).thenReturn(null);
		when(childRelation.getDocumentName()).thenReturn("Child");
		when(module.getDocument(customer, "Child")).thenReturn(childDocument);
		doReturn(childBean).when(childDocument).newInstance(user);

		withThreadLocalUser(user, () -> {
			Object result = BindUtil.instantiateAndGet(user, module, rootDocument, rootBean, "child");

			assertSame(childBean, result);
			assertSame(childBean, rootBean.get("child"));
		});
	}

	@Test
	@SuppressWarnings("static-method")
	void instantiateAndGetIndexedCollectionThrowsMetadataExceptionForOutOfRangeElement() {
		Customer customer = mock(Customer.class);
		User user = mock(User.class);
		Module module = mock(Module.class);
		Document rootDocument = mock(Document.class);
		Document lineDocument = mock(Document.class);
		Relation linesRelation = mock(Relation.class);

		Map<String, Object> properties = new HashMap<>();
		properties.put("lines", new ArrayList<>());
		DynamicBean rootBean = new DynamicBean("sales", "Order", properties);

		when(user.getCustomer()).thenReturn(customer);
		when(rootDocument.getAttribute("lines")).thenReturn(linesRelation);
		when(rootDocument.getExtends()).thenReturn(null);
		when(linesRelation.getDocumentName()).thenReturn("Line");
		when(module.getDocument(customer, "Line")).thenReturn(lineDocument);

		MetaDataException exception = assertThrows(MetaDataException.class,
				() -> BindUtil.instantiateAndGet(user, module, rootDocument, rootBean, "lines[2]"));

		assertTrue(exception.getMessage().contains("problem was encountered"));
	}

	@Test
	@SuppressWarnings("static-method")
	void instantiateAndGetThrowsWhenParentDocumentDoesNotExist() {
		Customer customer = mock(Customer.class);
		User user = mock(User.class);
		Module module = mock(Module.class);
		Document rootDocument = mock(Document.class);

		when(user.getCustomer()).thenReturn(customer);
		when(rootDocument.getParentDocument(customer)).thenReturn(null);

		MetaDataException exception = assertThrows(MetaDataException.class,
				() -> BindUtil.instantiateAndGet(user, module, rootDocument, mock(Bean.class), "parent.child"));

		assertTrue(exception.getMessage().contains("parent document does not exist"));
	}

	@Test
	@SuppressWarnings("static-method")
	void instantiateAndGetNavigatesParentThenCreatesNestedRelation() throws Exception {
		Customer customer = mock(Customer.class);
		User user = mock(User.class);
		Module module = mock(Module.class);
		Document rootDocument = mock(Document.class);
		Document parentDocument = mock(Document.class);
		Document childDocument = mock(Document.class);
		Relation childRelation = mock(Relation.class);

		Map<String, Object> parentProperties = new HashMap<>();
		parentProperties.put("child", null);
		DynamicBean parentBean = new DynamicBean("sales", "ParentDoc", parentProperties);

		Map<String, Object> rootProperties = new HashMap<>();
		rootProperties.put("parent", parentBean);
		DynamicBean rootBean = new DynamicBean("sales", "Order", rootProperties);

		DynamicBean childBean = new DynamicBean("sales", "Child", new HashMap<>());

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("sales")).thenReturn(module);

		when(rootDocument.getParentDocument(customer)).thenReturn(parentDocument);
		when(parentDocument.getAttribute("child")).thenReturn(childRelation);
		when(parentDocument.getExtends()).thenReturn(null);
		when(parentDocument.getPolymorphicAttribute(customer, "child")).thenReturn(childRelation);
		when(childRelation.getDocumentName()).thenReturn("Child");
		when(module.getDocument(customer, "Child")).thenReturn(childDocument);
		when(module.getDocument(customer, "ParentDoc")).thenReturn(parentDocument);
		doReturn(childBean).when(childDocument).newInstance(user);

		withThreadLocalUser(user, () -> {
			Object result = BindUtil.instantiateAndGet(user, module, rootDocument, rootBean, "parent.child");

			assertSame(childBean, result);
			assertSame(childBean, parentBean.get("child"));
		});
	}

	@Test
	@SuppressWarnings({ "static-method", "unchecked" })
	void copyCopiesScalarAndCollectionsAndReparentsChildElements() {
		Customer customer = mock(Customer.class);
		User user = mock(User.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Attribute textAttribute = mock(Attribute.class);
		org.skyve.metadata.model.document.Collection childCollectionAttribute =
				mock(org.skyve.metadata.model.document.Collection.class);
		Attribute inverseManyAttribute = mock(Attribute.class);
		ChildBean<Bean> childElement = mock(ChildBean.class);

		Map<String, Object> fromProperties = new HashMap<>();
		fromProperties.put("text", "copied-value");
		fromProperties.put("children", new ArrayList<>(List.of(childElement)));
		fromProperties.put("peers", new ArrayList<>(List.of(mock(Bean.class))));
		DynamicBean from = new DynamicBean("sales", "Order", fromProperties);

		Map<String, Object> toProperties = new HashMap<>();
		toProperties.put("text", "before");
		toProperties.put("children", new ArrayList<>());
		toProperties.put("peers", new ArrayList<>());
		DynamicBean to = new DynamicBean("sales", "Order", toProperties);

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(document);
		doReturn(Arrays.asList(textAttribute, childCollectionAttribute, inverseManyAttribute)).when(document).getAllAttributes(customer);

		when(textAttribute.getName()).thenReturn("text");
		when(textAttribute.getAttributeType()).thenReturn(Attribute.AttributeType.text);

		when(childCollectionAttribute.getName()).thenReturn("children");
		when(childCollectionAttribute.getAttributeType()).thenReturn(Attribute.AttributeType.collection);
		when(childCollectionAttribute.getType()).thenReturn(org.skyve.metadata.model.document.Collection.CollectionType.child);

		when(inverseManyAttribute.getName()).thenReturn("peers");
		when(inverseManyAttribute.getAttributeType()).thenReturn(Attribute.AttributeType.inverseMany);

		withThreadLocalUser(user, () -> BindUtil.copy(from, to));

		assertEquals("copied-value", to.get("text"));

		List<Bean> copiedChildren = (List<Bean>) to.get("children");
		assertEquals(1, copiedChildren.size());
		assertSame(childElement, copiedChildren.get(0));
		verify(childElement).setParent(to);

		List<Bean> copiedPeers = (List<Bean>) to.get("peers");
		assertEquals(1, copiedPeers.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void copySkipsCollectionWhenDestinationCollectionIsNull() {
		Customer customer = mock(Customer.class);
		User user = mock(User.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		org.skyve.metadata.model.document.Collection childCollectionAttribute =
				mock(org.skyve.metadata.model.document.Collection.class);

		Map<String, Object> fromProperties = new HashMap<>();
		fromProperties.put("children", new ArrayList<>());
		DynamicBean from = new DynamicBean("sales", "Order", fromProperties);

		Map<String, Object> toProperties = new HashMap<>();
		toProperties.put("children", null);
		DynamicBean to = new DynamicBean("sales", "Order", toProperties);

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(document);
		doReturn(Arrays.asList(childCollectionAttribute)).when(document).getAllAttributes(customer);
		when(childCollectionAttribute.getName()).thenReturn("children");
		when(childCollectionAttribute.getAttributeType()).thenReturn(Attribute.AttributeType.collection);
		when(childCollectionAttribute.getType()).thenReturn(org.skyve.metadata.model.document.Collection.CollectionType.child);

		assertDoesNotThrow(() -> withThreadLocalUser(user, () -> BindUtil.copy(from, to)));
		assertNull(to.get("children"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setElementParentSetsParentWhenRelationParentMatchesOwnerDocument() throws Exception {
		Method method = BindUtil.class.getDeclaredMethod("setElementParent",
				Customer.class,
				Module.class,
				Document.class,
				Relation.class,
				Bean.class,
				Bean.class);
		method.setAccessible(true);

		CustomerImpl customer = mock(CustomerImpl.class);
		Module module = mock(Module.class);
		Document ownerDocument = mock(Document.class);
		Document relatedDocument = mock(Document.class);
		Document parentDocument = mock(Document.class);
		Relation relation = mock(Relation.class);
		ChildBean<Bean> child = mock(ChildBean.class);
		Bean parent = mock(Bean.class);

		when(relation.getDocumentName()).thenReturn("ChildDoc");
		when(module.getDocument(customer, "ChildDoc")).thenReturn(relatedDocument);
		when(relatedDocument.getParentDocument(customer)).thenReturn(parentDocument);
		when(parentDocument.getOwningModuleName()).thenReturn("sales");
		when(parentDocument.getName()).thenReturn("Order");
		when(ownerDocument.getOwningModuleName()).thenReturn("sales");
		when(ownerDocument.getName()).thenReturn("Order");

		method.invoke(null, customer, module, ownerDocument, relation, child, parent);

		verify(child).setParent(parent);
	}

	@Test
	@SuppressWarnings("static-method")
	void orderByMetaDataReturnsEarlyWhenCompoundOwnerIsNull() {
		Map<String, Object> properties = new HashMap<>();
		properties.put("parent", null);
		DynamicBean bean = new DynamicBean("sales", "Order", properties);

		assertDoesNotThrow(() -> BindUtil.orderByMetaData(bean, "parent.lines"));
	}

	@Test
	@SuppressWarnings("static-method")
	void orderByMetaDataHandlesCollectionTargetWithoutOrdering() {
		Customer customer = mock(Customer.class);
		User user = mock(User.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		org.skyve.metadata.model.document.Collection collection =
				mock(org.skyve.metadata.model.document.Collection.class);

		Map<String, Object> properties = new HashMap<>();
		properties.put("lines", new ArrayList<>());
		DynamicBean bean = new DynamicBean("sales", "Order", properties);

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(document);
		when(document.getAttribute("lines")).thenReturn(collection);
		when(document.getExtends()).thenReturn(null);
		when(collection.getName()).thenReturn("lines");
		when(collection.getOrdered()).thenReturn(Boolean.FALSE);
		when(collection.getType()).thenReturn(org.skyve.metadata.model.document.Collection.CollectionType.child);
		doReturn(new ArrayList<>()).when(collection).getOrdering();

		assertDoesNotThrow(() -> withThreadLocalUser(user, () -> BindUtil.orderByMetaData(bean, "lines")));
	}

	@Test
	@SuppressWarnings("static-method")
	void setRelationInverseUsesInverseReferenceNameForBeanAssociation() throws Exception {
		Method method = BindUtil.class.getDeclaredMethod("setRelationInverse",
				Customer.class,
				Document.class,
				Relation.class,
				Bean.class,
				String.class,
				Bean.class,
				boolean.class);
		method.setAccessible(true);

		Customer customer = mock(Customer.class);
		Document ownerDocument = mock(Document.class);
		Inverse inverseRelation = mock(Inverse.class);
		Bean owner = mock(Bean.class);

		Map<String, Object> properties = new HashMap<>();
		properties.put("ownerRef", null);
		DynamicBean value = new DynamicBean("sales", "Child", properties);

		when(inverseRelation.getReferenceName()).thenReturn("ownerRef");

		method.invoke(null, customer, ownerDocument, inverseRelation, owner, "children", value, Boolean.FALSE);
		assertSame(owner, value.get("ownerRef"));

		method.invoke(null, customer, ownerDocument, inverseRelation, owner, "children", value, Boolean.TRUE);
		assertNull(value.get("ownerRef"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setRelationInverseFindsElementInverseAndMutatesCollection() throws Exception {
		Method method = BindUtil.class.getDeclaredMethod("setRelationInverse",
				Customer.class,
				Document.class,
				Relation.class,
				Bean.class,
				String.class,
				Bean.class,
				boolean.class);
		method.setAccessible(true);

		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document ownerDocument = mock(Document.class);
		Document elementDocument = mock(Document.class);
		Relation relation = mock(Relation.class);
		Inverse inverseAttribute = mock(Inverse.class);
		Bean owner = mock(Bean.class);

		List<Bean> parents = new ArrayList<>();
		Map<String, Object> properties = new HashMap<>();
		properties.put("parents", parents);
		DynamicBean value = new DynamicBean("sales", "Child", properties);

		when(ownerDocument.getName()).thenReturn("Order");
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Child")).thenReturn(elementDocument);
		doReturn(Arrays.asList(inverseAttribute)).when(elementDocument).getAllAttributes(customer);
		when(inverseAttribute.getDocumentName()).thenReturn("Order");
		when(inverseAttribute.getReferenceName()).thenReturn("children");
		when(inverseAttribute.getName()).thenReturn("parents");

		method.invoke(null, customer, ownerDocument, relation, owner, "children", value, Boolean.FALSE);
		assertEquals(1, parents.size());
		assertSame(owner, parents.get(0));

		method.invoke(null, customer, ownerDocument, relation, owner, "children", value, Boolean.TRUE);
		assertTrue(parents.isEmpty());
	}

	// ---- nullSafeConvert ----

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertIntegerFromInteger() {
		Object result = BindUtil.nullSafeConvert(Integer.class, Integer.valueOf(5));
		assertThat(result, is(Integer.valueOf(5)));
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertIntegerFromLong() {
		Object result = BindUtil.nullSafeConvert(Integer.class, Long.valueOf(42L));
		assertThat(result, is(Integer.valueOf(42)));
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertLongFromLong() {
		Object result = BindUtil.nullSafeConvert(Long.class, Long.valueOf(100L));
		assertThat(result, is(Long.valueOf(100L)));
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertLongFromInteger() {
		Object result = BindUtil.nullSafeConvert(Long.class, Integer.valueOf(99));
		assertThat(result, is(Long.valueOf(99L)));
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertShortFromInteger() {
		Object result = BindUtil.nullSafeConvert(Short.class, Integer.valueOf(10));
		assertThat(result, is(Short.valueOf((short) 10)));
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertFloatFromInteger() {
		Object result = BindUtil.nullSafeConvert(Float.class, Integer.valueOf(3));
		assertThat(result, is(Float.valueOf(3.0f)));
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertDoubleFromInteger() {
		Object result = BindUtil.nullSafeConvert(Double.class, Integer.valueOf(7));
		assertThat(result, is(Double.valueOf(7.0)));
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertBigDecimalFromString() {
		Object result = BindUtil.nullSafeConvert(BigDecimal.class, "3.14");
		assertThat(result, is(new BigDecimal("3.14")));
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertDecimal2FromString() {
		Object result = BindUtil.nullSafeConvert(Decimal2.class, "1.23");
		assertThat(result, is(new Decimal2("1.23")));
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertDecimal5FromString() {
		Object result = BindUtil.nullSafeConvert(Decimal5.class, "1.23456");
		assertThat(result, is(new Decimal5("1.23456")));
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertDecimal10FromString() {
		Object result = BindUtil.nullSafeConvert(Decimal10.class, "9.87654321");
		assertThat(result, is(new Decimal10("9.87654321")));
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertDateOnlyFromDate() {
		Date date = new Date();
		Object result = BindUtil.nullSafeConvert(DateOnly.class, date);
		assertThat(result, is(new DateOnly(date.getTime())));
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertTimeOnlyFromDate() {
		Date date = new Date();
		Object result = BindUtil.nullSafeConvert(TimeOnly.class, date);
		assertThat(result, is(new TimeOnly(date.getTime())));
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertDateTimeFromDate() {
		Date date = new Date();
		Object result = BindUtil.nullSafeConvert(DateTime.class, date);
		assertThat(result, is(new DateTime(date.getTime())));
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertTimestampFromDate() {
		Date date = new Date();
		Object result = BindUtil.nullSafeConvert(Timestamp.class, date);
		assertThat(result, is(new Timestamp(date.getTime())));
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertReturnsValueUnchangedForUnknownType() {
		String value = "unchanged";
		Object result = BindUtil.nullSafeConvert(String.class, value);
		assertThat(result, is(value));
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertDecimal2AlreadyDecimal2ReturnsUnchanged() {
		Decimal2 value = new Decimal2("1.50");
		Object result = BindUtil.nullSafeConvert(Decimal2.class, value);
		assertThat(result, is(value));
	}

	@Test
	@SuppressWarnings("static-method")
	void containsSkyveExpressionsReturnsTrueForExpressionAtStart() {
		assertTrue(BindUtil.containsSkyveExpressions("{binding}"));
	}

	@Test
	@SuppressWarnings("static-method")
	void containsSkyveExpressionsReturnsTrueForExpressionInMiddle() {
		assertTrue(BindUtil.containsSkyveExpressions("Hello {name} world"));
	}

	@Test
	@SuppressWarnings("static-method")
	void containsSkyveExpressionsReturnsFalseForEscapedBrace() {
		assertFalse(BindUtil.containsSkyveExpressions("No expression \\{here}"));
	}

	@Test
	@SuppressWarnings("static-method")
	void containsSkyveExpressionsReturnsFalseForPlainString() {
		assertFalse(BindUtil.containsSkyveExpressions("No expression here"));
	}

	@Test
	@SuppressWarnings("static-method")
	void isSkyveExpressionReturnsTrueForValidExpression() {
		assertTrue(BindUtil.isSkyveExpression("{binding}"));
	}

	@Test
	@SuppressWarnings("static-method")
	void isSkyveExpressionReturnsFalseForStringWithoutBraces() {
		assertFalse(BindUtil.isSkyveExpression("binding"));
	}

	@Test
	@SuppressWarnings("static-method")
	void isSkyveExpressionReturnsFalseForSingleChar() {
		assertFalse(BindUtil.isSkyveExpression("{"));
	}

	@Test
	@SuppressWarnings("static-method")
	void negateConditionReturnsNullForNull() {
		assertNull(BindUtil.negateCondition(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void negateConditionNegatesTrueToFalse() {
		assertEquals("false", BindUtil.negateCondition("true"));
	}

	@Test
	@SuppressWarnings("static-method")
	void negateConditionNegatesFalseToTrue() {
		assertEquals("true", BindUtil.negateCondition("false"));
	}

	@Test
	@SuppressWarnings("static-method")
	void negateConditionNegatesNotPrefixedCondition() {
		assertEquals("active", BindUtil.negateCondition("notActive"));
	}

	@Test
	@SuppressWarnings("static-method")
	void negateConditionAddsNotPrefix() {
		assertEquals("notActive", BindUtil.negateCondition("active"));
	}

	@Test
	@SuppressWarnings("static-method")
	void createCompoundBindingJoinsWithDot() {
		assertEquals("a.b.c", BindUtil.createCompoundBinding("a", "b", "c"));
	}

	@Test
	@SuppressWarnings("static-method")
	void createIndexedBindingAddsIndexBrackets() {
		assertEquals("list[2]", BindUtil.createIndexedBinding("list", 2));
	}

	@Test
	@SuppressWarnings("static-method")
	void createIdBindingAddsElementById() {
		assertEquals("list" + "ElementById(abc123)", BindUtil.createIdBinding("list", "abc123"));
	}

	@Test
	@SuppressWarnings("static-method")
	void sanitiseBindingReplacesSpecialChars() {
		assertEquals("a_b_2_", BindUtil.sanitiseBinding("a.b[2]"));
	}

	@Test
	@SuppressWarnings("static-method")
	void sanitiseBindingReturnsNullForNull() {
		assertNull(BindUtil.sanitiseBinding(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void unsanitiseBindingReplacesDotAndIndexPattern() {
		assertEquals("a.b[2]", BindUtil.unsanitiseBinding("a.b_2_"));
	}

	@Test
	@SuppressWarnings("static-method")
	void unsanitiseBindingReturnsNullForNull() {
		assertNull(BindUtil.unsanitiseBinding(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void isImplicitReturnsTrueForBizId() {
		assertTrue(BindUtil.isImplicit(Bean.DOCUMENT_ID));
	}

	@Test
	@SuppressWarnings("static-method")
	void isImplicitReturnsTrueForBizKey() {
		assertTrue(BindUtil.isImplicit(Bean.BIZ_KEY));
	}

	@Test
	@SuppressWarnings("static-method")
	void isImplicitReturnsFalseForNormalAttribute() {
		assertFalse(BindUtil.isImplicit("name"));
	}

	@Test
	@SuppressWarnings("static-method")
	void isImplicitReturnsFalseForNull() {
		assertFalse(BindUtil.isImplicit(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void implicitAttributeTypeReturnsStringForBizId() {
		assertEquals(String.class, BindUtil.implicitAttributeType(Bean.DOCUMENT_ID));
	}

	@Test
	@SuppressWarnings("static-method")
	void implicitAttributeTypeReturnsBooleanForCreatedKey() {
		assertEquals(Boolean.class, BindUtil.implicitAttributeType(Bean.CREATED_KEY));
	}

	@Test
	@SuppressWarnings("static-method")
	void implicitAttributeTypeReturnsNullForNonImplicit() {
		assertNull(BindUtil.implicitAttributeType("name"));
	}

	@Test
	@SuppressWarnings("static-method")
	void implicitAttributeTypeReturnsOptimisticLockForLockName() {
		assertEquals(OptimisticLock.class, BindUtil.implicitAttributeType(PersistentBean.LOCK_NAME));
	}

	@Test
	@SuppressWarnings("static-method")
	void implicitAttributeTypeReturnsBeanForParentName() {
		assertEquals(Bean.class, BindUtil.implicitAttributeType(ChildBean.PARENT_NAME));
	}

	@Test
	@SuppressWarnings("static-method")
	void implicitAttributeTypeReturnsIntegerForOrdinal() {
		assertEquals(Integer.class, BindUtil.implicitAttributeType(Bean.ORDINAL_NAME));
	}

	@Test
	@SuppressWarnings("static-method")
	void isAScalarTypeReturnsTrueForString() {
		assertTrue(BindUtil.isAScalarType(String.class));
	}

	@Test
	@SuppressWarnings("static-method")
	void isAScalarTypeReturnsTrueForInteger() {
		assertTrue(BindUtil.isAScalarType(Integer.class));
	}

	@Test
	@SuppressWarnings("static-method")
	void isAScalarTypeReturnsFalseForBean() {
		assertFalse(BindUtil.isAScalarType(Bean.class));
	}

	@Test
	@SuppressWarnings("static-method")
	void toTitleCaseConvertsJavaIdentifier() {
		assertEquals("First Name", BindUtil.toTitleCase("firstName"));
	}

	@Test
	@SuppressWarnings("static-method")
	void toTitleCaseHandlesSingleWord() {
		assertEquals("Name", BindUtil.toTitleCase("name"));
	}

	// ---- nullSafeConvert with Skyve Enumeration ----

	/**
	 * A test Skyve Enumeration enum with code-based lookup.
	 * Implements the single-interface contract required by BindUtil's Enumeration detection.
	 */
	enum TestSkyveEnum implements org.skyve.domain.types.Enumeration {
		CODE_A("CA", "Code Alpha"),
		CODE_B("CB", "Code Beta");

		private final String code;
		private final String description;

		TestSkyveEnum(String code, String description) {
			this.code = code;
			this.description = description;
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toLocalisedDescription() {
			return description;
		}

		@Override
		public org.skyve.metadata.model.document.Bizlet.DomainValue toDomainValue() {
			return new org.skyve.metadata.model.document.Bizlet.DomainValue(code, description);
		}

		public static TestSkyveEnum fromCode(String code) {
			for (TestSkyveEnum v : values()) {
				if (v.code.equals(code)) {
					return v;
				}
			}
			return null;
		}

		public static TestSkyveEnum fromLocalisedDescription(String desc) {
			for (TestSkyveEnum v : values()) {
				if (v.description.equals(desc)) {
					return v;
				}
			}
			return null;
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertSkyveEnumerationFromCodeStringConverts() {
		Object result = BindUtil.nullSafeConvert(TestSkyveEnum.class, "CA");
		assertEquals(TestSkyveEnum.CODE_A, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertSkyveEnumerationFromLocalisedDescriptionConverts() {
		Object result = BindUtil.nullSafeConvert(TestSkyveEnum.class, "Code Beta");
		assertEquals(TestSkyveEnum.CODE_B, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertSkyveEnumerationFromEnumerationInstanceConverts() {
		Object result = BindUtil.nullSafeConvert(TestSkyveEnum.class, TestSkyveEnum.CODE_A);
		assertEquals(TestSkyveEnum.CODE_A, result);
	}

	// ---- nullSafeConvert non-String enum value (covers else branch) ----

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertEnumFromEnumValueConverts() {
		// Passing an enum VALUE (not a String, not an Enumeration) hits the
		// else branch that calls nullSafeConvert(type, value.toString())
		Object result = BindUtil.nullSafeConvert(TestBindEnum.class, TestBindEnum.VALUE_B);
		assertEquals(TestBindEnum.VALUE_B, result);
	}

	// ---- nullSafeConvert Geometry (WKT) ----

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertGeometryFromValidWktStringConverts() {
		Object result = BindUtil.nullSafeConvert(Geometry.class, "POINT (1 2)");
		assertNotNull(result);
		assertTrue(result instanceof Geometry);
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertGeometryFromInvalidWktStringThrows() {
		assertThrows(DomainException.class,
				() -> BindUtil.nullSafeConvert(Geometry.class, "NOT VALID WKT!!!"));
	}

	// ---- nullSafeConvert Skyve Enumeration fromCode throws DomainException ----

	/**
	 * A Skyve Enumeration whose fromCode method always throws to exercise the
	 * DomainException catch block at BindUtil L504.
	 */
	enum ThrowingEnum implements org.skyve.domain.types.Enumeration {
		VAL;

		@Override
		public String toCode() {
			return "V";
		}

		@Override
		public String toLocalisedDescription() {
			return "Value";
		}

		@Override
		public org.skyve.metadata.model.document.Bizlet.DomainValue toDomainValue() {
			return new org.skyve.metadata.model.document.Bizlet.DomainValue("V", "Value");
		}

		@SuppressWarnings("unused")
		public static ThrowingEnum fromCode(String code) throws Exception {
			throw new Exception("intentional fromCode failure");
		}

		@SuppressWarnings("unused")
		public static ThrowingEnum fromLocalisedDescription(String desc) {
			return null;
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertSkyveEnumerationThrowsWhenFromCodeThrows() {
		assertThrows(DomainException.class,
				() -> BindUtil.nullSafeConvert(ThrowingEnum.class, "V"));
	}

	// ---- fromString with converter (covers L584, L595) ----

	@Test
	@SuppressWarnings("static-method")
	void fromStringWithNonNullConverterUsesConverterDirectly() throws Exception {
		// Covers L584: !fromSerializedFormat && converter != null -> result = converter.fromDisplayValue(stringValue)
		Converter<Integer> converter = mock(Converter.class);
		when(converter.fromDisplayValue("42")).thenReturn(Integer.valueOf(42));
		Object result = BindUtil.fromString(null, converter, Integer.class, "42");
		assertEquals(Integer.valueOf(42), result);
	}

	@Test
	@SuppressWarnings("static-method")
	void fromSerialisedWithNonDynamicConverterAndIntegerTypeUsesConverter() throws Exception {
		// Covers L595: fromSerializedFormat=true, non-DynamicEnumerationConverter, Integer type, converter != null
		Converter<Integer> converter = mock(Converter.class);
		when(converter.fromDisplayValue("99")).thenReturn(Integer.valueOf(99));
		Object result = BindUtil.fromSerialised(converter, Integer.class, "99");
		assertEquals(Integer.valueOf(99), result);
	}

	@Test
	@SuppressWarnings("static-method")
	void fromSerialisedWithDynamicEnumerationConverterUsesConverter() throws Exception {
		// Covers L588: fromSerializedFormat=true && converter instanceof DynamicEnumerationConverter
		DynamicEnumerationConverter dynamicConverter = mock(DynamicEnumerationConverter.class);
		when(dynamicConverter.fromDisplayValue("CA")).thenReturn("CA");
		Object result = BindUtil.fromSerialised(dynamicConverter, String.class, "CA");
		assertEquals("CA", result);
	}

	// ---- toDisplay with domainValues and Geometry ----

	@Test
	@SuppressWarnings("static-method")
	void toDisplayWithEnumerationValueInDomainValuesUsesToLocalisedDescription() {
		// Covers L723: value instanceof Enumeration
		List<DomainValue> domainValues = Arrays.asList(new DomainValue("CA", "Code A"));
		String result = BindUtil.toDisplay(mock(Customer.class), null, null, domainValues, TestSkyveEnum.CODE_A);
		assertEquals("Code Alpha", result);
	}

	@Test
	@SuppressWarnings("static-method")
	void toDisplayWithMatchingCodeInDomainValuesReturnsLocalisedDescription() {
		// Covers L726-732: found = true, matching code
		List<DomainValue> domainValues = Arrays.asList(new DomainValue("CA", "Code A"));
		String result = BindUtil.toDisplay(mock(Customer.class), null, null, domainValues, "CA");
		assertEquals("Code A", result);
	}

	@Test
	@SuppressWarnings("static-method")
	void toDisplayWithNonMatchingCodeInDomainValuesReturnsCodeValue() {
		// Covers L734-737: !found -> result = codeValue
		List<DomainValue> domainValues = Arrays.asList(new DomainValue("CA", "Code A"));
		String result = BindUtil.toDisplay(mock(Customer.class), null, null, domainValues, "UNKNOWN");
		assertEquals("UNKNOWN", result);
	}

	@Test
	@SuppressWarnings("static-method")
	void toDisplayWithConverterAndImplementingTypeUsesConverter() {
		// Covers L741: converter + implementingType path
		Converter<Integer> converter = mock(Converter.class);
		when(converter.toDisplayValue(Integer.valueOf(42))).thenReturn("42 items");
		String result = BindUtil.toDisplay(mock(Customer.class), converter, Integer.class, null, Integer.valueOf(42));
		assertEquals("42 items", result);
	}

	@Test
	@SuppressWarnings("static-method")
	void toDisplayWithGeometryReturnsWktString() throws Exception {
		// Covers L762: value instanceof Geometry
		org.locationtech.jts.io.WKTReader reader = new org.locationtech.jts.io.WKTReader();
		Geometry geometry = reader.read("POINT (1 2)");
		String result = BindUtil.toDisplay(mock(Customer.class), null, null, null, geometry);
		assertEquals("POINT (1 2)", result);
	}

	// ---- fromString with customer — date type converter paths ----

	@Test
	@SuppressWarnings("static-method")
	void fromStringDateOnlyWithCustomerConverterReturnsDateOnly() throws Exception {
		// Covers fromString L630-631: customer != null branch for DateOnly
		Customer customer = mock(Customer.class);
		Converter<DateOnly> dateConverter = mock(Converter.class);
		DateOnly expected = new DateOnly();
		when(customer.getDefaultDateConverter()).thenReturn(dateConverter);
		when(dateConverter.fromDisplayValue("01/01/2022")).thenReturn(expected);
		Object result = BindUtil.fromString(customer, null, DateOnly.class, "01/01/2022");
		assertTrue(result instanceof DateOnly);
	}

	@Test
	@SuppressWarnings("static-method")
	void fromStringTimeOnlyWithCustomerConverterReturnsTimeOnly() throws Exception {
		// Covers fromString L642-643: customer != null branch for TimeOnly
		Customer customer = mock(Customer.class);
		Converter<TimeOnly> timeConverter = mock(Converter.class);
		TimeOnly expected = new TimeOnly();
		when(customer.getDefaultTimeConverter()).thenReturn(timeConverter);
		when(timeConverter.fromDisplayValue("12:00:00")).thenReturn(expected);
		Object result = BindUtil.fromString(customer, null, TimeOnly.class, "12:00:00");
		assertTrue(result instanceof TimeOnly);
	}

	@Test
	@SuppressWarnings("static-method")
	void fromStringDateTimeWithCustomerConverterReturnsDateTime() throws Exception {
		// Covers fromString L654-655: customer != null branch for DateTime
		Customer customer = mock(Customer.class);
		Converter<DateTime> dateTimeConverter = mock(Converter.class);
		DateTime expected = new DateTime();
		when(customer.getDefaultDateTimeConverter()).thenReturn(dateTimeConverter);
		when(dateTimeConverter.fromDisplayValue("01/01/2022 12:00")).thenReturn(expected);
		Object result = BindUtil.fromString(customer, null, DateTime.class, "01/01/2022 12:00");
		assertTrue(result instanceof DateTime);
	}

	@Test
	@SuppressWarnings("static-method")
	void fromStringTimestampWithCustomerConverterReturnsTimestamp() throws Exception {
		// Covers fromString L666-667: customer != null branch for Timestamp
		Customer customer = mock(Customer.class);
		Converter<Timestamp> tsConverter = mock(Converter.class);
		Timestamp expected = new Timestamp();
		when(customer.getDefaultTimestampConverter()).thenReturn(tsConverter);
		when(tsConverter.fromDisplayValue("01/01/2022 12:00:00")).thenReturn(expected);
		Object result = BindUtil.fromString(customer, null, Timestamp.class, "01/01/2022 12:00:00");
		assertTrue(result instanceof Timestamp);
	}

	// ---- fromString Geometry branch ----

	@Test
	@SuppressWarnings("static-method")
	void fromStringGeometryWktConvertsToGeometry() {
		// Covers: Geometry.class.isAssignableFrom(type) → new WKTReader().read(stringValue)
		Object result = BindUtil.fromString(null, null, Geometry.class, "POINT (1 2)");
		assertNotNull(result);
		assertTrue(result instanceof Geometry);
	}

	// ---- toDisplay — Geometry branch ----

	@Test
	@SuppressWarnings("static-method")
	void toDisplayGeometryReturnsWktString() throws Exception {
		// Covers: value instanceof Geometry geometry → new WKTWriter().write(geometry)
		Geometry point = new org.locationtech.jts.io.WKTReader().read("POINT (3 4)");
		String result = BindUtil.toDisplay(mock(Customer.class), null, null, null, point);
		assertEquals("POINT (3 4)", result);
	}

	// ---- toDisplay — java.util.Date branch ----

	@Test
	@SuppressWarnings("static-method")
	void toDisplayJavaUtilDateReturnsFormattedString() {
		// Covers: value instanceof Date date → CORE.getDateFormat("dd/MM/yyyy").format(date)
		// Use a known epoch date
		java.util.Date date = new java.util.Date(0L); // 1970-01-01
		String result = BindUtil.toDisplay(mock(Customer.class), null, null, null, date);
		// dd/MM/yyyy format: 01/01/1970
		assertEquals("01/01/1970", result);
	}

	// ---- toDisplay — DateOnly/TimeOnly/DateTime/Timestamp branches ----

	@Test
	@SuppressWarnings("static-method")
	void toDisplayDateOnlyUsesDefaultDateConverter() {
		// Covers: value instanceof DateOnly date → customer.getDefaultDateConverter().toDisplayValue(date)
		Customer customer = mock(Customer.class);
		Converter<DateOnly> converter = mock(Converter.class);
		DateOnly date = new DateOnly(0L);
		when(customer.getDefaultDateConverter()).thenReturn(converter);
		when(converter.toDisplayValue(date)).thenReturn("01/01/1970");
		String result = BindUtil.toDisplay(customer, null, null, null, date);
		assertEquals("01/01/1970", result);
	}

	@Test
	@SuppressWarnings("static-method")
	void toDisplayTimeOnlyUsesDefaultTimeConverter() {
		// Covers: value instanceof TimeOnly time → customer.getDefaultTimeConverter().toDisplayValue(time)
		Customer customer = mock(Customer.class);
		Converter<TimeOnly> converter = mock(Converter.class);
		TimeOnly time = new TimeOnly(0L);
		when(customer.getDefaultTimeConverter()).thenReturn(converter);
		when(converter.toDisplayValue(time)).thenReturn("00:00:00");
		String result = BindUtil.toDisplay(customer, null, null, null, time);
		assertEquals("00:00:00", result);
	}

	@Test
	@SuppressWarnings("static-method")
	void toDisplayDateTimeUsesDefaultDateTimeConverter() {
		// Covers: value instanceof DateTime date → customer.getDefaultDateTimeConverter().toDisplayValue(date)
		Customer customer = mock(Customer.class);
		Converter<DateTime> converter = mock(Converter.class);
		DateTime dt = new DateTime(0L);
		when(customer.getDefaultDateTimeConverter()).thenReturn(converter);
		when(converter.toDisplayValue(dt)).thenReturn("01/01/1970 00:00:00");
		String result = BindUtil.toDisplay(customer, null, null, null, dt);
		assertEquals("01/01/1970 00:00:00", result);
	}

	@Test
	@SuppressWarnings("static-method")
	void toDisplayTimestampUsesDefaultTimestampConverter() {
		// Covers: value instanceof Timestamp time → customer.getDefaultTimestampConverter().toDisplayValue(time)
		Customer customer = mock(Customer.class);
		Converter<Timestamp> converter = mock(Converter.class);
		Timestamp ts = new Timestamp(0L);
		when(customer.getDefaultTimestampConverter()).thenReturn(converter);
		when(converter.toDisplayValue(ts)).thenReturn("01/01/1970 00:00:00.000");
		String result = BindUtil.toDisplay(customer, null, null, null, ts);
		assertEquals("01/01/1970 00:00:00.000", result);
	}

	@Test
	@SuppressWarnings("static-method")
	void toDisplayArbitraryObjectCallsToString() {
		// Covers: else → result = value.toString()
		Object obj = new Object() {
			@Override
			public String toString() {
				return "custom-object";
			}
		};
		String result = BindUtil.toDisplay(mock(Customer.class), null, null, null, obj);
		assertEquals("custom-object", result);
	}
}


