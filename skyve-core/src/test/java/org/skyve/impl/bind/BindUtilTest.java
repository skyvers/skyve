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
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.function.Function;
import java.util.function.UnaryOperator;

import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Geometry;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.enumeration.DynamicEnumerationConverter;
import org.skyve.impl.metadata.OrderingImpl;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.AssociationImpl;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.InverseMany;
import org.skyve.impl.metadata.model.document.InverseOne;
import org.skyve.impl.metadata.model.document.field.ConvertibleField;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Dynamic;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Condition;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Inverse;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.User;
import org.skyve.util.Binder.TargetMetaData;

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

	private static final class DynamicCollectionRepositoryFixture {
		private final CustomerImpl customer;
		private final ProvidedRepository previousRepository;

		private DynamicCollectionRepositoryFixture(boolean includeInverse, boolean childHasParentDocument) {
			customer = new CustomerImpl();
			customer.setName("acme");
			customer.getModuleEntries().put("sales", null);

			Module module = mock(Module.class);
			DocumentImpl ownerDocument = new DocumentImpl();
			ownerDocument.setOwningModuleName("sales");
			ownerDocument.setName("Order");

			CollectionImpl items = new CollectionImpl();
			items.setName("items");
			items.setDocumentName("Item");
			items.setType(CollectionType.child);
			ownerDocument.putRelation(items);

			DocumentImpl childDocument = new DocumentImpl();
			childDocument.setOwningModuleName("sales");
			childDocument.setName("Item");
			childDocument.setDynamism(new Dynamic());
			if (childHasParentDocument) {
				childDocument.setParentDocumentName("Order");
			}
			if (includeInverse) {
				InverseOne inverse = new InverseOne();
				inverse.setName("order");
				inverse.setDocumentName("Order");
				inverse.setReferenceName("items");
				childDocument.putRelation(inverse);
			}

			ProvidedRepository repository = mock(ProvidedRepository.class);
			when(repository.getModule(customer, "sales")).thenReturn(module);
			when(module.getDocument(customer, "Order")).thenReturn(ownerDocument);
			when(module.getDocument(customer, "Item")).thenReturn(childDocument);

			previousRepository = ProvidedRepositoryFactory.get();
			ProvidedRepositoryFactory.set(repository);
		}

		private User user() {
			User result = mock(User.class);
			when(result.getCustomer()).thenReturn(customer);
			return result;
		}

		private void close() {
			ProvidedRepositoryFactory.set(previousRepository);
		}

		@SuppressWarnings("unchecked")
		private void mapDerivedDocument(String documentName, String baseDocumentName) {
			try {
				Field derivationsField = CustomerImpl.class.getDeclaredField("derivations");
				derivationsField.setAccessible(true);
				Map<String, String> derivations = (Map<String, String>) derivationsField.get(customer);
				derivations.put("sales." + documentName, "sales." + baseDocumentName);
			}
			catch (ReflectiveOperationException e) {
				throw new AssertionError(e);
			}
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
	void formatMessageAppliesFunctionPostProcessorOverload() {
		Bean bean = mock(Bean.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		doReturn(customer).when(user).getCustomer();
		doReturn("alice").when(user).getName();

		withThreadLocalUser(user, () -> {
			java.util.function.Function<String, String> upperCase = value -> value.toUpperCase(Locale.ROOT);
			String result = BindUtil.formatMessage("Hello {USER}", upperCase, bean);
			assertEquals("Hello ALICE", result);
		});
	}

	@Test
	@SuppressWarnings("static-method")
	void formatMessageAppliesPostProcessorToImplicitExpression() {
		Bean bean = mock(Bean.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		doReturn(customer).when(user).getCustomer();
		doReturn("alice").when(user).getName();

		withThreadLocalUser(user, () -> {
			UnaryOperator<String> upperCase = value -> value.toUpperCase(Locale.ROOT);
			String result = BindUtil.formatMessage("Hello {USER}", upperCase, bean);
			assertEquals("Hello ALICE", result);
		});
		}

		@Test
		@SuppressWarnings("static-method")
		void formatMessageWithUnaryOperatorThatModifiesDisplay() {
			Bean bean = mock(Bean.class);
			User user = mock(User.class);
			Customer customer = mock(Customer.class);
			doReturn(customer).when(user).getCustomer();
			doReturn("world").when(user).getName();

			withThreadLocalUser(user, () -> {
				UnaryOperator<String> addPrefix = value -> "Hello " + value;
				String result = BindUtil.formatMessage("{USER}", addPrefix, bean);
				assertEquals("Hello world", result);
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

	// ---- prefixMessageExpressions ----

	@Test
	@SuppressWarnings("static-method")
	void prefixMessageExpressionsNullMessageReturnsNull() {
		assertNull(BindUtil.prefixMessageExpressions(null, "parent"));
	}

	@Test
	@SuppressWarnings("static-method")
	void prefixMessageExpressionsPrefixesEachUnescapedExpression() {
		String result = BindUtil.prefixMessageExpressions("Hello {name} from {address.city}", "parent");

		assertEquals("Hello {parent.name} from {parent.address.city}", result);
	}

	@Test
	@SuppressWarnings("static-method")
	void prefixMessageExpressionsLeavesEscapedExpressionLiteral() {
		String result = BindUtil.prefixMessageExpressions("Hello \\{name}", "parent");

		assertEquals("Hello \\{name}", result);
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

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertNonNumericValueForNumericTypeReturnsUnchanged() {
		String value = "not a number";

		Object result = BindUtil.nullSafeConvert(Integer.class, value);

		assertSame(value, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void nullSafeConvertInvalidWktThrowsDomainException() {
		DomainException exception = assertThrows(DomainException.class,
				() -> BindUtil.nullSafeConvert(Geometry.class, "not WKT"));

		assertTrue(exception.getMessage().contains("is not valid WKT"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertNullReturnsNull() {
		assertNull(BindUtil.convert(String.class, null));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertNonNullDelegatesToNullSafeConvert() {
		assertEquals(Integer.valueOf(12), BindUtil.convert(Integer.class, Long.valueOf(12L)));
	}

	// ---- fromString ----

	@Test
	@SuppressWarnings({"rawtypes", "static-method"})
	void fromStringUsesConverterBeforeScalarParsing() {
		Converter converter = mock(Converter.class);
		doReturn(Integer.valueOf(42)).when(converter).fromDisplayValue("forty two");

		Object result = BindUtil.fromString(null, converter, Integer.class, "forty two");

		assertEquals(Integer.valueOf(42), result);
	}

	@Test
	@SuppressWarnings({"static-method"})
	void fromSerialisedDynamicEnumerationConverterUsesConverter() {
		DynamicEnumerationConverter converter = mock(DynamicEnumerationConverter.class);
		doReturn("resolved").when(converter).fromDisplayValue("code");

		Object result = BindUtil.fromSerialised(converter, String.class, "code");

		assertEquals("resolved", result);
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
	void toDisplayCustomerValueOverloadReturnsStringForPrimitiveWrapper() {
		String result = BindUtil.toDisplay(mock(Customer.class), Boolean.FALSE);
		assertEquals("No", result);
	}

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

	@Test
	@SuppressWarnings("static-method")
	void toDisplayDomainValuesReturnsMatchingLocalisedDescription() {
		DomainValue domainValue = mock(DomainValue.class);
		doReturn("A").when(domainValue).getCode();
		doReturn("Alpha").when(domainValue).getLocalisedDescription();

		String result = BindUtil.toDisplay(mock(Customer.class), null, null, Arrays.asList(domainValue), "A");

		assertEquals("Alpha", result);
	}

	@Test
	@SuppressWarnings("static-method")
	void toDisplayDomainValuesFallsBackToCodeWhenNoMatch() {
		DomainValue domainValue = mock(DomainValue.class);
		doReturn("A").when(domainValue).getCode();

		String result = BindUtil.toDisplay(mock(Customer.class), null, null, Arrays.asList(domainValue), "B");

		assertEquals("B", result);
	}

	@Test
	@SuppressWarnings({"rawtypes", "static-method"})
	void toDisplayConverterCoercesValueToImplementingType() {
		Converter converter = mock(Converter.class);
		doReturn("12.30").when(converter).toDisplayValue(new Decimal2("12.30"));

		String result = BindUtil.toDisplay(mock(Customer.class), converter, Decimal2.class, null, "12.30");

		assertEquals("12.30", result);
	}

	@Test
	@SuppressWarnings("static-method")
	void getDisplayReturnsBizKeyWhenBindingResolvesToBean() {
		Bean child = mock(Bean.class);
		doReturn("child-key").when(child).getBizKey();

		Map<String, Object> properties = new HashMap<>();
		properties.put("child", child);
		Bean parent = new DynamicBean("mod", "Doc", properties);

		String result = BindUtil.getDisplay(mock(Customer.class), parent, "child");
		assertEquals("child-key", result);
	}

	@Test
	@SuppressWarnings("static-method")
	void getDisplayResolvesSimpleFieldMetadata() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		org.skyve.impl.metadata.model.document.DocumentImpl document = new org.skyve.impl.metadata.model.document.DocumentImpl();
		document.setOwningModuleName("sales");
		document.setName("Order");
		org.skyve.impl.metadata.model.document.field.Text field = new org.skyve.impl.metadata.model.document.field.Text();
		field.setName("status");
		field.setLength(10);

		Map<String, Object> properties = new HashMap<>();
		properties.put("status", "A");
		DynamicBean bean = new DynamicBean("sales", "Order", properties);

		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(document);
		document.putAttribute(field);

		String result = BindUtil.getDisplay(customer, bean, "status");
		assertEquals("A", result);
	}

	@Test
	@SuppressWarnings("static-method")
	void resolveDisplayRealBeanReturnsWrappedBean() throws Exception {
		Method method = BindUtil.class.getDeclaredMethod("resolveDisplayRealBean", Bean.class);
		method.setAccessible(true);

		Bean realBean = mock(Bean.class);
		Map<String, Object> properties = new HashMap<>();
		properties.put(DynamicBean.BEAN_PROPERTY_KEY, realBean);
		DynamicBean wrapper = new DynamicBean("sales", "Order", properties);

		assertSame(realBean, method.invoke(null, wrapper));
	}

	@Test
	@SuppressWarnings("static-method")
	void resolveOwningBeanForBindingReturnsNestedBean() throws Exception {
		Method method = BindUtil.class.getDeclaredMethod("resolveOwningBeanForBinding", Bean.class, String.class);
		method.setAccessible(true);

		Bean childBean = mock(Bean.class);
		Map<String, Object> properties = new HashMap<>();
		properties.put("parent", childBean);
		DynamicBean owningBean = new DynamicBean("sales", "Order", properties);

		assertSame(childBean, method.invoke(null, owningBean, "parent.status"));
	}

	@Test
	@SuppressWarnings("static-method")
	void resolveDisplayDomainValuesReturnsNullForDynamicDomainWithoutWrappedRealBean() throws Exception {
		Method method = BindUtil.class.getDeclaredMethod("resolveDisplayDomainValues",
				Customer.class,
				Bean.class,
				String.class,
				Document.class,
				org.skyve.impl.metadata.model.document.field.Field.class);
		method.setAccessible(true);
		CustomerImpl customer = new CustomerImpl();
		DocumentImpl document = new DocumentImpl();
		document.setOwningModuleName("sales");
		document.setName("Order");
		org.skyve.impl.metadata.model.document.field.Text field =
				new org.skyve.impl.metadata.model.document.field.Text();
		field.setName("status");
		field.setDomainType(org.skyve.metadata.model.document.DomainType.dynamic);
		DynamicBean bean = new DynamicBean("sales", "Order", new HashMap<>(Map.of("status", "A")));

		assertNull(method.invoke(null, customer, bean, "status", document, field));
	}

	@Test
	@SuppressWarnings("static-method")
	void dynamicCollectionFixtureAddAndRemoveMaintainsInverseReference() {
		DynamicCollectionRepositoryFixture fixture = new DynamicCollectionRepositoryFixture(true, false);
		try {
			withThreadLocalUser(fixture.user(), () -> {
				List<Bean> items = new ArrayList<>();
				DynamicBean owner = new DynamicBean("sales", "Order", new HashMap<>(Map.of("items", items)));
				Map<String, Object> itemProperties = new HashMap<>();
				itemProperties.put(Bean.DOCUMENT_ID, "item-1");
				itemProperties.put("order", null);
				DynamicBean item = new DynamicBean("sales", "Item", itemProperties);

				assertTrue(BindUtil.addElementToCollection(owner, "items", item));
				assertSame(owner, BindUtil.get(item, "order"));
				assertTrue(BindUtil.removeElementFromCollection(owner, "items", item));
				assertNull(BindUtil.get(item, "order"));
			});
		}
		finally {
			fixture.close();
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void dynamicCollectionFixtureIndexedAddAndIndexedRemoveRoundTripsElement() {
		DynamicCollectionRepositoryFixture fixture = new DynamicCollectionRepositoryFixture(true, false);
		try {
			withThreadLocalUser(fixture.user(), () -> {
				Map<String, Object> existingProperties = new HashMap<>();
				existingProperties.put(Bean.DOCUMENT_ID, "existing");
				existingProperties.put("order", null);
				DynamicBean existing = new DynamicBean("sales", "Item", existingProperties);
				List<Bean> items = new ArrayList<>();
				items.add(existing);
				DynamicBean owner = new DynamicBean("sales", "Order", new HashMap<>(Map.of("items", items)));
				Map<String, Object> insertedProperties = new HashMap<>();
				insertedProperties.put(Bean.DOCUMENT_ID, "inserted");
				insertedProperties.put("order", null);
				DynamicBean inserted = new DynamicBean("sales", "Item", insertedProperties);

				BindUtil.addElementToCollection(owner, "items", 0, inserted);
				assertSame(inserted, items.get(0));
				assertSame(owner, BindUtil.get(inserted, "order"));

				Bean removed = BindUtil.removeElementFromCollection(owner, "items", 0);
				assertSame(inserted, removed);
				assertNull(BindUtil.get(inserted, "order"));
			});
		}
		finally {
			fixture.close();
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void dynamicCollectionFixtureAssignsAndClearsChildParent() {
		DynamicCollectionRepositoryFixture fixture = new DynamicCollectionRepositoryFixture(false, true);
		try {
			withThreadLocalUser(fixture.user(), () -> {
				List<Bean> items = new ArrayList<>();
				DynamicBean owner = new DynamicBean("sales", "Order", new HashMap<>(Map.of("items", items)));
				ChildBean<Bean> child = mock(ChildBean.class);
				when(child.getBizModule()).thenReturn("sales");
				when(child.getBizDocument()).thenReturn("Item");

				assertTrue(BindUtil.addElementToCollection(owner, "items", child));
				verify(child).setParent(owner);

				assertTrue(BindUtil.removeElementFromCollection(owner, "items", child));
				verify(child).setParent(null);
			});
		}
		finally {
			fixture.close();
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void dynamicCollectionFixtureAssignsParentViaDerivedBaseDocument() {
		DynamicCollectionRepositoryFixture fixture = new DynamicCollectionRepositoryFixture(false, false);
		try {
			Module module = mock(Module.class);
			DocumentImpl ownerDocument = new DocumentImpl();
			ownerDocument.setOwningModuleName("sales");
			ownerDocument.setName("Order");
			CollectionImpl items = new CollectionImpl();
			items.setName("items");
			items.setDocumentName("Item");
			items.setType(CollectionType.child);
			ownerDocument.putRelation(items);

			DocumentImpl childDocument = new DocumentImpl();
			childDocument.setOwningModuleName("sales");
			childDocument.setName("Item");
			childDocument.setParentDocumentName("BaseOrder");
			childDocument.setDynamism(new Dynamic());

			DocumentImpl baseDocument = new DocumentImpl();
			baseDocument.setOwningModuleName("sales");
			baseDocument.setName("BaseOrder");

			ProvidedRepository repository = mock(ProvidedRepository.class);
			when(repository.getModule(fixture.customer, "sales")).thenReturn(module);
			when(module.getDocument(fixture.customer, "Order")).thenReturn(ownerDocument);
			when(module.getDocument(fixture.customer, "Item")).thenReturn(childDocument);
			when(module.getDocument(fixture.customer, "BaseOrder")).thenReturn(baseDocument);
			ProvidedRepositoryFactory.set(repository);
			fixture.mapDerivedDocument("Order", "BaseOrder");

			withThreadLocalUser(fixture.user(), () -> {
				List<Bean> itemsList = new ArrayList<>();
				DynamicBean owner = new DynamicBean("sales", "Order", new HashMap<>(Map.of("items", itemsList)));
				ChildBean<Bean> child = mock(ChildBean.class);
				when(child.getBizModule()).thenReturn("sales");
				when(child.getBizDocument()).thenReturn("Item");

				assertTrue(BindUtil.addElementToCollection(owner, "items", child));
				verify(child).setParent(owner);
			});
		}
		finally {
			fixture.close();
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void orderInverseManyByMetaDataSortsByDeclaredOrdering() {
		Map<String, Object> firstProps = new HashMap<>();
		firstProps.put("name", "Zulu");
		DynamicBean first = new DynamicBean("sales", "Item", firstProps);
		Map<String, Object> secondProps = new HashMap<>();
		secondProps.put("name", "Alpha");
		DynamicBean second = new DynamicBean("sales", "Item", secondProps);
		List<Bean> items = new ArrayList<>(Arrays.asList(first, second));
		DynamicBean owner = new DynamicBean("sales", "Order", new HashMap<>(Map.of("items", items)));

		InverseMany inverse = new InverseMany();
		inverse.setName("items");
		inverse.getOrdering().add(new OrderingImpl("name", SortDirection.ascending));

		BindUtil.orderInverseManyByMetaData(owner, inverse);

		assertSame(second, items.get(0));
		assertSame(first, items.get(1));
	}

	@Test
	@SuppressWarnings("static-method")
	void orderInverseManyByMetaDataWithNoOrderingLeavesListAsIs() {
		DynamicBean first = new DynamicBean("sales", "Item", new HashMap<>(Map.of("name", "Zulu")));
		DynamicBean second = new DynamicBean("sales", "Item", new HashMap<>(Map.of("name", "Alpha")));
		List<Bean> items = new ArrayList<>(Arrays.asList(first, second));
		DynamicBean owner = new DynamicBean("sales", "Order", new HashMap<>(Map.of("items", items)));

		InverseMany inverse = new InverseMany();
		inverse.setName("items");

		BindUtil.orderInverseManyByMetaData(owner, inverse);

		assertSame(first, items.get(0));
		assertSame(second, items.get(1));
	}

	@Test
	@SuppressWarnings("static-method")
	void getPropertyTypeCompoundBindingReturnsNestedPropertyType() {
		Map<String, Object> childProperties = new HashMap<>();
		childProperties.put("name", "Alpha");
		DynamicBean child = new DynamicBean("sales", "Item", childProperties);
		Map<String, Object> ownerProperties = new HashMap<>();
		ownerProperties.put("child", child);
		DynamicBean owner = new DynamicBean("sales", "Order", ownerProperties);

		assertEquals(String.class, BindUtil.getPropertyType(owner, "child.name"));
	}

	@Test
	@SuppressWarnings("static-method")
	void getPropertyTypeCompoundBindingThrowsWhenPenultimateIsNull() {
		Map<String, Object> ownerProperties = new HashMap<>();
		ownerProperties.put("child", null);
		DynamicBean owner = new DynamicBean("sales", "Order", ownerProperties);
		assertThrows(MetaDataException.class, () -> BindUtil.getPropertyType(owner, "child.name"));
	}

	@Test
	@SuppressWarnings("static-method")
	void getPropertyTypeDynamicAssociationUsesRelatedDocumentBeanClass() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document ownerDocument = mock(Document.class);
		AssociationImpl association = new AssociationImpl();
		association.setName("child");
		association.setDocumentName("Item");
		DocumentImpl relatedDocument = new DocumentImpl();
		relatedDocument.setOwningModuleName("sales");
		relatedDocument.setName("Item");
		relatedDocument.setDynamism(new Dynamic());

		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(ownerDocument);
		when(module.getDocument(customer, "Item")).thenReturn(relatedDocument);
		when(ownerDocument.getPolymorphicAttribute(customer, "child")).thenReturn(association);

		Map<String, Object> ownerProperties = new HashMap<>();
		ownerProperties.put("child", null);
		DynamicBean owner = new DynamicBean("sales", "Order", ownerProperties);

		withThreadLocalUser(user, () -> assertEquals(DynamicBean.class, BindUtil.getPropertyType(owner, "child")));
	}

	@Test
	@SuppressWarnings("static-method")
	void getPropertyTypeDynamicCollectionElementByIdUsesRelatedDocumentBeanClass() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document ownerDocument = mock(Document.class);
		CollectionImpl relation = new CollectionImpl();
		relation.setName("items");
		relation.setDocumentName("Item");
		relation.setType(CollectionType.child);
		DocumentImpl relatedDocument = new DocumentImpl();
		relatedDocument.setOwningModuleName("sales");
		relatedDocument.setName("Item");
		relatedDocument.setDynamism(new Dynamic());

		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(ownerDocument);
		when(module.getDocument(customer, "Item")).thenReturn(relatedDocument);
		when(ownerDocument.getPolymorphicAttribute(customer, "items")).thenReturn(relation);

		List<Bean> items = new ArrayList<>();
		DynamicBean owner = new DynamicBean("sales", "Order", new HashMap<>(Map.of("items", items)));
		String binding = BindUtil.createIdBinding("items", "missing");

		withThreadLocalUser(user, () -> assertEquals(DynamicBean.class, BindUtil.getPropertyType(owner, binding)));
	}

	@Test
	@SuppressWarnings("static-method")
	void getPropertyTypeDynamicBindingReturnsObjectWhenMetadataAttributeMissing() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document ownerDocument = mock(Document.class);
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(ownerDocument);
		when(ownerDocument.getPolymorphicAttribute(customer, "missing")).thenReturn(null);

		Map<String, Object> ownerProperties = new HashMap<>();
		ownerProperties.put("missing", null);
		DynamicBean owner = new DynamicBean("sales", "Order", ownerProperties);

		withThreadLocalUser(user, () -> assertEquals(Object.class, BindUtil.getPropertyType(owner, "missing")));
	}

	@Test
	@SuppressWarnings("static-method")
	void getPropertyTypeDynamicAssociationThrowsMetaDataExceptionWhenRelatedClassCannotResolve() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document ownerDocument = mock(Document.class);
		AssociationImpl association = new AssociationImpl();
		association.setName("child");
		association.setDocumentName("Item");
		DocumentImpl relatedDocument = new DocumentImpl();
		relatedDocument.setOwningModuleName("sales");
		relatedDocument.setName("Item");
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(ownerDocument);
		when(module.getDocument(customer, "Item")).thenReturn(relatedDocument);
		when(ownerDocument.getPolymorphicAttribute(customer, "child")).thenReturn(association);

		Map<String, Object> ownerProperties = new HashMap<>();
		ownerProperties.put("child", null);
		DynamicBean owner = new DynamicBean("sales", "Order", ownerProperties);

		withThreadLocalUser(user,
				() -> assertThrows(MetaDataException.class, () -> BindUtil.getPropertyType(owner, "child")));
	}

	@Test
	@SuppressWarnings("static-method")
	void extractDynamicElementBizIdReturnsBizIdPortion() throws Exception {
		Method method = BindUtil.class.getDeclaredMethod("extractDynamicElementBizId", String.class);
		method.setAccessible(true);

		String binding = BindUtil.createIdBinding("items", "bean-99");
		assertEquals("bean-99", method.invoke(null, binding));
	}

	@Test
	@SuppressWarnings("static-method")
	void setDynamicElementByIdValueReplacesMatchingDynamicListElement() throws Exception {
		Method method = BindUtil.class.getDeclaredMethod("setDynamicElementByIdValue",
				Object.class,
				String.class,
				Bean.class,
				String.class,
				String.class,
				Object.class);
		method.setAccessible(true);

		Map<String, Object> firstProps = new HashMap<>();
		firstProps.put(Bean.DOCUMENT_ID, "bean-1");
		DynamicBean first = new DynamicBean("sales", "Item", firstProps);
		Map<String, Object> secondProps = new HashMap<>();
		secondProps.put(Bean.DOCUMENT_ID, "bean-2");
		DynamicBean second = new DynamicBean("sales", "Item", secondProps);
		List<Bean> items = new ArrayList<>(Arrays.asList(first, second));
		DynamicBean owner = new DynamicBean("sales", "Order", new HashMap<>(Map.of("items", items)));
		DynamicBean replacement = new DynamicBean("sales",
				"Item",
				new HashMap<>(Map.of(Bean.DOCUMENT_ID, "replacement")));

		String simpleBinding = BindUtil.createIdBinding("items", "bean-1");
		method.invoke(null, owner, "items", owner, simpleBinding, "items", replacement);

		assertSame(replacement, items.get(0));
		assertSame(second, items.get(1));
	}

	@Test
	@SuppressWarnings("static-method")
	void setDynamicElementByIdValueThrowsWhenValueIsNotBean() throws Exception {
		Method method = BindUtil.class.getDeclaredMethod("setDynamicElementByIdValue",
				Object.class,
				String.class,
				Bean.class,
				String.class,
				String.class,
				Object.class);
		method.setAccessible(true);

		Map<String, Object> firstProps = new HashMap<>();
		firstProps.put(Bean.DOCUMENT_ID, "bean-1");
		DynamicBean first = new DynamicBean("sales", "Item", firstProps);
		DynamicBean owner = new DynamicBean("sales",
				"Order",
				new HashMap<>(Map.of("items", new ArrayList<>(Arrays.asList(first)))));

		String simpleBinding = BindUtil.createIdBinding("items", "bean-1");
		assertThrows(InvocationTargetException.class,
				() -> method.invoke(null, owner, "items", owner, simpleBinding, "items", "not-a-bean"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setDynamicElementByIdValueThrowsWhenDynamicListIsNull() throws Exception {
		Method method = BindUtil.class.getDeclaredMethod("setDynamicElementByIdValue",
				Object.class,
				String.class,
				Bean.class,
				String.class,
				String.class,
				Object.class);
		method.setAccessible(true);

		Map<String, Object> ownerProperties = new HashMap<>();
		ownerProperties.put("items", null);
		DynamicBean owner = new DynamicBean("sales", "Order", ownerProperties);
		DynamicBean replacement = new DynamicBean("sales",
				"Item",
				new HashMap<>(Map.of(Bean.DOCUMENT_ID, "replacement")));

		String simpleBinding = BindUtil.createIdBinding("items", "bean-1");
		assertThrows(InvocationTargetException.class,
				() -> method.invoke(null, owner, "items", owner, simpleBinding, "items", replacement));
	}

	@Test
	@SuppressWarnings("static-method")
	void setDynamicElementByIdValueThrowsWhenElementNotFoundInList() throws Exception {
		Method method = BindUtil.class.getDeclaredMethod("setDynamicElementByIdValue",
				Object.class,
				String.class,
				Bean.class,
				String.class,
				String.class,
				Object.class);
		method.setAccessible(true);

		Map<String, Object> firstProps = new HashMap<>();
		firstProps.put(Bean.DOCUMENT_ID, "bean-1");
		DynamicBean first = new DynamicBean("sales", "Item", firstProps);
		DynamicBean owner = new DynamicBean("sales",
				"Order",
				new HashMap<>(Map.of("items", new ArrayList<>(Arrays.asList(first)))));
		DynamicBean replacement = new DynamicBean("sales",
				"Item",
				new HashMap<>(Map.of(Bean.DOCUMENT_ID, "replacement")));

		String simpleBinding = BindUtil.createIdBinding("items", "missing");
		assertThrows(InvocationTargetException.class,
				() -> method.invoke(null, owner, "items", owner, simpleBinding, "items", replacement));
	}

	@Test
	@SuppressWarnings("static-method")
	void setIndexedDynamicValueSetsIndexedElement() throws Exception {
		Method method = BindUtil.class.getDeclaredMethod("setIndexedDynamicValue",
				Object.class,
				String.class,
				Bean.class,
				String.class,
				String.class,
				Object.class);
		method.setAccessible(true);

		List<Object> values = new ArrayList<>(Arrays.asList("old-0", "old-1"));
		DynamicBean owner = new DynamicBean("sales", "Order", new HashMap<>(Map.of("items", values)));

		method.invoke(null, owner, "items[1]", owner, "items[1]", "items", "new-1");
		assertEquals("old-0", values.get(0));
		assertEquals("new-1", values.get(1));
	}

	@Test
	@SuppressWarnings("static-method")
	void setIndexedDynamicValueThrowsWhenListIsNull() throws Exception {
		Method method = BindUtil.class.getDeclaredMethod("setIndexedDynamicValue",
				Object.class,
				String.class,
				Bean.class,
				String.class,
				String.class,
				Object.class);
		method.setAccessible(true);

		Map<String, Object> ownerProperties = new HashMap<>();
		ownerProperties.put("items", null);
		DynamicBean owner = new DynamicBean("sales", "Order", ownerProperties);

		assertThrows(InvocationTargetException.class,
				() -> method.invoke(null, owner, "items[0]", owner, "items[0]", "items", "new-value"));
	}

	@Test
	@SuppressWarnings("static-method")
	void invokeStringValueOfReturnsConvertedValueWhenStaticValueOfExists() throws Exception {
		Method method = BindUtil.class.getDeclaredMethod("invokeStringValueOf",
				Class.class,
				Object.class,
				String.class,
				Object.class);
		method.setAccessible(true);

		Object result = method.invoke(null, Integer.class, "42", "count", new Object());
		assertEquals(Integer.valueOf(42), result);
	}

	@Test
	@SuppressWarnings("static-method")
	void invokeStringValueOfThrowsDomainExceptionWhenTypeHasNoValueOf() throws Exception {
		Method method = BindUtil.class.getDeclaredMethod("invokeStringValueOf",
				Class.class,
				Object.class,
				String.class,
				Object.class);
		method.setAccessible(true);

		InvocationTargetException exception = assertThrows(InvocationTargetException.class,
				() -> method.invoke(null, java.util.Date.class, "2026-06-01", "dateField", new Object()));
		assertTrue(exception.getCause() instanceof DomainException);
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
	@SuppressWarnings({"static-method", "boxing"})
	void isDynamicDocumentTrueReturnsTrue() {
		Document document = mock(Document.class);
		when(document.isDynamic()).thenReturn(true);
		assertTrue(BindUtil.isDynamic(null, mock(Module.class), document, null));
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void isDynamicDocumentFalseDelegatesToRelationAndReturnsTrue() {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		when(document.isDynamic()).thenReturn(false);

		AssociationImpl relation = new AssociationImpl();
		relation.setDocumentName("Item");
		Document relatedDocument = mock(Document.class);
		when(relatedDocument.isDynamic()).thenReturn(true);
		when(module.getDocument(null, "Item")).thenReturn(relatedDocument);

		assertTrue(BindUtil.isDynamic(null, module, document, relation));
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void isDynamicRelationReturnsFalseWhenRelatedDocumentStatic() {
		Module module = mock(Module.class);
		AssociationImpl relation = new AssociationImpl();
		relation.setDocumentName("Item");
		Document relatedDocument = mock(Document.class);
		when(relatedDocument.isDynamic()).thenReturn(false);
		when(module.getDocument(null, "Item")).thenReturn(relatedDocument);

		assertFalse(BindUtil.isDynamic(null, module, relation));
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
				() -> BindUtil.populateProperties(mock(User.class), mock(Bean.class), properties, false));

		assertFalse(exception.getMessages().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void populatePropertiesSetsValidPropertyAndSkipsNullKey() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Attribute nameAttribute = mock(Attribute.class);
		User user = mock(User.class);

		Map<String, Object> props = new HashMap<>();
		props.put("name", null);
		DynamicBean bean = new DynamicBean("sales", "Order", props);

		SortedMap<String, Object> properties = new TreeMap<>(Comparator.nullsFirst(String::compareTo));
		properties.put(null, "ignored");
		properties.put("name", "Alpha");

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(document);
		when(document.getAttribute("name")).thenReturn(nameAttribute);
		when(document.getExtends()).thenReturn(null);
		when(nameAttribute.getAttributeType()).thenReturn(Attribute.AttributeType.text);
		doReturn(String.class).when(nameAttribute).getImplementingType();

		assertDoesNotThrow(() -> BindUtil.populateProperties(user, bean, properties, false));
	}

	@Test
	@SuppressWarnings("static-method")
	void populatePropertiesWithOnlyNullKeyCompletesWithoutError() {
		User user = mock(User.class);
		DynamicBean bean = new DynamicBean("sales", "Order", new HashMap<>());
		SortedMap<String, Object> properties = new TreeMap<>(Comparator.nullsFirst(String::compareTo));
		properties.put(null, "ignored");

		assertDoesNotThrow(() -> BindUtil.populateProperties(user, bean, properties, false));
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
	void getDynamicElementByIdReturnsMatchingElement() throws Exception {
		Method method = BindUtil.class.getDeclaredMethod("getDynamicElementById", String.class, int.class, List.class);
		method.setAccessible(true);

		Map<String, Object> firstProps = new HashMap<>();
		firstProps.put(Bean.DOCUMENT_ID, "bean-1");
		DynamicBean first = new DynamicBean("mod", "Item", firstProps);
		Map<String, Object> secondProps = new HashMap<>();
		secondProps.put(Bean.DOCUMENT_ID, "bean-2");
		DynamicBean second = new DynamicBean("mod", "Item", secondProps);
		List<DynamicBean> list = Arrays.asList(first, second);
		String binding = BindUtil.createIdBinding("items", "bean-1");

		Bean result = (Bean) method.invoke(null, binding, Integer.valueOf(binding.indexOf("ElementById(")), list);
		assertSame(first, result);
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

	// ---- fromString with converter for scalar types ----

	@Test
	@SuppressWarnings("static-method")
	void fromStringWithConverterForInteger() throws Exception {
		// Covers fromString L584: !fromSerializedFormat && converter != null
		Converter<Integer> converter = mock(Converter.class);
		when(converter.fromDisplayValue("100")).thenReturn(Integer.valueOf(100));
		Object result = BindUtil.fromString(null, converter, Integer.class, "100");
		assertEquals(Integer.valueOf(100), result);
	}

	@Test
	@SuppressWarnings("static-method")
	void fromStringWithConverterForLong() throws Exception {
		// Covers fromString L584: !fromSerializedFormat && converter != null
		Converter<Long> converter = mock(Converter.class);
		when(converter.fromDisplayValue("200")).thenReturn(Long.valueOf(200L));
		Object result = BindUtil.fromString(null, converter, Long.class, "200");
		assertEquals(Long.valueOf(200L), result);
	}

	@Test
	@SuppressWarnings("static-method")
	void fromStringWithConverterForFloat() throws Exception {
		// Covers fromString L584: !fromSerializedFormat && converter != null
		Converter<Float> converter = mock(Converter.class);
		when(converter.fromDisplayValue("3.14")).thenReturn(Float.valueOf(3.14f));
		Object result = BindUtil.fromString(null, converter, Float.class, "3.14");
		assertEquals(Float.valueOf(3.14f), result);
	}

	@Test
	@SuppressWarnings("static-method")
	void fromStringWithConverterForDouble() throws Exception {
		// Covers fromString L584: !fromSerializedFormat && converter != null
		Converter<Double> converter = mock(Converter.class);
		when(converter.fromDisplayValue("2.718")).thenReturn(Double.valueOf(2.718));
		Object result = BindUtil.fromString(null, converter, Double.class, "2.718");
		assertEquals(Double.valueOf(2.718), result);
	}

	@Test
	@SuppressWarnings("static-method")
	void fromStringWithConverterForBigDecimal() throws Exception {
		// Covers fromString L584: !fromSerializedFormat && converter != null
		Converter<BigDecimal> converter = mock(Converter.class);
		when(converter.fromDisplayValue("123.45")).thenReturn(new BigDecimal("123.45"));
		Object result = BindUtil.fromString(null, converter, BigDecimal.class, "123.45");
		assertEquals(new BigDecimal("123.45"), result);
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
	void toDisplayWithConverterButNullImplementingTypeFallsThroughToStandard() {
		// Covers L741: converter != null but implementingType == null -> falls through to standard display
		Converter<Integer> converter = mock(Converter.class);
		String result = BindUtil.toDisplay(mock(Customer.class), converter, null, null, Integer.valueOf(42));
		// Should fall through to toStandardDisplayValue which calls toString()
		assertEquals("42", result);
	}

	@Test
	@SuppressWarnings("static-method")
	void toDisplayWithNullConverterButImplementingTypeFallsThroughToStandard() {
		// Covers L741: converter == null -> falls through to standard display
		String result = BindUtil.toDisplay(mock(Customer.class), null, Integer.class, null, Integer.valueOf(42));
		// Should fall through to toStandardDisplayValue which calls toString()
		assertEquals("42", result);
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

	// ---- populateProperty — additional type-conversion paths -----------------

	/** Shared helper: build a standard mock chain for populateProperty tests. */
	private static Object[] buildPopulatePropertyMocks(String property) {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Attribute attribute = mock(Attribute.class);
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(document);
		when(document.getAttribute(property)).thenReturn(attribute);
		when(document.getExtends()).thenReturn(null);
		return new Object[]{user, customer, module, document, attribute};
	}

	@Test
	@SuppressWarnings("static-method")
	void populatePropertySetsDecimal2OnDynamicBean() {
		Object[] mocks = buildPopulatePropertyMocks("price");
		User user = (User) mocks[0];
		Attribute attribute = (Attribute) mocks[4];
		doReturn(Decimal2.class).when(attribute).getImplementingType();

		Map<String, Object> props = new HashMap<>();
		props.put("price", new Decimal2("0"));
		DynamicBean bean = new DynamicBean("sales", "Order", props);

		BindUtil.populateProperty(user, bean, "price", "3.14", false);

		assertEquals(new Decimal2("3.14"), bean.get("price"));
	}

	@Test
	@SuppressWarnings("static-method")
	void populatePropertySetsDecimal5OnDynamicBean() {
		Object[] mocks = buildPopulatePropertyMocks("amount");
		User user = (User) mocks[0];
		Attribute attribute = (Attribute) mocks[4];
		doReturn(Decimal5.class).when(attribute).getImplementingType();

		Map<String, Object> props = new HashMap<>();
		props.put("amount", new Decimal5("0"));
		DynamicBean bean = new DynamicBean("sales", "Order", props);

		BindUtil.populateProperty(user, bean, "amount", "1.23456", false);

		assertEquals(new Decimal5("1.23456"), bean.get("amount"));
	}

	@Test
	@SuppressWarnings("static-method")
	void populatePropertySetsDecimal10OnDynamicBean() {
		Object[] mocks = buildPopulatePropertyMocks("total");
		User user = (User) mocks[0];
		Attribute attribute = (Attribute) mocks[4];
		doReturn(Decimal10.class).when(attribute).getImplementingType();

		Map<String, Object> props = new HashMap<>();
		props.put("total", new Decimal10("0"));
		DynamicBean bean = new DynamicBean("sales", "Order", props);

		BindUtil.populateProperty(user, bean, "total", "9.1234567890", false);

		assertEquals(new Decimal10("9.1234567890"), bean.get("total"));
	}

	@Test
	@SuppressWarnings("static-method")
	void populatePropertySetsBooleanOnDynamicBean() {
		Object[] mocks = buildPopulatePropertyMocks("active");
		User user = (User) mocks[0];
		Attribute attribute = (Attribute) mocks[4];
		doReturn(Boolean.class).when(attribute).getImplementingType();

		Map<String, Object> props = new HashMap<>();
		props.put("active", Boolean.FALSE);
		DynamicBean bean = new DynamicBean("sales", "Order", props);

		BindUtil.populateProperty(user, bean, "active", "true", false);

		assertEquals(Boolean.TRUE, bean.get("active")); // NOSONAR - test assertion
	}

	@Test
	@SuppressWarnings("static-method")
	void populatePropertySetsLongOnDynamicBean() {
		Object[] mocks = buildPopulatePropertyMocks("count");
		User user = (User) mocks[0];
		Attribute attribute = (Attribute) mocks[4];
		doReturn(Long.class).when(attribute).getImplementingType();

		Map<String, Object> props = new HashMap<>();
		props.put("count", Long.valueOf(0L));
		DynamicBean bean = new DynamicBean("sales", "Order", props);

		BindUtil.populateProperty(user, bean, "count", "999", false);

		assertEquals(Long.valueOf(999L), bean.get("count"));
	}

	@Test
	@SuppressWarnings("static-method")
	void populatePropertySetsDateOnlyFromSerializedFormat() {
		// fromSerializedFormat=true → DateOnly(stringValue) constructor path
		Object[] mocks = buildPopulatePropertyMocks("birthDate");
		User user = (User) mocks[0];
		Attribute attribute = (Attribute) mocks[4];
		doReturn(DateOnly.class).when(attribute).getImplementingType();

		Map<String, Object> props = new HashMap<>();
		props.put("birthDate", new DateOnly(0L));
		DynamicBean bean = new DynamicBean("sales", "Order", props);

		BindUtil.populateProperty(user, bean, "birthDate", "2022-01-01", true);

		assertNotNull(bean.get("birthDate"));
		assertTrue(bean.get("birthDate") instanceof DateOnly);
	}

	@Test
	@SuppressWarnings("static-method")
	void populatePropertyWithStringArrayValueSetsProperty() {
		// Covers: value instanceof String[] array branch in convertPopulateIncomingValue
		Object[] mocks = buildPopulatePropertyMocks("name");
		User user = (User) mocks[0];
		Attribute attribute = (Attribute) mocks[4];
		doReturn(String.class).when(attribute).getImplementingType();

		Map<String, Object> props = new HashMap<>();
		props.put("name", "before");
		DynamicBean bean = new DynamicBean("sales", "Order", props);

		BindUtil.populateProperty(user, bean, "name", new String[]{"  hello  "}, false);

		assertEquals("hello", bean.get("name"));
	}

	@Test
	@SuppressWarnings("static-method")
	void populatePropertyWithNonStringValuePassesThroughDirectly() {
		// Covers: stringValue == null (non-string, non-String[]) → return value as-is
		Object[] mocks = buildPopulatePropertyMocks("active");
		User user = (User) mocks[0];
		Attribute attribute = (Attribute) mocks[4];
		doReturn(Boolean.class).when(attribute).getImplementingType();

		Map<String, Object> props = new HashMap<>();
		props.put("active", Boolean.FALSE);
		DynamicBean bean = new DynamicBean("sales", "Order", props);

		BindUtil.populateProperty(user, bean, "active", Boolean.TRUE, false);

		assertEquals(Boolean.TRUE, bean.get("active"));
	}

	@Test
	@SuppressWarnings("static-method")
	void populatePropertyWithNullValueSetsNull() {
		// Covers: value == null → convertPopulateIncomingValue returns null → sets null
		Object[] mocks = buildPopulatePropertyMocks("name");
		User user = (User) mocks[0];
		Attribute attribute = (Attribute) mocks[4];
		doReturn(String.class).when(attribute).getImplementingType();

		Map<String, Object> props = new HashMap<>();
		props.put("name", "existing");
		DynamicBean bean = new DynamicBean("sales", "Order", props);

		BindUtil.populateProperty(user, bean, "name", (Object) null, false);

		assertNull(bean.get("name"));
	}

	@Test
	@SuppressWarnings("static-method")
	void populatePropertyWithStringArrayNullFirstElementPassesThroughArray() {
		// Covers: value instanceof String[] array → array[0] == null → stringValue stays null
		// → stringValue == null → return value (the array itself)
		Object[] mocks = buildPopulatePropertyMocks("active");
		User user = (User) mocks[0];
		Attribute attribute = (Attribute) mocks[4];
		doReturn(Boolean.class).when(attribute).getImplementingType();

		Map<String, Object> props = new HashMap<>();
		props.put("active", Boolean.FALSE);
		DynamicBean bean = new DynamicBean("sales", "Order", props);

		// String[] with null first element → passthrough (the array is returned as-is
		// and convertAndSet coerces it to Boolean - but the primary branch is covered)
		assertDoesNotThrow(() -> BindUtil.populateProperty(user, bean, "active", new String[]{null}, false));
	}

	// ---- setAssociation -------------------------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void setAssociationSetsValueOnStaticBean() {
		// Covers: setAssociation static (isDynamic=false) path → set(associationOwner, name, value)
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Attribute attribute = mock(Attribute.class);
		User user = mock(User.class);

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(document);
		when(document.getPolymorphicAttribute(customer, "supplier")).thenReturn(attribute);
		// attribute.isDynamic() returns false by default → static path

		Map<String, Object> props = new HashMap<>();
		props.put("supplier", null);
		DynamicBean bean = new DynamicBean("sales", "Order", props);
		DynamicBean newSupplier = new DynamicBean("sales", "Supplier", new HashMap<>());

		withThreadLocalUser(user, () -> BindUtil.setAssociation(bean, "supplier", newSupplier));

		assertSame(newSupplier, bean.get("supplier"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setAssociationThrowsMetaDataExceptionForInvalidBinding() {
		// Covers: a == null → throw MetaDataException
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		User user = mock(User.class);

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(document);
		when(document.getPolymorphicAttribute(customer, "unknown")).thenReturn(null);

		Map<String, Object> props = new HashMap<>();
		props.put("unknown", null);
		DynamicBean bean = new DynamicBean("sales", "Order", props);
		DynamicBean newValue = new DynamicBean("sales", "Target", new HashMap<>());

		assertThrows(MetaDataException.class,
				() -> withThreadLocalUser(user, () -> BindUtil.setAssociation(bean, "unknown", newValue)));
	}

	// ---- getPropertyType — compound binding ----------------------------------
	// (compound binding tests for getPropertyType already exist above; this section intentionally omitted)

	// ---- populateProperty — resolvePopulateMetadata branches ----------------

	@Test
	@SuppressWarnings("static-method")
	void populatePropertyWithEmptyStringCoversResolveMetadataElseBranch() {
		Object[] mocks = buildPopulatePropertyMocks("name");
		User user = (User) mocks[0];
		Attribute attribute = (Attribute) mocks[4];
		doReturn(String.class).when(attribute).getImplementingType();

		Map<String, Object> props = new HashMap<>();
		props.put("name", "existing");
		DynamicBean bean = new DynamicBean("sales", "Order", props);

		BindUtil.populateProperty(user, bean, "name", "", false);

		assertNull(bean.get("name"));
	}

	@Test
	@SuppressWarnings("static-method")
	void populatePropertyWithConvertibleFieldAttributeUsesNullConverter() {
		// Covers: attribute instanceof ConvertibleField cf → converter = cf.getConverterForCustomer(customer)
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		ConvertibleField cf = mock(ConvertibleField.class);
		User user = mock(User.class);

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(document);
		when(document.getAttribute("name")).thenReturn(cf);
		when(document.getExtends()).thenReturn(null);
		doReturn(String.class).when(cf).getImplementingType();
		when(cf.getConverterForCustomer(customer)).thenReturn(null);

		Map<String, Object> props = new HashMap<>();
		props.put("name", "before");
		DynamicBean bean = new DynamicBean("sales", "Order", props);

		BindUtil.populateProperty(user, bean, "name", "hello", false);

		assertEquals("hello", bean.get("name"));
	}

	// ---- formatMessage(Function, ...) null branch --------------------------

	@Test
	@SuppressWarnings("static-method")
	void formatMessageWithNullFunctionPostProcessorPassesThroughDisplay() {
		Map<String, Object> props = new HashMap<>();
		props.put("name", "world");
		DynamicBean bean = new DynamicBean("mod", "Doc", props);
		String result = BindUtil.formatMessage("Hello", (Function<String, String>) null, bean);
		assertEquals("Hello", result);
	}

	// ---- fromString - parseGeometryFromString catch / SkyveException rethrow ----

	@Test
	@SuppressWarnings("static-method")
	void fromStringWithInvalidGeometryThrowsDomainException() {
		assertThrows(DomainException.class,
				() -> BindUtil.fromString(null, null, Geometry.class, "not valid wkt at all"));
	}

	// ---- toDisplay — Boolean TRUE / converter-only / catch block -----------

	@Test
	@SuppressWarnings("static-method")
	void toDisplayBooleanTrueReturnsYes() {
		String result = BindUtil.toDisplay(mock(Customer.class), null, null, null, Boolean.TRUE);
		assertEquals("Yes", result);
	}

	@Test
	@SuppressWarnings({"static-method", "rawtypes"})
	void toDisplayWithNonNullConverterAndNullImplementingTypeUsesStandardDisplay() {
		Converter converter = mock(Converter.class);
		String result = BindUtil.toDisplay(mock(Customer.class), converter, null, null, "hello");
		assertEquals("hello", result);
	}

	@Test
	@SuppressWarnings({"static-method", "unchecked", "rawtypes"})
	void toDisplayCatchBlockRethrowsSkyveException() {
		Converter converter = mock(Converter.class);
		DomainException expected = new DomainException("skyve-error");
		when(converter.toDisplayValue("hello")).thenThrow(expected);
		DomainException actual = assertThrows(DomainException.class,
				() -> BindUtil.toDisplay(mock(Customer.class), converter, String.class, null, "hello"));
		assertSame(expected, actual);
	}

	@Test
	@SuppressWarnings({"static-method", "unchecked", "rawtypes"})
	void toDisplayCatchBlockWrapsNonSkyveExceptionAsDomainException() {
		Converter converter = mock(Converter.class);
		when(converter.toDisplayValue("hello")).thenThrow(new RuntimeException("oops"));
		assertThrows(DomainException.class,
				() -> BindUtil.toDisplay(mock(Customer.class), converter, String.class, null, "hello"));
	}

	// ---- getDisplay — resolveDisplaySettings uncovered branches ------------

	@Test
	@SuppressWarnings("static-method")
	void getDisplayWithNullDocumentNameReturnsValueToString() {
		Bean bean = mock(Bean.class);
		when(bean.getBizDocument()).thenReturn(null);
		String result = BindUtil.getDisplay(mock(Customer.class), bean, "name", "hello");
		assertEquals("hello", result);
	}

	@Test
	@SuppressWarnings("static-method")
	void getDisplayWithUnresolvableAttributeReturnsValueToString() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(document);
		when(document.getAttribute("name")).thenReturn(null);
		when(document.getExtends()).thenReturn(null);

		Map<String, Object> props = new HashMap<>();
		props.put("name", "hello");
		DynamicBean bean = new DynamicBean("sales", "Order", props);

		String result = BindUtil.getDisplay(customer, bean, "name");
		assertEquals("hello", result);
	}

	// ---- ensureElementIsInCollection ---------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void ensureElementIsInCollectionReturnsExistingWhenFound() {
		Map<String, Object> existingProps = new HashMap<>();
		existingProps.put(Bean.DOCUMENT_ID, "bean-1");
		DynamicBean existing = new DynamicBean("sales", "Item", existingProps);
		List<Bean> items = new ArrayList<>(Arrays.asList(existing));
		DynamicBean owner = new DynamicBean("sales", "Order", new HashMap<>(Map.of("items", items)));

		Map<String, Object> elementProps = new HashMap<>();
		elementProps.put(Bean.DOCUMENT_ID, "bean-1");
		DynamicBean element = new DynamicBean("sales", "Item", elementProps);

		Bean result = BindUtil.ensureElementIsInCollection(owner, "items", element);
		assertSame(existing, result);
		assertEquals(1, items.size());
	}

	// ---- getElementInCollection(Bean, String, String) ----------------------

	@Test
	@SuppressWarnings("static-method")
	void getElementInCollectionBeanOwnerFindsMatchingBean() {
		Map<String, Object> p = new HashMap<>();
		p.put(Bean.DOCUMENT_ID, "bean-1");
		DynamicBean bean = new DynamicBean("sales", "Item", p);
		List<Bean> items = new ArrayList<>(Arrays.asList(bean));
		DynamicBean owner = new DynamicBean("sales", "Order", new HashMap<>(Map.of("items", items)));

		Bean result = BindUtil.getElementInCollection(owner, "items", "bean-1");
		assertSame(bean, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void getElementInCollectionBeanOwnerReturnsNullWhenNotFound() {
		Map<String, Object> p = new HashMap<>();
		p.put(Bean.DOCUMENT_ID, "bean-1");
		DynamicBean bean = new DynamicBean("sales", "Item", p);
		List<Bean> items = new ArrayList<>(Arrays.asList(bean));
		DynamicBean owner = new DynamicBean("sales", "Order", new HashMap<>(Map.of("items", items)));

		assertNull(BindUtil.getElementInCollection(owner, "items", "no-such-id"));
	}

	// ---- orderCollectionByMetaData -----------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void orderCollectionByMetaDataChildOrderedSortsByOrdinal() {
		DynamicBean first = new DynamicBean("sales", "Item", new HashMap<>(Map.of(Bean.ORDINAL_NAME, Integer.valueOf(2))));
		DynamicBean second = new DynamicBean("sales", "Item", new HashMap<>(Map.of(Bean.ORDINAL_NAME, Integer.valueOf(1))));
		List<Bean> items = new ArrayList<>(Arrays.asList(first, second));
		DynamicBean owner = new DynamicBean("sales", "Order", new HashMap<>(Map.of("items", items)));

		CollectionImpl collection = new CollectionImpl();
		collection.setName("items");
		collection.setType(CollectionType.child);
		collection.setOrdered(Boolean.TRUE);

		BindUtil.orderCollectionByMetaData(owner, collection);

		assertEquals(Integer.valueOf(1), ((DynamicBean) items.get(0)).get(Bean.ORDINAL_NAME));
		assertEquals(Integer.valueOf(2), ((DynamicBean) items.get(1)).get(Bean.ORDINAL_NAME));
	}

	@Test
	@SuppressWarnings("static-method")
	void orderCollectionByMetaDataWithExplicitOrderingSortsByOrdering() {
		DynamicBean first = new DynamicBean("sales", "Item", new HashMap<>(Map.of("name", "Zulu")));
		DynamicBean second = new DynamicBean("sales", "Item", new HashMap<>(Map.of("name", "Alpha")));
		List<Bean> items = new ArrayList<>(Arrays.asList(first, second));
		DynamicBean owner = new DynamicBean("sales", "Order", new HashMap<>(Map.of("items", items)));

		CollectionImpl collection = new CollectionImpl();
		collection.setName("items");
		collection.setType(CollectionType.child);
		collection.setOrdered(Boolean.FALSE);
		collection.getOrdering().add(new OrderingImpl("name", SortDirection.ascending));

		BindUtil.orderCollectionByMetaData(owner, collection);

		assertEquals("Alpha", ((DynamicBean) items.get(0)).get("name"));
		assertEquals("Zulu", ((DynamicBean) items.get(1)).get("name"));
	}

	@Test
	@SuppressWarnings("static-method")
	void orderCollectionByMetaDataNoOrderingLeavesListUnchanged() {
		DynamicBean first = new DynamicBean("sales", "Item", new HashMap<>(Map.of("name", "Zulu")));
		DynamicBean second = new DynamicBean("sales", "Item", new HashMap<>(Map.of("name", "Alpha")));
		List<Bean> items = new ArrayList<>(Arrays.asList(first, second));
		DynamicBean owner = new DynamicBean("sales", "Order", new HashMap<>(Map.of("items", items)));

		CollectionImpl collection = new CollectionImpl();
		collection.setName("items");
		collection.setType(CollectionType.child);
		collection.setOrdered(Boolean.FALSE);

		BindUtil.orderCollectionByMetaData(owner, collection);

		assertEquals("Zulu", ((DynamicBean) items.get(0)).get("name"));
		assertEquals("Alpha", ((DynamicBean) items.get(1)).get("name"));
	}

	// ---- orderByMetaData — compound binding path ---------------------------

	@Test
	@SuppressWarnings("static-method")
	void orderByMetaDataCompoundBindingNavigatesToNestedCollection() {
		Customer customer = mock(Customer.class);
		User user = mock(User.class);
		Module module = mock(Module.class);
		Document childDocument = mock(Document.class);
		Document rootDocument = mock(Document.class);
		CollectionImpl collection = new CollectionImpl();
		collection.setName("lines");
		collection.setType(CollectionType.child);
		collection.setOrdered(Boolean.TRUE);

		DynamicBean firstLine = new DynamicBean("sales", "Line", new HashMap<>(Map.of(Bean.ORDINAL_NAME, Integer.valueOf(2))));
		DynamicBean secondLine = new DynamicBean("sales", "Line", new HashMap<>(Map.of(Bean.ORDINAL_NAME, Integer.valueOf(1))));
		List<Bean> lines = new ArrayList<>(Arrays.asList(firstLine, secondLine));
		DynamicBean child = new DynamicBean("sales", "Child", new HashMap<>(Map.of("lines", lines)));
		DynamicBean root = new DynamicBean("sales", "Order", new HashMap<>(Map.of("child", child)));

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(rootDocument);
		when(module.getDocument(customer, "Child")).thenReturn(childDocument);
		doReturn(collection).when(childDocument).getPolymorphicAttribute(customer, "lines");
		when(childDocument.getAttribute("lines")).thenReturn(collection);
		when(childDocument.getExtends()).thenReturn(null);

		withThreadLocalUser(user, () -> BindUtil.orderByMetaData(root, "child.lines"));

		assertEquals(Integer.valueOf(1), ((DynamicBean) lines.get(0)).get(Bean.ORDINAL_NAME));
		assertEquals(Integer.valueOf(2), ((DynamicBean) lines.get(1)).get(Bean.ORDINAL_NAME));
	}

	// ---- orderByMetaData — inverseMany path --------------------------------

	@Test
	@SuppressWarnings("static-method")
	void orderByMetaDataInverseManyOrdersByMetadata() {
		Customer customer = mock(Customer.class);
		User user = mock(User.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		InverseMany inverse = new InverseMany();
		inverse.setName("peers");
		inverse.getOrdering().add(new OrderingImpl("name", SortDirection.ascending));

		DynamicBean first = new DynamicBean("sales", "Peer", new HashMap<>(Map.of("name", "Zulu")));
		DynamicBean second = new DynamicBean("sales", "Peer", new HashMap<>(Map.of("name", "Alpha")));
		List<Bean> peers = new ArrayList<>(Arrays.asList(first, second));
		DynamicBean owner = new DynamicBean("sales", "Order", new HashMap<>(Map.of("peers", peers)));

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(document);
		doReturn(inverse).when(document).getPolymorphicAttribute(customer, "peers");
		when(document.getAttribute("peers")).thenReturn(inverse);
		when(document.getExtends()).thenReturn(null);

		withThreadLocalUser(user, () -> BindUtil.orderByMetaData(owner, "peers"));

		assertEquals("Alpha", ((DynamicBean) peers.get(0)).get("name"));
		assertEquals("Zulu", ((DynamicBean) peers.get(1)).get("name"));
	}

	// ---- order — unsorted list path ----------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void orderUnsortedListSortsByOrdering() {
		NamedItem a = new NamedItem("zebra");
		NamedItem b = new NamedItem("apple");
		NamedItem c = new NamedItem("mango");
		List<NamedItem> list = new ArrayList<>(Arrays.asList(a, b, c));

		BindUtil.order(list, new OrderingImpl("name", SortDirection.ascending));

		assertEquals("apple", list.get(0).getName());
		assertEquals("mango", list.get(1).getName());
		assertEquals("zebra", list.get(2).getName());
	}


	// ---- normalize helpers / metadata navigation guard — focused coverage additions ----

	@Test
	@SuppressWarnings("static-method")
	void normaliseBindingTokenRemovesIndexedAndMappedSegments() throws Exception {
		Method method = BindUtil.class.getDeclaredMethod("normaliseBindingToken", String.class);
		method.setAccessible(true);

		assertEquals("items", method.invoke(null, "items[0]"));
		assertEquals("children", method.invoke(null, "childrenElementById(abc)"));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertNumericWrapperReturnsInputForNonNumber() {
		Object result = BindUtil.convert(Integer.class, "hello");
		assertEquals("hello", result);
	}

	@Test
	@SuppressWarnings("static-method")
	void convertTemporalTypeReturnsInputForNonDate() {
		Object result = BindUtil.convert(org.skyve.domain.types.DateOnly.class, "not-a-date");
		assertEquals("not-a-date", result);
	}

	@Test
	@SuppressWarnings("static-method")
	void resolveNavigatingModuleFallsBackToProvidedRepositoryWhenCustomerIsNull() throws Exception {
		Method method = BindUtil.class.getDeclaredMethod("resolveNavigatingModule", org.skyve.metadata.customer.Customer.class, Document.class);
		method.setAccessible(true);

		Module expected = mock(Module.class);
		Document document = mock(Document.class);
		when(document.getOwningModuleName()).thenReturn("sales");

		org.skyve.metadata.repository.ProvidedRepository previous = ProvidedRepositoryFactory.get();
		org.skyve.metadata.repository.ProvidedRepository repository = mock(org.skyve.metadata.repository.ProvidedRepository.class);
		when(repository.getModule(null, "sales")).thenReturn(expected);

		try {
			ProvidedRepositoryFactory.set(repository);
			Module actual = (Module) method.invoke(null, (Object) null, document);
			assertSame(expected, actual);
		}
		finally {
			ProvidedRepositoryFactory.set(previous);
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void resolveTerminalBindingTypeUsesRelationImplementingTypeIfRelationDocumentIsNotGenerated() throws Exception {
		Method method = BindUtil.class.getDeclaredMethod("resolveTerminalBindingType",
				org.skyve.metadata.customer.Customer.class,
				Module.class,
				Document.class,
				String.class,
				String.class,
				org.skyve.metadata.model.Attribute.class);
		method.setAccessible(true);

		org.skyve.metadata.customer.Customer customer = mock(org.skyve.metadata.customer.Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		org.skyve.metadata.model.document.Relation relation = mock(org.skyve.metadata.model.document.Relation.class);
		Document relatedDocument = mock(Document.class);

		when(document.getOwningModuleName()).thenReturn("sales");
		when(document.getName()).thenReturn("Order");
		when(relation.getDocumentName()).thenReturn("Child");
		when(relation.getImplementingType()).thenReturn(null);
		when(module.getDocument(customer, "Child")).thenReturn(relatedDocument);

		Object type = method.invoke(null,
				customer,
				module,
				document,
				"Order.child",
				"child",
				relation);

		assertEquals(Bean.class, type);
	}

	@Test
	@SuppressWarnings("static-method")
	void resolveTerminalBindingTypeWrapsClassNotFoundInMetadataException() throws Exception {
		Method method = BindUtil.class.getDeclaredMethod("resolveTerminalBindingType",
				org.skyve.metadata.customer.Customer.class,
				Module.class,
				Document.class,
				String.class,
				String.class,
				org.skyve.metadata.model.Attribute.class);
		method.setAccessible(true);

		org.skyve.metadata.customer.Customer customer = mock(org.skyve.metadata.customer.Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		org.skyve.metadata.model.document.Relation relation = mock(org.skyve.metadata.model.document.Relation.class);
		DocumentImpl relationDocument = new DocumentImpl();
		relationDocument.setOwningModuleName("sales");
		relationDocument.setName("Missing");

		when(document.getOwningModuleName()).thenReturn("sales");
		when(document.getName()).thenReturn("Order");
		when(relation.getDocumentName()).thenReturn("Missing");
		when(relation.getName()).thenReturn("missing");
		when(module.getDocument(customer, "Missing")).thenReturn(relationDocument);

		org.skyve.metadata.repository.ProvidedRepository previousRepository = ProvidedRepositoryFactory.get();
		org.skyve.metadata.repository.ProvidedRepository repository = mock(org.skyve.metadata.repository.ProvidedRepository.class);
		when(customer.getName()).thenReturn("acme");
		when(repository.vtable(anyString(), anyString())).thenReturn(null);

		try {
			ProvidedRepositoryFactory.set(repository);
			InvocationTargetException exception = assertThrows(
					InvocationTargetException.class,
					() -> method.invoke(null,
							customer,
							module,
							document,
							"Order.missing",
							"missing",
							relation));
			assertTrue(exception.getCause() instanceof MetaDataException);
		}
		finally {
			ProvidedRepositoryFactory.set(previousRepository);
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void evaluateConditionUsesMetadataExpressionAndNegation() {
		Customer customer = mock(Customer.class);
		User user = mock(User.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Condition condition = mock(Condition.class);
		when(condition.getExpression()).thenReturn("{flag}");

		Map<String, Object> values = new HashMap<>();
		values.put("flag", Boolean.TRUE);
		DynamicBean bean = new DynamicBean("sales", "Order", values);

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(document);
		when(document.getCondition("active")).thenReturn(condition);

		withThreadLocalUser(user, () -> {
			assertFalse(BindUtil.evaluateCondition(bean, "notActive"));
			assertTrue(BindUtil.evaluateCondition(bean, "active"));
		});
	}

	// ---- private binding helper branches — focused coverage additions ----

	@Test
	@SuppressWarnings("static-method")
	void parsePropertySelectorForIndexedBindingReturnsExpectedIndexAndName() throws Exception {
		Method method = BindUtil.class.getDeclaredMethod("parsePropertySelector", String.class);
		method.setAccessible(true);

		Object selector = method.invoke(null, "items[7]");

		assertEquals("items", ((BindUtil.PropertySelector) selector).propertyName);
		assertEquals(7, ((BindUtil.PropertySelector) selector).index);
		assertEquals(null, ((BindUtil.PropertySelector) selector).key);
	}

	@Test
	@SuppressWarnings("static-method")
	void parsePropertySelectorForMappedBindingReturnsExpectedKeyAndName() throws Exception {
		Method method = BindUtil.class.getDeclaredMethod("parsePropertySelector", String.class);
		method.setAccessible(true);

		BindUtil.PropertySelector selector = (BindUtil.PropertySelector) method.invoke(null, "customers(customer-1)");

		assertEquals("customers", selector.propertyName);
		assertEquals(-1, selector.index);
		assertEquals("customer-1", selector.key);
	}

	@Test
	@SuppressWarnings("static-method")
	void resolveParentDocumentNameUsesExtendsChain() throws Exception {
		Method method = BindUtil.class.getDeclaredMethod("resolveParentDocumentName",
				org.skyve.metadata.customer.Customer.class,
				Module.class,
				Document.class,
				String.class,
				String.class);
		method.setAccessible(true);

		org.skyve.metadata.customer.Customer customer = mock(org.skyve.metadata.customer.Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Document baseDocument = mock(Document.class);
		Extends base = mock(Extends.class);

		when(document.getOwningModuleName()).thenReturn("sales");
		when(document.getName()).thenReturn("OrderItem");
		when(document.getParentDocumentName()).thenReturn(null);
		when(document.getExtends()).thenReturn(base);
		when(base.getDocumentName()).thenReturn("Order");
		when(module.getDocument(customer, "Order")).thenReturn(baseDocument);
		when(baseDocument.getParentDocumentName()).thenReturn("Customer");
		when(baseDocument.getExtends()).thenReturn(null);

		String parent = (String) method.invoke(null, customer, module, document, ChildBean.PARENT_NAME, "sales.OrderItem.parent");
		assertEquals("Customer", parent);
	}

	@Test
	@SuppressWarnings("static-method")
	void resolveParentDocumentNameThrowsWhenNoParentDocument() throws Exception {
		Method method = BindUtil.class.getDeclaredMethod("resolveParentDocumentName",
				org.skyve.metadata.customer.Customer.class,
				Module.class,
				Document.class,
				String.class,
				String.class);
		method.setAccessible(true);

		org.skyve.metadata.customer.Customer customer = mock(org.skyve.metadata.customer.Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);

		when(document.getOwningModuleName()).thenReturn("sales");
		when(document.getName()).thenReturn("OrderItem");
		when(document.getParentDocumentName()).thenReturn(null);
		when(document.getExtends()).thenReturn(null);

		InvocationTargetException ex = assertThrows(InvocationTargetException.class, () -> {
			method.invoke(null, customer, module, document, ChildBean.PARENT_NAME, "sales.OrderItem.parent");
		});
		assertTrue(ex.getCause() instanceof MetaDataException);
	}

	@Test
	@SuppressWarnings("static-method")
	void resolveNextNavigatingDocumentReturnsParentWhenBindingIsParent() throws Exception {
		Method method = BindUtil.class.getDeclaredMethod("resolveNextNavigatingDocument",
				org.skyve.metadata.customer.Customer.class,
				Module.class,
				Document.class,
				String.class,
				String.class,
				org.skyve.metadata.model.Attribute.class,
				String.class);
		method.setAccessible(true);

		org.skyve.metadata.customer.Customer customer = mock(org.skyve.metadata.customer.Customer.class);
		Module module = mock(Module.class);
		Document navigatingDocument = mock(Document.class);
		Document parentDocument = mock(Document.class);

		when(module.getDocument(customer, "Customer")).thenReturn(parentDocument);

		Object result = method.invoke(null,
				customer,
				module,
				navigatingDocument,
				ChildBean.PARENT_NAME,
				"Customer",
				null,
				"sales.OrderItem.parent");
		assertSame(parentDocument, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void resolveNextNavigatingDocumentThrowsForInvalidRelationTarget() throws Exception {
		Method method = BindUtil.class.getDeclaredMethod("resolveNextNavigatingDocument",
				org.skyve.metadata.customer.Customer.class,
				Module.class,
				Document.class,
				String.class,
				String.class,
				org.skyve.metadata.model.Attribute.class,
				String.class);
		method.setAccessible(true);

		org.skyve.metadata.customer.Customer customer = mock(org.skyve.metadata.customer.Customer.class);
		Module module = mock(Module.class);
		Document navigatingDocument = mock(Document.class);
		Attribute attribute = mock(Attribute.class);

		when(navigatingDocument.getOwningModuleName()).thenReturn("sales");
		when(navigatingDocument.getName()).thenReturn("Order");

		InvocationTargetException ex = assertThrows(InvocationTargetException.class, () -> {
			method.invoke(null,
					customer,
					module,
					navigatingDocument,
					"invalid",
					null,
					attribute,
					"sales.Order.invalid");
		});
		assertTrue(ex.getCause() instanceof MetaDataException);
	}

	@Test
	@SuppressWarnings("static-method")
	void evaluateResolvedConditionExpressionHonoursNegation() throws Exception {
		Method method = BindUtil.class.getDeclaredMethod("evaluateResolvedConditionExpression",
				Bean.class,
				String.class,
				boolean.class);
		method.setAccessible(true);

		Map<String, Object> properties = new HashMap<>();
		properties.put("flag", Boolean.TRUE);
		DynamicBean bean = new DynamicBean("sales", "Order", properties);

		assertEquals(Boolean.FALSE, method.invoke(null, bean, "{flag}", Boolean.TRUE));
		assertEquals(Boolean.TRUE, method.invoke(null, bean, "{flag}", Boolean.FALSE));
	}

	@Test
	@SuppressWarnings("static-method")
	void setDynamicIndexedBindingThroughPublicSetUpdatesListElement() {
		List<Object> values = new ArrayList<>(Arrays.asList("before", "target"));
		DynamicBean bean = new DynamicBean("sales", "Order", new HashMap<>(Map.of("items", values)));

		BindUtil.set(bean, "items[1]", "after");

		assertEquals("after", values.get(1));
	}

	@Test
	@SuppressWarnings("static-method")
	void setDynamicElementByIdBindingThroughPublicSetReplacesElement() {
		DynamicBean first = new DynamicBean("sales", "Item", new HashMap<>(Map.of(Bean.DOCUMENT_ID, "one")));
		DynamicBean second = new DynamicBean("sales", "Item", new HashMap<>(Map.of(Bean.DOCUMENT_ID, "two")));
		DynamicBean replacement = new DynamicBean("sales", "Item", new HashMap<>(Map.of(Bean.DOCUMENT_ID, "two")));
		List<Bean> values = new ArrayList<>(Arrays.asList(first, second));
		DynamicBean bean = new DynamicBean("sales", "Order", new HashMap<>(Map.of("items", values)));

		BindUtil.set(bean, BindUtil.createIdBinding("items", "two"), replacement);

		assertSame(replacement, values.get(1));
	}

	@Test
	@SuppressWarnings("static-method")
	void setDynamicElementByIdBindingThroughPublicSetRejectsNonBeanValue() {
		DynamicBean first = new DynamicBean("sales", "Item", new HashMap<>(Map.of(Bean.DOCUMENT_ID, "one")));
		DynamicBean bean = new DynamicBean("sales", "Order", new HashMap<>(Map.of("items", new ArrayList<>(List.of(first)))));

		MetaDataException exception = assertThrows(MetaDataException.class,
				() -> BindUtil.set(bean, BindUtil.createIdBinding("items", "one"), "not a bean"));

		assertNotNull(exception);
	}

	@Test
	@SuppressWarnings("static-method")
	void getPropertyTypeDynamicListPropertyReturnsConcreteListClassWhenValueExists() {
		ArrayList<Object> values = new ArrayList<>();
		DynamicBean bean = new DynamicBean("sales", "Order", new HashMap<>(Map.of("items", values)));

		assertSame(ArrayList.class, BindUtil.getPropertyType(bean, "items"));
	}

	@Test
	@SuppressWarnings("static-method")
	void isMutableDynamicScalarReturnsTrueAndDynamicListReturnsFalse() {
		DynamicBean bean = new DynamicBean("sales",
				"Order",
				new HashMap<>(Map.of("name", "Alpha", "items", new ArrayList<>())));

		assertTrue(BindUtil.isMutable(bean, "name"));
		assertFalse(BindUtil.isMutable(bean, "items"));
	}

	@Test
	@SuppressWarnings("static-method")
	void getMetaDataForBindingTerminalParentReturnsImplicitBeanType() {
		Module module = mock(Module.class);
		Document document = mock(Document.class);

		when(document.getParentDocumentName()).thenReturn("Order");
		when(document.getAttribute(ChildBean.PARENT_NAME)).thenReturn(null);
		when(document.getExtends()).thenReturn(null);

		TargetMetaData target = BindUtil.getMetaDataForBinding(null, module, document, ChildBean.PARENT_NAME);

		assertSame(document, target.getDocument());
		assertNull(target.getAttribute());
		assertSame(Bean.class, target.getType());
	}

	@Test
	@SuppressWarnings("static-method")
	void getMetaDataForBindingResolvesParentDocumentNameFromBaseDocument() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document childDocument = mock(Document.class);
		Document baseDocument = mock(Document.class);
		Document parentDocument = mock(Document.class);
		Extends inherits = mock(Extends.class);
		Attribute name = mock(Attribute.class);

		when(childDocument.getParentDocumentName()).thenReturn(null);
		when(childDocument.getExtends()).thenReturn(inherits);
		when(inherits.getDocumentName()).thenReturn("BaseChild");
		when(module.getDocument(customer, "BaseChild")).thenReturn(baseDocument);
		when(baseDocument.getParentDocumentName()).thenReturn("Parent");
		when(baseDocument.getExtends()).thenReturn(null);
		when(module.getDocument(customer, "Parent")).thenReturn(parentDocument);
		when(parentDocument.getOwningModuleName()).thenReturn("sales");
		when(customer.getModule("sales")).thenReturn(module);
		when(parentDocument.getAttribute("name")).thenReturn(name);
		when(parentDocument.getExtends()).thenReturn(null);
		doReturn(String.class).when(name).getImplementingType();

		TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, childDocument, "parent.name");

		assertSame(parentDocument, target.getDocument());
		assertSame(name, target.getAttribute());
		assertSame(String.class, target.getType());
	}

	@Test
	@SuppressWarnings("static-method")
	void getMetaDataForBindingTerminalRelationFallsBackToRelationImplementingTypeForNonDocumentImpl() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Document relationDocument = mock(Document.class);
		Relation relation = mock(Relation.class);

		when(document.getAttribute("child")).thenReturn(relation);
		when(document.getExtends()).thenReturn(null);
		when(relation.getDocumentName()).thenReturn("Child");
		when(module.getDocument(customer, "Child")).thenReturn(relationDocument);
		doReturn(SampleChildBean.class).when(relation).getImplementingType();

		TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, "child");

		assertSame(document, target.getDocument());
		assertSame(relation, target.getAttribute());
		assertSame(SampleChildBean.class, target.getType());
	}

	@Test
	@SuppressWarnings("static-method")
	void privateSetPopulateValueSetsIndexedAndMappedProperties() throws Exception {
		Method parse = BindUtil.class.getDeclaredMethod("parsePropertySelector", String.class);
		parse.setAccessible(true);
		Class<?> selectorClass = Class.forName("org.skyve.impl.bind.BindUtil$PropertySelector");
		Method setPopulateValue = BindUtil.class.getDeclaredMethod("setPopulateValue", Object.class, selectorClass, Object.class);
		setPopulateValue.setAccessible(true);
		IndexedMappedPojo pojo = new IndexedMappedPojo();

		Object indexedSelector = parse.invoke(null, "values[1]");
		setPopulateValue.invoke(null, pojo, indexedSelector, "indexed");
		Object mappedSelector = parse.invoke(null, "attributes(home)");
		setPopulateValue.invoke(null, pojo, mappedSelector, "mapped");
		Object simpleSelector = parse.invoke(null, "name");
		setPopulateValue.invoke(null, pojo, simpleSelector, "simple");

		assertEquals("indexed", pojo.getValues().get(1));
		assertEquals("mapped", pojo.getAttributes().get("home"));
		assertEquals("simple", pojo.getName());
	}

	@Test
	@SuppressWarnings("static-method")
	void privateParsePropertySelectorToleratesMalformedIndexAndKey() throws Exception {
		Method parse = BindUtil.class.getDeclaredMethod("parsePropertySelector", String.class);
		parse.setAccessible(true);
		Class<?> selectorClass = Class.forName("org.skyve.impl.bind.BindUtil$PropertySelector");
		java.lang.reflect.Field propertyName = selectorClass.getDeclaredField("propertyName");
		java.lang.reflect.Field index = selectorClass.getDeclaredField("index");
		java.lang.reflect.Field key = selectorClass.getDeclaredField("key");
		propertyName.setAccessible(true);
		index.setAccessible(true);
		key.setAccessible(true);

		Object badIndex = parse.invoke(null, "values[abc]");
		Object badKey = parse.invoke(null, "attributes(home");

		assertEquals("values", propertyName.get(badIndex));
		assertEquals(Integer.valueOf(-1), index.get(badIndex));
		assertNull(key.get(badIndex));
		assertEquals("attributes", propertyName.get(badKey));
		assertNull(key.get(badKey));
	}

	@Test
	@SuppressWarnings("static-method")
	void isAScalarTypeRecognisesPrimitiveEnumAndObjectTypes() {
		assertTrue(BindUtil.isAScalarType(Integer.TYPE));
		assertTrue(BindUtil.isAScalarType(TestBindEnum.class));
		assertFalse(BindUtil.isAScalarType(List.class));
	}

	@Test
	@SuppressWarnings("static-method")
	void javaIdentifierHelpersCoverGetterSetterBooleanAndTitleCase() {
		assertEquals("name", BindUtil.toJavaPropertyName("getName"));
		assertEquals("active", BindUtil.toJavaPropertyName("setActive"));
		assertEquals("enabled", BindUtil.toJavaPropertyName("isEnabled"));
		assertEquals("plain", BindUtil.toJavaPropertyName("plain"));
		assertEquals("DOB Value", BindUtil.toTitleCase("DOB Value"));
		assertEquals("oneInvalidName", BindUtil.toJavaInstanceIdentifier("1 invalid_name"));
	}

	@Test
	@SuppressWarnings("static-method")
	void addElementToStaticCollectionInvokesGeneratedAddMethod() {
		StaticCollectionBean owner = new StaticCollectionBean();
		DynamicBean element = new DynamicBean("sales", "Item", new HashMap<>());

		withThreadLocalUser(staticCollectionUser(owner), () -> assertTrue(BindUtil.addElementToCollection(owner, "items", element)));

		assertEquals(List.of(element), owner.getItems());
	}

	@Test
	@SuppressWarnings("static-method")
	void addElementToStaticCollectionAtIndexInvokesGeneratedIndexedAddMethod() {
		DynamicBean existing = new DynamicBean("sales", "Item", new HashMap<>(Map.of(Bean.DOCUMENT_ID, "existing")));
		DynamicBean inserted = new DynamicBean("sales", "Item", new HashMap<>(Map.of(Bean.DOCUMENT_ID, "inserted")));
		StaticCollectionBean owner = new StaticCollectionBean();
		owner.getItems().add(existing);

		withThreadLocalUser(staticCollectionUser(owner), () -> BindUtil.addElementToCollection(owner, "items", 0, inserted));

		assertEquals(Arrays.asList(inserted, existing), owner.getItems());
	}

	@Test
	@SuppressWarnings("static-method")
	void removeElementFromStaticCollectionInvokesGeneratedRemoveMethod() {
		DynamicBean element = new DynamicBean("sales", "Item", new HashMap<>());
		StaticCollectionBean owner = new StaticCollectionBean();
		owner.getItems().add(element);

		withThreadLocalUser(staticCollectionUser(owner), () -> assertTrue(BindUtil.removeElementFromCollection(owner, "items", element)));
	}

	@Test
	@SuppressWarnings("static-method")
	void removeElementFromStaticCollectionByIndexInvokesGeneratedIndexedRemoveMethod() {
		DynamicBean first = new DynamicBean("sales", "Item", new HashMap<>(Map.of(Bean.DOCUMENT_ID, "first")));
		DynamicBean second = new DynamicBean("sales", "Item", new HashMap<>(Map.of(Bean.DOCUMENT_ID, "second")));
		StaticCollectionBean owner = new StaticCollectionBean();
		owner.getItems().add(first);
		owner.getItems().add(second);

		DynamicBean removed = withStaticCollectionUserReturning(owner, () -> BindUtil.removeElementFromCollection(owner, "items", 1));

		assertSame(second, removed);
		assertEquals(List.of(first), owner.getItems());
	}

	@SuppressWarnings("boxing")
	private static User staticCollectionUser(StaticCollectionBean owner) {
		Customer customer = mock(Customer.class);
		User user = mock(User.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Document relatedDocument = mock(Document.class);
		org.skyve.metadata.model.document.Collection collection =
				mock(org.skyve.metadata.model.document.Collection.class);

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(document);
		when(module.getDocument(customer, "Item")).thenReturn(relatedDocument);
		when(document.getPolymorphicAttribute(customer, "items")).thenReturn(collection);
		when(collection.getDocumentName()).thenReturn("Item");
		when(relatedDocument.isDynamic()).thenReturn(Boolean.FALSE);
		owner.setModuleMetaData(module);
		return user;
	}

	private static <T> T withStaticCollectionUserReturning(StaticCollectionBean owner, java.util.concurrent.Callable<T> run) {
		final Object[] result = new Object[1];
		withThreadLocalUser(staticCollectionUser(owner), () -> {
			try {
				result[0] = run.call();
			}
			catch (Exception e) {
				throw new AssertionError(e);
			}
		});
		@SuppressWarnings("unchecked")
		T typed = (T) result[0];
		return typed;
	}

	public static final class IndexedMappedPojo {
		private final List<String> values = new ArrayList<>(Arrays.asList("zero", "one"));
		private final Map<String, String> attributes = new HashMap<>();
		private String name;

		public List<String> getValues() {
			return values;
		}

		public Map<String, String> getAttributes() {
			return attributes;
		}

		public String getName() {
			return name;
		}

		public void setName(String name) {
			this.name = name;
		}
	}

	public static final class SampleChildBean extends DynamicBean {
		private static final long serialVersionUID = 1L;

		public SampleChildBean() {
			super("sales", "Child", new HashMap<>());
		}
	}

	public static final class StaticCollectionBean extends DynamicBean {
		private static final long serialVersionUID = 1L;

		private Module moduleMetaData;

		public StaticCollectionBean() {
			super("sales", "Order", new HashMap<>(Map.of("items", new ArrayList<Bean>())));
		}

		@Override
		public Module getModuleMetaData() {
			return moduleMetaData;
		}

		public void setModuleMetaData(Module moduleMetaData) {
			this.moduleMetaData = moduleMetaData;
		}

		@SuppressWarnings("unchecked")
		public List<Bean> getItems() {
			return (List<Bean>) get("items");
		}

		public boolean addItemsElement(Bean element) {
			return getItems().add(element);
		}

		public void addItemsElement(int index, Bean element) {
			getItems().add(index, element);
		}

		public boolean removeItemsElement(Bean element) {
			getItems().remove(element);
			return true;
		}

		public Bean removeItemsElement(int index) {
			return getItems().remove(index);
		}
	}
}
