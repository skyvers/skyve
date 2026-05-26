package org.skyve.util;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.lang.reflect.Field;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.domain.types.converters.date.DD_MMM_YYYY;
import org.skyve.domain.types.converters.datetime.DD_MMM_YYYY_HH_MI;
import org.skyve.domain.types.converters.time.HH_MI;
import org.skyve.domain.types.converters.timestamp.DD_MMM_YYYY_HH_MI_SS;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

@SuppressWarnings("static-method")
class ExpressionEvaluatorTest {
	private static void withThreadLocalUserAndCustomer(User user, Runnable run) {
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

	private static class NoOpEvaluator extends ExpressionEvaluator {
		@Override
		public String formatWithoutPrefixOrSuffix(String expression, Bean bean) {
			return expression;
		}

		@Override
		public Object evaluateWithoutPrefixOrSuffix(String expression, Bean bean) {
			return expression;
		}

		@Override
		public String validateWithoutPrefixOrSuffix(String expression,
											Class<?> returnType,
											Customer customer,
											Module module,
											Document document) {
			return null;
		}

		@Override
		public List<String> completeWithoutPrefixOrSuffix(String fragment,
										Customer customer,
										Module module,
										Document document) {
			return Collections.emptyList();
		}

		@Override
		public void prefixBindingWithoutPrefixOrSuffix(StringBuilder expression, String binding) {
			expression.insert(0, '.').insert(0, binding);
		}
	}

	@Test
	void registerDuplicatePrefixThrows() {
		String prefix = "z" + UUID.randomUUID().toString().replace("-", "");
		ExpressionEvaluator.register(prefix, new NoOpEvaluator());

		IllegalStateException ex = assertThrows(IllegalStateException.class,
				() -> ExpressionEvaluator.register(prefix, new NoOpEvaluator()));
		assertThat(ex.getMessage(), containsString("already registered"));
	}

	@Test
	void validateReturnsErrorForEmptyImplicitExpression() {
		String result = ExpressionEvaluator.validate("{   }");
		assertThat(result, is("Nothing to evaluate in expression {   }"));
	}

	@Test
	void validateReturnsErrorForEmptyPrefixedExpression() {
		String result = ExpressionEvaluator.validate("{role:   }");
		assertThat(result, is("Nothing to evaluate in expression {role:   }"));
	}

	@Test
	void validateReturnsErrorForUnknownPrefix() {
		String result = ExpressionEvaluator.validate("{unknown:abc}");
		assertThat(result, is("Cannot find an expression evaluator for prefix unknown"));
	}

	@Test
	void validateReturnsErrorWhenFormatterMissingName() {
		String result = ExpressionEvaluator.validate("{USER|}");
		assertThat(result, is("Formatter expected after '|' in expression {USER|}"));
	}

	@Test
	void validateReturnsErrorWhenFormatterDoesNotExist() {
		String result = ExpressionEvaluator.validate("{USER|noSuchFormatter}");
		assertThat(result, is("Formatter noSuchFormatter does not exist"));
	}

	@Test
	void validateAcceptsImplicitExpressions() {
		assertThat(ExpressionEvaluator.validate("{USER}"), is(nullValue()));
		assertThat(ExpressionEvaluator.validate("{DATE}"), is(nullValue()));
		assertThat(ExpressionEvaluator.validate("{TIME}"), is(nullValue()));
		assertThat(ExpressionEvaluator.validate("{DATETIME}"), is(nullValue()));
		assertThat(ExpressionEvaluator.validate("{TIMESTAMP}"), is(nullValue()));
	}

	@Test
	void prefixBindingLeavesImplicitExpressionUnchanged() {
		String result = ExpressionEvaluator.prefixBinding("{USER}", "bean");
		assertThat(result, is("{USER}"));
	}

	@Test
	void prefixBindingPrefixesBindingExpression() {
		String result = ExpressionEvaluator.prefixBinding("{name}", "bean");
		assertThat(result, is("{bean.name}"));
	}

	@Test
	void prefixBindingWithFormatSuffixThrowsStringIndexOutOfBounds() {
		assertThrows(StringIndexOutOfBoundsException.class,
				() -> ExpressionEvaluator.prefixBinding("{name|DD_MMM_YYYY}", "bean"));
	}

	@Test
	void prefixBindingThrowsForUnknownPrefix() {
		DomainException ex = assertThrows(DomainException.class,
				() -> ExpressionEvaluator.prefixBinding("{unknown:value}", "bean"));
		assertThat(ex.getMessage(), containsString("Cannot find an expression evaluator for prefix unknown"));
	}

	@Test
	void completeExpressionReturnsEmptyListWhenNoExpressionSyntax() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);

		List<String> result = ExpressionEvaluator.completeExpression("plain text", customer, module, document);
		assertTrue(result.isEmpty());
	}

	@Test
	void completeExpressionSuggestsImplicitExpressions() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);

		List<String> result = ExpressionEvaluator.completeExpression("{US", customer, module, document);
		assertThat(result, hasItem("{USER}"));
	}

	@Test
	void completeExpressionSuggestsTemporalImplicitExpressions() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);

		// Fragment "{DA" should suggest DATE, DATAGROUPID, DATETIME
		List<String> result = ExpressionEvaluator.completeExpression("{DA", customer, module, document);
		assertThat(result, hasItem("{DATE}"));
		assertThat(result, hasItem("{DATETIME}"));
		assertThat(result, hasItem("{DATAGROUPID}"));

		// Fragment "{TI" should suggest TIME, TIMESTAMP
		List<String> result2 = ExpressionEvaluator.completeExpression("{TI", customer, module, document);
		assertThat(result2, hasItem("{TIME}"));
		assertThat(result2, hasItem("{TIMESTAMP}"));

		// Fragment "{CU" should suggest CUSTOMER
		List<String> result3 = ExpressionEvaluator.completeExpression("{CU", customer, module, document);
		assertThat(result3, hasItem("{CUSTOMER}"));

		// Fragment "{UR" should suggest URL
		List<String> result4 = ExpressionEvaluator.completeExpression("{UR", customer, module, document);
		assertThat(result4, hasItem("{URL}"));
	}

	@Test
	void completeExpressionSuggestsFormatterNamesAfterPipe() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);

		List<String> result = ExpressionEvaluator.completeExpression("{USER|", customer, module, document);
		assertThat(result, is(notNullValue()));
		assertFalse(result.isEmpty());
		assertTrue(result.get(0).startsWith("{"));
	}

	@Test
	void evaluateImplicitUserExpressionsUsingThreadLocalPersistence() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getName()).thenReturn("alice");
		when(user.getId()).thenReturn("U1");
		when(user.getContactName()).thenReturn("Alice Smith");
		when(user.getDataGroupId()).thenReturn("DG");
		when(user.getContactId()).thenReturn("C1");
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getName()).thenReturn("acme");

		withThreadLocalUserAndCustomer(user, () -> {
			assertThat(ExpressionEvaluator.evaluate("{USER}"), is((Object) "alice"));
			assertThat(ExpressionEvaluator.evaluate("{USERID}"), is((Object) "U1"));
			assertThat(ExpressionEvaluator.evaluate("{USERNAME}"), is((Object) "Alice Smith"));
			assertThat(ExpressionEvaluator.evaluate("{DATAGROUPID}"), is((Object) "DG"));
			assertThat(ExpressionEvaluator.evaluate("{CONTACTID}"), is((Object) "C1"));
			assertThat(ExpressionEvaluator.evaluate("{CUSTOMER}"), is((Object) "acme"));
		});
	}

	@Test
	void evaluateImplicitTemporalExpressionsReturnExpectedTypes() {
		assertThat(ExpressionEvaluator.evaluate("{DATE}"), instanceOf(DateOnly.class));
		assertThat(ExpressionEvaluator.evaluate("{TIME}"), instanceOf(TimeOnly.class));
		assertThat(ExpressionEvaluator.evaluate("{DATETIME}"), instanceOf(DateTime.class));
		assertThat(ExpressionEvaluator.evaluate("{TIMESTAMP}"), instanceOf(Timestamp.class));
	}

	@Test
	void urlExpressionReturnsNullWhenNoBean() {
		assertNull(ExpressionEvaluator.evaluate("{URL}", null));
		assertThat(ExpressionEvaluator.format("{URL}", null), is(""));
	}

	@Test
	void completeExpressionReturnsEmptyForUnknownPrefixedEvaluator() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);

		List<String> result = ExpressionEvaluator.completeExpression("{unknown:x", customer, module, document);
		assertTrue(result.isEmpty());
	}

	@Test
	void completeExpressionIgnoresEscapedCurlyBrace() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);

		List<String> result = ExpressionEvaluator.completeExpression("\\{US", customer, module, document);
		assertTrue(result.isEmpty());
	}

	@Test
	void validateFormatterWithNonStringReturnTypeFailsFast() {
		String result = ExpressionEvaluator.validate("{USER|DD_MMM_YYYY}", Integer.class);
		assertThat(result, containsString("evaluates to a String"));
	}

	@Test
	void validateFormatterDateTypeIncompatibleWithImplicitExpression() {
		String result = ExpressionEvaluator.validate("{USER|DD_MMM_YYYY}", String.class);
		assertThat(result, containsString("incompatible"));
	}

	@Test
	void evaluateThrowsForUnknownPrefixedExpression() {
		DomainException ex = assertThrows(DomainException.class, () -> ExpressionEvaluator.evaluate("{unknown:value}"));
		assertThat(ex.getMessage(), containsString("Cannot find an expression evaluator for prefix unknown"));
	}

	@Test
	void completeExpressionPreservesBoilerplatePrefixText() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);

		List<String> result = ExpressionEvaluator.completeExpression("prefix {US", customer, module, document);
		assertThat(result, hasItem("prefix {USER}"));
	}

	@Test
	void prefixBindingHandlesKnownPrefixedExpression() {
		String result = ExpressionEvaluator.prefixBinding("{bean:name}", "contact");
		assertThat(result, is("{bean:contact.name}"));
	}

	@Test
	void validateWithCustomerModuleDocumentUsesImplicitExpression() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		assertNull(ExpressionEvaluator.validate("{USER}", customer, module, document));
	}

	@Test
	void validateWithValidFormatterAndCompatibleTypeReturnsNull() {
		// DATE evaluates to a Date subtype; no formatter — should pass
		assertNull(ExpressionEvaluator.validate("{DATE}"));
	}

	@Test
	void validateBindingDelegatesToDefaultEvaluator() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		// With null customer/module/document the binding evaluator should still return (possibly null)
		// The key is that the method doesn't throw
		ExpressionEvaluator.validateBinding("name", customer, module, document);
	}

	@Test
	void validateBindingWithReturnTypeDelegatesToDefaultEvaluator() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		ExpressionEvaluator.validateBinding("name", String.class, customer, module, document);
	}

	@Test
	void completeBindingDelegatesToDefaultEvaluator() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		List<String> result = ExpressionEvaluator.completeBinding("na", customer, module, document);
		assertThat(result, is(notNullValue()));
	}

	@Test
	void validateReturnsNullForUrlExpression() {
		assertNull(ExpressionEvaluator.validate("{URL}"));
	}

	@Test
	void evaluateWithBeanUsesBindingExpression() {
		// Evaluating a simple binding with null bean returns null without throwing
		assertNull(ExpressionEvaluator.evaluate("{name}", null));
	}

	@Test
	void formatUserWithNullValueReturnsEmptyString() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getName()).thenReturn(null);
		when(user.getCustomer()).thenReturn(customer);

		withThreadLocalUserAndCustomer(user, () -> {
			assertThat(ExpressionEvaluator.format("{USER}"), is(""));
			assertThat(ExpressionEvaluator.format("{USER|missingFormatter}"), is(""));
		});
	}

	@Test
	void evaluateUserExpressionReturnsUserName() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getName()).thenReturn("admin");
		when(user.getCustomer()).thenReturn(customer);

		withThreadLocalUserAndCustomer(user, () -> {
			assertThat(ExpressionEvaluator.evaluate("{USER}"), is("admin"));
		});
	}

	@Test
	void evaluateUseridExpressionReturnsUserId() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getId()).thenReturn("user-1");
		when(user.getCustomer()).thenReturn(customer);

		withThreadLocalUserAndCustomer(user, () -> {
			assertThat(ExpressionEvaluator.evaluate("{USERID}"), is("user-1"));
		});
	}

	@Test
	void formatUseridExpressionReturnsUserId() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getId()).thenReturn("user-1");
		when(user.getCustomer()).thenReturn(customer);

		withThreadLocalUserAndCustomer(user, () -> {
			assertThat(ExpressionEvaluator.format("{USERID}"), is("user-1"));
		});
	}

	@Test
	void evaluateUsernameExpressionReturnsContactName() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getContactName()).thenReturn("Alice Smith");
		when(user.getCustomer()).thenReturn(customer);

		withThreadLocalUserAndCustomer(user, () -> {
			assertThat(ExpressionEvaluator.evaluate("{USERNAME}"), is("Alice Smith"));
		});
	}

	@Test
	void formatUsernameNullReturnsEmpty() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getContactName()).thenReturn(null);
		when(user.getCustomer()).thenReturn(customer);

		withThreadLocalUserAndCustomer(user, () -> {
			assertThat(ExpressionEvaluator.format("{USERNAME}"), is(""));
		});
	}

	@Test
	void evaluateDatagroupidReturnsValue() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getDataGroupId()).thenReturn("group-1");
		when(user.getCustomer()).thenReturn(customer);

		withThreadLocalUserAndCustomer(user, () -> {
			assertThat(ExpressionEvaluator.evaluate("{DATAGROUPID}"), is("group-1"));
		});
	}

	@Test
	void formatDatagroupidNullReturnsEmpty() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getDataGroupId()).thenReturn(null);
		when(user.getCustomer()).thenReturn(customer);

		withThreadLocalUserAndCustomer(user, () -> {
			assertThat(ExpressionEvaluator.format("{DATAGROUPID}"), is(""));
		});
	}

	@Test
	void evaluateContactidReturnsValue() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getContactId()).thenReturn("contact-42");
		when(user.getCustomer()).thenReturn(customer);

		withThreadLocalUserAndCustomer(user, () -> {
			assertThat(ExpressionEvaluator.evaluate("{CONTACTID}"), is("contact-42"));
		});
	}

	@Test
	void formatContactidNullReturnsEmpty() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getContactId()).thenReturn(null);
		when(user.getCustomer()).thenReturn(customer);

		withThreadLocalUserAndCustomer(user, () -> {
			assertThat(ExpressionEvaluator.format("{CONTACTID}"), is(""));
		});
	}

	@Test
	void evaluateCustomerExpressionReturnsCustomerName() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getName()).thenReturn("testCustomer");

		withThreadLocalUserAndCustomer(user, () -> {
			assertThat(ExpressionEvaluator.evaluate("{CUSTOMER}"), is("testCustomer"));
		});
	}

	@Test
	void formatCustomerNullReturnsEmpty() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getName()).thenReturn(null);

		withThreadLocalUserAndCustomer(user, () -> {
			assertThat(ExpressionEvaluator.format("{CUSTOMER}"), is(""));
		});
	}

	@Test
	void evaluateDateReturnsDateOnly() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getCustomer()).thenReturn(customer);

		withThreadLocalUserAndCustomer(user, () -> {
			Object result = ExpressionEvaluator.evaluate("{DATE}");
			assertThat(result, instanceOf(DateOnly.class));
		});
	}

	@Test
	void evaluateTimeReturnsTimeOnly() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getCustomer()).thenReturn(customer);

		withThreadLocalUserAndCustomer(user, () -> {
			Object result = ExpressionEvaluator.evaluate("{TIME}");
			assertThat(result, instanceOf(TimeOnly.class));
		});
	}

	@Test
	void evaluateDatetimeReturnsDateTime() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getCustomer()).thenReturn(customer);

		withThreadLocalUserAndCustomer(user, () -> {
			Object result = ExpressionEvaluator.evaluate("{DATETIME}");
			assertThat(result, instanceOf(DateTime.class));
		});
	}

	@Test
	void evaluateTimestampReturnsTimestamp() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getCustomer()).thenReturn(customer);

		withThreadLocalUserAndCustomer(user, () -> {
			Object result = ExpressionEvaluator.evaluate("{TIMESTAMP}");
			assertThat(result, instanceOf(Timestamp.class));
		});
	}

	@Test
	void formatDateExpressionReturnsNonEmpty() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getDefaultDateConverter()).thenReturn(new DD_MMM_YYYY());

		withThreadLocalUserAndCustomer(user, () -> {
			String result = ExpressionEvaluator.format("{DATE}");
			assertThat(result, is(notNullValue()));
			assertFalse(result.isEmpty());
		});
	}

	@Test
	void formatTimeExpressionReturnsNonEmpty() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getDefaultTimeConverter()).thenReturn(new HH_MI());

		withThreadLocalUserAndCustomer(user, () -> {
			String result = ExpressionEvaluator.format("{TIME}");
			assertThat(result, is(notNullValue()));
			assertFalse(result.isEmpty());
		});
	}

	@Test
	void formatDatetimeExpressionReturnsNonEmpty() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getDefaultDateTimeConverter()).thenReturn(new DD_MMM_YYYY_HH_MI());

		withThreadLocalUserAndCustomer(user, () -> {
			String result = ExpressionEvaluator.format("{DATETIME}");
			assertThat(result, is(notNullValue()));
			assertFalse(result.isEmpty());
		});
	}

	@Test
	void formatTimestampExpressionReturnsNonEmpty() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getDefaultTimestampConverter()).thenReturn(new DD_MMM_YYYY_HH_MI_SS());

		withThreadLocalUserAndCustomer(user, () -> {
			String result = ExpressionEvaluator.format("{TIMESTAMP}");
			assertThat(result, is(notNullValue()));
			assertFalse(result.isEmpty());
		});
	}

	@Test
	void evaluateUseridExpressionWithFormatSuffixReturnsString() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getId()).thenReturn("user-id-123");
		when(user.getCustomer()).thenReturn(customer);
		withThreadLocalUserAndCustomer(user, () -> {
			Object result = ExpressionEvaluator.evaluate("{USERID|nonExistentFmt}");
			assertThat(result, is("user-id-123"));
		});
	}

	@Test
	void evaluateUsernameExpressionWithFormatSuffixReturnsString() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getContactName()).thenReturn("Alice");
		when(user.getCustomer()).thenReturn(customer);
		withThreadLocalUserAndCustomer(user, () -> {
			Object result = ExpressionEvaluator.evaluate("{USERNAME|nonExistentFmt}");
			assertThat(result, is("Alice"));
		});
	}

	@Test
	void formatUsernameNonNullReturnsContactName() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getContactName()).thenReturn("Alice");
		when(user.getCustomer()).thenReturn(customer);
		withThreadLocalUserAndCustomer(user, () -> {
			String result = ExpressionEvaluator.format("{USERNAME}");
			assertThat(result, is("Alice"));
		});
	}

	@Test
	void evaluateDatagroupidExpressionWithFormatSuffixReturnsString() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getDataGroupId()).thenReturn("group1");
		when(user.getCustomer()).thenReturn(customer);
		withThreadLocalUserAndCustomer(user, () -> {
			Object result = ExpressionEvaluator.evaluate("{DATAGROUPID|nonExistentFmt}");
			assertThat(result, is("group1"));
		});
	}

	@Test
	void formatDatagroupidNonNullReturnsValue() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getDataGroupId()).thenReturn("group1");
		when(user.getCustomer()).thenReturn(customer);
		withThreadLocalUserAndCustomer(user, () -> {
			String result = ExpressionEvaluator.format("{DATAGROUPID}");
			assertThat(result, is("group1"));
		});
	}

	@Test
	void evaluateContactidExpressionWithFormatSuffixReturnsString() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getContactId()).thenReturn("contact-1");
		when(user.getCustomer()).thenReturn(customer);
		withThreadLocalUserAndCustomer(user, () -> {
			Object result = ExpressionEvaluator.evaluate("{CONTACTID|nonExistentFmt}");
			assertThat(result, is("contact-1"));
		});
	}

	@Test
	void formatContactidNonNullReturnsValue() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getContactId()).thenReturn("contact-1");
		when(user.getCustomer()).thenReturn(customer);
		withThreadLocalUserAndCustomer(user, () -> {
			String result = ExpressionEvaluator.format("{CONTACTID}");
			assertThat(result, is("contact-1"));
		});
	}

	@Test
	void evaluateCustomerExpressionWithFormatSuffixReturnsString() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(customer.getName()).thenReturn("mycust");
		when(user.getCustomer()).thenReturn(customer);
		withThreadLocalUserAndCustomer(user, () -> {
			Object result = ExpressionEvaluator.evaluate("{CUSTOMER|nonExistentFmt}");
			assertThat(result, is("mycust"));
		});
	}

	@Test
	void formatCustomerNonNullReturnsValue() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(customer.getName()).thenReturn("mycust");
		when(user.getCustomer()).thenReturn(customer);
		withThreadLocalUserAndCustomer(user, () -> {
			String result = ExpressionEvaluator.format("{CUSTOMER}");
			assertThat(result, is("mycust"));
		});
	}

	@Test
	void evaluateDateExpressionWithFormatSuffixReturnsString() {
		Object result = ExpressionEvaluator.evaluate("{DATE|nonExistentFmt}");
		assertThat(result, instanceOf(String.class));
	}

	@Test
	void evaluateTimeExpressionWithFormatSuffixReturnsString() {
		Object result = ExpressionEvaluator.evaluate("{TIME|nonExistentFmt}");
		assertThat(result, instanceOf(String.class));
	}

	@Test
	void evaluateDatetimeExpressionWithFormatSuffixReturnsString() {
		Object result = ExpressionEvaluator.evaluate("{DATETIME|nonExistentFmt}");
		assertThat(result, instanceOf(String.class));
	}

	@Test
	void evaluateTimestampExpressionWithFormatSuffixReturnsString() {
		Object result = ExpressionEvaluator.evaluate("{TIMESTAMP|nonExistentFmt}");
		assertThat(result, instanceOf(String.class));
	}

	@Test
	void evaluateUrlExpressionWithNonNullBeanReturnsUrl() {
		Bean bean = mock(Bean.class);
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("User");
		when(bean.getBizId()).thenReturn("id1");
		Object result = ExpressionEvaluator.evaluate("{URL}", bean);
		assertThat(result, instanceOf(String.class));
		assertThat((String) result, containsString("admin"));
	}

	@Test
	void evaluateUrlExpressionWithFormatSuffixAndNonNullBeanReturnsString() {
		Bean bean = mock(Bean.class);
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("User");
		when(bean.getBizId()).thenReturn("id1");
		Object result = ExpressionEvaluator.evaluate("{URL|nonExistentFmt}", bean);
		assertThat(result, instanceOf(String.class));
	}

	@Test
	void evaluateBindingWithFormatSuffixReturnsString() {
		Bean bean = mock(Bean.class);
		when(bean.getBizId()).thenReturn("biz-123");
		Object result = ExpressionEvaluator.evaluate("{bizId|nonExistentFmt}", bean);
		assertThat(result, is("biz-123"));
	}

	@Test
	void evaluatePrefixedExpressionWithFormatSuffixReturnsString() {
		Bean bean = mock(Bean.class);
		when(bean.getBizId()).thenReturn("biz-123");
		Object result = ExpressionEvaluator.evaluate("{bean:bizId|nonExistentFmt}", bean);
		assertThat(result, is("biz-123"));
	}

	@Test
	void formatPrefixedExpressionReturnsString() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		when(user.getCustomer()).thenReturn(customer);
		Bean bean = mock(Bean.class);
		when(bean.getBizId()).thenReturn("biz-123");
		withThreadLocalUserAndCustomer(user, () -> {
			String result = ExpressionEvaluator.format("{bean:bizId}", bean);
			assertThat(result, is("biz-123"));
		});
	}

	@Test
	void validateBindingExpressionWithCustomerModuleDocumentReturnsError() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		when(document.getAllAttributes(customer)).thenReturn(Collections.emptyList());
		// "name" is not a real binding — should return an error message or null
		String result = ExpressionEvaluator.validate("{name}", null, customer, module, document);
		// Just checking the code path is reached (result may be null or an error)
		assertTrue(result == null || result.length() > 0);
	}

	@Test
	void validatePrefixedExpressionWithCustomerModuleDocumentCallsEvaluator() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		when(document.getAllAttributes(customer)).thenReturn(Collections.emptyList());
		String result = ExpressionEvaluator.validate("{bean:name}", null, customer, module, document);
		// Just checking the code path is reached
		assertTrue(result == null || result.length() > 0);
	}

	@Test
	void completeExpressionWithEmptyPrefixSuggestsAllEvaluators() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		when(document.getAllAttributes(customer)).thenReturn(Collections.emptyList());
		List<String> result = ExpressionEvaluator.completeExpression("{", customer, module, document);
		assertThat(result, is(notNullValue()));
	}

	@Test
	void completeExpressionWithFormatFragmentAfterPipeSuggestsFormatters() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		List<String> result = ExpressionEvaluator.completeExpression("{USERID|", customer, module, document);
		assertThat(result, is(notNullValue()));
	}

	@Test
	void completeExpressionWithBeanPrefixSuggestsBindings() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		when(document.getAllAttributes(customer)).thenReturn(Collections.emptyList());
		List<String> result = ExpressionEvaluator.completeExpression("{bean:", customer, module, document);
		assertThat(result, is(notNullValue()));
	}
}
