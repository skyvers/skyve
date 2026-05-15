package org.skyve.impl.bind;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

@ExtendWith(MockitoExtension.class)
public class UserAttributesExpressionEvaluatorTest {

	@Mock
	private Customer customer;

	@Mock
	private Module module;

	@Mock
	private Document document;

	@Test
	@SuppressWarnings("static-method")
	void prefixConstantIsUser() {
		assertThat(UserAttributesExpressionEvaluator.PREFIX, is("user"));
	}

	@Test
	@SuppressWarnings("static-method")
	void prefixBindingDoesNothing() {
		UserAttributesExpressionEvaluator evaluator = new UserAttributesExpressionEvaluator();
		StringBuilder sb = new StringBuilder("someKey");
		evaluator.prefixBindingWithoutPrefixOrSuffix(sb, "binding");
		assertThat(sb.toString(), is("someKey"));
	}

	@Test
	void completeReturnsEmptyList() {
		UserAttributesExpressionEvaluator evaluator = new UserAttributesExpressionEvaluator();
		List<String> result = evaluator.completeWithoutPrefixOrSuffix("x", customer, module, document);
		assertThat(result, empty());
	}

	@Test
	void validateReturnsNull() {
		UserAttributesExpressionEvaluator evaluator = new UserAttributesExpressionEvaluator();
		assertNull(evaluator.validateWithoutPrefixOrSuffix("key", null, customer, module, document));
	}

	@Test
	void evaluateReturnsAttributeValue() throws Exception {
		User user = Mockito.mock(User.class);
		Mockito.when(user.getAttributes()).thenReturn(Map.of("region", "EMEA"));

		AbstractPersistence persistence = Mockito.mock(AbstractPersistence.class);
		Mockito.when(persistence.getUser()).thenReturn(user);
		setThreadLocalPersistence(persistence);
		try {
			UserAttributesExpressionEvaluator evaluator = new UserAttributesExpressionEvaluator();
			Object result = evaluator.evaluateWithoutPrefixOrSuffix("region", null);
			assertEquals("EMEA", result);
		}
		finally {
			clearThreadLocalPersistence();
		}
	}

	@Test
	void formatReturnsDisplayValueForUserAttribute() throws Exception {
		User user = Mockito.mock(User.class);
		Mockito.when(user.getAttributes()).thenReturn(Map.of("region", "APAC"));
		Customer c = Mockito.mock(Customer.class);
		Mockito.when(user.getCustomer()).thenReturn(c);

		AbstractPersistence persistence = Mockito.mock(AbstractPersistence.class);
		Mockito.when(persistence.getUser()).thenReturn(user);
		setThreadLocalPersistence(persistence);
		try {
			UserAttributesExpressionEvaluator evaluator = new UserAttributesExpressionEvaluator();
			String result = evaluator.formatWithoutPrefixOrSuffix("region", null);
			assertThat(result, is("APAC"));
		}
		finally {
			clearThreadLocalPersistence();
		}
	}

	@SuppressWarnings("unchecked")
	private static void setThreadLocalPersistence(AbstractPersistence persistence) throws Exception {
		Field threadLocalField = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		threadLocalField.setAccessible(true);
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) threadLocalField.get(null);
		threadLocal.set(persistence);
	}

	@SuppressWarnings("unchecked")
	private static void clearThreadLocalPersistence() throws Exception {
		Field threadLocalField = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		threadLocalField.setAccessible(true);
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) threadLocalField.get(null);
		threadLocal.remove();
	}
}
