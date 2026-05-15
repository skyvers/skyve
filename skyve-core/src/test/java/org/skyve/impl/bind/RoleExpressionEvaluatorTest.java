package org.skyve.impl.bind;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.lang.reflect.Field;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.Role;
import org.skyve.metadata.user.User;

@ExtendWith(MockitoExtension.class)
public class RoleExpressionEvaluatorTest {

	@Mock
	private Customer customer;

	@Mock
	private Module module;

	@Mock
	private Document document;

	@Test
	@SuppressWarnings("static-method")
	public void prefixConstantIsRole() {
		assertThat(RoleExpressionEvaluator.PREFIX, is("role"));
	}

	@Test
	public void validateWithDotReturnNull() {
		RoleExpressionEvaluator evaluator = new RoleExpressionEvaluator();
		assertNull(evaluator.validateWithoutPrefixOrSuffix("admin.BasicUser", null, customer, module, document));
	}

	@Test
	public void validateWithoutDotReturnsError() {
		RoleExpressionEvaluator evaluator = new RoleExpressionEvaluator();
		String result = evaluator.validateWithoutPrefixOrSuffix("adminBasicUser", null, customer, module, document);
		assertThat(result, is("Role adminBasicUser needs to be in the format <module>.<role>"));
	}

	@Test
	public void validateWithDotAtStartReturnsError() {
		RoleExpressionEvaluator evaluator = new RoleExpressionEvaluator();
		String result = evaluator.validateWithoutPrefixOrSuffix(".BasicUser", null, customer, module, document);
		assertThat(result, is("Role .BasicUser needs to be in the format <module>.<role>"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void prefixBindingDoesNothing() {
		RoleExpressionEvaluator evaluator = new RoleExpressionEvaluator();
		StringBuilder sb = new StringBuilder("admin.BasicUser");
		evaluator.prefixBindingWithoutPrefixOrSuffix(sb, "someBinding");
		// unchanged
		assertThat(sb.toString(), is("admin.BasicUser"));
	}

	@Test
	public void completeReturnsAllRolesWhenFragmentIsNull() {
		Role mockRole = Mockito.mock(Role.class);
		Mockito.when(mockRole.getName()).thenReturn("BasicUser");
		Mockito.when(module.getName()).thenReturn("admin");
		Mockito.when(module.getRoles()).thenReturn(List.of(mockRole));
		Mockito.when(customer.getModules()).thenReturn(List.of(module));

		RoleExpressionEvaluator evaluator = new RoleExpressionEvaluator();
		List<String> result = evaluator.completeWithoutPrefixOrSuffix(null, customer, module, document);
		assertThat(result, contains("admin.BasicUser"));
	}

	@Test
	public void completeFiltersRolesByFragment() {
		Role mockRole1 = Mockito.mock(Role.class);
		Mockito.when(mockRole1.getName()).thenReturn("BasicUser");
		Role mockRole2 = Mockito.mock(Role.class);
		Mockito.when(mockRole2.getName()).thenReturn("SuperUser");
		Mockito.when(module.getName()).thenReturn("admin");
		Mockito.when(module.getRoles()).thenReturn(List.of(mockRole1, mockRole2));
		Mockito.when(customer.getModules()).thenReturn(List.of(module));

		RoleExpressionEvaluator evaluator = new RoleExpressionEvaluator();
		List<String> result = evaluator.completeWithoutPrefixOrSuffix("admin.B", customer, module, document);
		assertThat(result, contains("admin.BasicUser"));
	}

	@Test
	public void completeReturnsEmptyWhenNoRolesMatch() {
		Role mockRole = Mockito.mock(Role.class);
		Mockito.when(mockRole.getName()).thenReturn("BasicUser");
		Mockito.when(module.getName()).thenReturn("admin");
		Mockito.when(module.getRoles()).thenReturn(List.of(mockRole));
		Mockito.when(customer.getModules()).thenReturn(List.of(module));

		RoleExpressionEvaluator evaluator = new RoleExpressionEvaluator();
		List<String> result = evaluator.completeWithoutPrefixOrSuffix("admin.X", customer, module, document);
		assertThat(result, empty());
	}

	@Test
	public void evaluateReturnsTrueWhenUserInRole() throws Exception {
		User user = Mockito.mock(User.class);
		Mockito.when(user.isInRole("admin", "BasicUser")).thenReturn(true);

		AbstractPersistence persistence = Mockito.mock(AbstractPersistence.class);
		Mockito.when(persistence.getUser()).thenReturn(user);
		setThreadLocalPersistence(persistence);
		try {
			RoleExpressionEvaluator evaluator = new RoleExpressionEvaluator();
			Object result = evaluator.evaluateWithoutPrefixOrSuffix("admin.BasicUser", null);
			assertEquals(Boolean.TRUE, result);
		}
		finally {
			clearThreadLocalPersistence();
		}
	}

	@Test
	public void evaluateReturnsFalseWhenUserNotInRole() throws Exception {
		User user = Mockito.mock(User.class);
		Mockito.when(user.isInRole("admin", "BasicUser")).thenReturn(false);

		AbstractPersistence persistence = Mockito.mock(AbstractPersistence.class);
		Mockito.when(persistence.getUser()).thenReturn(user);
		setThreadLocalPersistence(persistence);
		try {
			RoleExpressionEvaluator evaluator = new RoleExpressionEvaluator();
			Object result = evaluator.evaluateWithoutPrefixOrSuffix("admin.BasicUser", null);
			assertEquals(Boolean.FALSE, result);
		}
		finally {
			clearThreadLocalPersistence();
		}
	}

	@Test
	public void formatReturnsDisplayValueForBooleanResult() throws Exception {
		User user = Mockito.mock(User.class);
		Mockito.when(user.isInRole("admin", "BasicUser")).thenReturn(true);
		Customer c = Mockito.mock(Customer.class);
		Mockito.when(user.getCustomer()).thenReturn(c);

		AbstractPersistence persistence = Mockito.mock(AbstractPersistence.class);
		Mockito.when(persistence.getUser()).thenReturn(user);
		setThreadLocalPersistence(persistence);
		try {
			RoleExpressionEvaluator evaluator = new RoleExpressionEvaluator();
			String result = evaluator.formatWithoutPrefixOrSuffix("admin.BasicUser", null);
			assertThat(result, is("Yes"));
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
