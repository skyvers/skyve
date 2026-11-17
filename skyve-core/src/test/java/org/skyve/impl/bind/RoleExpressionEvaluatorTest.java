package org.skyve.impl.bind;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.Role;
import org.skyve.metadata.user.User;

@ExtendWith(MockitoExtension.class)
@SuppressWarnings({"static-method", "boxing"})
class RoleExpressionEvaluatorTest {

	@Mock
	private Customer customer;

	@Mock
	private Module module;

	@Mock
	private Document document;

	@Test
	void prefixConstantIsRole() {
		assertThat(RoleExpressionEvaluator.PREFIX, is("role"));
	}

	@Test
	void validateWithDotReturnNull() {
		RoleExpressionEvaluator evaluator = new RoleExpressionEvaluator();
		assertNull(evaluator.validateWithoutPrefixOrSuffix("admin.BasicUser", null, customer, module, document));
	}

	@Test
	void validateWithoutDotReturnsError() {
		RoleExpressionEvaluator evaluator = new RoleExpressionEvaluator();
		String result = evaluator.validateWithoutPrefixOrSuffix("adminBasicUser", null, customer, module, document);
		assertThat(result, is("Role adminBasicUser needs to be in the format <module>.<role>"));
	}

	@Test
	void validateWithDotAtStartReturnsError() {
		RoleExpressionEvaluator evaluator = new RoleExpressionEvaluator();
		String result = evaluator.validateWithoutPrefixOrSuffix(".BasicUser", null, customer, module, document);
		assertThat(result, is("Role .BasicUser needs to be in the format <module>.<role>"));
	}

	@Test
	void prefixBindingDoesNothing() {
		RoleExpressionEvaluator evaluator = new RoleExpressionEvaluator();
		StringBuilder sb = new StringBuilder("admin.BasicUser");
		evaluator.prefixBindingWithoutPrefixOrSuffix(sb, "someBinding");
		// unchanged
		assertThat(sb.toString(), is("admin.BasicUser"));
	}

	@Test
	void completeReturnsAllRolesWhenFragmentIsNull() {
		Role mockRole = mock(Role.class);
		when(mockRole.getName()).thenReturn("BasicUser");
		when(module.getName()).thenReturn("admin");
		when(module.getRoles()).thenReturn(List.of(mockRole));
		when(customer.getModules()).thenReturn(List.of(module));

		RoleExpressionEvaluator evaluator = new RoleExpressionEvaluator();
		List<String> result = evaluator.completeWithoutPrefixOrSuffix(null, customer, module, document);
		assertThat(result, contains("admin.BasicUser"));
	}

	@Test
	void completeFiltersRolesByFragment() {
		Role mockRole1 = mock(Role.class);
		when(mockRole1.getName()).thenReturn("BasicUser");
		Role mockRole2 = mock(Role.class);
		when(mockRole2.getName()).thenReturn("SuperUser");
		when(module.getName()).thenReturn("admin");
		when(module.getRoles()).thenReturn(List.of(mockRole1, mockRole2));
		when(customer.getModules()).thenReturn(List.of(module));

		RoleExpressionEvaluator evaluator = new RoleExpressionEvaluator();
		List<String> result = evaluator.completeWithoutPrefixOrSuffix("admin.B", customer, module, document);
		assertThat(result, contains("admin.BasicUser"));
	}

	@Test
	void completeReturnsEmptyWhenNoRolesMatch() {
		Role mockRole = mock(Role.class);
		when(mockRole.getName()).thenReturn("BasicUser");
		when(module.getName()).thenReturn("admin");
		when(module.getRoles()).thenReturn(List.of(mockRole));
		when(customer.getModules()).thenReturn(List.of(module));

		RoleExpressionEvaluator evaluator = new RoleExpressionEvaluator();
		List<String> result = evaluator.completeWithoutPrefixOrSuffix("admin.X", customer, module, document);
		assertThat(result, empty());
	}

	@Test
	void evaluateReturnsTrueWhenUserInRole() throws Exception {
		User user = mock(User.class);
		when(user.isInRole("admin", "BasicUser")).thenReturn(Boolean.TRUE);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(user);
		ThreadLocalPersistenceTestUtil.setThreadLocalPersistence(persistence);
		try {
			RoleExpressionEvaluator evaluator = new RoleExpressionEvaluator();
			Object result = evaluator.evaluateWithoutPrefixOrSuffix("admin.BasicUser", null);
			assertEquals(Boolean.TRUE, result);
		}
		finally {
			ThreadLocalPersistenceTestUtil.clearThreadLocalPersistence();
		}
	}

	@Test
	void evaluateReturnsFalseWhenUserNotInRole() throws Exception {
		User user = mock(User.class);
		when(user.isInRole("admin", "BasicUser")).thenReturn(Boolean.FALSE);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(user);
		ThreadLocalPersistenceTestUtil.setThreadLocalPersistence(persistence);
		try {
			RoleExpressionEvaluator evaluator = new RoleExpressionEvaluator();
			Object result = evaluator.evaluateWithoutPrefixOrSuffix("admin.BasicUser", null);
			assertEquals(Boolean.FALSE, result);
		}
		finally {
			ThreadLocalPersistenceTestUtil.clearThreadLocalPersistence();
		}
	}

	@Test
	void formatReturnsDisplayValueForBooleanResult() throws Exception {
		User user = mock(User.class);
		when(user.isInRole("admin", "BasicUser")).thenReturn(Boolean.TRUE);
		Customer c = mock(Customer.class);
		when(user.getCustomer()).thenReturn(c);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(user);
		ThreadLocalPersistenceTestUtil.setThreadLocalPersistence(persistence);
		try {
			RoleExpressionEvaluator evaluator = new RoleExpressionEvaluator();
			String result = evaluator.formatWithoutPrefixOrSuffix("admin.BasicUser", null);
			assertThat(result, is("Yes"));
		}
		finally {
			ThreadLocalPersistenceTestUtil.clearThreadLocalPersistence();
		}
	}
}
