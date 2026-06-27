package org.skyve.impl.bind;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

@ExtendWith(MockitoExtension.class)
@SuppressWarnings("static-method")
class StashExpressionEvaluatorCoreIndependentTest {

	@Mock
	private Customer customer;

	@Mock
	private Module module;

	@Mock
	private Document document;

	@Test
	void prefixConstantIsStash() {
		assertThat(StashExpressionEvaluator.PREFIX, is("stash"));
	}

	@Test
	void prefixBindingDoesNothing() {
		StashExpressionEvaluator evaluator = new StashExpressionEvaluator();
		StringBuilder sb = new StringBuilder("someKey");
		evaluator.prefixBindingWithoutPrefixOrSuffix(sb, "binding");
		assertThat(sb.toString(), is("someKey"));
	}

	@Test
	void completeReturnsEmptyList() {
		StashExpressionEvaluator evaluator = new StashExpressionEvaluator();
		List<String> result = evaluator.completeWithoutPrefixOrSuffix("x", customer, module, document);
		assertThat(result, empty());
	}

	@Test
	void validateReturnsNull() {
		StashExpressionEvaluator evaluator = new StashExpressionEvaluator();
		assertNull(evaluator.validateWithoutPrefixOrSuffix("key", null, customer, module, document));
	}

	@Test
	void evaluateReturnsStashedValue() throws Exception {
		SortedMap<String, Object> stash = new TreeMap<>();
		stash.put("a", "b");

		AbstractPersistence persistence = Mockito.mock(AbstractPersistence.class);
		Mockito.when(persistence.getStash()).thenReturn(stash);
		ThreadLocalPersistenceTestUtil.setThreadLocalPersistence(persistence);
		try {
			StashExpressionEvaluator evaluator = new StashExpressionEvaluator();
			Object result = evaluator.evaluateWithoutPrefixOrSuffix("a", null);
			assertEquals("b", result);
		}
		finally {
			ThreadLocalPersistenceTestUtil.clearThreadLocalPersistence();
		}
	}

	@Test
	void formatReturnsDisplayValueForStashValue() throws Exception {
		SortedMap<String, Object> stash = new TreeMap<>();
		stash.put("n", Integer.valueOf(5));

		AbstractPersistence persistence = Mockito.mock(AbstractPersistence.class);
		Mockito.when(persistence.getStash()).thenReturn(stash);
		Customer c = Mockito.mock(Customer.class);
		org.skyve.metadata.user.User user = Mockito.mock(org.skyve.metadata.user.User.class);
		Mockito.when(user.getCustomer()).thenReturn(c);
		Mockito.when(persistence.getUser()).thenReturn(user);
		ThreadLocalPersistenceTestUtil.setThreadLocalPersistence(persistence);
		try {
			StashExpressionEvaluator evaluator = new StashExpressionEvaluator();
			String result = evaluator.formatWithoutPrefixOrSuffix("n", null);
			assertThat(result, is("5"));
		}
		finally {
			ThreadLocalPersistenceTestUtil.clearThreadLocalPersistence();
		}
	}
}
