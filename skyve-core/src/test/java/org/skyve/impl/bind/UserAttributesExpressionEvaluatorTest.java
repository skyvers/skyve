package org.skyve.impl.bind;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

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
}
