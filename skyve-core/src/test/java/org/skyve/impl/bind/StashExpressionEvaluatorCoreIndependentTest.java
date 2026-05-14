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
public class StashExpressionEvaluatorCoreIndependentTest {

	@Mock
	private Customer customer;

	@Mock
	private Module module;

	@Mock
	private Document document;

	@Test
	@SuppressWarnings("static-method")
	void prefixConstantIsStash() {
		assertThat(StashExpressionEvaluator.PREFIX, is("stash"));
	}

	@Test
	@SuppressWarnings("static-method")
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
}
