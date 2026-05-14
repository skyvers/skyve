package org.skyve.impl.bind;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

/**
 * Tests for CORE-free paths in {@link BindingExpressionEvaluator}.
 */
@SuppressWarnings("static-method")
class BindingExpressionEvaluatorTest {

	@Test
	void prefixConstantIsBean() {
		assertEquals("bean", BindingExpressionEvaluator.PREFIX);
	}

	@Test
	void evaluateWithNullBeanReturnsNull() {
		BindingExpressionEvaluator evaluator = new BindingExpressionEvaluator();
		assertNull(evaluator.evaluateWithoutPrefixOrSuffix("someProp", null));
	}

	@Test
	void formatWithNullBeanThrowsIllegalArgument() {
		BindingExpressionEvaluator evaluator = new BindingExpressionEvaluator();
		assertThrows(IllegalArgumentException.class,
				() -> evaluator.formatWithoutPrefixOrSuffix("someProp", null));
	}

	@Test
	void prefixBindingPrependsBindingWithDot() {
		BindingExpressionEvaluator evaluator = new BindingExpressionEvaluator();
		StringBuilder expression = new StringBuilder("propName");
		evaluator.prefixBindingWithoutPrefixOrSuffix(expression, "parent");
		assertEquals("parent.propName", expression.toString());
	}

	@Test
	void prefixBindingWithEmptyExpression() {
		BindingExpressionEvaluator evaluator = new BindingExpressionEvaluator();
		StringBuilder expression = new StringBuilder("");
		evaluator.prefixBindingWithoutPrefixOrSuffix(expression, "root");
		assertEquals("root.", expression.toString());
	}
}
