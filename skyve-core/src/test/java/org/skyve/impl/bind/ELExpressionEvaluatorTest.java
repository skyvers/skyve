package org.skyve.impl.bind;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.customer.Customer;

@SuppressWarnings("static-method")
class ELExpressionEvaluatorTest {

	// ---- validateWithoutPrefixOrSuffix ----

	@Test
	void validateReturnsNullWhenNotTypesafe() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		String result = evaluator.validateWithoutPrefixOrSuffix("bean.name", null, null, null, null);
		assertNull(result);
	}

	@Test
	void validateReturnsNullForAnyExpressionWhenNotTypesafe() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		assertNull(evaluator.validateWithoutPrefixOrSuffix(null, String.class, null, null, null));
		assertNull(evaluator.validateWithoutPrefixOrSuffix("", null, null, null, null));
		assertNull(evaluator.validateWithoutPrefixOrSuffix("malformed!!!", null, null, null, null));
	}

	// ---- completeWithoutPrefixOrSuffix — no-delimiter paths (no Customer needed) ----

	@Test
	void completeReturnsAllCompletesForNullFragment() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		List<String> result = evaluator.completeWithoutPrefixOrSuffix(null, null, null, null);
		// COMMENCING_COMPLETES has many entries; all should be returned for empty match
		assertTrue(result.size() > 10, "Should return all commencing completes for null fragment");
		assertTrue(result.contains("bean"), "Should contain 'bean'");
		assertTrue(result.contains("user"), "Should contain 'user'");
		assertTrue(result.contains("empty"), "Should contain 'empty'");
	}

	@Test
	void completeReturnsAllCompletesForEmptyFragment() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		List<String> result = evaluator.completeWithoutPrefixOrSuffix("", null, null, null);
		assertTrue(result.size() > 10, "Should return all commencing completes for empty fragment");
		assertTrue(result.contains("bean"), "Should contain 'bean'");
	}

	@Test
	void completeReturnsMatchingCompletesForFragmentWithNoDelimiter() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		List<String> result = evaluator.completeWithoutPrefixOrSuffix("bean", null, null, null);
		assertTrue(result.contains("bean"), "'bean' should be among completions for 'bean'");
		// 'user' should NOT appear since it doesn't start with 'bean'
		assertTrue(result.stream().allMatch(s -> s.startsWith("bean")),
				"All completions should start with 'bean'");
	}

	@Test
	void completeReturnsEmptyForClosingSquareBraceFragment() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		// ']' as last char → newExpression skips adding completes
		List<String> result = evaluator.completeWithoutPrefixOrSuffix("]", null, null, null);
		assertTrue(result.isEmpty(), "Closing brace fragment should return no completions");
	}

	@Test
	void completeReturnsEmptyForClosingParenFragment() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		List<String> result = evaluator.completeWithoutPrefixOrSuffix(")", null, null, null);
		assertTrue(result.isEmpty(), "Closing paren fragment should return no completions");
	}

	@Test
	void completeReturnsEmptyForUnrecognisedCompoundFragment() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		// "x.y" → has dot (delimiter) but no commencing token found → newExpression("x.y") → "y" matches nothing
		List<String> result = evaluator.completeWithoutPrefixOrSuffix("x.y", null, null, null);
		assertTrue(result.isEmpty(), "Unrecognised compound fragment should return no completions");
	}

	@Test
	void completeReturnsCompletesWithPrefixForUnrecognisedCompoundFragmentWithEmptyTail() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		// "x." → delimiter = dot, no commencing token → newExpression("x.") → lastChar='.' (not ] or )) → match="" → adds all with prefix "x."
		List<String> result = evaluator.completeWithoutPrefixOrSuffix("x.", null, null, null);
		assertTrue(result.size() > 10, "Should return all completes prefixed with 'x.'");
		assertTrue(result.contains("x.bean"), "Should contain 'x.bean'");
	}

	// ---- prefixBindingWithoutPrefixOrSuffix ----

	@Test
	void prefixBindingInsertsBindingAfterEachBeanDot() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		StringBuilder sb = new StringBuilder("bean.x + bean.y");
		evaluator.prefixBindingWithoutPrefixOrSuffix(sb, "field");
		assertEquals("bean.field.x + bean.field.y", sb.toString());
	}

	@Test
	void prefixBindingAppendsBindingWhenExpressionEndsWithBean() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		StringBuilder sb = new StringBuilder("bean");
		evaluator.prefixBindingWithoutPrefixOrSuffix(sb, "myField");
		assertEquals("bean.myField", sb.toString());
	}

	@Test
	void prefixBindingDoesNothingWhenNoBeanReference() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		StringBuilder sb = new StringBuilder("x + y");
		evaluator.prefixBindingWithoutPrefixOrSuffix(sb, "field");
		assertEquals("x + y", sb.toString());
	}

	@Test
	void prefixBindingHandlesSingleBeanDotReference() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		StringBuilder sb = new StringBuilder("bean.name");
		evaluator.prefixBindingWithoutPrefixOrSuffix(sb, "contact");
		assertEquals("bean.contact.name", sb.toString());
	}

	@Test
	void prefixBindingLeavesExpressionWithNoBeanUnchanged() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		StringBuilder sb = new StringBuilder("user.name");
		evaluator.prefixBindingWithoutPrefixOrSuffix(sb, "field");
		assertEquals("user.name", sb.toString());
	}

	// ---- constructor variants ----

	@Test
	void constructorWithTypesafeTrue() {
		assertDoesNotThrow(() -> new ELExpressionEvaluator(true));
	}

	@Test
	void constructorWithTypesafeFalse() {
		assertDoesNotThrow(() -> new ELExpressionEvaluator(false));
	}

	// ---- validateWithoutPrefixOrSuffix — typesafe=true paths ----

	@Test
	void validateWithTypesafeAndReturnTypeIncompatibilityReturnsErrorMessage() {
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(true);
		Customer customer = mock(Customer.class);
		// "user" evaluates to UserImpl.class (Class<?>) — not assignable to String
		String result = evaluator.validateWithoutPrefixOrSuffix("user", String.class, customer, null, null);
		assertNotNull(result, "Should return error message for incompatible return type");
		assertTrue(result.contains("incompatible"), "Error should mention incompatibility");
	}


}
