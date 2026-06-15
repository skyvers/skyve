package org.skyve.impl.bind;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class DescriptionExpressionEvaluatorTest {

	@Test
	void prefixBindingPrependsBindingWithDot() {
		DescriptionExpressionEvaluator evaluator = new DescriptionExpressionEvaluator();
		StringBuilder sb = new StringBuilder("fieldName");
		evaluator.prefixBindingWithoutPrefixOrSuffix(sb, "owner");
		assertThat(sb.toString(), is("owner.fieldName"));
	}

	@Test
	void prefixBindingPrependsEmptyBinding() {
		DescriptionExpressionEvaluator evaluator = new DescriptionExpressionEvaluator();
		StringBuilder sb = new StringBuilder("name");
		evaluator.prefixBindingWithoutPrefixOrSuffix(sb, "");
		assertThat(sb.toString(), is(".name"));
	}

	@Test
	void prefixConstantIsDesc() {
		assertThat(DescriptionExpressionEvaluator.PREFIX, is("desc"));
	}
}
