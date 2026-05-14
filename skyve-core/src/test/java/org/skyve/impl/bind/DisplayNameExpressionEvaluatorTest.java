package org.skyve.impl.bind;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
public class DisplayNameExpressionEvaluatorTest {

	@Test
	void prefixBindingPrependsBindingWithDot() {
		DisplayNameExpressionEvaluator evaluator = new DisplayNameExpressionEvaluator();
		StringBuilder sb = new StringBuilder("fieldName");
		evaluator.prefixBindingWithoutPrefixOrSuffix(sb, "owner");
		assertThat(sb.toString(), is("owner.fieldName"));
	}

	@Test
	void prefixBindingPrependsEmptyBinding() {
		DisplayNameExpressionEvaluator evaluator = new DisplayNameExpressionEvaluator();
		StringBuilder sb = new StringBuilder("name");
		evaluator.prefixBindingWithoutPrefixOrSuffix(sb, "");
		assertThat(sb.toString(), is(".name"));
	}

	@Test
	void prefixConstantIsDisp() {
		assertThat(DisplayNameExpressionEvaluator.PREFIX, is("disp"));
	}
}
