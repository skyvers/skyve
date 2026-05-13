package org.skyve.impl.bind;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class ExpressionEvaluatorTest {

	// --- RoleExpressionEvaluator.validateWithoutPrefixOrSuffix ---

	@Test
	void roleValidateExpressionWithNoDotReturnsError() {
		RoleExpressionEvaluator evaluator = new RoleExpressionEvaluator();
		String result = evaluator.validateWithoutPrefixOrSuffix("noModuleRole", null, null, null, null);
		assertThat(result != null, is(true));
	}

	@Test
	void roleValidateExpressionWithDotAtStartReturnsError() {
		RoleExpressionEvaluator evaluator = new RoleExpressionEvaluator();
		// dotIndex == 0 means <= 0 → error
		String result = evaluator.validateWithoutPrefixOrSuffix(".role", null, null, null, null);
		assertThat(result != null, is(true));
	}

	@Test
	void roleValidateExpressionWithDotInMiddleReturnsNull() {
		RoleExpressionEvaluator evaluator = new RoleExpressionEvaluator();
		String result = evaluator.validateWithoutPrefixOrSuffix("module.role", null, null, null, null);
		assertThat(result, is(nullValue()));
	}

	@Test
	void roleValidateExpressionWithMultipleDotsReturnsNull() {
		RoleExpressionEvaluator evaluator = new RoleExpressionEvaluator();
		String result = evaluator.validateWithoutPrefixOrSuffix("module.submodule.role", null, null, null, null);
		assertThat(result, is(nullValue()));
	}

	// --- RoleExpressionEvaluator constants ---

	@Test
	void roleEvaluatorPrefixIsRole() {
		assertThat(RoleExpressionEvaluator.PREFIX, is("role"));
	}

	// --- DescriptionExpressionEvaluator constants and prefix binding ---

	@Test
	void descriptionEvaluatorPrefixIsDesc() {
		assertThat(DescriptionExpressionEvaluator.PREFIX, is("desc"));
	}

	@Test
	void descriptionEvaluatorPrefixBindingPrependsBindingWithDot() {
		DescriptionExpressionEvaluator evaluator = new DescriptionExpressionEvaluator();
		StringBuilder sb = new StringBuilder("fieldName");
		evaluator.prefixBindingWithoutPrefixOrSuffix(sb, "parent");
		assertThat(sb.toString(), is("parent.fieldName"));
	}

	@Test
	void descriptionEvaluatorPrefixBindingWithEmptyExpressionGivesDotSuffix() {
		DescriptionExpressionEvaluator evaluator = new DescriptionExpressionEvaluator();
		StringBuilder sb = new StringBuilder("");
		evaluator.prefixBindingWithoutPrefixOrSuffix(sb, "parent");
		assertThat(sb.toString(), is("parent."));
	}

	// --- DisplayNameExpressionEvaluator constants and prefix binding ---

	@Test
	void displayNameEvaluatorPrefixIsDisp() {
		assertThat(DisplayNameExpressionEvaluator.PREFIX, is("disp"));
	}

	@Test
	void displayNameEvaluatorPrefixBindingPrependsBindingWithDot() {
		DisplayNameExpressionEvaluator evaluator = new DisplayNameExpressionEvaluator();
		StringBuilder sb = new StringBuilder("fieldName");
		evaluator.prefixBindingWithoutPrefixOrSuffix(sb, "contact");
		assertThat(sb.toString(), is("contact.fieldName"));
	}

	// --- RoleExpressionEvaluator.prefixBindingWithoutPrefixOrSuffix does nothing ---

	@Test
	void roleEvaluatorPrefixBindingDoesNotModifyExpression() {
		RoleExpressionEvaluator evaluator = new RoleExpressionEvaluator();
		StringBuilder sb = new StringBuilder("module.role");
		evaluator.prefixBindingWithoutPrefixOrSuffix(sb, "someBinding");
		assertThat(sb.toString(), is("module.role"));
	}
}
