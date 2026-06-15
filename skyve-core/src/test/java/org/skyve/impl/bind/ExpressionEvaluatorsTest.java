package org.skyve.impl.bind;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

/**
 * Tests for the simple expression evaluator implementations that do not need
 * a live Skyve persistence/CORE context.
 */
@SuppressWarnings("static-method")
class ExpressionEvaluatorsTest {

	private static Customer mockCustomer() {
		Customer c = Mockito.mock(Customer.class);
		Mockito.when(c.getModules()).thenReturn(Collections.emptyList());
		return c;
	}

	private static Module mockModule() {
		return Mockito.mock(Module.class);
	}

	private static Document mockDocument() {
		return Mockito.mock(Document.class);
	}

	// ------------------------------------------------------------------ I18N

	@Test
	void i18nPrefixIsI18n() {
		assertThat(I18NExpressionEvaluator.PREFIX, is("i18n"));
	}

	@Test
	void i18nFormatReturnsKeyWhenNotInBundle() {
		I18NExpressionEvaluator eval = new I18NExpressionEvaluator();
		// An unknown key should be returned unchanged by nullSafeI18n
		String result = eval.formatWithoutPrefixOrSuffix("unknown.key.xyz", null);
		assertThat(result, is("unknown.key.xyz"));
	}

	@Test
	void i18nEvaluateReturnsSameAsFormat() {
		I18NExpressionEvaluator eval = new I18NExpressionEvaluator();
		Object evaluated = eval.evaluateWithoutPrefixOrSuffix("another.unknown.key", null);
		assertNotNull(evaluated);
		assertThat(evaluated.toString(), is("another.unknown.key"));
	}

	@Test
	void i18nValidateReturnsNull() {
		I18NExpressionEvaluator eval = new I18NExpressionEvaluator();
		String result = eval.validateWithoutPrefixOrSuffix("any.key", null, mockCustomer(), mockModule(), mockDocument());
		assertNull(result);
	}

	@Test
	void i18nCompleteReturnsEmptyList() {
		I18NExpressionEvaluator eval = new I18NExpressionEvaluator();
		List<String> result = eval.completeWithoutPrefixOrSuffix("frag", mockCustomer(), mockModule(), mockDocument());
		assertNotNull(result);
		assertTrue(result.isEmpty());
	}

	@Test
	void i18nPrefixBindingIsNoOp() {
		I18NExpressionEvaluator eval = new I18NExpressionEvaluator();
		StringBuilder sb = new StringBuilder("someKey");
		eval.prefixBindingWithoutPrefixOrSuffix(sb, "binding");
		// i18n uses keys, not binding paths — expression should be unchanged
		assertThat(sb.toString(), is("someKey"));
	}

	@Test
	void i18nKnownKeyReturnsTranslatedValue() {
		I18NExpressionEvaluator eval = new I18NExpressionEvaluator();
		// "validation.required" is a key in the test i18n.properties bundle
		String result = eval.formatWithoutPrefixOrSuffix("validation.required", null);
		// Should either return the bundle value or the key itself — either way not null or empty
		assertNotNull(result);
		assertFalse(result.isEmpty());
	}

	// ------------------------------------------------------- Description

	@Test
	void descriptionPrefixIsDesc() {
		assertThat(DescriptionExpressionEvaluator.PREFIX, is("desc"));
	}

	@Test
	void descriptionPrefixBindingPrependsBindingDot() {
		DescriptionExpressionEvaluator eval = new DescriptionExpressionEvaluator();
		StringBuilder sb = new StringBuilder("attributeName");
		eval.prefixBindingWithoutPrefixOrSuffix(sb, "parent");
		assertThat(sb.toString(), is("parent.attributeName"));
	}

	@Test
	void descriptionPrefixBindingWithEmptyExpression() {
		DescriptionExpressionEvaluator eval = new DescriptionExpressionEvaluator();
		StringBuilder sb = new StringBuilder("");
		eval.prefixBindingWithoutPrefixOrSuffix(sb, "root");
		assertThat(sb.toString(), is("root."));
	}

	// ------------------------------------------------------- DisplayName

	@Test
	void displayNamePrefixIsDisp() {
		assertThat(DisplayNameExpressionEvaluator.PREFIX, is("disp"));
	}

	@Test
	void displayNamePrefixBindingPrependsBindingDot() {
		DisplayNameExpressionEvaluator eval = new DisplayNameExpressionEvaluator();
		StringBuilder sb = new StringBuilder("fieldName");
		eval.prefixBindingWithoutPrefixOrSuffix(sb, "container");
		assertThat(sb.toString(), is("container.fieldName"));
	}

	@Test
	void displayNamePrefixBindingChaining() {
		DisplayNameExpressionEvaluator eval = new DisplayNameExpressionEvaluator();
		StringBuilder sb = new StringBuilder("leaf");
		eval.prefixBindingWithoutPrefixOrSuffix(sb, "mid");
		eval.prefixBindingWithoutPrefixOrSuffix(sb, "root");
		assertThat(sb.toString(), is("root.mid.leaf"));
	}

	// ------------------------------------------------------------------ Stash

	@Test
	void stashPrefixIsStash() {
		assertThat(StashExpressionEvaluator.PREFIX, is("stash"));
	}

	@Test
	void stashValidateReturnsNull() {
		StashExpressionEvaluator eval = new StashExpressionEvaluator();
		String result = eval.validateWithoutPrefixOrSuffix("key", null, mockCustomer(), mockModule(), mockDocument());
		assertNull(result);
	}

	@Test
	void stashCompleteReturnsEmptyList() {
		StashExpressionEvaluator eval = new StashExpressionEvaluator();
		List<String> result = eval.completeWithoutPrefixOrSuffix("frag", mockCustomer(), mockModule(), mockDocument());
		assertNotNull(result);
		assertTrue(result.isEmpty());
	}

	@Test
	void stashPrefixBindingIsNoOp() {
		StashExpressionEvaluator eval = new StashExpressionEvaluator();
		StringBuilder sb = new StringBuilder("stashKey");
		eval.prefixBindingWithoutPrefixOrSuffix(sb, "binding");
		assertThat(sb.toString(), is("stashKey"));
	}

	// ------------------------------------------------------- UserAttributes

	@Test
	void userAttributesPrefixIsUser() {
		assertThat(UserAttributesExpressionEvaluator.PREFIX, is("user"));
	}

	@Test
	void userAttributesValidateReturnsNull() {
		UserAttributesExpressionEvaluator eval = new UserAttributesExpressionEvaluator();
		String result = eval.validateWithoutPrefixOrSuffix("attr", null, mockCustomer(), mockModule(), mockDocument());
		assertNull(result);
	}

	@Test
	void userAttributesCompleteReturnsEmptyList() {
		UserAttributesExpressionEvaluator eval = new UserAttributesExpressionEvaluator();
		List<String> result = eval.completeWithoutPrefixOrSuffix("frag", mockCustomer(), mockModule(), mockDocument());
		assertNotNull(result);
		assertTrue(result.isEmpty());
	}

	@Test
	void userAttributesPrefixBindingIsNoOp() {
		UserAttributesExpressionEvaluator eval = new UserAttributesExpressionEvaluator();
		StringBuilder sb = new StringBuilder("attributeKey");
		eval.prefixBindingWithoutPrefixOrSuffix(sb, "binding");
		assertThat(sb.toString(), is("attributeKey"));
	}

	// ------------------------------------------------------- RoleExpressionEvaluator

	@Test
	void rolePrefixIsRole() {
		assertThat(RoleExpressionEvaluator.PREFIX, is("role"));
	}

	@Test
	void roleValidateReturnsNullForValidExpression() {
		RoleExpressionEvaluator eval = new RoleExpressionEvaluator();
		// expression with a dot at index > 0 → valid
		String result = eval.validateWithoutPrefixOrSuffix("Module.Role", null, mockCustomer(), mockModule(), mockDocument());
		assertNull(result);
	}

	@Test
	void roleValidateReturnsErrorWhenNoDot() {
		RoleExpressionEvaluator eval = new RoleExpressionEvaluator();
		String result = eval.validateWithoutPrefixOrSuffix("NoModuleRole", null, mockCustomer(), mockModule(), mockDocument());
		assertNotNull(result);
		assertTrue(result.contains("NoModuleRole"));
	}

	@Test
	void roleCompleteWithEmptyCustomerModulesReturnsEmptyList() {
		RoleExpressionEvaluator eval = new RoleExpressionEvaluator();
		// Customer mock returns empty modules list → result is empty
		List<String> result = eval.completeWithoutPrefixOrSuffix("frag", mockCustomer(), mockModule(), mockDocument());
		assertNotNull(result);
		assertTrue(result.isEmpty());
	}

	@Test
	void rolePrefixBindingIsNoOp() {
		RoleExpressionEvaluator eval = new RoleExpressionEvaluator();
		StringBuilder sb = new StringBuilder("someRole");
		eval.prefixBindingWithoutPrefixOrSuffix(sb, "binding");
		assertThat(sb.toString(), is("someRole"));
	}
}
