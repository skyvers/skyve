package org.skyve.impl.web.faces.pipeline;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.function.BiConsumer;

import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.skyve.util.OWASP;

import jakarta.el.ELContext;
import jakarta.el.ExpressionFactory;
import jakarta.faces.application.Application;
import jakarta.faces.component.UIComponent;
import jakarta.faces.context.FacesContext;

@SuppressWarnings("static-method")
class AbstractFacesBuilderTest {
	private static final String UNSAFE_TEXT = "<img src=x onerror=alert(1)> & \"quoted\" 'single'";

	@Test
	void escapeFacesTextReturnsNullForNullValue() {
		assertNull(AbstractFacesBuilder.escapeFacesText(null, true));
		assertNull(AbstractFacesBuilder.escapeFacesText(null, true));
		assertNull(AbstractFacesBuilder.escapeFacesText(null, false));
	}

	@Test
	void escapeFacesTextEscapesHtmlByDefault() {
		assertEquals(OWASP.escapeHtml(UNSAFE_TEXT), AbstractFacesBuilder.escapeFacesText(UNSAFE_TEXT, true));
	}

	@Test
	void escapeFacesTextEscapesHtmlWhenExplicitlyTrue() {
		assertEquals(OWASP.escapeHtml(UNSAFE_TEXT), AbstractFacesBuilder.escapeFacesText(UNSAFE_TEXT, true));
	}

	@Test
	void escapeFacesTextLeavesTrustedHtmlRawWhenFalse() {
		assertEquals(UNSAFE_TEXT, AbstractFacesBuilder.escapeFacesText(UNSAFE_TEXT, false));
	}

	@Test
	void escapeFacesAttributeReturnsNullForNullValue() {
		assertNull(AbstractFacesBuilder.escapeFacesAttribute(null, true));
		assertNull(AbstractFacesBuilder.escapeFacesAttribute(null, true));
		assertNull(AbstractFacesBuilder.escapeFacesAttribute(null, false));
	}

	@Test
	void escapeFacesAttributeEscapesHtmlByDefaultThenAttributeSyntax() {
		String result = AbstractFacesBuilder.escapeFacesAttribute(UNSAFE_TEXT, true);

		assertEquals("&amp;lt;img src=x onerror=alert(1)&amp;gt; &amp;amp; &amp;#34;quoted&amp;#34; &amp;#39;single&amp;#39;", result);
		assertFalse(result.contains("\""));
		assertFalse(result.contains("'"));
		assertFalse(result.contains("<"));
		assertFalse(result.contains(">"));
	}

	@Test
	@SuppressWarnings("java:S4144") // deliberately the same as the test above - escaping is the default, so explicit true must behave identically
	void escapeFacesAttributeEscapesHtmlWhenExplicitlyTrueThenAttributeSyntax() {
		String result = AbstractFacesBuilder.escapeFacesAttribute(UNSAFE_TEXT, true);

		assertEquals("&amp;lt;img src=x onerror=alert(1)&amp;gt; &amp;amp; &amp;#34;quoted&amp;#34; &amp;#39;single&amp;#39;", result);
		assertFalse(result.contains("\""));
		assertFalse(result.contains("'"));
		assertFalse(result.contains("<"));
		assertFalse(result.contains(">"));
	}

	@Test
	void escapeFacesAttributeLeavesTrustedHtmlRawWhenFalseButEscapesAttributeSyntax() {
		String result = AbstractFacesBuilder.escapeFacesAttribute(UNSAFE_TEXT, false);

		assertEquals("&lt;img src=x onerror=alert(1)> &amp; &#34;quoted&#34; &#39;single&#39;", result);
		assertFalse(result.contains("\""));
		assertFalse(result.contains("'"));
		assertFalse(result.contains("<"));
	}

	@Test
	void isInlineConditionIsTrueForAnExpressionAndItsInverseOnly() {
		assertTrue(AbstractFacesBuilder.isInlineCondition("{el:bean.active}"));
		assertTrue(AbstractFacesBuilder.isInlineCondition("not{el:bean.active}"));
		assertFalse(AbstractFacesBuilder.isInlineCondition("active"));
		assertFalse(AbstractFacesBuilder.isInlineCondition("notActive"));
		assertFalse(AbstractFacesBuilder.isInlineCondition("true"));
		assertFalse(AbstractFacesBuilder.isInlineCondition(null));
	}

	@Test
	void createConditionFragmentLooksUpAConditionNameAsAMapKey() {
		assertEquals("skyve.currentBean['notActive']", AbstractFacesBuilder.createConditionFragment("skyve.currentBean", "notActive"));
	}

	@Test
	void createConditionFragmentEvaluatesAnInlineExpression() {
		assertEquals("skyve.currentBean.evaluateCondition('{el:bean.active}')",
						AbstractFacesBuilder.createConditionFragment("skyve.currentBean", "{el:bean.active}"));
	}

	@Test
	void createConditionFragmentEvaluatesTheInverseOfAnInlineExpression() {
		assertEquals("row.evaluateCondition('not{el:bean.active}')",
						AbstractFacesBuilder.createConditionFragment("row", "not{el:bean.active}"));
	}

	@Test
	void createConditionFragmentEscapesQuotesAndBackslashesForAnELStringLiteral() {
		assertEquals("skyve.currentBean.evaluateCondition('{el:bean.text eq \\'a\\\\b\\'}')",
						AbstractFacesBuilder.createConditionFragment("skyve.currentBean", "{el:bean.text eq 'a\\b'}"));
	}

	@Test
	void setInvisibleForARowLooksUpAConditionNameOnTheRowVariable() {
		assertEquals("#{row.notHidden}", renderedExpressionForRow("hidden", null));
	}

	@Test
	void setInvisibleForARowEvaluatesAnInlineConditionOnTheRowVariable() {
		// invisible when the expression is true, so rendered when its inverse is
		assertEquals("#{row.evaluateCondition('not{el:bean.text eq \\'a\\'}')}", renderedExpressionForRow("{el:bean.text eq 'a'}", null));
	}

	@Test
	void setInvisibleForARowEvaluatesTheAuthoredExpressionWhenTheInvisibleConditionIsItsInverse() {
		assertEquals("#{row.evaluateCondition('{el:bean.active}') and other}", renderedExpressionForRow("not{el:bean.active}", "other"));
	}

	@Test
	void setInvisibleForARowKeepsABooleanLiteral() {
		withFacesBuilder((builder, expressionFactory) -> {
			UIComponent component = mock(UIComponent.class);
			builder.setInvisible(component, "row", "true", null);

			// invisible is true, so rendered is the literal false and no row is consulted
			verify(expressionFactory).createValueExpression("false", Boolean.class);
			verify(component).setValueExpression(eq("rendered"), any());
		});
	}

	@Test
	void setInvisibleForARowWithAFalseLiteralLeavesOnlyTheExtraCondition() {
		assertEquals("#{other}", renderedExpressionForRow("false", "other"));
	}

	@Test
	void setInvisibleWithoutARowVariableLooksUpAConditionNameOnTheCurrentBean() {
		assertEquals("#{skyve.currentBean['notHidden']}", renderedExpression(null, "hidden", null));
	}

	@Test
	void setInvisibleWithoutARowVariableEvaluatesAnInlineConditionOnTheCurrentBean() {
		assertEquals("#{skyve.currentBean.evaluateCondition('not{el:bean.active}') and other}", renderedExpression(null, "{el:bean.active}", "other"));
	}

	@Test
	void setDisabledForARowLooksUpAConditionNameOnTheRowVariable() {
		assertEquals("#{row['locked']}", disabledExpression("row", "locked", null));
	}

	@Test
	void setDisabledForARowEvaluatesAnInlineConditionOnTheRowVariable() {
		assertEquals("#{row.evaluateCondition('{el:bean.text eq \\'a\\'}')}", disabledExpression("row", "{el:bean.text eq 'a'}", null));
	}

	@Test
	void setDisabledForARowSanitisesTheRowVariable() {
		assertEquals("#{parent_linesRow['notActive']}", disabledExpression("parent.linesRow", "notActive", null));
	}

	@Test
	void setDisabledForARowKeepsABooleanLiteral() {
		assertEquals("#{true}", disabledExpression("row", "true", null));
	}

	@Test
	void setDisabledForARowEvaluatesTheFormConditionOnTheCurrentBean() {
		assertEquals("#{row['locked'] or skyve.currentBean['formLocked']}", disabledExpression("row", "locked", "formLocked"));
	}

	@Test
	void setDisabledForARowWithOnlyAFormConditionEvaluatesItOnTheCurrentBean() {
		assertEquals("#{skyve.currentBean['formLocked']}", disabledExpression("row", null, "formLocked"));
	}

	@Test
	void setDisabledWithoutARowVariableLooksUpAConditionNameOnTheCurrentBean() {
		assertEquals("#{skyve.currentBean['locked']}", disabledExpression(null, "locked", null));
	}

	@Test
	void setDisabledWithoutARowVariableEvaluatesAnInlineConditionOnTheCurrentBean() {
		assertEquals("#{skyve.currentBean.evaluateCondition('{el:bean.active}')}", disabledExpression(null, "{el:bean.active}", null));
	}

	@Test
	void createOredValueExpressionFragmentMixesInlineNamedAndLiteralConditions() {
		withFacesBuilder((builder, expressionFactory) -> assertEquals("skyve.currentBean.evaluateCondition('{el:bean.active}') or skyve.currentBean['locked'] or true",
																		builder.createOredValueExpressionFragmentFromConditions(new String[] {"{el:bean.active}", "locked", "true"})));
	}

	/**
	 * Applies an invisible condition for a data widget row variable and returns the EL created for the rendered attribute.
	 */
	private static String renderedExpressionForRow(String invisibleCondition, String extraELToAnd) {
		return renderedExpression("row", invisibleCondition, extraELToAnd);
	}

	/**
	 * Applies an invisible condition for an optional data widget variable and returns the EL created for the rendered attribute.
	 */
	private static String renderedExpression(String dataWidgetVar, String invisibleCondition, String extraELToAnd) {
		String[] result = new String[1];
		withFacesBuilder((builder, expressionFactory) -> {
			UIComponent component = mock(UIComponent.class);
			builder.setInvisible(component, dataWidgetVar, invisibleCondition, extraELToAnd);

			ArgumentCaptor<String> expression = ArgumentCaptor.forClass(String.class);
			verify(expressionFactory).createValueExpression(any(ELContext.class), expression.capture(), eq(Boolean.class));
			verify(component).setValueExpression(eq("rendered"), any());
			result[0] = expression.getValue();
		});
		return result[0];
	}

	/**
	 * Applies a disabled condition for an optional data widget variable and returns the EL created for the disabled attribute.
	 */
	private static String disabledExpression(String dataWidgetVar, String disabledCondition, String formDisabledCondition) {
		String[] result = new String[1];
		withFacesBuilder((builder, expressionFactory) -> {
			UIComponent component = mock(UIComponent.class);
			builder.setDisabled(component, dataWidgetVar, disabledCondition, formDisabledCondition);

			ArgumentCaptor<String> expression = ArgumentCaptor.forClass(String.class);
			verify(expressionFactory).createValueExpression(any(ELContext.class), expression.capture(), eq(Boolean.class));
			verify(component).setValueExpression(eq("disabled"), any());
			result[0] = expression.getValue();
		});
		return result[0];
	}

	/**
	 * Runs a test body against a builder constructed while a mocked faces context is current.
	 */
	private static void withFacesBuilder(BiConsumer<AbstractFacesBuilder, ExpressionFactory> test) {
		FacesContext facesContext = mock(FacesContext.class);
		Application application = mock(Application.class);
		ExpressionFactory expressionFactory = mock(ExpressionFactory.class);
		when(facesContext.getApplication()).thenReturn(application);
		ELContext elc = mock(ELContext.class);
		when(facesContext.getELContext()).thenReturn(elc);
		when(application.getExpressionFactory()).thenReturn(expressionFactory);
		FacesContextBridge.setCurrent(facesContext);
		try {
			test.accept(new AbstractFacesBuilder() { /* nothing to implement */ }, expressionFactory);
		}
		finally {
			FacesContextBridge.setCurrent(null);
		}
	}

	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext facesContext) {
			setCurrentInstance(facesContext);
		}
	}

	@Test
	void sanitiseFacesTextReturnsNullForNullValue() {
		assertNull(AbstractFacesBuilder.sanitiseFacesText(null));
	}

	@Test
	void sanitiseFacesTextStripsMarkupAndKeepsText() {
		assertEquals("Name required", AbstractFacesBuilder.sanitiseFacesText("<b>Name required</b>"));
	}
}
