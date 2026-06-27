package org.skyve.impl.sail.mock;

import jakarta.el.ELContext;
import jakarta.el.ExpressionFactory;
import jakarta.el.MethodExpression;
import jakarta.el.ValueExpression;

/**
 * Provides a mock implementation used by SAIL execution tests in the web module.
 */
public class MockExpressionFactory extends ExpressionFactory {
	/**
	 * Returns no value expression because EL parsing is not required in these tests.
	 */
	@Override
	public ValueExpression createValueExpression(ELContext context, String expression, Class<?> expectedType) {
		return null;
	}

	/**
	 * Returns no value expression because EL parsing is not required in these tests.
	 */
	@Override
	public ValueExpression createValueExpression(Object instance, Class<?> expectedType) {
		return null;
	}

	/**
	 * Returns no method expression because EL method binding is not required in these tests.
	 */
	@Override
	public MethodExpression createMethodExpression(ELContext context,
													String expression,
													Class<?> expectedReturnType,
													Class<?>[] expectedParamTypes) {
		return null;
	}

	/**
	 * Returns no coerced value because type coercion is not required in these tests.
	 */
	@Override
	public <T> T coerceToType(Object obj, Class<T> targetType) {
		return null;
	}
}
