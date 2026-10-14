package org.skyve.impl.sail.mock;

import jakarta.annotation.Nullable;
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
	public @Nullable ValueExpression createValueExpression(@Nullable ELContext context, @Nullable String expression, @Nullable Class<?> expectedType) {
		return null;
	}

	/**
	 * Returns no value expression because EL parsing is not required in these tests.
	 */
	@Override
	public @Nullable ValueExpression createValueExpression(@Nullable Object instance, @Nullable Class<?> expectedType) {
		return null;
	}

	/**
	 * Returns no method expression because EL method binding is not required in these tests.
	 */
	@Override
	public @Nullable MethodExpression createMethodExpression(@Nullable ELContext context,
																@Nullable String expression,
																@Nullable Class<?> expectedReturnType,
																@Nullable Class<?>[] expectedParamTypes) {
		return null;
	}

	/**
	 * Returns no coerced value because type coercion is not required in these tests.
	 */
	@Override
	public @Nullable <T> T coerceToType(@Nullable Object obj, @Nullable Class<T> targetType) {
		return null;
	}
}
