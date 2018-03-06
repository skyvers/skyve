package org.skyve.impl.sail.mock;

import javax.el.ELContext;
import javax.el.ExpressionFactory;
import javax.el.MethodExpression;
import javax.el.ValueExpression;

public class MockExpressionFactory extends ExpressionFactory {
	@Override
	public ValueExpression createValueExpression(ELContext context, String expression, Class<?> expectedType) {
		return null;
	}

	@Override
	public ValueExpression createValueExpression(Object instance, Class<?> expectedType) {
		return null;
	}

	@Override
	public MethodExpression createMethodExpression(ELContext context,
													String expression,
													Class<?> expectedReturnType,
													Class<?>[] expectedParamTypes) {
		return null;
	}

	@Override
	public Object coerceToType(Object obj, Class<?> targetType) {
		return null;
	}
}
