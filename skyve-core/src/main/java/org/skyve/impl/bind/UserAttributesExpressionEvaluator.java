package org.skyve.impl.bind;

import org.skyve.CORE;
import org.skyve.domain.Bean;

class UserAttributesExpressionEvaluator extends ExpressionEvaluator {
	static final String PREFIX = "user";
	
	@Override
	public Object evaluateWithoutPrefix(String expression, Bean bean) {
		return CORE.getUser().getAttributes().get(expression);
	}

	@Override
	public String formatWithoutPrefix(String expression, Bean bean) {
		return BindUtil.toDisplay(CORE.getCustomer(), null, null, evaluateWithoutPrefix(expression, bean));
	}
}
