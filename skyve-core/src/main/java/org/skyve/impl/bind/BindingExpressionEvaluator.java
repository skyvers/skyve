package org.skyve.impl.bind;

import org.skyve.CORE;
import org.skyve.domain.Bean;

class BindingExpressionEvaluator extends ExpressionEvaluator {
	static final String PREFIX = "bean";
	
	@Override
	public Object evaluateWithoutPrefix(String expression, Bean bean) {
		return BindUtil.get(bean, expression);
	}

	@Override
	public String formatWithoutPrefix(String expression, Bean bean) {
		return BindUtil.getDisplay(CORE.getCustomer(), bean, expression);
	}
}
