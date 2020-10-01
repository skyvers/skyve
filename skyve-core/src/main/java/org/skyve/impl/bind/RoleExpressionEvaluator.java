package org.skyve.impl.bind;

import org.skyve.CORE;
import org.skyve.domain.Bean;

class RoleExpressionEvaluator extends ExpressionEvaluator {
	static final String PREFIX = "role";
	
	@Override
	public Object evaluateWithoutPrefix(String expression, Bean bean) {
		int dotIndex = expression.indexOf('.');
		return CORE.getUser().isInRole(expression.substring(0, dotIndex),
										expression.substring(dotIndex + 1)) ?
				Boolean.TRUE :
				Boolean.FALSE;
	}

	@Override
	public String formatWithoutPrefix(String expression, Bean bean) {
		return BindUtil.toDisplay(CORE.getCustomer(), null, null, evaluateWithoutPrefix(expression, bean));
	}
}
