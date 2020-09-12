package org.skyve.impl.bind;

import org.skyve.CORE;
import org.skyve.domain.Bean;

class StashExpressionEvaluator extends ExpressionEvaluator {
	static final String PREFIX = "stash";
	
	@Override
	public Object evaluateWithoutPrefix(String expression, Bean bean) {
		return CORE.getStash().get(expression);
	}

	@Override
	public String formatWithoutPrefix(String expression, Bean bean) {
		return BindUtil.toDisplay(CORE.getCustomer(), null, null, evaluateWithoutPrefix(expression, bean));
	}
}
