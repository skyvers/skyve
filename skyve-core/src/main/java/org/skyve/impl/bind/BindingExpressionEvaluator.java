package org.skyve.impl.bind;

import org.skyve.CORE;
import org.skyve.domain.Bean;

public class BindingExpressionEvaluator extends MetaDataExpressionEvaluator {
	public static final String PREFIX = "bean";
	
	@Override
	public Object evaluateWithoutPrefix(String expression, Bean bean) {
		return BindUtil.get(bean, expression);
	}

	@Override
	public String formatWithoutPrefix(String expression, Bean bean) {
		return BindUtil.getDisplay(CORE.getCustomer(), bean, expression);
	}
	
	@Override
	public void prefixBindingWithoutPrefix(StringBuilder expression, String binding) {
		expression.insert(0, '.');
		expression.insert(0, binding);
	}
}
