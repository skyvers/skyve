package org.skyve.impl.bind;

import org.skyve.CORE;
import org.skyve.domain.Bean;

public class BindingExpressionEvaluator extends MetaDataExpressionEvaluator {
	public static final String PREFIX = "bean";
	
	@Override
	public Object evaluateWithoutPrefixOrSuffix(String expression, Bean bean) {
		if (bean == null) {
			return null;
		}
		return BindUtil.get(bean, expression);
	}

	@Override
	public String formatWithoutPrefixOrSuffix(String expression, Bean bean) {
		if (bean == null) {
			throw new IllegalArgumentException("bean cannot be null");
		}
		return BindUtil.getDisplay(CORE.getCustomer(), bean, expression);
	}
	
	@Override
	public void prefixBindingWithoutPrefixOrSuffix(StringBuilder expression, String binding) {
		expression.insert(0, '.');
		expression.insert(0, binding);
	}
}
