package org.skyve.impl.bind;

import org.skyve.domain.Bean;
import org.skyve.metadata.model.Attribute;

public class DescriptionExpressionEvaluator extends MetaDataExpressionEvaluator {
	public static final String PREFIX = "desc";
	
	@Override
	public Object evaluateWithoutPrefixOrSuffix(String expression, Bean bean) {
		Attribute a = obtainAttribute(expression, bean);
		return (a == null) ? null : a.getLocalisedDescription();
	}

	@Override
	public String formatWithoutPrefixOrSuffix(String expression, Bean bean) {
		Object result = evaluateWithoutPrefixOrSuffix(expression, bean);
		return (result == null) ? "" : result.toString();
	}
	
	@Override
	public void prefixBindingWithoutPrefixOrSuffix(StringBuilder expression, String binding) {
		expression.insert(0, '.');
		expression.insert(0, binding);
	}
}
