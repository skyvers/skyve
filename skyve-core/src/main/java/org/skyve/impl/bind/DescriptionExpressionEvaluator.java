package org.skyve.impl.bind;

import org.skyve.domain.Bean;
import org.skyve.metadata.model.Attribute;

public class DescriptionExpressionEvaluator extends MetaDataExpressionEvaluator {
	public static final String PREFIX = "desc";
	
	@Override
	public Object evaluateWithoutPrefix(String expression, Bean bean) {
		Attribute a = obtainAttribute(expression, bean);
		return (a == null) ? null : a.getLocalisedDescription();
	}

	@Override
	public String formatWithoutPrefix(String expression, Bean bean) {
		Object result = evaluateWithoutPrefix(expression, bean);
		return (result == null) ? "" : result.toString();
	}
}
