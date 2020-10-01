package org.skyve.impl.bind;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.util.Util;

class I18NExpressionEvaluator extends ExpressionEvaluator {
	static final String PREFIX = "i18n";
	
	@Override
	public Object evaluateWithoutPrefix(String expression, Bean bean) {
		return formatWithoutPrefix(expression, bean);
	}

	@Override
	public String formatWithoutPrefix(String expression, Bean bean) {
		return Util.i18n(expression, CORE.getUser().getLocale());
	}
}
