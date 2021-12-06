package org.skyve.impl.bind;

import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;

class I18NExpressionEvaluator extends ExpressionEvaluator {
	static final String PREFIX = "i18n";
	
	@Override
	public Object evaluateWithoutPrefix(String expression, Bean bean) {
		return formatWithoutPrefix(expression, bean);
	}

	@Override
	public String formatWithoutPrefix(String expression, Bean bean) {
		return Util.i18n(expression);
	}
	
	@Override
	public String validateWithoutPrefix(String expression, Customer customer, Module module, Document document) {
		return null; // any key is valid
	}
}
