package org.skyve.impl.bind;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

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
	
	@Override
	public String validateWithoutPrefix(String expression,
											Class<?> returnType,
											Customer customer,
											Module module,
											Document document) {
		return null; // any key is valid
	}
}
