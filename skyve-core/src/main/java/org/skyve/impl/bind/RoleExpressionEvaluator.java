package org.skyve.impl.bind;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

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
	
	@Override
	public boolean validateWithoutPrefix(String expression, Customer customer, Module module, Document document) {
		return (expression.indexOf('.') > 0);
	}
}
