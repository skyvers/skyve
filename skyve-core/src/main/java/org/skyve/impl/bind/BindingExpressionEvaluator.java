package org.skyve.impl.bind;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Binder.TargetMetaData;

class BindingExpressionEvaluator extends ExpressionEvaluator {
	static final String PREFIX = "bean";
	
	@Override
	public Object evaluateWithoutPrefix(String expression, Bean bean) {
		return BindUtil.get(bean, expression);
	}

	@Override
	public String formatWithoutPrefix(String expression, Bean bean) {
		return BindUtil.getDisplay(CORE.getCustomer(), bean, expression);
	}
	
	@Override
	public boolean validateWithoutPrefix(String expression, Customer customer, Module module, Document document) {
		boolean valid = true;
		
		try {
			// Check the binding in this bean
			TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, expression);
			if (target == null) {
				valid = false;
			}
		}
		catch (@SuppressWarnings("unused") Exception e) {
			valid = false;
		}

		return valid;
	}
}
