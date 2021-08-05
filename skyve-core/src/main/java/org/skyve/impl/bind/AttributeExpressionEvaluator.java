package org.skyve.impl.bind;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Binder.TargetMetaData;

abstract class AttributeExpressionEvaluator extends ExpressionEvaluator {
	@SuppressWarnings("static-method")
	protected Attribute obtainAttribute(String expression, Bean bean) {
		Customer c = CORE.getCustomer();
		Module m = c.getModule(bean.getBizModule());
		Document d = m.getDocument(c, bean.getBizDocument());
		TargetMetaData target = BindUtil.getMetaDataForBinding(c, m, d, expression);
		if (target != null) {
			return target.getAttribute();
		}
		return null;
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
