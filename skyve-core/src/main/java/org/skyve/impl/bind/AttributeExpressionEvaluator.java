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
	public String validateWithoutPrefix(String expression, Customer customer, Module module, Document document) {
		String error = null;
		
		try {
			// Check the binding in this bean
			TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, expression);
			if (target == null) {
				error = "Binding " + expression + " does not resolve to a document attribute.";
			}
		}
		catch (Exception e) {
			error = e.getMessage();
			if (error == null) {
				error = "Binding " + expression + " does not resolve to a document attribute.";
			}
			else {
				error = "Binding " + expression + " does not resolve to a document attribute: " + error;
			}
		}

		return error;
	}
}
