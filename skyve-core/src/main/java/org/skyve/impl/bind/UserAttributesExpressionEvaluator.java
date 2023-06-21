package org.skyve.impl.bind;

import java.util.Collections;
import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.ExpressionEvaluator;

public class UserAttributesExpressionEvaluator extends ExpressionEvaluator {
	public static final String PREFIX = "user";
	
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
	
	@Override
	public List<String> completeWithoutPrefix(String fragment,
												Customer customer, 
												Module module,
												Document document) {
		return Collections.emptyList(); // any key is valid
	}
	
	@Override
	public void prefixBindingWithoutPrefix(StringBuilder expression, String binding) {
		// nothing to do here as user uses keys
	}
}
