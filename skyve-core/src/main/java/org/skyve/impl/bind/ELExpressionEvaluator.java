package org.skyve.impl.bind;

import javax.el.ELProcessor;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

class ELExpressionEvaluator extends ExpressionEvaluator {
	static final String PREFIX = "el";
	
	@Override
	public Object evaluateWithoutPrefix(String expression, Bean bean) {
		ELProcessor elp = new ELProcessor();
		elp.defineBean("bean", bean);
		elp.defineBean("user", CORE.getUser());
		elp.defineBean("stash", CORE.getStash());
		long millis = System.currentTimeMillis();
		elp.defineBean(DATE_EXPRESSION, new DateOnly(millis));
		elp.defineBean(TIME_EXPRESSION, new TimeOnly(millis));
		elp.defineBean(DATETIME_EXPRESSION, new DateTime(millis));
		elp.defineBean(TIMESTAMP_EXPRESSION, new Timestamp(millis));

		return elp.eval(expression);
	}

	@Override
	public String formatWithoutPrefix(String expression, Bean bean) {
		return BindUtil.toDisplay(CORE.getCustomer(), null, null, evaluateWithoutPrefix(expression, bean));
	}
	
	@Override
	public boolean validateWithoutPrefix(String expression, Customer customer, Module module, Document document) {
		// TODO investigate validating the EL.
		// The least I can try is creating a bean
		return true;
	}
}
