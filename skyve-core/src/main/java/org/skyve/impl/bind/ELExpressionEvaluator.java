package org.skyve.impl.bind;

import javax.el.ELProcessor;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Timestamp;

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
		elp.defineBean(TIME_EXPRESSION, new DateOnly(millis));
		elp.defineBean(DATETIME_EXPRESSION, new DateTime(millis));
		elp.defineBean(TIMESTAMP_EXPRESSION, new Timestamp(millis));

		return elp.eval(expression);
	}

	@Override
	public String formatWithoutPrefix(String expression, Bean bean) {
		return BindUtil.toDisplay(CORE.getCustomer(), null, null, evaluateWithoutPrefix(expression, bean));
	}
}
