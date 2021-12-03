package org.skyve.impl.bind;

import java.util.Map;

import javax.el.ELProcessor;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

class ELExpressionEvaluator extends ExpressionEvaluator {
	static final String EL_PREFIX = "el";
	static final String BIZEL_PREFIX = "bizel";
	
	private boolean typeSafe = false;
	
	public ELExpressionEvaluator(boolean typeSafe) {
		this.typeSafe = typeSafe;
	}

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
	public String validateWithoutPrefix(String expression, Customer customer, Module module, Document document) {
		String result = null;
		try {
			ELProcessor elp = new ELProcessor();
			elp.getELManager().addELResolver(new ValidationELResolver(customer));
			// type-safe (bizel) start with the document, otherwise start with Object.class
			if (document != null) {
				elp.defineBean("bean", typeSafe ? document : Object.class);
			}
			else {
				elp.defineBean("bean", Object.class);
			}
			elp.defineBean("user", UserImpl.class);
			elp.defineBean("stash", Map.class);
			elp.defineBean(DATE_EXPRESSION, DateOnly.class);
			elp.defineBean(TIME_EXPRESSION, TimeOnly.class);
			elp.defineBean(DATETIME_EXPRESSION, DateTime.class);
			elp.defineBean(TIMESTAMP_EXPRESSION, Timestamp.class);
			elp.eval(expression);
		}
		catch (Exception e) {
			result = e.getMessage();
			if (result == null) {
				result = expression + " is malformed and caused an exception " + e.getClass();
			}
		}
		return result;
	}
}
