package org.skyve.impl.bind;

import java.util.Map;

import javax.el.ELProcessor;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

class ELExpressionEvaluator extends ExpressionEvaluator {
	static final String EL_PREFIX = "el";
	static final String RTEL_PREFIX = "rtel";
	
	private boolean typesafe = false;
	
	public ELExpressionEvaluator(boolean typesafe) {
		this.typesafe = typesafe;
	}

	@Override
	public Object evaluateWithoutPrefix(String expression, Bean bean) {
		ELProcessor elp = new ELProcessor();
		elp.getELManager().addELResolver(new BindingELResolver());
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
	public String validateWithoutPrefix(String expression,
											Class<?> returnType,
											Customer customer,
											Module module,
											Document document) {
		String result = null;

		if (typesafe) {
			try {
				ELProcessor elp = new ELProcessor();
				elp.getELManager().addELResolver(new ValidationELResolver(customer));
				// type-safe (el) starts with the document, if no document, no bean defined in the context
				if (document != null) {
					elp.defineBean("bean", document);
				}
				elp.defineBean("user", UserImpl.class);
				elp.defineBean("stash", Map.class);
				elp.defineBean(DATE_EXPRESSION, new DateOnly());
				elp.defineBean(TIME_EXPRESSION, new TimeOnly());
				elp.defineBean(DATETIME_EXPRESSION, new DateTime());
				elp.defineBean(TIMESTAMP_EXPRESSION, new Timestamp());
				Object evaluation = elp.eval(expression);
				if (returnType != null) {
					Class<?> type = null;
					if (evaluation instanceof DocumentImpl) {
						type = ((DocumentImpl) evaluation).getBeanClass(customer);
					}
					else if (evaluation instanceof Class<?>) {
						type = (Class<?>) evaluation;
					}
					else if (evaluation != null) {
						type = evaluation.getClass();
					}
					if ((type != null) && (! returnType.isAssignableFrom(type))) {
						result = expression + " returns an instance of type " + type +
								" that is incompatible with required return type of " + returnType;
					}
				}
			}
			catch (Exception e) {
				e.printStackTrace();
				result = e.getMessage();
				if (result == null) {
					result = expression + " is malformed and caused an exception " + e.getClass();
				}
			}
		}
		
		return result;
	}
}
