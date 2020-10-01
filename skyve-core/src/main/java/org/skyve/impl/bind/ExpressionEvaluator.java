package org.skyve.impl.bind;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;

public abstract class ExpressionEvaluator {
	public static final String USER_EXPRESSION = "USER";
	public static final String USERID_EXPRESSION = "USERID";
	public static final String USERNAME_EXPRESSION = "USERNAME";
	public static final String DATAGROUPID_EXPRESSION = "DATAGROUPID";
	public static final String CONTACTID_EXPRESSION = "CONTACTID";
	public static final String CUSTOMER_EXPRESSION = "CUSTOMER";
	public static final String DATE_EXPRESSION = "DATE";
	public static final String TIME_EXPRESSION = "TIME";
	public static final String DATETIME_EXPRESSION = "DATETIME";
	public static final String TIMESTAMP_EXPRESSION = "TIMESTAMP";
	

	private static Map<String, ExpressionEvaluator> evaluators = new TreeMap<>();
	private static final ExpressionEvaluator DEFAULT_EVALUATOR = new BindingExpressionEvaluator();
	
	public static void register(String evaluatorPrefix, ExpressionEvaluator evaluator) {
		evaluators.put(evaluatorPrefix, evaluator);
	}
	
	static {
		evaluators.put(BindingExpressionEvaluator.PREFIX, DEFAULT_EVALUATOR);
		evaluators.put(ELExpressionEvaluator.PREFIX, new ELExpressionEvaluator());
		evaluators.put(I18NExpressionEvaluator.PREFIX, new I18NExpressionEvaluator());
		evaluators.put(RoleExpressionEvaluator.PREFIX, new RoleExpressionEvaluator());
		evaluators.put(StashExpressionEvaluator.PREFIX, new StashExpressionEvaluator());
		evaluators.put(UserAttributesExpressionEvaluator.PREFIX, new UserAttributesExpressionEvaluator());
	}
	
	public static String format(String expression) {
		return format(expression, null);
	}
	
	public static String format(String expression, Bean bean) {
		return (String) process(expression, bean, true);
	}
	
	public static Object evaluate(String expression) {
		return evaluate(expression, null);
	}
	
	public static Object evaluate(String expression, Bean bean) {
		return process(expression, bean, false);
	}

	private static Object process(String expression, Bean bean, boolean format) {
		int colonIndex = expression.indexOf(':');
		if (colonIndex < 0) {
			String expressionWithoutPrefix = expression.trim();

			if (USER_EXPRESSION.equals(expression)) {
				String result = CORE.getUser().getName();
				if (format) {
					return (result == null) ? "" : result;
				}
				return result;
			}
			if (USERID_EXPRESSION.equals(expression)) {
				String result = CORE.getUser().getId();
				if (format) {
					return (result == null) ? "" : result;
				}
				return result;
			}
			if (USERNAME_EXPRESSION.equals(expression)) {
				String result = CORE.getUser().getContactName();
				if (format) {
					return (result == null) ? "" : result;
				}
				return result;
			}
			if (DATAGROUPID_EXPRESSION.equals(expression)) {
				String result = CORE.getUser().getDataGroupId();
				if (format) {
					return (result == null) ? "" : result;
				}
				return result;
			}
			if (CONTACTID_EXPRESSION.equals(expression)) {
				String result = CORE.getUser().getContactId();
				if (format) {
					return (result == null) ? "" : result;
				}
				return result;
			}
			if (CUSTOMER_EXPRESSION.equals(expression)) {
				String result = CORE.getCustomer().getName();
				if (format) {
					return (result == null) ? "" : result;
				}
				return result;
			}
			if (DATE_EXPRESSION.equals(expression)) {
				DateOnly result = new DateOnly();
				if (format) {
					return BindUtil.toDisplay(CORE.getCustomer(), null, null, result);
				}
				return result;
			}
			if (TIME_EXPRESSION.equals(expression)) {
				TimeOnly result = new TimeOnly();
				if (format) {
					return BindUtil.toDisplay(CORE.getCustomer(), null, null, result);
				}
				return result;
			}
			if (DATETIME_EXPRESSION.equals(expression)) {
				DateTime result = new DateTime();
				if (format) {
					return BindUtil.toDisplay(CORE.getCustomer(), null, null, result);
				}
				return result;
			}
			if (TIMESTAMP_EXPRESSION.equals(expression)) {
				Timestamp result = new Timestamp();
				if (format) {
					return BindUtil.toDisplay(CORE.getCustomer(), null, null, result);
				}
				return result;
			}

			return format ? 
						DEFAULT_EVALUATOR.formatWithoutPrefix(expressionWithoutPrefix, bean) :
							DEFAULT_EVALUATOR.evaluateWithoutPrefix(expressionWithoutPrefix, bean);
		}
		
		String prefix = expression.substring(0, colonIndex).trim();
		String expressionWithoutPrefix = expression.substring(colonIndex + 1).trim();

		ExpressionEvaluator eval = evaluators.get(prefix);
		if (eval == null) {
			throw new DomainException("Cannot find an expression evaluator for prefix " + prefix);
		}
		
		return format ? eval.formatWithoutPrefix(expressionWithoutPrefix, bean) : eval.evaluateWithoutPrefix(expressionWithoutPrefix, bean);
	}
		
	public abstract String formatWithoutPrefix(String expression, Bean bean);
	public abstract Object evaluateWithoutPrefix(String expression, Bean bean);
}
