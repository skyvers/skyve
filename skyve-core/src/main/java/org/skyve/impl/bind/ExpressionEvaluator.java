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
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;

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
	public static final String URL_EXPRESSION = "URL";

	private static Map<String, ExpressionEvaluator> evaluators = new TreeMap<>();
	private static final ExpressionEvaluator DEFAULT_EVALUATOR = new BindingExpressionEvaluator();
	
	public static void register(String evaluatorPrefix, ExpressionEvaluator evaluator) {
		evaluators.put(evaluatorPrefix, evaluator);
	}
	
	static {
		evaluators.put(BindingExpressionEvaluator.PREFIX, DEFAULT_EVALUATOR);
		evaluators.put(ELExpressionEvaluator.BIZEL_PREFIX, new ELExpressionEvaluator(true));
		evaluators.put(ELExpressionEvaluator.EL_PREFIX, new ELExpressionEvaluator(false));
		evaluators.put(DisplayNameExpressionEvaluator.PREFIX, new DisplayNameExpressionEvaluator());
		evaluators.put(DescriptionExpressionEvaluator.PREFIX, new DescriptionExpressionEvaluator());
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

	/**
	 * Validate an expression
	 * @param expression
	 * @return	null if valid or the error message if not.
	 */
	public static String validate(String expression) {
		return validate(expression, null, null, null);
	}

	/**
	 * Validate an expression.
	 * @param expression
	 * @param customer
	 * @param module
	 * @param document
	 * @return	null if valid or the error message if not.
	 */
	public static String validate(String expression, Customer customer, Module module, Document document) {
		int colonIndex = expression.indexOf(':');
		if (colonIndex < 0) {
			String expressionWithoutPrefix = expression.trim();

			if (USER_EXPRESSION.equals(expressionWithoutPrefix) ||
					USERID_EXPRESSION.equals(expressionWithoutPrefix) ||
					USERNAME_EXPRESSION.equals(expressionWithoutPrefix) ||
					DATAGROUPID_EXPRESSION.equals(expressionWithoutPrefix) ||
					CONTACTID_EXPRESSION.equals(expressionWithoutPrefix) ||
					CUSTOMER_EXPRESSION.equals(expressionWithoutPrefix) ||
					DATE_EXPRESSION.equals(expressionWithoutPrefix) ||
					TIME_EXPRESSION.equals(expressionWithoutPrefix) ||
					DATETIME_EXPRESSION.equals(expressionWithoutPrefix) ||
					TIMESTAMP_EXPRESSION.equals(expressionWithoutPrefix) ||
					URL_EXPRESSION.equals(expressionWithoutPrefix)) {
				return null; // this is valid
			}

			return DEFAULT_EVALUATOR.validateWithoutPrefix(expressionWithoutPrefix, customer, module, document);
		}
		
		String prefix = expression.substring(0, colonIndex).trim();
		String expressionWithoutPrefix = expression.substring(colonIndex + 1).trim();

		ExpressionEvaluator eval = evaluators.get(prefix);
		if (eval == null) {
			throw new DomainException("Cannot find an expression evaluator for prefix " + prefix);
		}
		
		return eval.validateWithoutPrefix(expressionWithoutPrefix, customer, module, document);
	}
	
	private static Object process(String expression, Bean bean, boolean format) {
		int colonIndex = expression.indexOf(':');
		if (colonIndex < 0) {
			String expressionWithoutPrefix = expression.trim();

			if (USER_EXPRESSION.equals(expressionWithoutPrefix)) {
				String result = CORE.getUser().getName();
				if (format) {
					return (result == null) ? "" : result;
				}
				return result;
			}
			if (USERID_EXPRESSION.equals(expressionWithoutPrefix)) {
				String result = CORE.getUser().getId();
				if (format) {
					return (result == null) ? "" : result;
				}
				return result;
			}
			if (USERNAME_EXPRESSION.equals(expressionWithoutPrefix)) {
				String result = CORE.getUser().getContactName();
				if (format) {
					return (result == null) ? "" : result;
				}
				return result;
			}
			if (DATAGROUPID_EXPRESSION.equals(expressionWithoutPrefix)) {
				String result = CORE.getUser().getDataGroupId();
				if (format) {
					return (result == null) ? "" : result;
				}
				return result;
			}
			if (CONTACTID_EXPRESSION.equals(expressionWithoutPrefix)) {
				String result = CORE.getUser().getContactId();
				if (format) {
					return (result == null) ? "" : result;
				}
				return result;
			}
			if (CUSTOMER_EXPRESSION.equals(expressionWithoutPrefix)) {
				String result = CORE.getCustomer().getName();
				if (format) {
					return (result == null) ? "" : result;
				}
				return result;
			}
			if (DATE_EXPRESSION.equals(expressionWithoutPrefix)) {
				DateOnly result = new DateOnly();
				if (format) {
					return BindUtil.toDisplay(CORE.getCustomer(), null, null, result);
				}
				return result;
			}
			if (TIME_EXPRESSION.equals(expressionWithoutPrefix)) {
				TimeOnly result = new TimeOnly();
				if (format) {
					return BindUtil.toDisplay(CORE.getCustomer(), null, null, result);
				}
				return result;
			}
			if (DATETIME_EXPRESSION.equals(expressionWithoutPrefix)) {
				DateTime result = new DateTime();
				if (format) {
					return BindUtil.toDisplay(CORE.getCustomer(), null, null, result);
				}
				return result;
			}
			if (TIMESTAMP_EXPRESSION.equals(expressionWithoutPrefix)) {
				Timestamp result = new Timestamp();
				if (format) {
					return BindUtil.toDisplay(CORE.getCustomer(), null, null, result);
				}
				return result;
			}
			if (URL_EXPRESSION.equals(expressionWithoutPrefix)) {
				if (bean == null) {
					return format ? "" : null;
				}
				return Util.getDocumentUrl(bean);
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
	public abstract String validateWithoutPrefix(String expression, Customer customer, Module module, Document document);
}
