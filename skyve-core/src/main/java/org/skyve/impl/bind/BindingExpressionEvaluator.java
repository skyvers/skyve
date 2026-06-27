package org.skyve.impl.bind;

import org.skyve.CORE;
import org.skyve.domain.Bean;

/**
 * Resolves {@code {bean:...}} expressions using Skyve binding paths.
 *
 * <p>This evaluator delegates path traversal to {@link BindUtil} and formats results using
 * customer-aware display logic.
 */
public class BindingExpressionEvaluator extends MetaDataExpressionEvaluator {
	public static final String PREFIX = "bean";

	/**
	 * Creates a bean binding expression evaluator.
	 */
	public BindingExpressionEvaluator() {
		// default constructor
	}
	
	/**
	 * Evaluates a bean binding path and returns the raw value.
	 *
	 * @param expression the binding path without prefix/suffix
	 * @param bean the bean to evaluate against
	 * @return the resolved value, or {@code null} when {@code bean} is {@code null}
	 */
	@Override
	public Object evaluateWithoutPrefixOrSuffix(String expression, Bean bean) {
		if (bean == null) {
			return null;
		}
		return BindUtil.get(bean, expression);
	}

	/**
	 * Formats a bean binding path using customer-aware display conversion.
	 *
	 * @param expression the binding path without prefix/suffix
	 * @param bean the bean to evaluate against
	 * @return the formatted display value
	 * @throws IllegalArgumentException if {@code bean} is {@code null}
	 */
	@Override
	public String formatWithoutPrefixOrSuffix(String expression, Bean bean) {
		if (bean == null) {
			throw new IllegalArgumentException("bean cannot be null");
		}
		return BindUtil.getDisplay(CORE.getCustomer(), bean, expression);
	}
	
	/**
	 * Prefixes a bare binding with the owning binding path.
	 *
	 * @param expression the expression buffer to mutate
	 * @param binding the binding path to prepend
	 */
	@Override
	public void prefixBindingWithoutPrefixOrSuffix(StringBuilder expression, String binding) {
		expression.insert(0, '.');
		expression.insert(0, binding);
	}
}
