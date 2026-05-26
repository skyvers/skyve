package org.skyve.impl.bind;

import org.skyve.domain.Bean;
import org.skyve.metadata.model.Attribute;

/**
 * Resolves {@code {desc:...}} expressions to an attribute's localised description.
 */
public class DescriptionExpressionEvaluator extends MetaDataExpressionEvaluator {
	public static final String PREFIX = "desc";

	/**
	 * Creates an attribute description expression evaluator.
	 */
	public DescriptionExpressionEvaluator() {
		// default constructor
	}
	
	/**
	 * Evaluates an attribute expression and returns its localised description.
	 *
	 * @param expression the attribute expression without prefix/suffix
	 * @param bean the bean providing metadata context
	 * @return the attribute description, or {@code null} when the attribute cannot be resolved
	 */
	@Override
	public Object evaluateWithoutPrefixOrSuffix(String expression, Bean bean) {
		Attribute a = obtainAttribute(expression, bean);
		return (a == null) ? null : a.getLocalisedDescription();
	}

	/**
	 * Formats an attribute description expression.
	 *
	 * @param expression the attribute expression without prefix/suffix
	 * @param bean the bean providing metadata context
	 * @return the description text, or an empty string when unresolved
	 */
	@Override
	public String formatWithoutPrefixOrSuffix(String expression, Bean bean) {
		Object result = evaluateWithoutPrefixOrSuffix(expression, bean);
		return (result == null) ? "" : result.toString();
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
