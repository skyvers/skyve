package org.skyve.impl.bind;

import org.skyve.domain.Bean;
import org.skyve.metadata.model.Attribute;

/**
 * Resolves {@code {disp:...}} expressions to an attribute's localised display name.
 */
public class DisplayNameExpressionEvaluator extends MetaDataExpressionEvaluator {
	public static final String PREFIX = "disp";

	/**
	 * Creates an attribute display-name expression evaluator.
	 */
	public DisplayNameExpressionEvaluator() {
		// default constructor
	}
	
	/**
	 * Evaluates an attribute expression and returns its localised display name.
	 *
	 * @param expression the attribute expression without prefix/suffix
	 * @param bean the bean providing metadata context
	 * @return the display name, or {@code null} when the attribute cannot be resolved
	 */
	@Override
	public Object evaluateWithoutPrefixOrSuffix(String expression, Bean bean) {
		Attribute a = obtainAttribute(expression, bean);
		return (a == null) ? null : a.getLocalisedDisplayName();
	}

	/**
	 * Formats an attribute display-name expression.
	 *
	 * @param expression the attribute expression without prefix/suffix
	 * @param bean the bean providing metadata context
	 * @return the display-name text, or an empty string when unresolved
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
