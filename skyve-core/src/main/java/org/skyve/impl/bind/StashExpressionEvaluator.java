package org.skyve.impl.bind;

import java.util.Collections;
import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.ExpressionEvaluator;

/**
 * Resolves {@code {stash:...}} expressions against the current request stash.
 */
public class StashExpressionEvaluator extends ExpressionEvaluator {
	public static final String PREFIX = "stash";

	/**
	 * Creates a stash expression evaluator.
	 */
	public StashExpressionEvaluator() {
		// default constructor
	}
	
	/**
	 * Resolves a stash key from the current request stash.
	 *
	 * @param expression the stash key without prefix/suffix
	 * @param bean ignored for stash expressions
	 * @return the stash value, or {@code null} when the key is absent
	 */
	@Override
	public Object evaluateWithoutPrefixOrSuffix(String expression, Bean bean) {
		return CORE.getStash().get(expression);
	}

	/**
	 * Formats a stash value for display.
	 *
	 * @param expression the stash key without prefix/suffix
	 * @param bean ignored for stash expressions
	 * @return the display representation of the stash value
	 */
	@Override
	public String formatWithoutPrefixOrSuffix(String expression, Bean bean) {
		return BindUtil.toDisplay(CORE.getCustomer(), evaluateWithoutPrefixOrSuffix(expression, bean));
	}
	
	/**
	 * Validates a stash expression.
	 *
	 * @param expression the stash key without prefix/suffix
	 * @param returnType the expected return type
	 * @param customer the customer context
	 * @param module the module context
	 * @param document the document context
	 * @return always {@code null} because any stash key is syntactically valid
	 */
	@Override
	public String validateWithoutPrefixOrSuffix(String expression,
													Class<?> returnType,
													Customer customer,
													Module module,
													Document document) {
		return null; // any key is valid
	}
	
	/**
	 * Completes stash expressions.
	 *
	 * @param fragment the partial expression fragment
	 * @param customer the customer context
	 * @param module the module context
	 * @param document the document context
	 * @return an empty list because stash keys are runtime-defined
	 */
	@Override
	public List<String> completeWithoutPrefixOrSuffix(String fragment,
														Customer customer, 
														Module module,
														Document document) {
		return Collections.emptyList(); // any key is valid
	}

	/**
	 * Leaves stash expressions unchanged because they are key-based.
	 *
	 * @param expression the expression buffer
	 * @param binding ignored for stash expressions
	 */
	@Override
	public void prefixBindingWithoutPrefixOrSuffix(StringBuilder expression, String binding) {
		// nothing to do here as stash uses keys
	}
}
