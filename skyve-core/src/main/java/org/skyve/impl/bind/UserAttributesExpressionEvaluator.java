package org.skyve.impl.bind;

import java.util.Collections;
import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.ExpressionEvaluator;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Resolves {@code {user:...}} expressions against the current user's attribute map.
 */
public class UserAttributesExpressionEvaluator extends ExpressionEvaluator {
	public static final String PREFIX = "user";

	/**
	 * Creates a user-attributes expression evaluator.
	 */
	public UserAttributesExpressionEvaluator() {
		// default constructor
	}
	
	/**
	 * Resolves a value from the current user's attribute map.
	 *
	 * @param expression the user attribute key without prefix/suffix
	 * @param bean ignored for user expressions
	 * @return the attribute value, or {@code null} when absent
	 */
	@Override
	public @Nullable Object evaluateWithoutPrefixOrSuffix(@Nonnull String expression, @Nullable Bean bean) {
		return CORE.getUser().getAttributes().get(expression);
	}

	/**
	 * Formats a user attribute value for display.
	 *
	 * @param expression the user attribute key without prefix/suffix
	 * @param bean ignored for user expressions
	 * @return the display representation of the resolved value
	 */
	@Override
	public @Nonnull String formatWithoutPrefixOrSuffix(@Nonnull String expression, @Nullable Bean bean) {
		return BindUtil.toDisplay(CORE.getCustomer(), evaluateWithoutPrefixOrSuffix(expression, bean));
	}
	
	/**
	 * Validates a user attribute expression.
	 *
	 * @param expression the user attribute key without prefix/suffix
	 * @param returnType the expected return type
	 * @param customer the customer context
	 * @param module the module context
	 * @param document the document context
	 * @return always {@code null} because any key is syntactically valid
	 */
	@Override
	public @Nullable String validateWithoutPrefixOrSuffix(@Nonnull String expression,
																@Nullable Class<?> returnType,
																@Nullable Customer customer,
																@Nullable Module module,
																@Nullable Document document) {
		return null; // any key is valid
	}
	
	/**
	 * Completes user attribute expressions.
	 *
	 * @param fragment the partial expression fragment
	 * @param customer the customer context
	 * @param module the module context
	 * @param document the document context
	 * @return an empty list because attributes are dynamic at runtime
	 */
	@Override
	public @Nonnull List<String> completeWithoutPrefixOrSuffix(@Nullable String fragment,
																@Nonnull Customer customer, 
																@Nonnull Module module,
																@Nonnull Document document) {
		return Collections.emptyList(); // any key is valid
	}
	
	/**
	 * Leaves user expressions unchanged because they are key-based.
	 *
	 * @param expression the expression buffer
	 * @param binding ignored for user expressions
	 */
	@Override
	public void prefixBindingWithoutPrefixOrSuffix(@Nonnull StringBuilder expression, @Nonnull String binding) {
		// nothing to do here as user uses keys
	}
}
