package org.skyve.impl.bind;

import java.util.Collections;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.ExpressionEvaluator;
import org.skyve.util.Util;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Resolves {@code {i18n:...}} expressions using configured message bundles.
 *
 * <p>Any key is considered syntactically valid; unknown keys are handled by
 * {@link Util#nullSafeI18n(String)}.
 */
public class I18NExpressionEvaluator extends ExpressionEvaluator {
	public static final String PREFIX = "i18n";

	/**
	 * Creates an i18n expression evaluator.
	 */
	public I18NExpressionEvaluator() {
		// default constructor
	}
	
	/**
	 * Evaluates an i18n key expression.
	 *
	 * @param expression the i18n key without prefix/suffix
	 * @param bean ignored for i18n expressions
	 * @return the resolved i18n value
	 */
	@Override
	public @Nonnull Object evaluateWithoutPrefixOrSuffix(@Nonnull String expression, @Nullable Bean bean) {
		return formatWithoutPrefixOrSuffix(expression, bean);
	}

	/**
	 * Formats an i18n key expression.
	 *
	 * @param expression the i18n key without prefix/suffix
	 * @param bean ignored for i18n expressions
	 * @return the resolved i18n value, or fallback text when undefined
	 */
	@Override
	public @Nonnull String formatWithoutPrefixOrSuffix(@Nonnull String expression, @Nullable Bean bean) {
		return Util.nullSafeI18n(expression);
	}
	
	/**
	 * Validates an i18n expression.
	 *
	 * @param expression the i18n key without prefix/suffix
	 * @param returnType the expected return type
	 * @param customer the customer context
	 * @param module the module context
	 * @param document the document context
	 * @return always {@code null} because any key is valid
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
	 * Completes i18n expressions.
	 *
	 * @param fragment the partial expression fragment
	 * @param customer the customer context
	 * @param module the module context
	 * @param document the document context
	 * @return an empty list because keys are not enumerated
	 */
	@Override
	public @Nonnull List<String> completeWithoutPrefixOrSuffix(@Nullable String fragment,
														@Nonnull Customer customer, 
														@Nonnull Module module,
														@Nonnull Document document) {
		return Collections.emptyList(); // any key is valid
	}

	/**
	 * Leaves i18n expressions unchanged because they are key-based.
	 *
	 * @param expression the expression buffer
	 * @param binding ignored for i18n expressions
	 */
	@Override
	public void prefixBindingWithoutPrefixOrSuffix(@Nonnull StringBuilder expression, @Nonnull String binding) {
		// nothing to do here as i18n uses keys
	}
}
