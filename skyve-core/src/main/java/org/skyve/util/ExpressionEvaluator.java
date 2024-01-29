package org.skyve.util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.domain.types.formatters.Formatter;
import org.skyve.domain.types.formatters.Formatters;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.bind.BindingExpressionEvaluator;
import org.skyve.impl.bind.DescriptionExpressionEvaluator;
import org.skyve.impl.bind.DisplayNameExpressionEvaluator;
import org.skyve.impl.bind.ELExpressionEvaluator;
import org.skyve.impl.bind.I18NExpressionEvaluator;
import org.skyve.impl.bind.RoleExpressionEvaluator;
import org.skyve.impl.bind.StashExpressionEvaluator;
import org.skyve.impl.bind.UserAttributesExpressionEvaluator;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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

	// Map of expression evaluator implementations keyed by their prefix
	private static final Map<String, ExpressionEvaluator> EVALUATORS = new TreeMap<>();
	// Default evaluator to use when there is no prefix or when validating a binding
	private static final ExpressionEvaluator DEFAULT_EVALUATOR = new BindingExpressionEvaluator();
	
	/**
	 * Register an expression evaluator with a prefix.
	 * A prefix can only be registered once.
	 * Extra evaluators can be registered in Customisations.registerCustomExpressions() which guarantees thread safety.
	 * This method is not thread safe.
	 * @param evaluatorPrefix	The prefix to use in an expression
	 * @param evaluator	The expression evaluator to invoke
	 */
	public static void register(@Nonnull String evaluatorPrefix, @Nonnull ExpressionEvaluator evaluator) {
		if (EVALUATORS.put(evaluatorPrefix, evaluator) != null) {
			throw new IllegalStateException("ExpressionEvaluator prefix " + evaluatorPrefix + " is already registered and cannot be registered again.");
		}
	}
	
	// Standard Skyve evaluators
	static {
		EVALUATORS.put(BindingExpressionEvaluator.PREFIX, DEFAULT_EVALUATOR);
		EVALUATORS.put(ELExpressionEvaluator.EL_PREFIX, new ELExpressionEvaluator(true));
		EVALUATORS.put(DisplayNameExpressionEvaluator.PREFIX, new DisplayNameExpressionEvaluator());
		EVALUATORS.put(DescriptionExpressionEvaluator.PREFIX, new DescriptionExpressionEvaluator());
		EVALUATORS.put(I18NExpressionEvaluator.PREFIX, new I18NExpressionEvaluator());
		EVALUATORS.put(RoleExpressionEvaluator.PREFIX, new RoleExpressionEvaluator());
		EVALUATORS.put(StashExpressionEvaluator.PREFIX, new StashExpressionEvaluator());
		EVALUATORS.put(UserAttributesExpressionEvaluator.PREFIX, new UserAttributesExpressionEvaluator());
		EVALUATORS.put(ELExpressionEvaluator.RTEL_PREFIX, new ELExpressionEvaluator(false));
	}

	/**
	 * Formats a Skyve expression to a String based on normal Skyve formatting precedence - (formatter suffix, attribute converters, customer converters etc) 
	 * @param expression	The expression to format optionally including the prefix and/or suffix.
	 * @return	The evaluated formatted expression.
	 */
	public static @Nonnull String format(@Nonnull String expression) {
		return format(expression, null);
	}
	
	/**
	 * Formats a Skyve expression to a String using the given bean as context and based on normal Skyve formatting precedence - (formatter suffix, attribute converters, customer converters etc) 
	 * @param expression	The expression to format optionally including the prefix and/or suffix.
	 * @param bean	The bean to use for context in expression evaluation.
	 * @return	The evaluated formatted expression.
	 */
	public static @Nonnull String format(@Nonnull String expression, @Nullable Bean bean) {
		return (String) process(expression, bean, true);
	}
	
	/**
	 * Evaluates a Skyve expression. 
	 * @param expression	The expression to format optionally including the prefix and/or suffix.
	 * @return	The evaluated expression.
	 */
	public static @Nullable Object evaluate(@Nonnull String expression) {
		return evaluate(expression, null);
	}
	
	/**
	 * Evaluates a Skyve expression. 
	 * @param expression	The expression to format optionally including the prefix and/or suffix.
	 * @param bean	The bean to use for context in expression evaluation.
	 * @return	The evaluated expression.
	 */
	public static @Nullable Object evaluate(@Nonnull String expression, @Nullable Bean bean) {
		return process(expression, bean, false);
	}

	/**
	 * Validate a Skyve expression
	 * @param expression	The expression to format optionally including the prefix and/or suffix.
	 * @return	null if valid or the error message if not.
	 */
	public static @Nullable String validate(@Nonnull String expression) {
		return validate(expression, null, null, null, null);
	}

	/**
	 * Validate a Skyve expression
	 * @param expression	The expression to format optionally including the prefix and/or suffix.
	 * @param returnType A return type to assert - the expression evaluation must be assignable to the return type.
	 * @return	null if valid or the error message if not.
	 */
	public static @Nullable String validate(@Nonnull String expression, @Nullable Class<?> returnType) {
		return validate(expression, returnType, null, null, null);
	}

	/**
	 * Validate a Skyve expression.
	 * @param expression	The expression to format optionally including the prefix and/or suffix.
	 * @param customer	The customer to validate for.
	 * @param module	The module to validate for.
	 * @param document	The document to validate for.
	 * @return	null if valid or the error message if not.
	 */
	public static @Nullable String validate(@Nonnull String expression,
												@Nullable Customer customer,
												@Nullable Module module,
												@Nullable Document document) {
		return validate(expression, null, customer, module, document);
	}
	
	/**
	 * Validate a Skyve expression.
	 * @param expression	The expression to format optionally including the prefix and/or suffix.
	 * @param returnType A return type to assert - the expression evaluation must be assignable to the return type.
	 * @param customer	The customer to validate for.
	 * @param module	The module to validate for.
	 * @param document	The document to validate for.
	 * @return	null if valid or the error message if not.
	 */
	public static @Nullable String validate(@Nonnull String expression,
												@Nullable Class<?> returnType,
												@Nullable Customer customer,
												@Nullable Module module,
												@Nullable Document document) {
		String expressionWithoutSuffix = expression; // curly braces
		String formatName = null; // The format name in the format suffix
		Formatter<?> formatter = null; // The formatter in the format suffix
		
		// Look for a format suffix and extract the format name
		int pipeIndex = expression.lastIndexOf('|');
		if (pipeIndex > -1) { // found a pipe format suffix
			String formatSuffix = expression.substring(pipeIndex, expression.length() - 1); // assume '}' on end
			expressionWithoutSuffix = expression.replace(formatSuffix, "");
			formatName = UtilImpl.processStringValue(formatSuffix.substring(1)); //remove '|' at beginning
			if (formatName == null) {
				return "Formatter expected after '|' in expression " + expression;
			}
			formatter = Formatters.get(formatName);
			if (formatter == null) {
				return "Formatter " + formatName + " does not exist";
			}
			if ((returnType != null) && (! returnType.isAssignableFrom(String.class))) {
				return "Formatter " + formatName + " evaluates to a String, not " + returnType;
			}
		}
		
		String result = null; // null if valid or an error message
		Class<?> testType = returnType;	// The type to test against

		int colonIndex = expressionWithoutSuffix.indexOf(':');
		if (colonIndex < 0) { // no prefix
			String expressionWithoutPrefixOrSuffix = expressionWithoutSuffix.substring(1, expressionWithoutSuffix.length() - 1).trim(); // no curly braces
			if (expressionWithoutPrefixOrSuffix.isEmpty()) {
				return "Nothing to evaluate in expression " + expression;
			}
			
			// String implicit expressions
			if (USER_EXPRESSION.equals(expressionWithoutPrefixOrSuffix) ||
					USERID_EXPRESSION.equals(expressionWithoutPrefixOrSuffix) ||
					USERNAME_EXPRESSION.equals(expressionWithoutPrefixOrSuffix) ||
					DATAGROUPID_EXPRESSION.equals(expressionWithoutPrefixOrSuffix) ||
					CONTACTID_EXPRESSION.equals(expressionWithoutPrefixOrSuffix) ||
					CUSTOMER_EXPRESSION.equals(expressionWithoutPrefixOrSuffix) ||
					URL_EXPRESSION.equals(expressionWithoutPrefixOrSuffix)) {
				if (testType == null) {
					testType = String.class;
				}
			}
			// Temporal implicit expressions
			else if (DATE_EXPRESSION.equals(expressionWithoutPrefixOrSuffix) ||
					TIME_EXPRESSION.equals(expressionWithoutPrefixOrSuffix) ||
					DATETIME_EXPRESSION.equals(expressionWithoutPrefixOrSuffix) ||
					TIMESTAMP_EXPRESSION.equals(expressionWithoutPrefixOrSuffix)) {
				if (testType == null) {
					testType = Date.class;
				}
			}
			// Binding expression
			else {
				result = DEFAULT_EVALUATOR.validateWithoutPrefixOrSuffix(expressionWithoutPrefixOrSuffix,
																			// Ensure the expression matches the formatter type (if defined) or otherwise the returnType to assert
																			(formatter == null) ? returnType : formatter.getValueType(),
																			customer,
																			module,
																			document);
			}
		}
		else {
			// Get the prefix
			String prefix = expressionWithoutSuffix.substring(1, colonIndex).trim();
			String expressionWithoutPrefixOrSuffix = expressionWithoutSuffix.substring(colonIndex + 1, expressionWithoutSuffix.length() - 1).trim(); // no curly braces
			if (expressionWithoutPrefixOrSuffix.isEmpty()) {
				return "Nothing to evaluate in expression " + expression;
			}
	
			// Select the evaluator
			ExpressionEvaluator eval = EVALUATORS.get(prefix);
			if (eval == null) {
				result = "Cannot find an expression evaluator for prefix " + prefix;
			}
			else {
				result = eval.validateWithoutPrefixOrSuffix(expressionWithoutPrefixOrSuffix,
																// Ensure the expression matches the formatter type (if defined) or otherwise the returnType to assert
																(formatter == null) ? returnType : formatter.getValueType(),
																customer,
																module,
																document);
			}
		}
		
		if ((formatter != null) && (result == null) && (testType != null)) { // valid expression but we still need to validate the format
			if (! formatter.getValueType().isAssignableFrom(testType)) {
				result = "Formatter " + formatName + " for type " + formatter.getValueType() + 
							" is incompatible with expression " +
							expressionWithoutSuffix + " of type " + testType;
			}
		}
		
		return result;
	}
	
	/**
	 * Add the binding prefix onto any binding expressions in the bean expression.
	 * @param expression	The bean expression.
	 * @param binding	The binding to prefix with.
	 * @return	The prefixed expression.
	 */
	public static @Nonnull String prefixBinding(@Nonnull String expression, @Nonnull String binding) {
		String expressionWithoutSuffix = expression; // includes curly braces
		String formatName = null; // The formatter in the format suffix

		// Look for a format suffix and extract the format name
		int pipeIndex = expression.lastIndexOf('|');
		if (pipeIndex > -1) { // found a pipe format suffix
			String formatSuffix = expression.substring(pipeIndex, expression.length() - 1); // assume '}' on end
			if (! formatSuffix.isEmpty()) {
				expressionWithoutSuffix = expression.replace(formatSuffix, "");
				formatName = UtilImpl.processStringValue(formatSuffix.substring(1)); //remove '|' at beginning
			}
		}

		int colonIndex = expressionWithoutSuffix.indexOf(':');
		if (colonIndex < 0) {
			String expressionWithoutPrefixOrSuffix = expressionWithoutSuffix.substring(1, expression.length() - 1).trim(); // no curly braces

			if (USER_EXPRESSION.equals(expressionWithoutPrefixOrSuffix) ||
					USERID_EXPRESSION.equals(expressionWithoutPrefixOrSuffix) ||
					USERNAME_EXPRESSION.equals(expressionWithoutPrefixOrSuffix) ||
					DATAGROUPID_EXPRESSION.equals(expressionWithoutPrefixOrSuffix) ||
					CONTACTID_EXPRESSION.equals(expressionWithoutPrefixOrSuffix) ||
					CUSTOMER_EXPRESSION.equals(expressionWithoutPrefixOrSuffix) ||
					DATE_EXPRESSION.equals(expressionWithoutPrefixOrSuffix) ||
					TIME_EXPRESSION.equals(expressionWithoutPrefixOrSuffix) ||
					DATETIME_EXPRESSION.equals(expressionWithoutPrefixOrSuffix) ||
					TIMESTAMP_EXPRESSION.equals(expressionWithoutPrefixOrSuffix) ||
					URL_EXPRESSION.equals(expressionWithoutPrefixOrSuffix)) {
				return expression; // no change for these implicit expressions
			}

			StringBuilder result = new StringBuilder(expressionWithoutPrefixOrSuffix);
			DEFAULT_EVALUATOR.prefixBindingWithoutPrefixOrSuffix(result, binding);
			result.insert(0, '{');
			if (formatName != null) {
				result.append('|').append(formatName);
			}
			result.append('}');
			return result.toString();
		}
		
		String prefix = expressionWithoutSuffix.substring(1, colonIndex).trim();
		String expressionWithoutPrefixOfSuffix = expression.substring(colonIndex + 1, expression.length() - 1).trim(); // no curly braces

		ExpressionEvaluator eval = EVALUATORS.get(prefix);
		if (eval == null) {
			throw new DomainException("Cannot find an expression evaluator for prefix " + prefix);
		}
		
		StringBuilder result = new StringBuilder(expressionWithoutPrefixOfSuffix);
		eval.prefixBindingWithoutPrefixOrSuffix(result, binding);
		result.insert(0, ':').insert(0, prefix).insert(0, '{');
		if (formatName != null) {
			result.append('|').append(formatName);
		}
		result.append('}');
		return result.toString();
	}

	/**
	 * Validate a binding expression.
	 * @param binding	The binding to validate.
	 * @param customer	The customer to validate for.
	 * @param module	The module to validate for.
	 * @param document	The document to validate for.
	 * @return	null if valid or the error message if not.
	 */
	public static @Nullable String validateBinding(@Nonnull String binding,
													@Nullable Customer customer,
													@Nullable Module module,
													@Nullable Document document) {
		return validateBinding(binding, null, customer, module, document);
	}

	/**
	 * Validate a binding expression.
	 * @param binding	The binding to validate.
	 * @param returnType A return type to assert - the binding evaluation must be assignable to the return type.
	 * @param customer	The customer to validate for.
	 * @param module	The module to validate for.
	 * @param document	The document to validate for.
	 * @return	null if valid or the error message if not.
	 */
	public static @Nullable String validateBinding(@Nonnull String binding,
													@Nullable Class<?> returnType,
													@Nullable Customer customer,
													@Nullable Module module,
													@Nullable Document document) {
		return DEFAULT_EVALUATOR.validateWithoutPrefixOrSuffix(binding, returnType, customer, module, document);
	}
	
	/**
	 * Determine a list of bindings possible given the fragment.
	 * @param fragment	A partial expression to complete.
	 * @param customer	The customer to complete for.
	 * @param module	The module to complete for.
	 * @param document	The document to complete for.
	 * @return	A list of valid completions.
	 */
	public static @Nonnull List<String> completeBinding(@Nullable String fragment,
															@Nonnull Customer customer,
															@Nonnull Module module,
															@Nonnull Document document) {
		List<String> result = DEFAULT_EVALUATOR.completeWithoutPrefixOrSuffix(fragment, customer, module, document);
		if (result == null) {
			throw new IllegalStateException("Complete Binding of " + fragment + " yields null");
		}
		return result;
	}
	
	/**
	 * Determine a list of expressions possible given the fragment.
	 * @param fragment	A partial expression to complete.
	 * @param customer	The customer to complete for.
	 * @param module	The module to complete for.
	 * @param document	The document to complete for.
	 * @return	A list of valid completions.
	 */
	public static @Nonnull List<String> completeExpression(@Nullable String fragment,
															@Nonnull Customer customer,
															@Nonnull Module module,
															@Nonnull Document document) {
		List<String> result = null;
		
		if (fragment != null) {
			int openCurlyBraceIndex = fragment.indexOf('{');
			int colonIndex = -1;
			while (openCurlyBraceIndex >= 0) {
				if ((openCurlyBraceIndex == 0) || // first char is '{' 
						// '{' is present and not escaped with a preceding '\' - ie \{ is escaped
						((openCurlyBraceIndex > 0) && (fragment.charAt(openCurlyBraceIndex - 1) != '\\'))) {
	
					int closedCurlyBraceIndex = fragment.indexOf("}", openCurlyBraceIndex);
					// unfinished expression here
					if (closedCurlyBraceIndex < 0) {
						int pipeIndex = fragment.lastIndexOf('|');
						// unfinished formats here
						if (pipeIndex >= 0) {
							String formatFragment = (fragment.length() > (pipeIndex + 1)) ?
														fragment.substring(pipeIndex + 1, fragment.length()).trim() :
														"";
							result = new ArrayList<>();
							
							// Check formatters
							for (String name : Formatters.getNames()) {
								if (name.startsWith(formatFragment)) {
									result.add(name);
								}
							}
							break;
						}
						
						// unfinished expressions here
						colonIndex = fragment.indexOf(':', openCurlyBraceIndex);
						String fragmentWithoutPrefix = null;
						if (colonIndex < 0) {
							fragmentWithoutPrefix = (fragment.length() > (openCurlyBraceIndex + 1)) ?
														fragment.substring(openCurlyBraceIndex + 1, fragment.length()).trim() :
														"";
	
							result = new ArrayList<>();
							
							// Check expression prefixes
							for (String prefix : EVALUATORS.keySet()) {
								if (prefix.startsWith(fragmentWithoutPrefix)) {
									result.add(prefix);
								}
							}
							
							// Check implicit expressions
							if (USER_EXPRESSION.startsWith(fragmentWithoutPrefix)) {
								result.add(USER_EXPRESSION + '}');
							}
							if (USERID_EXPRESSION.startsWith(fragmentWithoutPrefix)) {
								result.add(USERID_EXPRESSION + '}');
							}
							if (USERNAME_EXPRESSION.startsWith(fragmentWithoutPrefix)) {
								result.add(USERNAME_EXPRESSION + '}');
							}
							if (DATAGROUPID_EXPRESSION.startsWith(fragmentWithoutPrefix)) {
								result.add(DATAGROUPID_EXPRESSION + '}');
							}
							if (CONTACTID_EXPRESSION.startsWith(fragmentWithoutPrefix)) {
								result.add(CONTACTID_EXPRESSION + '}');
							}
							if (CUSTOMER_EXPRESSION.startsWith(fragmentWithoutPrefix)) {
								result.add(CUSTOMER_EXPRESSION + '}');
							}
							if (DATE_EXPRESSION.startsWith(fragmentWithoutPrefix)) {
								result.add(DATE_EXPRESSION + '}');
							}
							if (TIME_EXPRESSION.startsWith(fragmentWithoutPrefix)) {
								result.add(TIME_EXPRESSION + '}');
							}
							if (DATETIME_EXPRESSION.startsWith(fragmentWithoutPrefix)) {
								result.add(DATETIME_EXPRESSION + '}');
							}
							if (TIMESTAMP_EXPRESSION.startsWith(fragmentWithoutPrefix)) {
								result.add(TIMESTAMP_EXPRESSION + '}');
							}
							if (URL_EXPRESSION.startsWith(fragmentWithoutPrefix)) {
								result.add(URL_EXPRESSION + '}');
							}
	
							// Check binding expressions
							List<String> completions = DEFAULT_EVALUATOR.completeWithoutPrefixOrSuffix(fragmentWithoutPrefix, customer, module, document);
							if (completions == null) {
								throw new IllegalStateException("Complete Expression of " + fragment + " yields null");
							}
							result.addAll(completions);
						}
						else {
							String prefix = fragment.substring(openCurlyBraceIndex + 1, colonIndex).trim();
							ExpressionEvaluator eval = EVALUATORS.get(prefix);
							if (eval != null) { // only complete if we have a valid evaluator
								fragmentWithoutPrefix = (fragment.length() > (colonIndex + 1)) ?
															fragment.substring(colonIndex + 1, fragment.length()).trim() :
															"";
								result = eval.completeWithoutPrefixOrSuffix(fragmentWithoutPrefix, customer, module, document);
								if (result == null) {
									throw new IllegalStateException("Complete Expression of " + fragment + " yields null");
								}
							}
						}
						break;
					}
				}
				
				// find the next occurrence
				openCurlyBraceIndex = fragment.indexOf("{", openCurlyBraceIndex + 1);
			}

			// put the prefixing boilerplate text from the fragment back in
			if (result != null) {
				String boilerplate = fragment.substring(0, Math.max(openCurlyBraceIndex, colonIndex) + 1);
				if (! boilerplate.isEmpty()) {
					for (int i = 0, l = result.size(); i < l; i++) {
						result.add(i, boilerplate + result.remove(i));
					}
				}
			}
		}

		// If there was no expression syntax to complete, suggest nothing
		if (result == null) {
			result = Collections.emptyList();
		}

		return result;
	}
	
	/**
	 * Called by format and evaluate methods.
	 * 
	 * @param expression The expression to evaluate
	 * @param bean The context bean
	 * @param format Whether this is called by format(), otherwise called by evaluate().
	 * @return The evaluated expression.
	 */
	private static Object process(String expression, Bean bean, boolean format) {
		String expressionWithoutSuffix = expression;
		String formatName = null;
		
		// Look for a format suffix and extract the format name
		int pipeIndex = expression.lastIndexOf('|');
		if (pipeIndex > -1) { // found a pipe format suffix
			String formatSuffix = expression.substring(pipeIndex, expression.length() - 1); // assume '}' on end
			if (! formatSuffix.isEmpty()) {
				expressionWithoutSuffix = expression.replace(formatSuffix, "");
				formatName = UtilImpl.processStringValue(formatSuffix.substring(1)); // remove '|' at beginning
			}
		}
		
		int colonIndex = expressionWithoutSuffix.indexOf(':');
		if (colonIndex < 0) {
			// Remove {} and trim.
			String expressionWithoutPrefixOrSuffix = expressionWithoutSuffix.substring(1, expressionWithoutSuffix.length() - 1).trim();

			if (USER_EXPRESSION.equals(expressionWithoutPrefixOrSuffix)) {
				String result = CORE.getUser().getName();
				if (formatName != null) {
					return CORE.format(formatName, result);
				}
				if (format) {
					return (result == null) ? "" : result;
				}
				return result;
			}
			if (USERID_EXPRESSION.equals(expressionWithoutPrefixOrSuffix)) {
				String result = CORE.getUser().getId();
				if (formatName != null) {
					return CORE.format(formatName, result);
				}
				if (format) {
					return (result == null) ? "" : result;
				}
				return result;
			}
			if (USERNAME_EXPRESSION.equals(expressionWithoutPrefixOrSuffix)) {
				String result = CORE.getUser().getContactName();
				if (formatName != null) {
					return CORE.format(formatName, result);
				}
				if (format) {
					return (result == null) ? "" : result;
				}
				return result;
			}
			if (DATAGROUPID_EXPRESSION.equals(expressionWithoutPrefixOrSuffix)) {
				String result = CORE.getUser().getDataGroupId();
				if (formatName != null) {
					return CORE.format(formatName, result);
				}
				if (format) {
					return (result == null) ? "" : result;
				}
				return result;
			}
			if (CONTACTID_EXPRESSION.equals(expressionWithoutPrefixOrSuffix)) {
				String result = CORE.getUser().getContactId();
				if (formatName != null) {
					return CORE.format(formatName, result);
				}
				if (format) {
					return (result == null) ? "" : result;
				}
				return result;
			}
			if (CUSTOMER_EXPRESSION.equals(expressionWithoutPrefixOrSuffix)) {
				String result = CORE.getCustomer().getName();
				if (formatName != null) {
					return CORE.format(formatName, result);
				}
				if (format) {
					return (result == null) ? "" : result;
				}
				return result;
			}
			if (DATE_EXPRESSION.equals(expressionWithoutPrefixOrSuffix)) {
				DateOnly result = new DateOnly();
				if (formatName != null) {
					return CORE.format(formatName, result);
				}
				if (format) {
					return BindUtil.toDisplay(CORE.getCustomer(), null, null, result);
				}
				return result;
			}
			if (TIME_EXPRESSION.equals(expressionWithoutPrefixOrSuffix)) {
				TimeOnly result = new TimeOnly();
				if (formatName != null) {
					return CORE.format(formatName, result);
				}
				if (format) {
					return BindUtil.toDisplay(CORE.getCustomer(), null, null, result);
				}
				return result;
			}
			if (DATETIME_EXPRESSION.equals(expressionWithoutPrefixOrSuffix)) {
				DateTime result = new DateTime();
				if (formatName != null) {
					return CORE.format(formatName, result);
				}
				if (format) {
					return BindUtil.toDisplay(CORE.getCustomer(), null, null, result);
				}
				return result;
			}
			if (TIMESTAMP_EXPRESSION.equals(expressionWithoutPrefixOrSuffix)) {
				Timestamp result = new Timestamp();
				if (formatName != null) {
					return CORE.format(formatName, result);
				}
				if (format) {
					return BindUtil.toDisplay(CORE.getCustomer(), null, null, result);
				}
				return result;
			}
			if (URL_EXPRESSION.equals(expressionWithoutPrefixOrSuffix)) {
				if (bean == null) {
					return format ? "" : null;
				}
				if (formatName != null) {
					return CORE.format(formatName, Util.getDocumentUrl(bean));
				}
				return Util.getDocumentUrl(bean);
			}

			if (formatName != null) {
				return CORE.format(formatName, DEFAULT_EVALUATOR.evaluateWithoutPrefixOrSuffix(expressionWithoutPrefixOrSuffix, bean));
			}
			if (format) {
				return DEFAULT_EVALUATOR.formatWithoutPrefixOrSuffix(expressionWithoutPrefixOrSuffix, bean);
			}
			return DEFAULT_EVALUATOR.evaluateWithoutPrefixOrSuffix(expressionWithoutPrefixOrSuffix, bean);
		}
		
		String prefix = expressionWithoutSuffix.substring(1, colonIndex).trim();
		String expressionWithoutPrefixOrSuffix = expressionWithoutSuffix.substring(colonIndex + 1, expressionWithoutSuffix.length() - 1).trim();

		ExpressionEvaluator eval = EVALUATORS.get(prefix);
		if (eval == null) {
			throw new DomainException("Cannot find an expression evaluator for prefix " + prefix);
		}
		
		if (formatName != null) {
			return CORE.format(formatName, eval.evaluateWithoutPrefixOrSuffix(expressionWithoutPrefixOrSuffix, bean));
		}
		if (format) {
			return eval.formatWithoutPrefixOrSuffix(expressionWithoutPrefixOrSuffix, bean);
		}
		return eval.evaluateWithoutPrefixOrSuffix(expressionWithoutPrefixOrSuffix, bean);
	}
	
	public abstract @Nonnull String formatWithoutPrefixOrSuffix(@Nonnull String expression, @Nullable Bean bean);
	public abstract @Nullable Object evaluateWithoutPrefixOrSuffix(@Nonnull String expression, @Nullable Bean bean);
	public abstract @Nullable String validateWithoutPrefixOrSuffix(@Nonnull String expression,
																	@Nullable Class<?> returnType,
																	@Nullable Customer customer,
																	@Nullable Module module,
																	@Nullable Document document);
	public abstract @Nonnull List<String> completeWithoutPrefixOrSuffix(@Nullable String fragment,
																			@Nonnull Customer customer,
																			@Nonnull Module module,
																			@Nonnull Document document);
	public abstract void prefixBindingWithoutPrefixOrSuffix(@Nonnull StringBuilder expression, @Nonnull String binding);
}
