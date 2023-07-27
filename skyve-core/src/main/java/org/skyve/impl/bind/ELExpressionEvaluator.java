package org.skyve.impl.bind;

import java.beans.PropertyDescriptor;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.el.ELManager;
import javax.el.ELProcessor;

import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.lang3.StringUtils;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.ExpressionEvaluator;
import org.skyve.util.Util;

public class ELExpressionEvaluator extends ExpressionEvaluator {
	public static final String EL_PREFIX = "el";
	public static final String RTEL_PREFIX = "rtel";
	
	// Regex expressions to find the start of an EL expression
	private static final String[] COMMENCING_REGEX_TOKENS = new String[] {"bean\\s*\\.",
																		"user\\s*\\.",
																		"stash\\s*\\[",
																		"stash\\s*\\.",
																		"newDateOnly\\s*\\(\\s*\\)",
																		"newDateOnlyFromMillis\\s*\\(",
																		"newDateOnlyFromDate\\s*\\(",
																		"newDateOnlyFromSerializedForm\\s*\\(",
																		"newDateOnlyFromLocalDate\\s*\\(",
																		"newDateOnlyFromLocalDateTime\\s*\\(",
																		"newDateTime\\s*\\(\\s*\\)",
																		"newDateTimeFromMillis\\s*\\(",
																		"newDateTimeFromDate\\s*\\(",
																		"newDateTimeFromSerializedForm\\s*\\(",
																		"newDateTimeFromLocalDate\\s*\\(",
																		"newDateTimeFromLocalDateTime\\s*\\(",
																		"newTimeOnly\\s*\\(\\s*\\)",
																		"newTimeOnlyFromMillis\\s*\\(",
																		"newTimeOnlyFromDate\\s*\\(",
																		"newTimeOnlyFromComponents\\s*\\(",
																		"newTimeOnlyFromSerializedForm\\s*\\(",
																		"newTimeOnlyFromLocalTime\\s*\\(",
																		"newTimeOnlyFromLocalDateTime\\s*\\(",
																		"newTimestamp\\s*\\(\\s*\\)",
																		"newTimestampFromMillis\\s*\\(",
																		"newTimestampFromDate\\s*\\(",
																		"newTimestampFromSerializedForm\\s*\\(",
																		"newTimestampFromLocalDate\\s*\\(",
																		"newTimestampFromLocalDateTime\\s*\\(",
																		"newDecimal2\\s*\\(",
																		"newDecimal2FromBigDecimal\\s*\\(",
																		"newDecimal2FromDecimal\\s*\\(",
																		"newDecimal2FromString\\s*\\(",
																		"newDecimal5\\s*\\(",
																		"newDecimal5FromBigDecimal\\s*\\(",
																		"newDecimal5FromDecimal\\s*\\(",
																		"newDecimal5FromString\\s*\\(",
																		"newDecimal10\\s*\\(",
																		"newDecimal10FromBigDecimal\\s*\\(",
																		"newDecimal10FromDecimal\\s*\\(",
																		"newDecimal10FromString\\s*\\(",
																		"newOptimisticLock\\s*\\(",
																		"newOptimisticLockFromString\\s*\\(",
																		"newGeometry\\s*\\("};

	// Completes used when we know we are not continuing an expression (with '.' or '[')
	private static final String[] COMMENCING_COMPLETES = new String[] {"empty",
																		"concat(",
																		"bean",
																		"user",
																		"stash",
																		"newDateOnly()",
																		"newDateOnlyFromMillis(",
																		"newDateOnlyFromDate(",
																		"newDateOnlyFromSerializedForm(",
																		"newDateOnlyFromLocalDate(",
																		"newDateOnlyFromLocalDateTime(",
																		"newDateTime()",
																		"newDateTimeFromMillis(",
																		"newDateTimeFromDate(",
																		"newDateTimeFromSerializedForm(",
																		"newDateTimeFromLocalDate(",
																		"newDateTimeFromLocalDateTime(",
																		"newTimeOnly()",
																		"newTimeOnlyFromMillis(",
																		"newTimeOnlyFromDate(",
																		"newTimeOnlyFromComponents(",
																		"newTimeOnlyFromSerializedForm(",
																		"newTimeOnlyFromLocalTime(",
																		"newTimeOnlyFromLocalDateTime(",
																		"newTimestamp()",
																		"newTimestampFromMillis(",
																		"newTimestampFromDate(",
																		"newTimestampFromSerializedForm(",
																		"newTimestampFromLocalDate(",
																		"newTimestampFromLocalDateTime(",
																		"newDecimal2(",
																		"newDecimal2FromBigDecimal(",
																		"newDecimal2FromDecimal(",
																		"newDecimal2FromString(",
																		"newDecimal5(",
																		"newDecimal5FromBigDecimal(",
																		"newDecimal5FromDecimal(",
																		"newDecimal5FromString(",
																		"newDecimal10(",
																		"newDecimal10FromBigDecimal(",
																		"newDecimal10FromDecimal(",
																		"newDecimal10FromString(",
																		"newOptimisticLock(",
																		"newOptimisticLockFromString(",
																		"newGeometry("};

	private boolean typesafe = false;
	
	public ELExpressionEvaluator(boolean typesafe) {
		this.typesafe = typesafe;
	}

	@Override
	public Object evaluateWithoutPrefixOrSuffix(String expression, Bean bean) {
		ELProcessor elp = newSkyveEvaluationProcessor(bean);
		return elp.eval(expression);
	}

	@Override
	public String formatWithoutPrefixOrSuffix(String expression, Bean bean) {
		return BindUtil.toDisplay(CORE.getCustomer(), null, null, evaluateWithoutPrefixOrSuffix(expression, bean));
	}
	
	@Override
	public String validateWithoutPrefixOrSuffix(String expression,
													Class<?> returnType,
													Customer customer,
													Module module,
													Document document) {
		String result = null;

		if (typesafe) {
			try {
				// type-safe (el) starts with the document, if no document, no bean defined in the context
				ELProcessor elp = newSkyveValidationProcessor(customer, document);
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
	
	@Override
	public List<String> completeWithoutPrefixOrSuffix(String fragment,
														Customer customer,
														Module module,
														Document document) {
		List<String> result = new ArrayList<>();
		
		String input = (fragment == null) ? "" : fragment;

		// Used to determine if we are continuing an expression or commencing one
		int lastDotIndex = input.lastIndexOf('.');
		int lastOpeningSquareBraceIndex = input.lastIndexOf('[');
		int lastClosingSquareBraceIndex = input.lastIndexOf(']');
		int lastDelimiterIndex = Math.max(lastDotIndex, Math.max(lastOpeningSquareBraceIndex, lastClosingSquareBraceIndex));
		
		if (lastDelimiterIndex > 0) { // potentially continuing an expression
			// Determine the closest commencing token to the last delimiter
			int lastCommencingTokenIndex = -1;
			for (String commencingToken : COMMENCING_REGEX_TOKENS) {
				int index = Util.lastIndexOfRegEx(input, commencingToken);
				if (index > lastCommencingTokenIndex) {
					lastCommencingTokenIndex = index;
				}
			}

			// If we have an expression currently being authored
			if ((lastCommencingTokenIndex >= 0) && (lastCommencingTokenIndex < lastDelimiterIndex)) {
				String lastExpression = (lastDelimiterIndex == lastClosingSquareBraceIndex) ? 
											input.substring(lastCommencingTokenIndex, lastDelimiterIndex + 1) :
											input.substring(lastCommencingTokenIndex, lastDelimiterIndex);
				try {
					// Evaluate the penultimate expression
					ELProcessor elp = newSkyveValidationProcessor(customer, document);
					Object lastEvaluation = elp.eval(lastExpression);
					if (lastEvaluation != null) { // valid penultimate expression
						String baseExpression = null;
						// We are continuing the expression with a dot operator
						if (lastDelimiterIndex == lastDotIndex) {
							baseExpression = input.substring(0, lastDotIndex) + '.';
							// The bit after the dot - there may be nothing
							String simpleBindingFragment = (lastDelimiterIndex == (input.length() - 1)) ? 
																null : 
																input.substring(lastDelimiterIndex + 1);
							// There is either nothing after the last dot or alphanumeric (a property name)
							if ((simpleBindingFragment == null) || StringUtils.isAlphanumeric(simpleBindingFragment)) {
								// Set below to determine methods and bean properties
								Class<?> lastEvaluationClass = null;
								// Add document attributes and conditions if applicable
								if (lastEvaluation instanceof Document) {
									MetaDataExpressionEvaluator.addAttributesAndConditions(baseExpression,
																							simpleBindingFragment,
																							customer,
																							(Document) lastEvaluation,
																							result);
									lastEvaluationClass = ((DocumentImpl) lastEvaluation).getBeanClass(customer);
								}
								else if (lastEvaluation instanceof Class<?>) {
									lastEvaluationClass = (Class<?>) lastEvaluation;
								}
								else {
									lastEvaluationClass = lastEvaluation.getClass();
								}
								
								// If we have a class from the penultimate expression evaluation
								if (lastEvaluationClass != null) {
									// Add any missing bean properties not covered by document attributes and conditions 
									for (PropertyDescriptor d : PropertyUtils.getPropertyDescriptors(lastEvaluationClass)) {
										String e = baseExpression + d.getName();
										if (e.startsWith(input) && (! result.contains(e))) {
											result.add(e);
										}
									}
	
									// Add any methods found - close the round function braces if no arguments are required
									for (Method m : lastEvaluationClass.getMethods()) {
										String e = baseExpression + m.getName() + '(';
										if (m.getParameterTypes().length == 0) {
											e += ')';
											if (e.startsWith(input)) {
												result.add(e);
											}
										}
										else {
											// If we have a completed function with opening brace, complete with commencing EL,
											if (e.equals(input)) {
												for (String commencingComplete : COMMENCING_COMPLETES) {
													result.add(e + commencingComplete);
												}
											}
											// otherwise complete the function expression
											else if (e.startsWith(input)) {
												result.add(e);
											}
										}
									}
								}
							}
							else { // not an alphanumeric property name after the dot - commence a new expression
								newExpression(input, result);
							}
						}
						// If we have open square brace (array or map notation)
						else if (lastDelimiterIndex == lastOpeningSquareBraceIndex) {
							baseExpression = input.substring(0, lastOpeningSquareBraceIndex);
							// if a list, complete with generic array notation
							if (lastEvaluation instanceof List<?>) {
								result.add(baseExpression + "[0]");
								result.add(baseExpression + "[1]");
								result.add(baseExpression + "[2]");
								result.add(baseExpression + "[3]");
								result.add(baseExpression + "[4]");
								result.add(baseExpression + "[5]");
								result.add(baseExpression + "[6]");
								result.add(baseExpression + "[7]");
								result.add(baseExpression + "[8]");
								result.add(baseExpression + "[9]");
							}
							// if a map, start EL map key notation
							else if (lastEvaluation instanceof Class<?>) {
								if (Map.class.isAssignableFrom((Class<?>) lastEvaluation)) {
									result.add(baseExpression + "['");
								}
							}
						}
						// not continuing an expression - commence a new expression
						else {
							newExpression(input, result);
						}
					}
				}
				catch (Exception e) {
					UtilImpl.LOGGER.warning(input + "is malformed and caused exception " + e.getClass() + ":-" + e.getMessage());
				}
			}
			else { // ending an expression chain
				newExpression(input, result);
			}
		}
		else { // no last delimiter
			newExpression(input, result);
		}

		return result;
	}
	
	/**
	 * Commence a new expression, if the expression has not just been closed ']' or ')'.
	 * Count backwards along the input chars for alphanumeric commencing expression to search on.
	 * @param input
	 * @param completions
	 */
	private static void newExpression(String input, List<String> completions) {
		int i = input.length();
		char lastChar = input.isEmpty() ? '\0' : input.charAt(i - 1);
		if (lastChar != ']' && lastChar != ')') {
			while ((i > 0) && Character.isLetterOrDigit(input.charAt(i - 1))) {
				i--;
			}
			String match = input.substring(i);
			boolean matchEmpty = match.isEmpty();
			String prefix = input.substring(0, i);
			for (String commencingComplete : COMMENCING_COMPLETES) {
				if (matchEmpty || commencingComplete.startsWith(match)) {
					completions.add(prefix + commencingComplete);
				}
			}
		}
	}
	
	@Override
	public void prefixBindingWithoutPrefixOrSuffix(StringBuilder expression, String binding) {
		// Append binding to "bean."
		int beanIndex = expression.indexOf("bean.");
		while (beanIndex >= 0) {
			beanIndex += 5;
			expression.insert(beanIndex, '.');
			expression.insert(beanIndex, binding);
			beanIndex = expression.indexOf("bean.", beanIndex);
		}
		// check if ends with "bean" and append binding
		int length = expression.length();
		if ((length >= 4) && "bean".equals(expression.substring(length - 4, length))) {
			expression.append('.').append(binding);
		}
	}
	
	public static ELProcessor newSkyveValidationProcessor(@Nonnull Customer customer, @Nullable Document document) {
		ELProcessor result = setupProcessor(customer, document, UserImpl.class, Map.class);
		result.getELManager().addELResolver(new ValidationELResolver(customer));
		return result;
	}
	
	public static ELProcessor newSkyveEvaluationProcessor(@Nullable Bean bean) {
		ELProcessor result = setupProcessor(null, bean, CORE.getUser(), CORE.getStash());
		result.getELManager().addELResolver(new BindingELResolver());
		return result;
	}
	
	private static ELProcessor setupProcessor(@SuppressWarnings("unused") Customer customer, Object bean, Object user, Object stash) {
		ELProcessor result = new ELProcessor();
		if (bean != null) {
			result.defineBean("bean", bean);
		}
		result.defineBean("user", user);
		result.defineBean("stash", stash);
		
		try {
			Class<?> functions = ELFunctions.class;
			result.defineFunction("", "", functions.getMethod("newDateOnly"));
			result.defineFunction("", "", functions.getMethod("newDateOnlyFromMillis", Long.TYPE));
			result.defineFunction("", "", functions.getMethod("newDateOnlyFromDate", Date.class));
			result.defineFunction("", "", functions.getMethod("newDateOnlyFromSerializedForm", String.class));
			result.defineFunction("", "", functions.getMethod("newDateOnlyFromLocalDate", LocalDate.class));
			result.defineFunction("", "", functions.getMethod("newDateOnlyFromLocalDateTime", LocalDateTime.class));

			result.defineFunction("", "", functions.getMethod("newDateTime"));
			result.defineFunction("", "", functions.getMethod("newDateTimeFromMillis", Long.TYPE));
			result.defineFunction("", "", functions.getMethod("newDateTimeFromDate", Date.class));
			result.defineFunction("", "", functions.getMethod("newDateTimeFromSerializedForm", String.class));
			result.defineFunction("", "", functions.getMethod("newDateTimeFromLocalDate", LocalDate.class));
			result.defineFunction("", "", functions.getMethod("newDateTimeFromLocalDateTime", LocalDateTime.class));

			result.defineFunction("", "", functions.getMethod("newTimeOnly"));
			result.defineFunction("", "", functions.getMethod("newTimeOnlyFromMillis", Long.TYPE));
			result.defineFunction("", "", functions.getMethod("newTimeOnlyFromDate", Date.class));
			result.defineFunction("", "", functions.getMethod("newTimeOnlyFromComponents", Integer.TYPE, Integer.TYPE, Integer.TYPE));
			result.defineFunction("", "", functions.getMethod("newTimeOnlyFromSerializedForm", String.class));
			result.defineFunction("", "", functions.getMethod("newTimeOnlyFromLocalTime", LocalTime.class));
			result.defineFunction("", "", functions.getMethod("newTimeOnlyFromLocalDateTime", LocalDateTime.class));
			
			result.defineFunction("", "", functions.getMethod("newTimestamp"));
			result.defineFunction("", "", functions.getMethod("newTimestampFromMillis", Long.TYPE));
			result.defineFunction("", "", functions.getMethod("newTimestampFromDate", Date.class));
			result.defineFunction("", "", functions.getMethod("newTimestampFromSerializedForm", String.class));
			result.defineFunction("", "", functions.getMethod("newTimestampFromLocalDate", LocalDate.class));
			result.defineFunction("", "", functions.getMethod("newTimestampFromLocalDateTime", LocalDateTime.class));

			result.defineFunction("", "", functions.getMethod("newDecimal2", Double.TYPE));
			result.defineFunction("", "", functions.getMethod("newDecimal2FromBigDecimal", BigDecimal.class));
			result.defineFunction("", "", functions.getMethod("newDecimal2FromDecimal", Decimal.class));
			result.defineFunction("", "", functions.getMethod("newDecimal2FromString", String.class));
			
			result.defineFunction("", "", functions.getMethod("newDecimal5", Double.TYPE));
			result.defineFunction("", "", functions.getMethod("newDecimal5FromBigDecimal", BigDecimal.class));
			result.defineFunction("", "", functions.getMethod("newDecimal5FromDecimal", Decimal.class));
			result.defineFunction("", "", functions.getMethod("newDecimal5FromString", String.class));

			result.defineFunction("", "", functions.getMethod("newDecimal10", Double.TYPE));
			result.defineFunction("", "", functions.getMethod("newDecimal10FromBigDecimal", BigDecimal.class));
			result.defineFunction("", "", functions.getMethod("newDecimal10FromDecimal", Decimal.class));
			result.defineFunction("", "", functions.getMethod("newDecimal10FromString", String.class));

			result.defineFunction("", "", functions.getMethod("newOptimisticLock", String.class, Date.class));
			result.defineFunction("", "", functions.getMethod("newOptimisticLockFromString", String.class));
			result.defineFunction("", "", functions.getMethod("newGeometry", String.class));
		}
		catch (NoSuchMethodException | SecurityException e) {
			throw new DomainException("Cannot define EL functions", e);
		}
		
		final ELManager elManager = result.getELManager();
		elManager.importClass(Decimal2.class.getCanonicalName());
		elManager.importClass(Decimal5.class.getCanonicalName());
		elManager.importClass(Decimal10.class.getCanonicalName());

/* TODO resolve this inner enum class problem.
	Cannot import the domain classes here coz its not on the classpath for the maven mojo.
	And I cant get it to work at runtime for a nested enum class either using $ or . in the class name.
	We might have to use the normal defaulting for default values and allow built-in EL String coercion for enums in expressions.

		Class<?> classToImport = null;
		
		if (bean instanceof Document) { // could be a Document in validation mode
			DocumentImpl d = (DocumentImpl) bean;
			if (! d.isDynamic()) {
				try {
					classToImport = d.getBeanClass(customer);
				} catch (ClassNotFoundException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
		else if ((bean != null) && (! (bean instanceof DynamicBean))) { // not dynamic
			classToImport = bean.getClass();
		}

		if (classToImport != null) {
			String name = classToImport.getCanonicalName();
			elManager.importClass(name);
			for (Class<?> innerClass : classToImport.getDeclaredClasses()) {
				String innerName = innerClass.getCanonicalName();// name + '$' + innerClass.getSimpleName();
				elManager.importClass(innerName);
			}
			classToImport.getSuperclass();
		}
*/		
		return result;
	}
}
