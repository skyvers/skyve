package org.skyve.impl.bind;

import java.beans.PropertyDescriptor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

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
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.ExpressionEvaluator;
import org.skyve.util.Util;
import org.skyve.util.logging.SkyveLoggerFactory;
import org.slf4j.Logger;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.el.ELClass;
import jakarta.el.ELContext;
import jakarta.el.ELManager;
import jakarta.el.ELProcessor;
import jakarta.el.ELResolver;
import jakarta.el.FunctionMapper;
import jakarta.el.VariableMapper;

/**
 * Evaluates Skyve EL expressions against bean, user, and stash contexts.
 *
 * <p>The evaluator supports runtime evaluation, validation-time type checking,
 * and completion hints for expression authoring.
 *
 * <p>Complexity: completion is linear in the number of known expression prefixes
 * and candidate members on the resolved type.
 */
public class ELExpressionEvaluator extends ExpressionEvaluator {
	/**
	 * Expression prefix used for validation-time EL evaluation.
	 */
	public static final String EL_PREFIX = "el";

	/**
	 * Expression prefix used for runtime EL evaluation.
	 */
	public static final String RTEL_PREFIX = "rtel";

	private static final Logger LOGGER = SkyveLoggerFactory.getLogger(ELExpressionEvaluator.class);
	private static final String BEAN_VARIABLE = "bean";
	private static final String USER_VARIABLE = "user";
	private static final String STASH_VARIABLE = "stash";
	private static final Map<String, Method> FIXED_FUNCTIONS = fixedFunctions();
	private static final BindingELResolver BINDING_RESOLVER = new BindingELResolver();
	/** Immutable enum bindings per bean class; ClassValue does not pin redeployed class loaders. */
	static final ClassValue<Map<String, ELClass>> ENUM_BEANS = new ClassValue<>() {
		@Override
		protected Map<String, ELClass> computeValue(Class<?> type) {
			return enumBeans(type);
		}
	};

	/** Keeps evaluation state local while resolving fixed functions from the shared mapper. */
	private static final class FunctionContext extends ELContext {
		private final ELResolver resolver;
		private final VariableMapper variables;
		private Map<String, Method> localFunctions;
		private final FunctionMapper functions = new FunctionMapper() {
			@Override
			public Method resolveFunction(String prefix, String localName) {
				String key = prefix + ':' + localName;
				Method result = (localFunctions == null) ? null : localFunctions.get(key);
				return (result == null) ? FIXED_FUNCTIONS.get(key) : result;
			}

			@Override
			public void mapFunction(String prefix, String localName, Method method) {
				if (localFunctions == null) {
					localFunctions = new HashMap<>();
				}
				localFunctions.put(prefix + ':' + localName, method);
			}
		};

		private FunctionContext(ELContext original) {
			resolver = original.getELResolver();
			variables = original.getVariableMapper();
		}

		@Override
		public ELResolver getELResolver() {
			return resolver;
		}

		@Override
		public FunctionMapper getFunctionMapper() {
			return functions;
		}

		@Override
		public VariableMapper getVariableMapper() {
			return variables;
		}
	}

	// Regex expressions to find the start of an EL expression
	private static final String[] COMMENCING_REGEX_TOKENS = new String[] {BEAN_VARIABLE + "\\s*\\.",
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
																		BEAN_VARIABLE,
																		USER_VARIABLE,
																		STASH_VARIABLE,
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
	
	/**
	 * Creates an EL evaluator.
	 *
	 * @param typesafe {@code true} to enable validation against the supplied metadata context
	 */
	public ELExpressionEvaluator(boolean typesafe) {
		this.typesafe = typesafe;
	}

	/**
	 * Evaluates the expression against the current EL context.
	 *
	 * @param expression the EL expression without prefix or suffix
	 * @param bean the bean used as the EL {@code bean} variable; may be {@code null}
	 * @return the evaluated value, or {@code null} when the expression resolves to null
	 */
	@Override
	public Object evaluateWithoutPrefixOrSuffix(String expression, Bean bean) {
		return newSkyveEvaluationProcessor(bean).eval(expression);
	}

	/**
	 * Formats the evaluated expression for display.
	 *
	 * @param expression the EL expression without prefix or suffix
	 * @param bean the bean used as the EL {@code bean} variable; may be {@code null}
	 * @return the display text derived from the evaluated value
	 */
	@Override
	public String formatWithoutPrefixOrSuffix(String expression, Bean bean) {
		return BindUtil.toDisplay(CORE.getCustomer(), evaluateWithoutPrefixOrSuffix(expression, bean));
	}
	
	/**
	 * Validates a type-safe EL expression against the supplied metadata context.
	 *
	 * @param expression the EL expression without prefix or suffix
	 * @param returnType the required result type, or {@code null} when unconstrained
	 * @param customer the customer metadata context
	 * @param module the module metadata context
	 * @param document the document metadata context
	 * @return a validation message when the expression is malformed or the result type is incompatible;
	 *         otherwise {@code null}
	 */
	@Override
	@SuppressWarnings("java:S3776") // Complexity OK
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
					if (evaluation instanceof DocumentImpl evaluationDocument) {
						// Only reachable with a document, which newSkyveValidationProcessor() requires a customer for
						if (customer == null) {
							throw new IllegalStateException("Cannot resolve a document bean class without a customer");
						}
						type = evaluationDocument.getBeanClass(customer);
					}
					else if (evaluation instanceof Class<?> evaluationClass) {
						type = evaluationClass;
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
				LOGGER.error(e.getMessage(), e);
				result = e.getMessage();
				if (result == null) {
					result = expression + " is malformed and caused an exception " + e.getClass();
				}
			}
		}
		
		return result;
	}
	
	/**
	 * Completes the expression fragment using the current EL context.
	 *
	 * @param fragment the partial expression being authored
	 * @param customer the customer metadata context; may be {@code null} when no document is resolved
	 * @param module the module metadata context
	 * @param document the document metadata context
	 * @return matching completion candidates in the order they were discovered
	 */
	@Override
	@SuppressWarnings({"java:S3776", "java:S6541"}) // complexity OK
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
				String lastExpression = input.substring(lastCommencingTokenIndex,
															(lastDelimiterIndex == lastClosingSquareBraceIndex) ?
																lastDelimiterIndex + 1 :
																lastDelimiterIndex);
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
								if (lastEvaluation instanceof DocumentImpl lastEvaluationDocument) {
									MetaDataExpressionEvaluator.addAttributesAndConditions(baseExpression,
																							simpleBindingFragment,
																							customer,
																							document,
																							result);
									lastEvaluationClass = lastEvaluationDocument.getBeanClass(customer);
								}
								else if (lastEvaluation instanceof Class<?> lastEvaluationType) {
									lastEvaluationClass = lastEvaluationType;
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
							else if ((lastEvaluation instanceof Class<?> lastEvaluationType) &&
										Map.class.isAssignableFrom(lastEvaluationType)) {
								result.add(baseExpression + "['");
							}
						}
						// not continuing an expression - commence a new expression
						else {
							newExpression(input, result);
						}
					}
				}
				catch (Exception e) {
				    LOGGER.warn("{} is malformed and caused exception {} :- {}", input, e.getClass(), e.getMessage());
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
	
	/**
	 * Prefixes {@code bean} bindings in the expression with the supplied binding path.
	 *
	 * @param expression the expression buffer to mutate
	 * @param binding the binding path to insert before each {@code bean} reference
	 */
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
	
	/**
	 * Creates an EL processor configured for validation against metadata.
	 *
	 * @param customer the customer metadata context; may be {@code null} only when {@code document} is {@code null}
	 * @param document the document metadata context; may be {@code null}
	 * @return a configured processor with Skyve EL functions and validation resolvers
	 * @throws IllegalArgumentException if {@code document} is given without a {@code customer}
	 */
	public static ELProcessor newSkyveValidationProcessor(@Nullable Customer customer, @Nullable Document document) {
		ELProcessor result = new ELProcessor();
		ELManager manager = result.getELManager();
		manager.setELContext(new FunctionContext(manager.getELContext()));
		result.defineBean(USER_VARIABLE, UserImpl.class);
		result.defineBean(STASH_VARIABLE, Map.class);
		manager.importClass(Decimal2.class.getCanonicalName());
		manager.importClass(Decimal5.class.getCanonicalName());
		manager.importClass(Decimal10.class.getCanonicalName());
		if (document != null) {
			if (customer == null) {
				throw new IllegalArgumentException("A customer is required to validate against a document");
			}
			result.defineBean(BEAN_VARIABLE, document);
			defineEnumBeans(result, customer, document);
		}
		manager.addELResolver(new ValidationELResolver(customer));
		return result;
	}
	
	/**
	 * Binds the document's static enumerations by simple name, using the generated enum class when it can be
	 * loaded and the enumeration metadata otherwise (during domain generation, before the class exists).
	 * Locally declared enums take precedence over imported enums with the same simple name.
	 *
	 * @param processor the validation processor to define the enum beans on
	 * @param customer the customer metadata context
	 * @param document the document whose attributes declare the enumerations
	 */
	private static void defineEnumBeans(@Nonnull ELProcessor processor, @Nonnull Customer customer, @Nonnull Document document) {
		Set<String> boundNames = new HashSet<>();
		for (Enumeration enumeration : staticEnumerations(customer, document)) {
			String name = enumeration.toJavaIdentifier();
			if (boundNames.add(name)) {
				Class<?> enumClass = loadEnumClass(enumeration);
				processor.defineBean(name, (enumClass == null) ? enumeration : new ELClass(enumClass));
			}
		}
	}

	/**
	 * Lists the document's static enumerations with locally declared enums ahead of imported ones.
	 *
	 * @param customer the customer metadata context
	 * @param document the document whose attributes declare the enumerations
	 * @return the static enumerations, local declarations first
	 */
	private static @Nonnull List<Enumeration> staticEnumerations(@Nonnull Customer customer, @Nonnull Document document) {
		List<Enumeration> result = new ArrayList<>();
		List<Enumeration> importedEnums = new ArrayList<>();
		for (Attribute attribute : document.getAllAttributes(customer)) {
			if ((attribute instanceof Enumeration enumeration) && (! enumeration.isDynamic())) {
				if ((enumeration.getAttributeRef() == null) && (enumeration.getImplementingEnumClassName() == null)) {
					result.add(enumeration);
				}
				else {
					importedEnums.add(enumeration);
				}
			}
		}
		// Locally declared enums retain their simple names when imported types collide.
		result.addAll(importedEnums);
		return result;
	}

	/**
	 * Loads the generated enum class for the enumeration.
	 *
	 * @param enumeration the enumeration metadata
	 * @return the enum class, or {@code null} if it has not been generated yet
	 */
	private static @Nullable Class<?> loadEnumClass(@Nonnull Enumeration enumeration) {
		try {
			return enumeration.getImplementingType();
		}
		catch (MetaDataException e) {
			if (! Enumeration.isEnumClassLoadingFailure(e)) {
				throw e;
			}
			return null;
		}
	}
	
	/**
	 * Creates an EL processor configured for runtime evaluation.
	 *
	 * @param bean the bean to expose as {@code bean}; may be {@code null}
	 * @return a configured processor with Skyve EL functions and runtime binding resolvers
	 */
	public static ELProcessor newSkyveEvaluationProcessor(@Nullable Bean bean) {
		ELProcessor result = new ELProcessor();
		ELManager manager = result.getELManager();
		manager.setELContext(new FunctionContext(manager.getELContext()));
		manager.addELResolver(BINDING_RESOLVER);
		if (bean != null) {
			manager.defineBean(BEAN_VARIABLE, bean);
			ENUM_BEANS.get(bean.getClass()).forEach(manager::defineBean);
		}
		manager.defineBean(USER_VARIABLE, CORE.getUser());
		manager.defineBean(STASH_VARIABLE, CORE.getStash());
		manager.importClass(Decimal2.class.getCanonicalName());
		manager.importClass(Decimal5.class.getCanonicalName());
		manager.importClass(Decimal10.class.getCanonicalName());
		return result;
	}

	/**
	 * Collects the declared and imported enum types from a bean class hierarchy, keyed by simple name.
	 * Declared enum types take precedence over imported instance field types, and subclass declarations
	 * take precedence over superclass declarations when simple names match.
	 *
	 * @param beanClass the bean class whose hierarchy declares enums or has enum-typed instance fields
	 * @return an immutable map of simple enum names to their EL classes
	 */
	static @Nonnull Map<String, ELClass> enumBeans(@Nonnull Class<?> beanClass) {
		Map<String, ELClass> result = new HashMap<>();
		for (Class<?> type = beanClass; type != null; type = type.getSuperclass()) {
			for (Class<?> nested : type.getDeclaredClasses()) {
				if (nested.isEnum() && Modifier.isPublic(nested.getModifiers())) {
					result.putIfAbsent(nested.getSimpleName(), new ELClass(nested));
				}
			}
		}
		for (Class<?> type = beanClass; type != null; type = type.getSuperclass()) {
			for (Field field : type.getDeclaredFields()) {
				Class<?> fieldType = field.getType();
				if ((! Modifier.isStatic(field.getModifiers())) && fieldType.isEnum() && Modifier.isPublic(fieldType.getModifiers())) {
					result.putIfAbsent(fieldType.getSimpleName(), new ELClass(fieldType));
				}
			}
		}
		return Map.copyOf(result);
	}
	
	/**
	 * Builds the fixed Skyve functions, keyed as {@code prefix:localName} with an empty prefix.
	 * The map is immutable so it can be shared safely by concurrent EL contexts.
	 */
	private static @Nonnull Map<String, Method> fixedFunctions() {
		Map<String, Method> result = new HashMap<>();
		
		try {
			Class<?> functions = ELFunctions.class;
			defineFunction(result, functions.getMethod("newDateOnly"));
			defineFunction(result, functions.getMethod("newDateOnlyFromMillis", Long.TYPE));
			defineFunction(result, functions.getMethod("newDateOnlyFromDate", Date.class));
			defineFunction(result, functions.getMethod("newDateOnlyFromSerializedForm", String.class));
			defineFunction(result, functions.getMethod("newDateOnlyFromLocalDate", LocalDate.class));
			defineFunction(result, functions.getMethod("newDateOnlyFromLocalDateTime", LocalDateTime.class));

			defineFunction(result, functions.getMethod("newDateTime"));
			defineFunction(result, functions.getMethod("newDateTimeFromMillis", Long.TYPE));
			defineFunction(result, functions.getMethod("newDateTimeFromDate", Date.class));
			defineFunction(result, functions.getMethod("newDateTimeFromSerializedForm", String.class));
			defineFunction(result, functions.getMethod("newDateTimeFromLocalDate", LocalDate.class));
			defineFunction(result, functions.getMethod("newDateTimeFromLocalDateTime", LocalDateTime.class));

			defineFunction(result, functions.getMethod("newTimeOnly"));
			defineFunction(result, functions.getMethod("newTimeOnlyFromMillis", Long.TYPE));
			defineFunction(result, functions.getMethod("newTimeOnlyFromDate", Date.class));
			defineFunction(result, functions.getMethod("newTimeOnlyFromComponents", Integer.TYPE, Integer.TYPE, Integer.TYPE));
			defineFunction(result, functions.getMethod("newTimeOnlyFromSerializedForm", String.class));
			defineFunction(result, functions.getMethod("newTimeOnlyFromLocalTime", LocalTime.class));
			defineFunction(result, functions.getMethod("newTimeOnlyFromLocalDateTime", LocalDateTime.class));
			
			defineFunction(result, functions.getMethod("newTimestamp"));
			defineFunction(result, functions.getMethod("newTimestampFromMillis", Long.TYPE));
			defineFunction(result, functions.getMethod("newTimestampFromDate", Date.class));
			defineFunction(result, functions.getMethod("newTimestampFromSerializedForm", String.class));
			defineFunction(result, functions.getMethod("newTimestampFromLocalDate", LocalDate.class));
			defineFunction(result, functions.getMethod("newTimestampFromLocalDateTime", LocalDateTime.class));

			defineFunction(result, functions.getMethod("newDecimal2", Double.TYPE));
			defineFunction(result, functions.getMethod("newDecimal2FromBigDecimal", BigDecimal.class));
			defineFunction(result, functions.getMethod("newDecimal2FromDecimal", Decimal.class));
			defineFunction(result, functions.getMethod("newDecimal2FromString", String.class));
			
			defineFunction(result, functions.getMethod("newDecimal5", Double.TYPE));
			defineFunction(result, functions.getMethod("newDecimal5FromBigDecimal", BigDecimal.class));
			defineFunction(result, functions.getMethod("newDecimal5FromDecimal", Decimal.class));
			defineFunction(result, functions.getMethod("newDecimal5FromString", String.class));

			defineFunction(result, functions.getMethod("newDecimal10", Double.TYPE));
			defineFunction(result, functions.getMethod("newDecimal10FromBigDecimal", BigDecimal.class));
			defineFunction(result, functions.getMethod("newDecimal10FromDecimal", Decimal.class));
			defineFunction(result, functions.getMethod("newDecimal10FromString", String.class));

			defineFunction(result, functions.getMethod("newOptimisticLock", String.class, Date.class));
			defineFunction(result, functions.getMethod("newOptimisticLockFromString", String.class));
			defineFunction(result, functions.getMethod("newGeometry", String.class));
		}
		catch (NoSuchMethodException | SecurityException e) {
			throw new DomainException("Cannot define EL functions", e);
		}
		
		return Map.copyOf(result);
	}

	private static void defineFunction(@Nonnull Map<String, Method> functions, @Nonnull Method method) {
		functions.put(':' + method.getName(), method);
	}
}
