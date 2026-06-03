package org.skyve.impl.bind;

import java.beans.Introspector;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.text.ParseException;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.StringTokenizer;
import java.util.function.Function;
import java.util.function.UnaryOperator;

import org.apache.commons.collections.comparators.ComparatorChain;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.io.WKTReader;
import org.locationtech.jts.io.WKTWriter;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.SkyveException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.Enumeration;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.enumeration.DynamicEnumerationConverter;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.InverseMany;
import org.skyve.impl.metadata.model.document.InverseOne;
import org.skyve.impl.metadata.model.document.field.ConvertibleField;
import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.util.NullTolerantBeanComparator;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.Ordering;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Condition;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Inverse;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.ExpressionEvaluator;
import org.skyve.util.logging.SkyveLoggerFactory;
import org.slf4j.Logger;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Core implementation of Skyve's bean binding engine.
 *
 * <p>Provides get/set/convert/format operations on bean attributes identified by
 * dot-separated binding expressions (e.g. {@code "contact.name"}). Traversal is
 * metadata-aware: it consults document attribute definitions to apply the correct
 * {@link org.skyve.domain.types.converters.Converter} for display formatting and
 * string-to-value parsing.
 *
 * <p>Key responsibilities:
 * <ul>
 *   <li>Property traversal via Apache Commons BeanUtils, extended by
 *       {@link DeproxyingPropertyUtilsBean} to handle Hibernate proxies.</li>
 *   <li>Expression evaluation delegation to the registered
 *       {@link org.skyve.util.ExpressionEvaluator} (EL, display-name, i18n, etc.).</li>
 *   <li>Collection sorting by declared or supplied {@link org.skyve.metadata.Ordering} lists.</li>
 *   <li>Format/parse of typed values using the attribute's converter.</li>
 * </ul>
 *
 * <p>The public API is exposed via {@link org.skyve.util.Binder}; use that class
 * directly from application code.
 *
 * <p>Threading: all methods are stateless and thread-safe.
 */
public final class BindUtil {
	private static final String DEFAULT_DISPLAY_DATE_FORMAT = "dd/MM/yyyy";
	private static final String ELEMENT = "Element";
	private static final String ELEMENT_BY_ID = "ElementById(";
	private static final Object UNRESOLVED = new Object();
	private static final DeproxyingPropertyUtilsBean PROPERTY_UTILS = new DeproxyingPropertyUtilsBean();
	
	private static final Logger LOGGER = SkyveLoggerFactory.getLogger(BindUtil.class); 

	private BindUtil() {
		// prevent instantiation
	}
	
	public static @Nonnull String formatMessage(@Nonnull String message, @Nonnull Bean... beans) {
		return formatMessage(message, (UnaryOperator<String>) null, beans);
	}

	/**
	 * Backward-compatible overload retained for callers compiled against a previous
	 * signature that accepted {@link Function}.
	 */
	public static @Nonnull String formatMessage(@Nonnull String message,
												@SuppressWarnings("java:S4276") // Allow null for backward compatibility
												@Nullable Function<String, String> postEvaluateDisplayValue,
												@Nonnull Bean... beans) {
		UnaryOperator<String> unaryOperator = (postEvaluateDisplayValue == null) ? null : postEvaluateDisplayValue::apply;
		return formatMessage(message, unaryOperator, beans);
	}
	
	public static @Nonnull String formatMessage(@Nonnull String message, 
													@Nullable UnaryOperator<String> postEvaluateDisplayValue,
													@Nonnull Bean... beans) {
		StringBuilder result = new StringBuilder(message);
		int openCurlyBraceIndex = nextMessageExpressionStart(result, 0);
		while (openCurlyBraceIndex >= 0) {
			int closedCurlyBraceIndex = findClosingBrace(message, result, openCurlyBraceIndex);
			String expression = result.substring(openCurlyBraceIndex, closedCurlyBraceIndex + 1);
			String displayValue = formatExpression(expression, postEvaluateDisplayValue, beans);
			result.replace(openCurlyBraceIndex, closedCurlyBraceIndex + 1, displayValue);
			openCurlyBraceIndex = nextMessageExpressionStart(result, openCurlyBraceIndex + displayValue.length());
		}

		return result.toString().replace("\\}", "}"); // remove any escaped closing curly braces now
	}

	public static boolean containsSkyveExpressions(@Nonnull String display) {
		boolean result = false;
		int openCurlyBraceIndex = display.indexOf('{');
		while ((! result) && (openCurlyBraceIndex >= 0)) {
			result = (openCurlyBraceIndex == 0) || // first char is '{' 
						// '{' is present and not escaped with a preceding '\' - ie \{ is escaped
						((openCurlyBraceIndex > 0) && (display.charAt(openCurlyBraceIndex - 1) != '\\'));
			openCurlyBraceIndex = display.indexOf("{", openCurlyBraceIndex + 1);			
		}
		
		return result;
	}
	
	public static boolean isSkyveExpression(@Nonnull String expression) {
		int length = expression.length();
		return ((length > 1) && 
					(expression.charAt(0) == '{') && 
					(expression.charAt(length - 1) == '}'));
	}
	
	public static @Nullable String validateMessageExpressions(@Nonnull String message, 
																// NB Binding Expression evaluators require a customer
																@Nonnull Customer customer,
																@Nonnull Document... documents) {
		String error = null;
		
		StringBuilder result = new StringBuilder(message);
		int openCurlyBraceIndex = nextMessageExpressionStart(result, 0);
		while ((error == null) && (openCurlyBraceIndex >= 0)) {
			int closedCurlyBraceIndex = result.indexOf("}", openCurlyBraceIndex);
			if (closedCurlyBraceIndex < 0) {
				error = "Opening '{' with no closing '}'";
			}
			else {
				String expression = result.substring(openCurlyBraceIndex, closedCurlyBraceIndex + 1);
				error = validateMessageExpression(expression, customer, documents);
				openCurlyBraceIndex = nextMessageExpressionStart(result, openCurlyBraceIndex + 1);
			}
		}

		return error;
	}
	
	public static @Nonnull TargetMetaData validateBinding(@Nonnull Customer customer,
															@Nonnull Module module,
															@Nonnull Document document,
															@Nonnull String binding) {
		String ultimateBinding = determineUltimateBinding(binding);
		Document contextDocument = resolveBindingValidationContext(customer, module, document, binding, ultimateBinding);
		String testConditionName = normaliseConditionBindingName(ultimateBinding);

		if (contextDocument.getCondition(testConditionName) != null) {
			return new TargetMetaData(contextDocument, null, Boolean.class);
		}

		Module contextModule = customer.getModule(contextDocument.getOwningModuleName());
		return BindUtil.getMetaDataForBinding(customer, contextModule, contextDocument, ultimateBinding);
	}

	private static int nextMessageExpressionStart(@Nonnull StringBuilder value, int fromIndex) {
		int openCurlyBraceIndex = value.indexOf("{", fromIndex);
		while (openCurlyBraceIndex >= 0) {
			if (isUnescapedOpeningBrace(value, openCurlyBraceIndex)) {
				return openCurlyBraceIndex;
			}
			value.deleteCharAt(openCurlyBraceIndex - 1);
			openCurlyBraceIndex = value.indexOf("{", openCurlyBraceIndex);
		}

		return -1;
	}

	private static boolean isUnescapedOpeningBrace(@Nonnull CharSequence value, int openCurlyBraceIndex) {
		return (openCurlyBraceIndex == 0) || (value.charAt(openCurlyBraceIndex - 1) != '\\');
	}

	private static int findClosingBrace(@Nonnull String message,
											@Nonnull StringBuilder result,
											int openCurlyBraceIndex) {
		int closedCurlyBraceIndex = result.indexOf("}", openCurlyBraceIndex);
		if (closedCurlyBraceIndex < 0) {
			throw new MetaDataException('[' + message + "] does not have a matching '}' for the '{' at position " + (openCurlyBraceIndex + 1));
		}

		return closedCurlyBraceIndex;
	}

	private static @Nonnull String formatExpression(@Nonnull String expression,
											   @Nullable UnaryOperator<String> postEvaluateDisplayValue,
											   @Nonnull Bean... beans) {
		Exception cause = null;
		for (Bean bean : beans) {
			try {
				String displayValue = ExpressionEvaluator.format(expression, bean);
				return (postEvaluateDisplayValue == null) ? displayValue : postEvaluateDisplayValue.apply(displayValue);
			}
			catch (Exception e) {
				cause = e;
			}
		}

		throw expressionEvaluationException(expression, cause, beans);
	}

	private static @Nonnull MetaDataException expressionEvaluationException(@Nonnull String expression,
														   @Nullable Exception cause,
														   @Nonnull Bean... beans) {
		StringBuilder message = new StringBuilder(64);
		message.append("Expression ").append(expression).append(" cannot be evaluated against bean");
		if (beans.length > 1) {
			message.append('s');
		}
		message.append(" - ");
		for (int offset = 0; offset < beans.length; offset++) {
			if (offset > 0) {
				message.append(", ");
			}
			message.append(beans[offset].getBizDocument());
		}

		return (cause == null) ? new MetaDataException(message.toString()) : new MetaDataException(message.toString(), cause);
	}

	private static @Nullable String validateMessageExpression(@Nonnull String expression,
											  @Nonnull Customer customer,
											  @Nonnull Document... documents) {
		StringBuilder errors = new StringBuilder(128);
		for (Document document : documents) {
			Module module = customer.getModule(document.getOwningModuleName());
			String error = ExpressionEvaluator.validate(expression, null, customer, module, document);
			if (error == null) {
				return null;
			}
			if (! errors.isEmpty()) {
				errors.append(". ");
			}
			errors.append(error);
		}

		return errors.toString();
	}

	private static @Nonnull String determineUltimateBinding(@Nonnull String binding) {
		int lastDotIndex = binding.lastIndexOf('.');
		return (lastDotIndex > 0) ? binding.substring(lastDotIndex + 1) : binding;
	}

	private static @Nonnull Document resolveBindingValidationContext(@Nonnull Customer customer,
														 @Nonnull Module module,
														 @Nonnull Document document,
														 @Nonnull String binding,
														 @Nonnull String ultimateBinding) {
		int lastDotIndex = binding.lastIndexOf('.');
		if (lastDotIndex <= 0) {
			return document;
		}

		String penultimateBinding = binding.substring(0, lastDotIndex);
		TargetMetaData target = resolvePenultimateBinding(customer, module, document, penultimateBinding);
		Attribute relation = target.getAttribute();
		if (relation instanceof Relation r) {
			Module owningModule = customer.getModule(target.getDocument().getOwningModuleName());
			return owningModule.getDocument(customer, r.getDocumentName());
		}
		if (ChildBean.PARENT_NAME.equals(penultimateBinding) || penultimateBinding.endsWith(ChildBean.CHILD_PARENT_NAME_SUFFIX)) {
			Document parentDocument = target.getDocument().getParentDocument(customer);
			if (parentDocument != null) {
				return parentDocument;
			}
			throw new MetaDataException("Binding " + penultimateBinding + " does not resolve to a parent document.");
		}

		throw new MetaDataException("Binding " + penultimateBinding + " does not resolve to a relation for " + ultimateBinding + '.');
	}

	private static @Nonnull TargetMetaData resolvePenultimateBinding(@Nonnull Customer customer,
														 @Nonnull Module module,
														 @Nonnull Document document,
														 @Nonnull String penultimateBinding) {
		try {
			return BindUtil.getMetaDataForBinding(customer, module, document, penultimateBinding);
		}
		catch (MetaDataException e) {
			throw e;
		}
		catch (Exception e) {
			throw new MetaDataException("Binding " + penultimateBinding + " does not resolve to a document attribute.", e);
		}
	}

	private static @Nonnull String normaliseConditionBindingName(@Nonnull String binding) {
		if (! binding.startsWith("not")) {
			return binding;
		}

		return Character.toLowerCase(binding.charAt(3)) + binding.substring(4);
	}
	
	/**
	 * Place the bindingPrefix + '.' + <existing expression> wherever a binding expression - {<expression>} occurs.
	 * @param message	The message to process. Can be null.
	 * @param bindingPrefix	The binding prefix to prefix with.
	 * @return	The prefixed message or null if message argument was null.
	 */
	public static @Nullable String prefixMessageExpressions(@Nullable String message, @Nonnull String bindingPrefix) {
		if (message == null) {
			return null;
		}

		StringBuilder result = new StringBuilder(message);
		int openCurlyBraceIndex = result.indexOf("{");
		while (openCurlyBraceIndex >= 0) {
			if ((openCurlyBraceIndex == 0) || // first char is '{' 
					// '{' is present and not escaped with a preceding '\' - ie \{ is escaped
					((openCurlyBraceIndex > 0) && (result.charAt(openCurlyBraceIndex - 1) != '\\'))) {

				int closedCurlyBraceIndex = result.indexOf("}", openCurlyBraceIndex);
				if (closedCurlyBraceIndex < 0) {
					throw new MetaDataException("Expression [" + message + "] has an unescaped opening '{' with no closing '}'");
				}

				String expression = result.substring(openCurlyBraceIndex, closedCurlyBraceIndex + 1);
				if (expression.length() == 2) {
					throw new MetaDataException("Expression [" + message + "] is empty");
				}
				String prefixedExpression = ExpressionEvaluator.prefixBinding(expression, bindingPrefix);
				result.replace(openCurlyBraceIndex, closedCurlyBraceIndex + 1, prefixedExpression);
			}
			openCurlyBraceIndex = result.indexOf("{", openCurlyBraceIndex + 1);
		}
		return result.toString();
	}

	public static boolean evaluateCondition(@Nonnull Bean bean, @Nonnull String condition) {
		if (String.valueOf(true).equals(condition)) {
			return true;
		}
		if (String.valueOf(false).equals(condition)) {
			return false;
		}

		try {
			return isSkyveExpression(condition) ? evaluateConditionExpression(bean, condition)
					: evaluateNamedCondition(bean, condition);
		}
		catch (Exception e) {
			throw new MetaDataException("Condition " + bean.getBizModule() + "." + bean.getBizDocument() + "." + condition + " is not valid", e);
		}
	}

	private static boolean evaluateConditionExpression(@Nonnull Bean bean, @Nonnull String expression) {
		return Boolean.TRUE.equals(ExpressionEvaluator.evaluate(expression, bean));
	}

	private static boolean evaluateNamedCondition(@Nonnull Bean bean, @Nonnull String condition) {
		boolean negated = condition.startsWith("not");
		String conditionName = negated ? Introspector.decapitalize(condition.substring(3)) : condition;
		Condition metadataCondition = getBeanCondition(bean, conditionName);
		if ((metadataCondition != null) && isSkyveExpression(metadataCondition.getExpression())) {
			return evaluateResolvedConditionExpression(bean, metadataCondition.getExpression(), negated);
		}

		return Boolean.TRUE.equals(BindUtil.get(bean, condition));
	}

	private static @Nullable Condition getBeanCondition(@Nonnull Bean bean, @Nonnull String conditionName) {
		Customer customer = CORE.getCustomer();
		Module module = customer.getModule(bean.getBizModule());
		Document document = module.getDocument(customer, bean.getBizDocument());
		return document.getCondition(conditionName);
	}

	private static boolean evaluateResolvedConditionExpression(@Nonnull Bean bean,
													   @Nonnull String expression,
													   boolean negated) {
		Boolean value = (Boolean) ExpressionEvaluator.evaluate(expression, bean);
		return negated ? Boolean.FALSE.equals(value) : Boolean.TRUE.equals(value);
	}

	public static @Nullable String negateCondition(@Nullable String condition) {
        String result = null;

        if (condition != null) {
	        if ("true".equals(condition)) {
	            result = "false";
	        }
	        else if ("false".equals(condition)) {
	            result = "true";
	        }
	        else if (condition.startsWith("not")) {
	            result = Introspector.decapitalize(condition.substring(3));
	        }
	        else {
	            StringBuilder sb = new StringBuilder(condition.length() + 3);
	            sb.append("not").append(Character.toUpperCase(condition.charAt(0)));
	            sb.append(condition.substring(1));
	            result = sb.toString();
	        }
        }
        
        return result;
	}
	
	/**
	 * Provides implicit conversions for types that do not require coercion, 
	 * that is they can be converted without input and without loss of precision.
	 * 
	 * @param type	The type to convert to
	 * @param value	The value to convert
	 * @return	A non-null converted value
	 */
	public static @Nonnull Object nullSafeConvert(@Nonnull Class<?> type, @Nonnull Object value) {
		if (isNumericWrapperType(type)) {
			return convertNumericWrapper(type, value);
		}
		if (isDecimalType(type)) {
			return convertDecimalType(type, value);
		}
		if (isTemporalType(type)) {
			return convertTemporalType(type, value);
		}
		if (Geometry.class.equals(type)) {
			return convertGeometryValue(value);
		}
		if (Enum.class.isAssignableFrom(type)) {
			return convertEnumValue(type, value);
		}

		return value;
	}

	private static boolean isNumericWrapperType(@Nonnull Class<?> type) {
		return type.equals(Integer.class) || type.equals(Long.class) || type.equals(Short.class) ||
				type.equals(Float.class) || type.equals(Double.class);
	}

	private static boolean isDecimalType(@Nonnull Class<?> type) {
		return type.equals(BigDecimal.class) || type.equals(Decimal2.class) ||
				type.equals(Decimal5.class) || type.equals(Decimal10.class);
	}

	private static boolean isTemporalType(@Nonnull Class<?> type) {
		return type.equals(DateOnly.class) || type.equals(TimeOnly.class) ||
				type.equals(DateTime.class) || type.equals(Timestamp.class);
	}

	private static @Nonnull Object convertNumericWrapper(@Nonnull Class<?> type, @Nonnull Object value) {
		if (! (value instanceof Number number)) {
			return value;
		}
		if (type.equals(Integer.class) && (! (value instanceof Integer))) {
			return Integer.valueOf(number.intValue());
		}
		if (type.equals(Long.class) && (! (value instanceof Long))) {
			return Long.valueOf(number.longValue());
		}
		if (type.equals(Short.class) && (! (value instanceof Short))) {
			return Short.valueOf(number.shortValue());
		}
		if (type.equals(Float.class) && (! (value instanceof Float))) {
			return Float.valueOf(number.floatValue());
		}
		if (type.equals(Double.class) && (! (value instanceof Double))) {
			return Double.valueOf(number.doubleValue());
		}

		return value;
	}

	private static @Nonnull Object convertDecimalType(@Nonnull Class<?> type, @Nonnull Object value) {
		if (type.equals(BigDecimal.class) && (! (value instanceof BigDecimal))) {
			return new BigDecimal(value.toString());
		}
		if (type.equals(Decimal2.class) && (! (value instanceof Decimal2))) {
			return new Decimal2(value.toString());
		}
		if (type.equals(Decimal5.class) && (! (value instanceof Decimal5))) {
			return new Decimal5(value.toString());
		}
		if (type.equals(Decimal10.class) && (! (value instanceof Decimal10))) {
			return new Decimal10(value.toString());
		}

		return value;
	}

	private static @Nonnull Object convertTemporalType(@Nonnull Class<?> type, @Nonnull Object value) {
		if (! (value instanceof Date date)) {
			return value;
		}
		if (type.equals(DateOnly.class) && (! (value instanceof DateOnly))) {
			return new DateOnly(date.getTime());
		}
		if (type.equals(TimeOnly.class) && (! (value instanceof TimeOnly))) {
			return new TimeOnly(date.getTime());
		}
		if (type.equals(DateTime.class) && (! (value instanceof DateTime))) {
			return new DateTime(date.getTime());
		}
		if (type.equals(Timestamp.class) && (! (value instanceof Timestamp))) {
			return new Timestamp(date.getTime());
		}

		return value;
	}

	private static @Nonnull Object convertGeometryValue(@Nonnull Object value) {
		if (! (value instanceof String string)) {
			return value;
		}
		try {
			return new WKTReader().read(string);
		}
		catch (org.locationtech.jts.io.ParseException e) {
			throw new DomainException(value + " is not valid WKT", e);
		}
	}

	private static @Nonnull Object convertEnumValue(@Nonnull Class<?> type, @Nonnull Object value) {
		if (value instanceof Enumeration enumeration) {
			return convertEnumValue(type, enumeration.toCode());
		}

		String stringValue = (value instanceof String string) ? string : value.toString();
		Object enumeration = resolveSkyveEnumeration(type, stringValue);
		if (enumeration == null) {
			enumeration = Enum.valueOf(type.asSubclass(Enum.class), stringValue);
		}
		if (enumeration == null) {
			throw new IllegalArgumentException(value + " is not a valid enumeration value");
		}

		return enumeration;
	}

	private static @Nullable Object resolveSkyveEnumeration(@Nonnull Class<?> type, @Nonnull String value) {
		Class<?>[] interfaces = type.getInterfaces();
		if ((interfaces.length != 1) || (! Enumeration.class.equals(interfaces[0]))) {
			return null;
		}
		try {
			Object enumeration = type.getMethod(Enumeration.FROM_CODE_METHOD_NAME, String.class).invoke(null, value);
			if (enumeration == null) {
				enumeration = type.getMethod(Enumeration.FROM_LOCALISED_DESCRIPTION_METHOD_NAME, String.class).invoke(null, value);
			}
			return enumeration;
		}
		catch (Exception e) {
			throw new DomainException(value + " is not a valid enumerated value in type " + type, e);
		}
	}

	/**
	 * Provides implicit conversions for types that do not require coercion, 
	 * that is they can be converted without input and without loss of precision.
	 * 
	 * @param type	The type to convert to
	 * @param value	The value to convert
	 * @return	A non-null converted value or null if value is null.
	 */
	public static @Nullable Object convert(@Nonnull Class<?> type, @Nullable Object value) {
		if (value == null) {
			return null;
		}
		return nullSafeConvert(type, value);
	}

	/**
	 * This method is synchronized as {@link Converter#fromDisplayValue(Object)} requires synchronization. 
	 * Explicit type coercion using the <code>converter</code> if supplied, or by java language coercion.
	 */
	public static synchronized @Nullable Object fromString(@Nullable Customer customer,
															@Nullable Converter<?> converter,
															@Nonnull Class<?> type,
															@Nonnull String stringValue) {
		return fromString(customer, converter, type, stringValue, false);
	}

	/**
	 * This method is synchronized as {@link Converter#fromDisplayValue(Object)} requires synchronization. 
	 * Explicit type coercion from serialised formats using the <code>converter</code> if supplied, or by java language coercion.
	 */
	public static synchronized @Nullable Object fromSerialised(@Nullable Converter<?> converter,
																@Nonnull Class<?> type,
																@Nonnull String stringValue) {
		return fromString(null, converter, type, stringValue, true);
	}

	/**
	 * This method is synchronized as {@link Converter#fromDisplayValue(Object)} requires synchronization. 
	 * Explicit type coercion from serialised formats using the <code>converter</code> if supplied, or by java language coercion.
	 */
	public static synchronized @Nullable Object fromSerialised(@Nonnull Class<?> type,
																@Nonnull String stringValue) {
		return fromString(null, null, type, stringValue, true);
	}

	private static @Nonnull Object fromString(@Nullable Customer customer,
												@Nullable Converter<?> converter,
												@Nonnull Class<?> type,
												@Nonnull String stringValue,
												boolean fromSerializedFormat) {
		try {
			return fromStringInternal(customer, converter, type, stringValue, fromSerializedFormat);
		}
		catch (Exception e) {
			if (e instanceof SkyveException se) {
				throw se;
			}
			throw new DomainException(e);
		}
	}

	private static @Nonnull Object fromStringInternal(@Nullable Customer customer,
														 @Nullable Converter<?> converter,
														 @Nonnull Class<?> type,
														 @Nonnull String stringValue,
														 boolean fromSerializedFormat) throws ParseException {
		if ((! fromSerializedFormat) && (converter != null)) {
			return converter.fromDisplayValue(stringValue);
		}
		if (fromSerializedFormat && (converter instanceof DynamicEnumerationConverter)) {
			return converter.fromDisplayValue(stringValue);
		}

		Object scalarValue = fromStringScalar(converter, type, stringValue);
		if (scalarValue != UNRESOLVED) {
			return scalarValue;
		}

		Object temporalValue = fromStringTemporal(customer, type, stringValue, fromSerializedFormat);
		if (temporalValue != UNRESOLVED) {
			return temporalValue;
		}

		if (Geometry.class.isAssignableFrom(type)) {
			return parseGeometryFromString(stringValue);
		}
		if (Date.class.isAssignableFrom(type)) {
			return new java.sql.Timestamp(CORE.getDateFormat(DEFAULT_DISPLAY_DATE_FORMAT).parse(stringValue).getTime());
		}
		if (type.equals(OptimisticLock.class)) {
			return new OptimisticLock(stringValue);
		}
		if (Enum.class.isAssignableFrom(type)) {
			return nullSafeConvert(type, stringValue);
		}

		throw new IllegalStateException("BindUtil.fromString() - Can't convert type " + type);
	}

	private static @Nonnull Object fromStringScalar(@Nullable Converter<?> converter,
													@Nonnull Class<?> type,
													@Nonnull String stringValue) {
		if (type.equals(String.class)) {
			return stringValue;
		}
		if (type.equals(Integer.class)) {
			return (converter != null) ? converter.fromDisplayValue(stringValue) : Integer.valueOf(stringValue);
		}
		if (type.equals(Long.class)) {
			return Long.valueOf(stringValue);
		}
		if (type.equals(Float.class)) {
			return Float.valueOf(stringValue);
		}
		if (type.equals(Double.class)) {
			return Double.valueOf(stringValue);
		}
		if (type.equals(BigDecimal.class)) {
			return new BigDecimal(stringValue);
		}
		if (type.equals(Decimal2.class)) {
			return new Decimal2(stringValue);
		}
		if (type.equals(Decimal5.class)) {
			return new Decimal5(stringValue);
		}
		if (type.equals(Decimal10.class)) {
			return new Decimal10(stringValue);
		}
		if (type.equals(Boolean.class)) {
			return Boolean.valueOf(stringValue.equals("true"));
		}

		return UNRESOLVED;
	}

	private static @Nonnull Object fromStringTemporal(@Nullable Customer customer,
													  @Nonnull Class<?> type,
													  @Nonnull String stringValue,
													  boolean fromSerializedFormat) throws ParseException {
		if (type.equals(DateOnly.class)) {
			return fromStringDateOnly(customer, stringValue, fromSerializedFormat);
		}
		if (type.equals(TimeOnly.class)) {
			return fromStringTimeOnly(customer, stringValue, fromSerializedFormat);
		}
		if (type.equals(DateTime.class)) {
			return fromStringDateTime(customer, stringValue, fromSerializedFormat);
		}
		if (type.equals(Timestamp.class)) {
			return fromStringTimestamp(customer, stringValue, fromSerializedFormat);
		}

		return UNRESOLVED;
	}

	private static @Nonnull Geometry parseGeometryFromString(@Nonnull String value) {
		try {
			return new WKTReader().read(value);
		}
		catch (org.locationtech.jts.io.ParseException e) {
			throw new DomainException(value + " is not valid WKT", e);
		}
	}

	private static @Nonnull Object fromStringDateOnly(@Nullable Customer customer,
													 @Nonnull String stringValue,
													 boolean fromSerializedFormat) throws ParseException {
		if (fromSerializedFormat) {
			return new DateOnly(stringValue);
		}
		if (customer != null) {
			Date date = customer.getDefaultDateConverter().fromDisplayValue(stringValue);
			return new DateOnly(date.getTime());
		}
		throw new IllegalStateException("BindUtil.fromString() - Can't convert " + stringValue + " to DateOnly");
	}

	private static @Nonnull Object fromStringTimeOnly(@Nullable Customer customer,
													 @Nonnull String stringValue,
													 boolean fromSerializedFormat) throws ParseException {
		if (fromSerializedFormat) {
			return new TimeOnly(stringValue);
		}
		if (customer != null) {
			Date date = customer.getDefaultTimeConverter().fromDisplayValue(stringValue);
			return new TimeOnly(date.getTime());
		}
		throw new IllegalStateException("BindUtil.fromString() - Can't convert " + stringValue + " to TimeOnly");
	}

	private static @Nonnull Object fromStringDateTime(@Nullable Customer customer,
													 @Nonnull String stringValue,
													 boolean fromSerializedFormat) throws ParseException {
		if (fromSerializedFormat) {
			return new DateTime(stringValue);
		}
		if (customer != null) {
			Date date = customer.getDefaultDateTimeConverter().fromDisplayValue(stringValue);
			return new DateTime(date.getTime());
		}
		throw new IllegalStateException("BindUtil.fromString() - Can't convert " + stringValue + " to DateTime");
	}

	private static @Nonnull Object fromStringTimestamp(@Nullable Customer customer,
													 @Nonnull String stringValue,
													 boolean fromSerializedFormat) throws ParseException {
		if (fromSerializedFormat) {
			return new Timestamp(stringValue);
		}
		if (customer != null) {
			Date date = customer.getDefaultTimestampConverter().fromDisplayValue(stringValue);
			return new Timestamp(date.getTime());
		}
		throw new IllegalStateException("BindUtil.fromString() - Can't convert " + stringValue + " to Timestamp");
	}

	public static @Nonnull String toDisplay(@Nonnull Customer customer, @Nullable Object value) {
		return toDisplay(customer, null, null, null, value);
	}
	
	/**
	 * This method is synchronized as {@link Converter#toDisplayValue(Object)} requires synchronization.
	 * 
	 * @param converter Can be <code>null</code>.
	 * @param object
	 * @return
	 */
	public static synchronized @Nonnull String toDisplay(@Nonnull Customer customer, 
															@Nullable @SuppressWarnings("rawtypes") Converter converter,
															@Nullable Class<?> implementingType,
															@Nullable List<DomainValue> domainValues, 
															@Nullable Object value) {
		try {
			return toDisplayInternal(customer, converter, implementingType, domainValues, value);
		}
		catch (Exception e) {
			if (e instanceof SkyveException se) {
				throw se;
			}

			throw new DomainException(e);
		}
	}

	private static @Nonnull String toDisplayInternal(@Nonnull Customer customer,
															  @Nullable @SuppressWarnings("rawtypes") Converter converter,
															  @Nullable Class<?> implementingType,
															  @Nullable List<DomainValue> domainValues,
															  @Nullable Object value) {
		if (value == null) {
			return "";
		}
		if (domainValues != null) {
			return toDomainDisplayValue(domainValues, value);
		}
		if ((converter != null) && (implementingType != null)) {
			return toConverterDisplayValue(converter, implementingType, value);
		}

		return toStandardDisplayValue(customer, value);
	}

	@SuppressWarnings({"rawtypes", "unchecked"})
	private static @Nonnull String toConverterDisplayValue(@Nonnull Converter converter,
														 @Nonnull Class<?> implementingType,
														 @Nonnull Object value) {
		return converter.toDisplayValue(nullSafeConvert(implementingType, value));
	}

	private static @Nonnull String toDomainDisplayValue(@Nonnull List<DomainValue> domainValues,
														 @Nonnull Object value) {
		if (value instanceof Enumeration enumeration) {
			return enumeration.toLocalisedDescription();
		}

		String codeValue = value.toString();
		for (DomainValue domainValue : domainValues) {
			if (domainValue.getCode().equals(codeValue)) {
				return domainValue.getLocalisedDescription();
			}
		}

		return codeValue;
	}

	private static @Nonnull String toStandardDisplayValue(@Nonnull Customer customer, @Nonnull Object value) {
		if (value instanceof DateOnly date) {
			return customer.getDefaultDateConverter().toDisplayValue(date);
		}
		if (value instanceof TimeOnly time) {
			return customer.getDefaultTimeConverter().toDisplayValue(time);
		}
		if (value instanceof DateTime date) {
			return customer.getDefaultDateTimeConverter().toDisplayValue(date);
		}
		if (value instanceof Timestamp time) {
			return customer.getDefaultTimestampConverter().toDisplayValue(time);
		}
		if (value instanceof Date date) {
			return CORE.getDateFormat(DEFAULT_DISPLAY_DATE_FORMAT).format(date);
		}
		if (value instanceof Boolean bool) {
			return bool.booleanValue() ? "Yes" : "No";
		}
		if (value instanceof Geometry geometry) {
			return new WKTWriter().write(geometry);
		}

		return value.toString();
	}

	public static @Nonnull String getDisplay(@Nonnull Customer customer,
												@Nonnull Bean bean,
												@Nonnull String binding) {
		return getDisplay(customer, bean, binding, get(bean, binding));
	}
	
	public static @Nonnull String getDisplay(@Nonnull Customer customer,
												@Nonnull Bean bean,
												@Nonnull String binding,
												@Nullable Object value) {
		if (value instanceof Bean b) {
			return b.getBizKey();
		}

		DisplaySettings displaySettings = resolveDisplaySettings(customer, bean, binding);
		return toDisplay(customer,
					displaySettings.converter,
					displaySettings.implementingType,
					displaySettings.domainValues,
					value);
	}

	private static @Nonnull DisplaySettings resolveDisplaySettings(@Nonnull Customer customer,
														   @Nonnull Bean bean,
														   @Nonnull String binding) {
		String documentName = bean.getBizDocument();
		if (documentName == null) {
			return DisplaySettings.EMPTY;
		}

		Module module = customer.getModule(bean.getBizModule());
		Document document = module.getDocument(customer, documentName);
		TargetMetaData target = resolveDisplayTarget(customer, module, document, binding);
		Attribute attribute = (target == null) ? null : target.getAttribute();
		Document targetDocument = (target == null) ? document : target.getDocument();
		if (! (attribute instanceof Field field)) {
			return DisplaySettings.EMPTY;
		}

		Converter<?> converter = (field instanceof ConvertibleField convertibleField)
				? convertibleField.getConverterForCustomer(customer)
				: null;
		Class<?> implementingType = field.getImplementingType();
		List<DomainValue> domainValues = resolveDisplayDomainValues(customer, bean, binding, targetDocument, field);
		return new DisplaySettings(converter, implementingType, domainValues);
	}

	private static @Nullable TargetMetaData resolveDisplayTarget(@Nonnull Customer customer,
														 @Nonnull Module module,
														 @Nonnull Document document,
														 @Nonnull String binding) {
		try {
			return BindUtil.getMetaDataForBinding(customer, module, document, binding);
		}
		catch (MetaDataException e) {
			if (LOGGER.isTraceEnabled()) {
				LOGGER.trace("Unable to resolve display target for binding {}", binding, e);
			}
			return null;
		}
	}

	private static @Nullable List<DomainValue> resolveDisplayDomainValues(@Nonnull Customer customer,
															@Nonnull Bean bean,
															@Nonnull String binding,
															@Nonnull Document document,
															@Nonnull Field field) {
		DomainType domainType = field.getDomainType();
		if (domainType == null) {
			return null;
		}

		DocumentImpl internalDocument = (DocumentImpl) document;
		if (! DomainType.dynamic.equals(domainType)) {
			return internalDocument.getDomainValues((CustomerImpl) customer, domainType, field, null, true);
		}

		Bean realBean = resolveDisplayRealBean(bean);
		if (realBean == null) {
			return null;
		}

		Bean owningBean = resolveOwningBeanForBinding(realBean, binding);
		if (owningBean == null) {
			return null;
		}

		return internalDocument.getDomainValues((CustomerImpl) customer, domainType, field, owningBean, true);
	}

	private static @Nullable Bean resolveDisplayRealBean(@Nonnull Bean bean) {
		if (! (bean instanceof DynamicBean dynamicBean)) {
			return bean;
		}
		if (! dynamicBean.isProperty(DynamicBean.BEAN_PROPERTY_KEY)) {
			return null;
		}

		return (Bean) dynamicBean.get(DynamicBean.BEAN_PROPERTY_KEY);
	}

	private static @Nullable Bean resolveOwningBeanForBinding(@Nonnull Bean realBean, @Nonnull String binding) {
		int lastDotIndex = binding.lastIndexOf('.');
		if (lastDotIndex < 0) {
			return realBean;
		}

		return (Bean) get(realBean, binding.substring(0, lastDotIndex));
	}

	private static final class DisplaySettings {
		private static final DisplaySettings EMPTY = new DisplaySettings(null, null, null);

		private final Converter<?> converter;
		private final Class<?> implementingType;
		private final List<DomainValue> domainValues;

		private DisplaySettings(@Nullable Converter<?> converter,
								   @Nullable Class<?> implementingType,
								   @Nullable List<DomainValue> domainValues) {
			this.converter = converter;
			this.implementingType = implementingType;
			this.domainValues = domainValues;
		}
	}

	/**
	 * Given a list of bindings, create a compound one. ie binding1.binding2.binding3 etc
	 * 
	 * @param bindings
	 * @return
	 */
	public static @Nonnull String createCompoundBinding(@Nonnull String... bindings) {
		StringBuilder result = new StringBuilder(64);

		for (String simpleBinding : bindings) {
			result.append(simpleBinding).append('.');
		}
		result.setLength(result.length() - 1);

		return result.toString();
	}

	/**
	 * Given a multiple cardinality binding and an index, create an indexed one. ie binding[index]
	 */
	public static @Nonnull String createIndexedBinding(@Nonnull String binding, int index) {
		StringBuilder result = new StringBuilder(64);

		result.append(binding).append('[').append(index).append(']');

		return result.toString();
	}

	/**
	 * Given a multiple cardinality binding and a bizId, create an element one. ie bindingElementById(id)
	 */
	public static @Nonnull String createIdBinding(@Nonnull String binding, @Nonnull String bizId) {
		StringBuilder result = new StringBuilder(64);

		result.append(binding).append(ELEMENT_BY_ID).append(bizId).append(')');

		return result.toString();
	}
	
	/**
	 * Replace '.', '[' & ']' with '_' to make valid client identifiers.
	 * Returns null if binding is null.
	 */
	public static @Nullable String sanitiseBinding(@Nullable String binding) {
		String result = null;
		if (binding != null) {
			result = binding.replace('.', '_').replace('[', '_').replace(']', '_');
		}
		return result;
	}

	/**
	 * Replace '_' with either '[', ']' or '_' depending on the context to make valid binding expressions from client identifiers.
	 */
	public static @Nullable String unsanitiseBinding(@Nullable String binding) {
		String result = null;
		if (binding != null) {
			result = binding.replaceAll("\\_(\\d*)\\_", "\\[$1\\]");
			result = result.replace('_', '.');
		}
		return result;
	}
	
	/**
	 * Check to see if the element is in the list. If not, then add it.
	 * 
	 * @param owner
	 * @param binding
	 * @param element
	 * @return	The found or added element.
	 */
	@SuppressWarnings("unchecked")
	public static @Nonnull Bean ensureElementIsInCollection(@Nonnull Bean owner, 
																@Nonnull String binding,
																@Nonnull Bean element) {
		List<Bean> list = (List<Bean>) get(owner, binding);
		if (list != null) {
			String elementId = element.getBizId();
	
			// check each bean in the list to see if its ID is the same
			for (Bean existing : list) {
				if (elementId.equals(existing.getBizId())) {
					return existing;
				}
			}
		}
		
		BindUtil.addElementToCollection(owner, binding, element);
		return element;
	}

	/**
	 * Return the element found or null if not found.
	 * 
	 * @param owner
	 * @param binding
	 * @param elementBizId
	 * @return	The found element or null.
	 */
	@SuppressWarnings("unchecked")
	public static @Nullable Bean getElementInCollection(@Nonnull Bean owner,
															@Nonnull String binding,
															@Nonnull String elementBizId) {
		List<Bean> list = (List<Bean>) get(owner, binding);
		if (list == null) {
			return null;
		}
		return getElementInCollection(list, elementBizId);
	}

	public static @Nullable <T extends Bean> T getElementInCollection(@Nonnull List<T> list, @Nonnull String elementBizId) {
		// check each bean in the list to see if its ID is the same
		for (T existing : list) {
			if (elementBizId.equals(existing.getBizId())) {
				return existing;
			}
		}

		return null;
	}
	
	public static <T extends Bean> void setElementInCollection(@Nonnull List<T> list, @Nonnull T element) {
		// Set each occurrence in the list where the bizIds are the same
		String elementBizId = element.getBizId();
		for (int i = 0, l = list.size(); i < l; i++) {
			if (elementBizId.equals(list.get(i).getBizId())) {
				list.set(i, element);
			}
		}
	}
	
	private static @Nonnull Pair<Bean, String> penultimateBind(@Nonnull Bean bean, @Nonnull String relationBinding) {
		Bean relationOwner = bean;
		String relationName = relationBinding;
		int dotIndex = relationName.lastIndexOf('.');
		if (dotIndex > -1) {
			relationName = relationName.substring(dotIndex + 1);
			relationOwner = (Bean) BindUtil.get(bean, relationBinding.substring(0, dotIndex));
		}
		return new ImmutablePair<>(relationOwner, relationName);
	}
	
	private static void setElementParent(@Nonnull Customer customer,
											@Nonnull Module module,
											@Nonnull Document document,
											@Nonnull Relation relation,
											@Nullable Bean element,
											@Nullable Bean parent) {
		// Set the parent of a child bean, if applicable
		if (! (element instanceof ChildBean<?>)) {
			return;
		}

		Document parentDocument = getRelatedParentDocument(customer, module, relation);
		if ((parentDocument != null) && canAssignParentDocument(customer, document, parentDocument)) {
			@SuppressWarnings("unchecked")
			ChildBean<Bean> uncheckedNewBean = (ChildBean<Bean>) element;
			uncheckedNewBean.setParent(parent);
		}
	}

	private static @Nullable Document getRelatedParentDocument(@Nonnull Customer customer,
													   @Nonnull Module module,
													   @Nonnull Relation relation) {
		Document relatedDocument = module.getDocument(customer, relation.getDocumentName());
		return relatedDocument.getParentDocument(customer);
	}

	private static boolean canAssignParentDocument(@Nonnull Customer customer,
											 @Nonnull Document document,
											 @Nonnull Document parentDocument) {
		String parentModuleName = parentDocument.getOwningModuleName();
		String parentDocumentName = parentDocument.getName();
		CustomerImpl internalCustomer = (CustomerImpl) customer;
		Document parentBeanDocument = document;
		while (parentBeanDocument != null) {
			if (parentModuleName.equals(parentBeanDocument.getOwningModuleName()) &&
					parentDocumentName.equals(parentBeanDocument.getName())) {
				return true;
			}
			parentBeanDocument = getBaseDocument(customer, internalCustomer, parentBeanDocument);
		}

		return false;
	}

	private static @Nullable Document getBaseDocument(@Nonnull Customer customer,
											 @Nonnull CustomerImpl internalCustomer,
											 @Nonnull Document document) {
		String baseDocumentName = internalCustomer.getBaseDocument(document);
		if (baseDocumentName == null) {
			return null;
		}

		int dotIndex = baseDocumentName.indexOf('.');
		Module baseModule = customer.getModule(baseDocumentName.substring(0, dotIndex));
		return baseModule.getDocument(customer, baseDocumentName.substring(dotIndex + 1));
	}
	
	private static void setRelationInverse(@Nonnull Customer customer,
											@Nonnull Document document,
											@Nonnull Relation relation,
											@Nonnull Bean owner,
											@Nonnull String relationName,
											@Nonnull Bean value,
											boolean remove) {
		String attributeName = null;
		if (relation instanceof Inverse inverse) { // we just set the inverse
			attributeName = inverse.getReferenceName();
		}
		else { // lets see if there is an inverse on the element side
			attributeName = findInverseAttributeName(customer, document, relationName, value);
		}
		
		if (attributeName != null) {
			Object inverseValue = get(value, attributeName);
			if (inverseValue instanceof List<?>) {
				@SuppressWarnings("unchecked")
				List<Bean> collection = (List<Bean>) inverseValue;
				if (remove) {
					collection.remove(owner);
				}
				else {
					collection.add(owner);
				}
			}
			else {
				set(value, attributeName, remove ? null : owner);
			}
		}
	}

	private static @Nullable String findInverseAttributeName(@Nonnull Customer customer,
													  @Nonnull Document document,
													  @Nonnull String relationName,
													  @Nonnull Bean value) {
		Module elementModule = customer.getModule(value.getBizModule());
		Document elementDocument = elementModule.getDocument(customer, value.getBizDocument());
		String ownerDocumentName = document.getName();
		for (Attribute attribute : elementDocument.getAllAttributes(customer)) {
			if ((attribute instanceof Inverse inverse) &&
					ownerDocumentName.equals(inverse.getDocumentName()) &&
					relationName.equals(inverse.getReferenceName())) {
				return inverse.getName();
			}
		}

		return null;
	}
	
	/**
	 * Call a Bean's association setter method.
	 * @param bean	The owning bean.
	 * @param associationBinding	The binding to the association.
	 * @param value	The value to set.
	 */
	public static void setAssociation(@Nonnull Bean bean,
										@Nonnull String associationBinding,
										@Nullable Bean value) {
		try {
			Pair<Bean, String> args = penultimateBind(bean, associationBinding);
			Bean associationOwner = args.getLeft();
			String associationName = args.getRight();
			
			Customer c = CORE.getCustomer();
			Module m = c.getModule(associationOwner.getBizModule());
			Document d = m.getDocument(c, associationOwner.getBizDocument());
			Attribute a = d.getPolymorphicAttribute(c, associationName);
			if (a == null) {
				throw new MetaDataException("Association binding " + associationBinding + " is invalid");
			}

			// Dynamic association - make it happen here with no convenience method
			if (BindUtil.isDynamic(c, m, d, a)) {
				Relation r = (Relation) a;
				Bean oldValue = (Bean) BindUtil.get(associationOwner, associationName);
				if (oldValue != null) {
					setRelationInverse(c, d, r, associationOwner, associationName, oldValue, true);
				}
				if (value != null) {
					setRelationInverse(c, d, r, associationOwner, associationName, value, false);
				}
			}

			// Static or Dynamic association - use set
			set(associationOwner, associationName, value);
		}
		catch (Exception e) {
			if (e instanceof SkyveException se) {
				throw se;
			}
			throw new DomainException(e);
		}
		
	}
	
	/**
	 * Call the addElement method on a Bean's collection.
	 * @param bean	The owning bean.
	 * @param collectionBinding	The binding to the collection.
	 * @param element	The element.
	 */
	public static boolean addElementToCollection(@Nonnull Bean bean,
													@Nonnull String collectionBinding,
													@Nonnull Bean element) {
		try {
			Pair<Bean, String> args = penultimateBind(bean, collectionBinding);
			Bean collectionOwner = args.getLeft();
			String collectionName = args.getRight();

			Customer c = CORE.getCustomer();
			Module m = c.getModule(collectionOwner.getBizModule());
			Document d = m.getDocument(c, collectionOwner.getBizDocument());
			Attribute a = d.getPolymorphicAttribute(c, collectionName);
			if (a == null) {
				throw new MetaDataException("Collection binding " + collectionBinding + " is invalid");
			}

			// Dynamic collection - make it happen here with no convenience method
			if (BindUtil.isDynamic(c, m, d, a)) {
				Relation r = (Relation) a;
				setElementParent(c, m, d, r, element, collectionOwner);
				setRelationInverse(c, d, r, collectionOwner, collectionName, element, false);
				@SuppressWarnings("unchecked")
				List<Bean> list = (List<Bean>) BindUtil.get(collectionOwner, collectionName);
				if (list == null) {
					return false;
				}
				return list.add(element);
			}

			// Static collection - use the add method
			StringBuilder sb = new StringBuilder(collectionName.length() + 10);
			sb.append("add").append(Character.toUpperCase(collectionName.charAt(0))).append(collectionName.substring(1)).append(ELEMENT);
			String methodName = sb.toString();
			
			Method[] methods = collectionOwner.getClass().getMethods(); 
			for (Method method : methods) {
				if (methodName.equals(method.getName()) && (method.getParameterTypes().length == 1)) {
					Object result = method.invoke(collectionOwner, element);
					return Boolean.TRUE.equals(result);
				}
			}
			
			throw new IllegalStateException("Method " + methodName + " not found on " + collectionOwner);
		}
		catch (Exception e) {
			if (e instanceof SkyveException skyveException) {
				throw skyveException;
			}
			throw new DomainException(e);
		}
	}

	/**
	 * Call the addElement method on a Bean's collection.
	 * @param bean	The owning bean.
	 * @param collectionBinding	The binding to the collection.
	 * @param index	The index to add the element at.
	 * @param element	The element.
	 */
	public static void addElementToCollection(@Nonnull Bean bean,
												@Nonnull String collectionBinding,
												int index,
												@Nonnull Bean element) {
		try {
			Pair<Bean, String> args = penultimateBind(bean, collectionBinding);
			Bean collectionOwner = args.getLeft();
			String collectionName = args.getRight();

			Customer c = CORE.getCustomer();
			Module m = c.getModule(collectionOwner.getBizModule());
			Document d = m.getDocument(c, collectionOwner.getBizDocument());
			Attribute a = d.getPolymorphicAttribute(c, collectionName);
			if (a == null) {
				throw new MetaDataException("Collection binding " + collectionBinding + " is invalid");
			}

			// Dynamic collection - make it happen here with no convenience method
			if (BindUtil.isDynamic(c, m, d, a)) {
				Relation r = (Relation) a;
				setElementParent(c, m, d, r, element, collectionOwner);
				setRelationInverse(c, d, r, collectionOwner, collectionName, element, false);
				@SuppressWarnings("unchecked")
				List<Bean> list = (List<Bean>) BindUtil.get(collectionOwner, collectionName);
				if (list != null) {
					list.add(index, element);
				}
				return;
			}

			// Static collection - use the add method
			StringBuilder sb = new StringBuilder(collectionName.length() + 10);
			sb.append("add").append(Character.toUpperCase(collectionName.charAt(0))).append(collectionName.substring(1)).append(ELEMENT);
			String methodName = sb.toString();
			
			Method[] methods = collectionOwner.getClass().getMethods(); 
			for (Method method : methods) {
				if (methodName.equals(method.getName()) && (method.getParameterTypes().length == 2)) {
					method.invoke(collectionOwner, Integer.valueOf(index), element);
					return;
				}
			}
			
			throw new IllegalStateException("Method " + methodName + " not found on " + collectionOwner);
		}
		catch (Exception e) {
			if (e instanceof SkyveException skyveException) {
				throw skyveException;
			}
			throw new DomainException(e);
		}
	}

	/**
	 * Call the removeElement method on a Bean's collection.
	 * @param bean	The owning bean.
	 * @param collectionBinding	The binding to the collection.
	 * @param element	The element.
	 */
	public static boolean removeElementFromCollection(@Nonnull Bean bean,
														@Nonnull String collectionBinding,
														@Nonnull Bean element) {
		try {
			Pair<Bean, String> args = penultimateBind(bean, collectionBinding);
			Bean collectionOwner = args.getLeft();
			String collectionName = args.getRight();

			Customer c = CORE.getCustomer();
			Module m = c.getModule(collectionOwner.getBizModule());
			Document d = m.getDocument(c, collectionOwner.getBizDocument());
			Attribute a = d.getPolymorphicAttribute(c, collectionName);
			if (a == null) {
				throw new MetaDataException("Collection binding " + collectionBinding + " is invalid");
			}

			// Dynamic collection - make it happen here with no convenience method
			if (BindUtil.isDynamic(c, m, d, a)) {
				Relation r = (Relation) a;
				setElementParent(c, m, d, r, element, null);
				setRelationInverse(c, d, r, collectionOwner, collectionName, element, true);
				@SuppressWarnings("unchecked")
				List<Bean> list = (List<Bean>) BindUtil.get(collectionOwner, collectionName);
				if (list == null) {
					return false;
				}
				return list.remove(element);
			}

			// Static collection - use the remove method
			StringBuilder sb = new StringBuilder(collectionName.length() + 13);
			sb.append("remove").append(Character.toUpperCase(collectionName.charAt(0))).append(collectionName.substring(1)).append(ELEMENT);
			String methodName = sb.toString();
			
			Method[] methods = collectionOwner.getClass().getMethods(); 
			for (Method method : methods) {
				if (methodName.equals(method.getName())) {
					Class<?>[] pts = method.getParameterTypes();
					// Skim over remove element by index method
					if ((pts.length == 1) && (! Integer.TYPE.equals(pts[0]))) {
						Object result = method.invoke(collectionOwner, element);
						return Boolean.TRUE.equals(result);
					}
				}
			}
			
			throw new IllegalStateException("Method " + methodName + " not found on " + collectionOwner);
		}
		catch (Exception e) {
			if (e instanceof SkyveException skyveException) {
				throw skyveException;
			}
			throw new DomainException(e);
		}
	}

	/**
	 * Call the removeElement method on a Bean's collection.
	 * @param bean	The owning bean.
	 * @param collectionBinding	The binding to the collection.
	 * @param index	The index to add the element at.
	 * @return	The removed element.
	 */
	public static @Nonnull <T extends Bean> T removeElementFromCollection(@Nonnull Bean bean,
																			@Nonnull String collectionBinding,
																			int index) {
		try {
			Pair<Bean, String> args = penultimateBind(bean, collectionBinding);
			Bean collectionOwner = args.getLeft();
			String collectionName = args.getRight();

			Customer c = CORE.getCustomer();
			Module m = c.getModule(collectionOwner.getBizModule());
			Document d = m.getDocument(c, collectionOwner.getBizDocument());
			Attribute a = d.getPolymorphicAttribute(c, collectionName);
			if (a == null) {
				throw new MetaDataException("Collection binding " + collectionBinding + " is invalid");
			}
			
			// Dynamic collection - make it happen here with no convenience method
			if (BindUtil.isDynamic(c, m, d, a)) {
				@SuppressWarnings("unchecked")
				List<T> list = (List<T>) BindUtil.get(collectionOwner, collectionName);
				// This should never happen
				if (list == null) {
					throw new IllegalStateException(collectionBinding + " points to a collection that is null");
				}
				T element = list.get(index);
				Relation r = (Relation) a;
				setElementParent(c, m, d, r, element, null);
				setRelationInverse(c, d, r, collectionOwner, collectionName, element, true);
				return list.remove(index);
			}

			// Static collection - use the remove method
			StringBuilder methodName = new StringBuilder(collectionName.length() + 13);
			methodName.append("remove").append(Character.toUpperCase(collectionName.charAt(0))).append(collectionName.substring(1)).append(ELEMENT);
			Method method = collectionOwner.getClass().getMethod(methodName.toString(), Integer.TYPE);
			@SuppressWarnings("unchecked")
			T result = (T) method.invoke(collectionOwner, Integer.valueOf(index));
			return result;
		}
		catch (Exception e) {
			if (e instanceof SkyveException se) {
				throw se;
			}
			throw new DomainException(e);
		}
	}

	/**
	 * Order a collection/inverseMany by its order metadata.
	 * 
	 * @param bean The bean that ultimately has the collection/inverse.
	 * @param binding The (possibly compound) collection/inverse binding.
	 */
	public static void orderByMetaData(@Nonnull Bean bean, @Nonnull String binding) {
		// Cater for compound bindings here
		Bean owningBean = bean;
		int lastDotIndex = binding.lastIndexOf('.'); // compound binding
		if (lastDotIndex > -1) {
			owningBean = (Bean) BindUtil.get(owningBean, binding.substring(0, lastDotIndex));
			if (owningBean == null) {
				return;
			}
		}
		
		// Order the collection / inverseMany
		Customer c = CORE.getCustomer();
		Module m = owningBean.getModuleMetaData();
		Document d = m.getDocument(c, owningBean.getBizDocument());
		TargetMetaData target = BindUtil.getMetaDataForBinding(c, m, d, (lastDotIndex < 0) ? binding : binding.substring(lastDotIndex + 1));
		Attribute targetAttribute = target.getAttribute();
		if (targetAttribute instanceof Collection collection) {
			orderCollectionByMetaData(owningBean, collection);
		}
		else if (targetAttribute instanceof InverseMany inverse) {
			orderInverseManyByMetaData(owningBean, inverse);
		}
	}

	/**
	 * Order the beans collection by its order metadata.
	 * 
	 * @param owningBean The bean with collection in it.
	 * @param collection The metadata representing the collection. 
	 * 						This method does not cater for compound binding expressions.
	 * 						Use {@link orderByMetaData(Bean, String)} for that.
	 */
	public static void orderCollectionByMetaData(@Nonnull Bean owningBean, @Nonnull Collection collection) {
		// We only sort by ordinal if this is a child collection as bizOrdinal is on the elements.
		// For aggregation/composition, bizOrdinal is on the joining table and handled automatically
		boolean sortByOrdinal = Boolean.TRUE.equals(collection.getOrdered()) && 
														(CollectionType.child.equals(collection.getType()));
		List<Ordering> ordering = collection.getOrdering();
		if (sortByOrdinal || (! ordering.isEmpty())) {
			@SuppressWarnings("unchecked")
			List<Bean> details = (List<Bean>) BindUtil.get(owningBean, collection.getName());
			order(details, sortByOrdinal, ordering);
		}
	}
	
	/**
	 * Order the beans inverseMany by its order metadata.
	 * 
	 * @param owningBean The bean with collection in it.
	 * @param inverse The metadata representing the inverseMany. 
	 * 					This method does not cater for compound binding expressions.
	 * 					Use {@link orderByMetaData(Bean, String)} for that.
	 */
	public static void orderInverseManyByMetaData(@Nonnull Bean owningBean, @Nonnull InverseMany inverse) {
		List<Ordering> ordering = inverse.getOrdering();
		if (! ordering.isEmpty()) {
			@SuppressWarnings("unchecked")
			List<Bean> details = (List<Bean>) BindUtil.get(owningBean, inverse.getName());
			order(details, false, ordering);
		}
	}
	
	/**
	 * Order a List of java beans by an arbitrary ordering.
	 * The list can be of any type, not just Bean.
	 * 
	 * @param beans	The list to order
	 * @param ordering The ordering
	 */
	public static void order(@Nullable List<?> beans, @Nonnull Ordering... ordering) {
		order(beans, false, Arrays.asList(ordering));
	}
	
	@SuppressWarnings("unchecked")
	private static void order(@Nullable List<?> beans, 
								boolean finallySortByOrdinal, 
								@Nonnull List<Ordering> ordering) {
		if (beans != null) {
			ComparatorChain comparatorChain = new ComparatorChain();
			if (finallySortByOrdinal) {
				comparatorChain.addComparator(new NullTolerantBeanComparator(Bean.ORDINAL_NAME), false);
			}
			for (Ordering order : ordering) {
				comparatorChain.addComparator(new NullTolerantBeanComparator(order.getBy()),
												SortDirection.descending.equals(order.getSort()));
			}
			
			// Test if the collection is sorted before sorting as 
			// Collections.sort() will affect the dirtiness of a hibernate collection
			boolean unsorted = false;
			Object smallerBean = null;
			for (Object bean : beans) {
				if ((smallerBean != null) && (comparatorChain.compare(smallerBean, bean) > 0)) {
					unsorted = true;
					break;
				}
				smallerBean = bean;
			}
			if (unsorted) {
				Collections.sort(beans, comparatorChain);
			}
		}
	}

	/**
	 * Get a simple or compound <code>bean</code> property value.
	 * 
	 * @param bean The bean to get the property value from.
	 * @param binding The fully qualified name of a bean property, separating components with a '.'. 
	 * 					Examples would be "identifier" (simple) or "identifier.clientId" (compound).
	 */
	public static @Nullable Object get(@Nonnull Object bean, @Nonnull String binding) {
		if ((bean instanceof DynamicBean dynamic) && dynamic.isProperty(binding)) {
			return dynamic.get(binding);
		}

		Object result = null;
		Object currentBean = bean;
		StringTokenizer tokenizer = new StringTokenizer(binding, ".");
		while (tokenizer.hasMoreTokens()) {
			String simpleBinding = tokenizer.nextToken();
			try {
				result = resolveBoundValue(currentBean, simpleBinding);
			}
			catch (Exception e) {
				LOGGER.error("Could not BindUtil.get({}, {})!", bean, binding);
				LOGGER.error("The subsequent stack trace relates to obtaining bean property {} from {}", simpleBinding, currentBean);
				LOGGER.error("If the stack trace contains something like \"Unknown property '{}\' on class 'class <blahblah>$$EnhancerByCGLIB$$$<blahblah>'\"  then you'll need to use Util.deproxy() before trying to bind to properties in the hibernate proxy.", simpleBinding); 
				LOGGER.error("See https://github.com/skyvers/skyve-cookbook/blob/master/README.md#deproxy for details");
				LOGGER.error("Exception message = {}", e.getMessage());
				throw new MetaDataException(e);
			}

			if (result == null) {
				break;
			}

			currentBean = result;
		}

		return result;
	}

	private static @Nullable Object resolveBoundValue(@Nonnull Object currentBean,
													 @Nonnull String simpleBinding)
			throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {
		if (currentBean instanceof Bean bean) {
			return resolveBeanBoundValue(bean, currentBean, simpleBinding);
		}

		return PROPERTY_UTILS.getProperty(currentBean, simpleBinding);
	}

	private static @Nullable Object resolveBeanBoundValue(@Nonnull Bean bean,
													 @Nonnull Object currentBean,
													 @Nonnull String simpleBinding)
				throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {
		String attributeName = getDynamicAttributeName(simpleBinding);
		if (! bean.isDynamic(attributeName)) {
			return PROPERTY_UTILS.getProperty(currentBean, simpleBinding);
		}

		Object result = bean.getDynamic(attributeName);
		return resolveDynamicBindingValue(simpleBinding, result);
	}

	private static @Nonnull String getDynamicAttributeName(@Nonnull String simpleBinding) {
		int braceIndex = getDynamicBindingIndex(simpleBinding);
		return (braceIndex >= 0) ? simpleBinding.substring(0, braceIndex) : simpleBinding;
	}

	private static int getDynamicBindingIndex(@Nonnull String simpleBinding) {
		int braceIndex = simpleBinding.indexOf('[');
		return (braceIndex >= 0) ? braceIndex : simpleBinding.indexOf(ELEMENT_BY_ID);
	}

	private static @Nullable Object resolveDynamicBindingValue(@Nonnull String simpleBinding,
													  @Nullable Object result) {
		int braceIndex = getDynamicBindingIndex(simpleBinding);
		if ((result == null) || (braceIndex <= 0)) {
			return result;
		}

		@SuppressWarnings("unchecked")
		List<? extends Bean> list = (List<? extends Bean>) result;
		return isIndexedDynamicBinding(simpleBinding)
				? list.get(Integer.parseInt(simpleBinding.substring(braceIndex + 1, simpleBinding.length() - 1)))
				: getDynamicElementById(simpleBinding, braceIndex, list);
	}

	private static boolean isIndexedDynamicBinding(@Nonnull String simpleBinding) {
		return simpleBinding.indexOf('[') >= 0;
	}

	private static @Nullable Bean getDynamicElementById(@Nonnull String simpleBinding,
													 int braceIndex,
													 @Nonnull List<? extends Bean> list) {
		String bizId = simpleBinding.substring(braceIndex + ELEMENT_BY_ID.length(), simpleBinding.length() - 1);
		return list.stream().filter(element -> bizId.equals(element.getBizId())).findFirst().orElse(null);
	}

	@SuppressWarnings("unchecked")
	public static Object get(@Nonnull Map<String, Object> map, @Nonnull String binding) {
		String alias = sanitiseBinding(binding);
		Object result = null;
		if (map.containsKey(binding)) {
			result = map.get(binding);
		}
		else if (map.containsKey(alias)) {
			result = map.get(alias);
		}
		else {
			Object currentMap = map;
			StringTokenizer tokenizer = new StringTokenizer(binding, ".");
			while (tokenizer.hasMoreTokens()) {
				String simpleKey = tokenizer.nextToken();
				if (currentMap instanceof Map<?, ?>) {
					result = ((Map<String, Object>) currentMap).get(simpleKey);
				}
	
				if (result == null) {
					break;
				}
	
				currentMap = result;
			}
		}
		
		return result;
	}
	
	public static void convertAndSet(@Nonnull Object bean, @Nonnull String binding, @Nullable Object value) {
		Class<?> type = getPropertyType(bean, binding);
		set(bean, binding, convert(type, value));
	}

	/**
	 * Set a simple or compound <code>bean</code> property value.
	 * 
	 * @param bean The bean to set the property value in.
	 * @param value The value to the bean property value to.
	 * @param binding The fully qualified name of a bean property, separating components with a '.'. 
	 * 					Examples would be "identifier" (simple) or "identifier.clientId" (compound).
	 */
	public static void set(@Nonnull Object bean, @Nonnull String binding, @Nullable Object value) {
		try {
			Object valueToSet = normaliseBoundValue(value);
	
			if ((bean instanceof DynamicBean b) && b.isProperty(binding)) {
				b.set(binding, valueToSet);
			}
			else {
				Pair<Object, String> target = resolvePenultimateTarget(bean, binding);
				Object penultimate = target.getLeft();
				String simpleBinding = target.getRight();
				if (penultimate == null) {
					throw new MetaDataException("Binding " + binding + " does not resolve to a target bean.");
				}

				if (valueToSet != null) {
					valueToSet = coerceValueForProperty(bean, binding, penultimate, simpleBinding, valueToSet);
				}

				setResolvedValue(bean, binding, penultimate, simpleBinding, valueToSet);
			}
		}
		catch (Exception e) {
			if (e instanceof SkyveException se) {
				throw se;
			}
			
			throw new MetaDataException(e);
		}
	}

	private static @Nullable Object normaliseBoundValue(@Nullable Object value) {
		return "".equals(value) ? null : value;
	}

	private static @Nonnull Pair<Object, String> resolvePenultimateTarget(@Nonnull Object bean, @Nonnull String binding) {
		int lastDotIndex = binding.lastIndexOf('.');
		if (lastDotIndex < 0) {
			return new ImmutablePair<>(bean, binding);
		}

		Object penultimate = get(bean, binding.substring(0, lastDotIndex));
		String simpleBinding = binding.substring(lastDotIndex + 1);
		return new ImmutablePair<>(penultimate, simpleBinding);
	}

	private static @Nonnull Object coerceValueForProperty(@Nonnull Object bean,
													 @Nonnull String binding,
													 @Nonnull Object penultimate,
													 @Nonnull String simpleBinding,
													 @Nonnull Object valueToSet)
			throws ReflectiveOperationException {
		Class<?> propertyType = BindUtil.getPropertyType(penultimate, simpleBinding);
		Object result = coerceStringValue(propertyType, valueToSet, binding, bean);
		return String.class.equals(propertyType) ? result.toString() : result;
	}

	private static @Nonnull Object coerceStringValue(@Nonnull Class<?> propertyType,
													 @Nonnull Object valueToSet,
													 @Nonnull String binding,
													 @Nonnull Object bean)
			throws ReflectiveOperationException {
		if ((! String.class.equals(valueToSet.getClass())) || String.class.equals(propertyType) || Object.class.equals(propertyType)) {
			return valueToSet;
		}

		try {
			return propertyType.getConstructor(valueToSet.getClass()).newInstance(valueToSet);
		}
		catch (@SuppressWarnings("unused") NoSuchMethodException e) {
			return invokeStringValueOf(propertyType, valueToSet, binding, bean);
		}
	}

	private static @Nonnull Object invokeStringValueOf(@Nonnull Class<?> propertyType,
													  @Nonnull Object valueToSet,
													  @Nonnull String binding,
													  @Nonnull Object bean)
			throws IllegalAccessException, InvocationTargetException {
		try {
			return propertyType.getMethod("valueOf", String.class).invoke(null, valueToSet);
		}
		catch (@SuppressWarnings("unused") NoSuchMethodException e) {
			throw new DomainException("Cannot coerce String value " + valueToSet + " to type " + propertyType + " for setting for binding " + binding + " on bean " + bean);
		}
	}

	private static void setResolvedValue(@Nonnull Object bean,
												 @Nonnull String binding,
												 @Nonnull Object penultimate,
												 @Nonnull String simpleBinding,
												 @Nullable Object valueToSet)
			throws ReflectiveOperationException {
		if (penultimate instanceof Bean targetBean) {
			String attributeName = getDynamicAttributeName(simpleBinding);
			int braceIndex = getDynamicBindingIndex(simpleBinding);
			if (targetBean.isDynamic(attributeName)) {
				setDynamicResolvedValue(bean, binding, targetBean, simpleBinding, attributeName, braceIndex, valueToSet);
				return;
			}
		}

		PROPERTY_UTILS.setProperty(penultimate, simpleBinding, valueToSet);
	}

	private static void setDynamicResolvedValue(@Nonnull Object bean,
													 @Nonnull String binding,
													 @Nonnull Bean targetBean,
													 @Nonnull String simpleBinding,
													 @Nonnull String attributeName,
													 int braceIndex,
													 @Nullable Object valueToSet) {
		if (braceIndex < 0) {
			targetBean.setDynamic(simpleBinding, valueToSet);
			return;
		}
		if (isIndexedDynamicBinding(simpleBinding)) {
			setIndexedDynamicValue(bean, binding, targetBean, simpleBinding, attributeName, valueToSet);
			return;
		}
		setDynamicElementByIdValue(bean, binding, targetBean, simpleBinding, attributeName, valueToSet);
	}

	private static void setIndexedDynamicValue(@Nonnull Object bean,
												  @Nonnull String binding,
												  @Nonnull Bean targetBean,
												  @Nonnull String simpleBinding,
												  @Nonnull String attributeName,
												  @Nullable Object valueToSet) {
		@SuppressWarnings("unchecked")
		List<Object> list = (List<Object>) targetBean.getDynamic(attributeName);
		if (list == null) {
			throw new IllegalStateException("Attempt to set " + binding + " in " + bean + " to " + valueToSet + " but the list is null");
		}

		int braceIndex = simpleBinding.indexOf('[');
		int index = Integer.parseInt(simpleBinding.substring(braceIndex + 1, simpleBinding.length() - 1));
		list.set(index, valueToSet);
	}

	private static void setDynamicElementByIdValue(@Nonnull Object bean,
													 @Nonnull String binding,
													 @Nonnull Bean targetBean,
													 @Nonnull String simpleBinding,
													 @Nonnull String attributeName,
													 @Nullable Object valueToSet) {
		if (! (valueToSet instanceof Bean beanToSet)) {
			throw new IllegalStateException("Attempt to set " + binding + " in " + bean + " to " + valueToSet + " but valueToSet should be a Bean");
		}

		@SuppressWarnings("unchecked")
		List<Bean> list = (List<Bean>) targetBean.getDynamic(attributeName);
		if (list == null) {
			throw new IllegalStateException("Attempt to set " + binding + " in " + bean + " to " + valueToSet + " but the list is null");
		}

		String bizId = extractDynamicElementBizId(simpleBinding);
		Bean result = list.stream().filter(element -> bizId.equals(element.getBizId())).findFirst().orElse(null);
		if (result == null) {
			throw new IllegalStateException("Attempt to set " + binding + " in " + bean + " to " + valueToSet + " but the element was not in the list");
		}

		list.set(list.indexOf(result), beanToSet);
	}

	private static @Nonnull String extractDynamicElementBizId(@Nonnull String simpleBinding) {
		int braceIndex = simpleBinding.indexOf(ELEMENT_BY_ID);
		return simpleBinding.substring(braceIndex + ELEMENT_BY_ID.length(), simpleBinding.length() - 1);
	}

	public static @Nonnull Class<?> getPropertyType(@Nonnull Object bean, @Nonnull String binding) {
		if (bean instanceof Bean b) {
			int lastDotIndex = binding.lastIndexOf('.');
			if (lastDotIndex < 0) {
				String attributeName = resolveDynamicPropertyName(b, binding);
				if (attributeName != null) {
					return resolveDynamicPropertyType(b, binding, attributeName);
				}
			}

			// Could be a compound, indexed or mapped binding if we reach here
			if (lastDotIndex >= 0) { // compound binding
				return resolveCompoundPropertyType(bean, binding, lastDotIndex);
			}
		}

		try {
			return PROPERTY_UTILS.getPropertyType(bean, binding);
		}
		catch (Exception e) {
			throw new MetaDataException(e);
		}
	}

	private static @Nullable String resolveDynamicPropertyName(@Nonnull Bean bean, @Nonnull String binding) {
		if (bean.isDynamic(binding)) {
			return binding;
		}

		String attributeName = getDynamicAttributeName(binding);
		return (! attributeName.equals(binding) && bean.isDynamic(attributeName)) ? attributeName : null;
	}

	private static @Nonnull Class<?> resolveDynamicPropertyType(@Nonnull Bean bean,
													   @Nonnull String binding,
													   @Nonnull String attributeName) {
		Object value = get(bean, binding);
		if (value != null) {
			return value.getClass();
		}

		Attribute attribute = getDynamicMetadataAttribute(bean, attributeName);
		if (attribute == null) {
			return Object.class;
		}

		Class<?> relationType = resolveRelatedDynamicPropertyType(bean, binding, attributeName, attribute);
		return (relationType != null) ? relationType : attribute.getImplementingType();
	}

	private static @Nullable Attribute getDynamicMetadataAttribute(@Nonnull Bean bean, @Nonnull String attributeName) {
		Customer customer = CORE.getCustomer();
		Module module = customer.getModule(bean.getBizModule());
		Document document = module.getDocument(customer, bean.getBizDocument());
		return document.getPolymorphicAttribute(customer, attributeName);
	}

	private static @Nullable Class<?> resolveRelatedDynamicPropertyType(@Nonnull Bean bean,
														@Nonnull String binding,
														@Nonnull String attributeName,
														@Nonnull Attribute attribute) {
		try {
			if ((attribute instanceof Association) || (attribute instanceof InverseOne)) {
				return resolveRelationBeanClass(bean, (Reference) attribute);
			}
			if ((! attributeName.equals(binding)) && ((attribute instanceof Collection) || (attribute instanceof InverseMany))) {
				return resolveRelationBeanClass(bean, (Reference) attribute);
			}
			return null;
		}
		catch (Exception e) {
			throw new MetaDataException(e);
		}
	}

	private static @Nonnull Class<?> resolveRelationBeanClass(@Nonnull Bean bean,
													   @Nonnull Reference reference) throws ClassNotFoundException {
		Customer customer = CORE.getCustomer();
		Module module = customer.getModule(bean.getBizModule());
		Document document = module.getDocument(customer, reference.getDocumentName());
		return ((DocumentImpl) document).getBeanClass(customer);
	}

	private static @Nonnull Class<?> resolveCompoundPropertyType(@Nonnull Object bean,
													 @Nonnull String binding,
													 int lastDotIndex) {
		Object penultimate = get(bean, binding.substring(0, lastDotIndex));
		if (penultimate == null) {
			throw new MetaDataException("Binding " + binding + " does not resolve to a target bean.");
		}
		return getPropertyType(penultimate, binding.substring(lastDotIndex + 1));
	}

	public static boolean isMutable(@Nonnull Object bean, @Nonnull String simplePropertyName) {
		if (bean instanceof Bean b) {
			if (Bean.MODULE_KEY.equals(simplePropertyName) || Bean.DOCUMENT_KEY.equals(simplePropertyName) ||
					Bean.CREATED_KEY.equals(simplePropertyName) || Bean.NOT_CREATED_KEY.equals(simplePropertyName) ||
					Bean.CHANGED_KEY.equals(simplePropertyName) || Bean.NOT_CHANGED_KEY.equals(simplePropertyName) ||
					Bean.PERSISTED_KEY.equals(simplePropertyName) || Bean.NOT_PERSISTED_KEY.equals(simplePropertyName) ||
					Bean.BIZ_KEY.equals(simplePropertyName)) {
				return false;
			}
			
			boolean dynamic = b.isDynamic(simplePropertyName);
			if ((b instanceof DynamicBean) && (! dynamic)) {
				throw new IllegalStateException(simplePropertyName + " is not a property of " + bean);
			}
			
			if (dynamic) {
				return ! List.class.isAssignableFrom(BindUtil.getPropertyType(bean, simplePropertyName));
			}
		}

		try {
			return (PROPERTY_UTILS.getWriteMethod(PROPERTY_UTILS.getPropertyDescriptor(bean, simplePropertyName)) != null);
		}
		catch (Exception e) {
			throw new MetaDataException(e);
		}
	}

	/**
	 * The list of Scalar types - where recursion down the object Graph stops
	 */
	private static final Class<?>[] scalarTypes = new Class<?>[] {Boolean.class, 
																	Byte.class, 
																	Character.class, 
																	Double.class,
																	Float.class, 
																	Integer.class, 
																	Long.class, 
																	Short.class, 
																	String.class, 
																	StringBuffer.class, 
																	BigInteger.class,
																	BigDecimal.class, 
																	Date.class, 
																	Decimal.class, 
																	Enum.class};

	/**
	 * Determine if the propertyType is a scalar type - one that can be presented meaningfully using a Single value.
	 * 
	 * @param propertyType The <code>Class</code> object that represents the property type
	 * @return <code>true</code> if propertyType is scalar, otherwise return <code>false</code>.
	 */
	public static final boolean isAScalarType(@Nonnull Class<?> propertyType) {
		boolean found = propertyType.isPrimitive(); // is the property type built-in

		// iterate through scalarTypes looking for a type
		// that the propertyType can be assigned to
		for (int scalarTypeIndex = 0; (scalarTypeIndex < scalarTypes.length) && (! found); scalarTypeIndex++) {
			found = (scalarTypes[scalarTypeIndex].isAssignableFrom(propertyType));
		}

		return found;
	}

	/**
	 * Determine if an attribute name is an implicit attribute.
	 * 
	 * @param attributeName
	 */
	public static final boolean isImplicit(@Nullable String attributeName) {
		return (HierarchicalBean.PARENT_ID.equals(attributeName) ||
					ChildBean.PARENT_NAME.equals(attributeName) ||
					Bean.DOCUMENT_ID.equals(attributeName) ||
					Bean.CUSTOMER_NAME.equals(attributeName) ||
					Bean.DATA_GROUP_ID.equals(attributeName) ||
					Bean.USER_ID.equals(attributeName) ||
					Bean.BIZ_KEY.equals(attributeName) ||
					Bean.ORDINAL_NAME.equals(attributeName) ||
					Bean.CREATED_KEY.equals(attributeName) ||
					Bean.NOT_CREATED_KEY.equals(attributeName) ||
					Bean.PERSISTED_KEY.equals(attributeName) ||
					Bean.NOT_PERSISTED_KEY.equals(attributeName) ||
					Bean.CHANGED_KEY.equals(attributeName) ||
					Bean.NOT_CHANGED_KEY.equals(attributeName) ||
					Bean.MODULE_KEY.equals(attributeName) ||
					Bean.DOCUMENT_KEY.equals(attributeName) ||
					PersistentBean.LOCK_NAME.equals(attributeName) ||
					PersistentBean.VERSION_NAME.equals(attributeName) ||
					PersistentBean.TAGGED_NAME.equals(attributeName) ||
					PersistentBean.FLAG_COMMENT_NAME.equals(attributeName));
	}
	
	/**
	 * Return the implicit attribute type if the attributeName given is for an implicit attribute. 
	 */
	public static final @Nullable Class<?> implicitAttributeType(@Nonnull String attributeName) {
		if (HierarchicalBean.PARENT_ID.equals(attributeName) ||
				Bean.DOCUMENT_ID.equals(attributeName) ||
				Bean.CUSTOMER_NAME.equals(attributeName) ||
				Bean.DATA_GROUP_ID.equals(attributeName) ||
				Bean.USER_ID.equals(attributeName) ||
				Bean.BIZ_KEY.equals(attributeName) ||
				Bean.MODULE_KEY.equals(attributeName) ||
				Bean.DOCUMENT_KEY.equals(attributeName) ||
				PersistentBean.FLAG_COMMENT_NAME.equals(attributeName)) {
			return String.class;
		}
		else if (ChildBean.PARENT_NAME.equals(attributeName)) {
			return Bean.class;
		}
		else if (Bean.ORDINAL_NAME.equals(attributeName) ||
					PersistentBean.VERSION_NAME.equals(attributeName)) {
			return Integer.class;
		}
		else if (Bean.CREATED_KEY.equals(attributeName) ||
					Bean.NOT_CREATED_KEY.equals(attributeName) ||
					Bean.PERSISTED_KEY.equals(attributeName) ||
					Bean.NOT_PERSISTED_KEY.equals(attributeName) ||
					Bean.CHANGED_KEY.equals(attributeName) ||
					Bean.NOT_CHANGED_KEY.equals(attributeName) ||
					PersistentBean.TAGGED_NAME.equals(attributeName)) {
			return Boolean.class;
		}
		else if (PersistentBean.LOCK_NAME.equals(attributeName)) {
			return OptimisticLock.class;
		}
		return null;
	}
	
	
	/**
	 * See Binder.isDynamic().
	 */
	public static boolean isDynamic(@Nullable Customer customer,
										@Nonnull Module module,
										@Nonnull Document document,
										@Nullable Attribute attribute) {
		boolean result = document.isDynamic();
		if (! result) {
			result = isDynamic(customer, module, attribute);
		}
		return result;
	}
	
	/**
	 * See Binder.isDynamic().
	 */
	public static boolean isDynamic(@Nullable Customer customer,
										@Nonnull Module module,
										@Nullable Attribute attribute) {
		if (attribute instanceof Field f) {
			return f.isDynamic();
		}
		if (attribute instanceof Relation r) {
			return isDynamic(customer, module, r);
		}
		return false;
	}
	
	/**
	 * See Binder.isDynamic().
	 */
	public static boolean isDynamic(@Nullable Customer customer, 
										@Nonnull Module module,
										@Nonnull Relation relation) {
		String dn = relation.getDocumentName();
		Document rd = module.getDocument(customer, dn);
		return rd.isDynamic();
	}
	
	// NB properties must be a sorted map to ensure that the shortest properties
	// are processed first - ie User.contact is populated before User.contact.firstName,
	// otherwise the firstName new value will be tromped...
	public static void populateProperties(@Nonnull User user, 
											@Nullable Bean bean, 
											@Nullable SortedMap<String, Object> properties, 
											boolean fromSerializedFormat) {
		ValidationException e = new ValidationException();

		// Do nothing unless both arguments have been specified
		if ((bean == null) || (properties == null)) {
			return;
		}

		// Loop through the property name/value pairs to be set
		for (Map.Entry<String, Object> entry : properties.entrySet()) {
			// Identify the property name and value(s) to be assigned
			String name = entry.getKey();
			if (name == null) {
				continue;
			}
			Object value = entry.getValue();

			// Perform the assignment for this property
			try {
				populateProperty(user, bean, name, value, fromSerializedFormat);
			}
			catch (Exception ex) {
				LOGGER.error("Exception thrown when populating from the request.", ex);
				e.getMessages().add(new Message(name, value + " is invalid."));
			}
		}

		if (! e.getMessages().isEmpty()) {
			throw e;
		}
	}

	public static void populateProperty(@Nonnull User user, 
											@Nonnull Bean bean, 
											@Nonnull String bindingName, 
											@Nullable Object bindingValue, 
											boolean fromSerializedFormat) {
		Customer customer = user.getCustomer();
		Module beanModule = customer.getModule(bean.getBizModule());
		Document beanDocument = beanModule.getDocument(customer, bean.getBizDocument());

		Pair<Object, String> targetAndName = resolvePopulateTarget(user, beanModule, beanDocument, bean, bindingName);
		Object target = targetAndName.getLeft();
		if (target == null) {
			return;
		}

		PropertySelector selector = parsePropertySelector(targetAndName.getRight());
		Class<?> type = resolvePopulatePropertyType(target, selector.propertyName);
		if ((type == null) || ((! List.class.isAssignableFrom(type)) && (! BindUtil.isMutable(target, selector.propertyName)))) {
			return;
		}

		PopulateMetadata metadata = resolvePopulateMetadata(user,
														 bean,
														 target,
														 targetAndName.getRight(),
														 selector.propertyName,
														 bindingValue);

		Object newValue = convertPopulateIncomingValue(user.getCustomer(),
														 metadata.converter,
														 type,
														 metadata.value,
														 fromSerializedFormat);

		setPopulateValue(target, selector, newValue);
	}

	private static @Nonnull Pair<Object, String> resolvePopulateTarget(@Nonnull User user,
															 @Nonnull Module beanModule,
															 @Nonnull Document beanDocument,
															 @Nonnull Bean bean,
															 @Nonnull String bindingName) {
		int delim = findLastNestedIndex(bindingName);
		if (delim < 0) {
			return new ImmutablePair<>(bean, bindingName);
		}

		String targetBinding = bindingName.substring(0, delim);
		Object target = BindUtil.instantiateAndGet(user, beanModule, beanDocument, bean, targetBinding);
		String name = bindingName.substring(delim + 1);
		return new ImmutablePair<>(target, name);
	}

	private static @Nullable Class<?> resolvePopulatePropertyType(@Nonnull Object target, @Nonnull String propertyName) {
		try {
			return getPropertyType(target, propertyName);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return null;
		}
	}

	private static @Nonnull PopulateMetadata resolvePopulateMetadata(@Nonnull User user,
															 @Nonnull Bean bean,
															 @Nonnull Object target,
															 @Nonnull String bindingName,
															 @Nonnull String propName,
															 @Nullable Object value) {
		Customer customer = user.getCustomer();
		Module beanModule = customer.getModule(bean.getBizModule());
		Document beanDocument = beanModule.getDocument(customer, bean.getBizDocument());

		Converter<?> converter = null;
		Object adjustedValue = value;
		if (target instanceof Bean targetBean) {
			String documentName = targetBean.getBizDocument();
			if (documentName != null) {
				Module module = customer.getModule(targetBean.getBizModule());
				Document document = module.getDocument(customer, documentName);
				TargetMetaData propertyTarget = BindUtil.getMetaDataForBinding(customer, module, document, propName);
				Attribute attribute = propertyTarget.getAttribute();
				if (attribute instanceof ConvertibleField cf) {
					converter = cf.getConverterForCustomer(customer);
				}
				else if (attribute instanceof Collection) {
					BindUtil.instantiateAndGet(user, beanModule, beanDocument, bean, bindingName);
				}
				else if ("".equals(adjustedValue)) {
					adjustedValue = null;
				}
			}
		}

		return new PopulateMetadata(converter, adjustedValue);
	}

	private static @Nullable Object convertPopulateIncomingValue(@Nonnull Customer customer,
															  @Nullable Converter<?> converter,
															  @Nonnull Class<?> type,
															  @Nullable Object value,
															  boolean fromSerializedFormat) {
		String stringValue = null;
		if (value instanceof String string) {
			stringValue = string.trim();
		}
		else if (value instanceof String[] array) {
			stringValue = array[0];
			if (stringValue != null) {
				stringValue = stringValue.trim();
			}
		}

		if (stringValue == null) {
			return value;
		}
		if (stringValue.isEmpty()) {
			return null;
		}

		return fromString(customer, converter, type, stringValue, fromSerializedFormat);
	}

	private static void setPopulateValue(@Nonnull Object target,
												 @Nonnull PropertySelector selector,
												 @Nullable Object newValue) {
		try {
			if (selector.index >= 0) {
				PROPERTY_UTILS.setIndexedProperty(target, selector.propertyName, selector.index, newValue);
			}
			else if (selector.key != null) {
				PROPERTY_UTILS.setMappedProperty(target, selector.propertyName, selector.key, newValue);
			}
			else {
				convertAndSet(target, selector.propertyName, newValue);
			}
		}
		catch (Exception e) {
			throw new MetaDataException("Cannot set " + selector.propertyName, e);
		}
	}

	private static @Nonnull PropertySelector parsePropertySelector(@Nonnull String name) {
		String propName = name;
		int index = -1;
		String key = null;

		int openSquareIndex = propName.indexOf('[');
		if (openSquareIndex >= 0) {
			int closeSquareIndex = propName.indexOf(']');
			try {
				index = Integer.parseInt(propName.substring(openSquareIndex + 1, closeSquareIndex));
			}
			catch (@SuppressWarnings("unused") NumberFormatException e) {
				// do nothing
			}
			propName = propName.substring(0, openSquareIndex);
		}

		int openParenIndex = propName.indexOf('(');
		if (openParenIndex >= 0) {
			int closeParenIndex = propName.indexOf(')');
			try {
				key = propName.substring(openParenIndex + 1, closeParenIndex);
			}
			catch (@SuppressWarnings("unused") IndexOutOfBoundsException e) {
				// do nothing
			}
			propName = propName.substring(0, openParenIndex);
		}

		return new PropertySelector(propName, index, key);
	}

	private static final class PopulateMetadata {
		private final Converter<?> converter;
		private final Object value;

		private PopulateMetadata(@Nullable Converter<?> converter, @Nullable Object value) {
			this.converter = converter;
			this.value = value;
		}
	}

	static final class PropertySelector {
		final String propertyName;
		final int index;
		final String key;

		private PropertySelector(@Nonnull String propertyName, int index, @Nullable String key) {
			this.propertyName = propertyName;
			this.index = index;
			this.key = key;
		}
	}

	private static int findLastNestedIndex(@Nonnull String expression) {
		// walk back from the end to the start
		// and find the first index that
		int bracketCount = 0;
		for (int i = expression.length() - 1; i >= 0; i--) {
			char at = expression.charAt(i);
			switch (at) {
			case '.':
				if (bracketCount < 1) {
					return i;
				}
				break;

			case '(', '[':
				// not bothered which
				--bracketCount;
				break;

			case ')', ']':
				// not bothered which
				++bracketCount;
				break;
			default:
			}
		}

		// can't find any
		return -1;
	}

	public static void copy(@Nonnull final Bean from, @Nonnull final Bean to) {
		final Customer c = CORE.getUser().getCustomer();
		final Module m = c.getModule(from.getBizModule());
		final Document d = m.getDocument(c, from.getBizDocument());

		for (final Attribute attribute : d.getAllAttributes(c)) {
			final String attributeName = attribute.getName();
			AttributeType attributeType = attribute.getAttributeType();

			if (AttributeType.collection.equals(attributeType)) {
				copyCollection(from,
								to,
								attributeName,
								CollectionType.child.equals(((Collection) attribute).getType()));
			}
			else if (AttributeType.inverseMany.equals(attributeType)) {
				copyCollection(from, to, attributeName, false);
			}
			else {
				BindUtil.set(to, attributeName, BindUtil.get(from, attributeName));
			}
		}
	}

	@SuppressWarnings("unchecked")
	private static void copyCollection(@Nonnull final Bean from,
										@Nonnull final Bean to,
										@Nonnull final String attributeName,
										boolean reparent) {
		List<Bean> colFrom = (List<Bean>) BindUtil.get(from, attributeName);
		List<Bean> colTo = (List<Bean>) BindUtil.get(to, attributeName);
		if (colTo != null) {
			replaceCollectionContents(colTo, colFrom);
			if (reparent && (colFrom != null)) {
				colFrom.forEach(e -> ((ChildBean<Bean>) e).setParent(to));
			}
		}
	}

	private static void replaceCollectionContents(@Nonnull List<Bean> targetCollection,
												   @Nullable List<Bean> sourceCollection) {
		for (int i = targetCollection.size() - 1; i >= 0; i--) {
			targetCollection.remove(i);
		}
		if (sourceCollection != null) {
			targetCollection.addAll(sourceCollection);
		}
	}

	
	/**
	 * Get the parent document and attribute for a binding expression.
	 * This will traverse the binding (across documents and references
	 * over the '.' and '[]' found in the expression) to find the ultimate document
	 * and attribute that the binding points to.
	 * 
	 * @param customer	The customer to do this for.
	 * @param module	The module to start at (with respect to)
	 * @param document	The document to start at (with respect to)
	 * @param binding	The binding expression.
	 * @return	The document and attribute that the binding expression points to.
	 * 			The document is never null whereas the attribute can be null if
	 * 			the binding expression ultimately resolves to an implicit attribute
	 * 			like bizKey or bizId or the like.
	 * @throws	MetaDataException if the binding is malformed or cannot be resolved.
	 */
	public static @Nonnull TargetMetaData getMetaDataForBinding(@Nullable Customer customer, 
																	@Nonnull Module module, 
																	@Nonnull Document document, 
																	@Nonnull String binding) {
		Document navigatingDocument = document;
		Module navigatingModule = module;
		Attribute attribute = null;
		Class<?> type = null;

		StringTokenizer tokenizer = new StringTokenizer(binding, ".");
		while (tokenizer.hasMoreTokens()) {
			String attributeName = normaliseBindingToken(tokenizer.nextToken());
			String parentDocumentName = resolveParentDocumentName(customer,
																 navigatingModule,
																 navigatingDocument,
																 attributeName,
																 binding);
			attribute = resolveInheritedAttribute(customer, navigatingModule, navigatingDocument, attributeName);
			if (tokenizer.hasMoreTokens()) {
				navigatingDocument = resolveNextNavigatingDocument(customer,
																	 navigatingModule,
																	 navigatingDocument,
																	 attributeName,
																	 parentDocumentName,
																	 attribute,
																	 binding);
				navigatingModule = resolveNavigatingModule(customer, navigatingDocument);
			}
			else {
				type = resolveTerminalBindingType(customer,
													 navigatingModule,
													 navigatingDocument,
													 binding,
													 attributeName,
													 attribute);
			}
		}

		if (type == null) {
			throw new MetaDataException(navigatingDocument.getOwningModuleName() + '.' + navigatingDocument.getName() +
					" @ " + binding + " does not resolve to a concrete attribute type.");
		}

		return new TargetMetaData(navigatingDocument, attribute, type);
	}

	private static @Nonnull String normaliseBindingToken(@Nonnull String token) {
		String result = token;
		int openBraceIndex = result.indexOf('[');
		if (openBraceIndex > -1) {
			result = result.substring(0, openBraceIndex);
		}
		int openParenthesisIndex = result.indexOf(ELEMENT_BY_ID);
		if (openParenthesisIndex > -1) {
			result = result.substring(0, openParenthesisIndex);
		}
		return result;
	}

	private static @Nullable String resolveParentDocumentName(@Nullable Customer customer,
														  @Nonnull Module navigatingModule,
														  @Nonnull Document navigatingDocument,
														  @Nonnull String attributeName,
														  @Nonnull String binding) {
		if (! ChildBean.PARENT_NAME.equals(attributeName)) {
			return null;
		}

		String parentDocumentName = navigatingDocument.getParentDocumentName();
		Extends inherits = navigatingDocument.getExtends();
		while ((parentDocumentName == null) && (inherits != null)) {
			Document baseDocument = navigatingModule.getDocument(customer, inherits.getDocumentName());
			parentDocumentName = baseDocument.getParentDocumentName();
			inherits = baseDocument.getExtends();
		}
		if (parentDocumentName == null) {
			throw new MetaDataException(navigatingDocument.getOwningModuleName() + '.' +
						navigatingDocument.getName() + " @ " + binding +
						" does not exist (token [parent] doesn't check out as " + navigatingDocument.getName() +
						" is not a child document)");
		}

		return parentDocumentName;
	}

	private static @Nullable Attribute resolveInheritedAttribute(@Nullable Customer customer,
														   @Nonnull Module navigatingModule,
														   @Nonnull Document navigatingDocument,
														   @Nonnull String attributeName) {
		Attribute attribute = navigatingDocument.getAttribute(attributeName);
		if ((attribute == null) && (customer != null)) {
			attribute = navigatingDocument.getPolymorphicAttribute(customer, attributeName);
		}
		Extends inherits = navigatingDocument.getExtends();
		while ((attribute == null) && (inherits != null)) {
			Document baseDocument = navigatingModule.getDocument(customer, inherits.getDocumentName());
			attribute = baseDocument.getAttribute(attributeName);
			if ((attribute == null) && (customer != null)) {
				attribute = baseDocument.getPolymorphicAttribute(customer, attributeName);
			}
			inherits = baseDocument.getExtends();
		}

		return attribute;
	}

	private static @Nonnull Document resolveNextNavigatingDocument(@Nullable Customer customer,
														   @Nonnull Module navigatingModule,
														   @Nonnull Document navigatingDocument,
														   @Nonnull String attributeName,
														   @Nullable String parentDocumentName,
														   @Nullable Attribute attribute,
														   @Nonnull String binding) {
		if (ChildBean.PARENT_NAME.equals(attributeName)) {
			if (parentDocumentName == null) {
				throw new MetaDataException(navigatingDocument.getOwningModuleName() + '.' +
							navigatingDocument.getName() + " @ " + binding +
							" does not exist (token [parent] doesn't check out as " + navigatingDocument.getName() +
							" is not a child document)");
			}
			return navigatingModule.getDocument(customer, parentDocumentName);
		}
		if (attribute instanceof Relation relation) {
			return navigatingModule.getDocument(customer, relation.getDocumentName());
		}

		throw new MetaDataException(navigatingDocument.getOwningModuleName() + '.' +
					navigatingDocument.getName() + " @ " + binding +
					" does not exist (token [" + attributeName + "] doesn't check out)");
	}

	private static @Nonnull Module resolveNavigatingModule(@Nullable Customer customer,
														 @Nonnull Document navigatingDocument) {
		Module module = (customer == null)
				? ProvidedRepositoryFactory.get().getModule(null, navigatingDocument.getOwningModuleName())
				: customer.getModule(navigatingDocument.getOwningModuleName());
		if (module == null) {
			throw new MetaDataException("Module " + navigatingDocument.getOwningModuleName() +
						" does not exist for navigating document " + navigatingDocument.getName());
		}
		return module;
	}

	private static @Nonnull Class<?> resolveTerminalBindingType(@Nullable Customer customer,
														  @Nonnull Module navigatingModule,
														  @Nonnull Document navigatingDocument,
														  @Nonnull String binding,
														  @Nonnull String attributeName,
														  @Nullable Attribute attribute) {
		if (attribute == null) {
			Class<?> implicitType = implicitAttributeType(attributeName);
			if (implicitType != null) {
				return implicitType;
			}
			throw new MetaDataException(navigatingDocument.getOwningModuleName() + '.' +
						navigatingDocument.getName() + " @ " + binding +
						" does not exist (last attribute not in document)");
		}

		if ((attribute instanceof Collection) || (attribute instanceof InverseMany)) {
			return List.class;
		}

		if (attribute instanceof Relation relation) {
			Document relationDocument = navigatingModule.getDocument(customer, relation.getDocumentName());
			if (! (relationDocument instanceof DocumentImpl relationDocumentImpl)) {
				Object relationType = relation.getImplementingType();
				return (relationType instanceof Class<?> relationClass) ? relationClass : Bean.class;
			}

			Customer resolvedCustomer = (customer == null) ? CORE.getCustomer() : customer;
			try {
				return relationDocumentImpl.getBeanClass(resolvedCustomer);
			}
			catch (ClassNotFoundException e) {
				throw new MetaDataException("Binding " + binding + " resolves to relation attribute " + relation.getName() +
						" but the document class for document " + relationDocumentImpl.getOwningModuleName() +
						'.' + relationDocumentImpl.getName() + " cannot be loaded.", e);
			}
		}

		// Generate-domain validation can run before enum classes exist, so use the metadata-safe fallback.
		return getImplementingTypeForGenerateDomainValidation(attribute);
	}

	/**
	 * Resolve an attribute implementing type for metadata validation.
	 * <p>
	 * This is primarily used by generateDomain/bootstrap flows where generated enum classes
	 * may not yet exist. In that case we use {@link Enum} as a best-effort fallback so
	 * validation can continue until generation emits the enum.
	 */
	static @Nonnull Class<?> getImplementingTypeForGenerateDomainValidation(@Nonnull Attribute attribute) {
		try {
			return attribute.getImplementingType();
		}
		catch (MetaDataException e) {
			if ((attribute instanceof org.skyve.impl.metadata.model.document.field.Enumeration) &&
					org.skyve.impl.metadata.model.document.field.Enumeration.isEnumClassLoadingFailure(e)) {
				return Enum.class;
			}
			throw e;
		}
	}

	public static @Nullable Object instantiateAndGet(@Nonnull User user,
														@Nonnull Module module,
														@Nonnull Document document,
														@Nonnull Bean bean,
														@Nonnull String binding) {
		Customer customer = user.getCustomer();
		Document navigatingDocument = document;
		Object owner = bean;
		Object value = null;

		StringTokenizer tokenizer = new StringTokenizer(binding, ".");
		while (tokenizer.hasMoreTokens()) {
			String bindingPart = tokenizer.nextToken();
			String fieldName = bindingPart;
			int openBraceIndex = fieldName.indexOf('[');
			if (openBraceIndex > -1) {
				fieldName = fieldName.substring(0, openBraceIndex);
			}
			Pair<Document, Attribute> navigation = resolveInstantiationNavigation(customer,
																			 module,
																			 navigatingDocument,
																			 fieldName,
																			 binding,
																			 tokenizer);
			navigatingDocument = navigation.getLeft();
			Attribute attribute = navigation.getRight();

			try {
				value = resolveExistingOrIndexedValue(user,
														 module,
														 customer,
														 owner,
														 bindingPart,
														 openBraceIndex,
														 attribute);
				if (value == null) {
					value = createAndAssociateNavigatingValue(user, navigatingDocument, owner, bindingPart);
				}
				owner = value;
			}
			catch (Exception e) {
				throw new MetaDataException(e);
			}
		}

		return value;
	}

	private static @Nonnull Pair<Document, Attribute> resolveInstantiationNavigation(@Nonnull Customer customer,
																			  @Nonnull Module module,
																			  @Nonnull Document navigatingDocument,
																			  @Nonnull String fieldName,
																			  @Nonnull String binding,
																			  @Nonnull StringTokenizer tokenizer) {
		if (ChildBean.PARENT_NAME.equals(fieldName)) {
			Document parentDocument = navigatingDocument.getParentDocument(customer);
			if (parentDocument == null) {
				throw new MetaDataException(binding + " should point to a parent document but parent document does not exist");
			}
			return new ImmutablePair<>(parentDocument, null);
		}

		TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, navigatingDocument, fieldName);
		Attribute attribute = target.getAttribute();
		if (attribute == null) {
			throw new IllegalArgumentException(fieldName + " within " + binding + "doesn't check out");
		}

		if (! (attribute instanceof Relation relation)) {
			String badToken = tokenizer.hasMoreTokens() ? tokenizer.nextToken() : fieldName;
			throw new MetaDataException(binding + " does not exist (token " + badToken + " doesn't check out)");
		}

		Document targetDocument = module.getDocument(customer, relation.getDocumentName());
		return new ImmutablePair<>(targetDocument, attribute);
	}

	@SuppressWarnings("unchecked")
	private static @Nullable Object resolveExistingOrIndexedValue(@Nonnull User user,
															   @Nonnull Module module,
															   @Nonnull Customer customer,
															   @Nonnull Object owner,
															   @Nonnull String bindingPart,
															   int openBraceIndex,
															   @Nullable Attribute attribute) {
		try {
			return BindUtil.get(owner, bindingPart);
		}
		catch (@SuppressWarnings("unused") IndexOutOfBoundsException e) {
			if ((attribute == null) || (openBraceIndex < 0)) {
				return null;
			}

			Document collectionDocument = module.getDocument(customer, ((Relation) attribute).getDocumentName());
			String collectionBinding = bindingPart.substring(0, openBraceIndex);
			int collectionIndex = Integer.parseInt(bindingPart.substring(openBraceIndex + 1, bindingPart.length() - 1));
			List<Bean> collection = (List<Bean>) BindUtil.get(owner, collectionBinding);
			if (collection == null) {
				return null;
			}

			while (collection.size() <= collectionIndex) {
				try {
					Bean fillerElement = collectionDocument.newInstance(user);
					BindUtil.addElementToCollection((Bean) owner, collectionBinding, fillerElement);
				}
				catch (Exception exception) {
					throw new MetaDataException(exception);
				}
			}

			return collection.get(collectionIndex);
		}
	}

	private static @Nonnull Object createAndAssociateNavigatingValue(@Nonnull User user,
															  @Nonnull Document navigatingDocument,
															  @Nonnull Object owner,
															  @Nonnull String bindingPart) {
		try {
			Object value = navigatingDocument.newInstance(user);
			BindUtil.setAssociation((Bean) owner, bindingPart, (Bean) value);
			return value;
		}
		catch (Exception exception) {
			throw new MetaDataException(exception);
		}
	}
	
	/**
	 * Fashion a type identifier from the given string.
	 * @param string
	 * @return	A valid java type identifier.  Title case.
	 */
	public static @Nonnull String toJavaTypeIdentifier(@Nonnull String string) {
		StringBuilder sb = new StringBuilder(toJavaInstanceIdentifier(string));
		sb.setCharAt(0, Character.toUpperCase(sb.charAt(0)));
		return sb.toString();
	}
	
	/**
	 * Fashion an instance identifier from the given string.
	 * @param string
	 * @return	A valid java instance identifier.  Camel case.
	 */
	public static @Nonnull String toJavaInstanceIdentifier(@Nonnull String string) {
		StringBuilder sb = new StringBuilder(string);
		removeOrReplaceInvalidCharacters(sb);
		return Introspector.decapitalize(sb.toString());
	}
	
	/**
	 * Return a java bean property name from a reflected method name.
	 * @param methodName	The method name
	 * @return	"get"/"set"/"is" prefix stripped and the remaining string decapitalised as appropriate.
	 */
	public static @Nonnull String toJavaPropertyName(@Nonnull String methodName) {
		String result = methodName;
		if (methodName.startsWith("get") || methodName.startsWith("set")) {
			result = Introspector.decapitalize(methodName.substring(3));
		}
		else if (methodName.startsWith("is")) {
			result = Introspector.decapitalize(methodName.substring(2));
		}
		return result;
	}
	
	/**
	 * Fashion a static identifier from the given string.
	 * @param string
	 * @return A valid java static identifier.  Upper Case with underscores.
	 */
	public static @Nonnull String toJavaStaticIdentifier(@Nonnull String string) {
		String javaIdentifierName = toJavaInstanceIdentifier(string);
		StringBuilder sb = new StringBuilder(javaIdentifierName.length() + 5);

		for (int i = 0, l = javaIdentifierName.length(); i < l; i++) {
            char ch = javaIdentifierName.charAt(i);
			if (Character.isUpperCase(ch)) {
                boolean nextCharLowerCase = false;
                boolean prevCharLowerCase = false;
                int nextIndex = i + 1;
                int prevIndex = i - 1;
                if (nextIndex < l) {
                    char nextChar = javaIdentifierName.charAt(nextIndex);
                    nextCharLowerCase = Character.isLowerCase(nextChar);
                }
                if(prevIndex >= 0) {
                    char prevChar = javaIdentifierName.charAt(prevIndex);
                    prevCharLowerCase = Character.isLowerCase(prevChar);
                }

                // if the previous char was upper case then don't add an underscore
                if ((prevCharLowerCase || nextCharLowerCase) && (i > 0)) {
                    sb.append('_');
                }
				sb.append(ch);
			}
			else {
                sb.append(Character.toUpperCase(ch));
			}
		}
		
		return sb.toString();
	}

	/**
	 * Fashion a title case identifier from the given string.
	 * 
	 * @param string The string to convert
	 * @return A title case string. First letter of each word upper cased with spaces between words.
	 */
	public static @Nonnull String toTitleCase(@Nonnull String string) {
		String javaIdentifierName = BindUtil.toJavaTypeIdentifier(string);
		StringBuilder sb = new StringBuilder(javaIdentifierName.length() + 5);

		for (int i = 0, l = javaIdentifierName.length(); i < l; i++) {
			char ch = javaIdentifierName.charAt(i);
			if (Character.isUpperCase(ch)) {
				boolean nextCharLowerCase = false;
				boolean prevCharLowerCase = false;
				int nextIndex = i + 1;
				int prevIndex = i - 1;
				if (nextIndex < l) {
					char nextChar = javaIdentifierName.charAt(nextIndex);
					nextCharLowerCase = Character.isLowerCase(nextChar);
				}
				if (prevIndex >= 0) {
					char prevChar = javaIdentifierName.charAt(prevIndex);
					prevCharLowerCase = Character.isLowerCase(prevChar);
				}

				// if the previous char was upper case then don't add a space
				if ((prevCharLowerCase || nextCharLowerCase) && (i > 0)) {
					sb.append(' ');
				}
				sb.append(ch);
			} else {
				sb.append(ch);
			}
		}

		return sb.toString();
	}

	private static void removeOrReplaceInvalidCharacters(@Nonnull StringBuilder sb) {
		int i = 0;
		boolean whiteSpaceOrUnderscore = false;
		while (i < sb.length()) {
			char charAt = sb.charAt(i);
			if (i == 0) {
				i = processLeadingIdentifierCharacter(sb, charAt);
			}
			else {
				whiteSpaceOrUnderscore = processIdentifierCharacter(sb, i, charAt, whiteSpaceOrUnderscore);
				if (Character.isJavaIdentifierPart(charAt) && (charAt != '_')) {
					i++;
				}
			}
		}
		
		if (! sb.isEmpty()) {
			replaceLeadingDigit(sb);
		}
	}

	private static int processLeadingIdentifierCharacter(@Nonnull StringBuilder sb, char charAt) {
		// Allow java start char except '_' which is reserved in skyve
		// plus digits which are converted to words below.
		if ((Character.isJavaIdentifierStart(charAt) && (charAt != '_')) || Character.isDigit(charAt)) {
			return 1;
		}
		sb.deleteCharAt(0);
		return 0;
	}

	private static boolean processIdentifierCharacter(@Nonnull StringBuilder sb,
												   int index,
												   char charAt,
												   boolean whiteSpaceOrUnderscore) {
		if (Character.isJavaIdentifierPart(charAt) && (charAt != '_')) {
			if (whiteSpaceOrUnderscore) {
				sb.setCharAt(index, Character.toUpperCase(charAt));
			}
			return false;
		}
		boolean nextUpperCase = Character.isWhitespace(charAt) || (charAt == '_');
		sb.deleteCharAt(index);
		return nextUpperCase;
	}

	private static void replaceLeadingDigit(@Nonnull StringBuilder sb) {
		switch (sb.charAt(0)) {
		case '0':
			sb.replace(0, 1, "zero");
			break;
		case '1':
			sb.replace(0, 1, "one");
			break;
		case '2':
			sb.replace(0, 1, "two");
			break;
		case '3':
			sb.replace(0, 1, "three");
			break;
		case '4':
			sb.replace(0, 1, "four");
			break;
		case '5':
			sb.replace(0, 1, "five");
			break;
		case '6':
			sb.replace(0, 1, "six");
			break;
		case '7':
			sb.replace(0, 1, "seven");
			break;
		case '8':
			sb.replace(0, 1, "eight");
			break;
		case '9':
			sb.replace(0, 1, "nine");
			break;
		default:
		}
	}
}
