package org.skyve.impl.bind;

import java.beans.Introspector;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.StringTokenizer;
import java.util.function.Function;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.apache.commons.collections.comparators.ComparatorChain;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.io.WKTReader;
import org.locationtech.jts.io.WKTWriter;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.DynamicBean;
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
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.enumeration.DynamicEnumerationConverter;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.InverseMany;
import org.skyve.impl.metadata.model.document.InverseOne;
import org.skyve.impl.metadata.model.document.field.ConvertableField;
import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.util.NullTolerantBeanComparator;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Condition;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Collection.Ordering;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Inverse;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.ExpressionEvaluator;

/**
 * Provides utilities for getting and setting simple and compound bean properties.
 */
public final class BindUtil {
	private static final String DEFAULT_DISPLAY_DATE_FORMAT = "dd/MM/yyyy";
	private static final DeproxyingPropertyUtilsBean PROPERTY_UTILS = new DeproxyingPropertyUtilsBean();
	
	public static String formatMessage(String message, Bean... beans) {
		return formatMessage(message, null, beans);
	}
	
	public static String formatMessage(String message, Function<String, String> postEvaluateDisplayValue, Bean... beans) {
		StringBuilder result = new StringBuilder(message);
		int openCurlyBraceIndex = result.indexOf("{");
		while (openCurlyBraceIndex >= 0) {
			if ((openCurlyBraceIndex == 0) || // first char is '{' 
					// '{' is present and not escaped with a preceding '\' - ie \{ is escaped
					((openCurlyBraceIndex > 0) && (result.charAt(openCurlyBraceIndex - 1) != '\\'))) {

				int closedCurlyBraceIndex = result.indexOf("}", openCurlyBraceIndex);
				if (closedCurlyBraceIndex < openCurlyBraceIndex) {
					throw new MetaDataException('[' + message + "] does not have a matching '}' for the '{' at position " + (openCurlyBraceIndex + 1));
				}
				String expression = result.substring(openCurlyBraceIndex, closedCurlyBraceIndex + 1);
				boolean success = false;
				Exception cause = null;
				for (Bean bean : beans) {
// The document name null check was commented out because PersistenceTest did not pass.
// Genn'd EL expressions in AllAttributesPersistent didn't get past the documentName test below
// because they were a hibernate proxy getting initiailised on construction.
// I cant work out why this test was here but it must have been guarding something.
// Maybe MapBean (now DynamicBean) never used to set the document name.
//					String documentName = bean.getBizDocument();
//					if (documentName != null) {
						try {
							// Try to get the value from this bean
							// Do not use BindUtil.getMetaDataForBinding as it may not be a document
							// property, it could be a condition or an implicit property.
							String displayValue = ExpressionEvaluator.format(expression, bean);
							// if there is a postEvaluateDisplayValue function, apply it
							if (postEvaluateDisplayValue != null) {
								displayValue = postEvaluateDisplayValue.apply(displayValue);
							}
							result.replace(openCurlyBraceIndex, closedCurlyBraceIndex + 1, displayValue);
							// move the openCurlyBraceIndex along by the display value length so that
							// any '{' occurrences replaced in as literals above are skipped.
							openCurlyBraceIndex += displayValue.length();
							// find the next occurrence
							openCurlyBraceIndex = result.indexOf("{", openCurlyBraceIndex);
							success = true;
							
							break;
						}
						catch (Exception e) {
							cause = e;
						}
					}
//				}
				
				if (! success) {
					StringBuilder exMessage = new StringBuilder();
					exMessage.append("Expression ").append(expression).append(" cannot be evaluated against bean");
					if (beans.length > 1) {
						exMessage.append("s");
					}
					exMessage.append(" - ");
					
					for (int offset = 0; offset < beans.length; offset++) {
						Bean bean = beans[offset];
						exMessage.append(bean.getBizDocument());
						
						if ((offset - 1) < beans.length) {
							exMessage.append(", ");
						}
					}
					
					if (cause == null) {
						throw new MetaDataException(exMessage.toString());
					}
					throw new MetaDataException(exMessage.toString(), cause);
				}
			}
			else { // escaped { found - ie "\{" - remove the escape chars and move on to the next pair of {}
				result.replace(openCurlyBraceIndex - 1, openCurlyBraceIndex, "");
				openCurlyBraceIndex = result.indexOf("{", openCurlyBraceIndex); // NB openCurlyBracedIndex was not decremented but a char was removed
			}
		}

		return result.toString().replace("\\}", "}"); // remove any escaped closing curly braces now
	}

	public static boolean containsSkyveExpressions(String display) {
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
	
	public static boolean isSkyveExpression(String expression) {
		int length = expression.length();
		return ((length > 1) && 
					(expression.charAt(0) == '{') && 
					(expression.charAt(length - 1) == '}'));
	}
	
	public static String validateMessageExpressions(Customer customer, Module module, Document document, String message) {
		String error = null;
		
		StringBuilder result = new StringBuilder(message);
		int openCurlyBraceIndex = result.indexOf("{");
		while ((error == null) && (openCurlyBraceIndex >= 0)) {
			if ((openCurlyBraceIndex == 0) || // first char is '{' 
					// '{' is present and not escaped with a preceding '\' - ie \{ is escaped
					((openCurlyBraceIndex > 0) && (result.charAt(openCurlyBraceIndex - 1) != '\\'))) {

				int closedCurlyBraceIndex = result.indexOf("}", openCurlyBraceIndex);
				if (closedCurlyBraceIndex < 0) {
					error = "Opening '{' with no closing '}'";
				}
				else {
					String expression = result.substring(openCurlyBraceIndex, closedCurlyBraceIndex + 1);
					if (expression.length() == 2) {
						error = "Expression is empty";
					}
					else {
						error = ExpressionEvaluator.validate(expression, null, customer, module, document);
						openCurlyBraceIndex = result.indexOf("{", openCurlyBraceIndex + 1);
					}
				}
			}
			else { // escaped { found - ie "\{" - remove the escape chars and move on to the next pair of {}
				result.replace(openCurlyBraceIndex - 1, openCurlyBraceIndex, "");
				openCurlyBraceIndex = result.indexOf("{", openCurlyBraceIndex); // NB openCurlyBracedIndex was not decremented but a char was removed
			}
		}

		return error;
	}
	
	/**
	 * Place the bindingPrefix + '.' + <existing expression> wherever a binding expression - {<expression>} occurs.
	 * @param message	The message to process. Can be null.
	 * @param bindingPrefix	The binding prefix to prefix with.
	 * @return	The prefixed message or null if message argument was null.
	 */
	public static String prefixMessageBindings(String message, String bindingPrefix) {
		if (message == null) {
			return null;
		}

		String bindingPrefixAndDot = bindingPrefix + '.';
		
		StringBuilder result = new StringBuilder(message);
		int openCurlyBraceIndex = result.indexOf("{");
		while (openCurlyBraceIndex >= 0) {
			if ((openCurlyBraceIndex == 0) || // first char is '{' 
					// '{' is present and not escaped with a preceding '\' - ie \{ is escaped
					((openCurlyBraceIndex > 0) && (result.charAt(openCurlyBraceIndex - 1) != '\\'))) {
				result.insert(openCurlyBraceIndex + 1, bindingPrefixAndDot);
				openCurlyBraceIndex = result.indexOf("{", openCurlyBraceIndex + 1);
			}
		}
		return result.toString();
	}

	public static boolean evaluateCondition(Bean bean, String condition) {
		boolean result = false;

		if (String.valueOf(true).equals(condition)) {
			result = true;
		}
		else if (! String.valueOf(false).equals(condition)) {
			try {
				if (BindUtil.isSkyveExpression(condition)) {
					result = Boolean.TRUE.equals(ExpressionEvaluator.evaluate(condition, bean));
				}
				else {
					boolean negated = false;
					String conditionName = condition;
					if (condition.startsWith("not")) {
						conditionName = Introspector.decapitalize(condition.substring(3));
						negated = true;
					}
					Customer c = CORE.getCustomer();
					Module m = c.getModule(bean.getBizModule());
					Document d = m.getDocument(c, bean.getBizDocument());
					Condition condish = d.getCondition(conditionName);
					if (condish != null) { // could be null if an implicit condition (created or persisted etc)
						String expression = condish.getExpression();
						if (BindUtil.isSkyveExpression(expression)) { // condition expression
							if (negated) {
								result = Boolean.FALSE.equals(ExpressionEvaluator.evaluate(expression, bean));
							}
							else {
								result = Boolean.TRUE.equals(ExpressionEvaluator.evaluate(expression, bean));
							}
						}
						else { // condition name
							result = (Boolean.TRUE.equals(BindUtil.get(bean, condition)));
						}
					}
					else { // implicit condition
						result = (Boolean.TRUE.equals(BindUtil.get(bean, condition)));
					}
				}
			}
			catch (Exception e) {
				throw new MetaDataException("Condition " + bean.getBizModule() + "." + bean.getBizDocument() + "." + condition + " is not valid", e);
			}
		}

		return result;
	}

	public static String negateCondition(String condition) {
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
	            sb.append("not").append(Character.valueOf(Character.toUpperCase(condition.charAt(0))));
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
	 * @param type
	 * @param value
	 * @return
	 */
	public static Object convert(Class<?> type, Object value) {
		Object result = value;

		if (value != null) {
			if (type.equals(Integer.class)) {
				if ((! (value instanceof Integer)) && (value instanceof Number)) {
					result = Integer.valueOf(((Number) value).intValue());
				}
			}
			else if (type.equals(Long.class)) {
				if ((! (value instanceof Long)) && (value instanceof Number)) {
					result = Long.valueOf(((Number) value).longValue());
				}
			}
			else if (type.equals(Short.class)) {
				if ((! (value instanceof Short)) && (value instanceof Number)) {
					result = Short.valueOf(((Number) value).shortValue());
				}
			}
			else if (type.equals(Float.class)) {
				if ((! (value instanceof Float)) && (value instanceof Number)) {
					result = Float.valueOf(((Number) value).floatValue());
				}
			}
			else if (type.equals(Double.class)) {
				if ((! (value instanceof Double)) && (value instanceof Number)) {
					result = Double.valueOf(((Number) value).doubleValue());
				}
			}
			else if (type.equals(BigDecimal.class)) {
				if (! (value instanceof BigDecimal)) {
					result = new BigDecimal(value.toString());
				}
			}
			else if (type.equals(Decimal2.class)) {
				if (! (value instanceof Decimal2)) {
					result = new Decimal2(value.toString());
				}
			}
			else if (type.equals(Decimal5.class)) {
				if (! (value instanceof Decimal5)) {
					result = new Decimal5(value.toString());
				}
			}
			else if (type.equals(Decimal10.class)) {
				if (! (value instanceof Decimal10)) {
					result = new Decimal10(value.toString());
				}
			}
			else if (type.equals(DateOnly.class)) {
				if ((! (value instanceof DateOnly)) && (value instanceof Date)) {
					result = new DateOnly(((Date) value).getTime());
				}
			}
			else if (type.equals(TimeOnly.class)) {
				if ((! (value instanceof TimeOnly)) && (value instanceof Date)) {
					result = new TimeOnly(((Date) value).getTime());
				}
			}
			else if (type.equals(DateTime.class)) {
				if ((! (value instanceof DateTime)) && (value instanceof Date)) {
					result = new DateTime(((Date) value).getTime());
				}
			}
			else if (type.equals(Timestamp.class)) {
				if ((! (value instanceof Timestamp)) && (value instanceof Date)) {
					result = new Timestamp(((Date) value).getTime());
				}
			}
			else if (type.equals(Geometry.class)) {
				if (value instanceof String) {
					try {
						result = new WKTReader().read((String) value);
					}
					catch (ParseException e) {
						throw new DomainException(value + " is not valid WKT", e);
					}
				}
			}
			// NB type.isEnum() doesn't work as our enums implement another interface
			// NB Enumeration.class.isAssignableFrom(type) doesn't work as enums are not assignable as they are a synthesised class
			else if (Enum.class.isAssignableFrom(type)) {
				if (value instanceof String) {
					// Since we can't test for assignable, see if we can see the Enumeration interface
					Class<?>[] interfaces = type.getInterfaces();
					if ((interfaces.length == 1) && (Enumeration.class.equals(interfaces[0]))) {
						try {
							result = type.getMethod(Enumeration.FROM_CODE_METHOD_NAME, String.class).invoke(null, value);
							if (result == null) {
								result = type.getMethod(Enumeration.FROM_LOCALISED_DESCRIPTION_METHOD_NAME, String.class).invoke(null, value);
							}
						}
						catch (Exception e) {
							throw new DomainException(value + " is not a valid enumerated value in type " + type, e);
						}
					}
					if (result == null) {
						@SuppressWarnings("unchecked")
						Object temp = Enum.valueOf(type.asSubclass(Enum.class), (String) value);
						result = temp;
					}
				}
				else if (value instanceof Enumeration) {
					convert(type, ((Enumeration) value).toCode());
				}
				else { // hopefully value is an enum
					convert(type, value.toString());
				}
			}
		}

		return result;
	}

	/**
	 * This method is synchronized as {@link Converter#fromDisplayValue(Object)} requires synchronization. 
	 * Explicit type coercion using the <code>converter</code> if supplied, or by java language coercion.
	 */
	public static synchronized Object fromString(Customer customer,
													Converter<?> converter,
													Class<?> type,
													String stringValue) {
		return fromString(customer, converter, type, stringValue, false);
	}

	/**
	 * This method is synchronized as {@link Converter#fromDisplayValue(Object)} requires synchronization. 
	 * Explicit type coercion from serialised formats using the <code>converter</code> if supplied, or by java language coercion.
	 */
	public static synchronized Object fromSerialised(Converter<?> converter,
														Class<?> type,
														String stringValue) {
		return fromString(null, converter, type, stringValue, true);
	}

	/**
	 * This method is synchronized as {@link Converter#fromDisplayValue(Object)} requires synchronization. 
	 * Explicit type coercion from serialised formats using the <code>converter</code> if supplied, or by java language coercion.
	 */
	public static synchronized Object fromSerialised(Class<?> type, String stringValue) {
		return fromString(null, null, type, stringValue, true);
	}

	private static Object fromString(Customer customer,
										Converter<?> converter,
										Class<?> type,
										String stringValue,
										boolean fromSerializedFormat) {
		Object result = null;

		try {
			// use the converter if this is not from a serialized value
			if ((! fromSerializedFormat) && (converter != null)) {
				result = converter.fromDisplayValue(stringValue);
			}
			// cater for dynamic enum conversion to String
			else if (fromSerializedFormat && (converter instanceof DynamicEnumerationConverter)) {
				result = converter.fromDisplayValue(stringValue);
			}
			else if (type.equals(String.class)) {
				result = stringValue;
			}
			else if (type.equals(Integer.class)) {
				if (converter != null) {
					result = converter.fromDisplayValue(stringValue);
				}
				else {
					result = Integer.valueOf(stringValue);
				}
			}
			else if (type.equals(Long.class)) {
				result = Long.valueOf(stringValue);
			}
			else if (type.equals(Float.class)) {
				result = Float.valueOf(stringValue);
			}
			else if (type.equals(Double.class)) {
				result = Double.valueOf(stringValue);
			}
			else if (type.equals(BigDecimal.class)) {
				result = new BigDecimal(stringValue);
			}
			else if (type.equals(Decimal2.class)) {
				result = new Decimal2(stringValue);
			}
			else if (type.equals(Decimal5.class)) {
				result = new Decimal5(stringValue);
			}
			else if (type.equals(Decimal10.class)) {
				result = new Decimal10(stringValue);
			}
			else if (type.equals(Boolean.class)) {
				result = Boolean.valueOf(stringValue.equals("true"));
			}
			else if (type.equals(DateOnly.class)) {
				if (fromSerializedFormat) {
					result = new DateOnly(stringValue);
				}
				else {
					Date date = customer.getDefaultDateConverter().fromDisplayValue(stringValue);
					if (date != null) {
						result = new DateOnly(date.getTime());
					}
				}
			}
			else if (type.equals(TimeOnly.class)) {
				if (fromSerializedFormat) {
					result = new TimeOnly(stringValue);
				}
				else {
					Date date = customer.getDefaultTimeConverter().fromDisplayValue(stringValue);
					if (date != null) {
						result = new TimeOnly(date.getTime());
					}
				}
			}
			else if (type.equals(DateTime.class)) {
				if (fromSerializedFormat) {
					result = new DateTime(stringValue);
				}
				else {
					Date date = customer.getDefaultDateTimeConverter().fromDisplayValue(stringValue);
					if (date != null) {
						result = new DateTime(date.getTime());
					}
				}
			}
			else if (type.equals(Timestamp.class)) {
				if (fromSerializedFormat) {
					result = new Timestamp(stringValue);
				}
				else {
					Date date = customer.getDefaultTimestampConverter().fromDisplayValue(stringValue);
					if (date != null) {
						result = new Timestamp(date.getTime());
					}
				}
			}
			else if (Geometry.class.isAssignableFrom(type)) {
				result = new WKTReader().read(stringValue);
			}
			else if (Date.class.isAssignableFrom(type)) {
				result = new java.sql.Timestamp(CORE.getDateFormat(DEFAULT_DISPLAY_DATE_FORMAT).parse(stringValue).getTime());
			}
			else if (type.equals(OptimisticLock.class)) {
				result = new OptimisticLock(stringValue);
			}
			// NB type.isEnum() doesn't work as our enums implement another interface
			// NB Enumeration.class.isAssignableFrom(type) doesn't work as enums are not assignable as they are a synthesised class
			else if (Enum.class.isAssignableFrom(type)) {
				result = convert(type, stringValue);
			}
			else {
				throw new IllegalStateException("BindUtil.setPropertyFromDisplay() - Can't convert type " + type);
			}
		}
		catch (Exception e) {
			if (e instanceof SkyveException) {
				throw (SkyveException) e;
			}
			throw new DomainException(e);
		}
		
		return result;
	}

	/**
	 * This method is synchronized as {@link Converter#toDisplayValue(Object)} requires synchronization.
	 * 
	 * @param converter Can be <code>null</code>.
	 * @param object
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static synchronized String toDisplay(Customer customer, 
													@SuppressWarnings("rawtypes") Converter converter, 
													List<DomainValue> domainValues, 
													Object value) {
		String result = "";
		try {
			if (value == null) {
				// do nothing as result is already empty
			}
			else if (domainValues != null) {
				if (value instanceof Enumeration) {
					result = ((Enumeration) value).toLocalisedDescription();
				}
				else {
					boolean found = false;
					String codeValue = value.toString();
					for (DomainValue domainValue : domainValues) {
						if (domainValue.getCode().equals(codeValue)) {
							result = domainValue.getLocalisedDescription();
							found = true;
							break;
						}
					}
					if (! found) {
						result = codeValue;
					}
				}
			}
			else if (converter != null) {
				result = converter.toDisplayValue(convert(converter.getAttributeType().getImplementingType(),
															value));
			}
			else if (value instanceof DateOnly) {
				result = customer.getDefaultDateConverter().toDisplayValue((DateOnly) value);
			}
			else if (value instanceof TimeOnly) {
				result = customer.getDefaultTimeConverter().toDisplayValue((TimeOnly) value);
			}
			else if (value instanceof DateTime) {
				result = customer.getDefaultDateTimeConverter().toDisplayValue((DateTime) value);
			}
			else if (value instanceof Timestamp) {
				result = customer.getDefaultTimestampConverter().toDisplayValue((Timestamp) value);
			}
			else if (value instanceof Date) {
				result = CORE.getDateFormat(DEFAULT_DISPLAY_DATE_FORMAT).format((Date) value);
			}
			else if (value instanceof Boolean) {
				result = (((Boolean) value).booleanValue() ? "Yes" : "No");
			}
			else if (value instanceof Geometry) {
				result = new WKTWriter().write((Geometry) value);
			}
			else {
				result = value.toString();
			}
		}
		catch (Exception e) {
			if (e instanceof SkyveException) {
				throw (SkyveException) e;
			}

			throw new DomainException(e);
		}
		return result;
	}

	@SuppressWarnings({"rawtypes", "unchecked"})
	public static Object getSerialized(Customer customer, Bean bean, String binding) {
		Object result = null;
		try {
			result = BindUtil.get(bean, binding);
			String documentName = bean.getBizDocument();
			if (documentName != null) {
				Module module = customer.getModule(bean.getBizModule());
				Document document = module.getDocument(customer, documentName);
				try {
					TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, binding);
					Attribute attribute = target.getAttribute();
					if (attribute instanceof ConvertableField) {
						Converter<?> converter = ((ConvertableField) attribute).getConverterForCustomer(customer);
						if (converter != null) {
							Format format = converter.getFormat();
							if (format != null) {
								result = format.toDisplayValue(result);
							}
						}
					}
				}
				catch (@SuppressWarnings("unused") MetaDataException e) {
					// The binding may be a column alias with no metadata, so do nothing when this occurs
				}
			}
		}
		catch (Exception e) {
			if (e instanceof SkyveException) {
				throw (SkyveException) e;
			}
			throw new DomainException(e);
		}
		
		return result;
	}

	public static String getDisplay(Customer customer, Bean bean, String binding) {
		return getDisplay(customer, bean, binding, get(bean, binding));
	}
	
	public static String getDisplay(Customer customer, Bean bean, String binding, Object value) {
		if (value instanceof Bean) {
			return ((Bean) value).getBizKey();
		}

		Converter<?> converter = null;
		List<DomainValue> domainValues = null;

		String documentName = bean.getBizDocument();
		if (documentName != null) {
			Module module = customer.getModule(bean.getBizModule());
			Document document = module.getDocument(customer, documentName);
			TargetMetaData target = null;
			Attribute attribute = null;
			try {
				target = BindUtil.getMetaDataForBinding(customer, module, document, binding);
				attribute = target.getAttribute();
			}
			catch (@SuppressWarnings("unused") MetaDataException e) {
				// do nothing
			}
			
			if (attribute instanceof Field) {
				Field field = (Field) attribute;
				if (field instanceof ConvertableField) {
					converter = ((ConvertableField) field).getConverterForCustomer(customer);
				}
				DomainType domainType = field.getDomainType();
				if (domainType != null) {
					DocumentImpl internalDocument = (DocumentImpl) document;
					if (DomainType.dynamic.equals(domainType)) {
						// Get the real deal if this is a DynamicBean from a query
						Bean realBean = bean;
						if (bean instanceof DynamicBean) {
							DynamicBean dynamicBean = (DynamicBean) bean;
							if (dynamicBean.isProperty(DynamicBean.BEAN_PROPERTY_KEY)) {
								realBean = (Bean) dynamicBean.get(DynamicBean.BEAN_PROPERTY_KEY);
							}
							else { // no THIS_ALIAS in this DynamicBean (maybe its an app coder's list model)
								realBean = null;
							}
						}
						
						if (realBean != null) {
							int lastDotIndex = binding.lastIndexOf('.');
							if (lastDotIndex >= 0) {
								Bean owningBean = (Bean) get(realBean, binding.substring(0, lastDotIndex));
								if ((owningBean != null) && (target != null)) {
									internalDocument = (DocumentImpl) target.getDocument();
									domainValues = internalDocument.getDomainValues((CustomerImpl) customer, domainType, field, owningBean, true);
								}
							}
							else {
								domainValues = internalDocument.getDomainValues((CustomerImpl) customer, domainType, field, realBean, true);							
							}
						}
					}
					else {
						domainValues = internalDocument.getDomainValues((CustomerImpl) customer, domainType, field, null, true);
					}
				}
			}
		}

		return toDisplay(customer, converter, domainValues, value);
	}

	/**
	 * Given a list of bindings, create a compound one. ie binding1.binding2.binding3 etc
	 * 
	 * @param bindings
	 * @return
	 */
	public static String createCompoundBinding(String... bindings) {
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
	public static String createIndexedBinding(String binding, int index) {
		StringBuilder result = new StringBuilder(64);

		result.append(binding).append('[').append(index).append(']');

		return result.toString();
	}

	/**
	 * Given a multiple cardinality binding and a bizId, create an element one. ie bindingElementById(id)
	 */
	public static String createIdBinding(String binding, String bizId) {
		StringBuilder result = new StringBuilder(64);

		result.append(binding).append("ElementById(").append(bizId).append(')');

		return result.toString();
	}
	
	/**
	 * Replace '.', '[' & ']' with '_' to make valid client identifiers.
	 */
	public static String sanitiseBinding(String binding) {
		String result = null;
		if (binding != null) {
			result = binding.replace('.', '_').replace('[', '_').replace(']', '_');
		}
		return result;
	}

	/**
	 * Replace '_' with either '[', ']' or '_' depending on the context to make valid binding expressions from client identifiers.
	 */
	public static String unsanitiseBinding(String binding) {
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
	public static Bean ensureElementIsInCollection(Bean owner, String binding, Bean element) {
		List<Bean> list = (List<Bean>) get(owner, binding);
		String elementId = element.getBizId();

		// check each bean in the list to see if its ID is the same
		for (Bean existing : list) {
			if (elementId.equals(existing.getBizId())) {
				return existing;
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
	public static Bean getElementInCollection(Bean owner, String binding, String elementBizId) {
		List<Bean> list = (List<Bean>) get(owner, binding);
		return getElementInCollection(list, elementBizId);
	}

	public static <T extends Bean> T getElementInCollection(List<T> list, String elementBizId) {
		// check each bean in the list to see if its ID is the same
		for (T existing : list) {
			if (elementBizId.equals(existing.getBizId())) {
				return existing;
			}
		}

		return null;
	}
	
	public static <T extends Bean> void setElementInCollection(List<T> list, T element) {
		// Set each occurrence in the list where the bizIds are the same
		String elementBizId = element.getBizId();
		for (int i = 0, l = list.size(); i < l; i++) {
			if (elementBizId.equals(list.get(i).getBizId())) {
				list.set(i, element);
			}
		}
	}
	
	private static Pair<Bean, String> penultimateBind(Bean bean, String relationBinding) {
		Bean relationOwner = bean;
		String relationName = relationBinding;
		int dotIndex = relationName.lastIndexOf('.');
		if (dotIndex > -1) {
			relationName = relationName.substring(dotIndex + 1);
			relationOwner = (Bean) BindUtil.get(bean, relationBinding.substring(0, dotIndex));
		}
		return new ImmutablePair<>(relationOwner, relationName);
	}
	
	private static void setElementParent(Customer c, Module m, Document d, Relation r, Bean element, Bean parent) {
		// Set the parent of a child bean, if applicable
		if (element instanceof ChildBean<?>) {
			String relatedDocumentName = r.getDocumentName();
			Document relatedDocument = m.getDocument(c, relatedDocumentName);
			Document parentDocument = relatedDocument.getParentDocument(c);
			if (parentDocument != null) {
				String parentModuleName = parentDocument.getOwningModuleName();
				String parentDocumentName = parentDocument.getName();
	
				// Check if processBean.setParent() can be called or not.
				// The processBean may be a child of some other bean and just being added to another collection here.
				// Or it could be a derived document, so need to check inheritance as well.
				CustomerImpl internalCustomer = (CustomerImpl) c;
				Document parentBeanDocument = d;
				while (parentBeanDocument != null) {
					if (parentModuleName.equals(parentBeanDocument.getOwningModuleName()) &&
							parentDocumentName.equals(parentBeanDocument.getName())) {
						@SuppressWarnings("unchecked")
						ChildBean<Bean> uncheckedNewBean = (ChildBean<Bean>) element;
						uncheckedNewBean.setParent(parent);
						parentBeanDocument = null;
					}
					else {
						String baseDocumentName = internalCustomer.getBaseDocument(parentBeanDocument);
	    				if (baseDocumentName == null) {
	    					parentBeanDocument = null;
	    				}
	    				else {
	        				int dotIndex = baseDocumentName.indexOf('.');
	        				Module baseModule = c.getModule(baseDocumentName.substring(0, dotIndex));
	        				parentBeanDocument = baseModule.getDocument(c, baseDocumentName.substring(dotIndex + 1));
	    				}
					}
				}
			}
		}
	}
	
	private static void setRelationInverse(Customer c, Document d, Relation r, Bean owner, String relationName, Bean value, boolean remove) {
		String attributeName = null;
		if (r instanceof Inverse) { // we just set the inverse
			attributeName = ((Inverse) r).getReferenceName();
		}
		else { // lets see if there is an inverse on the element side
			Module elementModule = c.getModule(value.getBizModule());
			Document elementDocument = elementModule.getDocument(c, value.getBizDocument());
			String ownerDocumentName = d.getName(); // owner could be null
			for (Attribute a : elementDocument.getAllAttributes(c)) {
				if (a instanceof Inverse) {
					Inverse i = (Inverse) a;
					if (ownerDocumentName.equals(i.getDocumentName()) && relationName.equals(i.getReferenceName())) {
						attributeName = i.getName();
						break;
					}
				}
			}
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
	
	/**
	 * Call a Bean's association setter method.
	 * @param bean	The owning bean.
	 * @param associationBinding	The binding to the association.
	 * @param value	The value to set.
	 */
	public static void setAssociation(Bean bean, String associationBinding, Bean value) {
		try {
			Pair<Bean, String> args = penultimateBind(bean, associationBinding);
			Bean associationOwner = args.getLeft();
			String associationName = args.getRight();
			
			Customer c = CORE.getCustomer();
			Module m = c.getModule(associationOwner.getBizModule());
			Document d = m.getDocument(c, associationOwner.getBizDocument());
			Attribute a = d.getPolymorphicAttribute(c, associationName);

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
			if (e instanceof SkyveException) {
				throw (SkyveException) e;
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
	public static boolean addElementToCollection(Bean bean, String collectionBinding, Bean element) {
		try {
			Pair<Bean, String> args = penultimateBind(bean, collectionBinding);
			Bean collectionOwner = args.getLeft();
			String collectionName = args.getRight();

			Customer c = CORE.getCustomer();
			Module m = c.getModule(collectionOwner.getBizModule());
			Document d = m.getDocument(c, collectionOwner.getBizDocument());
			Attribute a = d.getPolymorphicAttribute(c, collectionName);

			// Dynamic collection - make it happen here with no convenience method
			if (BindUtil.isDynamic(c, m, d, a)) {
				Relation r = (Relation) a;
				setElementParent(c, m, d, r, element, collectionOwner);
				setRelationInverse(c, d, r, collectionOwner, collectionName, element, false);
				@SuppressWarnings("unchecked")
				List<Bean> list = (List<Bean>) BindUtil.get(collectionOwner, collectionName);
				return list.add(element);
			}

			// Static collection - use the add method
			StringBuilder sb = new StringBuilder(collectionName.length() + 10);
			sb.append("add").append(Character.toUpperCase(collectionName.charAt(0))).append(collectionName.substring(1)).append("Element");
			String methodName = sb.toString();
			
			// NB - cant use getMethod directly as element may not be the exact class - ie dynamic proxy subclass etc
			// Method m = collectionOwner.getClass().getMethod(methodName.toString(), element.getClass());
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
			if (e instanceof SkyveException) {
				throw (SkyveException) e;
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
	public static void addElementToCollection(Bean bean, String collectionBinding, int index, Bean element) {
		try {
			Pair<Bean, String> args = penultimateBind(bean, collectionBinding);
			Bean collectionOwner = args.getLeft();
			String collectionName = args.getRight();

			Customer c = CORE.getCustomer();
			Module m = c.getModule(collectionOwner.getBizModule());
			Document d = m.getDocument(c, collectionOwner.getBizDocument());
			Attribute a = d.getPolymorphicAttribute(c, collectionName);

			// Dynamic collection - make it happen here with no convenience method
			if (BindUtil.isDynamic(c, m, d, a)) {
				Relation r = (Relation) a;
				setElementParent(c, m, d, r, element, collectionOwner);
				setRelationInverse(c, d, r, collectionOwner, collectionName, element, false);
				@SuppressWarnings("unchecked")
				List<Bean> list = (List<Bean>) BindUtil.get(collectionOwner, collectionName);
				list.add(index, element);
				return;
			}

			// Static collection - use the add method
			StringBuilder sb = new StringBuilder(collectionName.length() + 10);
			sb.append("add").append(Character.toUpperCase(collectionName.charAt(0))).append(collectionName.substring(1)).append("Element");
			String methodName = sb.toString();
			
			// NB - cant use getMethod directly as element may not be the exact class - ie dynamic proxy subclass etc
			// Method m = collectionOwner.getClass().getMethod(methodName.toString(), Integer.TYPE, element.getClass());
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
			if (e instanceof SkyveException) {
				throw (SkyveException) e;
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
	public static boolean removeElementFromCollection(Bean bean, String collectionBinding, Bean element) {
		try {
			Pair<Bean, String> args = penultimateBind(bean, collectionBinding);
			Bean collectionOwner = args.getLeft();
			String collectionName = args.getRight();

			Customer c = CORE.getCustomer();
			Module m = c.getModule(collectionOwner.getBizModule());
			Document d = m.getDocument(c, collectionOwner.getBizDocument());
			Attribute a = d.getPolymorphicAttribute(c, collectionName);

			// Dynamic collection - make it happen here with no convenience method
			if (BindUtil.isDynamic(c, m, d, a)) {
				Relation r = (Relation) a;
				setElementParent(c, m, d, r, element, null);
				setRelationInverse(c, d, r, collectionOwner, collectionName, element, true);
				@SuppressWarnings("unchecked")
				List<Bean> list = (List<Bean>) BindUtil.get(collectionOwner, collectionName);
				return list.remove(element);
			}

			// Static collection - use the remove method
			StringBuilder sb = new StringBuilder(collectionName.length() + 13);
			sb.append("remove").append(Character.toUpperCase(collectionName.charAt(0))).append(collectionName.substring(1)).append("Element");
			String methodName = sb.toString();
			
			// NB - cant use getMethod directly as element may not be the exact class - ie dynamic proxy subclass etc
			// Method m = collectionOwner.getClass().getMethod(methodName.toString(), element.getClass());
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
			if (e instanceof SkyveException) {
				throw (SkyveException) e;
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
	public static <T extends Bean> T removeElementFromCollection(Bean bean, String collectionBinding, int index) {
		try {
			Pair<Bean, String> args = penultimateBind(bean, collectionBinding);
			Bean collectionOwner = args.getLeft();
			String collectionName = args.getRight();

			Customer c = CORE.getCustomer();
			Module m = c.getModule(collectionOwner.getBizModule());
			Document d = m.getDocument(c, collectionOwner.getBizDocument());
			Attribute a = d.getPolymorphicAttribute(c, collectionName);
			
			// Dynamic collection - make it happen here with no convenience method
			if (BindUtil.isDynamic(c, m, d, a)) {
				@SuppressWarnings("unchecked")
				List<T> list = (List<T>) BindUtil.get(collectionOwner, collectionName);
				T element = list.get(index);
				Relation r = (Relation) a;
				setElementParent(c, m, d, r, element, null);
				setRelationInverse(c, d, r, collectionOwner, collectionName, element, true);
				return list.remove(index);
			}

			// Static collection - use the remove method
			StringBuilder methodName = new StringBuilder(collectionName.length() + 13);
			methodName.append("remove").append(Character.toUpperCase(collectionName.charAt(0))).append(collectionName.substring(1)).append("Element");
			Method method = collectionOwner.getClass().getMethod(methodName.toString(), Integer.TYPE);
			@SuppressWarnings("unchecked")
			T result = (T) method.invoke(collectionOwner, Integer.valueOf(index));
			return result;
		}
		catch (Exception e) {
			if (e instanceof SkyveException) {
				throw (SkyveException) e;
			}
			throw new DomainException(e);
		}
	}

	/**
	 * Sort a collection by its order metadata.
	 * 
	 * @param bean The bean that ultimately has the collection.
	 * @param customer The customer of the owningBean.
	 * @param module The module of the owningBean.
	 * @param document The document of the owningBean.
	 * @param collectionBinding The (possibly compound) collection binding (from Document context).
	 */
	public static void sortCollectionByMetaData(Bean bean,
													Customer customer,
													Module module,
													Document document,
													String collectionBinding) {
		// Cater for compound bindings here
		Bean owningBean = bean;
		int lastDotIndex = collectionBinding.lastIndexOf('.'); // compound binding
		if (lastDotIndex > 0) {
			owningBean = (Bean) BindUtil.get(owningBean, collectionBinding.substring(0, lastDotIndex));
		}
		TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, collectionBinding);
		Attribute targetCollection = target.getAttribute();
		if (targetCollection instanceof Collection) {
			sortCollectionByMetaData(owningBean, (Collection) targetCollection);
		}
	}

	/**
	 * Sort the beans collection by the order metadata provided.
	 * 
	 * @param owningBean The bean with collection in it.
	 * @param collection The metadata representing the collection. 
	 * 						This method does not cater for compound binding expressions.
	 * 						Use {@link sortCollectionByMetaData(Customer, Module, Document, Bean, String)} for that.
	 */
	@SuppressWarnings("unchecked")
	public static void sortCollectionByMetaData(Bean owningBean, Collection collection) {
		// We only sort by ordinal if this is a child collection as bizOrdinal is on the elements.
		// For aggregation/composition, bizOrdinal is on the joining table and handled automatically
		boolean sortByOrdinal = Boolean.TRUE.equals(collection.getOrdered()) && 
														(CollectionType.child.equals(collection.getType()));
		if (sortByOrdinal || (! collection.getOrdering().isEmpty())) {
			List<Bean> details = (List<Bean>) BindUtil.get(owningBean, collection.getName());
			sortCollectionByOrdering(details,
										sortByOrdinal,
										collection.getOrdering());
		}
	}
	
	/**
	 * Sort a collection of java beans by an arbitrary ordering list.
	 * The list can be of any type, not just Bean.
	 * 
	 * @param beans	The list to sort
	 * @param ordering The sort order
	 */
	public static void sortCollectionByOrdering(List<?> beans, Ordering... ordering) {
		sortCollectionByOrdering(beans, false, Arrays.asList(ordering));
	}
	
	@SuppressWarnings("unchecked")
	private static void sortCollectionByOrdering(List<?> beans, 
													boolean finallySortByOrdinal, 
													List<Ordering> ordering) {
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
				if (smallerBean != null) {
					if (comparatorChain.compare(smallerBean, bean) > 0) {
						unsorted = true;
						break;
					}
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
	public static Object get(Object bean, String binding) {
		if ((bean instanceof DynamicBean) && ((DynamicBean) bean).isProperty(binding)) {
			return ((DynamicBean) bean).get(binding);
		}

		Object result = null;
		Object currentBean = bean;
		StringTokenizer tokenizer = new StringTokenizer(binding, ".");
		while (tokenizer.hasMoreTokens()) {
			String simpleBinding = tokenizer.nextToken();
			try {
				if (currentBean instanceof Bean) {
					Bean b = (Bean) currentBean;
					String attributeName = simpleBinding;
					boolean indexed = false;
					int braceIndex = simpleBinding.indexOf('[');
					if (braceIndex < 0) {
						braceIndex = simpleBinding.indexOf("ElementById(");
					}
					else {
						indexed = true;
					}
					if (braceIndex >= 0) {
						attributeName = simpleBinding.substring(0, braceIndex);
					}
					if (b.isDynamic(attributeName)) {
						result = b.getDynamic(attributeName);
						if ((result != null) && (braceIndex > 0)) {
							@SuppressWarnings("unchecked")
							List<? extends Bean> list = (List<? extends Bean>) result;
							if (indexed) {
								int index = Integer.parseInt(simpleBinding.substring(braceIndex + 1, simpleBinding.length() - 1)); // substring between '[' and ']'
								result = list.get(index);
							}
							else { // by Id
								String bizId = simpleBinding.substring(braceIndex + 12, simpleBinding.length() - 1); // substring between '(' and ')'
								result = list.stream().filter(e -> bizId.equals(e.getBizId())).findFirst().orElse(null);
							}
						}
					}
					else {
						result = PROPERTY_UTILS.getProperty(currentBean, simpleBinding);
					}
				}
				else {
					result = PROPERTY_UTILS.getProperty(currentBean, simpleBinding);
				}
			}
			catch (Exception e) {
				UtilImpl.LOGGER.severe("Could not BindUtil.get(" + bean + ", " + binding + ")!");
				UtilImpl.LOGGER.severe("The subsequent stack trace relates to obtaining bean property " + simpleBinding + " from " + currentBean);
				UtilImpl.LOGGER.severe("If the stack trace contains something like \"Unknown property '" + simpleBinding + 
										"' on class 'class <blahblah>$$EnhancerByCGLIB$$$<blahblah>'\"" + 
										" then you'll need to use Util.deproxy() before trying to bind to properties in the hibernate proxy.");
				UtilImpl.LOGGER.severe("See https://github.com/skyvers/skyve-cookbook/blob/master/README.md#deproxy for details");
				UtilImpl.LOGGER.severe("Exception message = " + e.getMessage());
				throw new MetaDataException(e);
			}

			if (result == null) {
				break;
			}

			currentBean = result;
		}

		return result;
	}

	@SuppressWarnings("unchecked")
	public static Object get(Map<String, Object> map, String binding) {
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
	
	public static void convertAndSet(Object bean, String binding, Object value) {
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
	public static void set(Object bean, String binding, Object value) {
		try {
			Object valueToSet = value;
			// empty strings to null
			if ((valueToSet != null) && valueToSet.equals("")) {
				valueToSet = null;
			}
	
			if ((bean instanceof DynamicBean) && ((DynamicBean) bean).isProperty(binding)) {
				((DynamicBean) bean).set(binding, valueToSet);
			}
			else {
				// Get the penultimate object to ensure we traverse static and dynamic beans correctly
				String simpleBinding = binding;
				Object penultimate = bean;
				int lastDotIndex = binding.lastIndexOf('.');
				if (lastDotIndex >= 0) {
					penultimate = get(bean, binding.substring(0, lastDotIndex));
					simpleBinding = binding.substring(lastDotIndex + 1);
				}

				if (valueToSet != null) {
					Class<?> propertyType = BindUtil.getPropertyType(penultimate, simpleBinding);
		
					// if we are setting a String value to a non-string property then
					// use an appropriate constructor or static valueOf()
					// NB ensure we are not dealing with Object.clas as a property type.
					// 	Returned by getPropertyType either directly if bean is dynamic or through apache BeanUtils
					if (String.class.equals(valueToSet.getClass()) && 
							(! String.class.equals(propertyType)) && 
							(! Object.class.equals(propertyType))) {
						try {
							valueToSet = propertyType.getConstructor(valueToSet.getClass()).newInstance(valueToSet);
						}
						catch (@SuppressWarnings("unused") NoSuchMethodException e) {
							try {
								valueToSet = propertyType.getMethod("valueOf", String.class).invoke(null, valueToSet);
							}
							catch (@SuppressWarnings("unused") NoSuchMethodException e1) {
								throw new DomainException("Cannot coerce String value " + valueToSet + " to type " + propertyType + " for setting for binding " + binding + " on bean " + bean);
							}
						}
					}
			
					// Convert the value to String if required
					if (String.class.equals(propertyType)) {
						valueToSet = valueToSet.toString();
					} // if (we have a String property)
				}
				
				// Set static and dynamic beans
				if (penultimate instanceof Bean) {
					Bean b = (Bean) penultimate;
					String attributeName = simpleBinding;
					boolean indexed = true;
					int braceIndex = simpleBinding.indexOf('[');
					if (braceIndex < 0) {
						indexed = false;
						braceIndex = simpleBinding.indexOf("ElementById(");
					}
					if (braceIndex >= 0) {
						attributeName = simpleBinding.substring(0, braceIndex);
					}
					if (b.isDynamic(attributeName)) {
						if (braceIndex < 0) {
							b.setDynamic(simpleBinding, valueToSet);
						}
						else {
							if (indexed) {
								@SuppressWarnings("unchecked")
								List<Object> list = (List<Object>) b.getDynamic(attributeName);
								if (list != null) {
									int index = Integer.parseInt(simpleBinding.substring(braceIndex + 1, simpleBinding.length() - 1));
									list.set(index, valueToSet);
								}
								else {
									throw new IllegalStateException("Attempt to set " + binding + " in " + bean + " to " + valueToSet + " but the list is null");
								}
							}
							else { // by Id
								if (valueToSet instanceof Bean) {
									@SuppressWarnings("unchecked")
									List<Bean> list = (List<Bean>) b.getDynamic(attributeName);
									if (list != null) {
										String bizId = simpleBinding.substring(braceIndex + 12, simpleBinding.length() - 1);
										Bean result = list.stream().filter(e -> bizId.equals(e.getBizId())).findFirst().orElse(null);
										if (result != null) {
											int index = list.indexOf(result);
											list.set(index, (Bean) valueToSet);
										}
										else {
											throw new IllegalStateException("Attempt to set " + binding + " in " + bean + " to " + valueToSet + " but the element was not in the list");
										}
									}
									else {
										throw new IllegalStateException("Attempt to set " + binding + " in " + bean + " to " + valueToSet + " but the list is null");
									}
								}
								else {
									throw new IllegalStateException("Attempt to set " + binding + " in " + bean + " to " + valueToSet + " but valueToSet should be a Bean");
								}
							}
						}
					}
					else {
						PROPERTY_UTILS.setProperty(penultimate, simpleBinding, valueToSet);
					}
				}
				// Set anything else
				else {
					PROPERTY_UTILS.setProperty(penultimate, simpleBinding, valueToSet);
				}
			}
		}
		catch (Exception e) {
			if (e instanceof SkyveException) {
				throw (SkyveException) e;
			}
			
			throw new MetaDataException(e);
		}
	}

	public static Class<?> getPropertyType(Object bean, String binding) {
		if (bean instanceof Bean) {
			Bean b = (Bean) bean;

			// NB true if a DynamicBean or where the binding is to a dynamic attribute name
			boolean dynamic = b.isDynamic(binding);
			int lastDotIndex = binding.lastIndexOf('.');
			String attributeName = null;
			if (lastDotIndex < 0) { // not compound binding
				attributeName = binding;
				// If not dynamic, remove any braces on the expression and re-test if its dynamic
				if (! dynamic) {
					int braceIndex = binding.lastIndexOf('[');
					if (braceIndex < 0) {
						braceIndex = binding.indexOf("ElementById(");
					}
					if (braceIndex >= 0) {
						attributeName = binding.substring(0, braceIndex);
						dynamic = b.isDynamic(attributeName);
					}
				}
			}
			
			if (dynamic) {
				Object value = get(b, binding);
				if (value == null) {
					if (attributeName != null) { // not compound binding - possible attribute
						// Get the property type via the document
						Customer c = CORE.getCustomer();
						Module m = c.getModule(b.getBizModule());
						Document d = m.getDocument(c, b.getBizDocument());
						Attribute a = d.getPolymorphicAttribute(c, attributeName);
						if (a != null) {
							try {
								// Dynamic enumerations are strings, otherwise get the Enum class
								if (a instanceof org.skyve.impl.metadata.model.document.field.Enumeration) {
									org.skyve.impl.metadata.model.document.field.Enumeration e = (org.skyve.impl.metadata.model.document.field.Enumeration) a;
									e = e.getTarget();
									if (e.isDynamic()) {
										return String.class;
									}
									return e.getEnum();
								}
								// binding expression to Association or InverseOne
								if ((a instanceof Association) || (a instanceof InverseOne)) {
									d = m.getDocument(c, ((Reference) a).getDocumentName());
									return ((DocumentImpl) d).getBeanClass(c);
								}
								// indexed or mapped binding expression to Collection or InverseMany
								if ((! attributeName.equals(binding)) && 
										((a instanceof Collection) || (a instanceof InverseMany))) {
									d = m.getDocument(c, ((Reference) a).getDocumentName());
									return ((DocumentImpl) d).getBeanClass(c);
								}
							}
							catch (Exception e) {
								throw new MetaDataException(e);
							}
							
							return a.getAttributeType().getImplementingType();
						}
					}
					
					return Object.class;
				}
				return value.getClass();
			}

			// Could be a compound, indexed or mapped binding if we reach here
			if (lastDotIndex >= 0) { // compound binding
				// Navigate through static and dynamic beans as required to the penultimate object, then find the property type
				Object penultimate = get(bean, binding.substring(0, lastDotIndex));
				return getPropertyType(penultimate, binding.substring(lastDotIndex + 1));
			}
		}

		try {
			return PROPERTY_UTILS.getPropertyType(bean, binding);
		}
		catch (Exception e) {
			throw new MetaDataException(e);
		}
	}

	public static boolean isMutable(Object bean, String simplePropertyName) {
		if (bean instanceof Bean) {
			if (Bean.MODULE_KEY.equals(simplePropertyName) || Bean.DOCUMENT_KEY.equals(simplePropertyName) ||
					Bean.CREATED_KEY.equals(simplePropertyName) || Bean.NOT_CREATED_KEY.equals(simplePropertyName) ||
					Bean.CHANGED_KEY.equals(simplePropertyName) || Bean.NOT_CHANGED_KEY.equals(simplePropertyName) ||
					Bean.PERSISTED_KEY.equals(simplePropertyName) || Bean.NOT_PERSISTED_KEY.equals(simplePropertyName) ||
					Bean.BIZ_KEY.equals(simplePropertyName)) {
				return false;
			}
			
			Bean b = (Bean) bean;
			boolean dynamic = b.isDynamic(simplePropertyName);
			if ((b instanceof DynamicBean) && (! dynamic)) {
				throw new IllegalStateException(simplePropertyName + " is not a property of " + bean);
			}
			
			if (dynamic) {
				if (List.class.isAssignableFrom(BindUtil.getPropertyType(bean, simplePropertyName))) {
					return false;
				}
				return true;
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
	public static final boolean isAScalarType(Class<?> propertyType) {
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
	public static final boolean isImplicit(String attributeName) {
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
	public static final Class<?> implicitAttributeType(String attributeName) {
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
	public static boolean isDynamic(Customer customer, Module module, Document document, Attribute attribute) {
		boolean result = document.isDynamic();
		if (! result) {
			result = isDynamic(customer, module, attribute);
		}
		return result;
	}
	
	/**
	 * See Binder.isDynamic().
	 */
	public static boolean isDynamic(Customer customer, Module module, Attribute attribute) {
		if (attribute instanceof Field) {
			return ((Field) attribute).isDynamic();
		}
		if (attribute instanceof Relation) {
			return isDynamic(customer, module, (Relation) attribute);
		}
		return false;
	}
	
	/**
	 * See Binder.isDynamic().
	 */
	public static boolean isDynamic(Customer customer, Module module, Relation relation) {
		String dn = relation.getDocumentName();
		Document rd = module.getDocument(customer, dn);
		return rd.isDynamic();
	}
	
	// NB properties must be a sorted map to ensure that the shortest properties
	// are processed first - ie User.contact is populated before User.contact.firstName,
	// otherwise the firstName new value will be tromped...
	public static void populateProperties(User user, 
											Bean bean, 
											SortedMap<String, Object> properties, 
											boolean fromSerializedFormat) {
		ValidationException e = new ValidationException();

		// Do nothing unless both arguments have been specified
		if ((bean == null) || (properties == null)) {
			return;
		}

		// Loop through the property name/value pairs to be set
		for (String name : properties.keySet()) {
			// Identify the property name and value(s) to be assigned
			if (name == null) {
				continue;
			}
			Object value = properties.get(name);

			// Perform the assignment for this property
			try {
				populateProperty(user, bean, name, value, fromSerializedFormat);
			}
			catch (Exception ex) {
				System.err.println("Exception thrown when populating from the request.");
				ex.printStackTrace();
				e.getMessages().add(new Message(name, value + " is invalid."));
			}
		}

		if (! e.getMessages().isEmpty()) {
			throw e;
		}
	}

	public static void populateProperty(User user, 
											Bean bean, 
											String bindingName, 
											Object bindingValue, 
											boolean fromSerializedFormat) {
		String name = bindingName;
		Object value = bindingValue;
		
		Customer customer = user.getCustomer();
		Module beanModule = customer.getModule(bean.getBizModule());
		Document beanDocument = beanModule.getDocument(customer, bean.getBizDocument());

		// Resolve any nested expression to get the actual target bean
		Object target = bean;
		int delim = findLastNestedIndex(name);
		String targetBinding = null;
		if (delim >= 0) {
			targetBinding = name.substring(0, delim);
			target = BindUtil.instantiateAndGet(user, beanModule, beanDocument, bean, targetBinding);
			name = name.substring(delim + 1);
		}

		if (target == null) {
			if ((value == null) || value.equals("") || (targetBinding == null)) {
				return;
			}
		}

		// Declare local variables we will require
		String propName = null; // Simple name of target property
		Class<?> type = null; // Java type of target property
		int index = -1; // Indexed subscript value (if any)
		String key = null; // Mapped key value (if any)

		// Calculate the property name, index, and key values
		propName = name;
		int i = propName.indexOf('[');
		if (i >= 0) {
			int k = propName.indexOf(']');
			try {
				index = Integer.parseInt(propName.substring(i + 1, k));
			}
			catch (@SuppressWarnings("unused") NumberFormatException e) {
				// do nothing
			}
			propName = propName.substring(0, i);
		}
		int j = propName.indexOf('(');
		if (j >= 0) {
			int k = propName.indexOf(')');
			try {
				key = propName.substring(j + 1, k);
			}
			catch (@SuppressWarnings("unused") IndexOutOfBoundsException e) {
				// do nothing
			}
			propName = propName.substring(0, j);
		}

		try {
			type = getPropertyType(target, propName);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return;
		}
		
		if ((! List.class.isAssignableFrom(type)) && (! BindUtil.isMutable(target, propName))) {
			return;
		}

		Converter<?> converter = null;

		// Calculate the property type
		if (target instanceof Bean) {
			Bean targetBean = (Bean) target;

			String documentName = targetBean.getBizDocument();
			if (documentName != null) {
				Module module = customer.getModule(targetBean.getBizModule());
				Document document = module.getDocument(customer, documentName);
				// NB Use getMetaDataForBinding() to ensure we find attributes from base documents inherited
				TargetMetaData propertyTarget = BindUtil.getMetaDataForBinding(customer, module, document, propName);
				Attribute attribute = propertyTarget.getAttribute();
				if (attribute instanceof ConvertableField) {
					converter = ((ConvertableField) attribute).getConverterForCustomer(user.getCustomer());
				}
				else if (attribute instanceof Collection) {
					// ensure that collection elements are filled for the binding
					BindUtil.instantiateAndGet(user, beanModule, beanDocument, bean, name);
				}
				else { // id of a reference here
					if ("".equals(value)) {
						value = null;
					}
				}
			}
		}

		// Convert the specified value to the required type
		Object newValue = null;

		String stringValue = null;
		if (value instanceof String) {
			stringValue = ((String) value).trim();
		}
		else if (value instanceof String[]) {
			stringValue = ((String[]) value)[0];
			if (stringValue != null) {
				stringValue = stringValue.trim();
			}
		}
		if (stringValue != null) {
			if (! stringValue.isEmpty()) {
				newValue = fromString(user.getCustomer(), converter, type, stringValue, fromSerializedFormat);
			}
		}
		else {
			newValue = value;
		}

		// Invoke the setter method
		try {
			if (index >= 0) {
				PROPERTY_UTILS.setIndexedProperty(target, propName, index, newValue);
			}
			else if (key != null) {
				PROPERTY_UTILS.setMappedProperty(target, propName, key, newValue);
			}
			else {
				convertAndSet(target, propName, newValue);
			}
		}
		catch (Exception e) {
			throw new MetaDataException("Cannot set " + propName, e);
		}
	}

	private static int findLastNestedIndex(String expression) {
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

			case '(':
			case '[':
				// not bothered which
				--bracketCount;
				break;

			case ')':
			case ']':
				// not bothered which
				++bracketCount;
				break;
			default:
			}
		}

		// can't find any
		return -1;
	}

	public static void copy(final Bean from, final Bean to) {
		final Customer c = CORE.getUser().getCustomer();
		final Module m = c.getModule(from.getBizModule());
		final Document d = m.getDocument(c, from.getBizDocument());

		for (final Attribute attribute : d.getAllAttributes(c)) {
			final String attributeName = attribute.getName();

			if (AttributeType.collection.equals(attribute.getAttributeType())) {
				copyCollection(from,
								to,
								attributeName,
								CollectionType.child.equals(((Collection) attribute).getType()));
				continue;
			}

			if (AttributeType.inverseMany.equals(attribute.getAttributeType())) {
				copyCollection(from, to, attributeName, false);
				continue;
			}

			Binder.set(to, attributeName, Binder.get(from, attributeName));
		}
	}

	// TODO clearing colTo issues a delete statement in hibernate, this method should process each collection item.
	@SuppressWarnings("unchecked")
	private static void copyCollection(final Bean from,
										final Bean to,
										final String attributeName,
										boolean reparent) {
		List<Bean> colFrom = (List<Bean>) BindUtil.get(from, attributeName);
		List<Bean> colTo = (List<Bean>) BindUtil.get(to, attributeName);
		colTo.clear();
		colTo.addAll(colFrom);
		if (reparent) {
			colTo.forEach(e -> ((ChildBean<Bean>) e).setParent(to));
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

		StringTokenizer tokenizer = new StringTokenizer(binding, ".");
		while (tokenizer.hasMoreTokens()) {
			String fieldName = tokenizer.nextToken();
			String parentDocumentName = null;
			if (fieldName.equals(ChildBean.PARENT_NAME)) {
				parentDocumentName = navigatingDocument.getParentDocumentName();
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
			}
			int openBraceIndex = fieldName.indexOf('[');
			if (openBraceIndex > -1) {
				fieldName = fieldName.substring(0, openBraceIndex);
			}
			int openParenthesisIndex = fieldName.indexOf("ElementById(");
			if (openParenthesisIndex > -1) {
				fieldName = fieldName.substring(0, openParenthesisIndex);
			}
			attribute = navigatingDocument.getAttribute(fieldName);
			Extends inherits = navigatingDocument.getExtends();
			while ((attribute == null) && (inherits != null)) {
				Document baseDocument = navigatingModule.getDocument(customer, inherits.getDocumentName());
				attribute = baseDocument.getAttribute(fieldName);
				inherits = baseDocument.getExtends();
			}
			if (tokenizer.hasMoreTokens()) {
				if (ChildBean.PARENT_NAME.equals(fieldName)) {
					if (parentDocumentName == null) {
						throw new MetaDataException(navigatingDocument.getOwningModuleName() + '.' + 
														navigatingDocument.getName() + " @ " + binding + 
														" does not exist (token [parent] doesn't check out as " + navigatingDocument.getName() + 
														" is not a child document)");
					}
					navigatingDocument = navigatingModule.getDocument(customer, parentDocumentName);
				}
				else if (attribute instanceof Relation) {
					navigatingDocument = navigatingModule.getDocument(customer, ((Relation) attribute).getDocumentName());
				}
				else {
					throw new MetaDataException(navigatingDocument.getOwningModuleName() + '.' + 
													navigatingDocument.getName() + " @ " + binding + 
													" does not exist (token [" + 
													fieldName +
													"] doesn't check out)");
				}
				navigatingModule = (customer == null) ?
										ProvidedRepositoryFactory.get().getModule(null, navigatingDocument.getOwningModuleName()) :
										customer.getModule(navigatingDocument.getOwningModuleName());
			}
			else {
				// ignore validating implicit attributes
				if ((attribute == null) && (! isImplicit(fieldName))) {
					throw new MetaDataException(navigatingDocument.getOwningModuleName() + '.' + 
													navigatingDocument.getName() + " @ " + binding + 
													" does not exist (last attribute not in document)");
				}
			}
		}

		return new TargetMetaData(navigatingDocument, attribute);
	}

	@SuppressWarnings("unchecked")
	public static Object instantiateAndGet(User user, Module module, Document document, Bean bean, String binding) {
		Customer customer = user.getCustomer();
		Document navigatingDocument = document;
		Attribute attribute = null;
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

			// Cater for the "parent" property
			if (ChildBean.PARENT_NAME.equals(fieldName)) {
				navigatingDocument = navigatingDocument.getParentDocument(customer);
				if (navigatingDocument == null) {
					throw new MetaDataException(binding + " should point to a parent document but parent document does not exist");
				}
			}
			else {
				// NB Use getMetaDataForBinding() to ensure we find attributes from base documents inherited
				TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, navigatingDocument, fieldName);
				attribute = target.getAttribute();
				if (attribute == null) {
					throw new IllegalArgumentException(fieldName + " within " + binding + "doesn't check out");
				}
				navigatingDocument = module.getDocument(customer, ((Relation) attribute).getDocumentName());
				if ((tokenizer.hasMoreTokens()) && (attribute instanceof Relation)) {
					// do nothing here
				}
				else {
					if (tokenizer.hasMoreTokens()) {
						throw new MetaDataException(binding + 
														" does not exist (token " + 
														tokenizer.nextToken() +
														" doesn't check out)");
					}
				}
			}

			try {
				try {
					value = BindUtil.get(owner, bindingPart);
				}
				catch (@SuppressWarnings("unused") IndexOutOfBoundsException e) {
					if (attribute != null) {
						Document collectionDocument = module.getDocument(customer, ((Relation) attribute).getDocumentName());
	
						// Get the collection and add the new instance
						int closedBraceIndex = bindingPart.length() - 1;
						String collectionBinding = bindingPart.substring(0, openBraceIndex);
						int collectionIndex = Integer.parseInt(bindingPart.substring(openBraceIndex + 1, closedBraceIndex));
						List<Bean> collection = (List<Bean>) BindUtil.get(owner, collectionBinding);
	
						// parameters are ordered alphabetically,
						// so we should receive the array bindings in ascending order
						// but they may not be contiguous (some are deleted)
						while (collection.size() <= collectionIndex) {
							Bean fillerElement = collectionDocument.newInstance(user);
							BindUtil.addElementToCollection((Bean) owner, collectionBinding, fillerElement);
/*
							if (fillerElement instanceof ChildBean) {
								try {
									((ChildBean<Bean>) fillerElement).setParent((Bean) owner);
								}
								catch (@SuppressWarnings("unused") ClassCastException cce) {
									// continue on - this child bean is being linked to
									// by some other document as an "aggregation"
									// IE the owner variable above is not the parent document
									// but some aggregating document
								}
							}
							collection.add(fillerElement);
*/
						}
	
						value = collection.get(collectionIndex);
					}
				}
				if (value == null) {
					value = navigatingDocument.newInstance(user);
					BindUtil.setAssociation((Bean) owner, bindingPart, (Bean) value);
				}
				owner = value;
			}
			catch (Exception e) {
				throw new MetaDataException(e);
			}
		}

		return value;
	}
	
	/**
	 * Fashion a type identifier from the given string.
	 * @param string
	 * @return	A valid java type identifier.  Title case.
	 */
	public static String toJavaTypeIdentifier(String string) {
		StringBuilder sb = new StringBuilder(toJavaInstanceIdentifier(string));
		sb.setCharAt(0, Character.toUpperCase(sb.charAt(0)));
		return sb.toString();
	}
	
	/**
	 * Fashion an instance identifier from the given string.
	 * @param string
	 * @return	A valid java instance identifier.  Camel case.
	 */
	public static String toJavaInstanceIdentifier(String string)
	{
		StringBuilder sb = new StringBuilder(string);
		removeOrReplaceInvalidCharacters(sb);
		return Introspector.decapitalize(sb.toString());
	}
	
	/**
	 * Return a java bean property name from a reflected method name.
	 * @param methodName	The method name
	 * @return	"get"/"set"/"is" prefix stripped and the remaining string decapitalised as appropriate.
	 */
	public static String toJavaPropertyName(String methodName) {
		String propertyName = methodName;
		if (methodName.startsWith("get") || methodName.startsWith("set")) {
			propertyName = Introspector.decapitalize(methodName.substring(3));
		}
		else if (methodName.startsWith("is")) {
			propertyName = Introspector.decapitalize(methodName.substring(2));
		}
		return propertyName;
	}
	
	/**
	 * Fashion a static identifier from the given string.
	 * @param string
	 * @return A valid java static identifier.  Upper Case with underscores.
	 */
	public static String toJavaStaticIdentifier(String string) {
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
	public static String toTitleCase(String string) {
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

	private static void removeOrReplaceInvalidCharacters(StringBuilder sb) {
		int i = 0;
		boolean whiteSpaceOrUnderscore = false;
		while (i < sb.length()) {
			char charAt = sb.charAt(i);
			if (i == 0) {
				// Allow java start char except '_' which is reserved in skyve
				// plus digits which are converted to words below.
				if ((Character.isJavaIdentifierStart(charAt) && (charAt != '_')) || Character.isDigit(charAt)) {
					i++;
				}
				else {
					sb.deleteCharAt(0);
				}
			}
			else {
				if (Character.isJavaIdentifierPart(charAt) && (charAt != '_')) {
					if (whiteSpaceOrUnderscore) {
						sb.setCharAt(i, Character.toUpperCase(charAt));
					}
					whiteSpaceOrUnderscore = false;
					i++;
				}
				else {
					if (Character.isWhitespace(charAt) || (charAt == '_')) {
						whiteSpaceOrUnderscore = true;
					}
					sb.deleteCharAt(i);
				}
			}
		}
		
		if (sb.length() > 0) {
			char firstChar = sb.charAt(0);
			if (firstChar == '0') {
				sb.replace(0, 1, "zero");
			}
			else if (firstChar == '1') {
				sb.replace(0, 1, "one");
			}
			else if (firstChar == '2') {
				sb.replace(0, 1, "two");
			}
			else if (firstChar == '3') {
				sb.replace(0, 1, "three");
			}
			else if (firstChar == '4') {
				sb.replace(0, 1, "four");
			}
			else if (firstChar == '5') {
				sb.replace(0, 1, "five");
			}
			else if (firstChar == '6') {
				sb.replace(0, 1, "six");
			}
			else if (firstChar == '7') {
				sb.replace(0, 1, "seven");
			}
			else if (firstChar == '8') {
				sb.replace(0, 1, "eight");
			}
			else if (firstChar == '9') {
				sb.replace(0, 1, "nine");
			}
		}
	}
}