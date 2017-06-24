package org.skyve.impl.bind;

import java.beans.Introspector;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.StringTokenizer;

import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.collections.comparators.ComparatorChain;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.MapBean;
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
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.ConvertableField;
import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.impl.util.NullTolerantBeanComparator;
import org.skyve.impl.util.ThreadSafeFactory;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Collection.Ordering;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.util.Binder.TargetMetaData;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKTReader;

/**
 * Provides utilities for getting and setting simple and compound bean properties.
 */
public final class BindUtil {
	private static final String DEFAULT_DISPLAY_DATE_FORMAT = "dd/MM/yyyy";

	public static String formatMessage(Customer customer, String message, Bean... beans) {
		StringBuilder result = new StringBuilder(message);
		int openCurlyBraceIndex = result.indexOf("{");
		while (openCurlyBraceIndex >= 0) {
			if ((openCurlyBraceIndex == 0) || // first char is '{' 
					// '{' is present and not escaped with a preceding '\' - ie \{ is escaped
					((openCurlyBraceIndex > 0) && (result.charAt(openCurlyBraceIndex - 1) != '\\'))) {

				int closedCurlyBraceIndex = result.indexOf("}", openCurlyBraceIndex);
				String binding = result.substring(openCurlyBraceIndex + 1, closedCurlyBraceIndex);
				boolean found = false;
				Exception cause = null;
					for (Bean bean : beans) {
						String documentName = bean.getBizDocument();
						if (documentName != null) {
							try {
								// Try to get the value from this bean
								// Do not use BindUtil.getMetaDataForBinding as it may not be a document
								// property, it could be a condition or an implicit property.
								String displayValue = BindUtil.getDisplay(customer, bean, binding);
								result.replace(openCurlyBraceIndex, closedCurlyBraceIndex + 1, displayValue);
								openCurlyBraceIndex = result.indexOf("{", openCurlyBraceIndex + 1);
								found = true;
								
								break;
							}
							catch (Exception e) {
								cause = e;
							}
						}
					}
				
				if (! found) {
					StringBuilder exMessage = new StringBuilder();
					exMessage.append("Bean");
					if (beans.length > 1) {
						exMessage.append("s");
					}
					exMessage.append(" - ");
					
					for (int offset = 0; offset < beans.length; offset++) {
						Bean bean = beans[offset];
						exMessage.append(bean.getBizDocument());
						
						if (offset - 1 < beans.length) {
							exMessage.append(", ");
						}
					}
					
					exMessage.append(" do");
					if (beans.length == 1) {
						exMessage.append("es");
					}
					
					exMessage.append(" not contain binding - ");
					exMessage.append(binding);
					
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

	public static boolean messageIsBound(String message) {
		boolean bound = false;
		int openCurlyBraceIndex = message.indexOf('{');
		while ((! bound) && openCurlyBraceIndex >= 0) {
			bound = (openCurlyBraceIndex == 0) || // first char is '{' 
						// '{' is present and not escaped with a preceding '\' - ie \{ is escaped
						((openCurlyBraceIndex > 0) && (message.charAt(openCurlyBraceIndex - 1) != '\\'));
		}
		
		return bound;
	}
	
	public static boolean messageBindingsAreValid(Customer customer, Module module, Document document, String message) {
		boolean valid = true;
		
		StringBuilder result = new StringBuilder(message);
		int openCurlyBraceIndex = result.indexOf("{");
		while (valid && (openCurlyBraceIndex >= 0)) {
			if ((openCurlyBraceIndex == 0) || // first char is '{' 
					// '{' is present and not escaped with a preceding '\' - ie \{ is escaped
					((openCurlyBraceIndex > 0) && (result.charAt(openCurlyBraceIndex - 1) != '\\'))) {

				int closedCurlyBraceIndex = result.indexOf("}", openCurlyBraceIndex);
				if (closedCurlyBraceIndex < 0) {
					valid = false;
				}
				else {
					String binding = result.substring(openCurlyBraceIndex + 1, closedCurlyBraceIndex);
					if (binding.isEmpty()) {
						valid = false;
					}
					else {
						try {
							// Check the binding in this bean
							TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, binding);
							if (target == null) {
								valid = false;
							}
							openCurlyBraceIndex = result.indexOf("{", openCurlyBraceIndex + 1);
						}
						catch (Exception e) {
							valid = false;
						}
					}
				}
			}
			else { // escaped { found - ie "\{" - remove the escape chars and move on to the next pair of {}
				result.replace(openCurlyBraceIndex - 1, openCurlyBraceIndex, "");
				openCurlyBraceIndex = result.indexOf("{", openCurlyBraceIndex); // NB openCurlyBracedIndex was not decremented but a char was removed
			}
		}

		return valid;
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
	@SuppressWarnings("unchecked")
	public static Object convert(Class<?> type, Object value) {
		Object result = value;

		if (value != null) {
			if (type.equals(Integer.class)) {
				if ((! (value instanceof Integer)) && (value instanceof Number)) {
					result = new Integer(((Number) value).intValue());
				}
			}
			else if (type.equals(Long.class)) {
				if ((! (value instanceof Long)) && (value instanceof Number)) {
					result = new Long(((Number) value).longValue());
				}
			}
			else if (type.equals(Short.class)) {
				if ((! (value instanceof Short)) && (value instanceof Number)) {
					result = new Short(((Number) value).shortValue());
				}
			}
			else if (type.equals(Float.class)) {
				if ((! (value instanceof Float)) && (value instanceof Number)) {
					result = new Float(((Number) value).floatValue());
				}
			}
			else if (type.equals(Double.class)) {
				if ((! (value instanceof Double)) && (value instanceof Number)) {
					result = new Double(((Number) value).doubleValue());
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
								result = type.getMethod(Enumeration.FROM_DESCRIPTION_METHOD_NAME, String.class).invoke(null, value);
							}
						}
						catch (Exception e) {
							throw new DomainException(value + " is not a valid enumerated value in type " + type, e);
						}
					}
					if (result == null) {
						result = Enum.valueOf(type.asSubclass(Enum.class), (String) value);
					}
				}
			}
		}

		return result;
	}

	/**
	 * This method is synchronized as {@link Converter#fromDisplayValue(Object)} requires synchronization. 
	 * Explicit type coercion using the <code>converter</code> if supplied, or by java language coercion.
	 * 
	 * @param attribute Used for type conversion. Can be <code>null</code>.
	 * @param type
	 * @param displayValue
	 * @return
	 */
	public static synchronized Object fromString(Customer customer,
													Converter<?> converter,
													Class<?> type,
													String stringValue,
													boolean fromSerializedFormat) {
		Object result = null;

		try {
			if ((! fromSerializedFormat) && (converter != null)) {
				result = converter.fromDisplayValue(stringValue);
			}
			else if (type.equals(String.class)) {
				result = stringValue;
			}
			else if (type.equals(Integer.class)) {
				if(converter!=null){
					result = converter.fromDisplayValue(stringValue);
				}
				else {
					result = new Integer(stringValue);
				}
			}
			else if (type.equals(Long.class)) {
				result = new Long(stringValue);
			}
			else if (type.equals(Float.class)) {
				result = new Float(stringValue);
			}
			else if (type.equals(Double.class)) {
				result = new Double(stringValue);
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
					result = new DateOnly(date.getTime());
				}
			}
			else if (type.equals(TimeOnly.class)) {
				if (fromSerializedFormat) {
					result = new TimeOnly(stringValue);
				}
				else {
					Date date = customer.getDefaultTimeConverter().fromDisplayValue(stringValue);
					result = new TimeOnly(date.getTime());
				}
			}
			else if (type.equals(DateTime.class)) {
				if (fromSerializedFormat) {
					result = new DateTime(stringValue);
				}
				else {
					Date date = customer.getDefaultDateTimeConverter().fromDisplayValue(stringValue);
					result = new DateTime(date.getTime());
				}
			}
			else if (type.equals(Timestamp.class)) {
				if (fromSerializedFormat) {
					result = new Timestamp(stringValue);
				}
				else {
					Date date = customer.getDefaultTimestampConverter().fromDisplayValue(stringValue);
					result = new Timestamp(date.getTime());
				}
			}
			else if (Geometry.class.isAssignableFrom(type)) {
				result = new WKTReader().read(stringValue);
			}
			else if (Date.class.isAssignableFrom(type)) {
				result = new java.sql.Timestamp(ThreadSafeFactory.getDateFormat(DEFAULT_DISPLAY_DATE_FORMAT).parse(stringValue).getTime());
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
	private static synchronized String toDisplay(Customer customer, 
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
					result = ((Enumeration) value).toDescription();
				}
				else {
					boolean found = false;
					Object codeValue = value;
					for (DomainValue domainValue : domainValues) {
						if (domainValue.getCode().equals(codeValue)) {
							result = domainValue.getDescription();
							found = true;
							break;
						}
					}
					if (! found) {
						result = value.toString();
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
				result = ThreadSafeFactory.getDateFormat(DEFAULT_DISPLAY_DATE_FORMAT).format((Date) value);
			}
			else if (value instanceof Boolean) {
				result = (((Boolean) value).booleanValue() ? "Yes" : "No");
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
				catch (MetaDataException e) {
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
		Converter<?> converter = null;
		List<DomainValue> domainValues = null;
		Object value = get(bean, binding);

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
			catch (MetaDataException e) {
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
						// Get the real deal if this is a MapBean from a query
						Bean realBean = bean;
						if (bean instanceof MapBean) {
							realBean = (Bean) ((MapBean) bean).get(DocumentQuery.THIS_ALIAS);
						}
						
						int lastDotIndex = binding.lastIndexOf('.');
						if (lastDotIndex >= 0) {
							Bean owningBean = (Bean) get(realBean, binding.substring(0, lastDotIndex));
							if ((owningBean != null) && (target != null)) {
								internalDocument = (DocumentImpl) target.getDocument();
								domainValues = internalDocument.getDomainValues((CustomerImpl) customer, domainType, field, owningBean);
							}
						}
						else {
							domainValues = internalDocument.getDomainValues((CustomerImpl) customer, domainType, field, realBean);							
						}
					}
					else {
						domainValues = internalDocument.getDomainValues((CustomerImpl) customer, domainType, field, null);
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
	 * Given a simple binding and an index, create a compound one. ie binding[index]
	 */
	public static String createIndexedBinding(String binding, int index) {
		StringBuilder result = new StringBuilder(64);

		result.append(binding).append('[').append(index).append(']');

		return result.toString();
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

		list.add(element);
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
		Collection targetCollection = (Collection) target.getAttribute();
		sortCollectionByMetaData(owningBean, targetCollection);
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
	 * @param fullyQualifiedAttributeName The fully qualified name of a bean property, separating components with a '.'. 
	 * 										Examples would be "identifier" {simple} or "identifier.clientId" {compound}.
	 */
	public static Object get(Object bean, String fullyQualifiedPropertyName) {
		if (bean instanceof MapBean) {
			return ((MapBean) bean).get(fullyQualifiedPropertyName);
		}

		Object result = null;
		Object currentBean = bean;
		StringTokenizer tokenizer = new StringTokenizer(fullyQualifiedPropertyName, ".");
		while (tokenizer.hasMoreTokens()) {
			String simplePropertyName = tokenizer.nextToken();
			try {
				currentBean = UtilImpl.deproxy(currentBean);
				result = PropertyUtils.getProperty(currentBean, simplePropertyName);
			}
			catch (Exception e) {
				UtilImpl.LOGGER.severe("Could not BindUtil.get(" + bean + ", " + fullyQualifiedPropertyName + ")!");
				UtilImpl.LOGGER.severe("The subsequent stack trace relates to obtaining bean property " + simplePropertyName + " from " + currentBean);
				UtilImpl.LOGGER.severe("If the stack trace contains something like \"Unknown property '" + simplePropertyName + 
										"' on class 'class <blahblah>$$EnhancerByCGLIB$$$<blahblah>'\"" + 
										" then you'll need to use Util.deproxy() before trying to bind to properties in the hibernate proxy.");
				UtilImpl.LOGGER.severe("See https://github.com/skyvers/skyve-cookbook/blob/master/README.md#deproxy for details");
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
		String alias = binding.replace('.', '_');
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
	
	public static void convertAndSet(Object bean, String propertyName, Object value) {
		Class<?> type = getPropertyType(bean, propertyName);
		set(bean, propertyName, convert(type, value));
	}

	/**
	 * Set a simple or compound <code>bean</code> property value.
	 * 
	 * @param bean The bean to set the property value in.
	 * @param value The value to the bean property value to.
	 * @param fullyQualifiedPropertyName The fully qualified name of a bean property, separating components with a '.'. 
	 * 										Examples would be "identifier" {simple} or "identifier.clientId" {compound}.
	 */
	public static void set(Object bean, String fullyQualifiedPropertyName, Object value) {
		try {
			Object valueToSet = value;
			// empty strings to null
			if ((valueToSet != null) && valueToSet.equals("")) {
				valueToSet = null;
			}
	
			if (valueToSet != null) {
				Class<?> propertyType = BindUtil.getPropertyType(bean, fullyQualifiedPropertyName);
	
				// if we are setting a String value to a non-string property then
				// use an appropriate constructor or static valueOf()
				if (String.class.equals(valueToSet.getClass()) && (! String.class.equals(propertyType))) {
					try {
						valueToSet = propertyType.getConstructor(valueToSet.getClass()).newInstance(valueToSet);
					}
					catch (NoSuchMethodException e) {
						valueToSet = propertyType.getMethod("valueOf", String.class).invoke(null, valueToSet);
					}
				}
	
				// Convert the value to String if required
				if (String.class.equals(propertyType)) {
					valueToSet = valueToSet.toString();
				} // if (we have a String property)
			}
			PropertyUtils.setProperty(bean, fullyQualifiedPropertyName, valueToSet);
		}
		catch (Exception e) {
			if (e instanceof SkyveException) {
				throw (SkyveException) e;
			}
			
			throw new MetaDataException(e);
		}
	}

	public static Class<?> getPropertyType(Object bean, String propertyName) {
		try {
			return PropertyUtils.getPropertyType(bean, propertyName);
		}
		catch (Exception e) {
			throw new MetaDataException(e);
		}
	}

	public static boolean isWriteable(Object bean, String propertyName) {
		try {
			return (PropertyUtils.getWriteMethod(PropertyUtils.getPropertyDescriptor(bean, propertyName)) != null);
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
											String propertyName, 
											Object propertyValue, 
											boolean fromSerializedFormat) {
		String name = propertyName;
		Object value = propertyValue;
		
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
		int i = propName.indexOf(PropertyUtils.INDEXED_DELIM);
		if (i >= 0) {
			int k = propName.indexOf(PropertyUtils.INDEXED_DELIM2);
			try {
				index = Integer.parseInt(propName.substring(i + 1, k));
			}
			catch (NumberFormatException e) {
				// do nothing
			}
			propName = propName.substring(0, i);
		}
		int j = propName.indexOf(PropertyUtils.MAPPED_DELIM);
		if (j >= 0) {
			int k = propName.indexOf(PropertyUtils.MAPPED_DELIM2);
			try {
				key = propName.substring(j + 1, k);
			}
			catch (IndexOutOfBoundsException e) {
				// do nothing
			}
			propName = propName.substring(0, j);
		}

		if ((! List.class.isAssignableFrom(BindUtil.getPropertyType(target, propName))) &&
			(! BindUtil.isWriteable(target, propName))) {
			return;
		}

		Converter<?> converter = null;

		// Calculate the property type
		if (target instanceof Bean) {
			Bean targetBean = (Bean) target;

			try {
				type = getPropertyType(targetBean, propName);
			}
			catch (Exception e) {
				return;
			}

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
				PropertyUtils.setIndexedProperty(target, propName, index, newValue);
			}
			else if (key != null) {
				PropertyUtils.setMappedProperty(target, propName, key, newValue);
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
			case PropertyUtils.NESTED_DELIM:
				if (bracketCount < 1) {
					return i;
				}
				break;

			case PropertyUtils.MAPPED_DELIM:
			case PropertyUtils.INDEXED_DELIM:
				// not bothered which
				--bracketCount;
				break;

			case PropertyUtils.MAPPED_DELIM2:
			case PropertyUtils.INDEXED_DELIM2:
				// not bothered which
				++bracketCount;
				break;
			default:
			}
		}

		// can't find any
		return -1;
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
	 * 			The document is never null whereas the attribute can be null if'
	 * 			the binding expression ultimately resolves to an implicit attribute
	 * 			like bizKey or bizId or the like.
	 */
	public static TargetMetaData getMetaDataForBinding(Customer customer, 
														Module module, 
														Document document, 
														String binding) {
		Document navigatingDocument = document;
		Module navigatingModule = module;
		Attribute attribute = null;

		StringTokenizer tokenizer = new StringTokenizer(binding, ".");
		while (tokenizer.hasMoreTokens()) {
			String fieldName = tokenizer.nextToken();
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
					inherits = navigatingDocument.getExtends();
					String parentDocumentName = navigatingDocument.getParentDocumentName();
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
					navigatingDocument = navigatingModule.getDocument(customer, parentDocumentName);
				}
				else if (attribute instanceof Relation) {
					navigatingDocument = navigatingModule.getDocument(customer, ((Relation) attribute).getDocumentName());
				}
				else {
					throw new MetaDataException(navigatingDocument.getOwningModuleName() + '.' + 
													navigatingDocument.getName() + " @ " + binding + 
													" does not exist (token [" + 
													tokenizer.nextToken() +
													"] doesn't check out)");
				}
				navigatingModule = customer.getModule(navigatingDocument.getOwningModuleName());
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
						throw new IllegalArgumentException(binding + 
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
				catch (IndexOutOfBoundsException e) {
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
							if (fillerElement instanceof ChildBean) {
								try {
									((ChildBean<Bean>) fillerElement).setParent((Bean) owner);
								}
								catch (ClassCastException cce) {
									// continue on - this child bean is being linked to
									// by some other document as an "aggregation"
									// IE the owner variable above is not the parent document
									// but some aggregating document
								}
							}
							collection.add(fillerElement);
						}
	
						value = collection.get(collectionIndex);
					}
				}
				if (value == null) {
					value = navigatingDocument.newInstance(user);
					BindUtil.set(owner, bindingPart, value);
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
		removeInvalidCharacters(sb);
		sb.setCharAt(0, Character.toLowerCase(sb.charAt(0)));
		return sb.toString();
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

	private static void removeInvalidCharacters(StringBuilder sb) {
		int i = 0;
		boolean whiteSpaceOrUnderscore = false;
		while (i < sb.length()) {
			if (i == 0) {
				if (Character.isJavaIdentifierStart(sb.charAt(0)) && (sb.charAt(0) != '_')) {
					i++;
				}
				else {
					sb.deleteCharAt(0);
				}
			}
			else {
				if (Character.isJavaIdentifierPart(sb.charAt(i)) && (sb.charAt(i) != '_')) {
					if (whiteSpaceOrUnderscore) {
						sb.setCharAt(i, Character.toUpperCase(sb.charAt(i)));
					}
					whiteSpaceOrUnderscore = false;
					i++;
				}
				else {
					if (Character.isWhitespace(sb.charAt(i)) || (sb.charAt(i) == '_')) {
						whiteSpaceOrUnderscore = true;
					}
					sb.deleteCharAt(i);
				}
			}
		}
	}
}