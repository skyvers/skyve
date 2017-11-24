package org.skyve.util;

import java.util.List;
import java.util.SortedMap;

import org.skyve.domain.Bean;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.bind.BindUtil;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Collection.Ordering;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

/**
 * 
 */
public class Binder {
	/**
	 * Disallow instantiation
	 */
	private Binder() {
		// nothing to see here
	}
	
	/**
	 * 
	 * @param customer
	 * @param message
	 * @param bean
	 * @return
	 */
	public static String formatMessage(Customer customer, String message, Bean... beans) {
		return BindUtil.formatMessage(customer, message, beans);
	}

	public static String negateCondition(String condition) {
        return BindUtil.negateCondition(condition);
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
		return BindUtil.convert(type, value);
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
	public static Object fromString(Customer customer,
													Converter<?> converter,
													Class<?> type,
													String stringValue,
													boolean fromSerializedFormat) {
		return BindUtil.fromString(customer, converter, type, stringValue, fromSerializedFormat);
	}

	/**
	 * 
	 * @param customer
	 * @param bean
	 * @param binding
	 * @return
	 */
	public static String getDisplay(Customer customer, Bean bean, String binding) {
		return BindUtil.getDisplay(customer, bean, binding);
	}

	/**
	 * Given a list of bindings, create a compound one. ie binding1.binding2.binding3 etc
	 * 
	 * @param bindings
	 * @return
	 */
	public static String createCompoundBinding(String... bindings) {
		return BindUtil.createCompoundBinding(bindings);
	}

	/**
	 * Given a simple binding and an index, create a compound one. ie binding[index]
	 */
	public static String createIndexedBinding(String binding, int index) {
		return BindUtil.createIndexedBinding(binding, index);
	}

	/**
	 * Check to see if the element is in the list. If not, then add it.
	 * 
	 * @param owner
	 * @param binding
	 * @param element
	 * @return	The found or added element.
	 */
	public static Bean ensureElementIsInCollection(Bean owner, String binding, Bean element) {
		return BindUtil.ensureElementIsInCollection(owner, binding, element);
	}

	/**
	 * Return the element found or null if not found.
	 * 
	 * @param owner
	 * @param binding
	 * @param elementBizId
	 * @return	The found element or null.
	 */
	public static Bean getElementInCollection(Bean owner, String binding, String elementBizId) {
		return BindUtil.getElementInCollection(owner, binding, elementBizId);
	}

	/**
	 * 
	 * @param list
	 * @param elementBizId
	 * @return
	 */
	public static <T extends Bean> T findElementInCollection(List<T> list, String elementBizId) {
		// check each bean in the list to see if its ID is the same
		for (T existing : list) {
			if (elementBizId.equals(existing.getBizId())) {
				return existing;
			}
		}

		return null;
	}
	
	/**
	 * Sort a collection by its order metadata.
	 * 
	 * @param owningBean The bean that owns the collection.
	 * @param customer The customer of the owningBean.
	 * @param module The module of the owningBean.
	 * @param document The document of the owningBean.
	 * @param collectionBinding The (possibly compound) collection binding (from Document context).
	 */
	public static void sortCollectionByMetaData(Bean owningBean,
													Customer customer,
													Module module,
													Document document,
													String collectionBinding) {
		BindUtil.sortCollectionByMetaData(owningBean, customer, module, document, collectionBinding);
	}

	/**
	 * Sort the beans collection by the order metadata provided.
	 * 
	 * @param owningBean The bean with collection in it.
	 * @param collection The metadata representing the collection. 
	 * 						This method does not cater for compound binding expressions.
	 * 						Use {@link sortCollectionByMetaData(Customer, Module, Document, Bean, String)} for that.
	 */
	public static void sortCollectionByMetaData(Bean owningBean, Collection collection) {
		BindUtil.sortCollectionByMetaData(owningBean, collection);
	}
	
	/**
	 * Sort a collection of java beans by an arbitrary ordering list.
	 * The list can be of any type, not just Bean.
	 * 
	 * @param beans	The list to sort
	 * @param ordering The sort order
	 */
	public static void sortCollectionByOrdering(List<?> beans, Ordering... ordering) {
		BindUtil.sortCollectionByOrdering(beans, ordering);
	}
	
	/**
	 * Get a simple or compound <code>bean</code> property value.
	 * 
	 * @param bean The bean to get the property value from.
	 * @param fullyQualifiedAttributeName The fully qualified name of a bean property, separating components with a '.'. 
	 * 										Examples would be "identifier" {simple} or "identifier.clientId" {compound}.
	 */
	public static Object get(Object bean, String fullyQualifiedPropertyName) {
		return BindUtil.get(bean, fullyQualifiedPropertyName);
	}

	/**
	 * 
	 * @param bean
	 * @param propertyName
	 * @param value
	 */
	public static void convertAndSet(Object bean, String propertyName, Object value) {
		BindUtil.convertAndSet(bean, propertyName, value);
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
		BindUtil.set(bean, fullyQualifiedPropertyName, value);
	}

	/**
	 * 
	 * @param bean
	 * @param propertyName
	 * @return
	 */
	public static Class<?> getPropertyType(Object bean, String propertyName) {
		return BindUtil.getPropertyType(bean, propertyName);
	}

	/**
	 * 
	 * @param bean
	 * @param propertyName
	 * @return
	 */
	public static boolean isWriteable(Object bean, String propertyName) {
		return BindUtil.isWriteable(bean, propertyName);
	}

	/**
	 * Determine if the propertyType is a scalar type - one that can be presented meaningfully using a Single value.
	 * 
	 * @param propertyType The <code>Class</code> object that represents the property type
	 * @return <code>true</code> if propertyType is scalar, otherwise return <code>false</code>.
	 */
	public static final boolean isAScalarType(Class<?> propertyType) {
		return BindUtil.isAScalarType(propertyType);
	}

	/**
	 * Determine if an attribute name is an implicit attribute.
	 * 
	 * @param attributeName
	 */
	public static final boolean isImplicit(String attributeName) {
		return BindUtil.isImplicit(attributeName);
	}
	
	/**
	 * 
	 * @param user
	 * @param bean
	 * @param properties
	 * @param fromSerializedFormat
	 */
	// NB properties must be a sorted map to ensure that the shortest properties
	// are processed first - ie User.contact is populated before User.contact.firstName,
	// otherwise the firstName new value will be tromped...
	public static void populateProperties(User user, 
											Bean bean, 
											SortedMap<String, Object> properties, 
											boolean fromSerializedFormat) {
		BindUtil.populateProperties(user, bean, properties, fromSerializedFormat);
	}

	/**
	 * 
	 * @param user
	 * @param bean
	 * @param name
	 * @param value
	 * @param fromSerializedFormat
	 */
	public static void populateProperty(User user, 
											Bean bean, 
											String name, 
											Object value, 
											boolean fromSerializedFormat) {
		BindUtil.populateProperty(user, bean, name, value, fromSerializedFormat);
	}

	/**
	 * 
	 */
	public static class TargetMetaData {
		private Document document;
		private Attribute attribute;

		/**
		 * 
		 * @param document
		 * @param attribute
		 */
		public TargetMetaData(Document document, Attribute attribute) {
			this.document = document;
			this.attribute = attribute;
		}

		/**
		 * 
		 * @return
		 */
		public Document getDocument() {
			return document;
		}

		/**
		 * 
		 * @return
		 */
		public Attribute getAttribute() {
			return attribute;
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
	 * 			The document is never null whereas the attribute can be null if'
	 * 			the binding expression ultimately resolves to an implicit attribute
	 * 			like bizKey or bizId or the like.
	 */
	public static TargetMetaData getMetaDataForBinding(Customer customer, 
																Module module, 
																Document document, 
																String binding) {
		return BindUtil.getMetaDataForBinding(customer, module, document, binding);
	}

	/**
	 * 
	 * @param user
	 * @param module
	 * @param document
	 * @param bean
	 * @param binding
	 * @return
	 */
	public static Object instantiateAndGet(User user, Module module, Document document, Bean bean, String binding) {
		return BindUtil.instantiateAndGet(user, module, document, bean, binding);
	}
	
	/**
	 * Fashion a type identifier from the given string.
	 * @param string
	 * @return	A valid java type identifier.  Title case.
	 */
	public static String toJavaTypeIdentifier(String string) {
		return BindUtil.toJavaTypeIdentifier(string);
	}
	
	/**
	 * Fashion an instance identifier from the given string.
	 * @param string
	 * @return	A valid java instance identifier.  Camel case.
	 */
	public static String toJavaInstanceIdentifier(String string) {
		return BindUtil.toJavaInstanceIdentifier(string);
	}
	
	/**
	 * Fashion a static identifier from the given string.
	 * @param string
	 * @return A valid java static identifier.  Upper Case with underscores.
	 */
	public static String toJavaStaticIdentifier(String string) {
		return BindUtil.toJavaStaticIdentifier(string);
	}
}
