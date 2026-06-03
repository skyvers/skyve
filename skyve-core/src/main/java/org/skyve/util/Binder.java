package org.skyve.util;

import java.util.List;
import java.util.SortedMap;
import java.util.function.UnaryOperator;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.bind.BindUtil;
import org.skyve.metadata.Ordering;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Utilities for evaluating, getting, and setting bean attribute values via Skyve binding expressions.
 *
 * <p>Skyve binding expressions are dot-separated paths (e.g. {@code "contact.name"}) that
 * navigate through associations and collections to a leaf attribute. All methods in this
 * class accept such paths and delegate to {@link org.skyve.impl.bind.BindUtil} for the
 * actual reflection and metadata-aware traversal.
 *
 * <p>Useful operations:
 * <ul>
 *   <li>{@link #get(Bean, String)} — read a value at a binding path.</li>
 *   <li>{@link #set(Customer, Bean, String, Object)} — write a typed value at a binding path.</li>
 *   <li>{@link #convertAndSet(Customer, Bean, String, String)} — parse a display string and set
 *       it using the attribute's converter.</li>
 *   <li>{@link #getMetaData(Customer, Document, String)} — resolve the attribute metadata and
 *       its owning document at a binding path.</li>
 *   <li>{@link #sortCollectionByMetaDataOrdering}/{@link #sortCollectionByOrdering} — sort a
 *       child collection by its declared or supplied orderings.</li>
 * </ul>
 *
 * <p>Threading: all methods are stateless and thread-safe.
 */
public class Binder {
	/**
	 * Disallow instantiation
	 */
	private Binder() {
		// nothing to see here
	}
	
	/**
	 * Evaluates all Skyve expressions embedded in a message template.
	 *
	 * @param message	message text that may contain {@code {...}} expressions
	 * @param beans	bean context used when resolving expressions
	 * @return the formatted message with all resolvable expressions substituted
	 */
	public static @Nonnull String formatMessage(@Nonnull String message, @Nonnull Bean... beans) {
		return BindUtil.formatMessage(message, beans);
	}

	/**
	 * Evaluates all Skyve expressions embedded in a message template with an optional
	 * post-processing hook for display values.
	 *
	 * @param message	message text that may contain {@code {...}} expressions
	 * @param postEvaluateDisplayValue	optional callback to transform resolved display values
	 * @param beans	bean context used when resolving expressions
	 * @return the formatted message with post-processed display values where applicable
	 */
	public static @Nonnull String formatMessage(@Nonnull String message,
													@Nullable UnaryOperator<String> postEvaluateDisplayValue,
													@Nonnull Bean... beans) {
		return BindUtil.formatMessage(message,
									(postEvaluateDisplayValue == null) ? null : postEvaluateDisplayValue::apply,
									beans);
	}

	/**
	 * Validates all expressions embedded in a message template against one or more documents.
	 *
	 * @param message	message text that may contain {@code {...}} expressions
	 * @param documents	document metadata used for binding validation
	 * @return {@code null} when valid, otherwise a human-readable validation error
	 */
	public static @Nullable String validateMessage(@Nonnull String message, @Nonnull Document... documents) {
		return BindUtil.validateMessageExpressions(message, CORE.getCustomer(), documents);
	}

	/**
	 * Determines whether the supplied text is exactly one Skyve expression token.
	 *
	 * @param expression	candidate expression text
	 * @return {@code true} when the text is a single Skyve expression, otherwise {@code false}
	 */
	public static boolean isSkyveExpression(@Nonnull String expression) {
		return BindUtil.isSkyveExpression(expression);
	}
	
	/**
	 * Determines whether the supplied text contains one or more Skyve expression tokens.
	 *
	 * @param message	message text to inspect
	 * @return {@code true} when at least one expression token is present
	 */
	public static boolean containsSkyveExpressions(@Nonnull String message) {
		return BindUtil.containsSkyveExpressions(message);
	}
	
	/**
	 * Validates message expressions against documents resolved from a module/document-name list.
	 *
	 * @param message	message text that may contain {@code {...}} expressions
	 * @param moduleName	module used to resolve {@code documentNames}
	 * @param documentNames	document names used for expression validation context
	 * @return {@code null} when valid, otherwise a human-readable validation error
	 */
	public static @Nullable String validateMessage(@Nonnull String message,
													@Nonnull String moduleName,
													@Nonnull String... documentNames) {
		Customer c  = CORE.getCustomer();
		Module m = c.getModule(moduleName);
		int documentNamesLength = documentNames.length;
		if (documentNamesLength > 1) {
			Document[] documents = new Document[documentNamesLength];
			for (int i = 0; i < documentNamesLength; i++) {
				String documentName = documentNames[i];
				documents[i] = m.getDocument(c, documentName);
			}
			return BindUtil.validateMessageExpressions(message, c, documents);
		}
		else if (documentNamesLength == 1) {
			return BindUtil.validateMessageExpressions(message, c, m.getDocument(c, documentNames[0]));
		}

		return null;
	}

	/**
	 * Validates a binding expression and returns metadata for its resolved target.
	 *
	 * @param customer	customer context used for metadata resolution
	 * @param module	starting module for the binding
	 * @param document	starting document for the binding
	 * @param binding	dot-separated binding expression
	 * @return resolved target metadata including terminal document, optional attribute, and type
	 */
	public static @Nonnull TargetMetaData validateBinding(@Nonnull Customer customer,
															@Nonnull Module module,
															@Nonnull Document document,
															@Nonnull String binding) {
		return BindUtil.validateBinding(customer, module, document, binding);
	}
	
	/**
	 * Returns the logical negation of a Skyve condition expression.
	 *
	 * @param condition	condition to negate; may be {@code null}
	 * @return negated condition text, or {@code null} when the input is {@code null}
	 */
	public static @Nullable String negateCondition(@Nullable String condition) {
        return BindUtil.negateCondition(condition);
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
		return BindUtil.convert(type, value);
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
		return BindUtil.nullSafeConvert(type, value);
	}
	
	/**
	 * Explicit type coercion using the <code>converter</code> if supplied, or by java language coercion.
	 *
	 * @param customer	customer context used by customer-aware converters; may be {@code null}
	 * @param converter	explicit converter to use; may be {@code null}
	 * @param type	target Java type
	 * @param stringValue	input string value
	 * @return converted value, or {@code null} when conversion resolves to null
	 */
	public static @Nullable Object fromString(@Nullable Customer customer,
												@Nullable Converter<?> converter,
												@Nonnull Class<?> type,
												@Nonnull String stringValue) {
		return BindUtil.fromString(customer, converter, type, stringValue);
	}

	/**
	 * Explicit type coercion from serialised formats using the <code>converter</code> if supplied, or by java language coercion.
	 *
	 * @param converter	explicit converter to use; may be {@code null}
	 * @param type	target Java type
	 * @param stringValue	serialised source value
	 * @return converted value, or {@code null} when conversion resolves to null
	 */
	public static @Nullable Object fromSerialised(@Nullable Converter<?> converter,
													@Nonnull Class<?> type,
													@Nonnull String stringValue) {
		return BindUtil.fromSerialised(converter, type, stringValue);
	}

	/**
	 * Type coercion from serialised formats by java language coercion.
	 *
	 * @param type	target Java type
	 * @param stringValue	serialised source value
	 * @return converted value, or {@code null} when conversion resolves to null
	 */
	public static @Nullable Object fromSerialised(@Nonnull Class<?> type, @Nonnull String stringValue) {
		return BindUtil.fromSerialised(type, stringValue);
	}

	/**
	 * Returns the display-formatted value for a binding on the supplied bean.
	 *
	 * <p>Formatting precedence follows Skyve rules: attribute converter/display metadata,
	 * then customer-level converter rules.
	 *
	 * @param customer	customer context used for converter resolution
	 * @param bean	root bean that owns the binding path
	 * @param binding	dot-separated binding expression
	 * @return localised display text; never {@code null}
	 */
	public static @Nonnull String getDisplay(@Nonnull Customer customer,
												@Nonnull Bean bean,
												@Nonnull String binding) {
		return BindUtil.getDisplay(customer, bean, binding);
	}

	/**
	 * Given a list of bindings, create a compound one. ie binding1.binding2.binding3 etc
	 * 
	 * @param bindings	binding segments to join
	 * @return a compound binding expression joined with {@code '.'}
	 */
	public static @Nonnull String createCompoundBinding(@Nonnull String... bindings) {
		return BindUtil.createCompoundBinding(bindings);
	}

	/**
	 * Given a multiple cardinality binding and an index, create an indexed one. ie binding[index]
	 *
	 * @param binding	collection binding
	 * @param index	element index
	 * @return indexed binding expression
	 */
	public static @Nonnull String createIndexedBinding(@Nonnull String binding, int index) {
		return BindUtil.createIndexedBinding(binding, index);
	}

	/**
	 * Given a multiple cardinality binding and a bizId, create an element one. ie bindingElementById(id)
	 *
	 * @param binding	collection binding
	 * @param bizId	business identifier for the collection element
	 * @return id-based binding expression
	 */
	public static @Nonnull String createIdBinding(@Nonnull String binding, @Nonnull String bizId) {
		return BindUtil.createIdBinding(binding, bizId);
	}

	/**
	 * Check to see if the element is in the list. If not, then add it.
	 * 
	 * @param owner
	 * @param binding
	 * @param element
	 * @return	The found or added element.
	 */
	public static @Nonnull Bean ensureElementIsInCollection(@Nonnull Bean owner,
																@Nonnull String binding,
																@Nonnull Bean element) {
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
	public static @Nullable Bean getElementInCollection(@Nonnull Bean owner,
															@Nonnull String binding,
															@Nonnull String elementBizId) {
		return BindUtil.getElementInCollection(owner, binding, elementBizId);
	}

	/**
	 * Finds a collection element by business identifier.
	 *
	 * @param list	collection to search
	 * @param elementBizId	business identifier to match
	 * @return matching element, or {@code null} when no element matches
	 */
	public static @Nullable <T extends Bean> T findElementInCollection(@Nonnull List<T> list,
																		@Nonnull String elementBizId) {
		// check each bean in the list to see if its ID is the same
		for (T existing : list) {
			if (elementBizId.equals(existing.getBizId())) {
				return existing;
			}
		}

		return null;
	}
	
	/**
	 * Replaces an element in a collection by business identifier, or appends if absent.
	 *
	 * @param list	collection to update
	 * @param element	element to insert or replace
	 */
	public static <T extends Bean> void setElementInCollection(@Nonnull List<T> list, @Nonnull T element) {
		BindUtil.setElementInCollection(list, element);
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
		BindUtil.setAssociation(bean, associationBinding, value);
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
		return BindUtil.addElementToCollection(bean, collectionBinding, element);
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
		BindUtil.addElementToCollection(bean, collectionBinding, index, element);
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
		return BindUtil.removeElementFromCollection(bean, collectionBinding, element);
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
		return BindUtil.removeElementFromCollection(bean, collectionBinding, index);
	}
	
	/**
	 * Order a collection/inverseMany by its order metadata.
	 * 
	 * @param owningBean The bean that owns the collection/inverse.
	 * @param binding The (possibly compound) collection/inverse binding.
	 */
	public static void orderByMetaData(@Nonnull Bean owningBean,
											@Nonnull String collectionBinding) {
		BindUtil.orderByMetaData(owningBean, collectionBinding);
	}

	/**
	 * Order a list of java beans by an arbitrary ordering list.
	 * The list can be of any type, not just Bean.
	 * 
	 * @param beans	The list to order
	 * @param ordering The ordering
	 */
	public static void order(@Nullable List<?> beans, @Nonnull Ordering... ordering) {
		BindUtil.order(beans, ordering);
	}
	
	/**
	 * Get a simple or compound <code>bean</code> property value.
	 * 
	 * @param bean The bean to get the property value from.
	 * @param fullyQualifiedPropertyName The fully qualified name of a bean property, separating components with a '.'. 
	 * 										Examples would be "identifier" {simple} or "identifier.clientId" {compound}.
	 * @return the current property value, or {@code null}
	 */
	public static @Nullable Object get(@Nonnull Object bean, @Nonnull String fullyQualifiedPropertyName) {
		return BindUtil.get(bean, fullyQualifiedPropertyName);
	}

	/**
	 * Converts a value and assigns it to a simple or compound bean property.
	 *
	 * @param bean	bean to update
	 * @param propertyName	property name or compound binding
	 * @param value	value to coerce and assign
	 */
	public static void convertAndSet(@Nonnull Object bean,
										@Nonnull String propertyName,
										@Nullable Object value) {
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
	public static void set(@Nonnull Object bean,
							@Nonnull String fullyQualifiedPropertyName,
							@Nullable Object value) {
		BindUtil.set(bean, fullyQualifiedPropertyName, value);
	}

	/**
	 * Returns the runtime Java type of a simple or compound property.
	 *
	 * @param bean	bean that owns the property
	 * @param propertyName	property name or compound binding
	 * @return resolved Java type for the target property
	 */
	public static @Nonnull Class<?> getPropertyType(@Nonnull Object bean, @Nonnull String propertyName) {
		return BindUtil.getPropertyType(bean, propertyName);
	}

	/**
	 * Determines whether a simple or compound property can be modified.
	 *
	 * @param bean	bean that owns the property
	 * @param propertyName	property name or compound binding
	 * @return {@code true} when the property is mutable
	 */
	public static boolean isMutable(@Nonnull Object bean, @Nonnull String propertyName) {
		return BindUtil.isMutable(bean, propertyName);
	}

	/**
	 * Determine if the propertyType is a scalar type - one that can be presented meaningfully using a Single value.
	 * 
	 * @param propertyType The <code>Class</code> object that represents the property type
	 * @return <code>true</code> if propertyType is scalar, otherwise return <code>false</code>.
	 */
	public static final boolean isAScalarType(@Nonnull Class<?> propertyType) {
		return BindUtil.isAScalarType(propertyType);
	}

	/**
	 * Determine if an attribute name is an implicit attribute.
	 * 
	 * @param attributeName	attribute name to test
	 * @return {@code true} when the name is one of Skyve's implicit attributes
	 */
	public static final boolean isImplicit(@Nullable String attributeName) {
		return BindUtil.isImplicit(attributeName);
	}
	
	/**
	 * Determine if the attribute is dynamic or not.
	 * Fields can be set as dynamic, References are dynamic if they point to a dynamic document.
	 * If the owning document is dynamic then all its fields are dynamic.
	 * 
	 * @param customer	The current customer.
	 * @param module	The module of the owning document.
	 * @param document	The owning document.
	 * @param attribute	The attribute to check, or null to check if implicit binding and document is dynamic.
	 * @return	true if dynamic, otherwise false
	 */
	public static boolean isDynamic(@Nullable Customer customer,
										@Nonnull Module module,
										@Nonnull Document document,
										@Nullable Attribute attribute) {
		return BindUtil.isDynamic(customer, module, document, attribute);
	}
	
	/**
	 * Determine if the attribute is dynamic or not.
	 * Fields can be set as dynamic, References are dynamic if they point to a dynamic document.
	 * This does not take into account if the owning document is dynamic.
	 * 
	 * @param customer	The current customer.
	 * @param module	The module of the owning document.
	 * @param attribute	The attribute to check, or null to check if implicit binding and document is dynamic.
	 * @return	true if dynamic, otherwise false
	 */
	public static boolean isDynamic(@Nullable Customer customer,
										@Nonnull Module module,
										@Nullable Attribute attribute) {
		return BindUtil.isDynamic(customer, module, attribute);
	}
	
	/**
	 * A relation is dynamic if it points to a dynamic document.
	 * This does not take into account if the owning document is dynamic.
	 * 
	 * @param customer	The current customer.
	 * @param module	The module of the owning document.
	 * @param relation	The relation to check.
	 * @return	true if dynamic, otherwise false
	 */
	public static boolean isDynamic(@Nullable Customer customer,
										@Nonnull Module module,
										@Nonnull Relation relation) {
		return BindUtil.isDynamic(customer, module, relation);
	}

	/**
	 * Populates multiple bean properties from a sorted property map.
	 *
	 * <p>{@code properties} must be sorted shortest-binding-first so parent references
	 * are initialised before nested assignments.
	 *
	 * @param user	user context used for metadata-aware conversion
	 * @param bean	bean to populate; may be {@code null}
	 * @param properties	sorted map of property names to values
	 * @param fromSerializedFormat	whether values are supplied in serialised form
	 */
	// NB properties must be a sorted map to ensure that the shortest properties
	// are processed first - ie User.contact is populated before User.contact.firstName,
	// otherwise the firstName new value will be tromped...
	public static void populateProperties(@Nonnull User user, 
											@Nullable Bean bean, 
											@Nullable SortedMap<String, Object> properties, 
											boolean fromSerializedFormat) {
		BindUtil.populateProperties(user, bean, properties, fromSerializedFormat);
	}

	/**
	 * Populates a single bean property with conversion and metadata-aware assignment.
	 *
	 * @param user	user context used for metadata-aware conversion
	 * @param bean	bean to populate
	 * @param name	property name or compound binding
	 * @param value	value to assign
	 * @param fromSerializedFormat	whether {@code value} is supplied in serialised form
	 */
	public static void populateProperty(@Nonnull User user, 
											@Nonnull Bean bean, 
											@Nonnull String name, 
											@Nullable Object value, 
											boolean fromSerializedFormat) {
		BindUtil.populateProperty(user, bean, name, value, fromSerializedFormat);
	}

	/**
	 * Copies all the attribute values of the from bean onto the to bean.
	 * 
	 * @param from Bean to copy (source bean)
	 * @param to Bean to copy to (destination bean).
	 */
	public static void copy(@Nonnull final Bean from, @Nonnull final Bean to) {
		BindUtil.copy(from, to);
	}
	
	/**
	 * Describes the resolved target of a binding expression.
	 *
	 * <p>The target includes the terminal document, optional terminal attribute,
	 * and resolved Java type.
	 */
	public static class TargetMetaData {
		private Document document;
		private Attribute attribute;
		private Class<?> type;

		/**
		 * Creates target metadata for a resolved binding.
		 *
		 * @param document	terminal document reached by the binding
		 * @param attribute	terminal attribute, or {@code null} for implicit targets
		 * @param type	resolved Java type
		 */
		public TargetMetaData(@Nonnull Document document,
								@Nullable Attribute attribute,
								@Nonnull Class<?> type) {
			this.document = document;
			this.attribute = attribute;
			this.type = type;
		}

		public @Nonnull Document getDocument() {
			return document;
		}

		public @Nullable Attribute getAttribute() {
			return attribute;
		}

		public @Nonnull Class<?> getType() {
			return type;
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
	 */
	public static @Nonnull TargetMetaData getMetaDataForBinding(@Nullable Customer customer, 
																	@Nonnull Module module, 
																	@Nonnull Document document, 
																	@Nonnull String binding) {
		return BindUtil.getMetaDataForBinding(customer, module, document, binding);
	}

	/**
	 * Instantiates missing intermediate beans along a binding path and returns the target value.
	 *
	 * @param user	user context used for instance creation
	 * @param module	starting module for the binding
	 * @param document	starting document for the binding
	 * @param bean	root bean instance
	 * @param binding	target binding expression
	 * @return resolved value at {@code binding}, or {@code null}
	 */
	public static @Nullable Object instantiateAndGet(@Nonnull User user,
														@Nonnull Module module,
														@Nonnull Document document,
														@Nonnull Bean bean,
														@Nonnull String binding) {
		return BindUtil.instantiateAndGet(user, module, document, bean, binding);
	}
	
	/**
	 * Fashion a type identifier from the given string.
	 * @param string
	 * @return	A valid java type identifier.  Title case.
	 */
	public static @Nonnull String toJavaTypeIdentifier(@Nonnull String string) {
		return BindUtil.toJavaTypeIdentifier(string);
	}
	
	/**
	 * Fashion an instance identifier from the given string.
	 * @param string
	 * @return	A valid java instance identifier.  Camel case.
	 */
	public static @Nonnull String toJavaInstanceIdentifier(@Nonnull String string) {
		return BindUtil.toJavaInstanceIdentifier(string);
	}
	
	/**
	 * Fashion a static identifier from the given string.
	 * @param string
	 * @return A valid java static identifier.  Upper Case with underscores.
	 */
	public static @Nonnull String toJavaStaticIdentifier(@Nonnull String string) {
		return BindUtil.toJavaStaticIdentifier(string);
	}
	
	/**
	 * Return a java bean property name from a reflected method name.
	 * @param methodName	The method name
	 * @return	"get"/"set"/"is" prefix stripped and the remaining string decapitalised as appropriate.
	 */
	public static @Nonnull String toJavaPropertyName(@Nonnull String methodName) {
		return BindUtil.toJavaPropertyName(methodName);
	}

	/**
	 * Fashion a title case identifier from the given string.
	 * 
	 * @param string The string to convert
	 * @return A title case string. First letter of each word upper cased with spaces between words.
	 */
	public static @Nonnull String toTitleCase(@Nonnull String string) {
		return BindUtil.toTitleCase(string);
	}
}
