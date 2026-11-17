package org.skyve.impl.metadata.model.document.field;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.enumeration.DynamicEnumerationConverter;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.generate.DomainGenerator;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.SerializableMetaData;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.User;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementWrapper;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated field type whose valid values are restricted to a declared
 * enumeration of named constants.
 *
 * <p>An {@code Enumeration} field declares a set of {@link EnumeratedValue}
 * records, each with a {@code code} (stored in the database), an optional
 * {@code description} (displayed to the user), and an optional boolean flag
 * marking the value as the default.  The framework generates a Java enum from
 * these values during domain generation.
 *
 * <p>Enumeration values are resolved via the customer override chain: a customer
 * may extend or reorder the values for a given enumeration field.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache, except for the thread-safe
 * cache of generated enum classes that {@link #getImplementingType()} fills.
 *
 * @see ConstrainableField
 */
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "enum")
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE,
			propOrder = {"xmlTypeName", "xmlImplementingEnumClassName", "moduleRef", "documentRef", "attributeRef", "xmlValues"})
public class Enumeration extends ConstrainableField {
	private static final long serialVersionUID = -8699424041011741395L;

	@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE, propOrder = {"name", "code", "description"})
	public static class EnumeratedValue implements SerializableMetaData {
		private static final long serialVersionUID = -3923896082658921385L;

		private String name;
		private String code;
		private String description;

		public @Nullable String getName() {
			return name;
		}
		
		@XmlAttribute
		public void setName(@Nullable String name) {
			this.name = UtilImpl.processStringValue(name);
		}

		public @Nonnull String getCode() {
			return code;
		}
		
		@XmlAttribute(required = true)
		public void setCode(@Nonnull String code) {
			this.code = UtilImpl.processStringValue(code);
		}

		public @Nullable String getDescription() {
			return description;
		}

		@XmlAttribute
		public void setDescription(@Nullable String description) {
			this.description = UtilImpl.processStringValue(description);
		}
		
		public @Nonnull String toJavaIdentifier() {
			String result = name;

			if (result == null) {
				result = BindUtil.toJavaInstanceIdentifier((description == null) ? code : description);
				if (DomainGenerator.JAVA_RESERVED_WORDS.contains(result)) {
					result += "Value";
				}
			}
			
			return result;
		}
	}
	
	public Enumeration() {
		setAttributeType(AttributeType.enumeration);
	}
	
	// The generated enum type name if the name determined is not appropriate
	private String typeName;
	// The implementing enum class name if we want to replace the generated class with a specialised hand-written class
	private String implementingEnumClassName;
	private List<EnumeratedValue> values = new ArrayList<>();
	private String moduleRef;
	private String documentRef;
	private String attributeRef;
	private Document owningDocument;
	// Generated enum classes by the bean class they were found from - see getImplementingType(). Kept as
	// finding one walks the bean class's superclasses, and a class that does not declare the enum, such
	// as a hand-written extension, throws a ClassNotFoundException each time. Keyed by bean class, so a
	// customer's other bean class, or a class loaded through another class loader, is looked for afresh.
	// Transient, so that deserialising does not resolve classes through another class loader - see readResolve().
	private transient Map<Class<?>, Class<?>> generatedImplementingTypes = new ConcurrentHashMap<>();
	
	@XmlTransient
	public @Nullable String getTypeName() {
		return getTarget().typeName;
	}

	public @Nullable String getXmlTypeName() {
		return typeName;
	}
	
	@XmlAttribute(name = "typeName")
	public void setXmlTypeName(@Nullable String typeName) {
		this.typeName = UtilImpl.processStringValue(typeName);
	}
	
	@XmlTransient
	public @Nullable String getImplementingEnumClassName() {
		return getTarget().implementingEnumClassName;
	}

	public @Nullable String getXmlImplementingEnumClassName() {
		return implementingEnumClassName;
	}
	
	@XmlAttribute(name = "implementingEnumClassName")
	public void setXmlImplementingEnumClassName(@Nullable String implementingEnumClassName) {
		this.implementingEnumClassName = UtilImpl.processStringValue(implementingEnumClassName);
	}

	public @Nullable Document getOwningDocument() {
		return owningDocument;
	}

	@XmlTransient
	public void setOwningDocument(@Nonnull Document owningDocument) {
		this.owningDocument = owningDocument;
	}

	public @Nullable String getModuleRef() {
		return moduleRef;
	}

	@XmlAttribute
	public void setModuleRef(@Nullable String moduleRef) {
		this.moduleRef = UtilImpl.processStringValue(moduleRef);
	}

	public @Nullable String getDocumentRef() {
		return documentRef;
	}

	@XmlAttribute
	public void setDocumentRef(@Nullable String documentRef) {
		this.documentRef = UtilImpl.processStringValue(documentRef);
	}

	public @Nullable String getAttributeRef() {
		return attributeRef;
	}

	@XmlAttribute
	public void setAttributeRef(@Nullable String attributeRef) {
		this.attributeRef = UtilImpl.processStringValue(attributeRef);
	}

	@XmlElementWrapper(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "values")
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "value")
	public @Nonnull List<EnumeratedValue> getXmlValues() {
		return values;
	}

	@XmlTransient
	public @Nonnull List<EnumeratedValue> getValues() {
		return getTarget().values;
	}
	
	public @Nonnull String toJavaIdentifier() {
		String result = getTypeName();

		if (result == null) {
			result = BindUtil.toJavaTypeIdentifier(getTarget().getName());
		}
		
		return result;
	}
	
	/**
	 * Get the target - it is either another enumeration or itself.
	 * @return
	 */
	public @Nonnull Enumeration getTarget() {
		Enumeration result = this;
		
		// This is a reference to another enumeration
		if (attributeRef != null) {
			// Resolve the reference but don't store it 
			// as it will be potentially different in the future for different customers.

			String referencedModuleName = (moduleRef == null) ? owningDocument.getOwningModuleName() : moduleRef;
			String referencedDocumentName = (documentRef == null) ? owningDocument.getName() : documentRef;
			Document referencedDocument = owningDocument;
			
			if ((moduleRef != null) || (documentRef != null)) {
				Module referencedModule = ProvidedRepositoryFactory.getNullSafe().getModule(null, referencedModuleName);
				referencedDocument = referencedModule.getDocument(null, referencedDocumentName);
			}
			
			Attribute attribute = referencedDocument.getAttribute(attributeRef);
			if (! (attribute instanceof Enumeration)) {
				throw new MetaDataException("Enumeration " + owningDocument.getOwningModuleName() + '.' + owningDocument.getName() + '.' + getName() + 
												" does not reference a valid enumeration at " + 
												referencedModuleName + '.' + referencedDocumentName + '.' + attributeRef);
			}
			result = (Enumeration) attribute;
		}
		
		return result;
	}
	
	/**
	 * Returns the enum class that implements this enumeration: {@link String} for a dynamic
	 * enumeration, the hand-written <code>implementingEnumClassName</code>, or the generated enum.
	 * The generated enum is nested in the class that declares the enumeration's attribute, so it is
	 * found as Java finds an inherited nested type: in the bean class of the document that declares
	 * the attribute - the referenced document, for a reference to another document's enumeration -
	 * and then up its superclasses, as far as they are in the <code>modules</code> and
	 * <code>customers</code> packages. The bean class is the current user's customer's (see
	 * {@link DocumentImpl#getBeanClass(Customer)}), or the vanilla bean class without a user, as
	 * during domain generation. For a customer's override of a document, the classes are
	 * <code>&lt;Document&gt;Override</code>, the vanilla <code>&lt;Document&gt;Extension</code> if
	 * there is one, <code>&lt;Document&gt;Vanilla</code> if a customer's override leaves out vanilla
	 * attributes, and the domain class. If none of the customer's classes declares it - a vanilla
	 * document's reference to an enumeration that the customer's override of the referenced document
	 * leaves out - the vanilla bean class and its superclasses are looked in.
	 *
	 * <p>Side effects: caches a generated enum class by the bean class it is found from, so that each
	 * class is looked for once.
	 *
	 * @return	the implementing type; never {@code null}
	 * @throws MetaDataException	if the enum class cannot be loaded; its cause is a
	 * 			{@link ClassNotFoundException} when no class declares the enum, or the bean class
	 * 			cannot be loaded, as before the domain is first compiled
	 * 			(see {@link #isEnumClassLoadingFailure(Throwable)})
	 */
	@Override
	public @Nonnull Class<?> getImplementingType() {
		Enumeration target = getTarget();
		if (target.isDynamic()) {
			return String.class;
		}
		
		// NB Needs a method call to resolve the target
		final String enumClassName = getImplementingEnumClassName();
		if (enumClassName != null) { // hand-coded enum implementation
			try {
				return Thread.currentThread().getContextClassLoader().loadClass(enumClassName);
			}
			catch (Exception ex) {
				throw new MetaDataException("A problem was encountered loading enum " + enumClassName, ex);
			}
		}

		// generated implementation
		// NB a persistence has no user until one is set
		User user = AbstractPersistence.isPresent() ? AbstractPersistence.get().getUser() : null;
		Customer customer = ((user == null) || (user.getCustomerName() == null)) ? null : user.getCustomer();
		Class<?> beanClass = declaringBeanClass(target, customer);
		Class<?> result = generatedImplementingTypes.get(beanClass);
		if (result == null) {
			result = findGeneratedImplementingType(beanClass);
			if ((result == null) && (customer != null)) {
				// A vanilla document's enumeration can reference one that the customer's override of the
				// referenced document leaves out, which only the vanilla classes declare
				try {
					result = findGeneratedImplementingType(declaringBeanClass(target, null));
				}
				catch (@SuppressWarnings("unused") MetaDataException e) {
					// there is no vanilla document - throw for the customer's below
				}
			}
			if (result == null) {
				String generatedEnumClassName = beanClass.getName() + '$' + toJavaIdentifier();
				throw new MetaDataException("A problem was encountered loading enum " + generatedEnumClassName,
												new ClassNotFoundException(generatedEnumClassName));
			}
			generatedImplementingTypes.putIfAbsent(beanClass, result);
		}
		return result;
	}

	/**
	 * Resolves the bean class of the document that declares an enumeration's attribute.
	 *
	 * @param target	the enumeration that declares the values - this one, or the one it references
	 * @param customer	the current user's customer, or {@code null} for the vanilla bean class
	 * @return	the bean class
	 * @throws MetaDataException	if the bean class cannot be loaded, caused by the
	 * 			{@link ClassNotFoundException}
	 */
	private @Nonnull Class<?> declaringBeanClass(@Nonnull Enumeration target, @Nullable Customer customer) {
		if (! (target.getOwningDocument() instanceof DocumentImpl document)) { // cannot happen as the owning document is set when the document is converted
			throw new IllegalStateException("Enumeration " + target.getName() + " has no owning document");
		}
		try {
			return document.getBeanClass(customer);
		}
		catch (ClassNotFoundException e) {
			throw new MetaDataException("A problem was encountered loading enum " + toJavaIdentifier() +
											" of document " + document.getOwningModuleName() + '.' + document.getName(), e);
		}
	}

	/**
	 * Finds the generated enum class that implements this enumeration in the first class, from the
	 * bean class up, that declares it (see {@link #getImplementingType()}).
	 *
	 * @param beanClass	the bean class of the document that declares the enumeration's attribute
	 * @return	the enum class, or {@code null} if none of the document's classes declares it
	 */
	private @Nullable Class<?> findGeneratedImplementingType(@Nonnull Class<?> beanClass) {
		ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
		String enumName = '$' + toJavaIdentifier();
		Class<?> type = beanClass;
		while ((type != null) && isDocumentClass(type.getName())) {
			try {
				return classLoader.loadClass(type.getName() + enumName);
			}
			catch (@SuppressWarnings("unused") ClassNotFoundException e) {
				// not declared in this class
			}
			type = type.getSuperclass();
		}
		return null;
	}

	/**
	 * Determines whether a class is one of a document's - generated or hand-written - which are in the
	 * <code>modules</code> and <code>customers</code> packages, rather than a framework class it extends.
	 *
	 * @param className	the fully qualified class name
	 * @return	true if the class is in the <code>modules</code> or <code>customers</code> packages
	 */
	private static boolean isDocumentClass(@Nonnull String className) {
		return className.startsWith(ProvidedRepository.MODULES_NAME + '.') ||
				className.startsWith(ProvidedRepository.CUSTOMERS_NAME + '.');
	}

	/**
	 * Creates the transient cache of generated enum classes again after deserialisation.
	 *
	 * @return	this enumeration
	 */
	private @Nonnull Object readResolve() {
		generatedImplementingTypes = new ConcurrentHashMap<>();
		return this;
	}
	
	/**
	 * Returns a DynamicEnumerationConverter for a dynamic enumeration or null
	 */
	public @Nullable Converter<String> getConverter() {
		Enumeration e = getTarget();
		if (e.isDynamic()) {
			return new DynamicEnumerationConverter(e);
		}
		return null;
	}
	
	public @Nonnull String getEncapsulatingClassName() {
		StringBuilder result = new StringBuilder(64);
		
		result.append(ProvidedRepository.MODULES_NAME).append('.');
		String moduleName = getModuleRef();
		if (moduleName == null) {
			moduleName = getOwningDocument().getOwningModuleName();
		}
		result.append(moduleName).append('.');
		result.append(ProvidedRepository.DOMAIN_NAME).append('.');
		String documentName = getDocumentRef();
		if (documentName == null) {
			documentName = getOwningDocument().getName();
		}
		result.append(documentName);

		return result.toString();
	}
	
	/**
	 * TODO This is a hack for the chicken and egg enum generation problem to be solved by making generate domain 2 phased.
	 * Returns true if the given throwable or any of its causes is a ClassNotFoundException or NoClassDefFoundError which might indicate a failure to load the enum class.
	 * @param throwable	The throwable to check
	 * @return true if the given throwable or any of its causes is a ClassNotFoundException or NoClassDefFoundError, false otherwise
	 */
	public static boolean isEnumClassLoadingFailure(@Nonnull Throwable throwable) {
		Throwable cause = throwable;
		while (cause != null) {
			if ((cause instanceof ClassNotFoundException) || (cause instanceof NoClassDefFoundError)) {
				return true;
			}
			cause = cause.getCause();
		}
		return false;
	}
}
