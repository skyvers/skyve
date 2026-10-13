package org.skyve.impl.metadata.model.document.field;

import java.util.ArrayList;
import java.util.List;

import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.enumeration.DynamicEnumerationConverter;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.generate.DomainGenerator;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.SerializableMetaData;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;

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
 * and are read-only once placed in the repository cache.
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

		public @Nullable String getCode() {
			return code;
		}
		
		@XmlAttribute(required = true)
		public void setCode(@Nullable String code) {
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
				Module referencedModule = ProvidedRepositoryFactory.get().getModule(null, referencedModuleName);
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
	
	@Override
	public @Nonnull Class<?> getImplementingType() {
		Enumeration e = getTarget();
		if (e.isDynamic()) {
			return String.class;
		}
		
		String fullyQualifiedEnumName = null;

		// NB Needs a method call to resolve the target
		final String enumClassName = getImplementingEnumClassName();
		if (enumClassName != null) { // hand-coded enum implementation
			fullyQualifiedEnumName = enumClassName;
		}
		else { // generated implementation
			// No enum overriding, but there might be referencing
			String encapulatingClassName = getEncapsulatingClassName();
			fullyQualifiedEnumName = new StringBuilder(64).append(encapulatingClassName).append('$').append(toJavaIdentifier()).toString();
		}
		
		try {
			@SuppressWarnings("unchecked")
			Class<org.skyve.domain.types.Enumeration> result = (Class<org.skyve.domain.types.Enumeration>) Thread.currentThread().getContextClassLoader().loadClass(fullyQualifiedEnumName);
			return result;
		}
		catch (Exception ex) {
			throw new MetaDataException("A problem was encountered loading enum " + fullyQualifiedEnumName, ex);
		}
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
