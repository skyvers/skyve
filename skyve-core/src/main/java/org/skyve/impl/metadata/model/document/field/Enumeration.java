package org.skyve.impl.metadata.model.document.field;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.generate.DomainGenerator;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;

@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "enum")
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE,
			propOrder = {"xmlTypeName", "xmlImplementingEnumClassName", "moduleRef", "documentRef", "attributeRef", "xmlValues"})
public class Enumeration extends ConstrainableField {
	private static final long serialVersionUID = -8699424041011741395L;

	@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE, propOrder = {"name", "code", "description"})
	public static class EnumeratedValue implements MetaData {
		private static final long serialVersionUID = -3923896082658921385L;

		private String name;
		private String code;
		private String description;

		public String getName() {
			return name;
		}
		
		@XmlAttribute
		public void setName(String name) {
			this.name = UtilImpl.processStringValue(name);
		}

		public String getCode() {
			return code;
		}
		
		@XmlAttribute(required = true)
		public void setCode(String code) {
			this.code = UtilImpl.processStringValue(code);
		}

		public String getDescription() {
			return description;
		}

		@XmlAttribute
		public void setDescription(String description) {
			this.description = UtilImpl.processStringValue(description);
		}
		
		public String toJavaIdentifier() {
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
		// This class should never be Serialized, setRepository() is call on DocumentMetaData.convert()
		repository = ProvidedRepositoryFactory.get();
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
	
	private transient ProvidedRepository repository;
	
	@XmlTransient
	public void setRepository(ProvidedRepository repository) {
		this.repository = repository;
	}
	
	@XmlTransient
	public String getTypeName() {
		return getTarget().typeName;
	}

	public String getXmlTypeName() {
		return typeName;
	}
	
	@XmlAttribute(name = "typeName")
	public void setXmlTypeName(String typeName) {
		this.typeName = UtilImpl.processStringValue(typeName);
	}
	
	@XmlTransient
	public String getImplementingEnumClassName() {
		return getTarget().implementingEnumClassName;
	}

	public String getXmlImplementingEnumClassName() {
		return implementingEnumClassName;
	}
	
	@XmlAttribute(name = "implementingEnumClassName")
	public void setXmlImplementingEnumClassName(String implementingEnumClassName) {
		this.implementingEnumClassName = UtilImpl.processStringValue(implementingEnumClassName);
	}

	public Document getOwningDocument() {
		return owningDocument;
	}

	@XmlTransient
	public void setOwningDocument(Document owningDocument) {
		this.owningDocument = owningDocument;
	}

	public String getModuleRef() {
		return moduleRef;
	}

	@XmlAttribute
	public void setModuleRef(String moduleRef) {
		this.moduleRef = UtilImpl.processStringValue(moduleRef);
	}

	public String getDocumentRef() {
		return documentRef;
	}

	@XmlAttribute
	public void setDocumentRef(String documentRef) {
		this.documentRef = UtilImpl.processStringValue(documentRef);
	}

	public String getAttributeRef() {
		return attributeRef;
	}

	@XmlAttribute
	public void setAttributeRef(String attributeRef) {
		this.attributeRef = UtilImpl.processStringValue(attributeRef);
	}

	@XmlElementWrapper(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "values")
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "value")
	public List<EnumeratedValue> getXmlValues() {
		return values;
	}

	@XmlTransient
	public List<EnumeratedValue> getValues() {
		return getTarget().values;
	}
	
	public String toJavaIdentifier() {
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
	public Enumeration getTarget() {
		Enumeration result = this;
		
		// This is a reference to another enumeration
		if (attributeRef != null) {
			// Resolve the reference but don't store it 
			// as it will be potentially different in the future for different customers.

			String referencedModuleName = (moduleRef == null) ? owningDocument.getOwningModuleName() : moduleRef;
			String referencedDocumentName = (documentRef == null) ? owningDocument.getName() : documentRef;
			Document referencedDocument = owningDocument;
			
			if ((moduleRef != null) || (documentRef != null)) {
				Module referencedModule = repository.getModule(null, referencedModuleName);
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
	
	public Class<org.skyve.domain.types.Enumeration> getEnum() {
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
			Class<org.skyve.domain.types.Enumeration> result = (Class<org.skyve.domain.types.Enumeration>) Class.forName(fullyQualifiedEnumName, true, Thread.currentThread().getContextClassLoader());
			return result;
		}
		catch (Exception e) {
			throw new MetaDataException("A problem was encountered loading enum " + fullyQualifiedEnumName, e);
		}
	}
	
	public String getEncapsulatingClassName() {
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
}
