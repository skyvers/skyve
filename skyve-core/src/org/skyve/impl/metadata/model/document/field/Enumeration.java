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
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.module.Module;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Field;

@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "enum")
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE,
			propOrder = {"xmlTypeName", "moduleRef", "documentRef", "attributeRef", "xmlValues"})
public class Enumeration extends Field {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -8699424041011741395L;

	@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE, propOrder = {"name", "code", "description"})
	public static class EnumeratedValue {
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
			}
			
			return result;
		}
	}
	
	public Enumeration() {
		setAttributeType(AttributeType.enumeration);
	}
	
	private String typeName;
	private List<EnumeratedValue> values = new ArrayList<>();
	private String moduleRef;
	private String documentRef;
	private String attributeRef;
	private Document owningDocument;
	
	@XmlTransient
	public String getTypeName()
	throws MetaDataException {
		return getTarget().typeName;
	}

	public String getXmlTypeName() {
		return typeName;
	}
	
	@XmlAttribute(name = "typeName")
	public void setXmlTypeName(String typeName) {
		this.typeName = typeName;
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
	public List<EnumeratedValue> getValues()
	throws MetaDataException {
		return getTarget().values;
	}
	
	@XmlTransient
	@Override
	public DomainType getDomainType() {
		return DomainType.constant;
	}
	
	public String toJavaIdentifier()
	throws MetaDataException{
		String result = getTarget().typeName;

		if (result == null) {
			result = BindUtil.toJavaTypeIdentifier(getTarget().getName());
		}
		
		return result;
	}
	
	/**
	 * Get the target - it is either another enumeration or itself.
	 * @return
	 * @throws MetaDataException 
	 */
	public Enumeration getTarget()
	throws MetaDataException {
		Enumeration result = this;
		
		// This is a reference to another enumeration
		if (attributeRef != null) {
			// Resolve the reference but don't store it 
			// as it will be potentially different in the future for different customers.

			String referencedModuleName = (moduleRef == null) ? owningDocument.getOwningModuleName() : moduleRef;
			String referencedDocumentName = (documentRef == null) ? owningDocument.getName() : documentRef;
			Document referencedDocument = owningDocument;
			
			if ((moduleRef != null) || (documentRef != null)) {
				Module referencedModule = AbstractRepository.get().getModule(null, referencedModuleName);
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
}
