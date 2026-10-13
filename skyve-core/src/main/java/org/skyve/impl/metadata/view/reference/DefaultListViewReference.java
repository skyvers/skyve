package org.skyve.impl.metadata.view.reference;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * This reference can specify a module and document and use the default document query to show the list view.
 * 
 * @author mike
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
public class DefaultListViewReference implements Reference {
	private static final long serialVersionUID = -1982657919843061491L;
	
	private String moduleName;
	private String documentName;
	
	public @Nullable String getModuleName() {
		return moduleName;
	}

	@XmlAttribute(required = true)
	public void setModuleName(@Nullable String moduleName) {
		this.moduleName = UtilImpl.processStringValue(moduleName);
	}

	public @Nullable String getDocumentName() {
		return documentName;
	}

	@XmlAttribute(required = true)
	public void setDocumentName(@Nullable String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}
}
