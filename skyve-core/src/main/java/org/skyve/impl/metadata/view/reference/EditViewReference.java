package org.skyve.impl.metadata.view.reference;

import org.skyve.impl.metadata.view.widget.bound.AbstractBound;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * This reference can specify a module and document and optionally a binding to a bizId
 * and it will generate a reference to the detail view of that bean.
 * 
 * @author mike
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
public class EditViewReference extends AbstractBound implements Reference {
	private static final long serialVersionUID = -5721839106097161459L;
	
	private String moduleName;
	private String documentName;
	
	public String getModuleName() {
		return moduleName;
	}

	@XmlAttribute(required = true)
	public void setModuleName(String moduleName) {
		this.moduleName = UtilImpl.processStringValue(moduleName);
	}

	public String getDocumentName() {
		return documentName;
	}

	@XmlAttribute(required = true)
	public void setDocumentName(String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}
}
