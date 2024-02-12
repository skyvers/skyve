package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(name = "list", namespace = XMLMetaData.MODULE_NAMESPACE)
@XmlRootElement(name = "list", namespace = XMLMetaData.MODULE_NAMESPACE)
public class ListItemMetaData extends ItemMetaData {
	private static final long serialVersionUID = 6623610207302009047L;

	private String documentName;
	private String queryName;
	private String modelName;
	private Boolean autoPopulate;
	
	public String getDocumentName() {
		return documentName;
	}
	@XmlAttribute(name = "document")
	public void setDocumentName(String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}

	public String getQueryName() {
		return queryName;
	}
	@XmlAttribute(name = "query")
	public void setQueryName(String queryName) {
		this.queryName = UtilImpl.processStringValue(queryName);
	}

	public String getModelName() {
		return modelName;
	}
	@XmlAttribute(name = "model")
	public void setModelName(String modelName) {
		this.modelName = UtilImpl.processStringValue(modelName);
	}

	public Boolean getAutoPopulate() {
		return autoPopulate;
	}
	@XmlAttribute(name = "autoPopulate")
	public void setAutoPopulate(Boolean autoPopulate) {
		this.autoPopulate = autoPopulate;
	}
}
