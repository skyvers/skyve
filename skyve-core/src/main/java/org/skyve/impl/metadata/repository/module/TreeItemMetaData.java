package org.skyve.impl.metadata.repository.module;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

@XmlType(name = "tree", namespace = XMLMetaData.MODULE_NAMESPACE)
@XmlRootElement(name = "tree", namespace = XMLMetaData.MODULE_NAMESPACE)
public class TreeItemMetaData extends ItemMetaData {
	private static final long serialVersionUID = 6569811887401452181L;

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
