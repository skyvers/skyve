package org.skyve.wildcat.metadata.repository.module;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(name = "tree", namespace = XMLUtil.MODULE_NAMESPACE)
@XmlRootElement(name = "tree", namespace = XMLUtil.MODULE_NAMESPACE)
public class TreeItem extends Item {
	private String documentName;
	private String queryName;
	private String modelName;
	private String parentBinding;
	
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

	public String getParentBinding() {
		return parentBinding;
	}
	@XmlAttribute(required = true)
	public void setParentBinding(String parentBinding) {
		this.parentBinding = UtilImpl.processStringValue(parentBinding);
	}
}
