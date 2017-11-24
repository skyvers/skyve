package org.skyve.impl.metadata.repository.module;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

@XmlType(name = "calendar", namespace = XMLMetaData.MODULE_NAMESPACE)
@XmlRootElement(name = "calendar", namespace = XMLMetaData.MODULE_NAMESPACE)
public class CalendarItem extends Item {
	private static final long serialVersionUID = -6472812043264486704L;

	private String documentName;
	private String queryName;
	private String modelName;
	private String startBinding;
	private String endBinding;
	
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

	public String getStartBinding() {
		return startBinding;
	}
	@XmlAttribute(required = true)
	public void setStartBinding(String startBinding) {
		this.startBinding = UtilImpl.processStringValue(startBinding);
	}

	public String getEndBinding() {
		return endBinding;
	}
	@XmlAttribute(required = true)
	public void setEndBinding(String endBinding) {
		this.endBinding = UtilImpl.processStringValue(endBinding);
	}
}
