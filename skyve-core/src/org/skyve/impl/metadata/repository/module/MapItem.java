package org.skyve.impl.metadata.repository.module;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLUtil;

@XmlType(name = "map", namespace = XMLUtil.MODULE_NAMESPACE)
@XmlRootElement(name  = "map", namespace = XMLUtil.MODULE_NAMESPACE)
public class MapItem extends Item {
	private String documentName;
	private String queryName;
	private String modelName;
	private String geometryBinding;
	private Integer refreshTimeInSeconds;
	private Boolean showRefreshControls;
	
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

	public String getGeometryBinding() {
		return geometryBinding;
	}
	@XmlAttribute
	public void setGeometryBinding(String geometryBinding) {
		this.geometryBinding = UtilImpl.processStringValue(geometryBinding);
	}

	public Integer getRefreshTimeInSeconds() {
		return refreshTimeInSeconds;
	}
	@XmlAttribute(required = false)
	public void setRefreshTimeInSeconds(Integer refreshTimeInSeconds) {
		this.refreshTimeInSeconds = refreshTimeInSeconds;
	}
	
	public Boolean getShowRefreshControls() {
		return showRefreshControls;
	}
	@XmlAttribute(required = false)
	public void setShowRefreshControls(Boolean showRefreshControls) {
		this.showRefreshControls = showRefreshControls;
	}
}
