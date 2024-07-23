package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(name = "map", namespace = XMLMetaData.MODULE_NAMESPACE)
@XmlRootElement(name  = "map", namespace = XMLMetaData.MODULE_NAMESPACE)
public class MapItemMetaData extends ItemMetaData {
	private static final long serialVersionUID = -5244934493713612299L;

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
