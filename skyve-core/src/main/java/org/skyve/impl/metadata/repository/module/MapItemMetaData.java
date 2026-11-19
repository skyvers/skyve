package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for a {@code <map>} menu item in a module menu.
 *
 * <p>Navigates to a geospatial map view for the named document and query.
 * Extends {@link ItemMetaData}.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ItemMetaData
 */
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
	
	public @Nullable String getDocumentName() {
		return documentName;
	}
	@XmlAttribute(name = "document")
	public void setDocumentName(@Nullable String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}

	public @Nullable String getQueryName() {
		return queryName;
	}
	@XmlAttribute(name = "query")
	public void setQueryName(@Nullable String queryName) {
		this.queryName = UtilImpl.processStringValue(queryName);
	}

	public @Nullable String getModelName() {
		return modelName;
	}
	@XmlAttribute(name = "model")
	public void setModelName(@Nullable String modelName) {
		this.modelName = UtilImpl.processStringValue(modelName);
	}

	public @Nullable String getGeometryBinding() {
		return geometryBinding;
	}
	@XmlAttribute
	public void setGeometryBinding(@Nullable String geometryBinding) {
		this.geometryBinding = UtilImpl.processStringValue(geometryBinding);
	}

	public @Nullable Integer getRefreshTimeInSeconds() {
		return refreshTimeInSeconds;
	}
	@XmlAttribute(required = false)
	public void setRefreshTimeInSeconds(@Nullable Integer refreshTimeInSeconds) {
		this.refreshTimeInSeconds = refreshTimeInSeconds;
	}
	
	public @Nullable Boolean getShowRefreshControls() {
		return showRefreshControls;
	}
	@XmlAttribute(required = false)
	public void setShowRefreshControls(@Nullable Boolean showRefreshControls) {
		this.showRefreshControls = showRefreshControls;
	}
}
