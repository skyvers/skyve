package org.skyve.wildcat.metadata.module.menu;

public class MapItem extends AbstractMenuItem {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -7998788239200957754L;

	private String documentName;
	private String queryName;
	private String modelName;
	private String geometryBinding;
	private Integer refreshTimeInSeconds;
	private Boolean showRefreshControls;

	public String getDocumentName() {
		return documentName;
	}
	public void setDocumentName(String documentName) {
		this.documentName = documentName;
	}

	public String getQueryName() {
		return queryName;
	}
	public void setQueryName(String queryName) {
		this.queryName = queryName;
	}

	public String getModelName() {
		return modelName;
	}
	public void setModelName(String modelName) {
		this.modelName = modelName;
	}
	public String getGeometryBinding() {
		return geometryBinding;
	}
	public void setGeometryBinding(String geometryBinding) {
		this.geometryBinding = geometryBinding;
	}
	public Integer getRefreshTimeInSeconds() {
		return refreshTimeInSeconds;
	}
	public void setRefreshTimeInSeconds(Integer refreshTimeInSeconds) {
		this.refreshTimeInSeconds = refreshTimeInSeconds;
	}
	public Boolean getShowRefreshControls() {
		return showRefreshControls;
	}
	public void setShowRefreshControls(Boolean showRefreshControls) {
		this.showRefreshControls = showRefreshControls;
	}
}
