package org.skyve.wildcat.metadata.module.menu;

public class MapItem extends AbstractDocumentOrQueryOrModelMenuItem {
	private static final long serialVersionUID = -7998788239200957754L;

	private String geometryBinding;
	private Integer refreshTimeInSeconds;
	private Boolean showRefreshControls;

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
