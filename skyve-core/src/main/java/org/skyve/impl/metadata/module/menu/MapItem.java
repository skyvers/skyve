package org.skyve.impl.metadata.module.menu;

/**
 * Menu item that opens the map view of a document or query.
 *
 * <p>Extends {@link AbstractDocumentOrQueryOrModelMenuItem} with a
 * {@code geometryBinding} that identifies the geometry attribute to render as map
 * pins, an optional {@code refreshTimeInSeconds} for periodic data refresh, and
 * a {@code showRefreshControls} flag.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see AbstractDocumentOrQueryOrModelMenuItem
 */
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
