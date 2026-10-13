package org.skyve.impl.metadata.module.menu;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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

	public @Nullable String getGeometryBinding() {
		return geometryBinding;
	}
	public void setGeometryBinding(@Nonnull String geometryBinding) {
		this.geometryBinding = geometryBinding;
	}
	public @Nullable Integer getRefreshTimeInSeconds() {
		return refreshTimeInSeconds;
	}
	public void setRefreshTimeInSeconds(@Nullable Integer refreshTimeInSeconds) {
		this.refreshTimeInSeconds = refreshTimeInSeconds;
	}
	public @Nullable Boolean getShowRefreshControls() {
		return showRefreshControls;
	}
	public void setShowRefreshControls(@Nullable Boolean showRefreshControls) {
		this.showRefreshControls = showRefreshControls;
	}
}
