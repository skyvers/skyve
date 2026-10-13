package org.skyve.metadata.view.model.map;

import org.locationtech.jts.geom.Geometry;

import jakarta.annotation.Nullable;

public class MapFeature {
	private Geometry geometry;
	private boolean zoomable = true;
	private boolean editable = false;
	private String strokeColour;
	private String fillColour;
	private float fillOpacity = 1.0f;
	private String iconRelativeFilePath;
	private Integer iconAnchorX;
	private Integer iconAnchorY;

	public MapFeature() {
	}

	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public MapFeature(@Nullable Geometry geometry,
						boolean zoomable,
						boolean editable,
						@Nullable String strokeColour,
						@Nullable String fillColour,
						float fillOpacity,
						@Nullable String iconRelativeFilePath,
						@Nullable Integer iconAnchorX,
						@Nullable Integer iconAnchorY) {
		this.geometry = geometry;
		this.zoomable = zoomable;
		this.editable = editable;
		this.strokeColour = strokeColour;
		this.fillColour = fillColour;
		setFillOpacity(fillOpacity);
		this.iconRelativeFilePath = iconRelativeFilePath;
		this.iconAnchorX = iconAnchorX;
		this.iconAnchorY = iconAnchorY;
	}

	public @Nullable Geometry getGeometry() {
		return geometry;
	}

	public void setGeometry(@Nullable Geometry geometry) {
		this.geometry = geometry;
	}

	public boolean isZoomable() {
		return zoomable;
	}

	public void setZoomable(boolean zoomable) {
		this.zoomable = zoomable;
	}

	public boolean isEditable() {
		return editable;
	}

	public void setEditable(boolean editable) {
		this.editable = editable;
	}

	public @Nullable String getStrokeColour() {
		return strokeColour;
	}

	public void setStrokeColour(@Nullable String strokeColour) {
		this.strokeColour = strokeColour;
	}

	public @Nullable String getFillColour() {
		return fillColour;
	}

	public void setFillColour(@Nullable String fillColour) {
		this.fillColour = fillColour;
	}

	public float getFillOpacity() {
		return fillOpacity;
	}

	/**
	 * Set the fill opacity of this map feature. Expects a float between 0.0 (fully transparent) and 1.0 (fully opaque).
	 * 
	 * @param fillOpacity The fill opacity of the feature
	 */
	public void setFillOpacity(float fillOpacity) {
		float value = fillOpacity;
		if (fillOpacity > 1.0f) {
			value = 1.0f;
		}
		if (fillOpacity < 0.0f) {
			value = 0.0f;
		}

		this.fillOpacity = value;
	}

	public @Nullable String getIconRelativeFilePath() {
		return iconRelativeFilePath;
	}

	public void setIconRelativeFilePath(@Nullable String iconRelativeFilePath) {
		this.iconRelativeFilePath = iconRelativeFilePath;
	}

	public @Nullable Integer getIconAnchorX() {
		return iconAnchorX;
	}

	public void setIconAnchorX(@Nullable Integer iconAnchorX) {
		this.iconAnchorX = iconAnchorX;
	}

	public @Nullable Integer getIconAnchorY() {
		return iconAnchorY;
	}

	public void setIconAnchorY(@Nullable Integer iconAnchorY) {
		this.iconAnchorY = iconAnchorY;
	}
}
