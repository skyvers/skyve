package org.skyve.metadata.view.model.map;

import org.locationtech.jts.geom.Geometry;

public class MapFeature {
	private Geometry geometry;
	private boolean zoomable = true;
	private boolean editable = false;
	private String strokeColour;
	private String fillColour;
	private Float fillOpacity;
	private String iconRelativeFilePath;
	private Integer iconAnchorX;
	private Integer iconAnchorY;

	public MapFeature() {
	}

	public MapFeature(Geometry geometry,
			boolean zoomable,
			boolean editable,
			String strokeColour,
			String fillColour,
			Float fillOpacity,
			String iconRelativeFilePath,
			Integer iconAnchorX,
			Integer iconAnchorY) {
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

	public Geometry getGeometry() {
		return geometry;
	}

	public void setGeometry(Geometry geometry) {
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

	public String getStrokeColour() {
		return strokeColour;
	}

	public void setStrokeColour(String strokeColour) {
		this.strokeColour = strokeColour;
	}

	public String getFillColour() {
		return fillColour;
	}

	public void setFillColour(String fillColour) {
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
	public void setFillOpacity(Float fillOpacity) {
		if (fillOpacity != null) {
			if (fillOpacity.floatValue() > 1.0) {
				fillOpacity = 1.0f;
			}
			if (fillOpacity.floatValue() < 0.0) {
				fillOpacity = 0.0f;
			}

			this.fillOpacity = fillOpacity;
		}
	}

	public String getIconRelativeFilePath() {
		return iconRelativeFilePath;
	}

	public void setIconRelativeFilePath(String iconRelativeFilePath) {
		this.iconRelativeFilePath = iconRelativeFilePath;
	}

	public Integer getIconAnchorX() {
		return iconAnchorX;
	}

	public void setIconAnchorX(Integer iconAnchorX) {
		this.iconAnchorX = iconAnchorX;
	}

	public Integer getIconAnchorY() {
		return iconAnchorY;
	}

	public void setIconAnchorY(Integer iconAnchorY) {
		this.iconAnchorY = iconAnchorY;
	}
}
