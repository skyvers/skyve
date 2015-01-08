package org.skyve.metadata.view.model.map;

import com.vividsolutions.jts.geom.Geometry;

public class MapFeature {
	private Geometry geometry;
	private boolean zoomable = true;
	private boolean editable = false;
	private String strokeColour;
	private String fillColour;
	private String fillOpacity;
	private String iconDynamicImageName;
	private Integer iconAnchorX;
	private Integer iconAnchorY;

	public MapFeature() {
	}

	public MapFeature(Geometry geometry,
						boolean zoomable,
						boolean editable,
						String strokeColour,
						String fillColour,
						String fillOpacity,
						String iconDynamicImageName,
						Integer iconAnchorX,
						Integer iconAnchorY) {
		this.geometry = geometry;
		this.zoomable = zoomable;
		this.editable = editable;
		this.strokeColour = strokeColour;
		this.fillColour = fillColour;
		this.fillOpacity = fillOpacity;
		this.iconDynamicImageName = iconDynamicImageName;
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

	public String getFillOpacity() {
		return fillOpacity;
	}

	public void setFillOpacity(String fillOpacity) {
		this.fillOpacity = fillOpacity;
	}

	public String getIconDynamicImageName() {
		return iconDynamicImageName;
	}

	public void setIconDynamicImageName(String iconDynamicImageName) {
		this.iconDynamicImageName = iconDynamicImageName;
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
