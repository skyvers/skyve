package org.skyve.metadata.view.model.map;

import java.util.List;

import org.locationtech.jts.geom.Envelope;

import jakarta.annotation.Nullable;

public class MapResult {
	private List<MapItem> items;
	// Optional property to set the extents of the viewport in the map
	private Envelope mapExtents;

	public MapResult() {
		// nothing to see here
	}
	
	public MapResult(@Nullable List<MapItem> items, @Nullable Envelope mapExtents) {
		this.items = items;
		this.mapExtents = mapExtents;
	}

	public @Nullable List<MapItem> getItems() {
		return items;
	}
	public void setItems(@Nullable List<MapItem> items) {
		this.items = items;
	}
	public @Nullable Envelope getMapExtents() {
		return mapExtents;
	}
	public void setMapExtents(@Nullable Envelope mapExtents) {
		this.mapExtents = mapExtents;
	}
}
