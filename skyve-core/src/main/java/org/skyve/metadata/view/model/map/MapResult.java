package org.skyve.metadata.view.model.map;

import java.util.List;

import org.locationtech.jts.geom.Envelope;

public class MapResult {
	private List<MapItem> items;
	// Optional property to set the extents of the viewport in the map
	private Envelope mapExtents;

	public MapResult() {
		// nothing to see here
	}
	
	public MapResult(List<MapItem> items, Envelope mapExtents) {
		this.items = items;
		this.mapExtents = mapExtents;
	}

	public List<MapItem> getItems() {
		return items;
	}
	public void setItems(List<MapItem> items) {
		this.items = items;
	}
	public Envelope getMapExtents() {
		return mapExtents;
	}
	public void setMapExtents(Envelope mapExtents) {
		this.mapExtents = mapExtents;
	}
}
