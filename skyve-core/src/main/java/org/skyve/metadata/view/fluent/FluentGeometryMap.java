package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;

public class FluentGeometryMap extends FluentWidget {
	private GeometryMap map = null;
	
	public FluentGeometryMap() {
		map = new GeometryMap();
	}
	
	public FluentGeometryMap(GeometryMap map) {
		this.map = map;
	}

	public FluentGeometryMap from(@SuppressWarnings("hiding") GeometryMap map) {
		return this;
	}

	@Override
	public GeometryMap get() {
		return map;
	}
}
