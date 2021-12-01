package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.Geometry;

public class FluentGeometry extends FluentWidget {
	private Geometry geometry = null;
	
	public FluentGeometry() {
		geometry = new Geometry();
	}

	public FluentGeometry(Geometry geometry) {
		this.geometry = geometry;
	}

	public FluentGeometry from(@SuppressWarnings("hiding") Geometry geometry) {
		return this;
	}

	@Override
	public Geometry get() {
		return geometry;
	}
}
