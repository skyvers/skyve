package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Geometry;

public class FluentGeometry extends FluentField<FluentGeometry> {
	private Geometry geometry = null;
	
	public FluentGeometry() {
		geometry = new Geometry();
	}

	public FluentGeometry(Geometry geometry) {
		this.geometry = geometry;
	}

	public FluentGeometry from(@SuppressWarnings("hiding") Geometry geometry) {
		super.from(geometry);
		return this;
	}
	
	@Override
	public Geometry get() {
		return geometry;
	}
}
