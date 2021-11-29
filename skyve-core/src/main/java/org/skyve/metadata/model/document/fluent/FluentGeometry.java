package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Geometry;

public class FluentGeometry extends FluentField<FluentGeometry> {
	private Geometry geometry = new Geometry();
	
	public FluentGeometry() {
		// nothing to see
	}

	public FluentGeometry(Geometry geometry) {
		super(geometry);
	}
	
	@Override
	public Geometry get() {
		return geometry;
	}
}
