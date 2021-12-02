package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.ZoomIn;

public class FluentZoomIn extends FluentBound<FluentZoomIn> {
	private ZoomIn zoom = null;
	
	public FluentZoomIn() {
		zoom = new ZoomIn();
	}
	
	public FluentZoomIn(ZoomIn zoom) {
		this.zoom = zoom;
	}

	public FluentZoomIn from(@SuppressWarnings("hiding") ZoomIn zoom) {
		super.from(zoom);
		return this;
	}

	@Override
	public ZoomIn get() {
		return zoom;
	}
}
