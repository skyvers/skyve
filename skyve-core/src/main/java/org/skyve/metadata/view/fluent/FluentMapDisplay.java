package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.MapDisplay;

public class FluentMapDisplay extends FluentWidget {
	private MapDisplay map = null;
	
	public FluentMapDisplay() {
		map = new MapDisplay();
	}

	public FluentMapDisplay(MapDisplay map) {
		this.map = map;
	}

	public FluentMapDisplay from(@SuppressWarnings("hiding") MapDisplay map) {
		return this;
	}

	@Override
	public MapDisplay get() {
		return map;
	}
}
