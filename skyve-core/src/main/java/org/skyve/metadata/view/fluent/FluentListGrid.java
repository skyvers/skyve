package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;

public class FluentListGrid extends FluentWidget {
	private ListGrid grid = null;
	
	public FluentListGrid() {
		grid = new ListGrid();
	}
	
	public FluentListGrid(ListGrid grid) {
		this.grid = grid;
	}
	
	public FluentListGrid from(@SuppressWarnings("hiding") ListGrid grid) {
		return this;
	}
	
	@Override
	public ListGrid get() {
		return grid;
	}
}
