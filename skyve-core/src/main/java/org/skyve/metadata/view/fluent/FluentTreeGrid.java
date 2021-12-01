package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;

public class FluentTreeGrid extends FluentWidget {
	private TreeGrid grid = null;
	
	public FluentTreeGrid() {
		grid = new TreeGrid();
	}
	
	public FluentTreeGrid(TreeGrid grid) {
		this.grid = grid;
	}

	public FluentTreeGrid from(@SuppressWarnings("hiding") TreeGrid grid) {
		return this;
	}
		
	@Override
	public TreeGrid get() {
		return grid;
	}
}
