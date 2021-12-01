package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;

public class FluentDataGrid extends  FluentWidget {
	private DataGrid grid = null;
	
	public FluentDataGrid() {
		grid = new DataGrid();
	}
	
	public FluentDataGrid(DataGrid grid) {
		this.grid = grid;
	}
	
	public FluentDataGrid from(@SuppressWarnings("hiding") DataGrid grid) {
		return this;
	}
	
	@Override
	public DataGrid get() {
		return grid;
	}
}
