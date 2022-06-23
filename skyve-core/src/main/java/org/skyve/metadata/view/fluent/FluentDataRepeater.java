package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;

public class FluentDataRepeater extends FluentDataWidget<FluentDataRepeater> {
	private DataRepeater data = null;
	
	public FluentDataRepeater() {
		data = new DataRepeater();
	}

	public FluentDataRepeater(DataRepeater data) {
		this.data = data;
	}

	public FluentDataRepeater from(@SuppressWarnings("hiding") DataRepeater data) {

		showColumnHeaders(data.getShowColumnHeaders());
		showGrid(data.getShowGrid());

		for (DataGridColumn column : data.getColumns()) {
			if (column instanceof DataGridBoundColumn) {
				addBoundColumn(new FluentDataGridBoundColumn().from(column));
			}
			else {
				addContainerColumn(new FluentDataGridContainerColumn().from(column));
			}
		}

		super.from(data);
		return this;
	}

	public FluentDataRepeater showColumnHeaders(Boolean showColumnHeaders) {
		data.setShowColumnHeaders(showColumnHeaders);
		return this;
	}

	public FluentDataRepeater showGrid(Boolean showGrid) {
		data.setShowGrid(showGrid);
		return this;
	}

	public FluentDataRepeater addBoundColumn(FluentDataGridBoundColumn column) {
		data.getColumns().add(column.get());
		return this;
	}

	public FluentDataRepeater addBoundColumn(int index, FluentDataGridBoundColumn column) {
		data.getColumns().add(index, column.get());
		return this;
	}

	public FluentDataGridBoundColumn getBoundColumn(int index) {
		return new FluentDataGridBoundColumn((DataGridBoundColumn) data.getColumns().get(index));
	}
	
	public FluentDataRepeater addContainerColumn(FluentDataGridContainerColumn column) {
		data.getColumns().add(column.get());
		return this;
	}

	public FluentDataRepeater addContainerColumn(int index, FluentDataGridContainerColumn column) {
		data.getColumns().add(index, column.get());
		return this;
	}

	public FluentDataGridContainerColumn getContainerColumn(int index) {
		return new FluentDataGridContainerColumn((DataGridContainerColumn) data.getColumns().get(index));
	}

	public FluentDataRepeater removeColumn(int index) {
		data.getColumns().remove(index);
		return this;
	}
	
	public FluentDataRepeater clearColumns() {
		data.getColumns().clear();
		return this;
	}
	
	@Override
	public DataRepeater get() {
		return data;
	}
}
