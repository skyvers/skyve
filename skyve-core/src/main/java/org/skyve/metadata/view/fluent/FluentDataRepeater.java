package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridColumn;
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

	public FluentDataRepeater addContainerColumn(FluentDataGridContainerColumn column) {
		data.getColumns().add(column.get());
		return this;
	}

	@Override
	public DataRepeater get() {
		return data;
	}
}
