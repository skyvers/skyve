package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;

/**
 * Builds {@link DataRepeater} metadata for repeated tabular content.
 */
public class FluentDataRepeater extends FluentDataWidget<FluentDataRepeater> {
	private DataRepeater data = null;
	
	/**
	 * Creates a builder backed by a new {@link DataRepeater}.
	 */
	public FluentDataRepeater() {
		data = new DataRepeater();
	}

	/**
	 * Creates a builder backed by the supplied {@link DataRepeater}.
	 *
	 * @param data
	 *            the metadata instance to mutate
	 */
	public FluentDataRepeater(DataRepeater data) {
		this.data = data;
	}

	/**
	 * Copies repeater display and column state from runtime metadata.
	 *
	 * @param data
	 *            the source metadata to copy
	 * @return this builder
	 */
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

	/**
	 * Sets whether column headers are rendered for repeated rows.
	 *
	 * @param showColumnHeaders
	 *            {@code Boolean.TRUE} to render headers, {@code Boolean.FALSE} to hide them, or {@code null} to clear the setting
	 * @return this builder
	 */
	public FluentDataRepeater showColumnHeaders(Boolean showColumnHeaders) {
		data.setShowColumnHeaders(showColumnHeaders);
		return this;
	}

	/**
	 * Sets whether grid styling is rendered for repeated rows.
	 *
	 * @param showGrid
	 *            {@code Boolean.TRUE} to render the grid, {@code Boolean.FALSE} to hide it, or {@code null} to clear the setting
	 * @return this builder
	 */
	public FluentDataRepeater showGrid(Boolean showGrid) {
		data.setShowGrid(showGrid);
		return this;
	}

	/**
	 * Appends a bound column to the repeater.
	 *
	 * @param column
	 *            the bound column to append
	 * @return this builder
	 */
	public FluentDataRepeater addBoundColumn(FluentDataGridBoundColumn column) {
		data.getColumns().add(column.get());
		return this;
	}

	/**
	 * Inserts a bound column at the specified index.
	 *
	 * @param index
	 *            insertion index
	 * @param column
	 *            the bound column to insert
	 * @return this builder
	 */
	public FluentDataRepeater addBoundColumn(int index, FluentDataGridBoundColumn column) {
		data.getColumns().add(index, column.get());
		return this;
	}

	/**
	 * Returns a fluent wrapper for a bound column.
	 *
	 * @param index
	 *            the column position
	 * @return the wrapped bound column
	 */
	public FluentDataGridBoundColumn getBoundColumn(int index) {
		return new FluentDataGridBoundColumn((DataGridBoundColumn) data.getColumns().get(index));
	}
	
	/**
	 * Appends a container column to the repeater.
	 *
	 * @param column
	 *            the container column to append
	 * @return this builder
	 */
	public FluentDataRepeater addContainerColumn(FluentDataGridContainerColumn column) {
		data.getColumns().add(column.get());
		return this;
	}

	/**
	 * Inserts a container column at the specified index.
	 *
	 * @param index
	 *            insertion index
	 * @param column
	 *            the container column to insert
	 * @return this builder
	 */
	public FluentDataRepeater addContainerColumn(int index, FluentDataGridContainerColumn column) {
		data.getColumns().add(index, column.get());
		return this;
	}

	/**
	 * Returns a fluent wrapper for a container column.
	 *
	 * @param index
	 *            the column position
	 * @return the wrapped container column
	 */
	public FluentDataGridContainerColumn getContainerColumn(int index) {
		return new FluentDataGridContainerColumn((DataGridContainerColumn) data.getColumns().get(index));
	}

	/**
	 * Removes a column from the repeater.
	 *
	 * @param index
	 *            the column position to remove
	 * @return this builder
	 */
	public FluentDataRepeater removeColumn(int index) {
		data.getColumns().remove(index);
		return this;
	}
	
	/**
	 * Clears all configured columns from the repeater.
	 *
	 * @return this builder
	 */
	public FluentDataRepeater clearColumns() {
		data.getColumns().clear();
		return this;
	}
	
	/**
	 * Returns the underlying mutable metadata.
	 *
	 * @return the wrapped data repeater metadata
	 */
	@Override
	public DataRepeater get() {
		return data;
	}
}
