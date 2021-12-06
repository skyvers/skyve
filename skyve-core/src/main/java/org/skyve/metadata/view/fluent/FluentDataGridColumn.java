package org.skyve.metadata.view.fluent;


import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridColumn;

abstract class FluentDataGridColumn<T extends FluentDataGridColumn<T>> {
	protected FluentDataGridColumn() {
		// nothing to see
	}

	@SuppressWarnings("unchecked")
	protected T from(DataGridColumn column) {

		return (T) this;
	}

	public abstract DataGridColumn get();
}
