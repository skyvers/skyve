package org.skyve.metadata.view.fluent;


import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridColumn;

abstract class FluentDataGridColumn<T extends FluentDataGridColumn<T>> {
	protected FluentDataGridColumn() {
		// nothing to see
	}

	@SuppressWarnings("unchecked")
	protected T from(DataGridColumn column) {
		title(column.getTitle());
		alignment(column.getAlignment());
		Integer i = column.getPixelWidth();
		if (i != null) {
			pixelWidth(i.intValue());
		}
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T title(String title) {
		get().setTitle(title);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T alignment(HorizontalAlignment alignment) {
		get().setAlignment(alignment);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T pixelWidth(int pixelWidth) {
		get().setPixelWidth(Integer.valueOf(pixelWidth));
		return (T) this;
	}


	public abstract DataGridColumn get();
}
