package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;

public class FluentListRepeater extends FluentWidget {
	private ListRepeater list = null;

	public FluentListRepeater() {
		list = new ListRepeater();
	}

	public FluentListRepeater(ListRepeater list) {
		this.list = list;
	}

	public FluentListRepeater from(@SuppressWarnings("hiding") ListRepeater list) {

		showColumnHeaders(list.getShowColumnHeaders());
		showGrid(list.getShowGrid());
		return this;
	}

	public FluentListRepeater showColumnHeaders(boolean showColumnHeaders) {
		list.setShowColumnHeaders(showColumnHeaders ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentListRepeater showGrid(boolean showGrid) {
		list.setShowGrid(showGrid ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	@Override
	public ListRepeater get() {
		return list;
	}
}
