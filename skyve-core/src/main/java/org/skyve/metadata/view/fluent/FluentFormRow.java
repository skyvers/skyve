package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.container.form.FormRow;

public class FluentFormRow {
	private FormRow row = null;
	
	public FluentFormRow() {
		row = new FormRow();
	}

	public FluentFormRow(FormRow row) {
		this.row = row;
	}
	
	public FluentFormRow from(@SuppressWarnings("hiding") FormRow row) {
		row.getItems().forEach(i -> addItem(new FluentFormItem().from(i)));
		return this;
	}
	
	public FluentFormRow addItem(FluentFormItem item) {
		row.getItems().add(item.get());
		return this;
	}
	
	public FormRow get() {
		return row;
	}
}
