package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.container.form.FormRow;

import jakarta.annotation.Nonnull;

/**
 * Fluent builder for {@link FormRow} metadata.
 */
public class FluentFormRow {
	private FormRow row = null;
	
	/**
	 * Creates a builder backed by a new {@link FormRow} metadata instance.
	 */
	public FluentFormRow() {
		row = new FormRow();
	}

	/**
	 * Creates a builder backed by the supplied row metadata instance.
	 *
	 * @param row
	 *            the row metadata to mutate
	 */
	public FluentFormRow(FormRow row) {
		this.row = row;
	}
	
	/**
	 * Copies all form items from the supplied row.
	 *
	 * <p>Side effects: appends converted items to this builder's row in source order.
	 *
	 * @param row
	 *            the source row metadata
	 * @return this builder
	 */
	public FluentFormRow from(@SuppressWarnings("hiding") FormRow row) {
		row.getItems().forEach(i -> addItem(new FluentFormItem().from(i)));
		row.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));
		return this;
	}
	
	/**
	 * Appends an item to this row.
	 *
	 * @param item
	 *            the item to append
	 * @return this builder
	 */
	public FluentFormRow addItem(FluentFormItem item) {
		row.getItems().add(item.get());
		return this;
	}
	
	/**
	 * Inserts an item into this row at the supplied position.
	 *
	 * @param index
	 *            zero-based insertion index
	 * @param item
	 *            the item to insert
	 * @return this builder
	 */
	public FluentFormRow addItem(int index, FluentFormItem item) {
		row.getItems().add(index, item.get());
		return this;
	}
	
	/**
	 * Returns the item at the supplied position.
	 *
	 * @param index
	 *            zero-based row index
	 * @return the item wrapper at {@code index}
	 */
	public FluentFormItem getItem(int index) {
		return new FluentFormItem(row.getItems().get(index));
	}
	
	/**
	 * Removes the item at the supplied position.
	 *
	 * @param index
	 *            zero-based row index
	 * @return this builder
	 */
	public FluentFormRow removeItem(int index) {
		row.getItems().remove(index);
		return this;
	}

	/**
	 * Removes all items from this row.
	 *
	 * @return this builder
	 */
	public FluentFormRow clearItems() {
		row.getItems().clear();
		return this;
	}
	
	/**
	 * Adds or replaces a custom property on the wrapped metadata.
	 * Properties keep the order in which they were first added.
	 *
	 * @param k the property name
	 * @param v the property value
	 * @return this builder
	 */
	public @Nonnull FluentFormRow putProperty(@Nonnull String k, @Nonnull String v) {
		row.getProperties().put(k, v);
		return this;
	}

	/**
	 * Returns the backing metadata instance.
	 *
	 * @return the mutable row metadata
	 */
	public FormRow get() {
		return row;
	}
}
