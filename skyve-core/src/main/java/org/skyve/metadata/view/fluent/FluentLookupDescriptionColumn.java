package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.LookupDescriptionColumn;

/**
 * Builds {@link LookupDescriptionColumn} metadata using a fluent API.
 */
public class FluentLookupDescriptionColumn {
	private LookupDescriptionColumn column = null;

	/**
	 * Creates a fluent builder backed by a new {@link LookupDescriptionColumn} metadata instance.
	 */
	public FluentLookupDescriptionColumn() {
		column = new LookupDescriptionColumn();
	}

	/**
	 * Creates a fluent builder backed by the supplied {@link LookupDescriptionColumn} metadata instance.
	 *
	 * @param column the metadata instance to mutate
	 */
	public FluentLookupDescriptionColumn(LookupDescriptionColumn column) {
		this.column = column;
	}

	/**
	 * Copies lookup-column metadata into this fluent builder.
	 *
	 * @param column
	 *            the source metadata to copy
	 * @return this builder
	 */
	public FluentLookupDescriptionColumn from(@SuppressWarnings("hiding") LookupDescriptionColumn column) {
		name(column.getName());
		Boolean b = column.getFilterable();
		if (b != null) {
			filterable(b.booleanValue());
		}
		return this;
	}

	/**
	 * Sets whether this column supports filtering in the drop-down.
	 *
	 * @param filterable {@code true} to enable filtering
	 * @return this builder
	 */
	public FluentLookupDescriptionColumn filterable(boolean filterable) {
		column.setFilterable(filterable ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets the binding name for this drop-down column.
	 *
	 * @param name the binding name
	 * @return this builder
	 */
	public FluentLookupDescriptionColumn name(String name) {
		column.setName(name);
		return this;

	}

	/**
	 * Returns the wrapped {@link LookupDescriptionColumn} metadata instance.
	 *
	 * @return the mutable lookup-description column metadata being configured
	 */
	public LookupDescriptionColumn get() {
		return column;
	}
}
