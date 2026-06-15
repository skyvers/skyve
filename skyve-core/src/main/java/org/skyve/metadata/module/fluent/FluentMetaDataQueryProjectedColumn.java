package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.MetaDataQueryColumnMetaData;
import org.skyve.impl.metadata.repository.module.MetaDataQueryProjectedColumnMetaData;
import org.skyve.metadata.FormatterName;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.view.TextOutput.Sanitisation;

/**
 * Builds metadata query projected column definitions.
 */
public class FluentMetaDataQueryProjectedColumn extends FluentMetaDataQueryColumn<FluentMetaDataQueryProjectedColumn> {
	private MetaDataQueryProjectedColumnMetaData column = null;
	
	/**
	 * Creates a fluent wrapper with a new metadata instance.
	 */
	public FluentMetaDataQueryProjectedColumn() {
		column = new MetaDataQueryProjectedColumnMetaData();
	}

	/**
	 * Creates a fluent wrapper around an existing metadata instance.
	 *
	 * @param column The metadata to mutate.
	 */
	public FluentMetaDataQueryProjectedColumn(MetaDataQueryProjectedColumnMetaData column) {
		this.column = column;
	}

	/**
	 * Copies projected-column fields from an existing definition.
	 *
	 * @param column The source projected column definition.
	 * @return this fluent instance.
	 */
	public FluentMetaDataQueryProjectedColumn from(@SuppressWarnings("hiding") MetaDataQueryProjectedColumn column) {
		super.from(column);
		projected(column.isProjected());
		expression(column.getExpression());
		sortable(column.isSortable());
		filterable(column.isFilterable());
		editable(column.isEditable());
		escape(column.isEscape());
		sanitise(column.getSanitise());
		formatter(column.getFormatterName());
		customFormatter(column.getCustomFormatterName());
		return this;
	}
	
	/**
	 * Sets whether this column is included in projection output.
	 *
	 * @param projected {@code true} to project the column.
	 * @return this fluent instance.
	 */
	public FluentMetaDataQueryProjectedColumn projected(boolean projected) {
		column.setProjected(projected ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets the projection expression.
	 *
	 * @param expression The projection expression.
	 * @return this fluent instance.
	 */
	public FluentMetaDataQueryProjectedColumn expression(String expression) {
		column.setExpression(expression);
		return this;
	}

	/**
	 * Sets whether this column can be sorted.
	 *
	 * @param sortable {@code true} to allow sorting.
	 * @return this fluent instance.
	 */
	public FluentMetaDataQueryProjectedColumn sortable(boolean sortable) {
		column.setSortable(sortable ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets whether this column can be filtered.
	 *
	 * @param filterable {@code true} to allow filtering.
	 * @return this fluent instance.
	 */
	public FluentMetaDataQueryProjectedColumn filterable(boolean filterable) {
		column.setFilterable(filterable ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets whether this column is editable.
	 *
	 * @param editable {@code true} to allow editing.
	 * @return this fluent instance.
	 */
	public FluentMetaDataQueryProjectedColumn editable(boolean editable) {
		column.setEditable(editable ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets whether HTML characters are escaped.
	 *
	 * @param escape {@code true} to escape output.
	 * @return this fluent instance.
	 */
	public FluentMetaDataQueryProjectedColumn escape(boolean escape) {
		column.setEscape(escape ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}
	
	/**
	 * Sets the sanitisation policy for rendered output.
	 *
	 * @param sanitise The sanitisation policy.
	 * @return this fluent instance.
	 */
	public FluentMetaDataQueryProjectedColumn sanitise(Sanitisation sanitise) {
		column.setSanitise(sanitise);
		return this;
	}

	/**
	 * Sets the named formatter applied to this column.
	 *
	 * @param formatterName The formatter name.
	 * @return this fluent instance.
	 */
	public FluentMetaDataQueryProjectedColumn formatter(FormatterName formatterName) {
		column.setFormatterName(formatterName);
		return this;
	}

	/**
	 * Sets a custom formatter identifier.
	 *
	 * @param customFormatterName The custom formatter name.
	 * @return this fluent instance.
	 */
	public FluentMetaDataQueryProjectedColumn customFormatter(String customFormatterName) {
		column.setCustomFormatterName(customFormatterName);
		return this;
	}

	/**
	 * Returns the mutable metadata backing this fluent wrapper.
	 *
	 * @return The projected column metadata instance.
	 */
	@Override
	public MetaDataQueryColumnMetaData get() {
		return column;
	}
}
