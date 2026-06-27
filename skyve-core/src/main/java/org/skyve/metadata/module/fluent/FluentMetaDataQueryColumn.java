package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.MetaDataQueryColumnMetaData;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.module.query.MetaDataQueryColumn;

/**
 * Base fluent builder for metadata query column definitions.
 */
public abstract class FluentMetaDataQueryColumn<T extends FluentMetaDataQueryColumn<T>> {
	/**
	 * Creates an empty fluent metadata query column wrapper.
	 */
	protected FluentMetaDataQueryColumn() {
		// nothing to see here
	}
	
	/**
	 * Copies common column fields from an existing metadata query column.
	 *
	 * @param column The source column.
	 * @return this fluent instance.
	 */
	@SuppressWarnings("unchecked")
	protected T from(MetaDataQueryColumn column) {
		name(column.getName());
		binding(column.getBinding());
		displayName(column.getDisplayName());
		sortOrder(column.getSortOrder());
		filterOperator(column.getFilterOperator());
		filterExpression(column.getFilterExpression());
		hidden(column.isHidden());
		Integer i = column.getPixelWidth();
		if (i != null) {
			pixelWidth(i.intValue());
		}
		alignment(column.getAlignment());
		return (T) this;
	}
	
	/**
	 * Sets the logical column name.
	 *
	 * @param name The column name.
	 * @return this fluent instance.
	 */
	@SuppressWarnings("unchecked")
	public T name(String name) {
		get().setName(name);
		return (T) this;
	}

	/**
	 * Sets the binding expression used by this column.
	 *
	 * @param binding The binding expression.
	 * @return this fluent instance.
	 */
	@SuppressWarnings("unchecked")
	public T binding(String binding) {
		get().setBinding(binding);
		return (T) this;
	}

	/**
	 * Sets the display label for this column.
	 *
	 * @param displayName The display label.
	 * @return this fluent instance.
	 */
	@SuppressWarnings("unchecked")
	public T displayName(String displayName) {
		get().setDisplayName(displayName);
		return (T) this;
	}

	/**
	 * Sets the default sort direction for this column.
	 *
	 * @param sortOrder The sort direction.
	 * @return this fluent instance.
	 */
	@SuppressWarnings("unchecked")
	public T sortOrder(SortDirection sortOrder) {
		get().setSortOrder(sortOrder);
		return (T) this;
	}

	/**
	 * Sets the filter operator for this column.
	 *
	 * @param filterOperator The filter operator.
	 * @return this fluent instance.
	 */
	@SuppressWarnings("unchecked")
	public T filterOperator(FilterOperator filterOperator) {
		get().setFilterOperator(filterOperator);
		return (T) this;
	}

	/**
	 * Sets the filter expression for this column.
	 *
	 * @param filterExpression The filter expression.
	 * @return this fluent instance.
	 */
	@SuppressWarnings("unchecked")
	public T filterExpression(String filterExpression) {
		get().setFilterExpression(filterExpression);
		return (T) this;
	}

	/**
	 * Sets whether this column is hidden.
	 *
	 * @param hidden {@code true} to hide the column.
	 * @return this fluent instance.
	 */
	@SuppressWarnings("unchecked")
	public T hidden(boolean hidden) {
		get().setHidden(hidden ? Boolean.TRUE : Boolean.FALSE);
		return (T) this;
	}

	/**
	 * Sets the fixed pixel width for this column.
	 *
	 * @param pixelWidth The pixel width.
	 * @return this fluent instance.
	 */
	@SuppressWarnings("unchecked")
	public T pixelWidth(int pixelWidth) {
		get().setPixelWidth(Integer.valueOf(pixelWidth));
		return (T) this;
	}

	/**
	 * Sets the horizontal alignment for this column.
	 *
	 * @param alignment The alignment value.
	 * @return this fluent instance.
	 */
	@SuppressWarnings("unchecked")
	public T alignment(HorizontalAlignment alignment) {
		get().setAlignment(alignment);
		return (T) this;
	}

	/**
	 * Returns the mutable metadata backing this fluent wrapper.
	 *
	 * @return The metadata query column metadata instance.
	 */
	public abstract MetaDataQueryColumnMetaData get();
}
