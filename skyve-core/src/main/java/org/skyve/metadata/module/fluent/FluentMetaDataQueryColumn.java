package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.MetaDataQueryColumnMetaData;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.module.query.MetaDataQueryColumn;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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
	protected @Nonnull T from(@Nonnull MetaDataQueryColumn column) {
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
		column.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));

		return (T) this;
	}
	
	/**
	 * Sets the logical column name.
	 *
	 * @param name The column name.
	 * @return this fluent instance.
	 */
	@SuppressWarnings("unchecked")
	public @Nonnull T name(@Nullable String name) {
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
	public @Nonnull T binding(@Nullable String binding) {
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
	public @Nonnull T displayName(@Nullable String displayName) {
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
	public @Nonnull T sortOrder(@Nullable SortDirection sortOrder) {
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
	public @Nonnull T filterOperator(@Nullable FilterOperator filterOperator) {
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
	public @Nonnull T filterExpression(@Nullable String filterExpression) {
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
	public @Nonnull T hidden(boolean hidden) {
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
	public @Nonnull T pixelWidth(int pixelWidth) {
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
	public @Nonnull T alignment(@Nullable HorizontalAlignment alignment) {
		get().setAlignment(alignment);
		return (T) this;
	}

	/**
	 * Adds or replaces a custom property on the wrapped metadata.
	 * Properties keep the order in which they were first added.
	 *
	 * @param k the property name
	 * @param v the property value
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public @Nonnull T putProperty(@Nonnull String k, @Nonnull String v) {
		get().getProperties().put(k, v);
		return (T) this;
	}

	/**
	 * Returns the mutable metadata backing this fluent wrapper.
	 *
	 * @return The metadata query column metadata instance.
	 */
	public abstract @Nonnull MetaDataQueryColumnMetaData get();
}
