package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.MetaDataQueryColumnMetaData;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.module.query.MetaDataQueryColumn;

abstract class FluentMetaDataQueryColumn<T extends FluentMetaDataQueryColumn<T>> {
	protected FluentMetaDataQueryColumn() {
		// nothing to see here
	}
	
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
	
	@SuppressWarnings("unchecked")
	public T name(String name) {
		get().setName(name);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T binding(String binding) {
		get().setBinding(binding);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T displayName(String displayName) {
		get().setDisplayName(displayName);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T sortOrder(SortDirection sortOrder) {
		get().setSortOrder(sortOrder);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T filterOperator(FilterOperator filterOperator) {
		get().setFilterOperator(filterOperator);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T filterExpression(String filterExpression) {
		get().setFilterExpression(filterExpression);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T hidden(boolean hidden) {
		get().setHidden(hidden ? Boolean.TRUE : Boolean.FALSE);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T pixelWidth(int pixelWidth) {
		get().setPixelWidth(Integer.valueOf(pixelWidth));
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T alignment(HorizontalAlignment alignment) {
		get().setAlignment(alignment);
		return (T) this;
	}

	public abstract MetaDataQueryColumnMetaData get();
}
