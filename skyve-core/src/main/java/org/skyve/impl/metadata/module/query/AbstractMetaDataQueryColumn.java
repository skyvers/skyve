package org.skyve.impl.metadata.module.query;

import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.module.query.MetaDataQueryColumn;

import jakarta.annotation.Nullable;

/**
 * Abstract base for all metadata query column implementations.
 *
 * <p>Holds properties common to every column in a {@link MetaDataQueryDefinition}:
 * binding path, display name, sortability, filter settings, and display/edit
 * width hints.  Concrete subclasses add column-type-specific behaviour:
 * {@link MetaDataQueryProjectedColumnImpl} for bound data attributes and
 * {@link MetaDataQueryContentColumnImpl} for content (file) links.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see MetaDataQueryColumn
 */
public abstract class AbstractMetaDataQueryColumn implements MetaDataQueryColumn {
	private static final long serialVersionUID = 5165779649604451833L;

	private String name;

	private String binding;

	private FilterOperator filterOperator;

	private String filterExpression;

	private SortDirection sortOrder;

	private String displayName;

	private boolean hidden = false;

	private Integer pixelWidth;
	
	private HorizontalAlignment alignment;
	
	@Override
	public @Nullable String getName() {
		return name;
	}

	public void setName(@Nullable String name) {
		this.name = name;
	}

	@Override
	public @Nullable String getDisplayName() {
		return displayName;
	}

	public void setDisplayName(@Nullable String displayName) {
		this.displayName = displayName;
	}

	@Override
	public @Nullable String getBinding() {
		return binding;
	}

	public void setBinding(@Nullable String binding) {
		this.binding = binding;
	}

	@Override
	public @Nullable FilterOperator getFilterOperator() {
		return filterOperator;
	}

	public void setFilterOperator(@Nullable FilterOperator filterOperator) {
		this.filterOperator = filterOperator;
	}

	@Override
	public @Nullable String getFilterExpression() {
		return filterExpression;
	}

	public void setFilterExpression(@Nullable String filterExpression) {
		this.filterExpression = filterExpression;
	}

	@Override
	public @Nullable SortDirection getSortOrder() {
		return sortOrder;
	}

	public void setSortOrder(@Nullable SortDirection sortOrder) {
		this.sortOrder = sortOrder;
	}

	@Override
	public boolean isHidden() {
		return hidden;
	}

	public void setHidden(boolean hidden) {
		this.hidden = hidden;
	}

	@Override
	public @Nullable Integer getPixelWidth() {
		return pixelWidth;
	}

	public void setPixelWidth(@Nullable Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	@Override
	public @Nullable HorizontalAlignment getAlignment() {
		return alignment;
	}

	public void setAlignment(@Nullable HorizontalAlignment alignment) {
		this.alignment = alignment;
	}
}
