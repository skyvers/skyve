package org.skyve.impl.metadata.module.query;

import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.module.query.MetaDataQueryColumn;

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
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	@Override
	public String getDisplayName() {
		return displayName;
	}

	public void setDisplayName(String displayName) {
		this.displayName = displayName;
	}

	@Override
	public String getBinding() {
		return binding;
	}

	public void setBinding(String binding) {
		this.binding = binding;
	}

	@Override
	public FilterOperator getFilterOperator() {
		return filterOperator;
	}

	public void setFilterOperator(FilterOperator filterOperator) {
		this.filterOperator = filterOperator;
	}

	@Override
	public String getFilterExpression() {
		return filterExpression;
	}

	public void setFilterExpression(String filterExpression) {
		this.filterExpression = filterExpression;
	}

	@Override
	public SortDirection getSortOrder() {
		return sortOrder;
	}

	public void setSortOrder(SortDirection sortOrder) {
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
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	@Override
	public HorizontalAlignment getAlignment() {
		return alignment;
	}

	public void setAlignment(HorizontalAlignment alignment) {
		this.alignment = alignment;
	}
}
