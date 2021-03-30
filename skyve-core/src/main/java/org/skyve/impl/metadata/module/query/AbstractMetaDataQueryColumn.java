package org.skyve.impl.metadata.module.query;

import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.view.TextOutput.Sanitisation;

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

	private boolean escape = true;
	
	private Sanitisation sanitise = Sanitisation.relaxed;
	
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

	@Override
	public boolean isEscape() {
		return escape;
	}

	public void setEscape(boolean escape) {
		this.escape = escape;
	}

	@Override
	public Sanitisation getSanitise() {
		return sanitise;
	}

	public void setSanitise(Sanitisation sanitise) {
		this.sanitise = sanitise;
	}
}
